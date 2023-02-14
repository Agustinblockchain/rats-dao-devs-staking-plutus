{-# LANGUAGE DataKinds                  #-}
-- {-# LANGUAGE DeriveAnyClass             #-}
-- {-# LANGUAGE DeriveGeneric              #-}
-- {-# LANGUAGE DerivingStrategies         #-}
-- {-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase                 #-}
-- {-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
-- {-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE TupleSections              #-}
-- {-# LANGUAGE AllowAmbiguousTypes        #-}
-- {-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE BangPatterns #-}
-- -- {-# LANGUAGE Strict #-}
{- HLINT ignore "Use camelCase" -}

------------------------------------------------------------------------------------------
module Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.SendBackFund 
(
    policy_TxID_Master_SendBackFund 
) 
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Ada                                                 as LedgerAda
import qualified Ledger.Value                                               as LedgerValue (assetClassValue, AssetClass (AssetClass))
import qualified Plutonomy  
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, txOutValue, MintingPolicy) 
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo, valuePaidTo) 
import qualified PlutusTx                                                   (compile, applyCode, liftCode)
import           PlutusTx.Prelude                                           ( Bool, Maybe(Nothing, Just), Eq((==)), BuiltinData, Semigroup((<>)), (&&), error, traceError, ($), negate, traceIfFalse, (||), length )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (getFundAmountsRemains_ForMaster, mkUpdated_PoolDatum_With_SendBackFund, valueIncludesValue, valueEqualsValue, getPoolDatumTypo_FromDatum)
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (getInputsWithDatum, getOutputsWithDatum, isTerminated, validateMasterAction, isNFT_Minted_With_AC)
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (validateBurn_Token_Own_CS_Any_TN, checkIfAllAreFromSameAddress, checkIfAllSpendRedeemersAreEqual, getTxOut_Datum, getTxOut_Value, getTxOut_Value_And_SomeDatum)
import qualified Validators.StakePlusV2.Types.Constants                     as T (poolID_TN, txID_Master_SendBackFund_TN, const_1_PD)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, PoolDatumTypo (..))
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator (RedeemerMasterSendBackFund), RedeemerMasterSendBackFundTypo (..))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_Master_SendBackFund #-}
mkPolicy_TxID_Master_SendBackFund :: T.PoolParams -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_Master_SendBackFund !pParams !mintRedeemerRaw !ctxRaw  = 
  let
        !mintRedeemer = LedgerApiV2.unsafeFromBuiltinData @T.Redeemer_TxID mintRedeemerRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
    in
        if case mintRedeemer of
            (T.RedeemerBurn_TxID T.RedeemerBurn_TxIDTypo) -> OnChainNFTHelpers.validateBurn_Token_Own_CS_Any_TN ctx
            (T.RedeemerMint_TxID T.RedeemerMint_TxIDTypo{..}) ->
                let
                    !redeemer' = mrRedeemerValidator
                    !inputs_WithDatum = OnChainHelpers.getInputsWithDatum ctx
                    !outputs_WithDatum = OnChainHelpers.getOutputsWithDatum ctx
                    !inputs_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- inputs_WithDatum ]
                    !outputs_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- outputs_WithDatum ]
                in
                    traceIfFalse "INVIO" (OnChainNFTHelpers.checkIfAllAreFromSameAddress inputs_WithDatum outputs_WithDatum) && 
                    traceIfFalse "INVR" (OnChainNFTHelpers.checkIfAllSpendRedeemersAreEqual ctx redeemer') && 
                    
                    case redeemer' of
                        (T.RedeemerMasterSendBackFund redeemer) ->
                            validateMasterSendBackFund pParams ctx redeemer inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
                        _ -> traceError "INVOP"
        then ()

        else error ()

------------------------------------------------------------------------------

{-# INLINABLE validateMasterSendBackFund #-}
validateMasterSendBackFund :: T.PoolParams -> LedgerContextsV2.ScriptContext -> T.RedeemerMasterSendBackFundTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> Bool
validateMasterSendBackFund !pParams !ctx !redeemer !inputs_TxOut_Values_And_Datums !outputs_TxOut_Values_And_Datums  =
        OnChainHelpers.validateMasterAction pParams info master &&
        traceIfFalse "MSBF" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_SendBackFund_AC  info ) && 
        traceIfFalse "WIO" correctIO &&

        traceIfFalse "NOTTERMINATED" (OnChainHelpers.isTerminated pParams info poolDatum_In) &&
        
        (
            master == master_To_SendBack || traceIfFalse "MSBFV" correctFundAmount_SendBackToMaster 
        ) &&
        traceIfFalse "F0" correctFundCount && 
        traceIfFalse "PD" correctOutput_PoolDatum_Updated_With_SendBackFund &&        
        traceIfFalse "PDV" correctOutput_PoolDatum_Value_WithTokens        
    where
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !master = T.rmsbfMaster redeemer
        !master_To_SendBack = T.rmsbfMasterToSendBack redeemer
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        !txID_Master_SendBackFund_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !txID_Master_SendBackFund_AC = LedgerValue.AssetClass (txID_Master_SendBackFund_CS, T.txID_Master_SendBackFund_TN)
        ------------------
        !input_TxOut_Value_And_PoolDatum =
            case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum poolID_AC Helpers.getPoolDatumTypo_FromDatum inputs_TxOut_Values_And_Datums of
                Nothing -> traceError "IPD"
                Just x  -> x
        ------------------
        !output_TxOut_Value_And_PoolDatum =
            case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum poolID_AC Helpers.getPoolDatumTypo_FromDatum outputs_TxOut_Values_And_Datums of
                Nothing -> traceError "OPD"
                Just x  -> x
        ------------------
        correctIO :: Bool
        correctIO =
            -- caseIO # : REF INPUTS , NORMAL INPUTS      / OUTPUTS
            -- caseIO 1 : 0 R        , 1 (1 PD)           / 1 (1 PD)
            (length inputs_TxOut_Values_And_Datums  == T.const_1_PD ) &&
            (length outputs_TxOut_Values_And_Datums == T.const_1_PD ) 
        ------------------
        !poolDatum_In = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_PoolDatum
        ------------------
        !value_In_PoolDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_PoolDatum
        ---------------------
        !value_For_PoolDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_PoolDatum
        ---------------------
        !(sendBackFundAmountForMaster, sendBackminAda_ForMaster) = Helpers.getFundAmountsRemains_ForMaster poolDatum_In master_To_SendBack
        ------------------
        !value_For_SendBackminAda_ForMaster = LedgerAda.lovelaceValueOf sendBackminAda_ForMaster
        ------------------
        -- !harvest_CS =  T.ppHarvest_CS pParams
        -- !harvestIsAda = LedgerApiV2.adaSymbol == harvest_CS
        -- !haverstIsWithoutTokenName = not harvestIsAda && T.ppHarvest_TN pParams == LedgerApiV2.TokenName emptyByteString
        ------------------
        -- !(!value_For_SendBackFundAmount, getBackFundAmount_Calculated_From_Diff_PoolDatum) =
        --     if haverstIsWithoutTokenName then
        --         let
        --             !value_In_PoolDatum_FromCurrencySymbol = Helpers.getValueOfCurrencySymbol value_In_PoolDatum harvest_CS
        --             !value_For_PoolDatum_Real_FromCurrencySymbol = Helpers.getValueOfCurrencySymbol value_For_PoolDatum_Real harvest_CS
        --         ---------------------
        --             !value_For_SendBackFundAmount' = value_In_PoolDatum_FromCurrencySymbol <> negate value_For_PoolDatum_Real_FromCurrencySymbol
        --             !sendBackFundAmountForMaster'' = Helpers.getAmtOfCurrencySymbol value_For_SendBackFundAmount harvest_CS
        --         in
        --             (value_For_SendBackFundAmount', sendBackFundAmountForMaster'')
        --     else
        --         let
        --             !harvest_AC = LedgerValue.AssetClass (harvest_CS, T.ppHarvest_TN pParams)
        --             !value_For_SendBackFundAmount' = LedgerValue.assetClassValue harvest_AC sendBackFundAmountForMaster
        --         in
        --             (value_For_SendBackFundAmount', sendBackFundAmountForMaster)
        ------------------
        !harvest_AC = LedgerValue.AssetClass (T.ppHarvest_CS pParams, T.ppHarvest_TN pParams)
        !value_For_SendBackFundAmountForMaster = LedgerValue.assetClassValue harvest_AC sendBackFundAmountForMaster
        ------------------
        !value_For_SendBackFundAmountForMasterPlusAda = value_For_SendBackFundAmountForMaster <> value_For_SendBackminAda_ForMaster
        ------------------
        correctFundCount :: Bool
        !correctFundCount = T.pdFundCount poolDatum_In == 0
        ------------------
        correctFundAmount_SendBackToMaster :: Bool
        !correctFundAmount_SendBackToMaster = 
            let 
                !value_For_SendBackFundAmountForMaster_Real = LedgerContextsV2.valuePaidTo info master_To_SendBack
                !value_For_SendBackFundAmountForMaster_Control = value_For_SendBackFundAmountForMasterPlusAda
            in
                Helpers.valueIncludesValue value_For_SendBackFundAmountForMaster_Real value_For_SendBackFundAmountForMaster_Control
        ------------------
        correctOutput_PoolDatum_Updated_With_SendBackFund :: Bool
        !correctOutput_PoolDatum_Updated_With_SendBackFund =
            let
                !poolDatum_Out_Control = Helpers.mkUpdated_PoolDatum_With_SendBackFund poolDatum_In master_To_SendBack 
                !poolDatum_Out_Real = OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_PoolDatum
            in
                poolDatum_Out_Real == poolDatum_Out_Control 
        ------------------
        correctOutput_PoolDatum_Value_WithTokens :: Bool
        !correctOutput_PoolDatum_Value_WithTokens =
            let
                !value_For_Mint_TxID_Master_SendBackFund = LedgerValue.assetClassValue txID_Master_SendBackFund_AC 1
                !value_For_PoolDatum_Control = value_In_PoolDatum <> value_For_Mint_TxID_Master_SendBackFund <> negate value_For_SendBackFundAmountForMasterPlusAda
            in
                -- if haverstIsWithoutTokenName then
                --     sendBackFundAmountForMaster == getBackFundAmount_Calculated_From_Diff_PoolDatum && value_For_PoolDatum_Real == value_For_PoolDatum_Control
                -- else
                Helpers.valueEqualsValue value_For_PoolDatum_Real value_For_PoolDatum_Control

--------------------------------------------------------------------------------

policy_TxID_Master_SendBackFund :: T.PoolParams -> LedgerApiV2.MintingPolicy
policy_TxID_Master_SendBackFund pParams = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams 

original_policy :: T.PoolParams -> Plutonomy.MintingPolicy
original_policy pParams =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkPolicy_TxID_Master_SendBackFund ||])
    `PlutusTx.applyCode` PlutusTx.liftCode pParams
    
--------------------------------------------------------------------------------
