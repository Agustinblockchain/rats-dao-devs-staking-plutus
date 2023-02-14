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
module Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.FundAndMerge
(
    policy_TxID_Master_FundAndMerge -- , txID_Master_FundAndMerge_CS
)
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Ada                                                 as LedgerAda
import qualified Ledger.Value                                               as LedgerValue (assetClassValue, AssetClass (AssetClass))
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, txOutValue, MintingPolicy, CurrencySymbol) 
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo) 
import qualified PlutusTx                                                   (compile, applyCode, liftCode)
import           PlutusTx.Prelude                                           ( Bool, Maybe(Nothing, Just), Eq((==)), BuiltinData, Semigroup((<>)), (&&), error, traceError, ($), foldl, length, (<$>), traceIfFalse, (||), AdditiveSemigroup ((+)) )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (mkUpdated_PoolDatum_With_NewFundAmountAndMerging, mkUpdated_FundDatum_WithNewFundAmountAndMerging, valueEqualsValue, getFundDatumTypo_FromDatum, getPoolDatumTypo_FromDatum) 
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (getInputsWithDatum, getOutputsWithDatum, validateMasterAction, isNFT_Minted_With_AC, isNotTerminated) 
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (getTxOut_Datum, getTxOut_Value, validateBurn_Token_Own_CS_Any_TN, checkIfAllAreFromSameAddress, checkIfAllSpendRedeemersAreEqual, getTxOut_Value_And_SomeDatum, getTxOuts_Values_And_SomeDatums) 
import qualified Validators.StakePlusV2.Types.Constants                     as T (poolID_TN, fundID_TN, txID_Master_FundAndMerge_TN, const_1_PD, const_1_FD)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum)
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerMasterFundAndMergeTypo (..), RedeemerValidator (RedeemerMasterFundAndMerge))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_Master_FundAndMerge #-}
mkPolicy_TxID_Master_FundAndMerge :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_Master_FundAndMerge !pParams !txID_Master_Fund_CS !mintRedeemerRaw !ctxRaw  = 
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
                        (T.RedeemerMasterFundAndMerge redeemer) ->
                            validateMasterFundAndMerge pParams txID_Master_Fund_CS ctx redeemer inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
                        _ -> traceError "INVOP"
        then ()

        else error ()

------------------------------------------------------------------------------

{-# INLINABLE validateMasterFundAndMerge #-}
validateMasterFundAndMerge :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerContextsV2.ScriptContext -> T.RedeemerMasterFundAndMergeTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> Bool
validateMasterFundAndMerge !pParams !txID_Master_Fund_CS !ctx !redeemer !inputs_TxOut_Values_And_Datums !outputs_TxOut_Values_And_Datums  =
        OnChainHelpers.validateMasterAction pParams info master &&
        traceIfFalse "WIO" correctIO &&
        traceIfFalse "MFAM" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_FundAndMerge_AC  info ) && 

        (
            traceIfFalse "TERMINATED" (OnChainHelpers.isNotTerminated pParams info poolDatum_In) ||
            traceIfFalse "FUNDAMT0" (fundAmount == 0)
        ) &&
        
        traceIfFalse "PD" correctOutput_PoolDatum_Updated_With_NewFundAmountAndMerging &&        
        traceIfFalse "PDV" correctOutput_PoolDatum_Value_NotChanged &&        
        traceIfFalse "FD" correctOutput_FundDatum_WithNewFundAmountAndMerging && 
        traceIfFalse "FDV" correctOutput_FundDatum_Value_WithFunds  

    where
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !master = T.rmfamMaster redeemer
        !fundAmount = T.rmfamFundAmount redeemer
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        !fundID_CS = txID_Master_Fund_CS
        !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)
        ------------------
        !txID_Master_FundAndMerge_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !txID_Master_FundAndMerge_AC = LedgerValue.AssetClass (txID_Master_FundAndMerge_CS, T.txID_Master_FundAndMerge_TN)
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
        !inputs_TxOuts_Values_And_FundDatums =
            case OnChainNFTHelpers.getTxOuts_Values_And_SomeDatums fundID_AC Helpers.getFundDatumTypo_FromDatum inputs_TxOut_Values_And_Datums of
                Nothing -> traceError "IFDS"
                Just x  -> x
        ------------------
        !output_TxOut_Value_And_FundDatum =
            case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum fundID_AC Helpers.getFundDatumTypo_FromDatum outputs_TxOut_Values_And_Datums of
                Nothing -> traceError "OFD"
                Just x  -> x
        ------------------
        correctIO :: Bool
        correctIO =
            -- caseIO # : REF INPUTS , NORMAL INPUTS       / OUTPUTS
            -- caseIO 1 : 0 R        , 2+ (1 PD, 1+ FD)    / 2 (1 PD, 1 FD)
            (length inputs_TxOut_Values_And_Datums  == (T.const_1_PD + length inputs_TxOuts_Values_And_FundDatums) ) &&
            (length outputs_TxOut_Values_And_Datums == T.const_1_PD + T.const_1_FD                                 )
        ------------------
        !poolDatum_In = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_PoolDatum
        ------------------
        correctOutput_PoolDatum_Updated_With_NewFundAmountAndMerging :: Bool
        !correctOutput_PoolDatum_Updated_With_NewFundAmountAndMerging =
            let
                !mergingCount = length inputs_TxOuts_Values_And_FundDatums
                !poolDatum_Out_Control = Helpers.mkUpdated_PoolDatum_With_NewFundAmountAndMerging poolDatum_In master fundAmount mergingCount
                !poolDatum_Out_Real = OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_PoolDatum
            in
                poolDatum_Out_Real == poolDatum_Out_Control
        ------------------
        correctOutput_PoolDatum_Value_NotChanged :: Bool
        !correctOutput_PoolDatum_Value_NotChanged =
            let
                !value_In_PoolDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_PoolDatum
                !value_For_PoolDatum_Control = value_In_PoolDatum
                !value_For_PoolDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_PoolDatum
            in
                Helpers.valueEqualsValue value_For_PoolDatum_Real value_For_PoolDatum_Control
        ------------------
        correctOutput_FundDatum_WithNewFundAmountAndMerging :: Bool
        !correctOutput_FundDatum_WithNewFundAmountAndMerging =  
            let
                !fundDatums_In_To_Merge = OnChainNFTHelpers.getTxOut_Datum <$> inputs_TxOuts_Values_And_FundDatums
                !fundDatum_Out_Control = Helpers.mkUpdated_FundDatum_WithNewFundAmountAndMerging fundDatums_In_To_Merge fundAmount 
                !fundDatum_Out_Real = OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_FundDatum
            in
                fundDatum_Out_Real == fundDatum_Out_Control 
        ------------------
        correctOutput_FundDatum_Value_WithFunds :: Bool
        !correctOutput_FundDatum_Value_WithFunds =  
            let
                !value_In_FundDatum_To_Merge = foldl (<>) (LedgerAda.lovelaceValueOf 0) (OnChainNFTHelpers.getTxOut_Value <$> inputs_TxOuts_Values_And_FundDatums)
                !value_For_FundDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_FundDatum
            ------------------
                !value_For_Mint_TxID_Master_FundAndMerge = LedgerValue.assetClassValue txID_Master_FundAndMerge_AC 1
            ------------------
                -- !harvest_CS =  T.ppHarvest_CS pParams
                -- !harvestIsAda = LedgerApiV2.adaSymbol == harvest_CS
                -- !haverstIsWithoutTokenName = not harvestIsAda && T.ppHarvest_TN pParams == LedgerApiV2.TokenName emptyByteString
            --  in
                -- if haverstIsWithoutTokenName then
                --     let
                --         !value_In_FundDatum_To_Merge_FromCurrencySymbol = Helpers.getValueOfCurrencySymbol value_In_FundDatum_To_Merge harvest_CS
                --         !value_For_FundDatum_Real_FromCurrencySymbol = Helpers.getValueOfCurrencySymbol value_For_FundDatum_Real harvest_CS
                --     ------------------
                --         !value_FundAmount = value_For_FundDatum_Real_FromCurrencySymbol <> negate value_In_FundDatum_To_Merge_FromCurrencySymbol
                --         !fundAmount' = Helpers.getAmtOfCurrencySymbol value_FundAmount harvest_CS
                --     ------------------
                --         !value_For_FundDatum_Control = value_FundAmount <> value_In_FundDatum_To_Merge <> value_For_Mint_TxID_Master_FundAndMerge 
                --     in
                --         fundAmount == fundAmount' && value_For_FundDatum_Real == value_For_FundDatum_Control
                -- else
                -- let
                !harvest_AC = LedgerValue.AssetClass (T.ppHarvest_CS pParams, T.ppHarvest_TN pParams)
                !value_FundAmount = LedgerValue.assetClassValue harvest_AC fundAmount
            ------------------
                !value_For_FundDatum_Control = value_FundAmount <> value_In_FundDatum_To_Merge <> value_For_Mint_TxID_Master_FundAndMerge  
            in
                Helpers.valueEqualsValue value_For_FundDatum_Real value_For_FundDatum_Control

--------------------------------------------------------------------------------

{-# INLINEABLE policy_TxID_Master_FundAndMerge #-}
policy_TxID_Master_FundAndMerge :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.MintingPolicy
policy_TxID_Master_FundAndMerge pParams txID_Master_Fund_CS = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams txID_Master_Fund_CS

{-# INLINEABLE original_policy #-}
original_policy :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> Plutonomy.MintingPolicy
original_policy pParams txID_Master_Fund_CS =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [||mkPolicy_TxID_Master_FundAndMerge ||])
    `PlutusTx.applyCode` PlutusTx.liftCode pParams
    `PlutusTx.applyCode` PlutusTx.liftCode txID_Master_Fund_CS

--------------------------------------------------------------------------------
