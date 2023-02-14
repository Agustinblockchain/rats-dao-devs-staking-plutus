{-# LANGUAGE DataKinds                  #-}
-- {-# LANGUAGE DeriveAnyClass             #-}
-- {-# LANGUAGE DeriveGeneric              #-}
-- {-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
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
module Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.DeleteFund
(
    policy_TxID_Master_DeleteFund 
)
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Ada                                                 as LedgerAda (lovelaceValueOf)
import qualified Ledger.Value                                               as LedgerValue (assetClassValue, AssetClass (AssetClass), assetClassValueOf)
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, txOutValue, MintingPolicy, CurrencySymbol) 
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo, ownCurrencySymbol) 
import qualified PlutusTx                                                   (compile, applyCode, liftCode)
import           PlutusTx.Prelude                                           ( Bool, Maybe(Nothing, Just), Eq((==)), BuiltinData, Semigroup((<>)), (&&), error, traceError, ($), foldl, length, sum, (<$>), negate, traceIfFalse, AdditiveSemigroup ((+)) )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (mkUpdated_PoolDatum_With_DeletingFunds, valueEqualsValue, getPoolDatumTypo_FromDatum, getFundDatumTypo_FromDatum)
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (getInputsWithDatum, getOutputsWithDatum, validateMasterAction, isNFT_Minted_With_AC, isToken_Minted_With_AC_AndAmt, isTerminated) 
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (getTxOut_Datum, getTxOut_Value, validateBurn_Token_Own_CS_Any_TN, checkIfAllAreFromSameAddress, checkIfAllSpendRedeemersAreEqual, getTxOut_Value_And_SomeDatum, getTxOuts_Values_And_SomeDatums) 
import qualified Validators.StakePlusV2.Types.Constants                     as T (poolID_TN, fundID_TN, txID_Master_DeleteFund_TN, const_1_PD)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, FundDatumTypo (..))
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerMasterDeleteFundTypo (..), RedeemerValidator (RedeemerMasterDeleteFund))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_Master_DeleteFund #-}
mkPolicy_TxID_Master_DeleteFund :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_Master_DeleteFund !pParams !txID_Master_Fund_CS !mintRedeemerRaw !ctxRaw  = 
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
                        (T.RedeemerMasterDeleteFund redeemer) ->
                              validateMasterDeleteFund pParams txID_Master_Fund_CS ctx redeemer inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
                        _ -> traceError "INVOP"
        then ()

        else error ()

------------------------------------------------------------------------------

{-# INLINABLE validateMasterDeleteFund #-}
validateMasterDeleteFund :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerContextsV2.ScriptContext -> T.RedeemerMasterDeleteFundTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> Bool
validateMasterDeleteFund !pParams !txID_Master_Fund_CS !ctx !redeemer !inputs_TxOut_Values_And_Datums !outputs_TxOut_Values_And_Datums  =
        OnChainHelpers.validateMasterAction pParams info master &&
        traceIfFalse "WIO" correctIO &&
        traceIfFalse "MD" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_DeleteFund_AC  info ) && 
        
        traceIfFalse "NOTTERMINATED" (OnChainHelpers.isTerminated pParams info poolDatum_In) &&
        
        traceIfFalse "BFID" isBurning_FundIDs && 

        traceIfFalse "PD" correctOutput_PoolDatum_Updated_With_DeletingFunds &&        
        traceIfFalse "PDV" correctOutput_PoolDatum_Value_WithTokens        
    where
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !master = T.rmdfMaster redeemer
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        !fundID_CS = txID_Master_Fund_CS
        !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)
        ------------------
        !txID_Master_DeleteFund_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !txID_Master_DeleteFund_AC = LedgerValue.AssetClass (txID_Master_DeleteFund_CS, T.txID_Master_DeleteFund_TN)
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
        correctIO :: Bool
        correctIO =
            -- caseIO # : REF INPUTS , NORMAL INPUTS     / OUTPUTS
            -- caseIO 1 : 0 R        , 2+ (1 PD, 1+ FD)  / 1 (1 PD)
            (length inputs_TxOut_Values_And_Datums  == (T.const_1_PD + length inputs_TxOuts_Values_And_FundDatums)) &&
            (length outputs_TxOut_Values_And_Datums == T.const_1_PD                                               )                
        ------------------
        !poolDatum_In = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_PoolDatum
        ------------------
        !mergingCount = length inputs_TxOuts_Values_And_FundDatums
        ------------------
        !value_In_FundDatum_To_Delete = foldl (<>) (LedgerAda.lovelaceValueOf 0) (OnChainNFTHelpers.getTxOut_Value <$> inputs_TxOuts_Values_And_FundDatums)
        !fundID_To_Burn_Amount = LedgerValue.assetClassValueOf value_In_FundDatum_To_Delete fundID_AC
        !isBurning_FundIDs = OnChainHelpers.isToken_Minted_With_AC_AndAmt fundID_AC (negate fundID_To_Burn_Amount) info 
        ------------------
        correctOutput_PoolDatum_Updated_With_DeletingFunds :: Bool
        !correctOutput_PoolDatum_Updated_With_DeletingFunds =
            let
                !fundDatums_In_To_Delete = OnChainNFTHelpers.getTxOut_Datum <$> inputs_TxOuts_Values_And_FundDatums
                !mergingCashedOut = sum [ T.fdCashedOut fundDatumTypo | fundDatumTypo <- fundDatums_In_To_Delete ]
                !poolDatum_Out_Control = Helpers.mkUpdated_PoolDatum_With_DeletingFunds poolDatum_In mergingCount mergingCashedOut
                !poolDatum_Out_Real = OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_PoolDatum
            in
                poolDatum_Out_Real == poolDatum_Out_Control 
        ------------------
        correctOutput_PoolDatum_Value_WithTokens :: Bool
        !correctOutput_PoolDatum_Value_WithTokens = 
            let
                !value_In_PoolDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_PoolDatum
                !value_For_Mint_TxID_Master_DeleteFund = LedgerValue.assetClassValue txID_Master_DeleteFund_AC 1
                !value_For_Burn_TxID_FundID = LedgerValue.assetClassValue fundID_AC (negate fundID_To_Burn_Amount)
            ---------------------
                !value_For_PoolDatum_Control = value_In_PoolDatum <> value_In_FundDatum_To_Delete <> value_For_Mint_TxID_Master_DeleteFund <> value_For_Burn_TxID_FundID 
                !value_For_PoolDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_PoolDatum
            in
                
                Helpers.valueEqualsValue value_For_PoolDatum_Real value_For_PoolDatum_Control 

-- --------------------------------------------------------------------------------

{-# INLINEABLE policy_TxID_Master_DeleteFund #-}
policy_TxID_Master_DeleteFund :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.MintingPolicy
policy_TxID_Master_DeleteFund pParams txID_Master_Fund_CS = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams txID_Master_Fund_CS

{-# INLINEABLE original_policy #-}
original_policy :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> Plutonomy.MintingPolicy
original_policy pParams txID_Master_Fund_CS =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [||  mkPolicy_TxID_Master_DeleteFund ||])
    `PlutusTx.applyCode` PlutusTx.liftCode pParams
    `PlutusTx.applyCode` PlutusTx.liftCode txID_Master_Fund_CS

--------------------------------------------------------------------------------

