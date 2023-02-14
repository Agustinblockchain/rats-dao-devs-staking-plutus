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
module Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.SplitFund
(
        policy_TxID_Master_SplitFund 
)
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Ada                                                 as LedgerAda (lovelaceValueOf)
import qualified Ledger.Value                                               as LedgerValue (assetClassValue, AssetClass (AssetClass))
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, txOutValue, MintingPolicy, CurrencySymbol, adaSymbol, TokenName (TokenName)) 
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo, ownCurrencySymbol) 
import qualified PlutusTx                                                   (compile, applyCode, liftCode)
import           PlutusTx.Prelude                                           ( Bool, (<), Maybe(Nothing, Just), Eq((==)), BuiltinData, Semigroup((<>)), (&&), error, traceError, ($), sortBy, negate, traceIfFalse, emptyByteString, not, length, all, any, AdditiveSemigroup ((+)) )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (getFundAmountCanUse_in_FundDatum, mkUpdated_PoolDatum_With_SplitFundAmount, mkUpdated_FundDatum_With_WithSplitFund, createValueAddingTokensOfCurrencySymbol, getFundDatumTypo_FromDatum, valueEqualsValue, getPoolDatumTypo_FromDatum)
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (getInputsWithDatum, getOutputsWithDatum, validateMasterAction, isNFT_Minted_With_AC, isNotTerminated) 
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (getTxOut_Datum, getTxOut_Value, validateBurn_Token_Own_CS_Any_TN, checkIfAllAreFromSameAddress, checkIfAllSpendRedeemersAreEqual, sort_Value_And_FundDatum, getTxOut_Value_And_SomeDatum, getTxOuts_Values_And_SomeDatums) 
import qualified Validators.StakePlusV2.Types.Constants                     as T (poolID_TN, fundID_TN, txID_Master_SplitFund_TN, const_1_PD, const_1_FD)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, mkFundDatumTypo)
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator (RedeemerMasterSplitFund), RedeemerMasterSplitFundTypo (..))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_Master_SplitFund #-}
mkPolicy_TxID_Master_SplitFund :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_Master_SplitFund !pParams !txID_Master_Fund_CS !mintRedeemerRaw !ctxRaw  = 
    let
        !mintRedeemer = LedgerApiV2.unsafeFromBuiltinData @T.Redeemer_TxID mintRedeemerRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
    in
        if 
            case mintRedeemer of
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
                                (T.RedeemerMasterSplitFund redeemer) ->
                                    validateMasterSplitFund pParams txID_Master_Fund_CS ctx redeemer inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
                                _ -> traceError "INVOP"
        then ()

        else error ()

------------------------------------------------------------------------------

{-# INLINABLE validateMasterSplitFund #-}
validateMasterSplitFund :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerContextsV2.ScriptContext -> T.RedeemerMasterSplitFundTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> Bool
validateMasterSplitFund !pParams !txID_Master_Fund_CS !ctx !redeemer !inputs_TxOut_Values_And_Datums !outputs_TxOut_Values_And_Datums  =
        OnChainHelpers.validateMasterAction pParams info master &&
        traceIfFalse "WIO" correctIO &&
        traceIfFalse "MS" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_SplitFund_AC  info ) && 
        traceIfFalse "FundID" (OnChainHelpers.isNFT_Minted_With_AC fundID_AC info ) &&
        
        traceIfFalse "TERMINATED" (OnChainHelpers.isNotTerminated pParams info poolDatum_In) &&
        
        traceIfFalse "SA" correct_SplitFundAmount &&        
        traceIfFalse "PD" correctOutput_PoolDatum_Updated_With_SplitFundAmount &&        
        traceIfFalse "PDV" correctOutput_PoolDatum_Value_NotChanged &&        
        traceIfFalse "FDOV" correctOutputs_FundsDatums_And_Values_WithSplitFund  

    where

        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !master = T.rmsfMaster redeemer
        !splitFundAmount = T.rmsfSplitFundAmount redeemer 
        !minAda_For_FundDatum_New = T.rmsfMinAda redeemer
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        !fundID_CS = txID_Master_Fund_CS
        !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)
        ------------------
        !txID_Master_SplitFund_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !txID_Master_SplitFund_AC = LedgerValue.AssetClass (txID_Master_SplitFund_CS, T.txID_Master_SplitFund_TN)
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
        !input_TxOut_Value_And_FundDatum =
                case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum fundID_AC Helpers.getFundDatumTypo_FromDatum inputs_TxOut_Values_And_Datums of
                        Nothing -> traceError "IPD"
                        Just x  -> x
        ------------------
        !outputs_TxOuts_Values_And_FundDatums =
                case OnChainNFTHelpers.getTxOuts_Values_And_SomeDatums fundID_AC Helpers.getFundDatumTypo_FromDatum outputs_TxOut_Values_And_Datums of
                        Nothing -> traceError "OFDS"
                        Just x  -> x
        ------------------
        correctIO :: Bool
        correctIO =
            -- caseIO # : REF INPUTS , NORMAL INPUTS     / OUTPUTS
            -- caseIO 1 : 0 R        , 2 (1 PD, 1 FD)    / 3 (1 PD, 2 FD)
            (length inputs_TxOut_Values_And_Datums  == T.const_1_PD + T.const_1_FD                )  &&
            (length outputs_TxOut_Values_And_Datums == T.const_1_PD + T.const_1_FD + T.const_1_FD ) &&
            (length outputs_TxOuts_Values_And_FundDatums == T.const_1_FD + T.const_1_FD           ) 
        ------------------
        !poolDatum_In = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_PoolDatum
        ------------------
        !fundDatum_In_ToSplit = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_FundDatum
        ------------------
        correct_SplitFundAmount:: Bool
        !correct_SplitFundAmount =
            let
                !maxFundAmount_ToSplit = Helpers.getFundAmountCanUse_in_FundDatum fundDatum_In_ToSplit
            in 
                splitFundAmount < maxFundAmount_ToSplit 
        ------------------
        correctOutput_PoolDatum_Updated_With_SplitFundAmount :: Bool
        !correctOutput_PoolDatum_Updated_With_SplitFundAmount =
            let
                !poolDatum_Out_Control = Helpers.mkUpdated_PoolDatum_With_SplitFundAmount poolDatum_In master minAda_For_FundDatum_New
                !poolDatum_Out_Real = OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_PoolDatum
            in
                poolDatum_Out_Real == poolDatum_Out_Control
        ------------------
        correctOutput_PoolDatum_Value_NotChanged :: Bool
        !correctOutput_PoolDatum_Value_NotChanged =
            let
                !value_In_PoolDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_PoolDatum
        ------------------
                !value_For_PoolDatum_Control = value_In_PoolDatum
                !value_For_PoolDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_PoolDatum
            in
                Helpers.valueEqualsValue value_For_PoolDatum_Real value_For_PoolDatum_Control

        ------------------
        correctOutputs_FundsDatums_And_Values_WithSplitFund :: Bool
        !correctOutputs_FundsDatums_And_Values_WithSplitFund =  
            let
                !fundDatums_Out_Real_Ordered = sortBy OnChainNFTHelpers.sort_Value_And_FundDatum outputs_TxOuts_Values_And_FundDatums
            ------------------
                !fundDatum_Split_Out = Helpers.mkUpdated_FundDatum_With_WithSplitFund fundDatum_In_ToSplit splitFundAmount
            ------------------
                !cashedOut = 0
                !fundDatum_New_Out = T.mkFundDatumTypo splitFundAmount cashedOut minAda_For_FundDatum_New
            ------------------
                !value_In_fundDatum_To_Split = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_FundDatum
            ------------------
                !value_For_Mint_FundID = LedgerValue.assetClassValue fundID_AC 1
            ------------------
                !value_For_Mint_TxID_Master_SplitFund = LedgerValue.assetClassValue txID_Master_SplitFund_AC 1
            ------------------
                !harvest_CS = T.ppHarvest_CS pParams
                !harvest_AC = LedgerValue.AssetClass (harvest_CS, T.ppHarvest_TN pParams)
                !harvestIsAda = LedgerApiV2.adaSymbol == harvest_CS
                !haverstIsWithoutTokenName = not harvestIsAda && T.ppHarvest_TN pParams == LedgerApiV2.TokenName emptyByteString
            ------------------
                !value_SplitFundAmount = Helpers.createValueAddingTokensOfCurrencySymbol harvest_AC harvest_CS haverstIsWithoutTokenName value_In_fundDatum_To_Split splitFundAmount
                !value_MinAda_For_FundDatum_New = LedgerAda.lovelaceValueOf minAda_For_FundDatum_New
                !value_For_FundDatum_New = value_SplitFundAmount <> value_For_Mint_FundID <> value_For_Mint_TxID_Master_SplitFund <> value_MinAda_For_FundDatum_New
            ------------------
                !value_For_FundDatum_Split =  value_In_fundDatum_To_Split <> negate value_SplitFundAmount
            ------------------
                !fundDatums_Out_Control =
                        [ (value_For_FundDatum_Split, fundDatum_Split_Out)
                        , (value_For_FundDatum_New,  fundDatum_New_Out)]

                !fundDatums_Out_Control_Ordered = sortBy OnChainNFTHelpers.sort_Value_And_FundDatum fundDatums_Out_Control
            ------------------
            in
                    length fundDatums_Out_Real_Ordered == length fundDatums_Out_Control_Ordered
                    &&
                    -- all (\(v, d) ->
                    --     isJust (find (\(v', d') ->   d == d' && Helpers.valueEqualsValue v v' ) fundDatums_Out_Control_Ordered)
                    -- ) fundDatums_Out_Real_Ordered
                    all (
                            \(v, d) ->
                                    any (
                                            \(v', d') ->
                                                    d == d' && Helpers.valueEqualsValue v v'
                                            ) fundDatums_Out_Control_Ordered
                    ) fundDatums_Out_Real_Ordered

--------------------------------------------------------------------------------

policy_TxID_Master_SplitFund :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.MintingPolicy
policy_TxID_Master_SplitFund pParams txID_Master_Fund_CS = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams txID_Master_Fund_CS

original_policy :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> Plutonomy.MintingPolicy
original_policy pParams txID_Master_Fund_CS =
    Plutonomy.mkMintingPolicyScript $
        $$(PlutusTx.compile [||  mkPolicy_TxID_Master_SplitFund ||])
        `PlutusTx.applyCode` PlutusTx.liftCode pParams
        `PlutusTx.applyCode` PlutusTx.liftCode txID_Master_Fund_CS

--------------------------------------------------------------------------------

