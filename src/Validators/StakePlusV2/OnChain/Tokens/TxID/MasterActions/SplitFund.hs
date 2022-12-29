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
import qualified Ledger.Value                                               as LedgerValue (assetClassValue, AssetClass (AssetClass), assetClass)
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, txOutValue, MintingPolicy, CurrencySymbol, adaSymbol, TokenName (TokenName)) 
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo, ownCurrencySymbol) 
import qualified PlutusTx                                                   (compile, applyCode, liftCode)
import           PlutusTx.Prelude                                           ( Bool, (<), Maybe(Nothing, Just), Eq((==)), BuiltinData, Semigroup((<>)), (&&), error, traceError, ($), sortBy, negate, traceIfFalse, emptyByteString, not )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (getFundAmountCanUse_in_FundDatum, fromJust, mkUpdated_PoolDatum_With_SplitFundAmount, mkUpdated_FundDatum_With_WithSplitFund, createValueAddingTokensOfCurrencySymbol, getFundDatumTypo_FromDatum)
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (getInputsWithDatum, getOutputsWithDatum, validateMasterAction, isNFT_Minted_With_AC, isNotTerminated) 
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (getTxOuts_Values_And_FundDatums, getTxOut_Datum, getTxOut_Value, getTxOut_Value_And_PoolDatum, validateBurn_Token_Own_CS_Any_TN, checkIfAllAreFromSameAddress, checkIfAllSpendRedeemersAreEqual, sort_Value_And_FundDatum) 
import qualified Validators.StakePlusV2.Types.Constants                     as T (poolID_TN, fundID_TN, txID_Master_SplitFund_TN)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, DatumValidator (PoolDatum), mkFundDatum)
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator (RedeemerMasterSplitFund), RedeemerMasterSplitFundTypo (..))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_Master_SplitFund #-}
mkPolicy_TxID_Master_SplitFund :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_Master_SplitFund pParams curSymbol_TxID_Master_Fund mintRedeemerRaw ctxRaw  = 
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
                      (T.RedeemerMasterSplitFund redeemer) ->
                            validateMasterSplitFund pParams curSymbol_TxID_Master_Fund ctx redeemer inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
                      _ -> traceError "INVOP"-- "Minting TxID: Invalid Operation"
      then ()

      else error ()

------------------------------------------------------------------------------

{-# INLINABLE validateMasterSplitFund #-}
validateMasterSplitFund :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerContextsV2.ScriptContext -> T.RedeemerMasterSplitFundTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> Bool
validateMasterSplitFund pParams curSymbol_TxID_Master_Fund ctx redeemer inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums  =

        traceIfFalse "TERMINATED" (OnChainHelpers.isNotTerminated pParams info poolDatum_In) &&
        
        -- Basic validations for Master Actions
        OnChainHelpers.validateMasterAction pParams info master &&

        -- Check if the txID_Master_SplitFund_AC minted is there
        traceIfFalse "MS" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_SplitFund_AC  info ) && 

        traceIfFalse "SA" correct_SplitFundAmount && --"Wrong updated PoolDatum"

        -- With new FundID and merging.
        traceIfFalse "PD" correctOutput_PoolDatum_Updated_With_SplitFundAmount && --"Wrong updated PoolDatum"

        -- Must be the same than the input.
        traceIfFalse "PDV" correctOutput_PoolDatum_Value_NotChanged && -- "Wrong PoolDatum Value."

        traceIfFalse "FDOV" correctOutputs_FundsDatums_And_Values_WithSplitFund  --"Wrong New FundDatum"

    where

        ------------------
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !master = T.rmsfMaster redeemer
        !splitFundAmount = T.rmsfSplitFundAmount redeemer 
        !minAda_For_FundDatum_New = T.rmsfMinAda redeemer
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        !fundID_CS = curSymbol_TxID_Master_Fund
        !fundID_TN = T.fundID_TN
        !fundID_AC = LedgerValue.assetClass fundID_CS fundID_TN
        ------------------
        !txID_Master_SplitFund_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !txID_Master_SplitFund_AC = LedgerValue.AssetClass (txID_Master_SplitFund_CS, T.txID_Master_SplitFund_TN)
        ------------------
        !input_TxOut_Value_And_PoolDatum' = OnChainNFTHelpers.getTxOut_Value_And_PoolDatum poolID_AC inputs_TxOut_Values_And_Datums
        !input_TxOut_Value_And_PoolDatum =
            case input_TxOut_Value_And_PoolDatum' of
                Nothing -> traceError "IPD" -- "Error. Can't find input with PoolDatum"
                _       -> Helpers.fromJust input_TxOut_Value_And_PoolDatum'
        ------------------
        !output_TxOut_Value_And_PoolDatum' = OnChainNFTHelpers.getTxOut_Value_And_PoolDatum poolID_AC outputs_TxOut_Values_And_Datums
        !output_TxOut_Value_And_PoolDatum =
            case output_TxOut_Value_And_PoolDatum' of
                Nothing -> traceError "OPD" -- "Error. Can't find output with PoolDatum"
                _       -> Helpers.fromJust output_TxOut_Value_And_PoolDatum'
        ------------------
        !input_TxOut_Value_And_FundDatum' =
            case OnChainNFTHelpers.getTxOuts_Values_And_FundDatums fundID_AC inputs_TxOut_Values_And_Datums of
                [x] -> Just x
                _   -> Nothing
        !input_TxOut_Value_And_FundDatum =
            case input_TxOut_Value_And_FundDatum' of
                Nothing -> traceError "IFD" -- "Error. Can't find inputs with FundDatum"
                _       -> Helpers.fromJust input_TxOut_Value_And_FundDatum'
        ------------------
        !outputs_TxOuts_Values_And_FundDatums' =
            case OnChainNFTHelpers.getTxOuts_Values_And_FundDatums fundID_AC outputs_TxOut_Values_And_Datums of
                []  -> Nothing
                x   -> Just x
        !outputs_TxOuts_Values_And_FundDatums =
            case outputs_TxOuts_Values_And_FundDatums' of
                Nothing -> traceError "OFDS" -- "Error. Can't find outputs with FundDatum"
                _       -> Helpers.fromJust outputs_TxOuts_Values_And_FundDatums'
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
                !poolDatum_Out_Real = T.PoolDatum $ OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_PoolDatum
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
                value_For_PoolDatum_Real == value_For_PoolDatum_Control
        ------------------
        correctOutputs_FundsDatums_And_Values_WithSplitFund :: Bool
        !correctOutputs_FundsDatums_And_Values_WithSplitFund =  
            let
              !fundDatums_Out_Real_Ordered = sortBy OnChainNFTHelpers.sort_Value_And_FundDatum outputs_TxOuts_Values_And_FundDatums
            ------------------
              !fundDatum_Split_Out = Helpers.mkUpdated_FundDatum_With_WithSplitFund fundDatum_In_ToSplit splitFundAmount
            ------------------
              !cashedOut = 0
              !fundDatum_New_Out = T.mkFundDatum splitFundAmount cashedOut minAda_For_FundDatum_New
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
                  [ (value_For_FundDatum_Split, Helpers.getFundDatumTypo_FromDatum fundDatum_Split_Out)
                  , (value_For_FundDatum_New,  Helpers.getFundDatumTypo_FromDatum fundDatum_New_Out)]

              !fundDatums_Out_Control_Ordered = sortBy OnChainNFTHelpers.sort_Value_And_FundDatum fundDatums_Out_Control
            ------------------
            in
                fundDatums_Out_Real_Ordered  == fundDatums_Out_Control_Ordered 

--------------------------------------------------------------------------------

policy_TxID_Master_SplitFund :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.MintingPolicy
policy_TxID_Master_SplitFund pParams curSymbol_TxID_Master_Fund = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams curSymbol_TxID_Master_Fund

original_policy :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> Plutonomy.MintingPolicy
original_policy pParams curSymbol_TxID_Master_Fund =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [||  mkPolicy_TxID_Master_SplitFund ||])
    `PlutusTx.applyCode` PlutusTx.liftCode pParams
    `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_Master_Fund

--------------------------------------------------------------------------------

