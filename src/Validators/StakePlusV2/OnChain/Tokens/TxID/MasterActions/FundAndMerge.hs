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
{- HLINT ignore "Use camelCase" -}
------------------------------------------------------------------------------------------
module Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.FundAndMerge
(
    policy_TxID_Master_FundAndMerge -- , curSymbol_TxID_Master_FundAndMerge
)
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Ada                                                 as LedgerAda
import qualified Ledger.Value                                               as LedgerValue (assetClassValue, AssetClass (AssetClass), assetClass)
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, txOutValue, MintingPolicy, CurrencySymbol) 
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo) 
import qualified PlutusTx                                                   (compile, applyCode, liftCode)
import           PlutusTx.Prelude                                           ( Bool, Maybe(Nothing, Just), Eq((==)), BuiltinData, Semigroup((<>)), (&&), error, traceError, ($), foldl, length, (<$>), traceIfFalse, (||) )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (fromJust, mkUpdated_PoolDatum_With_NewFundAmountAndMerging, mkUpdated_FundDatum_WithNewFundAmountAndMerging) 
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (getInputsWithDatum, getOutputsWithDatum, validateMasterAction, isNFT_Minted_With_AC, isNotTerminated) 
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (getTxOut_Datum, getTxOut_Value, getTxOut_Value_And_PoolDatum, getTxOuts_Values_And_FundDatums, validateBurn_Token_Own_CS_Any_TN, checkIfAllAreFromSameAddress, checkIfAllSpendRedeemersAreEqual) 
import qualified Validators.StakePlusV2.Types.Constants                     as T (poolID_TN, fundID_TN, txID_Master_FundAndMerge_TN)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, DatumValidator (PoolDatum, FundDatum))
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerMasterFundAndMergeTypo (..), RedeemerValidator (RedeemerMasterFundAndMerge))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_Master_FundAndMerge #-}
mkPolicy_TxID_Master_FundAndMerge :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_Master_FundAndMerge pParams curSymbol_TxID_Master_Fund mintRedeemerRaw ctxRaw  = 
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
                              validateMasterFundAndMerge pParams curSymbol_TxID_Master_Fund ctx redeemer inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
                        _ -> traceError "INVOP"-- "Minting TxID: Invalid Operation"
        then ()

        else error ()

------------------------------------------------------------------------------

{-# INLINABLE validateMasterFundAndMerge #-}
validateMasterFundAndMerge :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerContextsV2.ScriptContext -> T.RedeemerMasterFundAndMergeTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> Bool
validateMasterFundAndMerge pParams curSymbol_TxID_Master_Fund ctx redeemer inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums  =

        (
            traceIfFalse "TERMINATED" (OnChainHelpers.isNotTerminated pParams info poolDatum_In) ||
            traceIfFalse "NOTTERMINATED" (fundAmount == 0)
        ) &&
        
        -- Basic validations for Master Actions
        OnChainHelpers.validateMasterAction pParams info master &&

        -- Check if the txID_Master_FundAndMerge_AC minted is there
        traceIfFalse "MFAM" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_FundAndMerge_AC  info ) && 

        -- With new FundID and merging.
        traceIfFalse "PD" correctOutput_PoolDatum_Updated_With_NewFundAmountAndMerging && --"Wrong updated PoolDatum"

        -- Must be the same than the input.
        traceIfFalse "PDV" correctOutput_PoolDatum_Value_NotChanged && -- "Wrong PoolDatum Value."

        -- Must include all the merging FundDatum.
        traceIfFalse "FD" correctOutput_FundDatum_WithNewFundAmountAndMerging && --"Wrong New FundDatum"

        -- Must include all the merging FundDatum values plus the new fund and the FundID .
        traceIfFalse "FDV" correctOutput_FundDatum_Value_WithFunds  --"Wrong FundDatum Value"

    where

        ------------------
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !master = T.rmfamMaster redeemer
        !fundAmount = T.rmfamFundAmount redeemer
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        !fundID_CS = curSymbol_TxID_Master_Fund
        !fundID_TN = T.fundID_TN
        !fundID_AC = LedgerValue.assetClass fundID_CS fundID_TN
        ------------------
        !txID_Master_FundAndMerge_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !txID_Master_FundAndMerge_AC = LedgerValue.AssetClass (txID_Master_FundAndMerge_CS, T.txID_Master_FundAndMerge_TN)
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
        !inputs_TxOuts_Values_And_FundDatums' =
            case OnChainNFTHelpers.getTxOuts_Values_And_FundDatums fundID_AC inputs_TxOut_Values_And_Datums of
                []  -> Nothing
                x   -> Just x
        !inputs_TxOuts_Values_And_FundDatums =
            case inputs_TxOuts_Values_And_FundDatums' of
                Nothing -> traceError "IFDS" -- "Error. Can't find inputs with FundDatum"
                _       -> Helpers.fromJust inputs_TxOuts_Values_And_FundDatums'
        ------------------
        !output_TxOut_Value_And_FundDatum' =
            case OnChainNFTHelpers.getTxOuts_Values_And_FundDatums fundID_AC outputs_TxOut_Values_And_Datums of
                [x] -> Just x
                _   -> Nothing
        !output_TxOut_Value_And_FundDatum =
            case output_TxOut_Value_And_FundDatum' of
                Nothing -> traceError "OFD" -- "Error. Can't find output with FundDatum"
                _       -> Helpers.fromJust output_TxOut_Value_And_FundDatum'
        ------------------
        !poolDatum_In = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_PoolDatum
        ------------------
        !fundDatums_In_To_Merge = OnChainNFTHelpers.getTxOut_Datum <$> inputs_TxOuts_Values_And_FundDatums
        ------------------
        correctOutput_PoolDatum_Updated_With_NewFundAmountAndMerging :: Bool
        !correctOutput_PoolDatum_Updated_With_NewFundAmountAndMerging =
            let
                !mergingCount = length fundDatums_In_To_Merge
                !poolDatum_Out_Control = Helpers.mkUpdated_PoolDatum_With_NewFundAmountAndMerging poolDatum_In master fundAmount mergingCount
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
        correctOutput_FundDatum_WithNewFundAmountAndMerging :: Bool
        !correctOutput_FundDatum_WithNewFundAmountAndMerging =  
            let
                !fundDatum_Out_Control = Helpers.mkUpdated_FundDatum_WithNewFundAmountAndMerging fundDatums_In_To_Merge fundAmount 
                !fundDatum_Out_Real = T.FundDatum $ OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_FundDatum
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
                --         !value_In_FundDatum_To_Merge_FromCurrencySymbol = Helpers.valueFromCurrencySymbol value_In_FundDatum_To_Merge harvest_CS
                --         !value_For_FundDatum_Real_FromCurrencySymbol = Helpers.valueFromCurrencySymbol value_For_FundDatum_Real harvest_CS
                --     ------------------
                --         !value_FundAmount = value_For_FundDatum_Real_FromCurrencySymbol <> negate value_In_FundDatum_To_Merge_FromCurrencySymbol
                --         !fundAmount' = Helpers.valueOfCurrencySymbol value_FundAmount harvest_CS
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
                value_For_FundDatum_Real ==  value_For_FundDatum_Control

--------------------------------------------------------------------------------

{-# INLINEABLE policy_TxID_Master_FundAndMerge #-}
policy_TxID_Master_FundAndMerge :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.MintingPolicy
policy_TxID_Master_FundAndMerge pParams curSymbol_TxID_Master_Fund = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams curSymbol_TxID_Master_Fund

{-# INLINEABLE original_policy #-}
original_policy :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> Plutonomy.MintingPolicy
original_policy pParams curSymbol_TxID_Master_Fund =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [||mkPolicy_TxID_Master_FundAndMerge ||])
    `PlutusTx.applyCode` PlutusTx.liftCode pParams
    `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_Master_Fund

--------------------------------------------------------------------------------
