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
module Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.DeleteFund
(
    policy_TxID_Master_DeleteFund 
)
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Ada                                                 as LedgerAda (lovelaceValueOf)
import qualified Ledger.Value                                               as LedgerValue (assetClassValue, AssetClass (AssetClass), assetClass, assetClassValueOf)
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, txOutValue, MintingPolicy, CurrencySymbol) 
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo, ownCurrencySymbol) 
import qualified PlutusTx                                                   (compile, applyCode, liftCode)
import           PlutusTx.Prelude                                           ( Bool, Maybe(Nothing, Just), Eq((==)), BuiltinData, Semigroup((<>)), (&&), error, traceError, ($), foldl, length, sum, (<$>), negate, traceIfFalse )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (fromJust, mkUpdated_PoolDatum_With_DeletingFunds)
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (getInputsWithDatum, getOutputsWithDatum, validateMasterAction, isNFT_Minted_With_AC, isToken_Minted_With_AC_AndAmt, isTerminated) 
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (getTxOuts_Values_And_FundDatums, getTxOut_Datum, getTxOut_Value, getTxOut_Value_And_PoolDatum, validateBurn_Token_Own_CS_Any_TN, checkIfAllAreFromSameAddress, checkIfAllSpendRedeemersAreEqual) 
import qualified Validators.StakePlusV2.Types.Constants                     as T (poolID_TN, fundID_TN, txID_Master_DeleteFund_TN)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, FundDatumTypo (..), DatumValidator (PoolDatum))
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerMasterDeleteFundTypo (..), RedeemerValidator (RedeemerMasterDeleteFund))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_Master_DeleteFund #-}
mkPolicy_TxID_Master_DeleteFund :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_Master_DeleteFund pParams curSymbol_TxID_Master_Fund mintRedeemerRaw ctxRaw  = 
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
                              validateMasterDeleteFund pParams curSymbol_TxID_Master_Fund ctx redeemer inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
                        _ -> traceError "INVOP"-- "Minting TxID: Invalid Operation"
        then ()

        else error ()

------------------------------------------------------------------------------

{-# INLINABLE validateMasterDeleteFund #-}
validateMasterDeleteFund :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerContextsV2.ScriptContext -> T.RedeemerMasterDeleteFundTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> Bool
validateMasterDeleteFund pParams curSymbol_TxID_Master_Fund ctx redeemer inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums  =

        traceIfFalse "NOTTERMINATED" (OnChainHelpers.isTerminated pParams info poolDatum_In) &&
        
        -- Basic validations for Master Actions
        OnChainHelpers.validateMasterAction pParams info master &&

        -- Check if the txID_Master_DeleteFund_AC minted is there
        traceIfFalse "MD" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_DeleteFund_AC  info ) && 

        traceIfFalse "PD" correctOutput_PoolDatum_Updated_With_DeletingFunds && --"Wrong updated PoolDatum"

        -- Must be the same than the input.
        traceIfFalse "PDV" correctOutput_PoolDatum_Value_WithTokens -- "Wrong PoolDatum Value."

    where

        ------------------
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !master = T.rmdfMaster redeemer
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        !fundID_CS = curSymbol_TxID_Master_Fund
        !fundID_TN = T.fundID_TN
        !fundID_AC = LedgerValue.assetClass fundID_CS fundID_TN
        ------------------
        !txID_Master_DeleteFund_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !txID_Master_DeleteFund_AC = LedgerValue.AssetClass (txID_Master_DeleteFund_CS, T.txID_Master_DeleteFund_TN)
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
        !poolDatum_In = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_PoolDatum
        ------------------
        !mergingCount = length inputs_TxOuts_Values_And_FundDatums
        ------------------
        correctOutput_PoolDatum_Updated_With_DeletingFunds :: Bool
        !correctOutput_PoolDatum_Updated_With_DeletingFunds =
            let
                !fundDatums_In_To_Delete = OnChainNFTHelpers.getTxOut_Datum <$> inputs_TxOuts_Values_And_FundDatums
                !mergingCashedOut = sum [ T.fdCashedOut fundDatumTypo | fundDatumTypo <- fundDatums_In_To_Delete ]
            ---------------------
                !poolDatum_Out_Control = Helpers.mkUpdated_PoolDatum_With_DeletingFunds poolDatum_In mergingCount mergingCashedOut
            ---------------------
                !poolDatum_Out_Real = T.PoolDatum $ OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_PoolDatum
            ---------------------
            in
                poolDatum_Out_Real == poolDatum_Out_Control 
        ------------------

        correctOutput_PoolDatum_Value_WithTokens :: Bool
        !correctOutput_PoolDatum_Value_WithTokens = 
            
            let
                !value_In_PoolDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_PoolDatum
                !value_In_FundDatum_To_Delete = foldl (<>) (LedgerAda.lovelaceValueOf 0) (OnChainNFTHelpers.getTxOut_Value <$> inputs_TxOuts_Values_And_FundDatums)
            ---------------------
                !value_For_Mint_TxID_Master_DeleteFund = LedgerValue.assetClassValue txID_Master_DeleteFund_AC 1
            ---------------------
                !fundID_To_Burn_Amount = LedgerValue.assetClassValueOf value_In_FundDatum_To_Delete fundID_AC
                !value_For_Burn_TxID_FundID = LedgerValue.assetClassValue fundID_AC (negate fundID_To_Burn_Amount)
            ---------------------
                !value_For_PoolDatum_Control = value_In_PoolDatum <> value_In_FundDatum_To_Delete <> value_For_Mint_TxID_Master_DeleteFund <> value_For_Burn_TxID_FundID 
                !value_For_PoolDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_PoolDatum
            ---------------------
                !isTokensBurning = OnChainHelpers.isToken_Minted_With_AC_AndAmt fundID_AC (negate fundID_To_Burn_Amount) info 
            in
                value_For_PoolDatum_Real == value_For_PoolDatum_Control && isTokensBurning

-- --------------------------------------------------------------------------------

{-# INLINEABLE policy_TxID_Master_DeleteFund #-}
policy_TxID_Master_DeleteFund :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.MintingPolicy
policy_TxID_Master_DeleteFund pParams curSymbol_TxID_Master_Fund = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams curSymbol_TxID_Master_Fund

{-# INLINEABLE original_policy #-}
original_policy :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> Plutonomy.MintingPolicy
original_policy pParams curSymbol_TxID_Master_Fund =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [||  mkPolicy_TxID_Master_DeleteFund ||])
    `PlutusTx.applyCode` PlutusTx.liftCode pParams
    `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_Master_Fund

--------------------------------------------------------------------------------

