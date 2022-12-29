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
module Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.TerminatePool 
(
    policy_TxID_Master_TerminatePool 
) 
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Value                                               as LedgerValue (AssetClass (AssetClass), assetClassValue)
import qualified Plutonomy  
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, txOutValue, MintingPolicy) 
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo) 
import qualified PlutusTx                                                   (compile, applyCode, liftCode)
import           PlutusTx.Prelude                                           ( Bool, Maybe(Nothing), Eq((==)), BuiltinData, Semigroup((<>)), (&&), error, traceError, ($), traceIfFalse )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (fromJust, mkUpdated_PoolDatum_With_Terminated) 
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (getInputsWithDatum, getOutputsWithDatum, validateMasterAction, isNFT_Minted_With_AC, isNotTerminated) 
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (getTxOut_Datum, getTxOut_Value_And_PoolDatum, validateBurn_Token_Own_CS_Any_TN, getTxOut_Value, checkIfAllAreFromSameAddress, checkIfAllSpendRedeemersAreEqual) 
import qualified Validators.StakePlusV2.Types.Constants                     as T (poolID_TN, txID_Master_TerminatePool_TN)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, DatumValidator (PoolDatum))
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator (RedeemerMasterTerminatePool), RedeemerMasterTerminatePoolTypo (..))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_Master_TerminatePool #-}
mkPolicy_TxID_Master_TerminatePool :: T.PoolParams -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_Master_TerminatePool pParams mintRedeemerRaw ctxRaw  = 
   let
        !mintRedeemer = LedgerApiV2.unsafeFromBuiltinData @T.Redeemer_TxID mintRedeemerRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
    in
        if
            case mintRedeemer of
            (T.RedeemerBurn_TxID T.RedeemerBurn_TxIDTypo) -> OnChainNFTHelpers.validateBurn_Token_Own_CS_Any_TN ctx
            (T.RedeemerMint_TxID T.RedeemerMint_TxIDTypo {..}) ->
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
                        (T.RedeemerMasterTerminatePool redeemer) ->
                            validateMasterTerminatePool pParams ctx redeemer inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
                        _ -> traceError "INVOP"-- "Minting TxID: Invalid Operation"
        then ()
        else error ()

--------------------------------------------------------------------------------

{-# INLINABLE validateMasterTerminatePool #-}
validateMasterTerminatePool :: T.PoolParams -> LedgerContextsV2.ScriptContext -> T.RedeemerMasterTerminatePoolTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> Bool 
validateMasterTerminatePool pParams ctx redeemer inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums  =

        -- chekeo que no este terminated already
        traceIfFalse "TERMINATED" (OnChainHelpers.isNotTerminated pParams info poolDatum_In )  &&  

        -- Basic validations for Master Actions 
        OnChainHelpers.validateMasterAction pParams info master &&

        -- "Can't Find Valid TxID" 
        traceIfFalse "MTP" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_TerminatePool_AC  info ) &&

        -- "Wrong Updated PoolDatum"
        traceIfFalse "PD" correctOutput_PoolDatum_Updated_With_Terminated &&

        -- Must be the same than the input.
        traceIfFalse "PDV" correctOutput_PoolDatum_Value_WithTokens -- "Wrong PoolDatum Value"

    where
        
        ------------------
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !master = T.rmtpMaster redeemer
        ------------------
        !txID_Master_TerminatePool_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !txID_Master_TerminatePool_AC =  LedgerValue.AssetClass (txID_Master_TerminatePool_CS, T.txID_Master_TerminatePool_TN)
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
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
        !poolDatum_In = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_PoolDatum
        ------------------
        correctOutput_PoolDatum_Updated_With_Terminated :: Bool
        !correctOutput_PoolDatum_Updated_With_Terminated =
            let
                !poolDatum_Out_Control = Helpers.mkUpdated_PoolDatum_With_Terminated poolDatum_In 
                !poolDatum_Out_Real = T.PoolDatum $ OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_PoolDatum
            in  
                poolDatum_Out_Real == poolDatum_Out_Control
        ------------------
        correctOutput_PoolDatum_Value_WithTokens :: Bool
        !correctOutput_PoolDatum_Value_WithTokens =
            let
                !output_TxOut_PoolDatum_Value = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_PoolDatum
            ------------------
                !value_For_Mint_TxID_Master_TerminatePool = LedgerValue.assetClassValue txID_Master_TerminatePool_AC 1
            in  
                output_TxOut_PoolDatum_Value == OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_PoolDatum <> value_For_Mint_TxID_Master_TerminatePool

        ------------------

--------------------------------------------------------------------------------

{-# INLINEABLE policy_TxID_Master_TerminatePool #-}
policy_TxID_Master_TerminatePool :: T.PoolParams -> LedgerApiV2.MintingPolicy
policy_TxID_Master_TerminatePool pParams = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams

{-# INLINEABLE original_policy #-}
original_policy :: T.PoolParams -> Plutonomy.MintingPolicy
original_policy pParams =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkPolicy_TxID_Master_TerminatePool ||])
    `PlutusTx.applyCode` PlutusTx.liftCode pParams

--------------------------------------------------------------------------------
