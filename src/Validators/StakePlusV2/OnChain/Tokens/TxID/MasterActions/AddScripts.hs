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
module Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.AddScripts
(
    policy_TxID_Master_AddScripts,
)
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Value                                               as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, MintingPolicy, TxOut (txOutValue))
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ScriptContext, TxInfo, scriptContextTxInfo, ownCurrencySymbol)
import qualified PlutusTx                                                   (compile, liftCode, applyCode)
import           PlutusTx.Prelude                                           ( Bool, BuiltinData, (&&), error, traceError, ($), traceIfFalse, (||), (++), Maybe (Nothing, Just), any, null, length, Eq ((==)), AdditiveSemigroup ((+)), (<$>), all )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (validateMasterAction, isNFT_Minted_With_AC, isToken_Minted_With_AC_AndAmt, getInputsWithDatum, getReferenceInputsWithDatum, getOutputsWithDatum)
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (validateBurn_Token_Own_CS_Any_TN, checkIfAllAreFromSameAddress, getTxOuts_Values_And_SomeDatums, getTxOut_Datum, checkIfSpendRedeemersIsEmpty)
import qualified Validators.StakePlusV2.Types.Constants                     as T (scriptID_TN, scriptID_Master_Fund_TN, scriptID_Master_FundAndMerge_TN, scriptID_Master_SplitFund_TN, scriptID_Master_ClosePool_TN, scriptID_Master_TerminatePool_TN, scriptID_Master_Emergency_TN, scriptID_Master_DeleteFund_TN, scriptID_Master_SendBackFund_TN, scriptID_Master_SendBackDeposit_TN, scriptID_Master_AddScripts_TN, scriptID_Master_DeleteScripts_TN, scriptID_User_Deposit_TN, scriptID_User_Harvest_TN, scriptID_User_Withdraw_TN, scriptID_Validator_TN, poolID_TN, const_1_PD)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(mrRedeemerValidator, RedeemerMint_TxIDTypo))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator(RedeemerMasterAddScripts), RedeemerMasterAddScriptsTypo (rmasMaster))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_Master_AddScripts #-}
mkPolicy_TxID_Master_AddScripts :: T.PoolParams -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_Master_AddScripts !pParams !mintRedeemerRaw !ctxRaw  =
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
                        !inputsReference_WithDatum = OnChainHelpers.getReferenceInputsWithDatum ctx
                        !inputs_Normal_And_Refs = inputs_WithDatum ++ inputsReference_WithDatum
                        !outputs_WithDatum = OnChainHelpers.getOutputsWithDatum ctx
                        !inputs_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- inputs_WithDatum ]
                        !inputsReference_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- inputsReference_WithDatum]
                        !outputs_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- outputs_WithDatum ]
                    in
                        traceIfFalse "INVIO" (OnChainNFTHelpers.checkIfAllAreFromSameAddress inputs_Normal_And_Refs outputs_WithDatum) &&
                        traceIfFalse "INVR" (OnChainNFTHelpers.checkIfSpendRedeemersIsEmpty ctx) && 

                        case redeemer' of
                            (T.RedeemerMasterAddScripts redeemer) ->
                                validateMasterAddScripts pParams ctx redeemer inputs_TxOut_Values_And_Datums inputsReference_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
                            _ -> traceError "INVOP"
        then ()

        else error ()

--------------------------------------------------------------------------------

{-# INLINABLE validateMasterAddScripts #-}
validateMasterAddScripts :: T.PoolParams -> LedgerContextsV2.ScriptContext -> T.RedeemerMasterAddScriptsTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> Bool
validateMasterAddScripts !pParams !ctx !redeemer !inputs_TxOut_Values_And_Datums !inputsReference_TxOut_Values_And_Datums !outputs_TxOut_Values_And_Datums =
        OnChainHelpers.validateMasterAction pParams info master &&
        traceIfFalse "WIO" correctIO &&

        (
            traceIfFalse "ScriptID" (OnChainHelpers.isToken_Minted_With_AC_AndAmt scriptID_AC countScriptDatum info ) &&
            traceIfFalse "ScriptID2" (
                OnChainHelpers.isNFT_Minted_With_AC scriptID_Validator_AC info ||
                OnChainHelpers.isNFT_Minted_With_AC scriptID_Master_Fund_AC info ||
                OnChainHelpers.isNFT_Minted_With_AC scriptID_Master_FundAndMerge_AC info ||
                OnChainHelpers.isNFT_Minted_With_AC scriptID_Master_SplitFund_AC info ||
                OnChainHelpers.isNFT_Minted_With_AC scriptID_Master_ClosePool_AC info ||
                OnChainHelpers.isNFT_Minted_With_AC scriptID_Master_TerminatePool_AC info ||
                OnChainHelpers.isNFT_Minted_With_AC scriptID_Master_Emergency_AC info ||
                OnChainHelpers.isNFT_Minted_With_AC scriptID_Master_DeleteFund_AC info ||
                OnChainHelpers.isNFT_Minted_With_AC scriptID_Master_SendBackFund_AC info ||
                OnChainHelpers.isNFT_Minted_With_AC scriptID_Master_SendBackDeposit_AC info ||
                OnChainHelpers.isNFT_Minted_With_AC scriptID_Master_AddScripts_AC info ||
                OnChainHelpers.isNFT_Minted_With_AC scriptID_Master_DeleteScripts_AC info ||
                OnChainHelpers.isNFT_Minted_With_AC scriptID_User_Deposit_AC info ||
                OnChainHelpers.isNFT_Minted_With_AC scriptID_User_Harvest_AC info ||
                OnChainHelpers.isNFT_Minted_With_AC scriptID_User_Withdraw_AC info
            )
        ) &&

        traceIfFalse "SD" correctOutputs_ScriptDatums_New

    where

        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !master = T.rmasMaster redeemer
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        !scriptID_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !scriptID_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_TN)
        ------------------
        !scriptID_Validator_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Validator_TN)
        ------------------
        !scriptID_Master_Fund_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_Fund_TN)
        !scriptID_Master_FundAndMerge_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_FundAndMerge_TN)
        !scriptID_Master_SplitFund_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_SplitFund_TN )
        !scriptID_Master_ClosePool_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_ClosePool_TN)
        !scriptID_Master_TerminatePool_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_TerminatePool_TN )
        !scriptID_Master_Emergency_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_Emergency_TN )
        !scriptID_Master_DeleteFund_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_DeleteFund_TN)
        !scriptID_Master_SendBackFund_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_SendBackFund_TN)
        !scriptID_Master_SendBackDeposit_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_SendBackDeposit_TN)
        !scriptID_Master_AddScripts_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_AddScripts_TN)
        !scriptID_Master_DeleteScripts_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_DeleteScripts_TN)
        !scriptID_User_Deposit_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_User_Deposit_TN)
        !scriptID_User_Harvest_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_User_Harvest_TN)
        !scriptID_User_Withdraw_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_User_Withdraw_TN)
        ------------------
        !outputs_TxOuts_Values_And_ScriptDatums =
            case OnChainNFTHelpers.getTxOuts_Values_And_SomeDatums scriptID_AC Helpers.getScriptDatumTypo_FromDatum outputs_TxOut_Values_And_Datums of
                Nothing -> traceError "OSDS"
                Just x  -> x
        ------------------
        !inputPDRef = any  (\(v, _) -> Helpers.isNFT_With_AC_InValue v poolID_AC ) inputsReference_TxOut_Values_And_Datums
        !outputPD = any  (\(v, _) -> Helpers.isNFT_With_AC_InValue v poolID_AC ) outputs_TxOut_Values_And_Datums
        ------------------
        !countScriptDatum = length outputs_TxOuts_Values_And_ScriptDatums
        ------------------
        correctIO :: Bool
        !correctIO =
            -- caseIO # : REF INPUTS , NORMAL INPUTS      / OUTPUTS
            -- caseIO 1 : 0 R        , 0                  / 2+ (1 PD, 1+ SD) 
            -- caseIO 2 : 1 R (1 PD) , 0                  / 1+ (1+ SD) 
            null inputs_TxOut_Values_And_Datums &&
            (   
                (inputPDRef && (length outputs_TxOut_Values_And_Datums == length outputs_TxOuts_Values_And_ScriptDatums ) ) ||
                (outputPD && (length outputs_TxOut_Values_And_Datums == length outputs_TxOuts_Values_And_ScriptDatums + T.const_1_PD  ) )
            )
            
        ------------------
        correctOutputs_ScriptDatums_New :: Bool
        !correctOutputs_ScriptDatums_New =
            let
                !scriptDatum_Out_Control = T.ScriptDatumTypo {T.sdMaster = master}
                !scriptDatums_Out_Real = OnChainNFTHelpers.getTxOut_Datum <$> outputs_TxOuts_Values_And_ScriptDatums
            in
                all (== scriptDatum_Out_Control) scriptDatums_Out_Real

--------------------------------------------------------------------------------

{-# INLINEABLE policy_TxID_Master_AddScripts #-}
policy_TxID_Master_AddScripts :: T.PoolParams -> LedgerApiV2.MintingPolicy
policy_TxID_Master_AddScripts pParams = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams

{-# INLINEABLE original_policy #-}
original_policy :: T.PoolParams -> Plutonomy.MintingPolicy
original_policy pParams =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkPolicy_TxID_Master_AddScripts ||])
    `PlutusTx.applyCode` PlutusTx.liftCode pParams

--------------------------------------------------------------------------------
