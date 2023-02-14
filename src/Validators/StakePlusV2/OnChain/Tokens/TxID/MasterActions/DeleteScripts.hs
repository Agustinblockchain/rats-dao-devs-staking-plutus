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
module Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.DeleteScripts
(
    policy_TxID_Master_DeleteScripts
) 
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Ada                                                 as LedgerAda (lovelaceValueOf)
import qualified Ledger.Value                                               as LedgerValue (AssetClass (AssetClass), assetClassValue, assetClassValueOf)
import qualified Plutonomy  
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, MintingPolicy, TxOut (txOutValue), CurrencySymbol, Value)
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo, valuePaidTo) 
import qualified PlutusTx                                                   (compile, liftCode, applyCode)
import           PlutusTx.Prelude                                           ( Bool, BuiltinData, (&&), error, traceError, ($), traceIfFalse, Maybe (Nothing, Just), Eq ((==)), Semigroup ((<>)), filter, (/=), find, all, length, negate, AdditiveSemigroup ((+)), (<$>), foldl )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (valueIncludesValue, getValueOfCurrencySymbol, valueEqualsValue, getPoolDatumTypo_FromDatum, getScriptDatumTypo_FromDatum)
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (validateMasterAction, isNFT_Minted_With_AC, getInputsWithDatum, getOutputsWithDatum, isToken_Minted_With_AC_AndAmt)
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (validateBurn_Token_Own_CS_Any_TN, checkIfAllAreFromSameAddress, getTxOut_Datum, getTxOut_Value, checkIfAllSpendRedeemersAreEqual, getTxOut_Value_And_SomeDatum, getTxOuts_Values_And_SomeDatums)
import qualified Validators.StakePlusV2.Types.Constants                     as T (txID_Master_DeleteScripts_TN, poolID_TN, scriptID_TN, const_1_PD)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, ScriptDatumTypo (sdMaster))
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator (RedeemerMasterDeleteScripts), RedeemerMasterDeleteScriptsTypo (..))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (ppPoolID_CS), Master)
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_Master_DeleteScripts #-}
mkPolicy_TxID_Master_DeleteScripts :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_Master_DeleteScripts !pParams !txID_Master_AddScripts_CS !mintRedeemerRaw !ctxRaw  = 
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
                            (T.RedeemerMasterDeleteScripts redeemer) -> 
                                validateMasterDeleteScripts pParams txID_Master_AddScripts_CS ctx redeemer inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums 
                            _ -> traceError "INVOP"
        then () 

        else error ()

--------------------------------------------------------------------------------

{-# INLINABLE validateMasterDeleteScripts #-}
validateMasterDeleteScripts :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerContextsV2.ScriptContext -> T.RedeemerMasterDeleteScriptsTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> Bool
validateMasterDeleteScripts pParams txID_Master_AddScripts_CS ctx redeemer inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums  =
        OnChainHelpers.validateMasterAction pParams info master &&
        traceIfFalse "WIO" correctIO &&
        traceIfFalse "MDS" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_DeleteScripts_AC  info )  &&

        -- traceIfFalse "NOTTERMINATED" (OnChainHelpers.isTerminated pParams info poolDatum_In) &&

        traceIfFalse "BSID" isBurning_ScriptIDs && 

        traceIfFalse "MSBFV" correctFundAmount_SendBackToMaster && 
        traceIfFalse "PD" correctOutput_PoolDatum_NotChanged &&
        traceIfFalse "PDV" correctOutput_PoolDatum_Value_WithTokens

    where
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !master = T.rmdsMaster redeemer
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        !scriptID_CS = txID_Master_AddScripts_CS
        !scriptID_AC = LedgerValue.AssetClass (scriptID_CS, T.scriptID_TN)
        ------------------
        !txID_Master_DeleteScripts_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !txID_Master_DeleteScripts_AC =  LedgerValue.AssetClass (txID_Master_DeleteScripts_CS, T.txID_Master_DeleteScripts_TN)
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
        !inputs_TxOuts_Values_And_ScriptDatums =
            case OnChainNFTHelpers.getTxOuts_Values_And_SomeDatums scriptID_AC Helpers.getScriptDatumTypo_FromDatum inputs_TxOut_Values_And_Datums of
                Nothing -> traceError "ISDS"
                Just x  -> x
        ------------------
        correctIO :: Bool
        correctIO =
            -- caseIO # : REF INPUTS , NORMAL INPUTS     / OUTPUTS
            -- caseIO 1 : 0 R        , 2+ (1 PD, 1+ SD)  / 1 (1 PD)
            (length inputs_TxOut_Values_And_Datums  == (T.const_1_PD + length inputs_TxOuts_Values_And_ScriptDatums)) &&
            (length outputs_TxOut_Values_And_Datums == T.const_1_PD                                               )
        ------------------
        !poolDatum_In = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_PoolDatum
        ------------------
        !value_In_ScriptDatum_To_Delete = foldl (<>) (LedgerAda.lovelaceValueOf 0) (OnChainNFTHelpers.getTxOut_Value <$> inputs_TxOuts_Values_And_ScriptDatums)
        !scriptID_To_Burn_Amount = LedgerValue.assetClassValueOf value_In_ScriptDatum_To_Delete scriptID_AC
        !isBurning_ScriptIDs = OnChainHelpers.isToken_Minted_With_AC_AndAmt scriptID_AC (negate scriptID_To_Burn_Amount) info 
        ------------------
        correctFundAmount_SendBackToMaster :: Bool
        !correctFundAmount_SendBackToMaster = 
            let 
            ------------------
                joinSameMaster :: [(T.Master, LedgerApiV2.Value)] -> [(T.Master, LedgerApiV2.Value, LedgerApiV2.Value)]
                joinSameMaster list = joinSameMasterHelper [] list
                    where 
            ------------------
                        joinSameMasterHelper :: [(T.Master, LedgerApiV2.Value, LedgerApiV2.Value)] -> [(T.Master, LedgerApiV2.Value)] -> [(T.Master, LedgerApiV2.Value, LedgerApiV2.Value)]
                        joinSameMasterHelper seen [] = seen
                        joinSameMasterHelper seen ((master_To_SendBack, value_In_ScriptDatum):xs) =
                            let
                                !master' = find (\(m, _, _) -> m == master_To_SendBack) seen
                            in
                                case master' of
                                    Nothing -> 
                                        let 
                                            !value_For_Master_Real = LedgerContextsV2.valuePaidTo info master_To_SendBack
                                            !elemet = (master_To_SendBack, value_In_ScriptDatum, value_For_Master_Real)
                                        in 
                                            joinSameMasterHelper (elemet:seen) xs
                                    Just (_, v1, v2) -> 
                                        let 
                                            !elemet = (master_To_SendBack, v1 <> value_In_ScriptDatum, v2)
                                            !seen_filter = filter (\(m', _, _) -> m' /= master_To_SendBack) seen
                                        in
                                            joinSameMasterHelper (elemet:seen_filter) xs

            ------------------
                !values_For_Each_Master = [ 
                        let 
                            !scriptDatum_In = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_ScriptDatum
                            !master_To_SendBack = T.sdMaster scriptDatum_In
                            !value_In_ScriptDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_ScriptDatum
                            !value_For_Burn  = negate $ Helpers.getValueOfCurrencySymbol value_In_ScriptDatum scriptID_CS
                            !value_For_Master = value_In_ScriptDatum <> value_For_Burn
                        in
                            (master_To_SendBack, value_For_Master)
                        | input_TxOut_Value_And_ScriptDatum <- inputs_TxOuts_Values_And_ScriptDatums
                    ]
            ------------------
                !values_For_Each_Master_Accumulated = joinSameMaster values_For_Each_Master
            in
                all (\(_, v1, v2) -> Helpers.valueIncludesValue v2 v1) values_For_Each_Master_Accumulated
        ------------------
        correctOutput_PoolDatum_NotChanged :: Bool
        !correctOutput_PoolDatum_NotChanged =
            let
                !poolDatum_Out_Control = poolDatum_In  
                !poolDatum_Out_Real = OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_PoolDatum
            in  
                poolDatum_Out_Real == poolDatum_Out_Control
        ------------------
        correctOutput_PoolDatum_Value_WithTokens :: Bool
        !correctOutput_PoolDatum_Value_WithTokens =
            let
                !value_In_PoolDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_PoolDatum
                !value_For_Mint_TxID_Master_DeleteScripts = LedgerValue.assetClassValue txID_Master_DeleteScripts_AC 1
            ---------------------
                !value_For_PoolDatum_Control = value_In_PoolDatum <> value_For_Mint_TxID_Master_DeleteScripts
                !value_For_PoolDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_PoolDatum
            in  
                Helpers.valueEqualsValue value_For_PoolDatum_Real value_For_PoolDatum_Control

----------------------------------------------------------------------------------

{-# INLINEABLE policy_TxID_Master_DeleteScripts #-}
policy_TxID_Master_DeleteScripts :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.MintingPolicy
policy_TxID_Master_DeleteScripts pParams scriptID_CS = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams scriptID_CS

{-# INLINEABLE original_policy #-}
original_policy :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> Plutonomy.MintingPolicy
original_policy pParams scriptID_CS =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkPolicy_TxID_Master_DeleteScripts ||])
    `PlutusTx.applyCode` PlutusTx.liftCode pParams
    `PlutusTx.applyCode` PlutusTx.liftCode scriptID_CS

--------------------------------------------------------------------------------
