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
module Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.DeleteScripts
(
    policy_TxID_Master_DeleteScripts
) 
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Value                                               as LedgerValue (AssetClass (AssetClass), assetClassValue)
import qualified Plutonomy  
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, MintingPolicy, TxOut (txOutValue), CurrencySymbol, Value)
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo, valuePaidTo) 
import qualified PlutusTx                                                   (compile, liftCode, applyCode)
import           PlutusTx.Prelude                                           ( Bool, BuiltinData, (&&), error, traceError, ($), traceIfFalse, Maybe (Nothing, Just), head, Eq ((==)), Semigroup ((<>)), elem, otherwise, (++), filter, (/=), find, all, length, negate )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (fromJust, valueIncludesValue, valueFromCurrencySymbol)
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (validateMasterAction, isNFT_Minted_With_AC, getInputsWithDatum, getOutputsWithDatum, isTerminated)
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (validateBurn_Token_Own_CS_Any_TN, checkIfAllAreFromSameAddress, getTxOut_Value_And_PoolDatum, getTxOut_Datum, getTxOut_Value, checkIfAllSpendRedeemersAreEqual, getTxOut_Value_And_ScriptDatum)
import qualified Validators.StakePlusV2.Types.Constants                     as T (txID_Master_DeleteScripts_TN, poolID_TN, txID_Master_AddScripts_TN)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, DatumValidator (PoolDatum), ScriptDatumTypo (sdMaster))
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator (RedeemerMasterDeleteScripts), RedeemerMasterDeleteScriptsTypo (..))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (ppPoolID_CS), Master)
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_Master_DeleteScripts #-}
mkPolicy_TxID_Master_DeleteScripts :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_Master_DeleteScripts pParams curSymbol_TxID_Master_AddScripts mintRedeemerRaw ctxRaw  = 
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
                            validateMasterDeleteScripts pParams curSymbol_TxID_Master_AddScripts ctx redeemer inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums 

                        _ -> traceError "INVOP"-- "Minting TxID: Invalid Operation"
        then () 

        else error ()

--------------------------------------------------------------------------------

{-# INLINABLE validateMasterDeleteScripts #-}
validateMasterDeleteScripts :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerContextsV2.ScriptContext -> T.RedeemerMasterDeleteScriptsTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> Bool
validateMasterDeleteScripts pParams curSymbol_TxID_Master_AddScripts ctx redeemer inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums  =
        
        traceIfFalse "NOTTERMINATED" (OnChainHelpers.isTerminated pParams info poolDatum_In) &&
        
        -- Basic validations for Master Actions 
        OnChainHelpers.validateMasterAction pParams info master &&
        
        traceIfFalse "MSBFV" correctFundAmount_SendBackToMaster && 

        -- "Can't Find Valid TxID" 
        traceIfFalse "MDS" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_DeleteScripts_AC  info )  &&

        -- "Wrong Updated PoolDatum"
        traceIfFalse "PD" correctOutput_PoolDatum_NotChanged &&

        -- Must be the same than the input.
        traceIfFalse "PDV" correctOutput_PoolDatum_Value_WithTokens -- "Wrong PoolDatum Value"

    where
        ------------------
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !master = T.rmdsMaster redeemer
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        !txID_Master_AddScripts_CS = curSymbol_TxID_Master_AddScripts
        !txID_Master_AddScripts_AC = LedgerValue.AssetClass (txID_Master_AddScripts_CS, T.txID_Master_AddScripts_TN)
        ------------------
        !txID_Master_DeleteScripts_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !txID_Master_DeleteScripts_AC =  LedgerValue.AssetClass (txID_Master_DeleteScripts_CS, T.txID_Master_DeleteScripts_TN)
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
        !inputs_TxOuts_Values_And_ScriptDatums' =
            case OnChainNFTHelpers.getTxOut_Value_And_ScriptDatum txID_Master_AddScripts_AC inputs_TxOut_Values_And_Datums of
                []  -> Nothing
                x   -> Just x
        !inputs_TxOuts_Values_And_ScriptDatums =
            case inputs_TxOuts_Values_And_ScriptDatums' of
                Nothing -> traceError "ISDS" -- "Error. Can't find inputs with ScriptDatums"
                _       -> Helpers.fromJust inputs_TxOuts_Values_And_ScriptDatums'
        ------------------
        !poolDatum_In = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_PoolDatum
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
                            !value_For_Burn  = negate $ Helpers.valueFromCurrencySymbol value_In_ScriptDatum txID_Master_AddScripts_CS
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
                !poolDatum_Out_Control = T.PoolDatum poolDatum_In  
                !poolDatum_Out_Real = T.PoolDatum $ OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_PoolDatum
            in  
                poolDatum_Out_Real == poolDatum_Out_Control
        ------------------
        correctOutput_PoolDatum_Value_WithTokens :: Bool
        !correctOutput_PoolDatum_Value_WithTokens =
            let
                !value_In_PoolDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_PoolDatum
            ---------------------
                !value_For_Mint_TxID_Master_DeleteScripts = LedgerValue.assetClassValue txID_Master_DeleteScripts_AC 1
            ---------------------
                !value_For_PoolDatum_Control = value_In_PoolDatum <> value_For_Mint_TxID_Master_DeleteScripts
                !value_For_PoolDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_PoolDatum
            in  
                value_For_PoolDatum_Real == value_For_PoolDatum_Control
        ------------------
----------------------------------------------------------------------------------

{-# INLINEABLE policy_TxID_Master_DeleteScripts #-}
policy_TxID_Master_DeleteScripts :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.MintingPolicy
policy_TxID_Master_DeleteScripts pParams curSymbol_TxID_Master_AddScripts = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams curSymbol_TxID_Master_AddScripts

{-# INLINEABLE original_policy #-}
original_policy :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> Plutonomy.MintingPolicy
original_policy pParams curSymbol_TxID_Master_AddScripts =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkPolicy_TxID_Master_DeleteScripts ||])
    `PlutusTx.applyCode` PlutusTx.liftCode pParams
    `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_Master_AddScripts

--------------------------------------------------------------------------------
