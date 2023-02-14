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
-- {-# LANGUAGE Strict #-}
{- HLINT ignore "Use camelCase" -}
------------------------------------------------------------------------------------------
module Validators.StakePlusV2.OnChain.Core.Validator
(
    codeValidator
)
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Plutonomy
import qualified Ledger.Value                                           as LedgerValue
import qualified Plutus.V2.Ledger.Api                                   as LedgerApiV2 (unsafeFromBuiltinData, Validator,  CurrencySymbol, txInInfoResolved, txOutValue) 
import qualified Plutus.V2.Ledger.Contexts                              as LedgerContextsV2 (findOwnInput, ScriptContext, scriptContextTxInfo) 
import qualified PlutusTx
import           PlutusTx.Prelude                                       ( Maybe(Just, Nothing), BuiltinData, (&&), (||), error, traceError, ($), traceIfFalse )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                         as Helpers (isToken_With_AC_InValue, isNFT_With_AC_InValue)
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers     as OnChainHelpers (isNFT_Minted_With_AC)
import qualified Validators.StakePlusV2.Types.Constants                 as T (poolID_TN, fundID_TN, userID_TN, scriptID_TN, txID_Master_FundAndMerge_TN, txID_User_Withdraw_TN, txID_User_Harvest_TN, txID_Master_DeleteScripts_TN, txID_Master_SendBackDeposit_TN, txID_Master_SendBackFund_TN, txID_Master_DeleteFund_TN, txID_Master_TerminatePool_TN, txID_Master_Emergency_TN, txID_Master_ClosePool_TN, txID_Master_SplitFund_TN)
import qualified Validators.StakePlusV2.Types.RedeemersValidator        as T (RedeemerValidator (..))
import qualified Validators.StakePlusV2.Types.Types                     as T (PoolParams (ppPoolID_CS), CS)

------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: T.CS -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol ->  LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol ->LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> BuiltinData -> BuiltinData  -> BuiltinData -> ()
mkValidator !ppPoolID_CS !txID_Master_Fund_CS !txID_Master_FundAndMerge_CS !txID_Master_SplitFund_CS !txID_Master_ClosePool_CS !txID_Master_TerminatePool_CS !txID_Master_Emergency_CS !txID_Master_DeleteFund_CS !txID_Master_SendBackFund_CS !txID_Master_SendBackDeposit_CS !txID_Master_AddScripts_CS !txID_Master_DeleteScripts_CS !txID_User_Deposit_CS !txID_User_Harvest_CS !txID_User_Withdraw_CS _ !validatorRedeemerRaw !ctxRaw =
    let
        !validatorRedeemer = LedgerApiV2.unsafeFromBuiltinData @T.RedeemerValidator validatorRedeemerRaw
        -- _ = LedgerApiV2.unsafeFromBuiltinData @T.DatumValidator validatorDatumRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !inputBeingValidated =
          case LedgerContextsV2.findOwnInput ctx of
            Nothing -> traceError "IE" 
            Just  x -> x
        !txOutBeingValidated = LedgerApiV2.txInInfoResolved inputBeingValidated
        ------------------
        !valueBeingValidated = LedgerApiV2.txOutValue txOutBeingValidated
        ------------------
        !poolID_AC = LedgerValue.AssetClass (ppPoolID_CS, T.poolID_TN)
        ----------------
        !fundID_CS = txID_Master_Fund_CS
        !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)
        ------------------
        !userID_CS = txID_User_Deposit_CS
        !userID_AC = LedgerValue.AssetClass (userID_CS, T.userID_TN) 
        ------------------
        !scriptID_CS = txID_Master_AddScripts_CS
        !scriptID_AC = LedgerValue.AssetClass (scriptID_CS, T.scriptID_TN) 
        ------------------
        !txID_Master_FundAndMerge_AC = LedgerValue.AssetClass (txID_Master_FundAndMerge_CS, T.txID_Master_FundAndMerge_TN)
        !txID_Master_SplitFund_AC = LedgerValue.AssetClass (txID_Master_SplitFund_CS, T.txID_Master_SplitFund_TN)
        !txID_Master_ClosePool_AC = LedgerValue.AssetClass (txID_Master_ClosePool_CS, T.txID_Master_ClosePool_TN)
        !txID_Master_TerminatePool_AC = LedgerValue.AssetClass (txID_Master_TerminatePool_CS, T.txID_Master_TerminatePool_TN)
        !txID_Master_Emergency_AC = LedgerValue.AssetClass (txID_Master_Emergency_CS, T.txID_Master_Emergency_TN)
        !txID_Master_DeleteFund_AC = LedgerValue.AssetClass (txID_Master_DeleteFund_CS, T.txID_Master_DeleteFund_TN)
        !txID_Master_SendBackFund_AC = LedgerValue.AssetClass (txID_Master_SendBackFund_CS, T.txID_Master_SendBackFund_TN)
        !txID_Master_SendBackDeposit_AC = LedgerValue.AssetClass (txID_Master_SendBackDeposit_CS, T.txID_Master_SendBackDeposit_TN)
        !txID_Master_DeleteScripts_AC = LedgerValue.AssetClass (txID_Master_DeleteScripts_CS, T.txID_Master_DeleteScripts_TN)
        !txID_User_Harvest_AC = LedgerValue.AssetClass (txID_User_Harvest_CS, T.txID_User_Harvest_TN)
        !txID_User_Withdraw_AC = LedgerValue.AssetClass (txID_User_Withdraw_CS, T.txID_User_Withdraw_TN)
    in
        if 
             case validatorRedeemer of
                (T.RedeemerMasterFund _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_AC fundID_AC info) &&
                    traceIfFalse "IE2"  (Helpers.isNFT_With_AC_InValue valueBeingValidated poolID_AC)
                (T.RedeemerMasterFundAndMerge _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_FundAndMerge_AC info) &&
                    traceIfFalse "IE2"  (
                        Helpers.isNFT_With_AC_InValue valueBeingValidated poolID_AC ||
                        Helpers.isToken_With_AC_InValue valueBeingValidated fundID_AC)
                (T.RedeemerMasterSplitFund _) ->
                    traceIfFalse "TxID" (
                        OnChainHelpers.isNFT_Minted_With_AC fundID_AC info && 
                        OnChainHelpers.isNFT_Minted_With_AC txID_Master_SplitFund_AC info
                        ) &&
                    traceIfFalse "IE2"  (
                        Helpers.isNFT_With_AC_InValue valueBeingValidated poolID_AC ||
                        Helpers.isToken_With_AC_InValue valueBeingValidated fundID_AC)
                (T.RedeemerMasterClosePool _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_ClosePool_AC info) &&
                    traceIfFalse "IE2"  (Helpers.isNFT_With_AC_InValue valueBeingValidated poolID_AC)
                (T.RedeemerMasterTerminatePool _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_TerminatePool_AC info) &&
                    traceIfFalse "IE2" (Helpers.isNFT_With_AC_InValue valueBeingValidated poolID_AC)
                (T.RedeemerMasterEmergency _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_Emergency_AC info) 
                    -- Emergency puede consumir cualquier tipo de UTXOs
                    -- se mintea solo cuando esta firmado por todos los masters o cuando el pool datum esta en emergencia.
                (T.RedeemerMasterDeleteFund _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_DeleteFund_AC info) &&
                    traceIfFalse "IE2" (
                        Helpers.isNFT_With_AC_InValue valueBeingValidated poolID_AC ||
                        Helpers.isToken_With_AC_InValue valueBeingValidated fundID_AC )
                (T.RedeemerMasterSendBackFund _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_SendBackFund_AC info) &&
                    traceIfFalse "IE2"  (
                        Helpers.isNFT_With_AC_InValue valueBeingValidated poolID_AC)
                (T.RedeemerMasterSendBackDeposit _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_SendBackDeposit_AC info) &&
                    traceIfFalse "IE2"  (
                        Helpers.isNFT_With_AC_InValue valueBeingValidated poolID_AC ||
                        Helpers.isToken_With_AC_InValue valueBeingValidated fundID_AC ||
                        Helpers.isNFT_With_AC_InValue valueBeingValidated userID_AC)
                -- (T.RedeemerMasterAddScripts _) ->
                -- no permito consumo con este redeemer
                (T.RedeemerMasterDeleteScripts _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_DeleteScripts_AC info)  &&
                    traceIfFalse "IE2"  (
                        Helpers.isNFT_With_AC_InValue valueBeingValidated poolID_AC ||
                        Helpers.isToken_With_AC_InValue valueBeingValidated scriptID_AC)
                -- (T.RedeemerUserDeposit _) ->
                -- no permito consumo con este redeemer
                (T.RedeemerUserHarvest _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_AC txID_User_Harvest_AC info) &&
                    traceIfFalse "IE2"  (
                        Helpers.isToken_With_AC_InValue valueBeingValidated fundID_AC ||
                        Helpers.isNFT_With_AC_InValue valueBeingValidated userID_AC)
                (T.RedeemerUserWithdraw _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_AC txID_User_Withdraw_AC info) 
                    &&
                    traceIfFalse "IE2"  (
                        Helpers.isNFT_With_AC_InValue valueBeingValidated poolID_AC ||
                        Helpers.isToken_With_AC_InValue valueBeingValidated fundID_AC ||
                        Helpers.isNFT_With_AC_InValue valueBeingValidated userID_AC)
                _ -> traceError "INVOP"
        then ()
        else error ()

--------------------------------------------------------------------------------

{-# INLINABLE codeValidator #-}
codeValidator ::  T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.Validator
codeValidator pParams txID_Master_Fund_CS txID_Master_FundAndMerge_CS txID_Master_SplitFund_CS txID_Master_ClosePool_CS txID_Master_TerminatePool_CS txID_Master_Emergency_CS txID_Master_DeleteFund_CS txID_Master_SendBackFund_CS txID_Master_SendBackDeposit_CS txID_Master_AddScripts_CS txID_Master_DeleteScripts_CS txID_User_Deposit_CS txID_User_Harvest_CS txID_User_Withdraw_CS =
    Plutonomy.optimizeUPLC $
      Plutonomy.validatorToPlutus $ plutonomyValidator pParams txID_Master_Fund_CS txID_Master_FundAndMerge_CS txID_Master_SplitFund_CS txID_Master_ClosePool_CS txID_Master_TerminatePool_CS txID_Master_Emergency_CS txID_Master_DeleteFund_CS txID_Master_SendBackFund_CS txID_Master_SendBackDeposit_CS txID_Master_AddScripts_CS txID_Master_DeleteScripts_CS txID_User_Deposit_CS txID_User_Harvest_CS txID_User_Withdraw_CS

--------------------------------------------------------------------------------

{-# INLINABLE plutonomyValidator #-}
plutonomyValidator :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> Plutonomy.Validator
plutonomyValidator pParams txID_Master_Fund_CS txID_Master_FundAndMerge_CS txID_Master_SplitFund_CS txID_Master_ClosePool_CS txID_Master_TerminatePool_CS txID_Master_Emergency_CS txID_Master_DeleteFund_CS txID_Master_SendBackFund_CS txID_Master_SendBackDeposit_CS txID_Master_AddScripts_CS txID_Master_DeleteScripts_CS txID_User_Deposit_CS txID_User_Harvest_CS txID_User_Withdraw_CS =
    Plutonomy.mkValidatorScript $
     $$(PlutusTx.compile [||mkValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode (T.ppPoolID_CS pParams)
        `PlutusTx.applyCode` PlutusTx.liftCode txID_Master_Fund_CS
        `PlutusTx.applyCode` PlutusTx.liftCode txID_Master_FundAndMerge_CS
        `PlutusTx.applyCode` PlutusTx.liftCode txID_Master_SplitFund_CS
        `PlutusTx.applyCode` PlutusTx.liftCode txID_Master_ClosePool_CS
        `PlutusTx.applyCode` PlutusTx.liftCode txID_Master_TerminatePool_CS
        `PlutusTx.applyCode` PlutusTx.liftCode txID_Master_Emergency_CS
        `PlutusTx.applyCode` PlutusTx.liftCode txID_Master_DeleteFund_CS
        `PlutusTx.applyCode` PlutusTx.liftCode txID_Master_SendBackFund_CS
        `PlutusTx.applyCode` PlutusTx.liftCode txID_Master_SendBackDeposit_CS
        `PlutusTx.applyCode` PlutusTx.liftCode txID_Master_AddScripts_CS
        `PlutusTx.applyCode` PlutusTx.liftCode txID_Master_DeleteScripts_CS
        `PlutusTx.applyCode` PlutusTx.liftCode txID_User_Deposit_CS
        `PlutusTx.applyCode` PlutusTx.liftCode txID_User_Harvest_CS
        `PlutusTx.applyCode` PlutusTx.liftCode txID_User_Withdraw_CS



--------------------------------------------------------------------------------
