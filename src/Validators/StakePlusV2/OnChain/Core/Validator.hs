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
import qualified Validators.StakePlusV2.Helpers                         as Helpers (isToken_With_AC_InValue)
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers     as OnChainHelpers (isNFT_Minted_With_CS)
import qualified Validators.StakePlusV2.Types.Constants                 as T (poolID_TN, fundID_TN, userID_TN, txID_Master_AddScripts_TN,)
import qualified Validators.StakePlusV2.Types.RedeemersValidator        as T (RedeemerValidator (..))
import qualified Validators.StakePlusV2.Types.Types                     as T (PoolParams (ppPoolID_CS), CS)

------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: T.CS -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol ->  LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol ->LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> BuiltinData -> BuiltinData  -> BuiltinData  -> ()
mkValidator ppPoolID_CS curSymbol_TxID_Master_Fund curSymbol_TxID_Master_FundAndMerge curSymbol_TxID_Master_SplitFund curSymbol_TxID_Master_ClosePool curSymbol_TxID_Master_TerminatePool  curSymbol_TxID_Master_DeleteFund curSymbol_TxID_Master_SendBackFund curSymbol_TxID_Master_SendBackDeposit curSymbol_TxID_Master_AddScripts curSymbol_TxID_Master_DeleteScripts curSymbol_TxID_User_Deposit curSymbol_TxID_User_Harvest curSymbol_TxID_User_Withdraw _ validatorRedeemerRaw ctxRaw =
    let
        ------------------
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
        ------------------
        !fundID_CS = curSymbol_TxID_Master_Fund
        !fundID_TN = T.fundID_TN
        !fundID_AC = LedgerValue.assetClass fundID_CS fundID_TN
        ------------------
        !userID_CS = curSymbol_TxID_User_Deposit
        !userID_TN = T.userID_TN
        !userID_AC = LedgerValue.assetClass userID_CS userID_TN
        ------------------
        !txID_Master_AddScripts_CS = curSymbol_TxID_Master_AddScripts
        !txID_Master_AddScripts_AC = LedgerValue.AssetClass (txID_Master_AddScripts_CS, T.txID_Master_AddScripts_TN)
        ------------------
    in
        if 
             case validatorRedeemer of
                (T.RedeemerMasterFund _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_CS curSymbol_TxID_Master_Fund info) &&
                    traceIfFalse "IE2"  (Helpers.isToken_With_AC_InValue valueBeingValidated poolID_AC)

                (T.RedeemerMasterFundAndMerge _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_CS curSymbol_TxID_Master_FundAndMerge info) &&
                    traceIfFalse "IE2"  (
                        Helpers.isToken_With_AC_InValue valueBeingValidated poolID_AC ||
                        Helpers.isToken_With_AC_InValue valueBeingValidated fundID_AC)

                (T.RedeemerMasterSplitFund _) ->
                    traceIfFalse "TxID" (
                        OnChainHelpers.isNFT_Minted_With_CS curSymbol_TxID_Master_Fund info &&
                        OnChainHelpers.isNFT_Minted_With_CS curSymbol_TxID_Master_SplitFund info
                        ) &&
                    traceIfFalse "IE2"  (
                        Helpers.isToken_With_AC_InValue valueBeingValidated poolID_AC ||
                        Helpers.isToken_With_AC_InValue valueBeingValidated fundID_AC)

                (T.RedeemerMasterClosePool _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_CS curSymbol_TxID_Master_ClosePool info) &&
                    traceIfFalse "IE2"  (Helpers.isToken_With_AC_InValue valueBeingValidated poolID_AC)

                (T.RedeemerMasterTerminatePool _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_CS curSymbol_TxID_Master_TerminatePool info) &&
                    traceIfFalse "IE2" (Helpers.isToken_With_AC_InValue valueBeingValidated poolID_AC)

                (T.RedeemerMasterDeleteFund _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_CS curSymbol_TxID_Master_DeleteFund info) &&
                    traceIfFalse "IE2" (
                        Helpers.isToken_With_AC_InValue valueBeingValidated poolID_AC ||
                        Helpers.isToken_With_AC_InValue valueBeingValidated fundID_AC )

                (T.RedeemerMasterSendBackFund _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_CS curSymbol_TxID_Master_SendBackFund info) &&
                    traceIfFalse "IE2"  (
                        Helpers.isToken_With_AC_InValue valueBeingValidated poolID_AC)

                (T.RedeemerMasterSendBackDeposit _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_CS curSymbol_TxID_Master_SendBackDeposit info) &&
                    traceIfFalse "IE2"  (
                        Helpers.isToken_With_AC_InValue valueBeingValidated poolID_AC ||
                        Helpers.isToken_With_AC_InValue valueBeingValidated fundID_AC ||
                        Helpers.isToken_With_AC_InValue valueBeingValidated userID_AC)

                -- (T.RedeemerMasterAddScripts _) ->
                --     traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_CS curSymbol_TxID_Master_AddScripts info)

                (T.RedeemerMasterDeleteScripts _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_CS curSymbol_TxID_Master_DeleteScripts info)  &&
                    traceIfFalse "IE2"  (
                        Helpers.isToken_With_AC_InValue valueBeingValidated poolID_AC ||
                        Helpers.isToken_With_AC_InValue valueBeingValidated txID_Master_AddScripts_AC)

                -- (T.RedeemerUserDeposit _) ->
                --     traceIfFalse "TxID" (OnChainHelpers.isToken_Minted_With_CS curSymbol_TxID_User_Deposit info) &&
                --     traceIfFalse "IE2"  (
                --         Helpers.isToken_With_AC_InValue valueBeingValidated poolID_AC ||
                --         Helpers.isToken_With_AC_InValue valueBeingValidated fundID_AC ||
                --         Helpers.isToken_With_AC_InValue valueBeingValidated userID_AC)

                (T.RedeemerUserHarvest _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_CS curSymbol_TxID_User_Harvest info) &&
                    traceIfFalse "IE2"  (
                        Helpers.isToken_With_AC_InValue valueBeingValidated fundID_AC ||
                        Helpers.isToken_With_AC_InValue valueBeingValidated userID_AC)

                (T.RedeemerUserWithdraw _) ->
                    traceIfFalse "TxID" (OnChainHelpers.isNFT_Minted_With_CS curSymbol_TxID_User_Withdraw info) &&
                    traceIfFalse "IE2"  (
                        Helpers.isToken_With_AC_InValue valueBeingValidated poolID_AC ||
                        Helpers.isToken_With_AC_InValue valueBeingValidated fundID_AC ||
                        Helpers.isToken_With_AC_InValue valueBeingValidated userID_AC)

                _ -> traceError "INVOP"

        then ()
        else error ()

--------------------------------------------------------------------------------

{-# INLINABLE codeValidator #-}
codeValidator ::  T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.Validator
codeValidator pParams curSymbol_TxID_Master_Fund curSymbol_TxID_Master_FundAndMerge curSymbol_TxID_Master_SplitFund curSymbol_TxID_Master_ClosePool curSymbol_TxID_Master_TerminatePool  curSymbol_TxID_Master_DeleteFund curSymbol_TxID_Master_SendBackFund curSymbol_TxID_Master_SendBackDeposit curSymbol_TxID_Master_AddScripts curSymbol_TxID_Master_DeleteScripts curSymbol_TxID_User_Deposit curSymbol_TxID_User_Harvest curSymbol_TxID_User_Withdraw =
    Plutonomy.optimizeUPLC $
      Plutonomy.validatorToPlutus $ plutonomyValidator pParams curSymbol_TxID_Master_Fund curSymbol_TxID_Master_FundAndMerge curSymbol_TxID_Master_SplitFund curSymbol_TxID_Master_ClosePool curSymbol_TxID_Master_TerminatePool  curSymbol_TxID_Master_DeleteFund curSymbol_TxID_Master_SendBackFund curSymbol_TxID_Master_SendBackDeposit curSymbol_TxID_Master_AddScripts curSymbol_TxID_Master_DeleteScripts curSymbol_TxID_User_Deposit curSymbol_TxID_User_Harvest curSymbol_TxID_User_Withdraw

--------------------------------------------------------------------------------

{-# INLINABLE plutonomyValidator #-}
plutonomyValidator :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> Plutonomy.Validator
plutonomyValidator pParams curSymbol_TxID_Master_Fund curSymbol_TxID_Master_FundAndMerge curSymbol_TxID_Master_SplitFund curSymbol_TxID_Master_ClosePool curSymbol_TxID_Master_TerminatePool  curSymbol_TxID_Master_DeleteFund curSymbol_TxID_Master_SendBackFund curSymbol_TxID_Master_SendBackDeposit curSymbol_TxID_Master_AddScripts curSymbol_TxID_Master_DeleteScripts curSymbol_TxID_User_Deposit curSymbol_TxID_User_Harvest curSymbol_TxID_User_Withdraw =
    Plutonomy.mkValidatorScript $
     $$(PlutusTx.compile [||mkValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode (T.ppPoolID_CS pParams)
        `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_Master_Fund
        `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_Master_FundAndMerge
        `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_Master_SplitFund
        `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_Master_ClosePool
        `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_Master_TerminatePool
        `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_Master_DeleteFund
        `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_Master_SendBackFund
        `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_Master_SendBackDeposit
        `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_Master_AddScripts
        `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_Master_DeleteScripts
        `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_User_Deposit
        `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_User_Harvest
        `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_User_Withdraw



--------------------------------------------------------------------------------
