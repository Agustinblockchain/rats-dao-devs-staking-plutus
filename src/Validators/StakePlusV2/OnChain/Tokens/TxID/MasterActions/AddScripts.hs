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
module Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.AddScripts
(
    policy_TxID_Master_AddScripts, --curSymbol_TxID_Master_AddScripts 
) 
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Value                                               as LedgerValue
import qualified Plutonomy  
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, MintingPolicy) 
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ScriptContext, TxInfo, scriptContextTxInfo, ownCurrencySymbol)
import qualified PlutusTx                                                   (compile, liftCode, applyCode)
import PlutusTx.Prelude                                                     ( Bool, BuiltinData, (&&), error, traceError, ($), traceIfFalse, (||) )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (validateMasterAction, isNFT_Minted_With_AC, isToken_Minted_With_AC_AndAmt, isToken_Minted_With_AC)
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (validateBurn_Token_Own_CS_Any_TN)
import qualified Validators.StakePlusV2.Types.Constants                     as T (txID_Master_AddScripts_TN, scriptID_Master_Fund_TN, scriptID_Master_FundAndMerge_TN, scriptID_Master_SplitFund_TN, scriptID_Master_ClosePool_TN, scriptID_Master_TerminatePool_TN, scriptID_Master_DeleteFund_TN, scriptID_Master_SendBackFund_TN, scriptID_Master_SendBackDeposit_TN, scriptID_Master_AddScripts_TN, scriptID_Master_DeleteScripts_TN, scriptID_User_Deposit_TN, scriptID_User_Harvest_TN, scriptID_User_Withdraw_TN, scriptID_Validator_TN)
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator(RedeemerMasterAddScripts), RedeemerMasterAddScriptsTypo (rmasMaster))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..), Master)
import Validators.StakePlusV2.Types.RedeemersMint

------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_Master_AddScripts #-}
mkPolicy_TxID_Master_AddScripts :: T.PoolParams -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_Master_AddScripts pParams mintRedeemerRaw ctxRaw  = 
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
                    in 
                        case redeemer' of 

                            (T.RedeemerMasterAddScripts redeemer) -> 
                                    validateMasterAddScripts pParams ctx master_InRedeemer 
                                where   
                                    master_InRedeemer = T.rmasMaster redeemer

                            _ -> traceError "INVOP"-- "Minting TxID: Invalid Operation"
        then () 

        else error ()

--------------------------------------------------------------------------------

{-# INLINABLE validateMasterAddScripts #-}
validateMasterAddScripts :: T.PoolParams -> LedgerContextsV2.ScriptContext -> T.Master -> Bool
validateMasterAddScripts pParams ctx master_InRedeemer = 

        -- Basic validations for Master Actions 
        OnChainHelpers.validateMasterAction pParams info master_InRedeemer &&

        -- "Can't Find Valid TxID" 
        traceIfFalse "MAS" (OnChainHelpers.isToken_Minted_With_AC txID_Master_AddScripts_AC info ) &&

        traceIfFalse "ScriptID" (
                OnChainHelpers.isToken_Minted_With_AC scriptID_Validator_AC info ||
                OnChainHelpers.isToken_Minted_With_AC scriptID_Master_Fund_AC info ||
                OnChainHelpers.isToken_Minted_With_AC scriptID_Master_FundAndMerge_AC info ||
                OnChainHelpers.isToken_Minted_With_AC scriptID_Master_SplitFund_AC info ||
                OnChainHelpers.isToken_Minted_With_AC scriptID_Master_ClosePool_AC info ||
                OnChainHelpers.isToken_Minted_With_AC scriptID_Master_TerminatePool_AC info ||
                OnChainHelpers.isToken_Minted_With_AC scriptID_Master_DeleteFund_AC info ||
                OnChainHelpers.isToken_Minted_With_AC scriptID_Master_SendBackFund_AC info ||
                OnChainHelpers.isToken_Minted_With_AC scriptID_Master_SendBackDeposit_AC info ||
                OnChainHelpers.isToken_Minted_With_AC scriptID_Master_AddScripts_AC info ||
                OnChainHelpers.isToken_Minted_With_AC scriptID_Master_DeleteScripts_AC info ||
                OnChainHelpers.isToken_Minted_With_AC scriptID_User_Deposit_AC info ||
                OnChainHelpers.isToken_Minted_With_AC scriptID_User_Harvest_AC info ||
                OnChainHelpers.isToken_Minted_With_AC scriptID_User_Withdraw_AC info 
            )

    where
        ------------------
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !txID_Master_AddScripts_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !txID_Master_AddScripts_AC =  LedgerValue.AssetClass (txID_Master_AddScripts_CS, T.txID_Master_AddScripts_TN)
        ------------------
        !scriptID_Validator_AC =  LedgerValue.AssetClass (txID_Master_AddScripts_CS, T.scriptID_Validator_TN)
        ------------------
        !scriptID_Master_Fund_AC =  LedgerValue.AssetClass (txID_Master_AddScripts_CS, T.scriptID_Master_Fund_TN)
        !scriptID_Master_FundAndMerge_AC =  LedgerValue.AssetClass (txID_Master_AddScripts_CS, T.scriptID_Master_FundAndMerge_TN)
        !scriptID_Master_SplitFund_AC =  LedgerValue.AssetClass (txID_Master_AddScripts_CS, T.scriptID_Master_SplitFund_TN )
        !scriptID_Master_ClosePool_AC =  LedgerValue.AssetClass (txID_Master_AddScripts_CS, T.scriptID_Master_ClosePool_TN)
        !scriptID_Master_TerminatePool_AC =  LedgerValue.AssetClass (txID_Master_AddScripts_CS, T.scriptID_Master_TerminatePool_TN )
        !scriptID_Master_DeleteFund_AC =  LedgerValue.AssetClass (txID_Master_AddScripts_CS, T.scriptID_Master_DeleteFund_TN)
        !scriptID_Master_SendBackFund_AC =  LedgerValue.AssetClass (txID_Master_AddScripts_CS, T.scriptID_Master_SendBackFund_TN)
        !scriptID_Master_SendBackDeposit_AC =  LedgerValue.AssetClass (txID_Master_AddScripts_CS, T.scriptID_Master_SendBackDeposit_TN)
        !scriptID_Master_AddScripts_AC =  LedgerValue.AssetClass (txID_Master_AddScripts_CS, T.scriptID_Master_AddScripts_TN)
        !scriptID_Master_DeleteScripts_AC =  LedgerValue.AssetClass (txID_Master_AddScripts_CS, T.scriptID_Master_DeleteScripts_TN)
        !scriptID_User_Deposit_AC =  LedgerValue.AssetClass (txID_Master_AddScripts_CS, T.scriptID_User_Deposit_TN)
        !scriptID_User_Harvest_AC =  LedgerValue.AssetClass (txID_Master_AddScripts_CS, T.scriptID_User_Harvest_TN)
        !scriptID_User_Withdraw_AC =  LedgerValue.AssetClass (txID_Master_AddScripts_CS, T.scriptID_User_Withdraw_TN)
        ------------------

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
