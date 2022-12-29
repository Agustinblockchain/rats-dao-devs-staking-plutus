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
module Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.SendBackDeposit 
(
    policy_TxID_Master_SendBackDeposit 
) 
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Ada                                                 as LedgerAda
import qualified Ledger.Value                                               as LedgerValue (assetClass, assetClassValue, AssetClass (AssetClass))
import qualified Plutonomy  
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, CurrencySymbol(..), txOutValue, MintingPolicy, adaSymbol, TokenName (TokenName))
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo, valuePaidTo) 
import qualified PlutusTx                                                   (compile, applyCode, liftCode)
import           PlutusTx.Prelude                                           ( Bool(..), Maybe(Nothing, Just), Eq((==)), BuiltinData, Semigroup((<>)), (&&), error, traceError, ($), (++), negate, traceIfFalse, not, emptyByteString )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (fromJust, createValueAddingTokensOfCurrencySymbol, valueIncludesValue)
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (getInputsWithDatum, getReferenceInputsWithDatum, getOutputsWithDatum, isTerminated, validateMasterAction, isNFT_Minted_With_AC)
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers 
import qualified Validators.StakePlusV2.Types.Constants                     as T (poolID_TN, fundID_TN, userID_TN, txID_Master_SendBackDeposit_TN)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, UserDatumTypo (..), PoolDatumTypo (..), DatumValidator (PoolDatum, FundDatum))
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator (RedeemerMasterSendBackDeposit), RedeemerMasterSendBackDepositTypo(..))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_Master_SendBackDeposit #-}
mkPolicy_TxID_Master_SendBackDeposit :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol ->  BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_Master_SendBackDeposit pParams curSymbol_TxID_Master_Fund curSymbol_TxID_User_Deposit mintRedeemerRaw ctxRaw  = 
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
                    !inputsReference_WithDatum = OnChainHelpers.getReferenceInputsWithDatum ctx
                    !inputs_Normal_And_Refs = inputs_WithDatum ++ inputsReference_WithDatum
                    !outputs_WithDatum = OnChainHelpers.getOutputsWithDatum ctx

                    !inputs_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- inputs_WithDatum ]
                    !inputsReference_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- inputsReference_WithDatum]
                    !outputs_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- outputs_WithDatum ]
                in
                    traceIfFalse "INVIO" (OnChainNFTHelpers.checkIfAllAreFromSameAddress inputs_Normal_And_Refs outputs_WithDatum) && 
                    traceIfFalse "INVR" (OnChainNFTHelpers.checkIfAllSpendRedeemersAreEqual ctx redeemer') && 
                
                    case redeemer' of 
                        (T.RedeemerMasterSendBackDeposit redeemer) -> 
                            validateMasterSendBackDeposit pParams curSymbol_TxID_Master_Fund curSymbol_TxID_User_Deposit ctx redeemer inputs_TxOut_Values_And_Datums inputsReference_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
                        _ -> traceError "INVOP"-- "Minting TxID: Invalid Operation"
        then () 

        else error ()
        
--------------------------------------------------------------------------------

{-# INLINABLE validateMasterSendBackDeposit #-}
validateMasterSendBackDeposit :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerContextsV2.ScriptContext -> T.RedeemerMasterSendBackDepositTypo -> [T.TxOut_Value_And_Datum] ->  [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] ->  Bool
validateMasterSendBackDeposit pParams curSymbol_TxID_Master_Fund curSymbol_TxID_User_Deposit ctx redeemer inputs_TxOut_Values_And_Datums inputsReference_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums  =
        
        traceIfFalse "NOTTERMINATED" (OnChainHelpers.isTerminated pParams info poolDatum_In) &&
        
        -- Basic validations for Master Actions
        OnChainHelpers.validateMasterAction pParams info master &&

        -- Check if the txID_User_Harvest_AC minted is there
        traceIfFalse "MSBI" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_SendBackDeposit_AC  info ) &&  -- "Can't Find Valid TxID" 
        
        traceIfFalse "MSBIV" correctInvestAmount_SendBackToUser && 

        correctOutput_PoolDatum_Or_FunDatum
    
    where
        ------------------
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        master = T.rmsbdMaster redeemer
        userToSendBack = T.rmsbdUserToSendBack redeemer
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        !userID_CS = curSymbol_TxID_User_Deposit
        !userID_TN = T.userID_TN
        !userID_AC = LedgerValue.assetClass userID_CS userID_TN
        ------------------
        !txID_Master_SendBackDeposit_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !txID_Master_SendBackDeposit_AC =  LedgerValue.AssetClass (txID_Master_SendBackDeposit_CS, T.txID_Master_SendBackDeposit_TN)
        ------------------
        !inputNormalAndReference = inputs_TxOut_Values_And_Datums ++ inputsReference_TxOut_Values_And_Datums
        ------------------
        !inputNormalOrReference_TxOut_Value_And_PoolDatum' = OnChainNFTHelpers.getTxOut_Value_And_PoolDatum poolID_AC inputNormalAndReference
        !inputNormalOrReference_TxOut_Value_And_PoolDatum = 
            case inputNormalOrReference_TxOut_Value_And_PoolDatum' of
                Nothing -> traceError "INRPD" -- "Error. Can't find single reference input with PoolDatun"
                _       -> Helpers.fromJust inputNormalOrReference_TxOut_Value_And_PoolDatum'
        ------------------
        !input_TxOut_Value_And_UserDatum' = 
            case OnChainNFTHelpers.getTxOuts_Values_And_UserDatums userID_AC inputs_TxOut_Values_And_Datums of
                [x] -> Just x
                _ -> Nothing
        !input_TxOut_Value_And_UserDatum = 
            case input_TxOut_Value_And_UserDatum' of
                Nothing -> traceError "IUD" -- "Error. Can't find input with UserDatum" 
                _       -> Helpers.fromJust input_TxOut_Value_And_UserDatum'
        ------------------
        !poolDatum_In = OnChainNFTHelpers.getTxOut_Datum inputNormalOrReference_TxOut_Value_And_PoolDatum
        ------------------
        !isFundCountZero = T.pdFundCount poolDatum_In == 0
        ------------------
        !userDatum_In = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_UserDatum
        ------------------
        !value_In_UserDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_UserDatum
        ---------------------
        !staking_CS = T.ppStaking_CS pParams
        !staking_AC = LedgerValue.AssetClass (staking_CS, T.ppStaking_TN pParams)
        !stakingIsAda = LedgerApiV2.adaSymbol == staking_CS
        !stakingIsWithoutTokenName = not stakingIsAda && T.ppStaking_TN pParams == LedgerApiV2.TokenName emptyByteString
        ---------------------
        !investAmount = T.udInvest userDatum_In
        !value_InvestAmount = Helpers.createValueAddingTokensOfCurrencySymbol staking_AC staking_CS stakingIsWithoutTokenName value_In_UserDatum investAmount
        ---------------------
        !mindAda_In_UserDatum = T.udMinAda userDatum_In
        !value_MinAda_In_UserDatum = LedgerAda.lovelaceValueOf mindAda_In_UserDatum
        ---------------------
        !value_InvestAmountPlusAda = value_InvestAmount <> value_MinAda_In_UserDatum
        !value_For_UserToSendBackDeposit = value_InvestAmountPlusAda
        ------------------
        correctInvestAmount_SendBackToUser :: Bool
        !correctInvestAmount_SendBackToUser = 
            Helpers.valueIncludesValue (LedgerContextsV2.valuePaidTo info userToSendBack) value_For_UserToSendBackDeposit
        ------------------
        correctOutput_PoolDatum_Or_FunDatum :: Bool
        !correctOutput_PoolDatum_Or_FunDatum =
            let
                !value_For_Mint_TxID_Master_SendBackDeposit = LedgerValue.assetClassValue txID_Master_SendBackDeposit_AC 1
                !value_For_Burn_UserID = LedgerValue.assetClassValue userID_AC (-1)
            in
                if isFundCountZero then
                    let 
                        ------------------
                        !output_TxOut_Value_And_PoolDatum' = OnChainNFTHelpers.getTxOut_Value_And_PoolDatum poolID_AC outputs_TxOut_Values_And_Datums
                        !output_TxOut_Value_And_PoolDatum =
                            case output_TxOut_Value_And_PoolDatum' of
                                Nothing -> traceError "OPD" -- "Error. Can't find output with PoolDatum"
                                _       -> Helpers.fromJust output_TxOut_Value_And_PoolDatum'
                        ------------------
                        correctOutput_PoolDatum_NotChanged :: Bool
                        !correctOutput_PoolDatum_NotChanged =
                            let
                                !poolDatum_Out_Control = T.PoolDatum $ OnChainNFTHelpers.getTxOut_Datum inputNormalOrReference_TxOut_Value_And_PoolDatum
                                !poolDatum_Out_Real = T.PoolDatum $ OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_PoolDatum
                            in
                                poolDatum_Out_Real == poolDatum_Out_Control
                        ------------------
                        correctOutput_PoolDatum_Value_WithTokens :: Bool
                        !correctOutput_PoolDatum_Value_WithTokens =
                            let
                                !value_In_PoolDatum = OnChainNFTHelpers.getTxOut_Value inputNormalOrReference_TxOut_Value_And_PoolDatum
                                !value_For_PoolDatum_Control = value_In_PoolDatum <> value_For_Mint_TxID_Master_SendBackDeposit <> value_In_UserDatum <> value_For_Burn_UserID <> negate value_For_UserToSendBackDeposit     
                            ---------------------
                                !value_For_PoolDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_PoolDatum
                            in
                                value_For_PoolDatum_Real == value_For_PoolDatum_Control
                    in 
                        -- Without the UserID
                        traceIfFalse "PD" correctOutput_PoolDatum_NotChanged && --Wrong Updated FundDatum

                        -- Must be the same than the input.
                        traceIfFalse "PDV" correctOutput_PoolDatum_Value_WithTokens --Wrong FundDatum Value
                else
                    let 
                        !fundID_CS = curSymbol_TxID_Master_Fund
                        !fundID_TN = T.fundID_TN
                        !fundID_AC = LedgerValue.assetClass fundID_CS fundID_TN
                        
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
                        !output_TxOut_Value_And_FundDatum' =
                            case OnChainNFTHelpers.getTxOuts_Values_And_FundDatums fundID_AC outputs_TxOut_Values_And_Datums of
                                [x] -> Just x
                                _   -> Nothing
                        !output_TxOut_Value_And_FundDatum =
                            case output_TxOut_Value_And_FundDatum' of
                                Nothing -> traceError "OFD" -- "Error. Can't find output with FundDatum"
                                _       -> Helpers.fromJust output_TxOut_Value_And_FundDatum'
                        ------------------      
                        isNotPresentPoolDatumAsRef :: Bool
                        !isNotPresentPoolDatumAsRef = 
                            let 
                                !inputNormal_TxOut_Value_And_PoolDatum' = OnChainNFTHelpers.getTxOut_Value_And_PoolDatum poolID_AC inputs_TxOut_Values_And_Datums
                            in
                                case inputNormal_TxOut_Value_And_PoolDatum' of
                                    Nothing -> True
                                    _       -> False
                        ------------------
                        correctOutput_FundDatum_NotChanged :: Bool
                        !correctOutput_FundDatum_NotChanged =
                            let
                                !fundDatum_Out_Control = T.FundDatum $ OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_FundDatum
                                !fundDatum_Out_Real = T.FundDatum $ OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_FundDatum
                            in
                                fundDatum_Out_Real == fundDatum_Out_Control
                        ------------------
                        correctOutputFundDatum_Value_WithTokens :: Bool
                        !correctOutputFundDatum_Value_WithTokens =
                            let
                                !value_In_FundDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_FundDatum
                                !value_For_FundDatum_Control = value_In_FundDatum <> value_For_Mint_TxID_Master_SendBackDeposit <> value_In_UserDatum <> value_For_Burn_UserID <> negate value_For_UserToSendBackDeposit
                            ---------------------
                                !value_For_FundDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_FundDatum
                            in
                                value_For_FundDatum_Real == value_For_FundDatum_Control
                    in 
                        traceIfFalse "IRPDE" isNotPresentPoolDatumAsRef &&  

                        -- Without the UserID
                        traceIfFalse "FD" correctOutput_FundDatum_NotChanged && --Wrong Updated FundDatum

                        -- Must be the same than the input.
                        traceIfFalse "FDV" correctOutputFundDatum_Value_WithTokens --Wrong FundDatum Value 

        -- 

--------------------------------------------------------------------------------

policy_TxID_Master_SendBackDeposit :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.MintingPolicy
policy_TxID_Master_SendBackDeposit pParams curSymbol_TxID_Master_Fund curSymbol_TxID_User_Deposit = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams curSymbol_TxID_Master_Fund curSymbol_TxID_User_Deposit 

original_policy :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> Plutonomy.MintingPolicy
original_policy pParams curSymbol_TxID_Master_Fund curSymbol_TxID_User_Deposit  =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkPolicy_TxID_Master_SendBackDeposit ||])
    `PlutusTx.applyCode` PlutusTx.liftCode pParams
    `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_Master_Fund
    `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_User_Deposit

--------------------------------------------------------------------------------
