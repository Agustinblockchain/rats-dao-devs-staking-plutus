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
module Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.SendBackDeposit 
(
    policy_TxID_Master_SendBackDeposit 
) 
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Ada                                                 as LedgerAda
import qualified Ledger.Value                                               as LedgerValue (assetClassValue, AssetClass (AssetClass))
import qualified Plutonomy  
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, CurrencySymbol(..), txOutValue, MintingPolicy, adaSymbol, TokenName (TokenName))
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo, valuePaidTo) 
import qualified PlutusTx                                                   (compile, applyCode, liftCode)
import           PlutusTx.Prelude                                           ( Bool(..), Maybe(Nothing, Just), Eq((==)), BuiltinData, Semigroup((<>)), (&&), error, traceError, ($), (++), traceIfFalse, not, emptyByteString, any, length, AdditiveSemigroup ((+)), otherwise, negate )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (createValueAddingTokensOfCurrencySymbol, valueIncludesValue, valueEqualsValue, isNFT_With_AC_InValue, getUserDatumTypo_FromDatum, getPoolDatumTypo_FromDatum, getFundDatumTypo_FromDatum, getValueOfAC, isToken_With_AC_InValue)
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (getInputsWithDatum, getReferenceInputsWithDatum, getOutputsWithDatum, isTerminated, validateMasterAction, isNFT_Minted_With_AC)
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers 
import qualified Validators.StakePlusV2.Types.Constants                     as T (poolID_TN, fundID_TN, userID_TN, txID_Master_SendBackDeposit_TN, txID_User_Harvest_TN, const_1_PD, const_1_UD, const_1_FD)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, UserDatumTypo (..), PoolDatumTypo (..))
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator (RedeemerMasterSendBackDeposit), RedeemerMasterSendBackDepositTypo(..))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_Master_SendBackDeposit #-}
mkPolicy_TxID_Master_SendBackDeposit :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_Master_SendBackDeposit !pParams !txID_Master_Fund_CS !txID_User_Deposit_CS !txID_User_Harvest_CS !mintRedeemerRaw !ctxRaw  = 
    let
        !mintRedeemer = LedgerApiV2.unsafeFromBuiltinData @T.Redeemer_TxID mintRedeemerRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
    in
        if 
            case mintRedeemer of 
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
                                validateMasterSendBackDeposit pParams txID_Master_Fund_CS txID_User_Deposit_CS txID_User_Harvest_CS ctx redeemer inputs_TxOut_Values_And_Datums inputsReference_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
                            _ -> traceError "INVOP"
        then () 

        else error ()
        
--------------------------------------------------------------------------------

{-# INLINABLE validateMasterSendBackDeposit #-}
validateMasterSendBackDeposit :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerContextsV2.ScriptContext -> T.RedeemerMasterSendBackDepositTypo -> [T.TxOut_Value_And_Datum] ->  [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] ->  Bool
validateMasterSendBackDeposit !pParams !txID_Master_Fund_CS !txID_User_Deposit_CS !txID_User_Harvest_CS !ctx !redeemer !inputs_TxOut_Values_And_Datums !inputsReference_TxOut_Values_And_Datums !outputs_TxOut_Values_And_Datums  =
        OnChainHelpers.validateMasterAction pParams info master &&
        traceIfFalse "MSBI" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_SendBackDeposit_AC  info ) &&  

        traceIfFalse "NOTTERMINATED" (OnChainHelpers.isTerminated pParams info poolDatum_In) &&
        
        traceIfFalse "MSBIV" correctInvestAmount_SendBackToUser && 

        correctOutputs_DependingOnCaseIO
    
    where
        ------------------
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        master = T.rmsbdMaster redeemer
        -- userToSendBack = T.rmsbdUserToSendBack redeemer
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        !fundID_CS = txID_Master_Fund_CS
        !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)
        ------------------
        !userID_CS = txID_User_Deposit_CS
        !userID_AC = LedgerValue.AssetClass (userID_CS, T.userID_TN) 
        ------------------
        !txID_Master_SendBackDeposit_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !txID_Master_SendBackDeposit_AC =  LedgerValue.AssetClass (txID_Master_SendBackDeposit_CS, T.txID_Master_SendBackDeposit_TN)
        ------------------
        -- caseIO # : REF INPUTS , NORMAL INPUTS    / OUTPUTS
        -- caseIO 1 : 1 R (1 RPD), 2 (1 FD, 1 UD)   / 1 (1 FD)
        -- caseIO 2 : 0 R        , 2 (1 PD, 1 UD)   / 1 (1 PD)
        ------------------
        !inputNormalAndReference = inputs_TxOut_Values_And_Datums ++ inputsReference_TxOut_Values_And_Datums
        ------------------
        !inputNormalOrReference_TxOut_Value_And_PoolDatum =
            case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum poolID_AC Helpers.getPoolDatumTypo_FromDatum inputNormalAndReference of
                Nothing -> traceError "INRPD"
                Just x  -> x
        ------------------
        !input_TxOut_Value_And_UserDatum =
            case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum userID_AC Helpers.getUserDatumTypo_FromDatum inputs_TxOut_Values_And_Datums of
                Nothing -> traceError "IUD" 
                Just x  -> x
        ------------------
        !inputPD = any  (\(v, _) -> Helpers.isNFT_With_AC_InValue v poolID_AC ) inputs_TxOut_Values_And_Datums
        -- !inputFD = any  (\(v, _) -> Helpers.isToken_With_AC_InValue v fundID_AC ) inputs_TxOut_Values_And_Datums
        ------------------
        !poolDatum_In = OnChainNFTHelpers.getTxOut_Datum inputNormalOrReference_TxOut_Value_And_PoolDatum
        ------------------
        !userDatum_In = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_UserDatum
        ------------------
        !user_UserDatum_In = T.udUser userDatum_In
        ------------------
        -- !investAmount = T.udInvest userDatum_In
        ------------------
        -- !staking_CS = T.ppStaking_CS pParams
        -- !staking_AC = LedgerValue.AssetClass (staking_CS, T.ppStaking_TN pParams)
        -- !stakingIsAda = LedgerApiV2.adaSymbol == staking_CS
        -- !stakingIsWithoutTokenName = not stakingIsAda && T.ppStaking_TN pParams == LedgerApiV2.TokenName emptyByteString
        ---------------------
        !value_In_UserDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_UserDatum
        ---------------------
        -- !value_InvestAmount = Helpers.createValueAddingTokensOfCurrencySymbol staking_AC staking_CS stakingIsWithoutTokenName value_In_UserDatum investAmount
        -- ---------------------
        -- !mindAda_In_UserDatum = T.udMinAda userDatum_In
        -- !value_MinAda_In_UserDatum = LedgerAda.lovelaceValueOf mindAda_In_UserDatum
        ---------------------
        -- !value_InvestAmountPlusAda = value_InvestAmount <> value_MinAda_In_UserDatum
        -- !value_For_UserToSendBackDeposit = value_InvestAmountPlusAda
        ------------------
        !txID_User_Harvest_AC = LedgerValue.AssetClass (txID_User_Harvest_CS, T.txID_User_Harvest_TN)
        !value_Of_TxID_User_Harvest = Helpers.getValueOfAC value_In_UserDatum txID_User_Harvest_AC
        !value_For_Mint_TxID_Master_SendBackDeposit = LedgerValue.assetClassValue txID_Master_SendBackDeposit_AC 1
        ------------------
        correctInvestAmount_SendBackToUser :: Bool
        !correctInvestAmount_SendBackToUser = 
            let 
                value_For_UserID = LedgerValue.assetClassValue userID_AC 1
                value_For_UserToSendBackDeposit = value_In_UserDatum <> negate value_Of_TxID_User_Harvest <> negate value_For_UserID
            in
                Helpers.valueIncludesValue (LedgerContextsV2.valuePaidTo info user_UserDatum_In) value_For_UserToSendBackDeposit
        ------------------
        correctOutputs_DependingOnCaseIO :: Bool
        !correctOutputs_DependingOnCaseIO 
            | inputPD =
                -- PD is in the input, as normal, must be at output    
                let
                    ------------------
                    !isFundCountZero = T.pdFundCount poolDatum_In == 0
                    ------------------
                    !output_TxOut_Value_And_PoolDatum =
                         case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum poolID_AC Helpers.getPoolDatumTypo_FromDatum outputs_TxOut_Values_And_Datums of
                            Nothing -> traceError "OPD" 
                            Just x  -> x
                    ------------------
                    correctIO :: Bool
                    correctIO =
                        -- caseIO # : REF INPUTS , NORMAL INPUTS    / OUTPUTS
                        -- caseIO 2 : 0 R        , 2 (1 PD, 1 UD)   / 1 (1 PD)
                        (length inputs_TxOut_Values_And_Datums == T.const_1_PD + T.const_1_UD ) && 
                        (length outputs_TxOut_Values_And_Datums == T.const_1_PD               )
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
                            !value_In_PoolDatum = OnChainNFTHelpers.getTxOut_Value inputNormalOrReference_TxOut_Value_And_PoolDatum
                            !value_For_PoolDatum_Control = value_In_PoolDatum <> value_For_Mint_TxID_Master_SendBackDeposit <> value_Of_TxID_User_Harvest
                            !value_For_PoolDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_PoolDatum
                        in
                            Helpers.valueEqualsValue value_For_PoolDatum_Real value_For_PoolDatum_Control
                    in
                        traceIfFalse "WIO" correctIO &&
                        traceIfFalse "FCNZ" isFundCountZero &&
                        traceIfFalse "PD" correctOutput_PoolDatum_NotChanged && 
                        traceIfFalse "PDV" correctOutput_PoolDatum_Value_WithTokens
            | otherwise =
            -- | inputFD =
                -- FD is in the input, as normal, must be at output
                    let 
                        !input_TxOut_Value_And_FundDatum =
                            case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum fundID_AC Helpers.getFundDatumTypo_FromDatum inputs_TxOut_Values_And_Datums of
                                Nothing -> traceError "IFD" 
                                Just x  -> x
                        ------------------
                        !output_TxOut_Value_And_FundDatum =
                            case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum fundID_AC Helpers.getFundDatumTypo_FromDatum outputs_TxOut_Values_And_Datums of
                                Nothing -> traceError "OFD"
                                Just x  -> x
                        ------------------
                        correctIO :: Bool
                        correctIO =
                            -- caseIO # : REF INPUTS , NORMAL INPUTS    / OUTPUTS
                            -- caseIO 1 : 1 R (1 RPD), 2 (1 FD, 1 UD)   / 1 (1 FD)
                            (length inputs_TxOut_Values_And_Datums == T.const_1_FD + T.const_1_UD ) && 
                            (length outputs_TxOut_Values_And_Datums == T.const_1_FD               )
                        ------------------ 
                        correctOutput_FundDatum_NotChanged :: Bool
                        !correctOutput_FundDatum_NotChanged =
                            let
                                !fundDatum_Out_Control = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_FundDatum
                                !fundDatum_Out_Real = OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_FundDatum
                            in
                                fundDatum_Out_Real == fundDatum_Out_Control
                        ------------------
                        correctOutputFundDatum_Value_WithTokens :: Bool
                        !correctOutputFundDatum_Value_WithTokens =
                             let
                                !value_In_FundDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_FundDatum
                                !value_For_FundDatum_Control = value_In_FundDatum <> value_For_Mint_TxID_Master_SendBackDeposit <> value_Of_TxID_User_Harvest
                                !value_For_FundDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_FundDatum
                            in
                                Helpers.valueEqualsValue value_For_FundDatum_Real value_For_FundDatum_Control
                    in
                        traceIfFalse "WIO" correctIO &&
                        traceIfFalse "FD" correctOutput_FundDatum_NotChanged && 
                        traceIfFalse "FDV" correctOutputFundDatum_Value_WithTokens
            -- | otherwise = traceError "WIO"
--------------------------------------------------------------------------------

policy_TxID_Master_SendBackDeposit :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.MintingPolicy
policy_TxID_Master_SendBackDeposit pParams txID_Master_Fund_CS txID_User_Deposit_CS txID_User_Harvest_CS = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams txID_Master_Fund_CS txID_User_Deposit_CS txID_User_Harvest_CS

original_policy :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> Plutonomy.MintingPolicy
original_policy pParams txID_Master_Fund_CS txID_User_Deposit_CS txID_User_Harvest_CS =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkPolicy_TxID_Master_SendBackDeposit ||])
    `PlutusTx.applyCode` PlutusTx.liftCode pParams
    `PlutusTx.applyCode` PlutusTx.liftCode txID_Master_Fund_CS
    `PlutusTx.applyCode` PlutusTx.liftCode txID_User_Deposit_CS
    `PlutusTx.applyCode` PlutusTx.liftCode txID_User_Harvest_CS

--------------------------------------------------------------------------------
