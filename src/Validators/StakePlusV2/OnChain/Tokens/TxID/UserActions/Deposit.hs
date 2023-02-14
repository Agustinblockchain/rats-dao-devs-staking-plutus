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
module Validators.StakePlusV2.OnChain.Tokens.TxID.UserActions.Deposit
(
    policy_TxID_User_Deposit
)
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Ada                                                 as LedgerAda (lovelaceValueOf)
import qualified Ledger.Value                                               as LedgerValue (assetClassValue, AssetClass (AssetClass))
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, adaSymbol, TokenName (..), txOutValue, MintingPolicy)
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo)
import qualified PlutusTx                                                   (compile, applyCode, liftCode)
import           PlutusTx.Prelude                                           ( Bool, Maybe(Nothing, Just), Eq((==)), BuiltinData, Semigroup((<>)), (&&), not, emptyByteString, error, traceError, ($), (++), traceIfFalse, null, length )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (getValueOfCurrencySymbol, getAmtOfCurrencySymbol, valueEqualsValue, getPoolDatumTypo_FromDatum, getUserDatumTypo_FromDatum)
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (getInputsWithDatum, getReferenceInputsWithDatum, getOutputsWithDatum, validateBeginAtReached, isNotClosed, validateUserAction, isNFT_Minted_With_AC, isToken_Minted_With_AC_AndAmt, isDateInRange)
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (validateBurn_Token_Own_CS_Any_TN, checkIfAllAreFromSameAddress, getTxOut_Datum, getTxOut_Value, getTxOut_Value_And_SomeDatum, checkIfSpendRedeemersIsEmpty)
import qualified Validators.StakePlusV2.Types.Constants                     as T (poolID_TN, userID_TN, userDeposit_TN, const_1_UD)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, UserDatumTypo (..), mkUserDatumTypo)
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator (RedeemerUserDeposit), RedeemerUserDepositTypo (..))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_User_Deposit #-}
mkPolicy_TxID_User_Deposit :: T.PoolParams -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_User_Deposit !pParams !mintRedeemerRaw !ctxRaw  =
    let
        !mintRedeemer = LedgerApiV2.unsafeFromBuiltinData @T.Redeemer_TxID mintRedeemerRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        -- !txOutsInputs = [  LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        -- !result = OnChainHelpers.tracetxOuts txOutsInputs ctx
        -- !txOutsInputsRef = [  LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoReferenceInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        -- !result2 = OnChainHelpers.tracetxOuts txOutsInputsRef ctx
        -- !result3 = OnChainHelpers.tracetxOuts (LedgerApiV2.txInfoOutputs (LedgerContextsV2.scriptContextTxInfo ctx)) ctx
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
                    traceIfFalse "INVR" (OnChainNFTHelpers.checkIfSpendRedeemersIsEmpty ctx) && 

                    case redeemer' of
                        (T.RedeemerUserDeposit redeemer) ->
                            validateUserDeposit pParams ctx redeemer inputs_TxOut_Values_And_Datums inputsReference_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
                        _ -> traceError "INVOP"
        then ()

        else error ()

--------------------------------------------------------------------------------

{-# INLINABLE validateUserDeposit #-}
validateUserDeposit :: T.PoolParams -> LedgerContextsV2.ScriptContext -> T.RedeemerUserDepositTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] ->[T.TxOut_Value_And_Datum] ->  Bool
validateUserDeposit !pParams !ctx !redeemer !inputs_TxOut_Values_And_Datums !inputsReference_TxOut_Values_And_Datums !outputs_TxOut_Values_And_Datums  =
        OnChainHelpers.validateUserAction pParams info user user_UserDatum_Out &&
        traceIfFalse "WIO" correctIO &&
        traceIfFalse "UserID" (OnChainHelpers.isNFT_Minted_With_AC userID_AC info ) &&
        traceIfFalse "UI" (OnChainHelpers.isToken_Minted_With_AC_AndAmt userDeposit_AC investAmount info ) &&  

        OnChainHelpers.validateBeginAtReached pParams info &&
        traceIfFalse "CLOSED" (OnChainHelpers.isNotClosed pParams info poolDatum_In )  &&

        traceIfFalse "DATE" (OnChainHelpers.isDateInRange createdAt info) && 
        traceIfFalse "UD" correctOutput_UserDatum_New && 
        traceIfFalse "UDV" correctOutput_UserDatum_Value_WithInvest 
    where
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !user = T.rudUser redeemer
        !investAmount = T.rudInvestAmount redeemer
        !createdAt = T.rudCreatedAt redeemer
        !minAda = T.rudMinAda redeemer
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        !userID_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !userID_AC = LedgerValue.AssetClass (userID_CS, T.userID_TN)
        ------------------
        !userDeposit_AC =  LedgerValue.AssetClass (userID_CS, T.userDeposit_TN)
        ------------------
        !inputReference_TxOut_Value_And_PoolDatum =
            case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum poolID_AC Helpers.getPoolDatumTypo_FromDatum inputsReference_TxOut_Values_And_Datums of
                Nothing -> traceError "IRPD" 
                Just x  -> x
        ------------------
        !output_TxOut_Value_And_UserDatum =
            case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum userID_AC Helpers.getUserDatumTypo_FromDatum outputs_TxOut_Values_And_Datums of
                Nothing -> traceError "OUD" 
                Just x  -> x
        ------------------
        correctIO :: Bool
        correctIO =
            -- caseIO # : REF INPUTS , NORMAL INPUTS      / OUTPUTS
            -- caseIO 1 : 1 R (1 RPD), 0                  / 1 (1 UD)
            null inputs_TxOut_Values_And_Datums &&
            length outputs_TxOut_Values_And_Datums == T.const_1_UD 
        ------------------
        !poolDatum_In = OnChainNFTHelpers.getTxOut_Datum inputReference_TxOut_Value_And_PoolDatum
        ------------------
        !userDatum_Out = OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_UserDatum
        ------------------
        !user_UserDatum_Out = T.udUser userDatum_Out
        ------------------
        correctOutput_UserDatum_New :: Bool
        !correctOutput_UserDatum_New =
            userDatum_Out == T.mkUserDatumTypo user investAmount createdAt 0 0 Nothing minAda
        ------------------
        correctOutput_UserDatum_Value_WithInvest :: Bool
        !correctOutput_UserDatum_Value_WithInvest =
            let
                !value_For_UserDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_UserDatum
            ------------------
                !value_For_Mint_UserID = LedgerValue.assetClassValue userID_AC 1
            ------------------
                !valueMinAda = LedgerAda.lovelaceValueOf minAda
            ------------------
                !staking_CS = T.ppStaking_CS pParams
                !stakingIsAda = LedgerApiV2.adaSymbol == staking_CS
                !stakingIsWithoutTokenName = not stakingIsAda &&  T.ppStaking_TN pParams == LedgerApiV2.TokenName emptyByteString
            in
                if stakingIsWithoutTokenName then
                    let
                        !valueInvestAmount = Helpers.getValueOfCurrencySymbol value_For_UserDatum_Real staking_CS
                        !investAmount' = Helpers.getAmtOfCurrencySymbol valueInvestAmount staking_CS
                    ------------------
                        !value_For_UserDatum_Control =  valueInvestAmount <> value_For_Mint_UserID <> valueMinAda
                    in
                        investAmount == investAmount' && Helpers.valueEqualsValue value_For_UserDatum_Real value_For_UserDatum_Control
                else
                    let
                        !staking_AC = LedgerValue.AssetClass (T.ppStaking_CS  pParams, T.ppStaking_TN pParams)
                        !valueInvestAmount = LedgerValue.assetClassValue staking_AC investAmount
                    ------------------
                        !value_For_UserDatum_Control =  valueInvestAmount <> value_For_Mint_UserID <> valueMinAda
                    in
                        Helpers.valueEqualsValue value_For_UserDatum_Real value_For_UserDatum_Control

-- --------------------------------------------------------------------------------

{-# INLINEABLE policy_TxID_User_Deposit #-}
policy_TxID_User_Deposit :: T.PoolParams -> LedgerApiV2.MintingPolicy
policy_TxID_User_Deposit pParams = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams

{-# INLINEABLE original_policy #-}
original_policy :: T.PoolParams -> Plutonomy.MintingPolicy
original_policy pParams =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkPolicy_TxID_User_Deposit ||])
    `PlutusTx.applyCode` PlutusTx.liftCode pParams

--------------------------------------------------------------------------------
