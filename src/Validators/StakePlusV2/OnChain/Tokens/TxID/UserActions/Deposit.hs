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
module Validators.StakePlusV2.OnChain.Tokens.TxID.UserActions.Deposit
(
    policy_TxID_User_Deposit 
)
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Ada                                                 as LedgerAda (lovelaceValueOf)
import qualified Ledger.Value                                               as LedgerValue (assetClassValue, AssetClass (AssetClass), assetClass)
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, adaSymbol, TokenName (..), txOutValue, MintingPolicy) 
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo) 
import qualified PlutusTx                                                   (compile, applyCode, liftCode)
import           PlutusTx.Prelude                                           ( Bool, Maybe(Nothing, Just), Eq((==)), BuiltinData, Semigroup((<>)), (&&), not, emptyByteString, error, traceError, ($), (++), traceIfFalse )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (fromJust, valueFromCurrencySymbol, valueOfCurrencySymbol)
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (getInputsWithDatum, getReferenceInputsWithDatum, getOutputsWithDatum, validateBeginAtReached, isNotClosed, isNotTerminated, validateUserAction, isNFT_Minted_With_AC, isToken_Minted_With_AC_AndAmt, isDateInRange)
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (validateBurn_Token_Own_CS_Any_TN, checkIfAllAreFromSameAddress, getTxOut_Value_And_PoolDatum, getTxOuts_Values_And_UserDatums, getTxOut_Datum, getTxOut_Value)
import qualified Validators.StakePlusV2.Types.Constants                     as T (poolID_TN, userID_TN, txID_User_Deposit_For_User_TN)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, UserDatumTypo (..), mkUserDatumTypo)
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator (RedeemerUserDeposit), RedeemerUserDepositTypo (..))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_User_Deposit #-}
mkPolicy_TxID_User_Deposit :: T.PoolParams -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_User_Deposit pParams mintRedeemerRaw ctxRaw  =
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
                    -- traceIfFalse "INVR" (OnChainNFTHelpers.checkIfAllSpendRedeemersAreEqual ctx redeemer') && 
                
                    case redeemer' of 
                        (T.RedeemerUserDeposit redeemer) ->
                            validateUserDeposit pParams ctx redeemer inputs_TxOut_Values_And_Datums inputsReference_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
                        _ -> traceError "INVOP"-- "Minting TxID: Invalid Operation"
        then ()

        else error ()

--------------------------------------------------------------------------------

{-# INLINABLE validateUserDeposit #-}
validateUserDeposit :: T.PoolParams -> LedgerContextsV2.ScriptContext -> T.RedeemerUserDepositTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] ->[T.TxOut_Value_And_Datum] ->  Bool
validateUserDeposit pParams ctx redeemer _ inputsReference_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums  =

        -- Puede registrarse si ya esta comenzado y no esta cerrado ni terminado
        OnChainHelpers.validateBeginAtReached pParams info &&
        traceIfFalse "CLOSED" (OnChainHelpers.isNotClosed pParams info poolDatum_In )  &&  
        traceIfFalse "TERMINATED" (OnChainHelpers.isNotTerminated pParams info poolDatum_In )  &&  

        -- Generic validation for User actions 
        OnChainHelpers.validateUserAction pParams info user user_UserDatum_Out && --OnChainNFT.curSymbol_UserID 

        -- Check if the UserID minted is there
        traceIfFalse "UserID" (OnChainHelpers.isNFT_Minted_With_AC userID_AC info ) &&  -- "Can't Find Valid UserID" 

        -- Check if the User_Deposit minted is there
        traceIfFalse "UI" (OnChainHelpers.isToken_Minted_With_AC_AndAmt txID_User_Deposit_For_User_AC investAmount info ) &&  -- "Can't Find Valid TxID" 

        -- Check if this the invest created at date is correct.
        traceIfFalse "DATE" (OnChainHelpers.isDateInRange createdAt info) && --"Created at Date is not Valid" 

        -- The new UserDatum must be initialized in the right way
        traceIfFalse "UD" correctOutput_UserDatum_New && --"Wrong New UserDatum" 

        -- Must be the new invest plus the NFT UserID 
        traceIfFalse "UDV" correctOutput_UserDatum_Value_WithInvest --"Wrong UserDatum Value" 

    where

        ------------------
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
        !userID_TN = T.userID_TN
        !userID_AC = LedgerValue.assetClass userID_CS userID_TN
        ------------------
        !txID_User_Deposit_For_User_AC =  LedgerValue.AssetClass (userID_CS, T.txID_User_Deposit_For_User_TN)
        ------------------
        !inputReference_TxOut_Value_And_PoolDatum' = OnChainNFTHelpers.getTxOut_Value_And_PoolDatum poolID_AC inputsReference_TxOut_Values_And_Datums
        !inputReference_TxOut_Value_And_PoolDatum =
            case inputReference_TxOut_Value_And_PoolDatum' of
                Nothing -> traceError "IRPD" -- "Error. Can't find single reference input with PoolDatun"
                _       -> Helpers.fromJust inputReference_TxOut_Value_And_PoolDatum'
        ------------------
        !output_TxOut_Value_And_UserDatum' =
            case OnChainNFTHelpers.getTxOuts_Values_And_UserDatums userID_AC outputs_TxOut_Values_And_Datums of
                [x] -> Just x
                _ -> Nothing
        !output_TxOut_Value_And_UserDatum =
            case output_TxOut_Value_And_UserDatum' of
                Nothing -> traceError "OUD" -- "Error. Can't find single output with UserDatum" 
                _       -> Helpers.fromJust output_TxOut_Value_And_UserDatum'
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
                        !valueInvestAmount = Helpers.valueFromCurrencySymbol value_For_UserDatum_Real staking_CS
                        !investAmount' = Helpers.valueOfCurrencySymbol valueInvestAmount staking_CS
                    ------------------
                        !value_For_UserDatum_Control =  valueInvestAmount <> value_For_Mint_UserID <> valueMinAda
                    in
                        investAmount == investAmount' && value_For_UserDatum_Real == value_For_UserDatum_Control
                else
                    let
                        !staking_AC = LedgerValue.AssetClass (T.ppStaking_CS  pParams, T.ppStaking_TN pParams)
                        !valueInvestAmount = LedgerValue.assetClassValue staking_AC investAmount
                    ------------------
                        !value_For_UserDatum_Control =  valueInvestAmount <> value_For_Mint_UserID <> valueMinAda
                    in
                        value_For_UserDatum_Real == value_For_UserDatum_Control

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
