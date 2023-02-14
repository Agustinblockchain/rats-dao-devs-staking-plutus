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
{-# LANGUAGE ScopedTypeVariables #-}
{- HLINT ignore "Use camelCase" -}
------------------------------------------------------------------------------------------
module Validators.StakePlusV2.OnChain.Tokens.TxID.UserActions.Withdraw
(
    policy_TxID_User_Withdraw
)
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
--import qualified Ledger.Ada                                                 as LedgerAda
import qualified Ledger.Value                                               as LedgerValue (assetClassValue, AssetClass (AssetClass))
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, txOutValue, MintingPolicy, CurrencySymbol)
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo)
import qualified PlutusTx                                                   (compile, applyCode, liftCode)
import           PlutusTx.Prelude                                           ( Bool(..), Maybe(Nothing, Just), Eq((==)), Semigroup((<>)), (&&), (||), traceError, ($), (++), negate, traceIfFalse, length, otherwise, AdditiveSemigroup ((+)), any, null )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (getValueOfAC, valueEqualsValue, isNFT_With_AC_InValue, getUserDatumTypo_FromDatum, getPoolDatumTypo_FromDatum, getFundDatumTypo_FromDatum, isToken_With_AC_InValue)
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (getInputsWithDatum, getReferenceInputsWithDatum, getOutputsWithDatum, validateUserAction, isNFT_Minted_With_AC, isClosed, isToken_Minted_With_AC_AndAmt, isNFT_Burning_With_AC, isTerminated)
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (validateBurn_Token_Own_CS_Any_TN, checkIfAllAreFromSameAddress, checkIfAllSpendRedeemersAreEqual, getTxOut_Datum, getTxOut_Value, getTxOut_Datum, getTxOut_Value, getTxOut_Datum, getTxOut_Value, getTxOut_Value_And_SomeDatum)
import qualified Validators.StakePlusV2.Types.Constants                     as T (poolID_TN, userID_TN, fundID_TN, txID_User_Withdraw_TN, userDeposit_TN, txID_User_Harvest_TN, const_1_PD, const_1_UD, const_1_FD)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, UserDatumTypo (..), PoolDatumTypo (..))
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator (RedeemerUserWithdraw), RedeemerUserWithdrawTypo (..))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
import PlutusTx.Builtins
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_User_Withdraw #-}
mkPolicy_TxID_User_Withdraw :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_User_Withdraw !pParams !txID_Master_Fund_CS !txID_User_Deposit_CS !txID_User_Harvest_CS !mintRedeemerRaw !ctxRaw  =
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
                        (T.RedeemerUserWithdraw redeemer) ->
                            validateUserWithdraw pParams txID_Master_Fund_CS txID_User_Deposit_CS txID_User_Harvest_CS ctx redeemer inputs_TxOut_Values_And_Datums inputsReference_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
                        _ -> traceError "INVOP"
        then ()

        else error ()

--------------------------------------------------------------------------------

{-# INLINABLE validateUserWithdraw #-}
validateUserWithdraw :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerContextsV2.ScriptContext -> T.RedeemerUserWithdrawTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] ->[T.TxOut_Value_And_Datum] ->  Bool
validateUserWithdraw !pParams !txID_Master_Fund_CS !txID_User_Deposit_CS !txID_User_Harvest_CS !ctx !redeemer !inputs_TxOut_Values_And_Datums !inputsReference_TxOut_Values_And_Datums !outputs_TxOut_Values_And_Datums  =
        OnChainHelpers.validateUserAction pParams info user user_UserDatum_In && 
        traceIfFalse "UGI" (OnChainHelpers.isNFT_Minted_With_AC txID_User_Withdraw_AC  info )  &&  
        
        traceIfFalse "BUD" (isBurning_UD || isClosed) &&
        traceIfFalse "BUID" (isBurning_UserID || isTerminated) &&
        
        correctOutputs_DependingOnCaseIO
    where
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        user = T.ruwUser redeemer
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        !fundID_CS = txID_Master_Fund_CS
        !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)
        ------------------
        !userID_CS = txID_User_Deposit_CS
        !userID_AC = LedgerValue.AssetClass (userID_CS, T.userID_TN) 
        ------------------
        !txID_User_Withdraw_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !txID_User_Withdraw_AC =  LedgerValue.AssetClass (txID_User_Withdraw_CS, T.txID_User_Withdraw_TN)
        ------------------
        !txID_User_Deposit_For_User_AC =  LedgerValue.AssetClass (userID_CS, T.userDeposit_TN)
        ------------------
        -- no hay chekeo de tiempos, puede recuperar su inversion/depostio siempre
        -- pero si lo hace antes de que el pool este closed tiene que entregar los tokens de user investAmount que se le dieron
        -- para evitar que se vuelta a registrar y obtenga nuevos tokens que pueden ser funcionales en algun optro sitio
        !isBurning_UD = OnChainHelpers.isToken_Minted_With_AC_AndAmt txID_User_Deposit_For_User_AC (negate investAmount) info
        !isBurning_UserID = OnChainHelpers.isNFT_Burning_With_AC userID_AC info
        ------------------
        -- caseIO # : REF INPUTS , NORMAL INPUTS    / OUTPUTS
        -- caseIO 1 : 0 R        , 1 (1 UD)         / 0
        -- caseIO 2 : 1 R (1 RPD), 1 (1 UD)         / 0
        -- caseIO 3 : 1 R (1 RPD), 2 (1 FD, 1 UD)   / 1 (1 FD)
        -- caseIO 4 : 0 R        , 2 (1 FD, 1 UD)   / 1 (1 FD)
        -- caseIO 5 : 0 R        , 2 (1 PD, 1 UD)   / 1 (1 PD)
        ------------------
        !inputPDRef = any  (\(v, _) -> Helpers.isNFT_With_AC_InValue v poolID_AC ) inputsReference_TxOut_Values_And_Datums
        !inputPD = any  (\(v, _) -> Helpers.isNFT_With_AC_InValue v poolID_AC ) inputs_TxOut_Values_And_Datums
        !inputFD = any  (\(v, _) -> Helpers.isToken_With_AC_InValue v fundID_AC ) inputs_TxOut_Values_And_Datums
        !inputPDRefOrNormal = inputPDRef || inputPD
        ------------------
        !input_TxOut_Value_And_UserDatum =
            case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum userID_AC Helpers.getUserDatumTypo_FromDatum inputs_TxOut_Values_And_Datums of
                Nothing -> traceError "IUD" 
                Just x  -> x
        ------------------
        !userDatum_In = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_UserDatum
        ------------------
        !user_UserDatum_In = T.udUser userDatum_In
        ------------------
        !investAmount = T.udInvest userDatum_In
        ------------------
        !(isClosed, isTerminated)
            | inputPDRefOrNormal =
                -- PD is in the input, as ref or normal
                let
                    !inputNormalAndReference = inputs_TxOut_Values_And_Datums ++ inputsReference_TxOut_Values_And_Datums
                    !inputNormalOrReference_TxOut_Value_And_PoolDatum =
                        case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum poolID_AC Helpers.getPoolDatumTypo_FromDatum inputNormalAndReference of
                            Nothing -> traceError "INRPD" 
                            Just x  -> x
                    ------------------
                    !poolDatum_In = OnChainNFTHelpers.getTxOut_Datum inputNormalOrReference_TxOut_Value_And_PoolDatum
                    ------------------
                    !isClosed' = OnChainHelpers.isClosed pParams info poolDatum_In
                    !isTerminated' = OnChainHelpers.isTerminated pParams info poolDatum_In
                in
                    (isClosed', isTerminated')
            | otherwise =
                (False, False)
        ------------------
        correctOutputs_DependingOnCaseIO :: Bool
        !correctOutputs_DependingOnCaseIO
            | inputPD =
                -- PD is in the input, as normal, must be at output
                let
                    !input_TxOut_Value_And_PoolDatum =
                        case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum poolID_AC Helpers.getPoolDatumTypo_FromDatum inputs_TxOut_Values_And_Datums of
                            Nothing ->  traceError "IPD"
                            Just x  -> x
                    ------------------
                    !output_TxOut_Value_And_PoolDatum =
                         case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum poolID_AC Helpers.getPoolDatumTypo_FromDatum outputs_TxOut_Values_And_Datums of
                            Nothing -> traceError "OPD" 
                            Just x  -> x
                    ------------------
                    !poolDatum_In = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_PoolDatum
                    ------------------
                    !isFundCountZero = T.pdFundCount poolDatum_In == 0
                    ------------------
                    correctIO :: Bool
                    correctIO =
                        -- case # : REF INPUTS , NORMAL INPUTS    / OUTPUTS
                        -- caseIO 5 : 0 R        , 2 (1 PD, 1 UD)   / 1 (1 PD)
                        length inputs_TxOut_Values_And_Datums == T.const_1_PD + T.const_1_UD && 
                        length outputs_TxOut_Values_And_Datums == T.const_1_PD 
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
                            !value_In_UserDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_UserDatum
                        ---------------------
                            !txID_User_Harvest_AC = LedgerValue.AssetClass (txID_User_Harvest_CS, T.txID_User_Harvest_TN)
                        ---------------------
                            !value_Of_TxID_User_Harvest = Helpers.getValueOfAC value_In_UserDatum txID_User_Harvest_AC
                        ---------------------
                            !value_For_Mint_TxID_User_Withdraw = LedgerValue.assetClassValue txID_User_Withdraw_AC 1
                        ---------------------
                            !value_In_PoolDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_PoolDatum
                            !value_For_PoolDatum_Control = value_In_PoolDatum <> value_For_Mint_TxID_User_Withdraw <> value_Of_TxID_User_Harvest
                        ---------------------
                            !value_For_PoolDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_PoolDatum
                        in
                            Helpers.valueEqualsValue value_For_PoolDatum_Real value_For_PoolDatum_Control
                in
                    traceIfFalse "WIO" correctIO &&
                    traceIfFalse "FCNZ" isFundCountZero &&
                    traceIfFalse "PD" correctOutput_PoolDatum_NotChanged && 
                    traceIfFalse "PDV" correctOutput_PoolDatum_Value_WithTokens
            | inputFD =
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
                        -- caseIO 3 : 1 R (1 RPD), 2 (1 FD, 1 UD)   / 1 (1 FD)
                        -- caseIO 4 : 0 R        , 2 (1 FD, 1 UD)   / 1 (1 FD)
                        length inputs_TxOut_Values_And_Datums == T.const_1_FD + T.const_1_UD && 
                        length outputs_TxOut_Values_And_Datums == T.const_1_FD
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
                            !value_In_UserDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_UserDatum
                        ---------------------
                            !txID_User_Harvest_AC = LedgerValue.AssetClass (txID_User_Harvest_CS, T.txID_User_Harvest_TN)
                        ---------------------
                            !value_Of_TxID_User_Harvest = Helpers.getValueOfAC value_In_UserDatum txID_User_Harvest_AC
                        ---------------------
                            !value_For_Mint_TxID_User_Withdraw = LedgerValue.assetClassValue txID_User_Withdraw_AC 1
                        ---------------------
                            !value_In_FundDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_FundDatum
                            !value_For_FundDatum_Control = value_In_FundDatum <> value_For_Mint_TxID_User_Withdraw <> value_Of_TxID_User_Harvest
                        ---------------------
                            !value_For_FundDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_FundDatum
                        in
                            Helpers.valueEqualsValue value_For_FundDatum_Real value_For_FundDatum_Control
                in
                    traceIfFalse "WIO" correctIO &&
                    traceIfFalse "FD" correctOutput_FundDatum_NotChanged && 
                    traceIfFalse "FDV" correctOutputFundDatum_Value_WithTokens
            | otherwise =
                -- no FD or PD in the input, no FD or PD at output  
                let
                    correctIO :: Bool
                    correctIO =
                        -- caseIO # : REF INPUTS , NORMAL INPUTS    / OUTPUTS
                        -- caseIO 1 : 0 R        , 1 (1 UD)         / 0
                        -- caseIO 2 : 1 R (1 RPD), 1 (1 UD)         / 0
                        length inputs_TxOut_Values_And_Datums == T.const_1_UD && 
                        null outputs_TxOut_Values_And_Datums  
                in
                    traceIfFalse "WIO" correctIO

--------------------------------------------------------------------------------

policy_TxID_User_Withdraw :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.MintingPolicy
policy_TxID_User_Withdraw pParams txID_Master_Fund_CS txID_User_Deposit_CS txID_User_Harvest_CS = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams txID_Master_Fund_CS txID_User_Deposit_CS txID_User_Harvest_CS

original_policy :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> Plutonomy.MintingPolicy
original_policy pParams txID_Master_Fund_CS txID_User_Deposit_CS txID_User_Harvest_CS =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkPolicy_TxID_User_Withdraw ||])
    `PlutusTx.applyCode` PlutusTx.liftCode pParams
    `PlutusTx.applyCode` PlutusTx.liftCode txID_Master_Fund_CS
    `PlutusTx.applyCode` PlutusTx.liftCode txID_User_Deposit_CS
    `PlutusTx.applyCode` PlutusTx.liftCode txID_User_Harvest_CS

--------------------------------------------------------------------------------
