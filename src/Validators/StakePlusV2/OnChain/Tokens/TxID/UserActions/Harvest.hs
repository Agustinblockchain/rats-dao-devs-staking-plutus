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

{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}

{- HLINT ignore "Use camelCase" -}


------------------------------------------------------------------------------------------
module Validators.StakePlusV2.OnChain.Tokens.TxID.UserActions.Harvest
(
    policy_TxID_User_Harvest
)
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Value                                               as LedgerValue (assetClassValue, AssetClass (AssetClass))
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, TokenName (..), txOutValue, MintingPolicy, adaSymbol, CurrencySymbol)
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo)
import qualified PlutusTx                                                   (compile, applyCode, liftCode)

import           PlutusTx.Prelude                                           ( Bool, Maybe(Just, Nothing), Eq((==)), BuiltinData, AdditiveGroup((-)), AdditiveSemigroup((+)), Semigroup((<>)), (&&), not, emptyByteString, error, traceError, ($), length, sum, (<$>), (++), sortBy, traceIfFalse, all, any )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (getRewardsPerInvest, getFundAmountCanUse_in_FundDatum, valueEqualsValue, getPoolDatumTypo_FromDatum, getUserDatumTypo_FromDatum, getFundDatumTypo_FromDatum)
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (getInputsWithDatum, getReferenceInputsWithDatum, getOutputsWithDatum, isNotTerminated, validateUserAction, isNFT_Minted_With_AC, isDateInRange, correctClaimValue)
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (validateBurn_Token_Own_CS_Any_TN, checkIfAllAreFromSameAddress, checkIfAllSpendRedeemersAreEqual, getTxOut_Datum, sort_Value_And_FundDatum, getFundDatumListWithNewValues, getTxOut_Datum, getTxOut_Value, getTxOut_Value_And_SomeDatum, getTxOuts_Values_And_SomeDatums)
import qualified Validators.StakePlusV2.Types.Constants                     as T (poolID_TN, fundID_TN, userID_TN, txID_User_Harvest_TN, const_1_UD)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, UserDatumTypo (..), PoolDatumTypo (..), mkUserDatumTypo)
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator (RedeemerUserHarvest), RedeemerUserHarvestTypo (..))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
------------------------------------------------------------------------------------------ 
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_User_Harvest #-}
mkPolicy_TxID_User_Harvest :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_User_Harvest !pParams !txID_Master_Fund_CS !txID_User_Deposit_CS !mintRedeemerRaw !ctxRaw  =
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
                            (T.RedeemerUserHarvest redeemer) ->
                                validateUserHarvest pParams txID_Master_Fund_CS txID_User_Deposit_CS ctx redeemer inputs_TxOut_Values_And_Datums inputsReference_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
                            _ -> traceError "INVOP"
        then ()

        else error ()

--------------------------------------------------------------------------------

{-# INLINABLE validateUserHarvest #-}
validateUserHarvest :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerContextsV2.ScriptContext -> T.RedeemerUserHarvestTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> Bool
validateUserHarvest !pParams !txID_Master_Fund_CS !txID_User_Deposit_CS !ctx !redeemer !inputs_TxOut_Values_And_Datums !inputsReference_TxOut_Values_And_Datums !outputs_TxOut_Values_And_Datums =
        OnChainHelpers.validateUserAction pParams info user user_UserDatum_In &&
        traceIfFalse "WIO" correctIO &&
        traceIfFalse "UGR" (OnChainHelpers.isNFT_Minted_With_AC txID_User_Harvest_AC  info ) &&  

        traceIfFalse "TERMINATED" (OnChainHelpers.isNotTerminated pParams info poolDatum_In )  &&
        
        traceIfFalse "IFDQTY" correctInputs_FundDatumsQty &&
        traceIfFalse "DATE" (OnChainHelpers.isDateInRange claimAt info) &&
        traceIfFalse "CLAIM" (OnChainHelpers.correctClaimValue claimAmount totalNewRewards valueCanUse_input_TxOut_FundDatum) &&
        traceIfFalse "FDOV" correctOutputs_FundsDatums_And_Values_WithClaim &&
        traceIfFalse "UD" correctOutput_UserDatum_New && 
        traceIfFalse "UDV" correctOutput_UserDatum_Value_WithTokens 
    where
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !user = T.ruhUser redeemer
        !claimAmount = T.ruhClaimAmount redeemer
        !claimAt = T.ruhClaimAt redeemer
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        !fundID_CS = txID_Master_Fund_CS
        !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)
        ------------------
        !userID_CS = txID_User_Deposit_CS
        !userID_AC = LedgerValue.AssetClass (userID_CS, T.userID_TN)
        ------------------
        !txID_User_Harvest_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !txID_User_Harvest_AC =  LedgerValue.AssetClass (txID_User_Harvest_CS, T.txID_User_Harvest_TN)
        ------------------
        !inputReference_TxOut_Value_And_PoolDatum =
            case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum poolID_AC Helpers.getPoolDatumTypo_FromDatum inputsReference_TxOut_Values_And_Datums of
                Nothing -> traceError "IRPD" 
                Just x  -> x
        ------------------
        !inputs_TxOuts_Values_And_FundDatums =
            case OnChainNFTHelpers.getTxOuts_Values_And_SomeDatums fundID_AC Helpers.getFundDatumTypo_FromDatum inputs_TxOut_Values_And_Datums of
                Nothing -> traceError "IFDS"
                Just x  -> x
        ------------------
        !outputs_TxOuts_Values_And_FundDatums =
            case OnChainNFTHelpers.getTxOuts_Values_And_SomeDatums fundID_AC Helpers.getFundDatumTypo_FromDatum outputs_TxOut_Values_And_Datums of
                Nothing -> traceError "OFDS"
                Just x  -> x
        ------------------
        !input_TxOut_Value_And_UserDatum =
            case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum userID_AC Helpers.getUserDatumTypo_FromDatum inputs_TxOut_Values_And_Datums of
                Nothing -> traceError "IUD"
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
            -- caseIO 1 : 1 R (1 RPD), 2+ (1+ FD, 1 UD)   / 2+ (1+ FD, 1 UD)
            length inputs_TxOut_Values_And_Datums  == length inputs_TxOuts_Values_And_FundDatums + T.const_1_UD && 
            length outputs_TxOut_Values_And_Datums == length outputs_TxOuts_Values_And_FundDatums + T.const_1_UD && 
            length inputs_TxOut_Values_And_Datums  == length outputs_TxOut_Values_And_Datums
        ------------------
        !poolDatum_In = OnChainNFTHelpers.getTxOut_Datum inputReference_TxOut_Value_And_PoolDatum
        ------------------
        !pdClosedAt = T.pdClosedAt poolDatum_In
        ------------------
        !userDatum_In = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_UserDatum
        ------------------
        !user_UserDatum_In = T.udUser userDatum_In
        ------------------
        !rewardsNotClaimed = T.udRewardsNotClaimed userDatum_In
        !rewards = Helpers.getRewardsPerInvest (T.ppDeadline pParams) pdClosedAt  (T.ppInterestRates pParams) (T.udLastClaimAt userDatum_In) claimAt (T.udCreatedAt userDatum_In) (T.udInvest userDatum_In ) rewardsNotClaimed
        !totalNewRewards = rewards  + rewardsNotClaimed
        ------------------
        !valueCanUse_input_TxOut_FundDatum = sum [Helpers.getFundAmountCanUse_in_FundDatum fd | fd <- OnChainNFTHelpers.getTxOut_Datum <$> inputs_TxOuts_Values_And_FundDatums]
        ------------------
        !harvest_CS =  T.ppHarvest_CS pParams
        !harvest_AC = LedgerValue.AssetClass (harvest_CS, T.ppHarvest_TN pParams)
        !harvestIsAda = LedgerApiV2.adaSymbol == harvest_CS
        !haverstIsWithoutTokenName = not harvestIsAda && T.ppHarvest_TN pParams == LedgerApiV2.TokenName emptyByteString
        ------------------
        !inputs_TxOuts_Values_And_FundDatums_Ordered = sortBy OnChainNFTHelpers.sort_Value_And_FundDatum inputs_TxOuts_Values_And_FundDatums
        !outputs_TxOuts_Values_And_FundDatums_Ordered = sortBy OnChainNFTHelpers.sort_Value_And_FundDatum outputs_TxOuts_Values_And_FundDatums
        ------------------
        !calculated_TxOuts_Values_And_FundDatums = OnChainNFTHelpers.getFundDatumListWithNewValues harvest_AC harvest_CS haverstIsWithoutTokenName inputs_TxOuts_Values_And_FundDatums_Ordered claimAmount
        ----------------
        correctInputs_FundDatumsQty :: Bool
        correctInputs_FundDatumsQty =
            -- the minimux qty possible of fund datums to cover the claimAmount"
            length calculated_TxOuts_Values_And_FundDatums == length inputs_TxOuts_Values_And_FundDatums
        ----------------
        correctOutputs_FundsDatums_And_Values_WithClaim :: Bool
        correctOutputs_FundsDatums_And_Values_WithClaim =
            let
                !calculated_TxOuts_Values_And_FundDatums_Ordered = sortBy OnChainNFTHelpers.sort_Value_And_FundDatum calculated_TxOuts_Values_And_FundDatums
            in
                length outputs_TxOuts_Values_And_FundDatums_Ordered == length calculated_TxOuts_Values_And_FundDatums_Ordered
                &&
                all (
                    \(v, d) ->
                        -- isJust (find (\(v', d') ->   d == d' && Helpers.valueEqualsValue v v' ) outputs_TxOuts_Values_And_FundDatums_Ordered)
                        any (
                            \(v', d') ->
                                d == d' && Helpers.valueEqualsValue v v'
                            ) outputs_TxOuts_Values_And_FundDatums_Ordered
                ) calculated_TxOuts_Values_And_FundDatums_Ordered
        ------------------
        correctOutput_UserDatum_New :: Bool
        correctOutput_UserDatum_New =
            let
                !rewardsNotClaimed' = totalNewRewards - claimAmount
            ------------------
                !userDatum_Out_Control = T.mkUserDatumTypo
                    (T.udUser userDatum_In)
                    (T.udInvest userDatum_In)
                    (T.udCreatedAt userDatum_In)
                    (T.udCashedOut userDatum_In + claimAmount)
                    rewardsNotClaimed'
                    (Just claimAt)
                    (T.udMinAda userDatum_In)
            ------------------
                !userDatum_Out_Real = OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_UserDatum
            in
                userDatum_Out_Real == userDatum_Out_Control
        ----------------
        correctOutput_UserDatum_Value_WithTokens :: Bool
        correctOutput_UserDatum_Value_WithTokens =
            let
                !value_In_UserDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_UserDatum
                !value_For_Mint_TxID_User_Harvest = LedgerValue.assetClassValue txID_User_Harvest_AC 1
                !value_For_UserDatum_Control = value_In_UserDatum <> value_For_Mint_TxID_User_Harvest
            ------------------
                !value_For_UserDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_UserDatum
            in
                Helpers.valueEqualsValue value_For_UserDatum_Real value_For_UserDatum_Control

--------------------------------------------------------------------------------

{-# INLINEABLE policy_TxID_User_Harvest #-}
policy_TxID_User_Harvest :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.MintingPolicy
policy_TxID_User_Harvest pParams txID_Master_Fund_CS txID_User_Deposit_CS = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams txID_Master_Fund_CS txID_User_Deposit_CS

{-# INLINEABLE original_policy #-}
original_policy :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> Plutonomy.MintingPolicy
original_policy pParams txID_Master_Fund_CS txID_User_Deposit_CS =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkPolicy_TxID_User_Harvest ||])
    `PlutusTx.applyCode` PlutusTx.liftCode pParams
    `PlutusTx.applyCode` PlutusTx.liftCode txID_Master_Fund_CS
    `PlutusTx.applyCode` PlutusTx.liftCode txID_User_Deposit_CS

--------------------------------------------------------------------------------
