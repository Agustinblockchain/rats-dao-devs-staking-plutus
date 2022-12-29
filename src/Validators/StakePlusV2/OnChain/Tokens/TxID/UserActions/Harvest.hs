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
module Validators.StakePlusV2.OnChain.Tokens.TxID.UserActions.Harvest
(
    policy_TxID_User_Harvest 
)
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Value                                               as LedgerValue (assetClassValue, AssetClass (AssetClass), assetClass)
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, TokenName (..), txOutValue, MintingPolicy, adaSymbol, CurrencySymbol) 
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo) 
import qualified PlutusTx                                                   (compile, applyCode, liftCode)
import           PlutusTx.Prelude                                           ( Bool, Maybe(Just, Nothing), Eq((==)), BuiltinData, AdditiveGroup((-)), AdditiveSemigroup((+)), Semigroup((<>)), (&&), not, emptyByteString, error, traceError, ($), (.), length, sum, (<$>), (++), sortBy, traceIfFalse )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (fromJust, getRewardsPerInvest)
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (getInputsWithDatum, getReferenceInputsWithDatum, getOutputsWithDatum, isNotTerminated, validateUserAction, isNFT_Minted_With_AC, isDateInRange, correctClaimValue)
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (validateBurn_Token_Own_CS_Any_TN, checkIfAllAreFromSameAddress, checkIfAllSpendRedeemersAreEqual, getTxOut_Value_And_PoolDatum, getTxOuts_Values_And_FundDatums, getTxOuts_Values_And_UserDatums, getTxOut_Datum, sort_Value_And_FundDatum, getFundDatumListWithNewValues, getTxOut_Datum, getTxOut_Value)
import qualified Validators.StakePlusV2.Types.Constants                     as T (poolID_TN, fundID_TN, userID_TN, txID_User_Harvest_TN)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, UserDatumTypo (..), PoolDatumTypo (..), FundDatumTypo (..), mkUserDatum, DatumValidator (UserDatum))
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator (RedeemerUserHarvest), RedeemerUserHarvestTypo (..))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
------------------------------------------------------------------------------------------ 
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_User_Harvest #-}
mkPolicy_TxID_User_Harvest :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_User_Harvest pParams curSymbol_TxID_Master_Fund curSymbol_TxID_User_Deposit mintRedeemerRaw ctxRaw  =
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
                                validateUserHarvest pParams curSymbol_TxID_Master_Fund curSymbol_TxID_User_Deposit ctx redeemer inputs_TxOut_Values_And_Datums inputsReference_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
                            where


                        _ -> traceError "INVOP"-- "Minting TxID: Invalid Operation"
        then ()

        else error ()

--------------------------------------------------------------------------------

{-# INLINABLE validateUserHarvest #-}
validateUserHarvest :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerContextsV2.ScriptContext -> T.RedeemerUserHarvestTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum]  -> Bool
validateUserHarvest pParams curSymbol_TxID_Master_Fund curSymbol_TxID_User_Deposit ctx redeemer inputs_TxOut_Values_And_Datums inputsReference_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums  =

         -- Puede obtener rewards si no esta terminado
        traceIfFalse "TERMINATED" (OnChainHelpers.isNotTerminated pParams info poolDatum_In )  &&

        -- Generic validation for User actions 
        OnChainHelpers.validateUserAction pParams info user user_UserDatum_In && --OnChainNFT.curSymbol_UserID 

        -- the minimux qty possible of fund datums to cover the claimAmount"
        traceIfFalse "IFDQTY" correctInputs_FundDatumsQty &&

        -- Check if the txID_User_Harvest_AC minted is there
        traceIfFalse "UGR" (OnChainHelpers.isNFT_Minted_With_AC txID_User_Harvest_AC  info ) &&  -- "Can't Find Valid TxID" 

        -- Check if this the claim date at is correct.
        traceIfFalse "DATE" (OnChainHelpers.isDateInRange claimAt info) && --"Claim DateAt is not Valid"

        -- Check if this the claim is correct. Must be bigger than minimum claim and less than the rewards acumulated
        traceIfFalse "CLAIM" (OnChainHelpers.correctClaimValue claimAmount totalNewRewards valueCanUse_input_TxOut_FundDatum) && --"Claim Value is not Valid"

        --With New UserID
        traceIfFalse "FDOV" correctOutputs_FundsDatums_And_Values_WithClaim && --"Wrong Updated FundDatum"  or "Wrong FundDatum Value"

        -- Must update with new claim
        traceIfFalse "UD" correctOutput_UserDatum_New && --"Wrong Updated UserDatum"

        -- Must be the same than input
        traceIfFalse "UDV" correctOutput_UserDatum_Value_WithTokens --"Wrong UserDatum Value"

    where

        ------------------
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !user = T.ruhUser redeemer
        !claimAmount = T.ruhClaimAmount redeemer
        !claimAt = T.ruhClaimAt redeemer
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        !fundID_CS = curSymbol_TxID_Master_Fund
        !fundID_TN = T.fundID_TN
        !fundID_AC = LedgerValue.assetClass fundID_CS fundID_TN
        ------------------
        !userID_CS = curSymbol_TxID_User_Deposit
        !userID_TN = T.userID_TN
        !userID_AC = LedgerValue.assetClass userID_CS userID_TN
        ------------------
        !txID_User_GetRewars_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !txID_User_Harvest_AC =  LedgerValue.AssetClass (txID_User_GetRewars_CS, T.txID_User_Harvest_TN)
        ------------------
        !inputReference_TxOut_Value_And_PoolDatum' = OnChainNFTHelpers.getTxOut_Value_And_PoolDatum poolID_AC inputsReference_TxOut_Values_And_Datums
        !inputReference_TxOut_Value_And_PoolDatum =
            case inputReference_TxOut_Value_And_PoolDatum' of
                Nothing -> traceError "IRPD" -- "Error. Can't find single reference input with PoolDatun"
                _       -> Helpers.fromJust inputReference_TxOut_Value_And_PoolDatum'
        ------------------
        !inputs_TxOuts_Values_And_FundDatums' =
            case OnChainNFTHelpers.getTxOuts_Values_And_FundDatums fundID_AC inputs_TxOut_Values_And_Datums of
                []  -> Nothing
                x   -> Just x
        !inputs_TxOuts_Values_And_FundDatums =
            case inputs_TxOuts_Values_And_FundDatums' of
                Nothing -> traceError "IFDS"
                _       -> Helpers.fromJust inputs_TxOuts_Values_And_FundDatums'
        ------------------
        !outputs_TxOuts_Values_And_FundDatums' =
            case OnChainNFTHelpers.getTxOuts_Values_And_FundDatums fundID_AC outputs_TxOut_Values_And_Datums of
                []  -> Nothing
                x   -> Just x
        !outputs_TxOuts_Values_And_FundDatums =
            case outputs_TxOuts_Values_And_FundDatums' of
                Nothing -> traceError "OFDS"
                _       -> Helpers.fromJust outputs_TxOuts_Values_And_FundDatums'
        ------------------
        !input_TxOut_Value_And_UserDatum' =
            case OnChainNFTHelpers.getTxOuts_Values_And_UserDatums userID_AC inputs_TxOut_Values_And_Datums of
                [x] -> Just x
                _ -> Nothing
        !input_TxOut_Value_And_UserDatum =
            case input_TxOut_Value_And_UserDatum' of
                Nothing -> traceError "IUD" -- "Error. Can't find single input with UserDatum"
                _       -> Helpers.fromJust input_TxOut_Value_And_UserDatum'
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
        !valueCanUse_input_TxOut_FundDatum = sum (T.fdFundAmount . OnChainNFTHelpers.getTxOut_Datum  <$> inputs_TxOuts_Values_And_FundDatums) - sum (T.fdCashedOut  . OnChainNFTHelpers.getTxOut_Datum <$> inputs_TxOuts_Values_And_FundDatums)
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
        ------------------
        !calculated_TxOuts_Values_And_FundDatums_Ordered = sortBy OnChainNFTHelpers.sort_Value_And_FundDatum calculated_TxOuts_Values_And_FundDatums
        ----------------
        correctInputs_FundDatumsQty :: Bool
        !correctInputs_FundDatumsQty =
            length calculated_TxOuts_Values_And_FundDatums == length inputs_TxOuts_Values_And_FundDatums
        ----------------
        correctOutputs_FundsDatums_And_Values_WithClaim :: Bool
        !correctOutputs_FundsDatums_And_Values_WithClaim =
            outputs_TxOuts_Values_And_FundDatums_Ordered == calculated_TxOuts_Values_And_FundDatums_Ordered
        ------------------
        correctOutput_UserDatum_New :: Bool
        correctOutput_UserDatum_New =
            let
                !rewardsNotClaimed' = totalNewRewards - claimAmount
            ------------------
                !userDatum_Out_Control = T.mkUserDatum
                    (T.udUser userDatum_In)
                    (T.udInvest userDatum_In)
                    (T.udCreatedAt userDatum_In)
                    (T.udCashedOut userDatum_In + claimAmount)
                    rewardsNotClaimed'
                    (Just claimAt)
                    (T.udMinAda userDatum_In)
            ------------------
                !userDatum_Out_Real = T.UserDatum $ OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_UserDatum
            in
                userDatum_Out_Real == userDatum_Out_Control
        ------------------
        correctOutput_UserDatum_Value_WithTokens :: Bool
        correctOutput_UserDatum_Value_WithTokens =
            let
                !value_In_UserDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_UserDatum
                !value_For_Mint_TxID_User_Harvest = LedgerValue.assetClassValue txID_User_Harvest_AC 1
                !value_For_UserDatum_Control = value_In_UserDatum <> value_For_Mint_TxID_User_Harvest
            ------------------
                !value_For_UserDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_UserDatum
            in
                value_For_UserDatum_Real ==  value_For_UserDatum_Control
                
        ----------------


--------------------------------------------------------------------------------

{-# INLINEABLE policy_TxID_User_Harvest #-}
policy_TxID_User_Harvest :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.MintingPolicy
policy_TxID_User_Harvest pParams curSymbol_TxID_Master_Fund curSymbol_TxID_User_Deposit = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams curSymbol_TxID_Master_Fund curSymbol_TxID_User_Deposit

{-# INLINEABLE original_policy #-}
original_policy :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> Plutonomy.MintingPolicy
original_policy pParams curSymbol_TxID_Master_Fund curSymbol_TxID_User_Deposit =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkPolicy_TxID_User_Harvest ||])
    `PlutusTx.applyCode` PlutusTx.liftCode pParams
    `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_Master_Fund
    `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_User_Deposit

--------------------------------------------------------------------------------
