-- {-# LANGUAGE DataKinds                  #-}
-- {-# LANGUAGE DeriveAnyClass             #-}
-- {-# LANGUAGE DeriveGeneric              #-}
-- {-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase                 #-}
-- {-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
-- {-# LANGUAGE RecordWildCards            #-}
-- {-# LANGUAGE ScopedTypeVariables        #-}
-- {-# LANGUAGE TemplateHaskell            #-}
-- {-# LANGUAGE TypeApplications           #-}
-- {-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE TupleSections              #-}
-- {-# LANGUAGE AllowAmbiguousTypes        #-}
-- {-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{- HLINT ignore "Use camelCase" -}
------------------------------------------------------------------------------------------
module Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Ada                                                 as LedgerAda
import qualified Ledger.Value                                               as LedgerValue
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ScriptContext (scriptContextTxInfo), TxInfo, ownCurrencySymbol) 
import qualified PlutusTx.AssocMap                                          as TxAssocMap
import           PlutusTx.Prelude                                           ( Bool(..), Integer, Maybe(..), Ordering(GT, LT), Eq((==)), Ord((>=), (<), (>)), AdditiveGroup((-)), Semigroup((<>)), (&&), not, ($), fst, snd, all, any, null, head, tail, negate, traceIfFalse )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (datumIs_PoolDatum, datumIs_FundDatum, datumIs_UserDatum, fromJust, isNFT_With_AC_InValue, isToken_With_AC_InValue, getPoolDatumTypo_FromDatum, getFundDatumTypo_FromDatum, getUserDatumTypo_FromDatum, getFundAmountCanUse_in_FundDatum, createValueAddingTokensOfCurrencySymbol, mkUpdated_FundDatum_With_NewClaimRewards, datumIs_ScriptDatum, getScriptDatumTypo_FromDatum)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, TxOut_With_Datum, TxOut_Value_And_PoolDatum, TxOut_Value_And_FundDatum, TxOut_Value_And_UserDatum, DatumValidator (FundDatum), FundDatumTypo (..), TxOut_Value_And_ScriptDatum)
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator)
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE hasInputUTxO #-}
hasInputUTxO :: LedgerApiV2.TxOutRef -> LedgerContextsV2.TxInfo -> Bool
hasInputUTxO txOutRef info = any (\i -> LedgerApiV2.txInInfoOutRef i == txOutRef) $ LedgerApiV2.txInfoInputs info
-------------------------------------------------------------------------------------------  

{-# INLINABLE getOwnMintedTokenNameAndAmt  #-}
getOwnMintedTokenNameAndAmt :: LedgerContextsV2.ScriptContext -> [(LedgerApiV2.TokenName, Integer)]
getOwnMintedTokenNameAndAmt ctx =
    let
        !cs = LedgerContextsV2.ownCurrencySymbol ctx

        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx

        !flatten = TxAssocMap.lookup cs (LedgerApiV2.getValue $ LedgerApiV2.txInfoMint info)
    in
        TxAssocMap.toList $ Helpers.fromJust flatten

        -- case TxAssocMap.toList $ Helpers.fromJust flatten of
        --     [(tn, amt)] -> Just (cs, tn, amt)
        --     _           -> Nothing

-------------------------------------------------------------------------------------------  

{-# INLINABLE validateMint_NFT_Own_CS_Any_TN #-}
validateMint_NFT_Own_CS_Any_TN :: LedgerContextsV2.ScriptContext -> Bool
validateMint_NFT_Own_CS_Any_TN ctx  =
    traceIfFalse "MAMT" checkNFTMintedAmountV2  -- "Minting NFT: Wrong Mint Amount"
    where
        !checkNFTMintedAmountV2 =
            case getOwnMintedTokenNameAndAmt ctx of
                []  -> False
                x   -> all (\(_, amt) -> amt == 1) x
                -- [(_, amt)] -> amt == 1
                -- _           -> False
                -- Just (_, _, amt)   -> amt == 1
                -- _                  -> False

-------------------------------------------------------------------------------------------

{-# INLINABLE validateBurn_NFT_Own_CS_Any_TN #-}
validateBurn_NFT_Own_CS_Any_TN :: LedgerContextsV2.ScriptContext -> Bool
validateBurn_NFT_Own_CS_Any_TN  ctx  =
    traceIfFalse "BNFTAMT" checkNFTBurnedAmountV2 -- "Minting NFT: Wrong Burn Amount"
    where
        !checkNFTBurnedAmountV2 =
            case getOwnMintedTokenNameAndAmt ctx of
                []  -> False
                x   -> all (\(_, amt) -> amt == negate 1) x

                -- [(_, amt)] -> amt == negate 1
                -- _           -> False
                -- Just (_, _, amt)    -> amt == negate 1
                -- _                   -> False

-------------------------------------------------------------------------------------------

{-# INLINABLE validateBurn_Token_Own_CS_Any_TN #-}
validateBurn_Token_Own_CS_Any_TN ::  LedgerContextsV2.ScriptContext -> Bool
validateBurn_Token_Own_CS_Any_TN ctx  =
    traceIfFalse "BTAMT" checkAnyBurnedAmountV2 -- "Minting Token: Wrong Burn Amount"
    where
        !checkAnyBurnedAmountV2 =
            case getOwnMintedTokenNameAndAmt ctx of
                    []  -> False
                    x   -> all (\(_, amt) -> amt < 0) x
                    -- [(_, amt)] -> amt == negate 1
                    -- _        -> False
                -- Just (_, _, amt)    -> amt < 0
                -- _                   -> False

-------------------------------------------------------------------------------------------  
-- Helpers para la validacion de la tx
-------------------------------------------------------------------------------------------  

{-# INLINABLE getTxOut_Value #-}
getTxOut_Value :: (x, y) -> x
getTxOut_Value = fst

{-# INLINABLE getTxOut_Datum #-}
getTxOut_Datum :: (x, y) -> y
getTxOut_Datum = snd

-------------------------------------------------------------------------------------------

{-# INLINABLE getTxOut_Value_And_PoolDatum #-}
getTxOut_Value_And_PoolDatum :: LedgerValue.AssetClass -> [T.TxOut_Value_And_Datum] -> Maybe T.TxOut_Value_And_PoolDatum
getTxOut_Value_And_PoolDatum poolID_AC txOut_Values_And_Datums =
    let
        !txIDInOutWithPoolDatum = [ (value, datum) | (value, datum) <-  txOut_Values_And_Datums, Helpers.isNFT_With_AC_InValue value poolID_AC]
        -- !txIDInOutWithPoolDatum' = [  (value, Helpers.getPoolDatumTypo_FromDatum datum) | (value, datum)  <- txIDInOutWithPoolDatum,  Helpers.datumIs_PoolDatum datum ]
        !txIDInOutWithPoolDatum' = [  (value, Helpers.getPoolDatumTypo_FromDatum datum) | (value, datum)  <- txIDInOutWithPoolDatum ]

    in
        case txIDInOutWithPoolDatum' of
            [x] -> Just x
            _   -> Nothing

-------------------------------------------------------------------------------------------

{-# INLINABLE getTxOuts_Values_And_FundDatums #-}
getTxOuts_Values_And_FundDatums :: LedgerValue.AssetClass -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_FundDatum]
getTxOuts_Values_And_FundDatums fundID_AC txOuts_Value_And_Datum =
    let
        !txIDInOutWithFundDatum = [ (value, datum) | (value, datum) <-  txOuts_Value_And_Datum, Helpers.isToken_With_AC_InValue value fundID_AC]
        -- !txIDInOutWithFundDatum' = [  (value, Helpers.getFundDatumTypo_FromDatum  datum) | (value, datum) <- txIDInOutWithFundDatum,  Helpers.datumIs_FundDatum datum ]
        !txIDInOutWithFundDatum' = [  (value, Helpers.getFundDatumTypo_FromDatum  datum) | (value, datum) <- txIDInOutWithFundDatum]
    in
        txIDInOutWithFundDatum'

-------------------------------------------------------------------------------------------  

{-# INLINABLE getTxOuts_Values_And_UserDatums #-}
getTxOuts_Values_And_UserDatums :: LedgerValue.AssetClass -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_UserDatum]
getTxOuts_Values_And_UserDatums userID_AC txOuts_Value_And_Datum =
    let
        !txIDInOutWithUserDatum = [ (value, datum) | (value, datum) <-  txOuts_Value_And_Datum, Helpers.isToken_With_AC_InValue value userID_AC]
        -- !txIDInOutWithUserDatum' = [(value, Helpers.getUserDatumTypo_FromDatum datum) | (value, datum) <- txIDInOutWithUserDatum,  Helpers.datumIs_UserDatum datum ]
        !txIDInOutWithUserDatum' = [(value, Helpers.getUserDatumTypo_FromDatum datum) | (value, datum) <- txIDInOutWithUserDatum]
    in
        txIDInOutWithUserDatum'

-------------------------------------------------------------------------------------------  

{-# INLINABLE getTxOut_Value_And_ScriptDatum #-}
getTxOut_Value_And_ScriptDatum :: LedgerValue.AssetClass -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_ScriptDatum]
getTxOut_Value_And_ScriptDatum txID_Master_AddScripts_AC txOuts_Value_And_Datum =
    let
        !txIDInOutWithScriptDatum = [ (value, datum) | (value, datum) <-  txOuts_Value_And_Datum, Helpers.isToken_With_AC_InValue value txID_Master_AddScripts_AC]
        -- !txIDInOutWithScriptDatum' = [(value, Helpers.getSome_ScriptDatumTypo_FromDatum datum) | (value, datum) <- txIDInOutWithScriptDatum,  Helpers.datumIs_Some_ScriptDatum datum ]
        !txIDInOutWithScriptDatum' = [(value, Helpers.getScriptDatumTypo_FromDatum datum) | (value, datum) <- txIDInOutWithScriptDatum ]
    in
        txIDInOutWithScriptDatum'

-------------------------------------------------------------------------------------------  

-- for ordering a list of uTxO 
{-# INLINABLE sort_Value_And_FundDatum #-}
sort_Value_And_FundDatum :: T.TxOut_Value_And_FundDatum -> T.TxOut_Value_And_FundDatum -> Ordering
sort_Value_And_FundDatum uTxO1 uTxO2 =
    let 
        !fundDatum1 = getTxOut_Datum uTxO1
        !fundDatum2 = getTxOut_Datum uTxO2
        !getFundAmountCanUse_in_FundDatum1 = Helpers.getFundAmountCanUse_in_FundDatum fundDatum1
        !getFundAmountCanUse_in_FundDatum2 = Helpers.getFundAmountCanUse_in_FundDatum fundDatum2
    in
        if getFundAmountCanUse_in_FundDatum1 > getFundAmountCanUse_in_FundDatum2 then 
            LT 
        else 
            if getFundAmountCanUse_in_FundDatum1 < getFundAmountCanUse_in_FundDatum2 then 
                GT 
            else 
                let 
                    !fdFundAmount1 = T.fdFundAmount fundDatum1
                    !fdFundAmount2 = T.fdFundAmount fundDatum2
                in
                    if fdFundAmount1 > fdFundAmount2 then 
                        LT 
                    else 
                        if fdFundAmount1 < fdFundAmount2 then 
                            GT 
                        else 
                            let
                                !fdCashedOut1 = T.fdCashedOut fundDatum1
                                !fdCashedOut2 = T.fdCashedOut fundDatum2
                            in
                                if fdCashedOut1 > fdCashedOut2 then 
                                    LT 
                                else 
                                    if fdCashedOut1 < fdCashedOut2 then 
                                        GT 
                                    else 
                                       let 
                                            !value1 = getTxOut_Value uTxO1
                                            !value2 = getTxOut_Value uTxO2
                                            !ada1 = LedgerAda.fromValue value1
                                            !ada2 = LedgerAda.fromValue value2
                                        in
                                            if ada1 > ada2 then 
                                                LT 
                                            else 
                                                if ada1 < ada2 then 
                                                    GT 
                                                else 
                                                    if LedgerValue.gt value1 value2 then 
                                                        LT 
                                                    else 
                                                        GT

-------------------------------------------------------------------------------------------

-- it creates the FundDatum, hash and value to each of the uTxO selected
{-# INLINABLE getFundDatumListWithNewValues #-}
getFundDatumListWithNewValues :: LedgerValue.AssetClass -> LedgerApiV2.CurrencySymbol -> Bool -> [T.TxOut_Value_And_FundDatum] -> Integer -> [T.TxOut_Value_And_FundDatum]
getFundDatumListWithNewValues harvest_AC harvest_CS haverstIsWithoutTokenName txOuts_Values_And_FundDatums_WithEnoughValueToClaim claim =
    if (claim > 0) && not (null txOuts_Values_And_FundDatums_WithEnoughValueToClaim)  then
        let
            ---------------
            !uTxO = head txOuts_Values_And_FundDatums_WithEnoughValueToClaim
            !fundDatum' = getTxOut_Datum uTxO
            ---------------
            !value = getTxOut_Value uTxO
            !amountCanUse   = Helpers.getFundAmountCanUse_in_FundDatum fundDatum'
            ---------------
        in
            ---------------
            if amountCanUse - claim >= 0 then
                -- this means that with this uTxO i cover all the claim, dont need to keep adding uTxO
                let
                    !valueToSubstract =
                        -- if haverstIsWithoutTokenName then
                        --     -- si la unidad de harvest es un token, pero no tiene nombre, 
                        --     -- significa que estoy usando tokens con misma currency symbol pero diferente token name.
                        --     -- voy a tomar aleatoriamente claim cantidad de tokens de la currency symbol sin importar el token name
                        Helpers.createValueAddingTokensOfCurrencySymbol harvest_AC harvest_CS haverstIsWithoutTokenName value claim
                        -- else
                        -- si la unidad de harvest es ada, o es un token con nombre, 
                        -- LedgerValue.assetClassValue harvest_AC claim
                    ---------------
                    !newValue = value <> negate valueToSubstract
                    ---------------
                    !newFundDatum = Helpers.mkUpdated_FundDatum_With_NewClaimRewards fundDatum' claim
                    ---------------
                    !(T.FundDatum newFundDatumTypo) = newFundDatum
                    ---------------
                in
                    [(newValue, newFundDatumTypo)]
            ---------------
            else
            ---------------
                -- this means that remaining claim is bigger than the value, so i need to keep adding uTxO
                -- ill take all i can from this uTxO
                let
                    !valueToSubstract =
                        -- if haverstIsWithoutTokenName then
                        --     -- si la unidad de harvest es un token, pero no tiene nombre, 
                        --     -- significa que estoy usando tokens con misma currency symbol pero diferente token name.
                        --     -- voy a tomar aleatoriamente harvestUnitFromValueCanUse cantidad de tokens de la currency symbol sin importar el token name
                        Helpers.createValueAddingTokensOfCurrencySymbol harvest_AC harvest_CS haverstIsWithoutTokenName value amountCanUse
                        -- else
                        -- si la unidad de harvest es ada, o es un token con nombre, 
                        -- LedgerValue.assetClassValue harvest_AC amountCanUse
                    ---------------
                    !newValue = value <> negate valueToSubstract
                    ---------------
                    !newFundDatum = Helpers.mkUpdated_FundDatum_With_NewClaimRewards fundDatum' amountCanUse
                    ---------------
                    !(T.FundDatum newFundDatumTypo) = newFundDatum
                    ---------------
                    !newClaim = claim - amountCanUse
                    ---------------
                    !new = (newValue, newFundDatumTypo)
                    ---------------
                    !others = getFundDatumListWithNewValues harvest_AC harvest_CS haverstIsWithoutTokenName (tail txOuts_Values_And_FundDatums_WithEnoughValueToClaim) newClaim
                    ---------------
                in
                    new : others
    else
        []


-------------------------------------------------------------------------------------------

{-# INLINABLE checkIfAllAreFromSameAddress #-}
checkIfAllAreFromSameAddress :: [T.TxOut_With_Datum] -> [T.TxOut_With_Datum] -> Bool
checkIfAllAreFromSameAddress inputs_Normal_And_Refs outputs_WithDatum =
    let
        !inputsAddresses = [ LedgerApiV2.txOutAddress txtout | (txtout, _) <- inputs_Normal_And_Refs ]
        !outputsAddresses = [ LedgerApiV2.txOutAddress txtout | (txtout, _) <- outputs_WithDatum ]

        !address = head inputsAddresses
    in
        all (== address) inputsAddresses  &&  all (== address) outputsAddresses

-------------------------------------------------------------------------------------------

{-# INLINABLE checkIfAllSpendRedeemersAreEqual #-}
checkIfAllSpendRedeemersAreEqual :: LedgerContextsV2.ScriptContext -> T.RedeemerValidator -> Bool
checkIfAllSpendRedeemersAreEqual ctx redeemer =
    let

        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx

        txInfoRedeemers :: [(LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)]
        !txInfoRedeemers = TxAssocMap.toList $ LedgerApiV2.txInfoRedeemers info

        isSpending :: LedgerApiV2.ScriptPurpose -> Bool
        isSpending sp =
            case sp of
                LedgerApiV2.Spending _ -> True
                _ -> False

        spendRedeemers :: [LedgerApiV2.Redeemer]
        !spendRedeemers = [ red | (sp,red) <- txInfoRedeemers, isSpending sp ]

        validatorRedeemers :: [T.RedeemerValidator]
        !validatorRedeemers = [ LedgerApiV2.unsafeFromBuiltinData @T.RedeemerValidator $ LedgerApiV2.getRedeemer red | red <- spendRedeemers]

    in
        all (== redeemer) validatorRedeemers && not (null validatorRedeemers)


