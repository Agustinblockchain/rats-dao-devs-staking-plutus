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
-- {-# LANGUAGE RecordWildCards            #-}
-- {-# LANGUAGE ScopedTypeVariables        #-}
-- {-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
-- {-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE TupleSections              #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE NumericUnderscores #-}
-- {-# LANGUAGE NumericUnderscores         #-}
{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}

------------------------------------------------------------------------------------------
module Validators.StakePlusV2.OffChain.OffChainHelpers where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
-- import qualified Cardano.Api                                            as CardanoApi
-- import qualified Cardano.Api.Shelley                                    as ApiShelley
import qualified Control.Lens                                           as ControlLens
-- import qualified Control.Monad                                          as ControlMonad (void)
-- import qualified Control.Monad.Freer.State                           as ControlMonadFreerState (State, get, gets, modify, put)
import qualified Data.Map                                               as DataMap
import qualified Data.Text                                              as DataText ( Text)
import qualified Data.Void                                              as DataVoid (Void)
import qualified Ledger                                                 (PaymentPubKeyHash(..), AssetClass)
-- import qualified Ledger.Index                                        as LedgerIndex
-- import qualified Ledger.Index                                        as LedgerIndex
import qualified Ledger.Tx                                              as LedgerTx (datumInDatumFromQuery, decoratedTxOutDatum, DecoratedTxOut, decoratedTxOutValue)
import qualified Ledger.Value                                           as LedgerValue
import qualified Plutus.ChainIndex.Types                                as ChainIndexTypes (RollbackState (Unknown, Committed, TentativelyConfirmed), TxValidity (TxValid, TxInvalid, UnknownValidity), TxStatus) -- TxOutState (Spent, Unspent), TxOutStatus, TxStatus,
-- import qualified Plutus.ChainIndex.Tx                                as PlutusChainIndexTx  (citoValue, ChainIndexTxOut, citoDatum)
-- import qualified Plutus.Trace.Emulator                               as TraceEmulator
import qualified Plutus.Contract                                        as PlutusContract
import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators as UtilsTypedScriptsValidatorsV1 (RedeemerType, DatumType) -- TypedValidator, ValidatorTypes, mkTypedValidator, mkUntypedValidator, validatorAddress, validatorHash, validatorScript
import qualified Ledger.Ada                                             as LedgerAda
import qualified Ledger.Constraints                                     as LedgerConstraints
import qualified Ledger.Constraints.TxConstraints                       as LedgerTxConstraints
import qualified Plutus.V1.Ledger.Interval                              as LedgerIntervalV1 (Interval) --from, contains, interval
-- import qualified Plutus.V2.Ledger.Address                            as LedgerAddressV2
import qualified Plutus.V2.Ledger.Api                                   as LedgerApiV2
-- import qualified Plutus.V2.Ledger.Value                              as LedgerValueV2
-- import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                                                as P
import qualified Text.Printf                                            as TextPrintf (printf)
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                         as Helpers
import qualified Validators.StakePlusV2.Types.DatumsValidator           as T
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------

{- | Try to get the generic Datum from a DecoratedTxOut. -}
getDatumFromDecoratedTxOut :: LedgerTx.DecoratedTxOut -> Maybe T.DatumValidator
getDatumFromDecoratedTxOut decoratedTxOut = do

    (_, mdatum) <- decoratedTxOut ControlLens.^? LedgerTx.decoratedTxOutDatum
    LedgerApiV2.Datum d <- mdatum ControlLens.^? LedgerTx.datumInDatumFromQuery

    case (LedgerApiV2.fromBuiltinData d :: Maybe T.DatumValidator) of
        Nothing -> Nothing

        Just (T.PoolDatum poolDatum) -> do
            Just (T.PoolDatum poolDatum)
        Just (T.FundDatum fundDatum) -> do
            Just (T.FundDatum fundDatum)
        Just (T.UserDatum userDatum) -> do
            Just (T.UserDatum userDatum)

        Just (T.ScriptDatum scriptDatum) -> do
            Just (T.ScriptDatum scriptDatum)

------------------------------------------------------------------------------------------

getValueFromDecoratedTxOut :: LedgerTx.DecoratedTxOut -> LedgerValue.Value
getValueFromDecoratedTxOut decoratedTxOut =
    let
        (Just value) = decoratedTxOut ControlLens.^? LedgerTx.decoratedTxOutValue
    in
        value

------------------------------------------------------------------------------------------

isNFTInDecoratedTxOut :: LedgerTx.DecoratedTxOut -> Ledger.AssetClass -> Bool
isNFTInDecoratedTxOut ciTxOut = Helpers.isNFT_With_AC_InValue (getValueFromDecoratedTxOut ciTxOut)

------------------------------------------------------------------------------------------

isTokenInDecoratedTxOut :: LedgerTx.DecoratedTxOut -> Ledger.AssetClass -> Bool
isTokenInDecoratedTxOut ciTxOut = Helpers.isToken_With_AC_InValue (getValueFromDecoratedTxOut ciTxOut)

------------------------------------------------------------------------------------------

{- | Get valid uTxO with PoolDatum and PoolID_AC as value (NFT) in the uTxO list. It should be only one. -}
getUTxO_With_PoolDatum ::  LedgerValue.AssetClass ->  DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut -> PlutusContract.Contract w s DataText.Text (Maybe (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut))
getUTxO_With_PoolDatum poolID_AC uTxOs = do
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "getUTxO_With_PoolDatum : uTxOs: %s" (P.show  uTxOs)
    let
        -- !uTxOsWithPoolDatum = [ (txOutRef, scriptDecoratedTxOut) | (txOutRef, scriptDecoratedTxOut) <- DataMap.toList uTxOs, Helpers.datumMaybeIs_PoolDatum (getDatumFromDecoratedTxOut scriptDecoratedTxOut) ]
        -- !uTxOsWithPoolDatum_And_Token = [ (txOutRef, scriptDecoratedTxOut) | (txOutRef, scriptDecoratedTxOut) <- uTxOsWithPoolDatum, isNFTInDecoratedTxOut scriptDecoratedTxOut poolID_AC]
        !uTxOsWithPoolDatum = [ (txOutRef, scriptDecoratedTxOut) | (txOutRef, scriptDecoratedTxOut) <- DataMap.toList uTxOs, isNFTInDecoratedTxOut scriptDecoratedTxOut poolID_AC]
        -- PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxOs List with Valid PoolDatum: %s" (P.show $ fst <$> uTxOsWithPoolDatumAndCorrectPoolIDAndCorrectValue)
    case uTxOsWithPoolDatum of
        [x] -> return $ Just x
        _ -> return Nothing
------------------------------------------------------------------------------------------

{- | Get valid uTxOs with FundDatum, registered in the PoolDatum and with FundID as value (NFT) in the uTxO list -}
getUTxOs_With_FundDatum ::  LedgerValue.AssetClass -> DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut -> PlutusContract.Contract w s DataText.Text [(LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut)]
getUTxOs_With_FundDatum fundID_AC uTxOs = do
    let
        -- uTxOsWithFundDatum = [ (txOutRef, scriptDecoratedTxOut, Helpers.getFundDatumTypo_FromMaybeDatum $ getDatumFromDecoratedTxOut scriptDecoratedTxOut) | (txOutRef, scriptDecoratedTxOut) <- DataMap.toList uTxOs, Helpers.datumMaybeIs_FundDatum (getDatumFromDecoratedTxOut scriptDecoratedTxOut) ]
        -- uTxOsWithFundDatumAnd_Token = [ (txOutRef, scriptDecoratedTxOut) | (txOutRef, scriptDecoratedTxOut, _) <- uTxOsWithFundDatum, isTokenInDecoratedTxOut scriptDecoratedTxOut fundID_AC]
        -- uTxOsWithFundDatum = [ (txOutRef, scriptDecoratedTxOut, Helpers.getFundDatumTypo_FromMaybeDatum $ getDatumFromDecoratedTxOut scriptDecoratedTxOut) | (txOutRef, scriptDecoratedTxOut) <- DataMap.toList uTxOs, Helpers.datumMaybeIs_FundDatum (getDatumFromDecoratedTxOut scriptDecoratedTxOut) ]
        uTxOsWithFundDatum = [ (txOutRef, scriptDecoratedTxOut) | (txOutRef, scriptDecoratedTxOut) <- DataMap.toList uTxOs, isTokenInDecoratedTxOut scriptDecoratedTxOut fundID_AC]
        --PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxOs List with Valid FundDatum: %s" (P.show $ fst <$> uTxOsWithFundDatumAnd_Token)
    return uTxOsWithFundDatum
------------------------------------------------------------------------------------------

{- | Get valid uTxOs with UserDatum and UserID as value (NFT) in the uTxO list -}
getUTxOs_With_UserDatum :: LedgerValue.AssetClass -> DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut -> PlutusContract.Contract w s DataText.Text [(LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut)]
getUTxOs_With_UserDatum userID_AC uTxOs = do
    let
        -- uTxOsWithUserDatum = [ (txOutRef, scriptDecoratedTxOut, Helpers.getUserDatumTypo_FromMaybeDatum $ getDatumFromDecoratedTxOut scriptDecoratedTxOut) | (txOutRef, scriptDecoratedTxOut) <- DataMap.toList uTxOs, Helpers.datumMaybeIs_UserDatum (getDatumFromDecoratedTxOut scriptDecoratedTxOut) ]
        -- uTxOsWithUserDatum_And_Token = [ (txOutRef, scriptDecoratedTxOut) | (txOutRef, scriptDecoratedTxOut, _) <- uTxOsWithUserDatum, isTokenInDecoratedTxOut scriptDecoratedTxOut userID_AC]
        uTxOsWithUserDatum = [ (txOutRef, scriptDecoratedTxOut) | (txOutRef, scriptDecoratedTxOut) <- DataMap.toList uTxOs, isTokenInDecoratedTxOut scriptDecoratedTxOut userID_AC]
        --PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxOs List with Valid UserDatum: %s" (P.show $ fst <$> uTxOsWithUserDatum_And_Token)

    return uTxOsWithUserDatum

------------------------------------------------------------------------------------------

getUTxO_With_ScriptDatum :: LedgerValue.AssetClass -> LedgerValue.AssetClass -> DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut -> PlutusContract.Contract w s DataText.Text (Maybe (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut))
getUTxO_With_ScriptDatum scriptID_AC script_AC uTxOs = do
    let
        !uTxOsWithScriptDatum = [ (txOutRef, scriptDecoratedTxOut) | (txOutRef, scriptDecoratedTxOut) <- DataMap.toList uTxOs, isTokenInDecoratedTxOut scriptDecoratedTxOut scriptID_AC && isTokenInDecoratedTxOut scriptDecoratedTxOut script_AC ]
    case uTxOsWithScriptDatum of
        [x] -> return $ Just x
        (x:_) -> return $ Just x
        _ -> return Nothing

------------------------------------------------------------------------------------------

getMaxToClaimInUxtoList :: [(LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut)] -> Integer
getMaxToClaimInUxtoList uTxOs_With_FundDatums = do
    if length uTxOs_With_FundDatums > 0 then do
        let
            ---------------
            !uTxO = head uTxOs_With_FundDatums
            !fundDatum' = Helpers.getFundDatumTypo_FromMaybeDatum $ getDatumFromDecoratedTxOut $ snd uTxO
            ---------------
            !amountCanUse   = Helpers.getFundAmountCanUse_in_FundDatum fundDatum'
            ---------------
        amountCanUse + getMaxToClaimInUxtoList (tail uTxOs_With_FundDatums)
    else
        0
------------------------------------------------------------------------------------------

sort_Value_And_FundDatum :: (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut) -> (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut) -> Ordering
sort_Value_And_FundDatum uTxO1 uTxO2 =
    let
        !fundDatum1 = Helpers.getFundDatumTypo_FromMaybeDatum $ getDatumFromDecoratedTxOut $ snd uTxO1
        !fundDatum2 = Helpers.getFundDatumTypo_FromMaybeDatum $ getDatumFromDecoratedTxOut $ snd uTxO2
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
                                            !value1 = getValueFromDecoratedTxOut $ snd uTxO1
                                            !value2 = getValueFromDecoratedTxOut $ snd uTxO2
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
                                                        -- TODO : ordenar mejor los values
                                                        LT
                                                    else
                                                        GT

------------------------------------------------------------------------------------------

-- get the small list of uTxO which can cover the paymet of the reward claimed
getUTxOListWithEnoughValueToClaim :: [(LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut)] -> Integer -> [(LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut)]
getUTxOListWithEnoughValueToClaim uTxOs_With_FundDatums claim = do

    if (claim > 0) && (length uTxOs_With_FundDatums > 0) then do
        -- this means that there is still claim to cover... i need to keep adding uTxO if there is more in the list
        let
            ---------------
            !uTxO = head uTxOs_With_FundDatums
            !fundDatum' = Helpers.getFundDatumTypo_FromMaybeDatum $ getDatumFromDecoratedTxOut $ snd uTxO
            ---------------
            !amountCanUse   = Helpers.getFundAmountCanUse_in_FundDatum fundDatum'
            ---------------
            newClaim = claim - amountCanUse
            ---------------
        uTxO : getUTxOListWithEnoughValueToClaim (tail uTxOs_With_FundDatums) newClaim
    else
        []

------------------------------------------------------------------------------------------

-- it creates the FundDatum, hash and value to each of the uTxO selected
getFundDatumListWithNewValues :: LedgerValue.AssetClass -> LedgerApiV2.CurrencySymbol -> Bool -> [(LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut)] -> Integer -> PlutusContract.Contract w0 s0 e0 [(T.FundDatumTypo, LedgerValue.Value)]
getFundDatumListWithNewValues harvest_AC harvest_CS haverstIsWithoutTokenName uTxOs_With_FundDatums claim =
    if (claim > 0) && not (null uTxOs_With_FundDatums)  then
        let
            ---------------
            !uTxO = head uTxOs_With_FundDatums
            !fundDatum' = Helpers.getFundDatumTypo_FromMaybeDatum $ getDatumFromDecoratedTxOut $ snd uTxO
            ---------------
            !value = getValueFromDecoratedTxOut $ snd uTxO
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
                    !newFundDatumTypo = Helpers.mkUpdated_FundDatum_With_NewClaimRewards fundDatum' claim
                    ---------------
                    -- !(T.FundDatum newFundDatumTypo) = newFundDatum
                    ---------------
                in
                    return [(newFundDatumTypo, newValue)]
            ---------------
            else do
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
                    !newFundDatumTypo = Helpers.mkUpdated_FundDatum_With_NewClaimRewards fundDatum' amountCanUse
                    ---------------
                    -- !(T.FundDatum newFundDatumTypo) = newFundDatum
                    ---------------
                    !newClaim = claim - amountCanUse
                    ---------------
                    !new = (newFundDatumTypo, newValue )
                    ---------------
                !others <- getFundDatumListWithNewValues harvest_AC harvest_CS haverstIsWithoutTokenName (tail uTxOs_With_FundDatums) newClaim
                ---------------
                return (new : others)
    else
        return []

------------------------------------------------------------------------------------------

isTxValid :: ChainIndexTypes.TxStatus -> Bool
isTxValid txStatus =
    case txStatus of
        ChainIndexTypes.Unknown -> False
        ChainIndexTypes.Committed ChainIndexTypes.UnknownValidity _ -> False
        ChainIndexTypes.Committed ChainIndexTypes.TxInvalid _ -> False
        ChainIndexTypes.Committed ChainIndexTypes.TxValid _ -> True
        ChainIndexTypes.TentativelyConfirmed _ ChainIndexTypes.UnknownValidity _ -> False
        ChainIndexTypes.TentativelyConfirmed _ ChainIndexTypes.TxInvalid _ -> False
        ChainIndexTypes.TentativelyConfirmed _ ChainIndexTypes.TxValid _ ->  True

------------------------------------------------------------------------------------------

mintNFT_With_TxOut :: DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut
                                 -> LedgerApiV2.MintingPolicy
                                 -> LedgerApiV2.TxOutRef
                                 -> Maybe LedgerApiV2.Redeemer
                                 -> LedgerValue.Value
                                 -> LedgerIntervalV1.Interval LedgerApiV2.POSIXTime
                                 -> Ledger.PaymentPubKeyHash
                                 -> (LedgerConstraints.ScriptLookups a0, LedgerTxConstraints.TxConstraints (UtilsTypedScriptsValidatorsV1.RedeemerType DataVoid.Void) (UtilsTypedScriptsValidatorsV1.DatumType DataVoid.Void))
mintNFT_With_TxOut uTxOs policy txOutRef redeemerMint valueForMint validityRange pPKH = do
    let
        use = head [(t,ci) | (t,ci) <- DataMap.toList uTxOs, t == txOutRef]
        --map = zip (fst use) (snd use)
        mapz = DataMap.fromList [use]

        lookupsTxMint =
            -- This script is goint to use see all the uTxO from master or user walllet
            -- LedgerConstraints.unspentOutputs uTxOs P.<> 
            LedgerConstraints.unspentOutputs mapz  P.<>

            -- Is going to Mint the NFT
            LedgerConstraints.plutusV2MintingPolicy policy

        txMint =
            -- Is going to spend the user uTxO assinged to the TxID
            LedgerConstraints.mustSpendPubKeyOutput txOutRef P.<>
            -- Is going to Mint the NFT
            case redeemerMint of
                Nothing -> LedgerConstraints.mustMintValue valueForMint
                Just r  -> LedgerConstraints.mustMintValueWithRedeemer r valueForMint
            P.<>
            -- Is goint create the valid range based in validTimeRange Pool Param
            LedgerConstraints.mustValidateIn validityRange P.<>
            -- Must be signed by the master or the user PPKH
            LedgerConstraints.mustBeSignedBy pPKH

    (lookupsTxMint, txMint)

------------------------------------------------------------------------------------------

mintToken_With_Policy :: DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut
                                 -> LedgerApiV2.MintingPolicy
                                 -> Maybe LedgerApiV2.Redeemer
                                 -> LedgerValue.Value
                                 -> LedgerIntervalV1.Interval LedgerApiV2.POSIXTime
                                 -> Ledger.PaymentPubKeyHash
                                 -> (LedgerConstraints.ScriptLookups a0, LedgerTxConstraints.TxConstraints (UtilsTypedScriptsValidatorsV1.RedeemerType DataVoid.Void) (UtilsTypedScriptsValidatorsV1.DatumType DataVoid.Void))
mintToken_With_Policy uTxOs policy redeemerMint valueForMint validityRange pPKH = do
    let

        lookupsTxMint =
            -- This script is goint to use see all the uTxO from master or user walllet
            LedgerConstraints.unspentOutputs uTxOs P.<>

            -- Is going to Mint the NFT
            LedgerConstraints.plutusV2MintingPolicy policy

        txMint =
            -- Is going to spend the user uTxO assinged to the TxID
            -- Is going to Mint the NFT
            case redeemerMint of
                Nothing -> LedgerConstraints.mustMintValue valueForMint
                Just r  -> LedgerConstraints.mustMintValueWithRedeemer r valueForMint
            P.<>
            -- Is goint create the valid range based in validTimeRange Pool Param
            LedgerConstraints.mustValidateIn validityRange P.<>
            -- Must be signed by the master or the user PPKH
            LedgerConstraints.mustBeSignedBy pPKH

    (lookupsTxMint, txMint)

------------------------------------------------------------------------------------------

mintToken_With_RefPolicy :: DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut
                                 -> (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut)
                                 -> Maybe LedgerApiV2.Redeemer
                                 -> LedgerValue.Value
                                 -> LedgerIntervalV1.Interval LedgerApiV2.POSIXTime
                                 -> Ledger.PaymentPubKeyHash
                                 -> (LedgerConstraints.ScriptLookups a0, LedgerTxConstraints.TxConstraints (UtilsTypedScriptsValidatorsV1.RedeemerType DataVoid.Void) (UtilsTypedScriptsValidatorsV1.DatumType DataVoid.Void))
mintToken_With_RefPolicy uTxOs uTxOAtScriptWithScript redeemerMint valueForMint validityRange pPKH = do
    let

        lookupsTxMint =
            -- This script is goint to use see all the uTxO from master or user walllet
            LedgerConstraints.unspentOutputs uTxOs P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxOAtScriptWithScript] )

        txMint =
            case redeemerMint of
                Nothing -> LedgerTxConstraints.mustMintValueWithReference (fst uTxOAtScriptWithScript) valueForMint
                Just r  -> LedgerTxConstraints.mustMintValueWithRedeemerAndReference r (Just $ fst uTxOAtScriptWithScript) valueForMint
            P.<>
            LedgerConstraints.mustValidateIn validityRange P.<>
            LedgerConstraints.mustBeSignedBy pPKH

    (lookupsTxMint, txMint)

------------------------------------------------------------------------------------------

burntToken_With_Policy :: DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut
                                 -> LedgerApiV2.MintingPolicy
                                 -> Maybe LedgerApiV2.Redeemer
                                 -> LedgerValue.Value
                                 -> LedgerIntervalV1.Interval LedgerApiV2.POSIXTime
                                 -> Ledger.PaymentPubKeyHash
                                 -> (LedgerConstraints.ScriptLookups a0, LedgerTxConstraints.TxConstraints (UtilsTypedScriptsValidatorsV1.RedeemerType DataVoid.Void) (UtilsTypedScriptsValidatorsV1.DatumType DataVoid.Void))
burntToken_With_Policy uTxOs policy redeemerBurn valueForBurn validityRange pPKH = do
    let
        lookupsTxBurn =
            -- This script is goint to use see all the uTxO from master or user walllet
            LedgerConstraints.unspentOutputs uTxOs P.<>
            -- Is going to Burn the NFT
            LedgerConstraints.plutusV2MintingPolicy policy

        txBurn =
            -- Is going to Burn the NFT
            case redeemerBurn of
                Nothing -> LedgerConstraints.mustMintValue valueForBurn
                Just r  -> LedgerConstraints.mustMintValueWithRedeemer r valueForBurn
            P.<>
            -- Is goint create the valid range based in validTimeRange Pool Param
            LedgerConstraints.mustValidateIn validityRange P.<>
            -- Must be signed by the master or the user PPKH
            LedgerConstraints.mustBeSignedBy pPKH

    (lookupsTxBurn, txBurn)

------------------------------------------------------------------------------------------

burntToken_With_RefPolicy :: DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut
                                 -> (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut)
                                 -> Maybe LedgerApiV2.Redeemer
                                 -> LedgerValue.Value
                                 -> LedgerIntervalV1.Interval LedgerApiV2.POSIXTime
                                 -> Ledger.PaymentPubKeyHash
                                 -> (LedgerConstraints.ScriptLookups a0, LedgerTxConstraints.TxConstraints (UtilsTypedScriptsValidatorsV1.RedeemerType DataVoid.Void) (UtilsTypedScriptsValidatorsV1.DatumType DataVoid.Void))
burntToken_With_RefPolicy uTxOs uTxOAtScriptWithScript redeemerBurn valueForBurn validityRange pPKH = do
    let
        lookupsTxBurn =
            -- This script is goint to use see all the uTxO from master or user walllet
            LedgerConstraints.unspentOutputs uTxOs P.<>
            -- Is going to Burn the NFT
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxOAtScriptWithScript] )

        txBurn =
            -- Is going to Burn the NFT
            case redeemerBurn of
                Nothing -> LedgerTxConstraints.mustMintValueWithReference (fst uTxOAtScriptWithScript) valueForBurn
                Just r  -> LedgerTxConstraints.mustMintValueWithRedeemerAndReference r (Just $ fst uTxOAtScriptWithScript) valueForBurn
            P.<>
            -- Is goint create the valid range based in validTimeRange Pool Param
            LedgerConstraints.mustValidateIn validityRange P.<>
            -- Must be signed by the master or the user PPKH
            LedgerConstraints.mustBeSignedBy pPKH

    (lookupsTxBurn, txBurn)

------------------------------------------------------------------------------------------

createValueAddingTokensOfCurrencySymbol :: LedgerValue.AssetClass -> LedgerApiV2.CurrencySymbol -> Bool -> LedgerApiV2.Value -> Integer -> PlutusContract.Contract w s DataText.Text LedgerApiV2.Value
createValueAddingTokensOfCurrencySymbol ac cs acIsWithoutTokenName value cantidad = do
    if not acIsWithoutTokenName then do
        return $ LedgerValue.assetClassValue ac cantidad
    else do
        -- si la unidad es un token, pero no tiene nombre, 
        -- significa que estoy usando tokens con misma currency symbol pero diferente token name.
        -- voy a tomar en orden la cantidad de tokens de la currency symbol sin importar el token name
        let

            !tokenOfCurrencySymbol = [ (tn, am) | (cs', tn, am) <- Helpers.flattenValue value, cs' == cs ]
        PlutusContract.logInfo @P.String $ TextPrintf.printf "Currency Symbol: %s" (P.show cs)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "Token Of Currency Symbol: %s" (P.show tokenOfCurrencySymbol)
        let
            compareTokenName :: (LedgerApiV2.TokenName, Integer) -> (LedgerApiV2.TokenName, Integer) -> P.Ordering
            compareTokenName (tn1, _) (tn2, _)
                | tn1 < tn2 = LT
                | otherwise = GT

            !tokenOfCurrencySymbol_Ordered = sortBy compareTokenName tokenOfCurrencySymbol

            sumarTokens :: [(LedgerApiV2.TokenName, Integer)] -> Integer -> PlutusContract.Contract w s DataText.Text LedgerApiV2.Value
            sumarTokens [] left = do
                if left > 0 then do
                    PlutusContract.throwError "Can't find enough tokens with that Currency Symbol in Value"
                else
                    return $ LedgerAda.lovelaceValueOf 0
            sumarTokens list left =
                let
                    (tn, am) = head list
                    !harvest_AC = LedgerValue.AssetClass (cs, tn)
                in
                    if am > left then
                        return $ LedgerValue.assetClassValue harvest_AC left
                    else do
                        tailSum <- sumarTokens (tail list) (left - am)
                        return $ LedgerValue.assetClassValue harvest_AC am <> tailSum

        sumarTokens tokenOfCurrencySymbol_Ordered cantidad

------------------------------------------------------------------------------------------

checkIfThereIsUTxOFreeForCollateral :: DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut -> PlutusContract.Contract w s DataText.Text Bool
checkIfThereIsUTxOFreeForCollateral uTxO = do
    let
        !uTxOFreeForCollateral = [ (ref, out) | (ref, out) <- DataMap.toList uTxO,
            let
                value =  getValueFromDecoratedTxOut out
            in
                LedgerValue.adaOnlyValue value == value
                &&
                LedgerAda.fromValue value >= 5000000
            ]
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO Free For Collateral: %s" (P.show uTxOFreeForCollateral)
    if P.not (P.null uTxOFreeForCollateral) then
        return True
    else
        return False

------------------------------------------------------------------------------------------
