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
{-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE TupleSections              #-}
-- {-# LANGUAGE AllowAmbiguousTypes        #-}
-- {-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}
{- HLINT ignore "Use camelCase" -}
------------------------------------------------------------------------------------------
module Validators.StakePlusV2.OnChain.Core.OnChainHelpers where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Value                                       as LedgerValue
import qualified Plutus.V1.Ledger.Interval                          as LedgerIntervalV1 (member, contains, from, interval)
import qualified Plutus.V2.Ledger.Api                               as LedgerApiV2 (TxOut, TokenName, unsafeFromBuiltinData, CurrencySymbol, Extended( Finite ), LowerBound(..), Interval(..), getDatum, txInfoInputs, txInfoOutputs , txInInfoResolved, CurrencySymbol, txInfoMint, txInfoValidRange, POSIXTime, txInfoReferenceInputs, UnsafeFromData) 
import qualified Plutus.V2.Ledger.Contexts                          as LedgerContextsV2 (ScriptContext, TxInfo, scriptContextTxInfo, txSignedBy, findDatum) 
import qualified Plutus.V2.Ledger.Tx                                as LedgerTxV2 (txOutDatum , OutputDatum (..)) 
import           PlutusTx.Prelude                                   ( Bool(..), Integer, Maybe(..), Eq((==)), Ord((<=)), AdditiveSemigroup((+)), (&&), not, (||), (/=), ($), any, length, null, or, isJust, traceIfFalse, all )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                     as Helpers (datumIs_PoolDatum, datumIs_FundDatum, datumIs_UserDatum, fromJust, isNFT_With_AC_InValue, isNFT_With_TN_InValue, isNFT_With_CS_InValue, isToken_With_AC_AndAmt_InValue, isToken_With_AC_InValue, isToken_With_CS_InValue, datumIs_ScriptDatum) 
import qualified Validators.StakePlusV2.Types.Constants             as T (poolDatum_Terminated, validTimeRange, poolDatum_Emergency)
import qualified Validators.StakePlusV2.Types.DatumsValidator       as T (DatumValidator, PoolDatumTypo (..), TxOut_With_Datum)
import qualified Validators.StakePlusV2.Types.Types                 as T (PoolParams (..), Master, User)

------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------

{-# INLINABLE tracetxOut #-}
tracetxOut :: LedgerApiV2.TxOut -> LedgerContextsV2.ScriptContext -> Bool
tracetxOut !txOut !ctx =
    case LedgerTxV2.txOutDatum txOut of
        LedgerTxV2.NoOutputDatum            -> traceIfFalse "No D" False
        (LedgerTxV2.OutputDatumHash dh)     ->
            case LedgerContextsV2.findDatum dh (LedgerContextsV2.scriptContextTxInfo ctx) of
                        Nothing     -> traceIfFalse "H ERR" False
                        _           -> traceIfFalse "H OK" False
        (LedgerTxV2.OutputDatum _)          -> traceIfFalse "D OK" False

{-# INLINABLE traceDatum #-}
traceDatum :: T.DatumValidator -> Bool
traceDatum !dat =
    traceIfFalse "TPD" (not $ Helpers.datumIs_PoolDatum dat) &&
    traceIfFalse "TFD" (not $ Helpers.datumIs_FundDatum dat) &&
    traceIfFalse "TUD" (not $ Helpers.datumIs_UserDatum dat) &&
    traceIfFalse "TSD" (not $ Helpers.datumIs_ScriptDatum dat) &&
    traceIfFalse "TOD" False

{-# INLINABLE tracetxOuts #-}
tracetxOuts :: [LedgerApiV2.TxOut] -> LedgerContextsV2.ScriptContext -> Bool
tracetxOuts !tracetxOuts' !ctx =
    let
        -- !res1 = traceIfFalse  "0" (length tracetxOuts' /= 0) &&
        !res1 = traceIfFalse  "0" (not (null tracetxOuts')) &&
                traceIfFalse  "1" (length tracetxOuts' /= 1) &&
                traceIfFalse  "2" (length tracetxOuts' /= 2) &&
                traceIfFalse ">2" (length tracetxOuts' <= 2)
        !txOutReftxOutsAndDatums = [  getDatumFromTxOut txOut ctx | txOut <- tracetxOuts' ]
        !txOutReftxOutsAndJustDatums = [  Helpers.fromJust dat | dat <- txOutReftxOutsAndDatums, isJust dat ]
        -- !res2 = traceIfFalse   "0 D" (length txOutReftxOutsAndJustDatums /= 0) &&
        !res2 = traceIfFalse   "0 D" (not (null txOutReftxOutsAndJustDatums)) &&
                traceIfFalse   "1 D" (length txOutReftxOutsAndJustDatums /= 1) &&
                traceIfFalse   "2 D" (length txOutReftxOutsAndJustDatums /= 2) &&
                traceIfFalse  ">2 D" (length txOutReftxOutsAndJustDatums <= 2)
        !restracetxOuts =  or [ tracetxOut txOut ctx | txOut <- tracetxOuts']
        !restraceDatums =  or [ traceDatum d | d <- txOutReftxOutsAndJustDatums]
    in
        (res1 || res2 || restracetxOuts || restraceDatums) && False

--------------------------------------------------------------------------------

{- | Gets the datum attached to a uTxO. -}
{-# INLINABLE getDatumFromTxOut #-}
-- getDatumFromTxOut :: LedgerApiV2.FromData T.DatumValidator => LedgerApiV2.TxOut -> LedgerContextsV2.ScriptContext -> Maybe T.DatumValidator
getDatumFromTxOut :: LedgerApiV2.UnsafeFromData d => LedgerApiV2.TxOut -> LedgerContextsV2.ScriptContext -> Maybe d
getDatumFromTxOut !txtout !ctx =
    let
        findDatum LedgerTxV2.NoOutputDatum        = Nothing
        findDatum (LedgerTxV2.OutputDatumHash dh) = LedgerContextsV2.findDatum dh (LedgerContextsV2.scriptContextTxInfo ctx)
        findDatum (LedgerTxV2.OutputDatum d)      = Just d -- LedgerContextsV2.findDatumHash d (LedgerContextsV2.scriptContextTxInfo ctx) >> 
    in
        case findDatum $ LedgerTxV2.txOutDatum txtout of
            Nothing -> Nothing
            Just x -> Just $ LedgerApiV2.unsafeFromBuiltinData $ LedgerApiV2.getDatum x

--------------------------------------------------------------------------------

{- | Get the inputs with Datums. -}
{-# INLINABLE getInputsWithDatum #-}
getInputsWithDatum :: LedgerContextsV2.ScriptContext -> [T.TxOut_With_Datum]
getInputsWithDatum !ctx =
    let 
        !txOutReftxOutsAndDatums =
            let
                !txOuts = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
            in
                [ (txOut, getDatumFromTxOut txOut ctx) | txOut <- txOuts]
    in 
        [ (txOut, Helpers.fromJust dat) | (txOut, dat) <- txOutReftxOutsAndDatums, isJust dat ]
      

{- | Get the reference inputs with Datums. -}
{-# INLINABLE getReferenceInputsWithDatum #-}
getReferenceInputsWithDatum :: LedgerContextsV2.ScriptContext -> [T.TxOut_With_Datum]
getReferenceInputsWithDatum !ctx =
    let 
        !txOutReftxOutsAndDatums =
            let
                !txOuts = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoReferenceInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
            in
                [(txOut, getDatumFromTxOut txOut ctx) | txOut <- txOuts]
    in
        [ (txtout, Helpers.fromJust dat) | (txtout, dat) <- txOutReftxOutsAndDatums, isJust dat ]
            

--------------------------------------------------------------------------------

{- | Get the outputs with Datums. -}
{-# INLINABLE getOutputsWithDatum #-}
getOutputsWithDatum :: LedgerContextsV2.ScriptContext -> [T.TxOut_With_Datum]
getOutputsWithDatum !ctx = 
    let
        !txOutsAndDatums = 
            let
                !txOuts  = LedgerApiV2.txInfoOutputs (LedgerContextsV2.scriptContextTxInfo ctx)
            in
                [ (txOut, getDatumFromTxOut txOut ctx) | txOut <- txOuts ]
    in 
        [ (txtout, Helpers.fromJust dat) | (txtout, dat) <- txOutsAndDatums, isJust dat ]

--------------------------------------------------------------------------------

{- | Check if there is any token minted with the right asset class. -}
{-# INLINABLE isNFT_Minted_With_AC #-}
isNFT_Minted_With_AC :: LedgerValue.AssetClass -> LedgerContextsV2.TxInfo -> Bool
isNFT_Minted_With_AC !ac !info =
    let
        !mintedValue = LedgerApiV2.txInfoMint info
    in
        Helpers.isNFT_With_AC_InValue mintedValue ac

--------------------------------------------------------------------------------

{- | Check if there is any token minted with the right asset class. -}
{-# INLINABLE isNFT_Burning_With_AC #-}
isNFT_Burning_With_AC :: LedgerValue.AssetClass -> LedgerContextsV2.TxInfo -> Bool
isNFT_Burning_With_AC !ac !info =
    let
        !mintedValue = LedgerApiV2.txInfoMint info
    in
        Helpers.isToken_With_AC_AndAmt_InValue mintedValue ac (-1)

--------------------------------------------------------------------------------

{- | Check if there is any token minted with the right currecy symbol, donsent matter the token name . -}
{-# INLINABLE isNFT_Minted_With_CS #-}
isNFT_Minted_With_CS :: LedgerApiV2.CurrencySymbol -> LedgerContextsV2.TxInfo -> Bool
isNFT_Minted_With_CS !currencySymbol !info =
    let
        !mintedValue = LedgerApiV2.txInfoMint info
    in
        Helpers.isNFT_With_CS_InValue mintedValue currencySymbol

--------------------------------------------------------------------------------

{- | Check if there is any token minted with the right token name, donsent matter the currency symbol . -}
{-# INLINABLE isNFT_Minted_With_TN #-}
isNFT_Minted_With_TN :: LedgerApiV2.TokenName -> LedgerContextsV2.TxInfo -> Bool
isNFT_Minted_With_TN !tokenName !info =
    let
        !mintedValue = LedgerApiV2.txInfoMint info
    in
        Helpers.isNFT_With_TN_InValue mintedValue tokenName

--------------------------------------------------------------------------------

{- | Check if there is any token minted with the right asset class and amount. -}
{-# INLINABLE isToken_Minted_With_AC_AndAmt #-}
isToken_Minted_With_AC_AndAmt :: LedgerValue.AssetClass -> Integer -> LedgerContextsV2.TxInfo -> Bool
isToken_Minted_With_AC_AndAmt !ac !amt !info =
    let
        !mintedValue = LedgerApiV2.txInfoMint info
    in
        Helpers.isToken_With_AC_AndAmt_InValue mintedValue ac amt

--------------------------------------------------------------------------------

{- | Check if there is any token minted with the right currecy symbol, donsent matter the token name and amount. -}
{-# INLINABLE isToken_Minted_With_AC #-}
isToken_Minted_With_AC :: LedgerValue.AssetClass -> LedgerContextsV2.TxInfo -> Bool
isToken_Minted_With_AC !ac !info =
    let
        !mintedValue = LedgerApiV2.txInfoMint info
    in
        Helpers.isToken_With_AC_InValue mintedValue ac

--------------------------------------------------------------------------------

{-# INLINABLE isToken_Minted_With_CS #-}
isToken_Minted_With_CS :: LedgerApiV2.CurrencySymbol -> LedgerContextsV2.TxInfo -> Bool
isToken_Minted_With_CS !cs !info =
    let
        !mintedValue = LedgerApiV2.txInfoMint info
    in
        Helpers.isToken_With_CS_InValue mintedValue cs


--------------------------------------------------------------------------------

{- | Checking Signatures  -}

{-# INLINABLE signedByPoolParamMaster #-}
signedByPoolParamMaster :: T.PoolParams -> LedgerContextsV2.TxInfo -> Bool
signedByPoolParamMaster !pParams = signedByMasters (T.ppMasters pParams)

{-# INLINABLE signedByAllPoolParamMaster #-}
signedByAllPoolParamMaster :: T.PoolParams -> LedgerContextsV2.TxInfo -> Bool
signedByAllPoolParamMaster !pParams = signedByAllMasters (T.ppMasters pParams)

{-# INLINABLE signedByMasters #-}
signedByMasters :: [T.Master] -> LedgerContextsV2.TxInfo -> Bool
signedByMasters !masters !info =
    any (LedgerContextsV2.txSignedBy info ) masters

{-# INLINABLE signedByAllMasters #-}
signedByAllMasters :: [T.Master] -> LedgerContextsV2.TxInfo -> Bool
signedByAllMasters !masters !info =
    all (LedgerContextsV2.txSignedBy info ) masters

{-# INLINABLE signedByMaster #-}
signedByMaster :: T.Master -> LedgerContextsV2.TxInfo -> Bool
signedByMaster !master !info  =
    let
        -- -- LedgerApiV1.getLedgerBytes
        -- master' = LedgerApiV2.LedgerBytes master
        -- master'' = LedgerBytes.bytes master'
        -- Right master''' = LedgerBytes.fromHex master'' 
        -- master'''' = LedgerApiV2.getLedgerBytes master'''
    in
        -- LedgerContextsV2.txSignedBy info (LedgerApiV2.PubKeyHash master)
        LedgerContextsV2.txSignedBy info master

{-# INLINABLE signedByUser #-}
signedByUser :: T.User -> LedgerContextsV2.TxInfo -> Bool
signedByUser !user !info  =
    LedgerContextsV2.txSignedBy info user

--------------------------------------------------------------------------------

{-# INLINABLE isDateReached #-}
isDateReached :: LedgerApiV2.POSIXTime -> LedgerContextsV2.TxInfo -> Bool
isDateReached !date !info  = LedgerIntervalV1.contains (LedgerIntervalV1.from date) $ LedgerApiV2.txInfoValidRange info

{-# INLINABLE isDateNotReached #-}
isDateNotReached :: LedgerApiV2.POSIXTime -> LedgerContextsV2.TxInfo -> Bool
isDateNotReached !date !info = not (isDateReached date info)

--------------------------------------------------------------------------------

{-# INLINABLE isCloseAtNotSet #-}
isCloseAtNotSet :: T.PoolDatumTypo -> Bool
isCloseAtNotSet !poolDatum  = 
    case T.pdClosedAt poolDatum of
                Nothing -> True
                Just _ -> False

{-# INLINABLE isCloseAtSet #-}
isCloseAtSet :: T.PoolDatumTypo -> Bool
isCloseAtSet !poolDatum  = 
    case T.pdClosedAt poolDatum of
                Nothing -> False
                Just _ -> True

--------------------------------------------------------------------------------

-- el close puede ser el deadline o el closedat forzado
-- ademas verifica que no este en terminated, si es asi, tambien esta closed

{-# INLINABLE isNotClosed #-}
isNotClosed :: T.PoolParams -> LedgerContextsV2.TxInfo -> T.PoolDatumTypo -> Bool
isNotClosed !pParams !info !poolDatum = not (isClosed pParams info poolDatum)
    
{-# INLINABLE isClosed #-}
isClosed :: T.PoolParams -> LedgerContextsV2.TxInfo -> T.PoolDatumTypo -> Bool
isClosed !pParams !info !poolDatum = 
        T.pdIsTerminated poolDatum  == T.poolDatum_Terminated ||
        isDateReached close info
    where
        !close = case T.pdClosedAt poolDatum of
            Nothing -> T.ppDeadline pParams 
            Just closedAt -> closedAt 

--------------------------------------------------------------------------------

-- el terminate puede ser manual, con el flag en el pooldatum
-- o puede ser automatico si:
-- la fecha de deadline mas el tiempo de gracia ya pasaron
-- la fecha de cierreAt forzado, mas el tiempo de gracia, ya pasaron

{-# INLINABLE isNotTerminated #-}
isNotTerminated :: T.PoolParams -> LedgerContextsV2.TxInfo -> T.PoolDatumTypo -> Bool
isNotTerminated !pParams !info !poolDatum = not (isTerminated pParams info poolDatum)
    
{-# INLINABLE isTerminated #-}
isTerminated :: T.PoolParams -> LedgerContextsV2.TxInfo -> T.PoolDatumTypo -> Bool
isTerminated !pParams !info !poolDatum = 
        T.pdIsTerminated poolDatum  == T.poolDatum_Terminated ||
        isDateReached closePlusGrace info
    where
        !closePlusGrace = case T.pdClosedAt poolDatum of
            Nothing -> T.ppDeadline pParams + T.ppGraceTime pParams
            Just closedAt -> closedAt + T.ppGraceTime pParams

--------------------------------------------------------------------------------

{-# INLINABLE isNotEmergency #-}
isNotEmergency :: T.PoolDatumTypo -> Bool
isNotEmergency !poolDatum = not (isEmergency poolDatum)
    
{-# INLINABLE isEmergency #-}
isEmergency :: T.PoolDatumTypo -> Bool
isEmergency !poolDatum = T.pdIsEmergency poolDatum  == T.poolDatum_Emergency 

--------------------------------------------------------------------------------

{-# INLINABLE validateBeginAtNotReached #-}
validateBeginAtNotReached :: T.PoolParams -> LedgerContextsV2.TxInfo -> Bool
validateBeginAtNotReached !pParams !info  =
    -- Check that the Pool BeginAt is not reached. That means that the Pool is not yet alive. 
    traceIfFalse "BEGINATREACHED" (isDateNotReached (T.ppBeginAt pParams) info) --BeginAt reached

{-# INLINABLE validateBeginAtReached #-}
validateBeginAtReached :: T.PoolParams -> LedgerContextsV2.TxInfo -> Bool
validateBeginAtReached !pParams !info  =
    -- Check that the Pool BeginAt is already reached. That means that the Pool is alive. 
    traceIfFalse "BEGINATNOTREACHED" (isDateReached (T.ppBeginAt pParams) info) --BeginAt not reached

--------------------------------------------------------------------------------

--Checking Deadline

{-# INLINABLE validateDeadlineNotReached #-}
validateDeadlineNotReached :: T.PoolParams -> LedgerContextsV2.TxInfo -> Bool
validateDeadlineNotReached !pParams !info  =
    -- Check that the Pool Deadline is not reached. That means that the Pool is still alive. 
    traceIfFalse "DEADLINEREACHED" (isDateNotReached (T.ppDeadline pParams) info) --Deadline reached

{-# INLINABLE validateDeadlineReached #-}
validateDeadlineReached :: T.PoolParams -> LedgerContextsV2.TxInfo -> Bool
validateDeadlineReached !pParams !info  =
    -- Check that the Pool Deadline is already reached. That means that the Pool is close. 
    traceIfFalse "DEADLINENOTREACHED" (isDateReached (T.ppDeadline pParams) info) --Deadline not reached

--------------------------------------------------------------------------------------

{-# INLINABLE getLowerBoundFromInterval #-}
getLowerBoundFromInterval :: LedgerApiV2.Interval a -> Maybe a
getLowerBoundFromInterval !iv = case LedgerApiV2.ivFrom iv of
    LedgerApiV2.LowerBound (LedgerApiV2.Finite lBound) _ -> Just lBound
    _                           -> Nothing

--------------------------------------------------------------------------------------

{-# INLINABLE checkIntervalSize #-}
checkIntervalSize :: LedgerApiV2.Interval LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Bool
checkIntervalSize iv len =
    case getLowerBoundFromInterval iv of
        Just t -> LedgerIntervalV1.interval t (t + len) `LedgerIntervalV1.contains` iv
        Nothing -> False

--------------------------------------------------------------------------------

{- Check that the tx range interval of validity. Must be lees than T.validTimeRange Pool Param. -}
{-# INLINABLE isValidRange #-}
isValidRange :: LedgerContextsV2.TxInfo -> Bool
isValidRange !info = checkIntervalSize (LedgerApiV2.txInfoValidRange info) T.validTimeRange

--------------------------------------------------------------------------------

{- Check if the date is correct.  -}
{-# INLINABLE isDateInRange #-}
isDateInRange :: LedgerApiV2.POSIXTime -> LedgerContextsV2.TxInfo -> Bool
isDateInRange !dateAt !info =
    -- XXX: +1 because timeRange lower closure may be False
   (dateAt + 1) `LedgerIntervalV1.member` LedgerApiV2.txInfoValidRange info

--------------------------------------------------------------------------------

{-# INLINABLE correctClaimValue #-}
correctClaimValue :: Integer -> Integer -> Integer -> Bool
-- correctClaimValue claim totalNewRewards = claim >= T.ppMinimunClaim && claim <= totalNewRewards
correctClaimValue !claim !totalNewRewards !available = claim <= totalNewRewards && claim <= available

--------------------------------------------------------------------------------

{-# INLINABLE validateMasterAction #-}
validateMasterAction :: T.PoolParams -> LedgerContextsV2.TxInfo -> T.Master -> Bool
validateMasterAction !pParams !info !master_InRedeemer =
        -- Check if this tx was signed by any of the Masters included in the PoolParams. 
        -- This are all the Masters that can interact with the script at any time. Nobody else will be able to change or redeem the funds.
        traceIfFalse "PPMSM" (signedByPoolParamMaster pParams info) && --Pool Params Master's signature missing 1

        -- Check if this tx was signed by the Master specified in the redeemer.
        traceIfFalse "MSM" (signedByMaster master_InRedeemer info) && --"Master Funder's signature missing"

        -- Check that the tx range interval of validity. Can't be infinitum and need to be lees than validTimeRange Pool Param . 
        traceIfFalse "RANGE" (isValidRange info) -- Tx validity time range is not valid

--------------------------------------------------------------------------------

{-# INLINABLE validateUserAction #-}
validateUserAction :: T.PoolParams -> LedgerContextsV2.TxInfo -> T.User -> T.User -> Bool --LedgerApiV2.CurrencySymbol -> 
validateUserAction _ !info !user_InRedeemer !user_In_New_OR_Edit_UserDatum = -- userPolicy

        -- Check if this tx was signed by the User specified in the redeemer.
        traceIfFalse "USM" (signedByUser user_InRedeemer info) && -- User's Signature missing

        -- Check the user in redeemer is the same in the new or edited UserDatum
        traceIfFalse "UR" (user_InRedeemer == user_In_New_OR_Edit_UserDatum) && -- Wrong User in Redeemer

        -- Check that the tx range interval of validity. Can't be infinitum and need to be lees than validTimeRange Pool Param . 
        traceIfFalse "RANGE" (isValidRange info) -- Tx validity time range is not valid

--------------------------------------------------------------------------------

