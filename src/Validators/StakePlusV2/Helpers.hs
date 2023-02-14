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
-- {-# LANGUAGE TemplateHaskell            #-}
-- {-# LANGUAGE TypeApplications           #-}
-- {-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE TupleSections              #-}
-- {-# LANGUAGE AllowAmbiguousTypes        #-}
-- {-# LANGUAGE NumericUnderscores         #-}

{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}

------------------------------------------------------------------------------------------
module Validators.StakePlusV2.Helpers where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Ada                                        as LedgerAda
import qualified Ledger.Value                                      as LedgerValue
import           PlutusTx.Prelude                                  ( otherwise, Bool(..), Integer, Maybe(..), Ordering(GT, LT), BuiltinByteString, Eq(..), Ord((>), (<=), (>=), (<), compare), AdditiveGroup((-)), AdditiveSemigroup((+)), Semigroup((<>)), (||), consByteString, emptyByteString, lengthOfByteString, (/=), traceError, ($), find, foldl, length, sum, head, sortBy, tail, negate, divide, quotient, remainder, MultiplicativeSemigroup((*)), not, all, zip, (++), (&&))
import qualified PlutusTx.AssocMap                                 as TxAssocMap
import qualified PlutusTx.Foldable                                 as TxFold
import qualified PlutusTx.Ratio                                    as TxRatio
import qualified Plutus.V2.Ledger.Api                              as LedgerApiV2
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Types.Constants            as T
import qualified Validators.StakePlusV2.Types.DatumsValidator      as T
import qualified Validators.StakePlusV2.Types.Types                as T (Master, InterestRate (iMinDays, iPercentage))
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------

{- | Function to return the Just value from a Maybe. -}
{-# INLINABLE fromJust #-}
fromJust :: Maybe a -> a
fromJust (Just !valueInfo)   = valueInfo
fromJust Nothing             = traceError "JN"

{-# INLINABLE enumerate #-}
enumerate :: [b] -> [(Integer, b)]
enumerate !x =
    let
        !len = length x

        createList :: Integer -> Integer -> [Integer] -> [Integer]
        createList !n !i !list
            | n == 0  = list
            | otherwise = createList (n-1) (i+1) (list ++ [i] )

    in
        zip (createList len 0 []) x

--------------------------------------------------------------------------------------

{-# INLINABLE isElement #-}
--check if an element is in a list
isElement :: Eq a => a -> [a] -> Bool
isElement _ [] = False
isElement !x (y:ys) = x == y || isElement x ys

--------------------------------------------------------------------------------------

{- | Check if the Datum is a PoolDatum. -}
{-# INLINABLE datumIs_PoolDatum #-}
datumIs_PoolDatum :: T.DatumValidator -> Bool
datumIs_PoolDatum (T.PoolDatum _) = True
datumIs_PoolDatum _               = False

{- | Check if the Datum is a FundDatum. -}
{-# INLINABLE datumIs_FundDatum #-}
datumIs_FundDatum :: T.DatumValidator -> Bool
datumIs_FundDatum (T.FundDatum _) = True
datumIs_FundDatum _               = False

{- | Check if the Datum is a UserDatum. -}
{-# INLINABLE datumIs_UserDatum #-}
datumIs_UserDatum :: T.DatumValidator -> Bool
datumIs_UserDatum (T.UserDatum _) = True
datumIs_UserDatum _               = False

{- | Check if the Datum is a ScriptDatum. -}
{-# INLINABLE datumIs_ScriptDatum #-}
datumIs_ScriptDatum :: T.DatumValidator -> Bool
datumIs_ScriptDatum (T.ScriptDatum _) = True
datumIs_ScriptDatum _                = False

--------------------------------------------------------------------------------------

{- | Check if the Datum is a PoolDatum. -}
{-# INLINABLE datumMaybeIs_PoolDatum #-}
datumMaybeIs_PoolDatum :: Maybe T.DatumValidator -> Bool
datumMaybeIs_PoolDatum (Just (T.PoolDatum _)) = True
datumMaybeIs_PoolDatum _                      = False

{- | Check if the Datum is a FundDatum. -}
{-# INLINABLE datumMaybeIs_FundDatum #-}
datumMaybeIs_FundDatum :: Maybe T.DatumValidator -> Bool
datumMaybeIs_FundDatum (Just (T.FundDatum _)) = True
datumMaybeIs_FundDatum _                      = False

{- | Check if the Datum is a UserDatum. -}
{-# INLINABLE datumMaybeIs_UserDatum #-}
datumMaybeIs_UserDatum :: Maybe T.DatumValidator -> Bool
datumMaybeIs_UserDatum (Just (T.UserDatum _)) = True
datumMaybeIs_UserDatum _                      = False

{-# INLINABLE datumMaybeIs_ScriptDatum #-}
datumMaybeIs_ScriptDatum :: Maybe T.DatumValidator -> Bool
datumMaybeIs_ScriptDatum (Just (T.ScriptDatum _)) = True
datumMaybeIs_ScriptDatum _                      = False

--------------------------------------------------------------------------------------

{- | Try to get the PoolDatum from a generic Maybe Datum. -}
{-# INLINABLE getPoolDatumTypo_FromMaybeDatum #-}
getPoolDatumTypo_FromMaybeDatum :: Maybe T.DatumValidator -> T.PoolDatumTypo
getPoolDatumTypo_FromMaybeDatum !datum =
    case datum of
        Just (T.PoolDatum poolDatum) -> poolDatum
        _                            -> traceError "GD"

{- | Get Just PoolDatum from a Datum. -}
{-# INLINABLE getPoolDatumTypo_FromDatum #-}
getPoolDatumTypo_FromDatum :: T.DatumValidator -> T.PoolDatumTypo
getPoolDatumTypo_FromDatum !datum =
    case datum of
        (T.PoolDatum poolDatum) -> poolDatum
        _                       -> traceError "GD"

--------------------------------------------------------------------------------------

{- |  Try to get the FundDatum from a generic Maybe Datum. -}
{-# INLINABLE getFundDatumTypo_FromMaybeDatum #-}
getFundDatumTypo_FromMaybeDatum :: Maybe T.DatumValidator -> T.FundDatumTypo
getFundDatumTypo_FromMaybeDatum !datum =
    case datum of
        Just (T.FundDatum fundDatum) -> fundDatum
        _                            -> traceError "GD"

{- | Get Just FundDatum from a Datum. -}
{-# INLINABLE getFundDatumTypo_FromDatum #-}
getFundDatumTypo_FromDatum :: T.DatumValidator -> T.FundDatumTypo
getFundDatumTypo_FromDatum !datum =
    case datum of
        (T.FundDatum fundDatum) -> fundDatum
        _                       -> traceError "GD"

--------------------------------------------------------------------------------------

{- |  Try to get the UserDatum from a generic Maybe Datum. -}
{-# INLINABLE getUserDatumTypo_FromMaybeDatum #-}
getUserDatumTypo_FromMaybeDatum :: Maybe T.DatumValidator -> T.UserDatumTypo
getUserDatumTypo_FromMaybeDatum !datum =
    case datum of
        Just (T.UserDatum userDatum) -> userDatum
        _                            -> traceError "GD"

{- | Get Just UserDatum from a Datum. -}
{-# INLINABLE getUserDatumTypo_FromDatum #-}
getUserDatumTypo_FromDatum :: T.DatumValidator -> T.UserDatumTypo
getUserDatumTypo_FromDatum !datum =
    case datum of
        (T.UserDatum userDatum) -> userDatum
        _                       -> traceError "GD"

--------------------------------------------------------------------------------------

{-# INLINABLE getScriptDatumTypo_FromMaybeDatum #-}
getScriptDatumTypo_FromMaybeDatum :: Maybe T.DatumValidator -> T.ScriptDatumTypo
getScriptDatumTypo_FromMaybeDatum !datum =
    case datum of
        Just (T.ScriptDatum scriptDatum)    -> scriptDatum
        _                                   -> traceError "GD"

{- | Get Just ScriptDatum from a Datum. -}
{-# INLINABLE getScriptDatumTypo_FromDatum #-}
getScriptDatumTypo_FromDatum :: T.DatumValidator -> T.ScriptDatumTypo
getScriptDatumTypo_FromDatum !datum =
    case datum of
        (T.ScriptDatum scriptDatum) -> scriptDatum
        _                           -> traceError "GD"

--------------------------------------------------------------------------------------

{-# INLINABLE mkUpdated_PoolDatum_With_NewFund #-}
mkUpdated_PoolDatum_With_NewFund :: T.PoolDatumTypo -> T.Master -> Integer -> Integer -> T.PoolDatumTypo
mkUpdated_PoolDatum_With_NewFund !poolDatum !master !fund !minAda =
    let
        !masterFundersAll = T.pdMasterFunders poolDatum
        !masterFunder_others = [ masterFunder | masterFunder <- masterFundersAll,  T.mfMaster masterFunder /= master]
        !masterFunderOld = find (\masterFunder -> T.mfMaster masterFunder == master) masterFundersAll
        !masterFundersNew =
            (case masterFunderOld :: Maybe T.MasterFunder of
                Just !mf ->
                    T.mkMasterFunder master (T.mfFundAmount mf + fund) T.poolDatum_NotClaimedFund (T.mfMinAda mf + minAda)
                _ -> T.mkMasterFunder master fund T.poolDatum_NotClaimedFund minAda
                :masterFunder_others)
    in
        T.mkPoolDatumTypo masterFundersNew (T.pdFundCount poolDatum + 1) (T.pdTotalCashedOut poolDatum) (T.pdClosedAt poolDatum) (T.pdIsTerminated poolDatum) (T.pdIsEmergency poolDatum) (T.pdMinAda poolDatum)

--------------------------------------------------------------------------------------

{-# INLINABLE mkUpdated_PoolDatum_With_NewFundAmountAndMerging #-}
mkUpdated_PoolDatum_With_NewFundAmountAndMerging :: T.PoolDatumTypo -> T.Master -> Integer -> Integer -> T.PoolDatumTypo
mkUpdated_PoolDatum_With_NewFundAmountAndMerging !poolDatum !master !fundAmount !mergingCount =
    let
        !masterFundersAll = T.pdMasterFunders poolDatum
        !masterFunder_others = [ masterFunder | masterFunder <- masterFundersAll,  T.mfMaster masterFunder /= master]
        !masterFunderOld = find (\masterFunder -> T.mfMaster masterFunder == master) masterFundersAll
        !masterFundersNew =
            (case masterFunderOld :: Maybe T.MasterFunder of
                Just !mf ->
                    T.mkMasterFunder master (T.mfFundAmount mf + fundAmount) T.poolDatum_NotClaimedFund (T.mfMinAda mf)
                _ -> T.mkMasterFunder master fundAmount T.poolDatum_NotClaimedFund 0
                :masterFunder_others)
    in
        T.mkPoolDatumTypo masterFundersNew (T.pdFundCount poolDatum - mergingCount + 1) (T.pdTotalCashedOut poolDatum) (T.pdClosedAt poolDatum) (T.pdIsTerminated poolDatum) (T.pdIsEmergency poolDatum) (T.pdMinAda poolDatum)

--------------------------------------------------------------------------------------

{-# INLINABLE mkUpdated_PoolDatum_With_SplitFundAmount #-}
mkUpdated_PoolDatum_With_SplitFundAmount :: T.PoolDatumTypo -> T.Master -> Integer -> T.PoolDatumTypo
mkUpdated_PoolDatum_With_SplitFundAmount !poolDatum !master !minAda =
    let
        !masterFundersAll = T.pdMasterFunders poolDatum
        !masterFunder_others = [ masterFunder | masterFunder <- masterFundersAll,  T.mfMaster masterFunder /= master]
        !masterFunderOld = find (\masterFunder -> T.mfMaster masterFunder == master) masterFundersAll
        !masterFundersNew =
            (case masterFunderOld :: Maybe T.MasterFunder of
                Just !mf ->
                    T.mkMasterFunder master (T.mfFundAmount mf) T.poolDatum_NotClaimedFund (T.mfMinAda mf + minAda)
                _ -> T.mkMasterFunder master 0 T.poolDatum_NotClaimedFund minAda
                :masterFunder_others)
    in
        T.mkPoolDatumTypo masterFundersNew (T.pdFundCount poolDatum + 1) (T.pdTotalCashedOut poolDatum) (T.pdClosedAt poolDatum) (T.pdIsTerminated poolDatum) (T.pdIsEmergency poolDatum) (T.pdMinAda poolDatum)

--------------------------------------------------------------------------------------

{-# INLINABLE mkUpdated_PoolDatum_With_ClosedAt #-}
mkUpdated_PoolDatum_With_ClosedAt :: T.PoolDatumTypo -> LedgerApiV2.POSIXTime -> T.PoolDatumTypo
mkUpdated_PoolDatum_With_ClosedAt !poolDatum !closedAt =
    T.mkPoolDatumTypo (T.pdMasterFunders poolDatum) (T.pdFundCount poolDatum ) (T.pdTotalCashedOut poolDatum) (Just closedAt) (T.pdIsTerminated poolDatum) (T.pdIsEmergency poolDatum) (T.pdMinAda poolDatum)

--------------------------------------------------------------------------------------

{-# INLINABLE mkUpdated_PoolDatum_With_Terminated #-}
mkUpdated_PoolDatum_With_Terminated :: T.PoolDatumTypo -> T.PoolDatumTypo
mkUpdated_PoolDatum_With_Terminated !poolDatum =
    let
        !isTerminated = T.poolDatum_Terminated
    in
        T.mkPoolDatumTypo (T.pdMasterFunders poolDatum) (T.pdFundCount poolDatum ) (T.pdTotalCashedOut poolDatum) (T.pdClosedAt poolDatum) isTerminated (T.pdIsEmergency poolDatum) (T.pdMinAda poolDatum)

--------------------------------------------------------------------------------------

{-# INLINABLE mkUpdated_PoolDatum_With_Emergency #-}
mkUpdated_PoolDatum_With_Emergency :: T.PoolDatumTypo -> Integer -> T.PoolDatumTypo
mkUpdated_PoolDatum_With_Emergency !poolDatum !isEmergency =
    T.mkPoolDatumTypo (T.pdMasterFunders poolDatum) (T.pdFundCount poolDatum ) (T.pdTotalCashedOut poolDatum) (T.pdClosedAt poolDatum) (T.pdIsTerminated poolDatum) isEmergency (T.pdMinAda poolDatum)

--------------------------------------------------------------------------------------

{-# INLINABLE mkUpdated_PoolDatum_With_DeletingFunds #-}
mkUpdated_PoolDatum_With_DeletingFunds :: T.PoolDatumTypo -> Integer -> Integer -> T.PoolDatumTypo
mkUpdated_PoolDatum_With_DeletingFunds !poolDatum !mergingCount !mergingCashedOut =
    T.mkPoolDatumTypo (T.pdMasterFunders poolDatum) (T.pdFundCount poolDatum  - mergingCount) (T.pdTotalCashedOut poolDatum + mergingCashedOut) (T.pdClosedAt poolDatum) (T.pdIsTerminated poolDatum) (T.pdIsEmergency poolDatum) (T.pdMinAda poolDatum)

--------------------------------------------------------------------------------------

{-# INLINABLE mkUpdated_PoolDatum_With_SendBackFund #-}
mkUpdated_PoolDatum_With_SendBackFund :: T.PoolDatumTypo -> T.Master -> T.PoolDatumTypo
mkUpdated_PoolDatum_With_SendBackFund !poolDatum !master =
    let
        !masterFundersAll = T.pdMasterFunders poolDatum
        !masterFunder_others = [ masterFunder | masterFunder <- masterFundersAll,  T.mfMaster masterFunder /= master]
        !masterFunderOld = find (\masterFunder -> T.mfMaster masterFunder == master) masterFundersAll
        !masterFundersNew =
            (case masterFunderOld :: Maybe T.MasterFunder of
                Just !mf ->
                    T.mkMasterFunder master (T.mfFundAmount mf) T.poolDatum_ClaimedFund (T.mfMinAda mf)
                _ ->
                    traceError "MF"
                :masterFunder_others)
    in
        T.mkPoolDatumTypo masterFundersNew (T.pdFundCount poolDatum) (T.pdTotalCashedOut poolDatum) (T.pdClosedAt poolDatum) (T.pdIsTerminated poolDatum) (T.pdIsEmergency poolDatum) (T.pdMinAda poolDatum)

--------------------------------------------------------------------------------------

{-# INLINABLE mkUpdated_FundDatum_WithNewFundAmountAndMerging #-}
mkUpdated_FundDatum_WithNewFundAmountAndMerging :: [T.FundDatumTypo] -> Integer -> T.FundDatumTypo
mkUpdated_FundDatum_WithNewFundAmountAndMerging !fundDatumsToMerge !fundAmount =
    let
        !fundAmount' = sum [ T.fdFundAmount datum | datum <- fundDatumsToMerge ] + fundAmount
        !cashedout = sum [ T.fdCashedOut datum | datum <- fundDatumsToMerge ]
        !minAda = sum [ T.fdMinAda datum | datum <- fundDatumsToMerge ]

    in  T.mkFundDatumTypo fundAmount' cashedout minAda

--------------------------------------------------------------------------------------

{-# INLINABLE mkUpdated_FundDatum_With_NewClaimRewards #-}
mkUpdated_FundDatum_With_NewClaimRewards :: T.FundDatumTypo -> Integer -> T.FundDatumTypo
mkUpdated_FundDatum_With_NewClaimRewards !fundDatum !cashedout  =
    T.mkFundDatumTypo (T.fdFundAmount fundDatum)  (T.fdCashedOut fundDatum + cashedout ) (T.fdMinAda fundDatum)

--------------------------------------------------------------------------------------

{-# INLINABLE mkUpdated_FundDatum_With_WithSplitFund #-}
mkUpdated_FundDatum_With_WithSplitFund :: T.FundDatumTypo -> Integer -> T.FundDatumTypo
mkUpdated_FundDatum_With_WithSplitFund !fundDatum !splitFundAmount  =
    T.mkFundDatumTypo (T.fdFundAmount fundDatum - splitFundAmount)  (T.fdCashedOut fundDatum ) (T.fdMinAda fundDatum)

--------------------------------------------------------------------------------------

{-# INLINABLE zeroToBBS #-}
zeroToBBS :: BuiltinByteString
zeroToBBS = emptyByteString --intToBBS 0

{-# INLINEABLE intToBBS #-}
intToBBS :: Integer -> BuiltinByteString
intToBBS !x
  -- 45 is ASCII code for '-'
  | x < 0 = consByteString 45 $ intToBBS (negate x)
  -- x is single-digit
  | x `quotient` 10 == 0 = digitToBS x
  | otherwise = intToBBS (x `quotient` 10) <> digitToBS (x `remainder` 10)
  where
    digitToBS :: Integer -> BuiltinByteString
    -- 48 is ASCII code for '0'
    digitToBS !d = consByteString (d + 48) emptyByteString

{-# INLINABLE txOutRefToBBS #-}
txOutRefToBBS :: LedgerApiV2.TxOutRef -> BuiltinByteString
txOutRefToBBS !txOutRef =
    let
        idTxOut = LedgerApiV2.txOutRefId txOutRef
        indexTxOut = LedgerApiV2.txOutRefIdx txOutRef
    in  intToBBS indexTxOut <> LedgerApiV2.getTxId idTxOut

{-# INLINABLE flatValueToBBS #-}
flatValueToBBS :: LedgerApiV2.CurrencySymbol -> LedgerApiV2.TokenName -> Integer -> BuiltinByteString
flatValueToBBS !cs !tn !am = LedgerApiV2.unCurrencySymbol cs <> LedgerApiV2.unTokenName tn <> intToBBS am

{-# INLINABLE txValueToBBS #-}
txValueToBBS :: LedgerApiV2.Value -> BuiltinByteString
txValueToBBS !value = foldl (<>) zeroToBBS [ flatValueToBBS cs tn am | (cs, tn, am) <- LedgerValue.flattenValue value]

{-# INLINABLE assetClassToBBS #-}
assetClassToBBS :: LedgerValue.AssetClass -> BuiltinByteString
assetClassToBBS !ac =
    let !(cs, tn) = LedgerValue.unAssetClass ac
    in  LedgerApiV2.unCurrencySymbol cs <> LedgerApiV2.unTokenName tn

{-# INLINABLE pOSIXTimeToBBS #-}
pOSIXTimeToBBS :: LedgerApiV2.POSIXTime -> BuiltinByteString
pOSIXTimeToBBS !time = intToBBS $ LedgerApiV2.getPOSIXTime time

--------------------------------------------------------------------------------------

{-# INLINABLE isToken_With_AC_InValue #-}
isToken_With_AC_InValue :: LedgerApiV2.Value -> LedgerValue.AssetClass -> Bool
isToken_With_AC_InValue !value !ac = LedgerValue.assetClassValueOf value ac >= 1

{-# INLINABLE isToken_With_TN_InValue #-}
isToken_With_TN_InValue :: LedgerApiV2.Value -> LedgerApiV2.TokenName -> Bool
isToken_With_TN_InValue !value !tn = getAmtOfTokenName value tn >= 1

{-# INLINABLE isToken_With_CS_InValue #-}
isToken_With_CS_InValue :: LedgerApiV2.Value -> LedgerApiV2.CurrencySymbol -> Bool
isToken_With_CS_InValue !value !cs = getAmtOfCurrencySymbol value cs >= 1

{-# INLINABLE isToken_With_AC_AndAmt_InValue #-}
isToken_With_AC_AndAmt_InValue :: LedgerApiV2.Value -> LedgerValue.AssetClass -> Integer -> Bool
isToken_With_AC_AndAmt_InValue !value !ac !amt = LedgerValue.assetClassValueOf value ac == amt

------------------------------------------------------------------------------------------------

{-# INLINABLE isNFT_With_AC_InValue #-}
isNFT_With_AC_InValue :: LedgerApiV2.Value -> LedgerValue.AssetClass -> Bool
isNFT_With_AC_InValue !value !ac = LedgerValue.assetClassValueOf value ac == 1

{-# INLINABLE isNFT_With_TN_InValue #-}
isNFT_With_TN_InValue :: LedgerApiV2.Value -> LedgerApiV2.TokenName -> Bool
isNFT_With_TN_InValue !value !tn = getAmtOfTokenName value tn == 1

{-# INLINABLE isNFT_With_CS_InValue #-}
isNFT_With_CS_InValue :: LedgerApiV2.Value -> LedgerApiV2.CurrencySymbol -> Bool
isNFT_With_CS_InValue !value !cs = getAmtOfCurrencySymbol value cs == 1

------------------------------------------------------------------------------------------------

{-# INLINABLE flattenValue #-}
flattenValue :: LedgerValue.Value -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
flattenValue (LedgerValue.Value !mp) =
    let
        !f1 = TxAssocMap.toList mp
        !f2 = [ ( cs , TxAssocMap.toList mp') | (cs, mp') <- f1 ]
        !f3 = [ (cs , tn, amt) | (cs, f4) <- f2, (tn, amt) <- f4 ]
    in
        f3

{-# INLINABLE flattenValueDeleteZeros #-}
flattenValueDeleteZeros :: LedgerValue.Value -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
flattenValueDeleteZeros (LedgerValue.Value !mp) =
    let
        !f1 = TxAssocMap.toList mp
        !f2 = [ ( cs , TxAssocMap.toList mp') | (cs, mp') <- f1 ]
        !f3 = [ (cs , tn, amt) | (cs, f4) <- f2, (tn, amt) <- f4, amt /= 0 ]
    in
        f3

{-# INLINABLE sortFlattenValue #-}
sortFlattenValue :: LedgerValue.Value -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
sortFlattenValue (LedgerValue.Value !mp) =
    let
        sort_CurrencySymbol :: (LedgerApiV2.CurrencySymbol, a) -> (LedgerApiV2.CurrencySymbol, a) -> Ordering
        sort_CurrencySymbol (!cs1, _) (!cs2, _) = compare cs1 cs2

        sort_TokenName :: (LedgerApiV2.TokenName, a) -> (LedgerApiV2.TokenName, a) -> Ordering
        sort_TokenName (!tn1, _) (!tn2, _) = compare tn1 tn2

        !f1 = sortBy sort_CurrencySymbol (TxAssocMap.toList mp)

        !f2 = [ ( cs ,  sortBy sort_TokenName  (TxAssocMap.toList mp')) | (cs, mp') <- f1 ]

        !f3 = [ (cs , tn, amt) | (cs, f4) <- f2, (tn, amt) <- f4 ]
    in
        f3


------------------------------------------------------------------------------------------------

{-# INLINABLE getValueOfAC #-}
-- | Get the total value of an Aseets Class in the 'Value'.
getValueOfAC :: LedgerValue.Value -> LedgerValue.AssetClass -> LedgerValue.Value
getValueOfAC !v !ac =
    LedgerValue.assetClassValue ac (LedgerValue.assetClassValueOf v ac)

------------------------------------------------------------------------------------------------

{-# INLINABLE getValueOfCurrencySymbol #-}
-- | Get the total value of a CurrencySymbol in the 'Value'. Doesnt matter the TokenName
getValueOfCurrencySymbol :: LedgerValue.Value -> LedgerApiV2.CurrencySymbol -> LedgerValue.Value
getValueOfCurrencySymbol (LedgerValue.Value !mp) !cs =
    case TxAssocMap.lookup cs mp of
        Nothing -> LedgerAda.lovelaceValueOf 0
        Just mp' -> LedgerValue.Value $ TxAssocMap.singleton cs mp'

------------------------------------------------------------------------------------------------

{-# INLINABLE getAmtOfCurrencySymbol #-}
-- | Get the quantity of the given CurrencySymbol in the 'Value'. Doesnt matter the TokenName
getAmtOfCurrencySymbol :: LedgerValue.Value -> LedgerApiV2.CurrencySymbol -> Integer
getAmtOfCurrencySymbol !value !cs =
    TxFold.foldl (+) 0 [ am | (cs', _, am) <- flattenValue value, cs' == cs ]

------------------------------------------------------------------------------------------------

{-# INLINABLE getAmtOfTokenName #-}
-- | Get the quantity of the given TokenName in the 'Value'. Doesnt matter the CurrencySymbol
getAmtOfTokenName :: LedgerValue.Value -> LedgerApiV2.TokenName -> Integer
getAmtOfTokenName !value !tn =
    TxFold.foldl (+) 0 [ am | (_, tn', am) <- flattenValue value, tn' == tn ]

------------------------------------------------------------------------------------------------

{-# INLINABLE getCurrencySymbol_Of_TokenName_InValue #-}
-- | Get the CurrencySymbol of the given TokenName in the 'Value'.
getCurrencySymbol_Of_TokenName_InValue :: LedgerValue.Value -> LedgerApiV2.TokenName -> LedgerApiV2.CurrencySymbol
getCurrencySymbol_Of_TokenName_InValue !value !tn =
    head [ cs | (cs, tn', _) <- flattenValue value, tn' == tn ]

------------------------------------------------------------------------------------------------

{-# INLINABLE createValueAddingTokensOfCurrencySymbol #-}
createValueAddingTokensOfCurrencySymbol :: LedgerValue.AssetClass -> LedgerApiV2.CurrencySymbol -> Bool -> LedgerApiV2.Value -> Integer -> LedgerApiV2.Value
createValueAddingTokensOfCurrencySymbol !ac !cs !acIsWithoutTokenName !value !cantidad =
    if not acIsWithoutTokenName then
        LedgerValue.assetClassValue ac cantidad
    else
        let
            !tokenOfCurrencySymbol = [ (tn, am) | (cs', tn, am) <- flattenValue value, cs' == cs ]

            compareTokenName :: (LedgerApiV2.TokenName, Integer) -> (LedgerApiV2.TokenName, Integer) -> Ordering
            compareTokenName (!tn1, _) (!tn2, _)
                | tn1 < tn2 = LT
                | otherwise = GT

            !tokenOfCurrencySymbol_Ordered = sortBy compareTokenName tokenOfCurrencySymbol

            sumarTokens :: [(LedgerApiV2.TokenName, Integer)] -> Integer -> LedgerApiV2.Value
            sumarTokens [] !left =
                if left > 0 then do
                    traceError "TOKENS"
                else
                    LedgerAda.lovelaceValueOf 0
            sumarTokens !list !left =
                let
                    (tn, am) = head list
                    !harvest_AC = LedgerValue.AssetClass (cs, tn)
                in
                    if am > left then
                        LedgerValue.assetClassValue harvest_AC left
                    else
                        LedgerValue.assetClassValue harvest_AC am <> sumarTokens (tail list) (left - am)
        in
            sumarTokens tokenOfCurrencySymbol_Ordered cantidad

------------------------------------------------------------------------------------------------

{-# INLINABLE valueIncludesValue #-}
valueIncludesValue :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueIncludesValue !value !valueToFind  =
    let
        !valueToFind' = flattenValue valueToFind
    in
        all (\(cs, tn, amount) ->
                let
                    !ac = LedgerValue.AssetClass (cs, tn)
                in 
                    LedgerValue.assetClassValueOf value ac >= amount
            ) valueToFind'

{-# INLINABLE valueEqualsValue #-}
valueEqualsValue :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue !value1 !value2 = 
    let
        !value1' = flattenValueDeleteZeros value1
        !value2' = flattenValueDeleteZeros value2
    --     -- !value1' = sortFlattenValue value1
    --     -- !value2' = sortFlattenValue value2
    --     -- sortFlattenValue
    in
        -- value1' == value2' &&
        length value1' == length value2' &&
        -- all (\(cs, tn, amount) ->
        --         isJust (find (\(cs', tn', amount') -> cs == cs' && tn == tn' && amount == amount') value2')
        --     ) value1'   &&
        all (\(cs, tn, amount) -> 
                let 
                    !ac = LedgerValue.AssetClass (cs, tn)
                in 
                    LedgerValue.assetClassValueOf value2 ac == amount
            ) value1'

        -- value1 == value2

-------------------------------------------------------------------------------------------


{-# INLINABLE calculateMinAda #-}
calculateMinAda :: Integer -> Integer -> Integer -> Bool -> Integer
calculateMinAda !numAssets !sumAssetNameLengths !numPIDs !isHash =
    let
        -- const numPIDs=1
        -- The number of policy scripts referenced in the UTxO. If there is only one type of token in the UTxO, then this is just 1.
        -- var numAssets=1
        -- The number of asset names present in the UTxO. If there is only one type of token, then this is just 1.
        -- const sumAssetNameLengths=32
        -- Bytes    The number of bytes needed to store all of the asset names. If these do not include unicode characters (e.g., emojis), then this is just the total number of letters (characters) in the asset names. For instance, a token named "Test" needs 4 bytes for its name.

        --Fixed parameters
        !minUTxOValue  =1000000 :: Integer
        --ADA	The minimum number of ADA that must be present in ADA-only UTxOs.
        !pidSize=28
        --Bytes	The number of bytes in a policy ID.
        !coinSize=2
        --Bytes	At the Alonzo HFC, this parameter was corrected to be 2 because the original value 0 was an implementation error.
        !uTxOEntrySizeWithoutVal=27
        --Bytes	The number of bytes in a transaction if there were no value at all in it.
        !adaOnlyUTxOSize=uTxOEntrySizeWithoutVal + coinSize
        --Bytes	The number of bytes in a transaction if it were to only contain ADA.
        !coinsPerUTxOWord = TxRatio.truncate $ TxRatio.unsafeRatio minUTxOValue adaOnlyUTxOSize -- = 34482 
        -- coinsPerUTxOByte =  TxRatio.truncate $ TxRatio.unsafeRatio coinsPerUTxOWord  8 -- = 4310.25

        !hash = if isHash then (10 :: Integer) else 0 --si hay data hash suman 10 words

        roundupBytesToWords :: Integer -> Integer
        roundupBytesToWords !number = TxRatio.truncate ( TxRatio.unsafeRatio (number + 7)  8)

        !sizeWords = 6 + roundupBytesToWords (numAssets * 12 + sumAssetNameLengths + numPIDs * pidSize )

        !sizeCoins = coinsPerUTxOWord * (uTxOEntrySizeWithoutVal + sizeWords + hash)
        --sizeCoins =  coinsPerUTxOByte * (160 + (sizeWords + hash )* 8 ) 

        !minAda =  if minUTxOValue > sizeCoins then minUTxOValue else sizeCoins
        -- minAda =  if minUTxOValue > sizeCoins then minUTxOValue else sizeCoins
    in
        TxRatio.truncate (TxRatio.unsafeRatio (130*minAda) 100) -- 130% of the minimum UTxO value


{-# INLINABLE calculateMinAdaOfValue #-}
calculateMinAdaOfValue :: LedgerApiV2.Value -> Bool -> Integer
calculateMinAdaOfValue !value !isHash =
    let
        -- !valueWithOutAda = value <> negate ( LedgerValue.adaOnlyValue value)
        !valueWithOutAda = value
        !flattenValue' = flattenValue valueWithOutAda
        -- por que siempre esta el pid de ada, no lo pude eliminar ni con la resta anterior (valueWithOutAda)
        -- al hacer LedgerValue.Value mp, el mapa pm contiene de nuevo el pid de ada, con 0 amount.
        sumarPId (LedgerValue.Value mp) = length (TxAssocMap.toList mp) - 1
        !numPIDs = sumarPId valueWithOutAda
        !numAssets = length [ tn | (_, tn, amt) <- flattenValue' , amt > 0 ]
        !sumAssetNameLengths = sum [ lengthOfByteString  $ LedgerApiV2.unTokenName tn | (_, tn, amt) <- flattenValue' , amt > 0 ]

        !minAda =  calculateMinAda numAssets sumAssetNameLengths numPIDs isHash
    in
        minAda

------------------------------------------------------------------------------------------------

{-# INLINABLE getFundAmountCanUse_in_FundDatum #-}
getFundAmountCanUse_in_FundDatum :: T.FundDatumTypo -> Integer
getFundAmountCanUse_in_FundDatum !fundDatum =
    T.fdFundAmount fundDatum - T.fdCashedOut fundDatum

--------------------------------------------------------------------------------------

{-# INLINABLE getRewardsPerInvest #-}
getRewardsPerInvest :: LedgerApiV2.POSIXTime -> Maybe LedgerApiV2.POSIXTime -> [T.InterestRate] -> Maybe LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> Integer -> Integer
getRewardsPerInvest !deadline !closedAt !interestRates !lastClaim !now !depositTime !invest !rewardsNotClaimed =
    case lastClaim of
        Nothing     -> getRewards $ LedgerApiV2.getPOSIXTime (upperTime - lowerTime upperTime depositTime)
        Just lClaim -> getRewards $ LedgerApiV2.getPOSIXTime (upperTime - lowerTime upperTime lClaim)
  where

    !upperTime =
        case closedAt  of
            Just x -> x
            Nothing ->
                if now > deadline then
                    deadline
                else
                    now

    lowerTime upperTime' depositOrLClaim =
        if depositOrLClaim > upperTime then
            upperTime'
        else
            depositOrLClaim

    -- El interes es deacuerdo a la fecha total que lleva de inversion
    diffForInterestRate :: LedgerApiV2.POSIXTime
    !diffForInterestRate = upperTime - depositTime

    msPerDay :: Integer
    !msPerDay = 1000 * 60 * 60 * 24

    msPerYear :: Integer
    !msPerYear = msPerDay * 365

    --Days to LedgerApiV2.POSIXTime
    days :: Integer -> LedgerApiV2.POSIXTime
    days !n = LedgerApiV2.POSIXTime (n * msPerDay)

    -- Explicacion de la formula

    --getRewards duration = (getLevel * duration * invest) -`divide` msPerYear
    --getLevel = levelAPR $ depositLevel depositTime now

    -- si quiero que gane 1 por cada slot (que duran 1s o 1000ms) 
    -- en una invest de 100 ADA, durante 1s gano 100 ADA con este calculo:
    -- (interestRate % * duration ms * invest     )  `divide` (1000ms)    
    -- (1              * 1000 ms     * 100_000_000)  `divide` (1000  ) = 100_000_000 = 1 ADA

    -- ESTE USO EN TESTING --> 
    -- si quiero que gane 1 cada decima de segundo, o sea 1s/10 = 0.1s o 1000ms/10 = 100ms
    -- en una invest de 100 ADA durante 1s gano 1000 ADA con este calculo:
    -- (interestRate % * duration ms * invest     )  `divide` (100ms)   
    -- (1              * 1000 ms     * 100_000_000)  `divide` (100  ) = 1_000_000_000 = 1000 ADA

    -- si quiero que gane 1 cada año, tengo el calculo de ms por año en msPerYear = 1000 * 60 * 60 * 24 * 365 = 31536000000 ms
    -- en una invest de 100 ADA durante 1 año gano 100 ADA con este calculo:
    -- (interestRate % * duration ms     * invest     )  `divide` (msPerYear  )   
    -- (1              * 31536000000     * 100_000_000)  `divide` (31536000000) = 100_000_000 = 100 ADA

    -- si quiero que gane 5 cada año, tengo el calculo de ms por año en msPerYear = 1000 * 60 * 60 * 24 * 365 = 31536000000 ms
    -- en una invest de 100 ADA durante 1 año gano 500 ADA con este calculo:
    -- (interestRate % * duration ms     * invest     )  `divide` (msPerYear  )   
    -- (5              * 31536000000     * 100_000_000)  `divide` (31536000000) = 500_000_000 = 500 ADA

    -- si quiero que gane 2 cada año, tengo el calculo de ms por año en msPerYear = 1000 * 60 * 60 * 24 * 365 = 31536000000 ms
    -- en una invest de 100 ADA durante 6 meses gano 100 ADA con este calculo:
    -- (interestRate % * duration ms     * invest     )  `divide` (msPerYear  )   
    -- (2              * 31536000000/2   * 100_000_000)  `divide` (31536000000) = 100_000_000 = 100 ADA

    getRewards :: Integer -> Integer
    getRewards !duration =
        let
            !rewards = (getInterestRate interestRates * duration * invest) `divide` msPerYear
        in
            if rewards + rewardsNotClaimed > T.maxRewards then
                T.maxRewards - rewardsNotClaimed
            else
                rewards

    isDiffLessThanMinDays :: LedgerApiV2.POSIXTime -> Maybe Integer -> Bool
    isDiffLessThanMinDays !diff' !minDays =
        case minDays of
            Nothing -> True -- el ultimo valor de la lista de intereses no tiene minDays, es Nothing, para que toda diff caiga ahí si no cae en otro grupo
            Just d  -> diff' <= days d

    getInterestRate :: [T.InterestRate]  -> Integer
    getInterestRate !interestRates' =
        case interestRates' of
            []   -> traceError "INT" -- no debería suceder que no encuentra un rate adecuado...
            x:xs -> if isDiffLessThanMinDays diffForInterestRate (T.iMinDays x) then
                        T.iPercentage x
                    else
                       getInterestRate xs

------------------------------------------------------------------------------------------------

{-# INLINABLE getFundAmountsRemains_ForMaster #-}
getFundAmountsRemains_ForMaster :: T.PoolDatumTypo -> T.Master -> (Integer, Integer)
getFundAmountsRemains_ForMaster !poolDatum !master =
    let
        !masterFundersAll = T.pdMasterFunders poolDatum
        ---------------------
        !totalFunding = sum [ T.mfFundAmount mf | mf <- masterFundersAll ]
        !totalRewardsCashedOut = T.pdTotalCashedOut poolDatum
        !remaindFunds = totalFunding - totalRewardsCashedOut
        ---------------------
        !totalMinAda = sum [ T.mfMinAda mf | mf <- masterFundersAll ]
        ---------------------
        !masterFunder = find  (\mF' -> T.mfMaster mF' == master) masterFundersAll
    in
        case masterFunder of
            Nothing -> traceError "MF"
            Just mf ->
                if T.mfClaimedFund mf == T.poolDatum_ClaimedFund then
                    traceError "MFGB"
                else
                    let
                        !masterFundAmount = T.mfFundAmount mf
                        !masterParticipation = (masterFundAmount * 1000000000) `divide` totalFunding
                        !masterMinAda = T.mfMinAda mf
                        !masterParticipationAda = (masterMinAda * 1000000000) `divide` totalMinAda
                    in
                        (
                            (masterParticipation * remaindFunds) `divide` 1000000000,
                            (masterParticipationAda * totalMinAda) `divide` 1000000000
                        )

-------------------------------------------------------------------------------------------
