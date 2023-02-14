--{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
--{-# LANGUAGE DerivingStrategies         #-}
--{-# LANGUAGE FlexibleContexts           #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
--{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
--{-# LANGUAGE TypeApplications           #-}
--{-# LANGUAGE TypeFamilies               #-}
--{-# LANGUAGE TypeOperators              #-}
--{-# LANGUAGE RankNTypes                 #-}
--{-# LANGUAGE TupleSections              #-}
--{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE NumericUnderscores         #-}
{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE BangPatterns #-}
---- {-# LANGUAGE Strict #-}
-----------------------------------------------------------------------------------------
module Validators.StakePlusV2.Types.DatumsValidator where
-----------------------------------------------------------------------------------------
-- Import Externos
-----------------------------------------------------------------------------------------
import qualified Data.Aeson                             as DataAeson (ToJSON, FromJSON)
import qualified GHC.Generics                           as GHCGenerics (Generic)
import qualified Plutus.V2.Ledger.Api                   as LedgerApiV2
import qualified PlutusTx   
import           PlutusTx.Prelude                       ( otherwise, Bool(False), Integer, Maybe, Ordering(GT, LT), Eq(..), Ord((<)), (&&), ($), sortBy )
import qualified Prelude                                as P
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import Validators.StakePlusV2.Types.Types               ( Master, User ) 
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------

data MasterFunder = MasterFunder {
        mfMaster :: Master,
        mfFundAmount :: Integer,
        mfClaimedFund :: Integer,
        mfMinAda :: Integer
    }
    deriving (P.Ord, P.Eq, P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq MasterFunder where
    {-# INLINABLE (==) #-}
    mi1 == mi2 = mfMaster mi1 ==    mfMaster mi2 && 
                                    mfFundAmount mi1 == mfFundAmount mi2 && 
                                    mfClaimedFund mi1 == mfClaimedFund mi2 && 
                                    mfMinAda mi1 == mfMinAda mi2  
PlutusTx.makeIsDataIndexed ''MasterFunder [ 
        ('MasterFunder, 0)
    ]

------------------------------------------------------------------------------------------

data PoolDatumTypo  = PoolDatumTypo { 
        pdMasterFunders   :: [MasterFunder],
        pdFundCount :: Integer,
        pdTotalCashedOut :: Integer,
        pdClosedAt :: Maybe LedgerApiV2.POSIXTime,
        pdIsTerminated  :: Integer,
        pdIsEmergency  :: Integer,
        pdMinAda :: Integer
    }
    deriving (P.Ord, P.Eq, P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON)


instance Eq PoolDatumTypo where
    {-# INLINABLE (==) #-}
    ps1 == ps2 =   
                pdMasterFunders ps1 == pdMasterFunders ps2 &&
                pdFundCount ps1 == pdFundCount ps2 &&
                pdTotalCashedOut ps1 == pdTotalCashedOut ps2 &&
                pdClosedAt ps1 == pdClosedAt ps2 &&
                pdIsTerminated ps1 == pdIsTerminated ps2  &&
                pdMinAda ps1 == pdMinAda ps2 

PlutusTx.makeIsDataIndexed ''PoolDatumTypo [ 
        ('PoolDatumTypo, 0)
    ]

  
------------------------------------------------------------------------------------------

data FundDatumTypo  = FundDatumTypo { 
        fdFundAmount   :: Integer,
        fdCashedOut   :: Integer,
        fdMinAda :: Integer
    } 
    deriving (P.Ord, P.Eq, P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq FundDatumTypo where
    {-# INLINABLE (==) #-}
    ps1 == ps2 = 
                 fdFundAmount ps1 == fdFundAmount ps2 &&
                 fdCashedOut ps1 == fdCashedOut ps2  &&
                 fdMinAda ps1 == fdMinAda ps2

PlutusTx.makeIsDataIndexed ''FundDatumTypo [ 
        ('FundDatumTypo, 0)
    ]
------------------------------------------------------------------------------------------

data UserDatumTypo = UserDatumTypo
    {   
        udUser   :: User, 
        udInvest   :: Integer,
        udCreatedAt  :: LedgerApiV2.POSIXTime,
        udCashedOut   :: Integer,
        udRewardsNotClaimed   :: Integer,
        udLastClaimAt   :: Maybe LedgerApiV2.POSIXTime,
        udMinAda :: Integer
    } 
    deriving (P.Ord, P.Eq, P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq UserDatumTypo where
    {-# INLINABLE (==) #-}
    us1 == us2 =    
                    udUser  us1 == udUser  us2 &&
                    udInvest us1 == udInvest us2 &&
                    udCreatedAt us1 == udCreatedAt us2 &&
                    udCashedOut us1 == udCashedOut us2 &&
                    udRewardsNotClaimed us1 == udRewardsNotClaimed us2 &&
                    udLastClaimAt us1 == udLastClaimAt us2 &&
                    udMinAda us1 == udMinAda us2

PlutusTx.makeIsDataIndexed ''UserDatumTypo [ 
        ('UserDatumTypo, 0)
    ]

------------------------------------------------------------------------------------------

newtype ScriptDatumTypo = ScriptDatumTypo
    {  
        sdMaster   :: Master
    } 
    deriving (P.Ord, P.Eq, P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq ScriptDatumTypo where
    {-# INLINABLE (==) #-}
    sd1 == sd2 =   sdMaster  sd1 == sdMaster  sd2 
                    
PlutusTx.makeIsDataIndexed ''ScriptDatumTypo [ 
        ('ScriptDatumTypo, 0)
    ]

----------------------------------------------------------------------------------------

data DatumValidator = 
    PoolDatum PoolDatumTypo | 
    FundDatum FundDatumTypo | 
    UserDatum UserDatumTypo | 
    ScriptDatum ScriptDatumTypo 
    deriving (P.Ord, P.Eq, P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq DatumValidator where
    {-# INLINABLE (==) #-}
    PoolDatum mps1 == PoolDatum mps2 = mps1 == mps2
    FundDatum ps1 == FundDatum ps2 = ps1 == ps2
    UserDatum us1  == UserDatum us2 = us1 == us2
    ScriptDatum sd1 == ScriptDatum sd2 = sd1 == sd2
    _ == _ = False

PlutusTx.makeIsDataIndexed ''DatumValidator [ 
        ('PoolDatum, 0),
        ('FundDatum, 1), 
        ('UserDatum, 2),
        ('ScriptDatum, 3)
    ]
------------------------------------------------------------------------------------------

{-# INLINABLE mkMasterFunder #-}
mkMasterFunder :: Master -> Integer -> Integer -> Integer -> MasterFunder
mkMasterFunder master fund claimedFund minAda = MasterFunder { mfMaster = master, mfFundAmount = fund, mfClaimedFund = claimedFund, mfMinAda = minAda}

------------------------------------------------------------------------------------------

{-# INLINABLE mkPoolDatumTypo #-}
mkPoolDatumTypo :: [MasterFunder] -> Integer -> Integer -> Maybe LedgerApiV2.POSIXTime -> Integer -> Integer -> Integer -> PoolDatumTypo
mkPoolDatumTypo masterFunders fundCount totalCashedOut isClosedAt isTerminated isEmergency minAda =
    let
        compareMasterFunders :: MasterFunder -> MasterFunder -> Ordering
        compareMasterFunders masterFunder1 masterFunder2
            | mfMaster masterFunder1 < mfMaster masterFunder2 = LT
            | otherwise = GT

        !masterFundersOrder = sortBy compareMasterFunders masterFunders
        
    in
        PoolDatumTypo {
                pdMasterFunders = masterFundersOrder,
                pdFundCount = fundCount,
                pdTotalCashedOut = totalCashedOut,
                pdClosedAt = isClosedAt,
                pdIsTerminated = isTerminated,
                pdIsEmergency = isEmergency,
                pdMinAda = minAda
            }

------------------------------------------------------------------------------------------

{-# INLINABLE mkPoolDatum #-}
mkPoolDatum :: [MasterFunder] -> Integer -> Integer -> Maybe LedgerApiV2.POSIXTime -> Integer -> Integer -> Integer -> DatumValidator
mkPoolDatum masterFunders fundCount totalCashedOut isClosedAt isTerminated isEmergency minAda = PoolDatum $ mkPoolDatumTypo masterFunders fundCount totalCashedOut isClosedAt isTerminated isEmergency minAda

------------------------------------------------------------------------------------------
      
{-# INLINABLE mkFundDatumTypo #-}
mkFundDatumTypo :: Integer -> Integer -> Integer -> FundDatumTypo
mkFundDatumTypo fundAmount cashedout minAda  =
    FundDatumTypo {
            fdFundAmount = fundAmount, 
            fdCashedOut = cashedout,
            fdMinAda = minAda
        }

------------------------------------------------------------------------------------------

{-# INLINABLE mkFundDatum #-}
mkFundDatum :: Integer -> Integer -> Integer -> DatumValidator
mkFundDatum fundAmount cashedout minAda = FundDatum $ mkFundDatumTypo fundAmount cashedout minAda 

------------------------------------------------------------------------------------------

{-# INLINABLE mkUserDatumTypo #-}
mkUserDatumTypo ::User -> Integer -> LedgerApiV2.POSIXTime -> Integer -> Integer -> Maybe LedgerApiV2.POSIXTime  -> Integer ->  UserDatumTypo
mkUserDatumTypo  user invest createdat cashedout rewardsNotClaimed lastClaim minAda = 
    UserDatumTypo { 
        udUser = user, 
        udInvest = invest , 
        udCreatedAt = createdat, 
        udRewardsNotClaimed = rewardsNotClaimed, 
        udCashedOut = cashedout, 
        udLastClaimAt = lastClaim, 
        udMinAda = minAda
        }

{-# INLINABLE mkUserDatum #-}
mkUserDatum :: User -> Integer -> LedgerApiV2.POSIXTime -> Integer -> Integer -> Maybe LedgerApiV2.POSIXTime -> Integer ->  DatumValidator
mkUserDatum user invest createdat cashedout rewardsNotClaimed lastClaim minAda = UserDatum $ mkUserDatumTypo user invest createdat cashedout rewardsNotClaimed lastClaim minAda

------------------------------------------------------------------------------------------

type TxOut_With_Datum    = (LedgerApiV2.TxOut, DatumValidator)

--------------------------------------------------------------------------------------------

type TxOut_Value_And_Datum     = (LedgerApiV2.Value, DatumValidator)
type TxOut_Value_And_PoolDatum = (LedgerApiV2.Value, PoolDatumTypo)
type TxOut_Value_And_FundDatum = (LedgerApiV2.Value, FundDatumTypo)
type TxOut_Value_And_UserDatum = (LedgerApiV2.Value, UserDatumTypo)
type TxOut_Value_And_ScriptDatum = (LedgerApiV2.Value, ScriptDatumTypo)

type TxOut_Value_And_PoolDatumEX = (Integer, PoolDatumTypo)
type TxOut_Value_And_UserDatumEX = (Integer, UserDatumTypo)
type TxOut_Value_And_FundDatumEX = (Integer, FundDatumTypo)
type TxOut_Value_And_ScriptDatumEX = (Integer, ScriptDatumTypo)

--------------------------------------------------------------------------------------------