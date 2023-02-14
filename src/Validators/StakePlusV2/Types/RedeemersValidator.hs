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
------------------------------------------------------------------------------------------
module Validators.StakePlusV2.Types.RedeemersValidator where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Data.Aeson                             as DataAeson (ToJSON, FromJSON)
import qualified GHC.Generics                           as GHCGenerics (Generic)
import qualified Plutus.V2.Ledger.Api                   as LedgerApiV2
import qualified PlutusTx   
import           PlutusTx.Prelude                       ( Bool(False), Integer, Eq(..), (&&), ($) )
import qualified Prelude                                as P
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import Validators.StakePlusV2.Types.Types               ( Master, User )
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------

data RedeemerMasterFundTypo  = RedeemerMasterFundTypo { 
        rmfMaster :: Master,
        rmfFundAmount :: Integer,
        rmfMinAda :: Integer
    } 
    deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON) 

instance Eq RedeemerMasterFundTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 = rmfMaster r1   == rmfMaster  r2  &&
               rmfFundAmount r1 == rmfFundAmount r2 &&
               rmfMinAda r1 == rmfMinAda r2

PlutusTx.makeIsDataIndexed ''RedeemerMasterFundTypo [ 
        ('RedeemerMasterFundTypo, 0)
    ]

------------------------------------------------------------------------------------------

data RedeemerMasterFundAndMergeTypo  = RedeemerMasterFundAndMergeTypo { 
        rmfamMaster :: Master,
        rmfamFundAmount :: Integer
    } 
    deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON) 

instance Eq RedeemerMasterFundAndMergeTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 = rmfamMaster r1 == rmfamMaster r2 &&
               rmfamFundAmount r1 == rmfamFundAmount r2

PlutusTx.makeIsDataIndexed ''RedeemerMasterFundAndMergeTypo [ 
        ('RedeemerMasterFundAndMergeTypo, 0)
    ]

------------------------------------------------------------------------------------------

data RedeemerMasterSplitFundTypo  = RedeemerMasterSplitFundTypo { 
        rmsfMaster :: Master,
        rmsfSplitFundAmount :: Integer,
        rmsfMinAda :: Integer
    } 
    deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON) 

instance Eq RedeemerMasterSplitFundTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  rmsfMaster r1 == rmsfMaster r2 &&
                rmsfSplitFundAmount r1 == rmsfSplitFundAmount r2 &&
                rmsfMinAda r1 == rmsfMinAda r2

PlutusTx.makeIsDataIndexed ''RedeemerMasterSplitFundTypo [ 
        ('RedeemerMasterSplitFundTypo, 0)
    ]

------------------------------------------------------------------------------------------

data RedeemerMasterClosePoolTypo  = RedeemerMasterClosePoolTypo { 
        rmcpMaster :: Master,
        rmcpClosedAt :: LedgerApiV2.POSIXTime
    } 
    deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON) 

instance Eq RedeemerMasterClosePoolTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 = rmcpMaster r1 == rmcpMaster r2 &&
               rmcpClosedAt r1 == rmcpClosedAt r2

PlutusTx.makeIsDataIndexed ''RedeemerMasterClosePoolTypo [ 
        ('RedeemerMasterClosePoolTypo, 0)
    ]

------------------------------------------------------------------------------------------

newtype RedeemerMasterTerminatePoolTypo  = RedeemerMasterTerminatePoolTypo { 
        rmtpMaster :: Master
    } 
    deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON) 

instance Eq RedeemerMasterTerminatePoolTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 = rmtpMaster r1 == rmtpMaster r2

PlutusTx.makeIsDataIndexed ''RedeemerMasterTerminatePoolTypo [ 
        ('RedeemerMasterTerminatePoolTypo, 0)
    ]

------------------------------------------------------------------------------------------

newtype RedeemerMasterEmergencyTypo  = RedeemerMasterEmergencyTypo { 
        rmeMaster :: Master
    } 
    deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON) 

instance Eq RedeemerMasterEmergencyTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 = rmeMaster r1 == rmeMaster r2

PlutusTx.makeIsDataIndexed ''RedeemerMasterEmergencyTypo [ 
        ('RedeemerMasterEmergencyTypo, 0)
    ]

------------------------------------------------------------------------------------------

newtype RedeemerMasterDeleteFundTypo  = RedeemerMasterDeleteFundTypo { 
        rmdfMaster :: Master
    } 
    deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON) 

instance Eq RedeemerMasterDeleteFundTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 = rmdfMaster r1 == rmdfMaster r2

PlutusTx.makeIsDataIndexed ''RedeemerMasterDeleteFundTypo [ 
        ('RedeemerMasterDeleteFundTypo, 0)
    ]


------------------------------------------------------------------------------------------

data RedeemerMasterSendBackFundTypo  = RedeemerMasterSendBackFundTypo { 
        rmsbfMaster :: Master,
        rmsbfMasterToSendBack :: Master
    } 
    deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON) 

instance Eq RedeemerMasterSendBackFundTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 = 
        rmsbfMaster r1 == rmsbfMaster r2 &&
        rmsbfMasterToSendBack r1 == rmsbfMasterToSendBack r2 

PlutusTx.makeIsDataIndexed ''RedeemerMasterSendBackFundTypo [ 
        ('RedeemerMasterSendBackFundTypo, 0)
    ]

------------------------------------------------------------------------------------------

data RedeemerMasterSendBackDepositTypo  = RedeemerMasterSendBackDepositTypo { 
        rmsbdMaster :: Master,
        rmsbdUserToSendBack :: User
    } 
    deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON) 

instance Eq RedeemerMasterSendBackDepositTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 = 
        rmsbdMaster r1 == rmsbdMaster r2 &&
        rmsbdUserToSendBack r1 == rmsbdUserToSendBack r2

PlutusTx.makeIsDataIndexed ''RedeemerMasterSendBackDepositTypo [ 
        ('RedeemerMasterSendBackDepositTypo, 0)
    ]

------------------------------------------------------------------------------------------

newtype RedeemerMasterAddScriptsTypo  = RedeemerMasterAddScriptsTypo { 
        rmasMaster :: Master
    } 
    deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON) 

instance Eq RedeemerMasterAddScriptsTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 = rmasMaster r1   == rmasMaster  r2 

PlutusTx.makeIsDataIndexed ''RedeemerMasterAddScriptsTypo [ 
        ('RedeemerMasterAddScriptsTypo, 0)
    ]

------------------------------------------------------------------------------------------

newtype RedeemerMasterDeleteScriptsTypo  = RedeemerMasterDeleteScriptsTypo { 
        rmdsMaster :: Master
    } 
    deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON) 

instance Eq RedeemerMasterDeleteScriptsTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  rmdsMaster r1   == rmdsMaster  r2 

PlutusTx.makeIsDataIndexed ''RedeemerMasterDeleteScriptsTypo [ 
        ('RedeemerMasterDeleteScriptsTypo, 0)
    ]


------------------------------------------------------------------------------------------

data RedeemerUserDepositTypo  = RedeemerUserDepositTypo { 
        rudUser :: User,
        rudInvestAmount :: Integer,
        rudCreatedAt :: LedgerApiV2.POSIXTime,
        rudMinAda :: Integer 
    }  
    deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON) 

instance Eq RedeemerUserDepositTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 = rudUser r1 == rudUser r2 &&
               rudInvestAmount r1 == rudInvestAmount r2  &&
               rudCreatedAt r1 == rudCreatedAt r2  &&
               rudMinAda r1 == rudMinAda r2 

PlutusTx.makeIsDataIndexed ''RedeemerUserDepositTypo [ 
        ('RedeemerUserDepositTypo, 0)
    ]

------------------------------------------------------------------------------------------

data RedeemerUserHarvestTypo  = RedeemerUserHarvestTypo { 
        ruhUser :: User ,
        ruhClaimAmount :: Integer,
        ruhClaimAt :: LedgerApiV2.POSIXTime
    }  
    deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON) 

instance Eq RedeemerUserHarvestTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 = 
        ruhUser r1 == ruhUser r2 &&
        ruhClaimAmount r1 == ruhClaimAmount r2  &&
        ruhClaimAt r1 == ruhClaimAt r2 

PlutusTx.makeIsDataIndexed ''RedeemerUserHarvestTypo [ 
        ('RedeemerUserHarvestTypo, 0)
    ]

------------------------------------------------------------------------------------------

newtype RedeemerUserWithdrawTypo  = RedeemerUserWithdrawTypo { 
        ruwUser :: User 
    }  
    deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON) 

instance Eq RedeemerUserWithdrawTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 = ruwUser r1 == ruwUser r2 

PlutusTx.makeIsDataIndexed ''RedeemerUserWithdrawTypo [ 
        ('RedeemerUserWithdrawTypo, 0)
    ]

------------------------------------------------------------------------------------------

data RedeemerValidator = 
    RedeemerMasterFund RedeemerMasterFundTypo | 
    RedeemerMasterFundAndMerge RedeemerMasterFundAndMergeTypo  | 
    RedeemerMasterSplitFund RedeemerMasterSplitFundTypo | 
    RedeemerMasterClosePool RedeemerMasterClosePoolTypo | 
    RedeemerMasterTerminatePool RedeemerMasterTerminatePoolTypo | 
    RedeemerMasterEmergency RedeemerMasterEmergencyTypo | 
    RedeemerMasterDeleteFund RedeemerMasterDeleteFundTypo | 
    RedeemerMasterSendBackFund RedeemerMasterSendBackFundTypo |  
    RedeemerMasterSendBackDeposit RedeemerMasterSendBackDepositTypo |  
    RedeemerMasterAddScripts RedeemerMasterAddScriptsTypo | 
    RedeemerMasterDeleteScripts RedeemerMasterDeleteScriptsTypo | 
    RedeemerUserDeposit RedeemerUserDepositTypo  |   
    RedeemerUserHarvest RedeemerUserHarvestTypo  |  
    RedeemerUserWithdraw RedeemerUserWithdrawTypo  
    deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON) 

instance Eq RedeemerValidator where
    {-# INLINABLE (==) #-}
    RedeemerMasterFund rmf1  == RedeemerMasterFund rmf2   = rmf1 == rmf2
    RedeemerMasterFundAndMerge rmfam1  == RedeemerMasterFundAndMerge rmfam2   = rmfam1 == rmfam2
    RedeemerMasterSplitFund rmcp1 == RedeemerMasterSplitFund  rmcp2 = rmcp1 == rmcp2
    RedeemerMasterClosePool rmcp1 == RedeemerMasterClosePool  rmcp2 = rmcp1 == rmcp2
    RedeemerMasterTerminatePool rmcp1 == RedeemerMasterTerminatePool  rmcp2 = rmcp1 == rmcp2
    RedeemerMasterEmergency rmcp1 == RedeemerMasterEmergency  rmcp2 = rmcp1 == rmcp2
    RedeemerMasterDeleteFund rmcp1 == RedeemerMasterDeleteFund  rmcp2 = rmcp1 == rmcp2
    RedeemerMasterSendBackFund rmgp1 == RedeemerMasterSendBackFund  rmgp2 = rmgp1 == rmgp2
    RedeemerMasterSendBackDeposit rmgp1 == RedeemerMasterSendBackDeposit  rmgp2 = rmgp1 == rmgp2
    RedeemerMasterAddScripts rmcp1 == RedeemerMasterAddScripts  rmcp2 = rmcp1 == rmcp2
    RedeemerMasterDeleteScripts rmcp1 == RedeemerMasterDeleteScripts  rmcp2 = rmcp1 == rmcp2
    RedeemerUserDeposit rud1 == RedeemerUserDeposit rud2 = rud1 == rud2
    RedeemerUserHarvest ruh1 == RedeemerUserHarvest ruh2 = ruh1 == ruh2
    RedeemerUserWithdraw  ruw1 == RedeemerUserWithdraw ruw2 = ruw1 == ruw2
    _ == _ = False

PlutusTx.makeIsDataIndexed ''RedeemerValidator [ 
        ('RedeemerMasterFund, 1),
        ('RedeemerMasterFundAndMerge, 2),
        ('RedeemerMasterSplitFund, 3),
        ('RedeemerMasterClosePool, 4),
        ('RedeemerMasterTerminatePool, 5),
        ('RedeemerMasterEmergency, 24),
        ('RedeemerMasterDeleteFund, 6),
        ('RedeemerMasterSendBackFund, 7),
        ('RedeemerMasterSendBackDeposit, 8),
        ('RedeemerMasterAddScripts, 9),
        ('RedeemerMasterDeleteScripts, 10),
        ('RedeemerUserDeposit,  21) ,
        ('RedeemerUserHarvest,  22),
        ('RedeemerUserWithdraw, 23)
    ]

------------------------------------------------------------------------------------------

redeemerValidatorToBuiltinData :: RedeemerValidator -> LedgerApiV2.Redeemer
redeemerValidatorToBuiltinData redeemer = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData redeemer

------------------------------------------------------------------------------------------

mkRedeemerMasterFund :: Master -> Integer -> Integer -> RedeemerValidator
mkRedeemerMasterFund master fundAmount minAda = RedeemerMasterFund $ mkRedeemerMasterFundTypo master fundAmount minAda

mkRedeemerMasterFundTypo :: Master -> Integer -> Integer -> RedeemerMasterFundTypo
mkRedeemerMasterFundTypo master fundAmount minAda = RedeemerMasterFundTypo {
        rmfMaster = master,
        rmfFundAmount = fundAmount, 
        rmfMinAda = minAda
    }

------------------------------------------------------------------------------------------

mkRedeemerMasterFundAndMerge :: Master -> Integer -> RedeemerValidator
mkRedeemerMasterFundAndMerge master fundAmount = RedeemerMasterFundAndMerge $ mkRedeemerMasterFundAndMergeTypo master fundAmount

mkRedeemerMasterFundAndMergeTypo :: Master -> Integer -> RedeemerMasterFundAndMergeTypo
mkRedeemerMasterFundAndMergeTypo master fundAmount = RedeemerMasterFundAndMergeTypo {
        rmfamMaster = master,
        rmfamFundAmount = fundAmount
    }

------------------------------------------------------------------------------------------

mkRedeemerMasterSplitFund :: Master -> Integer -> Integer -> RedeemerValidator
mkRedeemerMasterSplitFund master splitFundAmount  minAda_For_FundDatum_New = RedeemerMasterSplitFund $ mkRedeemerMasterSplitFundTypo master splitFundAmount minAda_For_FundDatum_New

mkRedeemerMasterSplitFundTypo :: Master -> Integer -> Integer -> RedeemerMasterSplitFundTypo
mkRedeemerMasterSplitFundTypo master splitFundAmount  minAda_For_FundDatum_New = RedeemerMasterSplitFundTypo {
        rmsfMaster = master,
        rmsfSplitFundAmount  = splitFundAmount,
        rmsfMinAda  = minAda_For_FundDatum_New
    }
------------------------------------------------------------------------------------------

mkRedeemerMasterClosePool :: Master -> LedgerApiV2.POSIXTime -> RedeemerValidator
mkRedeemerMasterClosePool master closedAt = RedeemerMasterClosePool $ mkRedeemerMasterClosePoolTypo master closedAt 

mkRedeemerMasterClosePoolTypo :: Master -> LedgerApiV2.POSIXTime -> RedeemerMasterClosePoolTypo
mkRedeemerMasterClosePoolTypo master closedAt = RedeemerMasterClosePoolTypo {
        rmcpMaster = master,
        rmcpClosedAt = closedAt
    }

------------------------------------------------------------------------------------------

mkRedeemerMasterTerminatePool :: Master -> RedeemerValidator
mkRedeemerMasterTerminatePool master = RedeemerMasterTerminatePool $ mkRedeemerMasterTerminatePoolTypo master 

mkRedeemerMasterTerminatePoolTypo :: Master -> RedeemerMasterTerminatePoolTypo
mkRedeemerMasterTerminatePoolTypo master = RedeemerMasterTerminatePoolTypo {
        rmtpMaster = master
    }

------------------------------------------------------------------------------------------

mkRedeemerMasterEmergency :: Master -> RedeemerValidator
mkRedeemerMasterEmergency !master = RedeemerMasterEmergency $ mkRedeemerMasterEmergencyTypo master

mkRedeemerMasterEmergencyTypo :: Master -> RedeemerMasterEmergencyTypo
mkRedeemerMasterEmergencyTypo !master = RedeemerMasterEmergencyTypo {
        rmeMaster = master
    }

------------------------------------------------------------------------------------------

mkRedeemerMasterDeleteFund :: Master -> RedeemerValidator
mkRedeemerMasterDeleteFund master = RedeemerMasterDeleteFund $ mkRedeemerMasterDeleteFundTypo master 

mkRedeemerMasterDeleteFundTypo :: Master -> RedeemerMasterDeleteFundTypo
mkRedeemerMasterDeleteFundTypo master = RedeemerMasterDeleteFundTypo {
        rmdfMaster = master
    }

------------------------------------------------------------------------------------------

mkRedeemerMasterSendBackFund :: Master -> Master -> RedeemerValidator
mkRedeemerMasterSendBackFund master master_To_SendBack = RedeemerMasterSendBackFund $ mkRedeemerMasterSendBackFundTypo master master_To_SendBack 

mkRedeemerMasterSendBackFundTypo :: Master -> Master -> RedeemerMasterSendBackFundTypo
mkRedeemerMasterSendBackFundTypo master master_To_SendBack = RedeemerMasterSendBackFundTypo {
        rmsbfMaster = master,
        rmsbfMasterToSendBack = master_To_SendBack
    }

------------------------------------------------------------------------------------------

mkRedeemerMasterSendBackDeposit :: Master -> User ->RedeemerValidator
mkRedeemerMasterSendBackDeposit master user = RedeemerMasterSendBackDeposit $ mkRedeemerMasterSendBackDepositTypo master user

mkRedeemerMasterSendBackDepositTypo :: Master -> User ->RedeemerMasterSendBackDepositTypo
mkRedeemerMasterSendBackDepositTypo master user = RedeemerMasterSendBackDepositTypo {
        rmsbdMaster = master,
        rmsbdUserToSendBack = user
    }

------------------------------------------------------------------------------------------

mkRedeemerMasterAddScripts :: Master -> RedeemerValidator
mkRedeemerMasterAddScripts master = RedeemerMasterAddScripts $ mkRedeemerMasterAddScriptsTypo master 

mkRedeemerMasterAddScriptsTypo :: Master -> RedeemerMasterAddScriptsTypo
mkRedeemerMasterAddScriptsTypo master = RedeemerMasterAddScriptsTypo {
        rmasMaster = master
    }

------------------------------------------------------------------------------------------

mkRedeemerMasterDeleteScripts :: Master -> RedeemerValidator
mkRedeemerMasterDeleteScripts master = RedeemerMasterDeleteScripts $ mkRedeemerMasterDeleteScriptsTypo master

mkRedeemerMasterDeleteScriptsTypo :: Master -> RedeemerMasterDeleteScriptsTypo
mkRedeemerMasterDeleteScriptsTypo master  = RedeemerMasterDeleteScriptsTypo {
        rmdsMaster = master
    }

------------------------------------------------------------------------------------------

mkRedeemerUserDeposit :: User -> Integer -> LedgerApiV2.POSIXTime -> Integer ->  RedeemerValidator
mkRedeemerUserDeposit user invest createdAt minAda = RedeemerUserDeposit $ mkRedeemerUserDepositTypo user invest createdAt minAda

mkRedeemerUserDepositTypo :: User -> Integer -> LedgerApiV2.POSIXTime -> Integer -> RedeemerUserDepositTypo
mkRedeemerUserDepositTypo user invest createdAt minAda = RedeemerUserDepositTypo { 
        rudUser = user,
        rudInvestAmount = invest,
        rudCreatedAt = createdAt,
        rudMinAda = minAda
    }  

------------------------------------------------------------------------------------------


mkRedeemerUserHarvest :: User -> Integer -> LedgerApiV2.POSIXTime -> RedeemerValidator
mkRedeemerUserHarvest user claimAmount claimAt = RedeemerUserHarvest $ mkRedeemerUserHarvestTypo user claimAmount claimAt

mkRedeemerUserHarvestTypo :: User -> Integer -> LedgerApiV2.POSIXTime -> RedeemerUserHarvestTypo
mkRedeemerUserHarvestTypo user claimAmount claimAt = RedeemerUserHarvestTypo {
        ruhUser = user,
        ruhClaimAmount = claimAmount,
        ruhClaimAt = claimAt
 }
    
------------------------------------------------------------------------------------------

mkRedeemerUserWithdraw :: User -> RedeemerValidator
mkRedeemerUserWithdraw user = RedeemerUserWithdraw $ mkRedeemerUserWithdrawTypo user

mkRedeemerUserWithdrawTypo :: User -> RedeemerUserWithdrawTypo
mkRedeemerUserWithdrawTypo user = RedeemerUserWithdrawTypo {
        ruwUser = user
    }

------------------------------------------------------------------------------------------
