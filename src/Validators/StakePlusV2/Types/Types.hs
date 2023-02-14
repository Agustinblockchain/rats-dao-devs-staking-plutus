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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}
{- HLINT ignore "Use camelCase" -}
------------------------------------------------------------------------------------------
module Validators.StakePlusV2.Types.Types where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified PlutusTx                            
import           PlutusTx.Prelude                    ( Integer, Maybe, Eq(..), (&&) )
import qualified Prelude                             as P
import qualified Schema                              (ToSchema)
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------

type Master = LedgerApiV2.PubKeyHash
type User = LedgerApiV2.PubKeyHash

------------------------------------------------------------------------------------------

type CS = LedgerApiV2.CurrencySymbol 
type TN = LedgerApiV2.TokenName

type FundID_TN = TN
type UserID_TN = TN

------------------------------------------------------------------------------------------

newtype Estado = Estado { getEstado :: P.String } deriving  (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON) 

------------------------------------------------------------------------------------------

data InterestRate = InterestRate {
        iMinDays :: Maybe Integer,
        iPercentage :: Integer
    }
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema)

instance Eq InterestRate where
    {-# INLINABLE (==) #-}
    i1 == i2 = iMinDays i1 == iMinDays i2 && iPercentage i1 == iPercentage i2 

PlutusTx.makeLift ''InterestRate
PlutusTx.makeIsDataIndexed ''InterestRate [ 
        ('InterestRate, 0)
    ]

------------------------------------------------------------------------------------------

data PoolParams = PoolParams
    { 
        ppPoolID_CS  :: CS, 
        -- el NFT que identifica a este Pool.
        ppMasters :: [Master], 
        -- Masters del Pool: una lista de Payment Pub Key Hashes que determinan que wallets podrán tener funciones de privilegio sobre el Pool.
        ppBeginAt    :: LedgerApiV2.POSIXTime, 
        -- fecha en que el Pool se inicia.
        ppDeadline    :: LedgerApiV2.POSIXTime, 
        -- grace time: cuantos milisegundos despues del deadline es posible cobrar rewards
        ppGraceTime :: LedgerApiV2.POSIXTime, 
        -- fecha en que el Pool se termina.
        ppStaking_CS    :: CS, 
        ppStaking_TN    :: TN, 
        -- el Asset Class que identifica a la unidad de staking de este Pool.
        ppHarvest_CS    :: CS, 
        ppHarvest_TN    :: TN, 
        -- el Asset Class que identifica a la unidad de harvest de este Pool.
        ppInterestRates    :: [InterestRate] 
        -- para el calculo de rewards.
    }
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema)

instance Eq PoolParams where
    {-# INLINABLE (==) #-}
    pp1 == pp2 =    ppPoolID_CS pp1 == ppPoolID_CS pp2 && 
                    ppMasters pp1 == ppMasters pp2 && 
                    ppDeadline pp1 == ppDeadline pp2 && 
                    ppHarvest_CS pp1 == ppHarvest_CS pp2 && 
                    ppHarvest_TN pp1 == ppHarvest_TN pp2 && 
                    ppStaking_CS pp1 == ppStaking_CS pp2 && 
                    ppHarvest_TN pp1 == ppHarvest_TN pp2 && 
                    ppInterestRates pp1 == ppInterestRates pp2 

PlutusTx.makeLift ''PoolParams
PlutusTx.makeIsDataIndexed ''PoolParams [ 
        ('PoolParams, 0)
    ]

------------------------------------------------------------------------------------------
