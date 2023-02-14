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
{-# LANGUAGE BangPatterns #-}
---- {-# LANGUAGE Strict #-}
{- HLINT ignore "Use camelCase" -}
-----------------------------------------------------------------------------------------
module Validators.StakePlusV2.Types.RedeemersMint where
-----------------------------------------------------------------------------------------
-- Import Externos
-----------------------------------------------------------------------------------------
import qualified Data.Aeson                             as DataAeson (ToJSON, FromJSON)
import qualified GHC.Generics                           as GHCGenerics (Generic)
import qualified Plutus.V2.Ledger.Api                   as LedgerApiV2
import qualified PlutusTx   
import           PlutusTx.Prelude                       ( Bool(False), Eq(..), ($) )
import qualified Prelude                                as P
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import Validators.StakePlusV2.Types.RedeemersValidator  ( RedeemerValidator )   
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------

newtype RedeemerMint_TxIDTypo  = RedeemerMint_TxIDTypo { 
        mrRedeemerValidator :: RedeemerValidator
    } deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON) 

instance Eq RedeemerMint_TxIDTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  r1   ==   r2 

PlutusTx.makeIsDataIndexed ''RedeemerMint_TxIDTypo [ 
        ('RedeemerMint_TxIDTypo, 0)
    ]

--------------------------------------------------------------------------------------------

data RedeemerBurn_TxIDTypo  = RedeemerBurn_TxIDTypo { 
    } deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON) 

instance Eq RedeemerBurn_TxIDTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1   ==  r2 

PlutusTx.makeIsDataIndexed ''RedeemerBurn_TxIDTypo [ 
        ('RedeemerBurn_TxIDTypo, 0)
    ]

--------------------------------------------------------------------------------------------

data Redeemer_TxID = 
    RedeemerMint_TxID RedeemerMint_TxIDTypo | 
    RedeemerBurn_TxID RedeemerBurn_TxIDTypo   
    deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON) 

instance Eq Redeemer_TxID where
    {-# INLINABLE (==) #-}
    RedeemerMint_TxID rmtx1  == RedeemerMint_TxID rmtx2   = rmtx1 == rmtx2
    RedeemerBurn_TxID rmtx1  == RedeemerBurn_TxID rmtx2   = rmtx1 == rmtx2
    _ == _ = False

PlutusTx.makeIsDataIndexed ''Redeemer_TxID [ 
        ('RedeemerMint_TxID, 0),
        ('RedeemerBurn_TxID, 1) 
    ]

--------------------------------------------------------------------------------------------

mkRedeemerMint_TxID :: RedeemerValidator -> LedgerApiV2.Redeemer 
mkRedeemerMint_TxID redeemer = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData (RedeemerMint_TxID $ RedeemerMint_TxIDTypo  redeemer ) 

mkRedeemerMint_TxIDTypo :: RedeemerValidator -> RedeemerMint_TxIDTypo 
mkRedeemerMint_TxIDTypo  redeemer = RedeemerMint_TxIDTypo {
        mrRedeemerValidator = redeemer
    }

--------------------------------------------------------------------------------------------

mkRedeemerBurn_TxID :: LedgerApiV2.Redeemer
mkRedeemerBurn_TxID = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData (RedeemerBurn_TxID RedeemerBurn_TxIDTypo )

mkRedeemerBurn_TxIDTypo :: RedeemerBurn_TxIDTypo
mkRedeemerBurn_TxIDTypo = RedeemerBurn_TxIDTypo {
    }

--------------------------------------------------------------------------------------------
