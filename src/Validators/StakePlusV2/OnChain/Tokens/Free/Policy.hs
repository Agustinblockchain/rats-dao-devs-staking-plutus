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
-- {-# LANGUAGE Strict #-}
{- HLINT ignore "Use camelCase" -}
------------------------------------------------------------------------------------------
module Validators.StakePlusV2.OnChain.Tokens.Free.Policy
(
    policy_Free 
)
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2
import qualified PlutusTx   
import qualified PlutusTx.Builtins.Internal                                 as TxBuiltinsInternal (BuiltinInteger )
import           PlutusTx.Prelude                                           ( BuiltinData, ($), Integer, (>), error )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------

{-# INLINABLE mkPolicyFree #-}
mkPolicyFree :: TxBuiltinsInternal.BuiltinInteger -> BuiltinData -> BuiltinData -> ()
mkPolicyFree numero _ _ = 
  if numero > 0 then () else error ()  

--------------------------------------------------------------------------------

{-# INLINEABLE policy_Free #-}
policy_Free :: Integer -> LedgerApiV2.MintingPolicy
policy_Free numero  = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy numero

{-# INLINEABLE original_policy #-}
original_policy :: Integer -> Plutonomy.MintingPolicy
original_policy numero  =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkPolicyFree ||])
    `PlutusTx.applyCode` PlutusTx.liftCode numero

--------------------------------------------------------------------------------
