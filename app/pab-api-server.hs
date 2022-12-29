{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
--{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Main where

import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Run                      (runWith)

import qualified Validators.StakePlusV2.PAB.PAB          (ValidatorContracts )

--Modulo: 

main :: IO ()
main = 
    runWith (Builtin.handleBuiltin @Validators.StakePlusV2.PAB.PAB.ValidatorContracts)


