module Main where

import Control.Exception (throwIO)
import Validators.StakePlusV2.PAB.PABSimulator as PABSimulator
import System.Environment (getArgs)

--Modulo:

main :: IO ()
main = do
  PABSimulator.testWithPABSimulator
