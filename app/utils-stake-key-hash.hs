module Main where

import System.Environment (getArgs)
import qualified Utils (unsafeReadAddress, unsafeStakePubKeyHash)

--Modulo:

main :: IO ()
main = do
  [addr'] <- getArgs
  print $ Utils.unsafeStakePubKeyHash $ Utils.unsafeReadAddress addr'
