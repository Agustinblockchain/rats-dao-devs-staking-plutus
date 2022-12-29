module Main where

import           System.Environment       (getArgs)

import qualified Utils                    (unsafeReadAddress, unsafePaymentPubKeyHash)

--Modulo: 

main :: IO ()
main = do
    [addr'] <- getArgs
    print $ Utils.unsafePaymentPubKeyHash $ Utils.unsafeReadAddress addr'
