module Main where

import           Data.String              (IsString (..))
import           System.Environment       (getArgs)

import qualified Utils                    (unsafeTokenNameToHex)

--Modulo: 

main :: IO ()
main = do
    [tn'] <- getArgs
    let tn = fromString tn'
    putStrLn $ Utils.unsafeTokenNameToHex tn
