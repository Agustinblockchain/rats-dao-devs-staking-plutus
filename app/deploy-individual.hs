module Main where

import Control.Exception (throwIO)
import Data.List
import qualified Deploy
import System.Environment (getArgs)

--Modulo:

main :: IO ()
main = do
  [ script, nombrePool, mastersStr, uTxOutRefStr, begintAtPoolStr, deadlinePoolStr, graceTimeStr, staking_UI, staking_CS_Str, staking_TN_Str, harvest_UI, harvest_CS_Str, harvest_TN_Str, interestStr, pathOutputFiles ] <- getArgs
  Deploy.exportarPoolParamsYScriptsIndividual script nombrePool mastersStr uTxOutRefStr begintAtPoolStr deadlinePoolStr graceTimeStr staking_UI staking_CS_Str staking_TN_Str harvest_UI harvest_CS_Str harvest_TN_Str interestStr pathOutputFiles
