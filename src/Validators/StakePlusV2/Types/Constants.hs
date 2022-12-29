--{-# LANGUAGE DataKinds                  #-}
-- {-# LANGUAGE DeriveAnyClass             #-}
-- {-# LANGUAGE DeriveGeneric              #-}
--{-# LANGUAGE DerivingStrategies         #-}
--{-# LANGUAGE FlexibleContexts           #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
--{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- {-# LANGUAGE TemplateHaskell            #-}
--{-# LANGUAGE TypeApplications           #-}
--{-# LANGUAGE TypeFamilies               #-}
--{-# LANGUAGE TypeOperators              #-}
--{-# LANGUAGE RankNTypes                 #-}
--{-# LANGUAGE TupleSections              #-}
--{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE BangPatterns #-}
{- HLINT ignore "Use camelCase" -}
------------------------------------------------------------------------------------------
module Validators.StakePlusV2.Types.Constants where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Plutus.V2.Ledger.Api                   as LedgerApiV2
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Types.Types    as T ( TN ) 
import           PlutusTx.Prelude                      (Integer)
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------

-- FOR CONFIGURATION:

validTimeRange :: LedgerApiV2.POSIXTime
validTimeRange = 900_000 -- 15 * 60 * 1000 = 15 minutos

maxRewards :: Integer
maxRewards = 1_000_000_000_000_000

------------------------------------

poolID_TN :: T.TN
poolID_TN = LedgerApiV2.TokenName "P"

fundID_TN :: T.TN
fundID_TN = LedgerApiV2.TokenName "F"

userID_TN :: T.TN
userID_TN = LedgerApiV2.TokenName "U"

------------------------------------

scriptID_Validator_TN :: T.TN
scriptID_Validator_TN = LedgerApiV2.TokenName "SV"

scriptID_Master_Fund_TN :: T.TN
scriptID_Master_Fund_TN = LedgerApiV2.TokenName "SMF"

scriptID_Master_FundAndMerge_TN :: T.TN
scriptID_Master_FundAndMerge_TN = LedgerApiV2.TokenName "SMFAM"

scriptID_Master_SplitFund_TN :: T.TN
scriptID_Master_SplitFund_TN = LedgerApiV2.TokenName "SMSF"

scriptID_Master_ClosePool_TN :: T.TN
scriptID_Master_ClosePool_TN = LedgerApiV2.TokenName "SMCP"

scriptID_Master_TerminatePool_TN :: T.TN
scriptID_Master_TerminatePool_TN = LedgerApiV2.TokenName "SMTP"

scriptID_Master_DeleteFund_TN :: T.TN
scriptID_Master_DeleteFund_TN = LedgerApiV2.TokenName "SMDF"

scriptID_Master_SendBackFund_TN :: T.TN
scriptID_Master_SendBackFund_TN = LedgerApiV2.TokenName "SMSBF"

scriptID_Master_SendBackDeposit_TN :: T.TN
scriptID_Master_SendBackDeposit_TN = LedgerApiV2.TokenName "SMSBD"

scriptID_Master_AddScripts_TN :: T.TN
scriptID_Master_AddScripts_TN = LedgerApiV2.TokenName "SMAS"

scriptID_Master_DeleteScripts_TN :: T.TN
scriptID_Master_DeleteScripts_TN = LedgerApiV2.TokenName "SMDS"

scriptID_User_Deposit_TN :: T.TN
scriptID_User_Deposit_TN = LedgerApiV2.TokenName "SUD"

scriptID_User_Deposit_For_User_TN :: T.TN
scriptID_User_Deposit_For_User_TN = LedgerApiV2.TokenName "SUD"

scriptID_User_Harvest_TN :: T.TN
scriptID_User_Harvest_TN = LedgerApiV2.TokenName "SUH"

scriptID_User_Withdraw_TN :: T.TN
scriptID_User_Withdraw_TN = LedgerApiV2.TokenName "SUW"

------------------------------------

-- txID_Master_Fund_TN :: T.TN
-- txID_Master_Fund_TN = fund_ID_TN

txID_Master_FundAndMerge_TN :: T.TN
txID_Master_FundAndMerge_TN = LedgerApiV2.TokenName "MFAM"

txID_Master_SplitFund_TN :: T.TN
txID_Master_SplitFund_TN = LedgerApiV2.TokenName "MSF"

txID_Master_ClosePool_TN :: T.TN
txID_Master_ClosePool_TN = LedgerApiV2.TokenName "MCP"

txID_Master_TerminatePool_TN :: T.TN
txID_Master_TerminatePool_TN = LedgerApiV2.TokenName "MTP"

txID_Master_DeleteFund_TN :: T.TN
txID_Master_DeleteFund_TN = LedgerApiV2.TokenName "MDF"

txID_Master_SendBackFund_TN :: T.TN
txID_Master_SendBackFund_TN = LedgerApiV2.TokenName "MSBF"

txID_Master_SendBackDeposit_TN :: T.TN
txID_Master_SendBackDeposit_TN = LedgerApiV2.TokenName "MSBD"

txID_Master_AddScripts_TN :: T.TN
txID_Master_AddScripts_TN = LedgerApiV2.TokenName "MAS"

txID_Master_DeleteScripts_TN :: T.TN
txID_Master_DeleteScripts_TN = LedgerApiV2.TokenName "MDS"

-- txID_User_Deposit_TN :: T.TN
-- txID_User_Deposit_TN = userID_TN

txID_User_Deposit_For_User_TN :: T.TN
txID_User_Deposit_For_User_TN = LedgerApiV2.TokenName "UD"

txID_User_Harvest_TN :: T.TN
txID_User_Harvest_TN = LedgerApiV2.TokenName "UH"

txID_User_Withdraw_TN :: T.TN
txID_User_Withdraw_TN = LedgerApiV2.TokenName "UW"

------------------------------------

maxDiffTokensForPoolAndFundDatum :: Integer
maxDiffTokensForPoolAndFundDatum = 11

maxDiffTokensForUserDatum :: Integer
maxDiffTokensForUserDatum = 3

tokenNameLenght :: Integer
tokenNameLenght = 4

poolDatum_NotTerminated :: Integer
poolDatum_NotTerminated = 0

poolDatum_Terminated :: Integer
poolDatum_Terminated = 1

poolDatum_NotClaimedFund :: Integer
poolDatum_NotClaimedFund = 0

poolDatum_ClaimedFund :: Integer
poolDatum_ClaimedFund = 1


------------------------------------------------------------------------------------------