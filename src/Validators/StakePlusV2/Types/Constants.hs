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
-- {-# LANGUAGE Strict #-}
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

-- NFT que identifica al PoolDatum. Es uno solo. 
-- La poliza es la Validators.StakePlusV2.OnChain.Tokens.PoolID.Policy
poolID_TN :: T.TN
poolID_TN = LedgerApiV2.TokenName "P"

-- Tokens que identifican a los FundDatums. 
-- La poliza es la Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.Fund (policy_TxID_Master_Fund)
-- la misma poliza que se usa para validar la transaccion de Master - New Fund.
-- no hace falta tener dos polizas separadas. Una para FundID y otra para la transaccion de New Fund, por que siempre se usarian al mismo tiempo.
fundID_TN :: T.TN
fundID_TN = LedgerApiV2.TokenName "F"

-- Tokens que identifican a los UserDatums. 
-- La poliza es la Validators.StakePlusV2.OnChain.Tokens.TxID.UserActions.Deposit (policy_TxID_User_Deposit)
-- la misma poliza que se usa para validar la transaccion de User - Deposit.
-- no hace falta tener dos polizas separadas. Una para UserID y otra para la transaccion de Deposit, por que siempre se usarian al mismo tiempo.
userID_TN :: T.TN
userID_TN = LedgerApiV2.TokenName "U"

-- Tokens que se entregan a los Usuarios con cada Depósito. 
-- La poliza es la Validators.StakePlusV2.OnChain.Tokens.TxID.UserActions.Deposit (policy_TxID_User_Deposit)
-- la misma poliza que se usa para validar la transaccion de User - Deposit.
userDeposit_TN :: T.TN
userDeposit_TN = LedgerApiV2.TokenName "UD"

-- Tokens que identifican a los ScriptDatums. 
-- La poliza es la Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.AddScripts (policy_TxID_Master_AddScripts)
-- la misma poliza que se usa para validar la transaccion de Master - Add Scripts.
-- no hace falta tener dos polizas separadas. Una para ScriptID y otra para la transaccion de Add Scripts, por que siempre se usarian al mismo tiempo.
scriptID_TN :: T.TN
scriptID_TN = LedgerApiV2.TokenName "S"

------------------------------------

-- Tokens que identifican a cada una de las transacciones.

-- txID_Master_Fund_TN :: T.TN
-- txID_Master_Fund_TN = fund_ID_TN
-- NO se necesita Token de validacion de transaccion para Master_Fund, por que se usa para eso el NFT de FundID, que se mintea en cada nuevo Fund

txID_Master_FundAndMerge_TN :: T.TN
txID_Master_FundAndMerge_TN = LedgerApiV2.TokenName "MFAM"

txID_Master_SplitFund_TN :: T.TN
txID_Master_SplitFund_TN = LedgerApiV2.TokenName "MSF"

txID_Master_ClosePool_TN :: T.TN
txID_Master_ClosePool_TN = LedgerApiV2.TokenName "MCP"

txID_Master_TerminatePool_TN :: T.TN
txID_Master_TerminatePool_TN = LedgerApiV2.TokenName "MTP"

txID_Master_Emergency_TN :: T.TN
txID_Master_Emergency_TN = LedgerApiV2.TokenName "ME"

txID_Master_DeleteFund_TN :: T.TN
txID_Master_DeleteFund_TN = LedgerApiV2.TokenName "MDF"

txID_Master_SendBackFund_TN :: T.TN
txID_Master_SendBackFund_TN = LedgerApiV2.TokenName "MSBF"

txID_Master_SendBackDeposit_TN :: T.TN
txID_Master_SendBackDeposit_TN = LedgerApiV2.TokenName "MSBD"

-- txID_Master_AddScripts_TN :: T.TN
-- txID_Master_AddScripts_TN = LedgerApiV2.TokenName "MAS"
-- NO se necesita Token de validacion de transaccion para txID_Master_AddScripts_TN, por que se usa para eso el NFT de ScriptID, que se mintea en cada nuevo Script

txID_Master_DeleteScripts_TN :: T.TN
txID_Master_DeleteScripts_TN = LedgerApiV2.TokenName "MDS"

-- txID_User_Deposit_TN :: T.TN
-- txID_User_Deposit_TN = userID_TN
-- NO se necesita Token de validacion de transaccion para User_Deposit, por que se usa para eso el NFT de UserID, que se mintea en cada nuevo User

txID_User_Harvest_TN :: T.TN
txID_User_Harvest_TN = LedgerApiV2.TokenName "UH"

txID_User_Withdraw_TN :: T.TN
txID_User_Withdraw_TN = LedgerApiV2.TokenName "UW"

------------------------------------

-- Tokens que identifican a cada uno de los scripts. 
-- la poliza es la de txID_Master_AddScripts_CS

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

scriptID_Master_Emergency_TN :: T.TN
scriptID_Master_Emergency_TN = LedgerApiV2.TokenName "SME"

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

scriptID_User_Harvest_TN :: T.TN
scriptID_User_Harvest_TN = LedgerApiV2.TokenName "SUH"

scriptID_User_Withdraw_TN :: T.TN
scriptID_User_Withdraw_TN = LedgerApiV2.TokenName "SUW"

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

poolDatum_NotEmergency :: Integer
poolDatum_NotEmergency = 0

poolDatum_Emergency :: Integer
poolDatum_Emergency = 1

poolDatum_NotClaimedFund :: Integer
poolDatum_NotClaimedFund = 0

poolDatum_ClaimedFund :: Integer
poolDatum_ClaimedFund = 1


------------------------------------------------------------------------------------------

const_1_PD :: Integer 
const_1_PD = 1

const_1_FD :: Integer 
const_1_FD = 1

const_1_UD :: Integer 
const_1_UD = 1

const_1_SD :: Integer 
const_1_SD = 1