-- {-# LANGUAGE DataKinds                  #-}
-- {-# LANGUAGE DeriveAnyClass             #-}
-- {-# LANGUAGE DeriveGeneric              #-}
-- {-# LANGUAGE DerivingStrategies         #-}
-- {-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase                 #-}
-- {-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
-- {-# LANGUAGE RecordWildCards            #-}
-- {-# LANGUAGE ScopedTypeVariables        #-}
-- {-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
-- {-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE TupleSections              #-}
-- {-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE QuasiQuotes                #-}
{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}
-----------------------------------------------------------------------------------------
module Validators.StakePlusV2.PAB.PABSimulator where
-----------------------------------------------------------------------------------------
-- Import Externos
-----------------------------------------------------------------------------------------
import qualified Control.Concurrent.STM                                                     as ConcurrentSTM (atomically)
import qualified Control.Monad.IO.Class                                                     as MonadIOClass (MonadIO (..))
import qualified Control.Monad                                                              as Monad (void)
-- import qualified Control.Monad.Freer                                                     as MonadFreer (interpret)
import qualified Control.Monad.Freer.Internal                                               as MonadFreerInternal (Eff)
import qualified Data.Aeson                                                                 as DataAeson (decode)
import qualified Data.ByteString                                                            as DataByteString
import qualified Data.Default                                                               as DataDefault (def)
-- import qualified Data.Fixed                                                              as DataFixed (Pico, Fixed ( MkFixed ))
import qualified Data.List                                                                  as DataList
-- import qualified Data.Map                                                                as DataMap
import qualified Data.Maybe                                                                 as DataMaybe (fromJust) --,fromMaybe,
-- import qualified Data.String                                                                as DataString (IsString (fromString))
-- import qualified Data.Text                                                               as DataText (pack, Text)
-- import qualified Data.Text.Internal.Search                                                  as DataTextSearch
-- import qualified Data.Time.Clock                                                         as DataTimeClock (secondsToNominalDiffTime)
-- import qualified Data.Time.Clock.POSIX                                                   as DataTimeClockPOSIX (posixSecondsToUTCTime)
-- import qualified Data.Time.Format                                                        as DataTimeFormat (defaultTimeLocale, formatTime)
import qualified Ledger
-- import qualified Ledger.Ada                                                                 as LedgerAda
-- import qualified Ledger.Address                                                          as LedgerAddress (Address)
import qualified Ledger.Blockchain                                                          as LedgerBlockchain (value)
-- import qualified Ledger.CardanoWallet                                                       as LedgerCardanoWallet
import qualified Ledger.TimeSlot                                                            as LedgerTimeSlot
import qualified Ledger.Value                                                               as LedgerValue
-- import qualified Playground.Contract                                                     as PlaygroundContract (IO)
import qualified Prelude                                                                    as P
import qualified Plutus.PAB.Core                                                            as PABCore (PABEffects)
import qualified Plutus.PAB.Effects.Contract.Builtin                                        as PABEffectsContractBuiltin (Builtin)
import qualified Plutus.PAB.Simulator                                                       as PABSimulator
import qualified Plutus.PAB.Webserver.Server                                                as PABServer
-- import qualified Plutus.V1.Ledger.Api                                                    as LedgerApiV1
-- import qualified Plutus.V1.Ledger.Bytes                                                  as LedgerBytesV1
-- import qualified Plutus.V2.Ledger.Address                                                as LedgerAddressV2
import qualified Plutus.V2.Ledger.Api                                                       as LedgerApiV2
-- import qualified Plutus.V2.Ledger.Value                                                  as LedgerValueV2
-- import qualified Plutus.V2.Ledger.Tx                                                     as LedgerTxV2 (txOutDatum)
import qualified PlutusTx.Builtins.Class                                                    as TxBuiltinsClass
-- import qualified PlutusTx.Builtins.Internal                                              as TxBuiltinsInternal hiding (head, consByteString)
-- import qualified PlutusTx.Eq                                                             as PlutusTxEq
import           PlutusTx.Prelude                                                           hiding (unless)
import qualified System.Directory                                                           as SystemDirectory
import qualified System.Environment                                                         as SystemEnvironment (lookupEnv)
import qualified System.FilePath.Posix                                                      as SystemFilePathPosix
import qualified Text.Hex                                                                   as TextHex
import qualified Text.Read                                                                  as TextRead (readMaybe)
-- import qualified Text.RE.Replace                                                            as TextREReplace
-- import qualified Text.RE.TDFA.String                                                        as TextRETDFAString
-- import qualified Wallet.Emulator.Wallet                                                     as WalletEmulator
--------------------------------------------------------------------------------
-- Import Internos
--------------------------------------------------------------------------------
import qualified Deploy
import qualified Validators.StakePlusV2.Helpers                                             as Helpers
import qualified Validators.StakePlusV2.OnChain.Core.Validator                              as OnChain
import qualified Validators.StakePlusV2.OnChain.Tokens.PoolID.Policy                        as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.Fund              as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.FundAndMerge      as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.SplitFund         as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.ClosePool         as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.TerminatePool     as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.Emergency         as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.DeleteFund        as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.SendBackFund      as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.SendBackDeposit   as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.AddScripts        as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.DeleteScripts     as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.UserActions.Deposit             as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.UserActions.Harvest             as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.UserActions.Withdraw            as OnChainNFT
import qualified Validators.StakePlusV2.PAB.PAB                                             as PAB
import qualified Validators.StakePlusV2.PAB.PABSimulatorHelpers                             as PABSimulatorHelpers
import qualified Validators.StakePlusV2.Types.Constants                                     as T 
import qualified Validators.StakePlusV2.Types.PABParams                                     as T (PABBalanceAtScriptFullParams (..), PABBalanceAtScriptParams (..), PABSplitUtxOParams (..), PABUserDepositParams (..), PABPoolParams (..), PABUserHarvestParams (..), PABUserWithdrawParams (..), PABMasterMintFreeParams (..), PABMasterPreparePoolParams (..), PABMasterFundParams (..), PABMasterFundAndMergeParams (..), PABMasterSplitFundParams (..), PABMasterClosePoolParams (..), PABMasterTerminatePoolParams (..), PABMasterEmergencyParams (..), PABMasterDeleteFundParams (..), PABMasterSendBackFundParams (..), PABMasterSendBackDepositParams (..), PABMasterAddScriptsParams (..) , PABMasterDeleteScriptsParams (..) )
import qualified Validators.StakePlusV2.Types.Types                                         as T (PoolParams (..),InterestRate (..))
import qualified Utils
--------------------------------------------------------------------------------
-- Modulo
--------------------------------------------------------------------------------

testWithPABSimulator :: P.IO ()
testWithPABSimulator  = Monad.void $ PABSimulator.runSimulationWith PABSimulatorHelpers.handlers traceWithPABSimulator

traceWithPABSimulator  :: MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
traceWithPABSimulator = do
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PABServer.startServerDebug

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "********* PAB Server is running *********"

    mainLoop (Nothing, 2) Nothing shutdown

--------------------------------------------------------------------------------

mainLoop :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
mainLoop (walletNro', walletCount) pabPoolParams' shutdown = do

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "-----------"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "STAKING POOL"

    case walletNro' of
        Nothing ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "1 - Choose Wallet"
        Just walletNro ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "1 - Choose Wallet (" ++ P.show walletNro ++ ")"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "21 - New Staking Pool"

    case pabPoolParams' of
        Nothing ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "22 - Choose Staking Pool"
        Just pabPoolParams ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "22 - Choose Staking Pool (" ++ P.show (T.ppPoolID_CS $ T.pppPoolParams pabPoolParams ) ++ ")"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "-----------"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Master Actions"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "31 - Prepare Pool"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "32 - New Fund"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "33 - Fund And Merge"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "34 - Merge Fund"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "35 - Split Fund"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "36 - Close Pool"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "37 - Terminate Pool"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "38 - Delete Fund"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "39 - Emergency"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "----"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "51 - Add Scripts"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "52 - Delete Scripts"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "----"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "391 - Get Back Fund"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "392 - Send Back Fund"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "393 - Send Back Invest"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "-----------"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "User Actions"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "41 - Deposit"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "42 - Harvest"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "43 - Withdraw"



    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "-----------"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Others Actions"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "6 - Mint Tokens"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "----"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "7 - Split UtxO at Wallet"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "----"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "81 - All Balances"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "82 - UtxOs at Wallet"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "83 - UtxOs at Script"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "84 - UtxOs at Script Full"


    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "----"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "90 - Evaluate Validator"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "91 - Evaluate Harvest"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "92 - Evaluate Withdraw"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "93 - Evaluate Emergency"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "-----------"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "99 - Exit"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Please enter an option:"
    opcion <- MonadIOClass.liftIO P.getLine

    case TextRead.readMaybe opcion :: Maybe Integer of
        Just 1 ->
            elegirWallet (walletNro', walletCount) pabPoolParams' shutdown
        Just 21 ->
            createPoolParams (walletNro', walletCount) pabPoolParams' shutdown
        Just 22 ->
            elegirPoolParams (walletNro', walletCount) pabPoolParams' shutdown
        Just 31 ->
            masterPreparePool (walletNro', walletCount) pabPoolParams' shutdown
        Just 32 ->
            masterNewFund (walletNro', walletCount) pabPoolParams' shutdown
        Just 33 ->
            masterFundAndMerge (walletNro', walletCount) pabPoolParams' shutdown
        Just 34->
            masterMergeFunds (walletNro', walletCount) pabPoolParams' shutdown
        Just 35 ->
            masterSplitFund (walletNro', walletCount) pabPoolParams' shutdown
        Just 36 ->
            masterClosePool (walletNro', walletCount) pabPoolParams' shutdown
        Just 37 ->
            masterTerminatePool (walletNro', walletCount) pabPoolParams' shutdown
        Just 38 ->
            masterDeleteFunds (walletNro', walletCount) pabPoolParams' shutdown
        Just 39 ->
            masterEmergency (walletNro', walletCount) pabPoolParams' shutdown
        Just 391 ->
            masterGetBackFund (walletNro', walletCount) pabPoolParams' shutdown
        Just 392 ->
            masterSendBackFund (walletNro', walletCount) pabPoolParams' shutdown
        Just 393 ->
            masterSendBackDeposit (walletNro', walletCount) pabPoolParams' shutdown
        Just 41 ->
            userDeposit (walletNro', walletCount) pabPoolParams' shutdown
        Just 42 ->
            userHarvest (walletNro', walletCount) pabPoolParams' shutdown
        Just 43 ->
            userWithdraw (walletNro', walletCount) pabPoolParams' shutdown
        Just 51 ->
            masterAddScripts (walletNro', walletCount) pabPoolParams' shutdown
        Just 52 ->
            masterDeleteScripts (walletNro', walletCount) pabPoolParams' shutdown
        Just 6 ->
            mintTokens (walletNro', walletCount) pabPoolParams' shutdown
        Just 7 ->
            splitUtxO (walletNro', walletCount) pabPoolParams' shutdown
        Just 81 -> do
            PABSimulatorHelpers.balances (walletNro', walletCount) pabPoolParams' shutdown
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine
            mainLoop (walletNro', walletCount) pabPoolParams' shutdown
        Just 82 ->
            uTxOAtWallet (walletNro', walletCount) pabPoolParams' shutdown
        Just 83 ->
            uTxOAtScript (walletNro', walletCount) pabPoolParams' shutdown
        Just 84 ->
            uTxOAtScriptFull (walletNro', walletCount) pabPoolParams' shutdown
        Just 90 ->
            evaluate_Validator (walletNro', walletCount) pabPoolParams' shutdown
        Just 91 ->
            evaluate_User_Harvest (walletNro', walletCount) pabPoolParams' shutdown
        Just 92 ->
            evaluate_User_Withdraw (walletNro', walletCount) pabPoolParams' shutdown
        Just 93 ->
            evaluate_Master_Emergency (walletNro', walletCount) pabPoolParams' shutdown
        Just 99 -> do
            PABSimulatorHelpers.balances (walletNro', walletCount) pabPoolParams' shutdown
            shutdown
        _ -> mainLoop (walletNro', walletCount) pabPoolParams' shutdown

-----------------------------------------------------------------------------------------

evaluate_Validator :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
evaluate_Validator (walletNro', walletCount) pabPoolParams' shutdown = do
    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do
            
            PABSimulatorHelpers.evaluate_Validator pabPoolParams

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

-----------------------------------------------------------------------------------------

evaluate_User_Harvest :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
evaluate_User_Harvest (walletNro', walletCount) pabPoolParams' shutdown = do
    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do
            
            PABSimulatorHelpers.evaluate_Policy_TxID_User_Harvest pabPoolParams

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

-----------------------------------------------------------------------------------------

evaluate_User_Withdraw :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
evaluate_User_Withdraw (walletNro', walletCount) pabPoolParams' shutdown = do
    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do
            
            PABSimulatorHelpers.evaluate_Policy_TxID_User_Withdraw pabPoolParams

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

-----------------------------------------------------------------------------------------

evaluate_Master_Emergency :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
evaluate_Master_Emergency (walletNro', walletCount) pabPoolParams' shutdown = do
    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do
            
            PABSimulatorHelpers.evaluate_Policy_TxID_Master_Emergency pabPoolParams

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

-----------------------------------------------------------------------------------------

elegirWallet :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
elegirWallet (_, walletCount) pabPoolParams' shutdown = do
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Available Wallets:"
    
    -- let 
    --     wallet = WalletEmulator.knownWallet walletNro
    --     wId = WalletEmulator.getWalletId  wallet
    --     list= WalletEmulator.knownWallets

    --     list2 = LedgerCardanoWallet.knownMockWallets 

    -- PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Wallet: " ++ P.show wallet
    -- PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "wId: " ++ P.show wId
    -- PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "list: " ++ P.show list
    -- PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "list2: " ++ P.show list2

    let
        -- walletCount = length WalletEmulator.knownWallets 
        formatWallets = concat [ [
                    "----------------" ,
                    "#: " ++ P.show walletNro,
                    "Pk: " ++ P.show (PABSimulatorHelpers.walletPaymentPubKeyHash walletNro) 
                ] | walletNro <- [1..walletCount]
            ]
    mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) formatWallets

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "----------------"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "0 - Add Wallet (up to 10)"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "----------------"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet:"
    numberSrt <- MonadIOClass.liftIO P.getLine
    case TextRead.readMaybe numberSrt :: Maybe Integer of
        Just x ->
            if x == 0 then do
                -- PABSimulator.addWalletWith (Just $ LedgerAda.lovelaceOf 100)  
                
                elegirWallet (Nothing, walletCount+1) pabPoolParams' shutdown
            else
                if x >= 1 && x <= walletCount then 
                    mainLoop (Just x, walletCount) pabPoolParams' shutdown
                else
                    elegirWallet (Nothing, walletCount) pabPoolParams' shutdown
        _ ->
            elegirWallet (Nothing, walletCount) pabPoolParams' shutdown

--------------------------------------------------------------------------------

uTxOAtWallet :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
uTxOAtWallet (walletNro', walletCount) pabPoolParams' shutdown =
    case walletNro' of
        Just walletNro -> do

            blockchain <- PABSimulator.blockchain

            let

                uTxOutRefAt = fst <$>  PABSimulatorHelpers.getUTxOsListInPABSimulator blockchain (PABSimulatorHelpers.walletPaymentPubKeyHashAddress walletNro)

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "UtxOs at Wallet"

            mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) ["Values at: " ++ P.show uTxO ++ " " ++  P.show (LedgerBlockchain.value blockchain uTxO) | uTxO <- uTxOutRefAt ]

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) pabPoolParams' shutdown

        _ -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (walletNro', walletCount) pabPoolParams' shutdown

--------------------------------------------------------------------------------

uTxOAtScript :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
uTxOAtScript (walletNro', walletCount) pabPoolParams' shutdown =

    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do

            let
                pABBalanceAtScriptParams = PAB.BalanceAtScript T.PABBalanceAtScriptParams{
                        T.pbPABPoolParams = pabPoolParams
                    }

            cBalanceAtScript <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) pABBalanceAtScriptParams

            _ <- PABSimulator.waitUntilFinished cBalanceAtScript

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (walletNro', walletCount) (Just pabPoolParams) shutdown

        _ -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (walletNro', walletCount) pabPoolParams' shutdown

--------------------------------------------------------------------------------

uTxOAtScriptFull :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
uTxOAtScriptFull (walletNro', walletCount) pabPoolParams' shutdown =

    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do

            let
                pABBalanceAtScriptFullParams = PAB.BalanceAtScriptFull T.PABBalanceAtScriptFullParams{
                        T.pbfPABPoolParams = pabPoolParams
                    }

            cBalanceAtScriptFull <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) pABBalanceAtScriptFullParams

            _ <- PABSimulator.waitUntilFinished cBalanceAtScriptFull

            blockchain <- PABSimulator.blockchain

            let
                address = T.pppValidatorAddress pabPoolParams

                uTxOuts = PABSimulatorHelpers.getUTxOsListInPABSimulator blockchain address

                datumFrom _ =
                    "TODO: get datum from uTxO"

                formatValues uTxORef = [P.show val   |  val <- LedgerValue.flattenValue $ Helpers.fromJust $ LedgerBlockchain.value blockchain uTxORef ]

                formatUTxOValues = concat [P.show ( 1 P.+  Helpers.fromJust(DataList.elemIndex (uTxORef, uTxOut) uTxOuts)): (    "At: " ++ P.show uTxORef):("Datum: " ++  datumFrom uTxOut):formatValues uTxORef | (uTxORef, uTxOut) <- uTxOuts ]


            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "UtxOs at Script"

            mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) formatUTxOValues

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (walletNro', walletCount) (Just pabPoolParams) shutdown

        _ -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (walletNro', walletCount) pabPoolParams' shutdown

--------------------------------------------------------------------------------

splitUtxO :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
splitUtxO (walletNro', walletCount) pabPoolParams' shutdown =

    case walletNro' of
        Just walletNro -> do
            let 
                !ada_AC = LedgerValue.AssetClass (LedgerValue.adaSymbol, LedgerValue.adaToken)
                !minAda = Helpers.calculateMinAda 1 0 1 True

            !splitAmount <- PABSimulatorHelpers.getAmount "ADA (lovelace)" ada_AC minAda
            let
                pABSplitUtxOParams = PAB.SplitUtxO T.PABSplitUtxOParams{
                        T.psuSplitAmount = splitAmount
                    }

            cSplitUtxO <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) pABSplitUtxOParams

            _ <- PABSimulator.waitUntilFinished cSplitUtxO

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (walletNro', walletCount) pabPoolParams' shutdown

        _ -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (walletNro', walletCount) pabPoolParams' shutdown

--------------------------------------------------------------------------------

createPoolParams :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
createPoolParams (walletNro', walletCount) pabPoolParams' shutdown =

    case walletNro' of
        Just walletNro -> do

            basePathFilesMaybe <- MonadIOClass.liftIO $ SystemEnvironment.lookupEnv "RATS_PLUTUS_FILES"

            let 
                basePathFiles = case basePathFilesMaybe of
                    Nothing -> "files/validators/V2/StakePlusV2/PABSimulator"
                    Just path -> path SystemFilePathPosix.</> "PABSimulator"
                    
            MonadIOClass.liftIO $ SystemDirectory.createDirectoryIfMissing True basePathFiles --SystemFilePathPosix.</> v1dir

            blockchain <- PABSimulator.blockchain

            mastersNros <- PABSimulatorHelpers.elegirWalletsParaMasters walletCount []
            
            let
                masters = [
                    Ledger.unPaymentPubKeyHash $ PABSimulatorHelpers.walletPaymentPubKeyHash mastersNro
                    | mastersNro <- mastersNros]

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "------"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Enter Pool Name:"
            nombrePool <- PABSimulatorHelpers.getStr

            -- let
            --     onlyLettersAndNumbers :: P.String -> P.String
            --     onlyLettersAndNumbers s = TextREReplace.replaceAll "" $ s TextRETDFAString.*=~ [TextRETDFAString.re|$([^a-zA-Z0-9])|]
            --     nombrePool = onlyLettersAndNumbers nombrePool'

            MonadIOClass.liftIO $ SystemDirectory.removePathForcibly (basePathFiles SystemFilePathPosix.</> nombrePool )
            MonadIOClass.liftIO $ SystemDirectory.createDirectoryIfMissing True (basePathFiles SystemFilePathPosix.</> nombrePool )

            let
                getTime :: LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) LedgerApiV2.POSIXTime
                getTime defTime lowerLimit = do
                    str <- MonadIOClass.liftIO P.getLine
                    if length str == 0
                    then do 
                        return defTime
                    else
                        case TextRead.readMaybe str :: Maybe Integer of
                            Just x -> 
                                if LedgerApiV2.POSIXTime x >= lowerLimit then do 
                                    return $ LedgerApiV2.POSIXTime x 
                                else do 
                                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Invalid input, try again:"
                                    getTime defTime lowerLimit
                            _ -> do
                                PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Invalid input, try again:"
                                getTime defTime lowerLimit

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            -- let now = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            let beginAtPool'  = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Enter Begin Date in Milisecconds (def=" ++ P.show beginAtPool' ++ "):"
            beginAtPool <- getTime beginAtPool' 0 

            slot' <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let deadlinePool'  = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def (slot'+900)
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Enter Deadline in Milisecconds (def=" ++ P.show deadlinePool' ++ " / 15 min):"
            deadlinePool <- getTime deadlinePool' beginAtPool

            let graceTime'  = 1000 P.* 60 P.* 15 -- 15 minutes
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Enter Grace Time in Milisecconds (def=" ++ P.show graceTime' ++ "):"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "No Grace Time = 0"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "15 minutes = 15 * 60 * 1000 = 900000"
            graceTime <- getTime graceTime' 0

            let
                isHexOk hex =
                    case hex of
                        Nothing -> False
                        _       -> True

                getUnitName = do
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Enter Unit Name For User Interface:"
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Leave empty to use ADA (lovelace)"
                    str <- MonadIOClass.liftIO P.getLine
                    if length str == 0
                    then return "ADA (loveLace)"
                    else return str
                
                getCurrencySymbol = do
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Enter Currency Symbol / Policy Id (56 characters of Hex string):"
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Leave empty to use ADA (lovelace)"
                    cS_Str <- MonadIOClass.liftIO P.getLine
                    if length cS_Str P.== 0 || length cS_Str P.== 56 then do
                        let hex = TextHex.decodeHex $ Utils.stringToStrictText cS_Str
                        if isHexOk hex then
                            return $ DataMaybe.fromJust hex
                        else do
                            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Invalid Currency Symbol, try again:"
                            getCurrencySymbol
                    else do
                        PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Invalid Currency Symbol, try again:"
                        getCurrencySymbol
                
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "------"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Staking Unit"
            staking_UI <- getUnitName
            (staking_CS, staking_TN) <- do
                staking_CS_Str <- getCurrencySymbol
                if DataByteString.length staking_CS_Str P.== 0
                then do
                    return (LedgerApiV2.adaSymbol, LedgerApiV2.adaToken)
                else do
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Enter TokenName:"
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Leave empty to use any TokenName within the Currency Symbol choosen" 
                    staking_TN_Str <- MonadIOClass.liftIO P.getLine
                    return (LedgerApiV2.CurrencySymbol $ TxBuiltinsClass.toBuiltin staking_CS_Str, LedgerApiV2.TokenName $ Utils.stringToBuiltinByteString staking_TN_Str)

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "------"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Harvest Unit"
            harvest_UI <- getUnitName
            (harvest_CS, harvest_TN) <- do
                harvest_CS_Str <- getCurrencySymbol
                if DataByteString.length harvest_CS_Str P.== 0
                then do
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Harvest Currency Symbol is empty, using ADA (lovelace)"
                    return (LedgerApiV2.adaSymbol, LedgerApiV2.adaToken)
                else do
                    let
                        getTokenName = do
                            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Enter TokenName (can't be empty, must use a TokenName):"
                            harvest_TN_Str <- MonadIOClass.liftIO P.getLine
                            if length harvest_TN_Str == 0
                            then do
                                getTokenName
                            else do
                                return harvest_TN_Str
                    harvest_TN' <- getTokenName
                    return (LedgerApiV2.CurrencySymbol $ TxBuiltinsClass.toBuiltin harvest_CS_Str, LedgerApiV2.TokenName $ Utils.stringToBuiltinByteString harvest_TN')

            let
                staking_AC = LedgerValue.AssetClass (staking_CS, staking_TN)
                harvest_AC = LedgerValue.AssetClass (harvest_CS, harvest_TN)

            let 
                getInterestRate :: Integer -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) Integer
                getInterestRate defInterest = do
                    str <- MonadIOClass.liftIO P.getLine
                    if length str == 0
                    then return defInterest
                    else
                        case TextRead.readMaybe str :: Maybe Integer of
                            Just x -> return x
                            _ -> do
                                PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Invalid Interest, try again:"
                                getInterestRate defInterest
                
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "------"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Enter Anual Interest (def=525600):"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "1 Harvest Unit per each Staking Unit per Annum = 1"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "1 Harvest Unit per each Staking Unit per Minute (365*24*60) = 525600"
            interest <- getInterestRate 525600

            -- Deploy.exportarPoolParamsYScripts nombrePool mastersStr uTxOutRefStr begintAtPoolStr deadlinePoolStr graceTimeStr staking_UI staking_CS_Str staking_TN_Str harvest_UI harvest_CS_Str harvest_TN_Str interestStr pathOutputFiles

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Generating Script Minting PoolID"

            let
                uTxOutRefAt = fst <$> PABSimulatorHelpers.getUTxOsListInPABSimulator blockchain (PABSimulatorHelpers.walletPaymentPubKeyHashAddress  walletNro)
                poolID_TxOutRef = head uTxOutRefAt

                policy_PoolID = OnChainNFT.policy_PoolID masters poolID_TxOutRef
                curSymbol_PoolID = Utils.getCurSymbolOfPolicy policy_PoolID
            _ <- MonadIOClass.liftIO $ Deploy.writeMintingPolicy policy_PoolID curSymbol_PoolID (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_PoolID"
            
            let    
                poolID_CS = curSymbol_PoolID
                -- poolID_TN = T.poolID_TN
                -- poolID_AC = LedgerValue.AssetClass (poolID_CS, poolID_TN)

                pParams = T.PoolParams
                    {
                        T.ppPoolID_CS = poolID_CS,

                        T.ppMasters = masters,
                        T.ppBeginAt = beginAtPool,
                        T.ppDeadline = deadlinePool,
                        T.ppGraceTime = graceTime,

                        T.ppStaking_CS = fst $ LedgerValue.unAssetClass staking_AC,
                        T.ppStaking_TN = snd $ LedgerValue.unAssetClass staking_AC,

                        T.ppHarvest_CS = fst $ LedgerValue.unAssetClass harvest_AC,
                        T.ppHarvest_TN = snd $ LedgerValue.unAssetClass harvest_AC,

                        --T.ppInterestRates = [T.InterestRate { T.iMinDays = Just 90, T.iPercentage = 1 }, T.InterestRate { T.iMinDays = Just 180, T.iPercentage = 2 }, T.InterestRate { T.iMinDays = Nothing, T.iPercentage = 3 }],
                        T.ppInterestRates = [T.InterestRate {T.iMinDays = Nothing, T.iPercentage = interest}]

                    }

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "PoolID TxOutRef: " ++ P.show poolID_TxOutRef

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Generating 'Master Fund' Minting Script..."
            let
                !policy_TxID_Master_Fund = OnChainNFT.policy_TxID_Master_Fund pParams
                !txID_Master_Fund_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_Fund
            _ <- MonadIOClass.liftIO $ Deploy.writeMintingPolicy policy_TxID_Master_Fund txID_Master_Fund_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_Fund"

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Generating 'User Deposit' Minting Script..."
            let
                !policy_TxID_User_Deposit = OnChainNFT.policy_TxID_User_Deposit pParams
                !txID_User_Deposit_CS = Utils.getCurSymbolOfPolicy policy_TxID_User_Deposit
            _ <- MonadIOClass.liftIO $ Deploy.writeMintingPolicy policy_TxID_User_Deposit txID_User_Deposit_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_User_Deposit"

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Generating 'User Harvest' Minting Script..."
            let
                !policy_TxID_User_Harvest = OnChainNFT.policy_TxID_User_Harvest pParams txID_Master_Fund_CS txID_User_Deposit_CS
                !txID_User_Harvest_CS = Utils.getCurSymbolOfPolicy policy_TxID_User_Harvest
            _ <- MonadIOClass.liftIO $ Deploy.writeMintingPolicy policy_TxID_User_Harvest txID_User_Harvest_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_User_Harvest"

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Generating 'Master Fund And Merge' Minting Script..."
            let
                !policy_TxID_Master_FundAndMerge = OnChainNFT.policy_TxID_Master_FundAndMerge pParams txID_Master_Fund_CS
                !txID_Master_FundAndMerge_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_FundAndMerge
            _ <- MonadIOClass.liftIO $ Deploy.writeMintingPolicy policy_TxID_Master_FundAndMerge txID_Master_FundAndMerge_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_FundAndMerge"

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Generating 'Master Split Fund' Minting Script..."
            let
                !policy_TxID_Master_SplitFund = OnChainNFT.policy_TxID_Master_SplitFund pParams txID_Master_Fund_CS
                !txID_Master_SplitFund_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_SplitFund
            _ <- MonadIOClass.liftIO $ Deploy.writeMintingPolicy policy_TxID_Master_SplitFund txID_Master_SplitFund_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_SplitFund"

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Generating 'Master Close Pool' Minting Script..."
            let
                !policy_TxID_Master_ClosePool = OnChainNFT.policy_TxID_Master_ClosePool pParams
                !txID_Master_ClosePool_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_ClosePool
            _ <- MonadIOClass.liftIO $ Deploy.writeMintingPolicy policy_TxID_Master_ClosePool txID_Master_ClosePool_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_ClosePool"

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Generating 'Master Terminate Pool' Minting Script..."
            let
                !policy_TxID_Master_TerminatePool = OnChainNFT.policy_TxID_Master_TerminatePool pParams
                !txID_Master_TerminatePool_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_TerminatePool
            _ <- MonadIOClass.liftIO $ Deploy.writeMintingPolicy policy_TxID_Master_TerminatePool txID_Master_TerminatePool_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_TerminatePool"

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Generating 'Master Emergency' Minting Script..."
            let
                !policy_TxID_Master_Emergency = OnChainNFT.policy_TxID_Master_Emergency pParams
                !txID_Master_Emergency_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_Emergency
            _ <- MonadIOClass.liftIO $ Deploy.writeMintingPolicy policy_TxID_Master_Emergency txID_Master_Emergency_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_Emergency"

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Generating 'Master Delete Fund' Minting Script..."
            let
                !policy_TxID_Master_DeleteFund = OnChainNFT.policy_TxID_Master_DeleteFund pParams txID_Master_Fund_CS
                !txID_Master_DeleteFund_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_DeleteFund
            _ <- MonadIOClass.liftIO $ Deploy.writeMintingPolicy policy_TxID_Master_DeleteFund txID_Master_DeleteFund_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_DeleteFund"

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Generating 'Master Send Back Fund' Minting Script..."
            let
                !policy_TxID_Master_SendBackFund = OnChainNFT.policy_TxID_Master_SendBackFund pParams
                !txID_Master_SendBackFund_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_SendBackFund
            _ <- MonadIOClass.liftIO $ Deploy.writeMintingPolicy policy_TxID_Master_SendBackFund txID_Master_SendBackFund_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_SendBackFund"

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Generating 'Master Send Back Deposit' Minting Script..."
            let
                !policy_TxID_Master_SendBackDeposit = OnChainNFT.policy_TxID_Master_SendBackDeposit pParams txID_Master_Fund_CS txID_User_Deposit_CS txID_User_Harvest_CS
                !txID_Master_SendBackDeposit_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_SendBackDeposit
            _ <- MonadIOClass.liftIO $ Deploy.writeMintingPolicy policy_TxID_Master_SendBackDeposit txID_Master_SendBackDeposit_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_SendBackDeposit"

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Generating 'Master Add Scripts' Minting Script..."
            let
                !policy_TxID_Master_AddScripts = OnChainNFT.policy_TxID_Master_AddScripts pParams
                !txID_Master_AddScripts_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_AddScripts
            _ <- MonadIOClass.liftIO $ Deploy.writeMintingPolicy policy_TxID_Master_AddScripts txID_Master_AddScripts_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_AddScripts"

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Generating 'Master Delete Scripts' Minting Script..."
            let
                !policy_TxID_Master_DeleteScripts = OnChainNFT.policy_TxID_Master_DeleteScripts pParams txID_Master_AddScripts_CS
                !txID_Master_DeleteScripts_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_DeleteScripts
            _ <- MonadIOClass.liftIO $ Deploy.writeMintingPolicy policy_TxID_Master_DeleteScripts txID_Master_DeleteScripts_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_DeleteScripts"

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Generating 'User Withdraw' Minting Script..."
            let
                !policy_TxID_User_Withdraw = OnChainNFT.policy_TxID_User_Withdraw pParams txID_Master_Fund_CS txID_User_Deposit_CS txID_User_Harvest_CS
                !txID_User_Withdraw_CS = Utils.getCurSymbolOfPolicy policy_TxID_User_Withdraw
            _ <- MonadIOClass.liftIO $ Deploy.writeMintingPolicy policy_TxID_User_Withdraw txID_User_Withdraw_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Mint_User_Withdraw"

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Generating Main Validator Script..."

            let
                validator = OnChain.codeValidator pParams txID_Master_Fund_CS txID_Master_FundAndMerge_CS txID_Master_SplitFund_CS txID_Master_ClosePool_CS txID_Master_TerminatePool_CS txID_Master_Emergency_CS txID_Master_DeleteFund_CS txID_Master_SendBackFund_CS txID_Master_SendBackDeposit_CS txID_Master_AddScripts_CS txID_Master_DeleteScripts_CS txID_User_Deposit_CS txID_User_Harvest_CS  txID_User_Withdraw_CS
                hash = Utils.hashValidator validator
                address = Utils.addressValidator hash
            _ <- MonadIOClass.liftIO $ Deploy.writeValidator validator (basePathFiles SystemFilePathPosix.</> nombrePool) "Validator"
            _ <- MonadIOClass.liftIO $ Deploy.writeValidatorHash hash (basePathFiles SystemFilePathPosix.</> nombrePool) "Validator"
            _ <- MonadIOClass.liftIO $ Deploy.writeValidatorAddress address (basePathFiles SystemFilePathPosix.</> nombrePool) "Validator"

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Creating PAB Pool Params File..."

            let
                pabPoolParams = T.PABPoolParams
                    {   
                        T.pppStaking_UI = staking_UI,
                        T.pppHarvest_UI = harvest_UI,

                        T.pppPoolParams = pParams,
                        T.pppPoolID_TxOutRef = poolID_TxOutRef,

                        T.pppValidator = validator,
                        T.pppValidatorHash = hash,
                        T.pppValidatorAddress = address,

                        T.pppPolicy_PoolID  = policy_PoolID,
                        T.pppCurSymbol_PoolID = poolID_CS,

                        T.pppPolicy_TxID_Master_Fund  = policy_TxID_Master_Fund,
                        T.pppPolicy_TxID_Master_FundAndMerge  = policy_TxID_Master_FundAndMerge,
                        T.pppPolicy_TxID_Master_SplitFund  = policy_TxID_Master_SplitFund,
                        T.pppPolicy_TxID_Master_ClosePool  = policy_TxID_Master_ClosePool,
                        T.pppPolicy_TxID_Master_TerminatePool  = policy_TxID_Master_TerminatePool,
                        T.pppPolicy_TxID_Master_Emergency  = policy_TxID_Master_Emergency,
                        T.pppPolicy_TxID_Master_DeleteFund  = policy_TxID_Master_DeleteFund,
                        T.pppPolicy_TxID_Master_SendBackFund  = policy_TxID_Master_SendBackFund,
                        T.pppPolicy_TxID_Master_SendBackDeposit  = policy_TxID_Master_SendBackDeposit,
                        T.pppPolicy_TxID_Master_AddScripts  = policy_TxID_Master_AddScripts,
                        T.pppPolicy_TxID_Master_DeleteScripts  = policy_TxID_Master_DeleteScripts,

                        T.pppPolicy_TxID_User_Deposit  = policy_TxID_User_Deposit,
                        T.pppPolicy_TxID_User_Harvest  = policy_TxID_User_Harvest,
                        T.pppPolicy_TxID_User_Withdraw  = policy_TxID_User_Withdraw,

                        T.pppCurSymbol_TxID_Master_Fund = txID_Master_Fund_CS,
                        T.pppCurSymbol_TxID_Master_FundAndMerge = txID_Master_FundAndMerge_CS,
                        T.pppCurSymbol_TxID_Master_SplitFund = txID_Master_SplitFund_CS,
                        T.pppCurSymbol_TxID_Master_ClosePool = txID_Master_ClosePool_CS,
                        T.pppCurSymbol_TxID_Master_TerminatePool = txID_Master_TerminatePool_CS,
                        T.pppCurSymbol_TxID_Master_Emergency = txID_Master_Emergency_CS,
                        T.pppCurSymbol_TxID_Master_DeleteFund = txID_Master_DeleteFund_CS,
                        T.pppCurSymbol_TxID_Master_SendBackFund = txID_Master_SendBackFund_CS,
                        T.pppCurSymbol_TxID_Master_SendBackDeposit = txID_Master_SendBackDeposit_CS,
                        T.pppCurSymbol_TxID_Master_AddScripts = txID_Master_AddScripts_CS,
                        T.pppCurSymbol_TxID_Master_DeleteScripts = txID_Master_DeleteScripts_CS,

                        T.pppCurSymbol_TxID_User_Deposit = txID_User_Deposit_CS,
                        T.pppCurSymbol_TxID_User_Harvest = txID_User_Harvest_CS,
                        T.pppCurSymbol_TxID_User_Withdraw = txID_User_Withdraw_CS
                    }

            MonadIOClass.liftIO $ Utils.writeEncodedToFile (basePathFiles SystemFilePathPosix.</> nombrePool SystemFilePathPosix.</> "PABPoolParams-HEX.json") pabPoolParams

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Saved PAB Pool Params in :" ++ P.show (basePathFiles SystemFilePathPosix.</> nombrePool SystemFilePathPosix.</> "PABPoolParams-HEX.json")

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        _ -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (walletNro', walletCount) pabPoolParams' shutdown

--------------------------------------------------------------------------------

elegirPoolParams :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
elegirPoolParams (walletNro', walletCount) pabPoolParams' shutdown = do

    basePathFilesMaybe <- MonadIOClass.liftIO $ SystemEnvironment.lookupEnv "RATS_PLUTUS_FILES"
    let
        basePathFiles = case basePathFilesMaybe of
            Nothing -> "files/validators/V2/StakePlusV2/PABSimulator"
            Just path -> path SystemFilePathPosix.</> "PABSimulator"

    files <- MonadIOClass.liftIO $ SystemDirectory.listDirectory basePathFiles

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Staking Pools:"

    let

        -- filterFiles = filter (
        --                     \n -> case DataTextSearch.indices "PABPoolParams" (DataString.fromString n) of
        --                             (_:_) -> True
        --                             [] -> False
        --                     ) files

        !filterFiles = files    

        enumerate x = zip [0..] x

        formatList list = concat
            [
                [P.show (n+1 :: Integer) ++ ": " ++ P.show item]
                |
                (n, item) <- enumerate list

            ]

    mapM_  ( PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) (formatList filterFiles)

    nombrePool <- PABSimulatorHelpers.getFile basePathFiles filterFiles

    jsonFile <- MonadIOClass.liftIO $ Utils.readFile (basePathFiles SystemFilePathPosix.</> nombrePool SystemFilePathPosix.</> "PABPoolParams-HEX.json")

    case DataAeson.decode jsonFile :: Maybe T.PABPoolParams  of
        Nothing -> elegirPoolParams (walletNro', walletCount) pabPoolParams' shutdown
        Just pabPoolParams -> mainLoop (walletNro', walletCount) (Just pabPoolParams) shutdown

--------------------------------------------------------------------------------

mintTokens :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
mintTokens (walletNro', walletCount) pabPoolParams' shutdown =

    case walletNro' of

        Just walletNro -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Enter policy number: "
            policyNum <- PABSimulatorHelpers.getInt

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Enter TokenName: "
            mintTokenNameBaseStr <- PABSimulatorHelpers.getStr
            let mintTokenNameBaseBBS = Utils.stringToBuiltinByteString mintTokenNameBaseStr

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Enter if you want to mint different TokenNames using the given TokenName as a base (y/n): "
            mintDiffTokenName <- PABSimulatorHelpers.getBool

            pABMasterMintFreeParams <- 
                    if mintDiffTokenName then do
                        PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Enter number of different TokenNames: "
                        pmmfMintDiifTokenNameCount <- PABSimulatorHelpers.getInt

                        PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Enter amount to mint for each TokenName: "
                        mintAmount <- PABSimulatorHelpers.getInt

                        return $ PAB.MasterMintFree T.PABMasterMintFreeParams{
                                T.pmmfMintPolicyNum = policyNum,
                                T.pmmfMintTokenNameBase = mintTokenNameBaseBBS,
                                T.pmmfMintDiifTokenNameCount = pmmfMintDiifTokenNameCount,
                                T.pmmfMintAmount = mintAmount
                            }
                    else do
                        PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Enter amount to mint: "
                        mintAmount <- PABSimulatorHelpers.getInt

                        return $ PAB.MasterMintFree T.PABMasterMintFreeParams{
                                T.pmmfMintPolicyNum = policyNum,
                                T.pmmfMintTokenNameBase = mintTokenNameBaseBBS,
                                T.pmmfMintDiifTokenNameCount = 0,
                                T.pmmfMintAmount = mintAmount
                            }

            cMasterMintFree_Master <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) pABMasterMintFreeParams

            _ <- PABSimulator.waitUntilFinished cMasterMintFree_Master

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  PABSimulatorHelpers.getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) pabPoolParams' shutdown

        _ -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

--------------------------------------------------------------------------------

masterPreparePool :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
masterPreparePool (walletNro', walletCount) pabPoolParams' shutdown =

    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do
            let
                poolID_TxOutRef = T.pppPoolID_TxOutRef pabPoolParams --T.ppPoolID_TxOutRef $ T.pppPoolParams pabPoolParams

            let pABMasterPreparePoolParams = PAB.MasterPreparePool T.PABMasterPreparePoolParams{
                        T.pmcpPABPoolParams = pabPoolParams,
                        T.pmcpPoolID_TxOutRef = poolID_TxOutRef
                    }

            cmasterPreparePool_Master <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) pABMasterPreparePoolParams

            _ <- PABSimulator.waitUntilFinished cmasterPreparePool_Master

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  PABSimulatorHelpers.getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

--------------------------------------------------------------------------------

masterNewFund :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
masterNewFund (walletNro', walletCount) pabPoolParams' shutdown =

    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do

            let 
                harvest_UI = T.pppHarvest_UI pabPoolParams
                harvest_AC = LedgerValue.AssetClass ( T.ppHarvest_CS $ T.pppPoolParams pabPoolParams, T.ppHarvest_TN $ T.pppPoolParams pabPoolParams)

            fundAmount <- PABSimulatorHelpers.getAmount harvest_UI harvest_AC 0

            cMasterFund_Master <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) (PAB.MasterFund T.PABMasterFundParams{
                    T.pmfpPABPoolParams = pabPoolParams,
                    T.pmfpFundAmount   = fundAmount
                })

            _ <- PABSimulator.waitUntilFinished cMasterFund_Master

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  PABSimulatorHelpers.getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine
            mainLoop (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

--------------------------------------------------------------------------------

masterFundAndMerge :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
masterFundAndMerge (walletNro', walletCount) pabPoolParams' shutdown =
    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do

            blockchain <- PABSimulator.blockchain

            let
                !address = T.pppValidatorAddress pabPoolParams

                !uTxOuts = PABSimulatorHelpers.getUTxOsListInPABSimulator blockchain address

                !fundID_CS = T.pppCurSymbol_TxID_Master_Fund pabPoolParams
                !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)

            selectedUTxOs <- PABSimulatorHelpers.selectUTxOs fundID_AC [] uTxOuts blockchain

            let
                selectedUTxOsRef = snd <$> selectedUTxOs

            let 
                !harvest_UI = T.pppHarvest_UI pabPoolParams
                !harvest_AC = LedgerValue.AssetClass ( T.ppHarvest_CS $ T.pppPoolParams pabPoolParams, T.ppHarvest_TN $ T.pppPoolParams pabPoolParams)

            fundAmount <- PABSimulatorHelpers.getAmount harvest_UI harvest_AC 0

            cMasterFundAndMerge_Master <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) (PAB.MasterFundAndMerge T.PABMasterFundAndMergeParams{
                    T.pmfampPABPoolParams = pabPoolParams,
                    T.pmfampFundIDs_TxOutRefs = selectedUTxOsRef,
                    T.pmfampFundAmount   = fundAmount
                })

            _ <- PABSimulator.waitUntilFinished cMasterFundAndMerge_Master

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  PABSimulatorHelpers.getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine
            mainLoop (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

--------------------------------------------------------------------------------

masterMergeFunds :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
masterMergeFunds (walletNro', walletCount) pabPoolParams' shutdown =
    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do

            blockchain <- PABSimulator.blockchain

            let
                address = T.pppValidatorAddress pabPoolParams

                uTxOuts = PABSimulatorHelpers.getUTxOsListInPABSimulator blockchain address

                !fundID_CS = T.pppCurSymbol_TxID_Master_Fund pabPoolParams
                !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)

            selectedUTxOs <- PABSimulatorHelpers.selectUTxOs fundID_AC [] uTxOuts blockchain

            let
                selectedUTxOsRef = snd <$> selectedUTxOs
            
            let 
                fundAmount = 0

            cMasterFundAndMerge_Master <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) (PAB.MasterFundAndMerge T.PABMasterFundAndMergeParams{
                    T.pmfampPABPoolParams = pabPoolParams,
                    T.pmfampFundIDs_TxOutRefs = selectedUTxOsRef,
                    T.pmfampFundAmount   = fundAmount
                })

            _ <- PABSimulator.waitUntilFinished cMasterFundAndMerge_Master

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  PABSimulatorHelpers.getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine
            mainLoop (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

--------------------------------------------------------------------------------

masterSplitFund :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
masterSplitFund (walletNro', walletCount) pabPoolParams' shutdown =
    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do

            blockchain <- PABSimulator.blockchain

            let
                address = T.pppValidatorAddress pabPoolParams

                uTxOuts = PABSimulatorHelpers.getUTxOsListInPABSimulator blockchain address

                !fundID_CS = T.pppCurSymbol_TxID_Master_Fund pabPoolParams
                !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)

            selectedUTxO' <- PABSimulatorHelpers.selectUTxO fundID_AC uTxOuts blockchain

            case selectedUTxO' of
                Nothing -> do
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine
                    mainLoop  (Just walletNro, walletCount) (Just pabPoolParams) shutdown
                Just selectedUTxO -> do
                    let
                        !selectedUTxORef = snd selectedUTxO

                    let 
                        !harvest_UI = T.pppHarvest_UI pabPoolParams
                        !harvest_AC = LedgerValue.AssetClass ( T.ppHarvest_CS $ T.pppPoolParams pabPoolParams, T.ppHarvest_TN $ T.pppPoolParams pabPoolParams)

                    !splitAmount <- PABSimulatorHelpers.getAmount harvest_UI harvest_AC 0

                    !cMasterSplitFund_Master <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) (PAB.MasterSplitFund T.PABMasterSplitFundParams{
                            T.pmsPABPoolParams = pabPoolParams,
                            T.pmsFundID_TxOutRef  = selectedUTxORef,
                            T.pmsSplitFundAmount   = splitAmount
                        })

                    _ <- PABSimulator.waitUntilFinished cMasterSplitFund_Master

                    slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
                    let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  PABSimulatorHelpers.getFormatTime posixTime

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine
                    mainLoop (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

--------------------------------------------------------------------------------

masterClosePool :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
masterClosePool (walletNro', walletCount) pabPoolParams' shutdown =
    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do

            cMasterClosePool_Master <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) (PAB.MasterClosePool T.PABMasterClosePoolParams{
                    T.pmcPABPoolParams = pabPoolParams
                })

            _ <- PABSimulator.waitUntilFinished cMasterClosePool_Master

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  PABSimulatorHelpers.getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine
            mainLoop (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

--------------------------------------------------------------------------------

masterTerminatePool :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
masterTerminatePool (walletNro', walletCount) pabPoolParams' shutdown =
    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do

            cMasterTerminatePool_Master <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) (PAB.MasterTerminatePool T.PABMasterTerminatePoolParams{
                    T.pmtPABPoolParams = pabPoolParams
                })

            _ <- PABSimulator.waitUntilFinished cMasterTerminatePool_Master

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  PABSimulatorHelpers.getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine
            mainLoop (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

--------------------------------------------------------------------------------

masterEmergency :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
masterEmergency (walletNro', walletCount) pabPoolParams' shutdown =
    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do

            cMasterEmergency_Master <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) (PAB.MasterEmergency T.PABMasterEmergencyParams{
                    T.pmePABPoolParams = pabPoolParams
                })

            _ <- PABSimulator.waitUntilFinished cMasterEmergency_Master

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  PABSimulatorHelpers.getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine
            mainLoop (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

--------------------------------------------------------------------------------

masterDeleteFunds :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
masterDeleteFunds (walletNro', walletCount) pabPoolParams' shutdown =
    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do

            blockchain <- PABSimulator.blockchain

            let
                address = T.pppValidatorAddress pabPoolParams

                uTxOuts = PABSimulatorHelpers.getUTxOsListInPABSimulator blockchain address

                !fundID_CS = T.pppCurSymbol_TxID_Master_Fund pabPoolParams
                !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)

            selectedUTxOs <- PABSimulatorHelpers.selectUTxOs fundID_AC [] uTxOuts blockchain

            let
                selectedUTxOsRef = snd <$> selectedUTxOs

            cMasterDeleteFund_Master <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) (PAB.MasterDeleteFund T.PABMasterDeleteFundParams{
                    T.pmdPABPoolParams = pabPoolParams,
                    T.pmdFundIDs_TxOutRefs = selectedUTxOsRef
                })

            _ <- PABSimulator.waitUntilFinished cMasterDeleteFund_Master

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  PABSimulatorHelpers.getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine
            mainLoop (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

--------------------------------------------------------------------------------

masterGetBackFund :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
masterGetBackFund (walletNro', walletCount) pabPoolParams' shutdown =
    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do
            
            let
                master = PABSimulatorHelpers.walletPaymentPubKeyHash walletNro

            cMasterSendBackFund_Master <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) (PAB.MasterSendBackFund T.PABMasterSendBackFundParams{
                    T.pmsbfPABPoolParams = pabPoolParams,
                    T.pmsbfMasterToSendBack = Ledger.unPaymentPubKeyHash master
                })

            _ <- PABSimulator.waitUntilFinished cMasterSendBackFund_Master

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  PABSimulatorHelpers.getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine
            mainLoop (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

--------------------------------------------------------------------------------

masterSendBackFund :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
masterSendBackFund (walletNro', walletCount) pabPoolParams' shutdown =
    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do

            masterWalletNro <- PABSimulatorHelpers.elegirMasterParaSendBack walletCount
            let
                master = PABSimulatorHelpers.walletPaymentPubKeyHash masterWalletNro

            cMasterSendBackFund_Master <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) (PAB.MasterSendBackFund T.PABMasterSendBackFundParams{
                    T.pmsbfPABPoolParams = pabPoolParams,
                    T.pmsbfMasterToSendBack = Ledger.unPaymentPubKeyHash master
                })

            _ <- PABSimulator.waitUntilFinished cMasterSendBackFund_Master

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  PABSimulatorHelpers.getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine
            mainLoop (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

--------------------------------------------------------------------------------

masterSendBackDeposit :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
masterSendBackDeposit (walletNro', walletCount) pabPoolParams' shutdown =
    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do

            blockchain <- PABSimulator.blockchain

            let
                address = T.pppValidatorAddress pabPoolParams

                uTxOuts = PABSimulatorHelpers.getUTxOsListInPABSimulator blockchain address

                !userID_CS = T.pppCurSymbol_TxID_User_Deposit pabPoolParams
                !userID_AC = LedgerValue.AssetClass (userID_CS, T.userID_TN) 

            selectedUTxO' <- PABSimulatorHelpers.selectUTxO userID_AC uTxOuts blockchain

            case selectedUTxO' of
                Nothing -> do
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine
                    mainLoop  (Just walletNro, walletCount) (Just pabPoolParams) shutdown
                Just selectedUTxO -> do
                    let
                        selectedUTxORef = snd selectedUTxO

                    cMasterSendBackDeposit_Master <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) (PAB.MasterSendBackDeposit T.PABMasterSendBackDepositParams{
                            T.pmsbiPABPoolParams = pabPoolParams,
                            T.pmsbiUserID_TxOutRef = selectedUTxORef
                        })

                    _ <- PABSimulator.waitUntilFinished cMasterSendBackDeposit_Master

                    slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
                    let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  PABSimulatorHelpers.getFormatTime posixTime

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine
                    mainLoop (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

--------------------------------------------------------------------------------

userDeposit :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
userDeposit (walletNro', walletCount) pabPoolParams' shutdown =
    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do

            let 
                !staking_UI = T.pppStaking_UI pabPoolParams
                !staking_AC = LedgerValue.AssetClass ( T.ppStaking_CS $ T.pppPoolParams pabPoolParams, T.ppStaking_TN $ T.pppPoolParams pabPoolParams)

            !investAmount <- PABSimulatorHelpers.getAmount staking_UI staking_AC 0

            let
                !pABuserDepositParams = T.PABUserDepositParams{
                        T.puiPABPoolParams = pabPoolParams,
                        T.puiInvestAmount   = investAmount
                    }

            !cUserDeposit_User <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) (PAB.UserDeposit pABuserDepositParams )

            _ <- PABSimulator.waitUntilFinished cUserDeposit_User

            !slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let !posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  PABSimulatorHelpers.getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop  (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

--------------------------------------------------------------------------------

userHarvest :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
userHarvest (walletNro', walletCount) pabPoolParams' shutdown =
    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do

            blockchain <- PABSimulator.blockchain

            let
                address = T.pppValidatorAddress pabPoolParams

                uTxOuts = PABSimulatorHelpers.getUTxOsListInPABSimulator blockchain address

                !userID_CS = T.pppCurSymbol_TxID_User_Deposit pabPoolParams
                !userID_AC = LedgerValue.AssetClass (userID_CS, T.userID_TN) 

            selectedUTxO' <- PABSimulatorHelpers.selectUTxO userID_AC uTxOuts blockchain

            case selectedUTxO' of
                Nothing -> do
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine
                    mainLoop  (Just walletNro, walletCount) (Just pabPoolParams) shutdown
                Just selectedUTxO -> do
                    let
                        selectedUTxORef = snd selectedUTxO

                    let 
                        !harvest_UI = T.pppHarvest_UI pabPoolParams
                        !harvest_AC = LedgerValue.AssetClass ( T.ppHarvest_CS $ T.pppPoolParams pabPoolParams, T.ppHarvest_TN $ T.pppPoolParams pabPoolParams)

                    !claimAmount <- PABSimulatorHelpers.getAmount harvest_UI harvest_AC 0

                    let
                        !userHarvestParams = T.PABUserHarvestParams{
                                T.pugrPABPoolParams =  pabPoolParams,
                                T.pugrUserID_TxOutRef = selectedUTxORef,
                                T.pugrClaimAmount  = claimAmount
                            }

                    !cUserHarvest_User <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) (PAB.UserHarvest userHarvestParams )

                    _ <- PABSimulator.waitUntilFinished cUserHarvest_User

                    !slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
                    let !posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  PABSimulatorHelpers.getFormatTime posixTime

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine

                    mainLoop  (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

--------------------------------------------------------------------------------

userWithdraw :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
userWithdraw (walletNro', walletCount) pabPoolParams' shutdown =
    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do

            blockchain <- PABSimulator.blockchain

            let
                address = T.pppValidatorAddress pabPoolParams

                uTxOuts = PABSimulatorHelpers.getUTxOsListInPABSimulator blockchain address

                !userID_CS = T.pppCurSymbol_TxID_User_Deposit pabPoolParams
                !userID_AC = LedgerValue.AssetClass (userID_CS, T.userID_TN) 

            selectedUTxO' <- PABSimulatorHelpers.selectUTxO userID_AC uTxOuts blockchain

            case selectedUTxO' of
                Nothing -> do
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine
                    mainLoop  (Just walletNro, walletCount) (Just pabPoolParams) shutdown
                Just selectedUTxO -> do
                    let
                        selectedUTxORef = snd selectedUTxO

                        userWithdrawParams = T.PABUserWithdrawParams{
                                T.pugbiPABPoolParams = pabPoolParams,
                                T.pugbiUserID_TxOutRef = selectedUTxORef
                            }

                    cUserWithdraw_User <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) (PAB.UserWithdraw userWithdrawParams )

                    _ <- PABSimulator.waitUntilFinished cUserWithdraw_User

                    slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
                    let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  PABSimulatorHelpers.getFormatTime posixTime

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine

                    mainLoop  (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

--------------------------------------------------------------------------------

masterAddScripts :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
masterAddScripts (walletNro', walletCount) pabPoolParams' shutdown =
    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do

            let
                masterAddScriptsParams = T.PABMasterAddScriptsParams{
                        T.pmasPABPoolParams = pabPoolParams
                    }

            cMasterAddScripts_User <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) (PAB.MasterAddScripts masterAddScriptsParams )

            _ <- PABSimulator.waitUntilFinished cMasterAddScripts_User

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  PABSimulatorHelpers.getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop  (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

--------------------------------------------------------------------------------

masterDeleteScripts :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
masterDeleteScripts (walletNro', walletCount) pabPoolParams' shutdown =
    case (walletNro',  pabPoolParams') of
        (Just walletNro, Just pabPoolParams) -> do

            let
                masterDeleteScriptsParams = T.PABMasterDeleteScriptsParams {
                        T.pmdsPABPoolParams = pabPoolParams
                    }

            cMasterDeleteScripts_User <- PABSimulator.activateContract (PABSimulatorHelpers.getWallet walletNro) (PAB.MasterDeleteScripts masterDeleteScriptsParams )

            _ <- PABSimulator.waitUntilFinished cMasterDeleteScripts_User

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  PABSimulatorHelpers.getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop  (Just walletNro, walletCount) (Just pabPoolParams) shutdown

        (_, Just pabPoolParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) (Just pabPoolParams) shutdown

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Just walletNro, walletCount) Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (Nothing, walletCount) Nothing shutdown

--------------------------------------------------------------------------------