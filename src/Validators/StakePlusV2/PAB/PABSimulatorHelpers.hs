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
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}
-- {-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE TupleSections              #-}
-- {-# LANGUAGE AllowAmbiguousTypes        #-}
-- {-# LANGUAGE NumericUnderscores         #-}
-- {-# LANGUAGE QuasiQuotes                #-}
{- HLINT ignore "Use camelCase" -}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}
-----------------------------------------------------------------------------------------
module Validators.StakePlusV2.PAB.PABSimulatorHelpers where
-----------------------------------------------------------------------------------------
-- Import Externos
-----------------------------------------------------------------------------------------
import qualified Control.Concurrent.STM                                 as ConcurrentSTM (atomically)
import qualified Control.Monad.IO.Class                                 as MonadIOClass (MonadIO (..))
import qualified Control.Monad.Freer                                    as MonadFreer (interpret)
import qualified Control.Monad.Freer.Internal                           as MonadFreerInternal (Eff)
import qualified Data.Default                                           as DataDefault (def)
import qualified Data.Fixed                                             as DataFixed (Pico, Fixed ( MkFixed ))
import qualified Data.List                                              as DataList
import qualified Data.Map                                               as DataMap
import qualified Data.Maybe                                             as DataMaybe (Maybe(Nothing))
import qualified Data.Time.Clock                                        as DataTimeClock (secondsToNominalDiffTime)
import qualified Data.Time.Clock.POSIX                                  as DataTimeClockPOSIX (posixSecondsToUTCTime)
import qualified Data.Time.Format                                       as DataTimeFormat (defaultTimeLocale, formatTime)
import qualified Ledger
import qualified Ledger.Ada                                             as LedgerAda
import qualified Ledger.Address                                         as LedgerAddress (Address)
import qualified Ledger.Blockchain                                      as LedgerBlockchain
import qualified Ledger.CardanoWallet                                   as LedgerCardanoWallet
import qualified Ledger.TimeSlot                                        as LedgerTimeSlot
import qualified Ledger.Value                                           as LedgerValue
-- import qualified Playground.Contract                                 as PlaygroundContract (IO)
import qualified Prelude                                                as P
import qualified Plutus.PAB.Core                                        as PABCore (PABEffects)
import qualified Plutus.PAB.Effects.Contract.Builtin                    as PABEffectsContractBuiltin (Builtin, BuiltinHandler(contractHandler), handleBuiltin)
import qualified Plutus.PAB.Simulator                                   as PABSimulator
-- import qualified Plutus.V2.Ledger.Address                            as LedgerAddressV2
import qualified Plutus.V2.Ledger.Api                                   as LedgerApiV2
-- import qualified Plutus.V2.Ledger.Value                              as LedgerValueV2
-- import qualified Plutus.V2.Ledger.Tx                                 as LedgerTxV2 (txOutDatum)
import qualified PlutusTx
import qualified PlutusTx.Builtins.Internal                             as TxBuiltinsInternal hiding (head, consByteString)
import qualified PlutusTx.Eq                                            as PlutusTxEq
import           PlutusTx.Prelude                                       hiding (unless)
import qualified System.Directory                                       as SystemDirectory
import qualified System.FilePath.Posix                                  as SystemFilePathPosix
-- import qualified Text.Hex                                               as TextHex
import qualified Text.Read                                              as TextRead (readMaybe)
import qualified Wallet.Emulator.Wallet                                 as WalletEmulator
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------

import qualified Validators.StakePlusV2.Helpers                                             as Helpers
import qualified Validators.StakePlusV2.PAB.PAB                                             as PAB
import qualified Validators.StakePlusV2.OffChain.OffChainHelpers                            as OffChainHelpers
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
import qualified Validators.StakePlusV2.Types.Constants                                     as T
import qualified Validators.StakePlusV2.Types.DatumsValidator                               as T
import qualified Validators.StakePlusV2.Types.Examples                                      as T
import qualified Validators.StakePlusV2.Types.PABParams                                     as T
import qualified Validators.StakePlusV2.Types.RedeemersMint                                 as T
import qualified Validators.StakePlusV2.Types.RedeemersValidator                            as T
import qualified Validators.StakePlusV2.Types.Types                                         as T
import qualified Utils
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------

handlers :: PABSimulator.SimulatorEffectHandlers (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)
handlers = PABSimulator.mkSimulatorHandlers  DataDefault.def P.$ MonadFreer.interpret (PABEffectsContractBuiltin.contractHandler PABEffectsContractBuiltin.handleBuiltin)

------------------------------------------------------------------------------------------

getWallet :: Integer -> WalletEmulator.Wallet
getWallet = WalletEmulator.knownWallet

------------------------------------------------------------------------------------------

walletPaymentPubKeyHash :: Integer -> Ledger.PaymentPubKeyHash
walletPaymentPubKeyHash walletNumber = LedgerCardanoWallet.paymentPubKeyHash (LedgerCardanoWallet.fromWalletNumber $ LedgerCardanoWallet.WalletNumber walletNumber)

walletPaymentPubKeyHashAddress :: Integer -> LedgerAddress.Address
walletPaymentPubKeyHashAddress walletNumber = Ledger.pubKeyHashAddress (walletPaymentPubKeyHash walletNumber) Nothing

------------------------------------------------------------------------------------------

getUTxOsListInPABSimulator :: Ledger.Blockchain -> LedgerAddress.Address -> [(Ledger.TxOutRef, Ledger.TxOut)]
getUTxOsListInPABSimulator blockchain addr = do
    let
        !unspentOutputList = Ledger.unspentOutputs blockchain
        !uTxOs = [(txOutRef, txOut)  | (txOutRef, txOut) <- DataMap.toList  unspentOutputList, Ledger.txOutAddress  txOut == addr]
    uTxOs

------------------------------------------------------------------------------------------

getFormatTime :: LedgerApiV2.POSIXTime -> P.String
getFormatTime posixTime =
    let
        milisegundosFixedPico :: DataFixed.Pico
        !milisegundosFixedPico = DataFixed.MkFixed  (LedgerApiV2.getPOSIXTime posixTime * 1000000000)
        !seconds = DataTimeClock.secondsToNominalDiffTime milisegundosFixedPico
    in
        DataTimeFormat.formatTime DataTimeFormat.defaultTimeLocale  "%c" $ DataTimeClockPOSIX.posixSecondsToUTCTime seconds

------------------------------------------------------------------------------------------

getAmountWithMax :: P.String -> Ledger.AssetClass -> Integer -> Integer -> MonadFreerInternal.Eff
                  (PABCore.PABEffects
                     (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)
                     (PABSimulator.SimulatorState
                        (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)))
                  Integer
getAmountWithMax unit_UI unit_AC minAmount maxAmount = do
    let

        unit_Str = unit_UI
            -- TODO: mostrar el hex bien
            -- ++ " ("
            -- if LedgerApiV2.adaSymbol /= fst (LedgerValue.unAssetClass unit_AC) then
            --     let 
            --         --  $ Utils.stringToStrictText
            --         cs = P.show (  LedgerValue.unCurrencySymbol $ fst $ LedgerValue.unAssetClass unit_AC) 
            --     in
            --         cs ++ "." ++ P.show (LedgerValue.unTokenName $ snd $ LedgerValue.unAssetClass unit_AC)
            -- else 
            --     ""
            -- ++ ")"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Enter amount of " ++ unit_Str ++ " (min: " ++ P.show minAmount ++ " - max: " ++ P.show maxAmount ++ "): "
    !numberSrt <- MonadIOClass.liftIO P.getLine
    case TextRead.readMaybe numberSrt of
        Just x ->
            if x >= minAmount && x <= maxAmount then return x
            else do
                PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Invalid input"
                getAmountWithMax unit_UI unit_AC minAmount maxAmount
        _ -> do
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Invalid input"
            getAmountWithMax unit_UI unit_AC minAmount maxAmount

------------------------------------------------------------------------------------------

getAmount :: P.String -> Ledger.AssetClass -> Integer -> MonadFreerInternal.Eff
                  (PABCore.PABEffects
                     (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)
                     (PABSimulator.SimulatorState
                        (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)))
                  Integer
getAmount unit_UI unit_AC minAmount  = do
    let

        unit_Str = unit_UI
            -- TODO: mostrar el hex bien
            -- ++ " ("
            -- if LedgerApiV2.adaSymbol /= fst (LedgerValue.unAssetClass unit_AC) then
            --     let 
            --         --  $ Utils.stringToStrictText
            --         cs = P.show (  LedgerValue.unCurrencySymbol $ fst $ LedgerValue.unAssetClass unit_AC) 
            --     in
            --         cs ++ "." ++ P.show (LedgerValue.unTokenName $ snd $ LedgerValue.unAssetClass unit_AC)
            -- else 
            --     ""
            -- ++ ")"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Enter amount of " ++ unit_Str ++ " (min: " ++ P.show minAmount ++ "): "
    !numberSrt <- MonadIOClass.liftIO P.getLine
    case TextRead.readMaybe numberSrt of
        Just x ->
            if x >= minAmount then return x
            else do
                PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Invalid input"
                getAmount unit_UI unit_AC minAmount
        _ -> do
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Invalid input"
            getAmount unit_UI unit_AC minAmount

-----------------------------------------------------------------------------------------

getInt :: MonadFreerInternal.Eff
                  (PABCore.PABEffects
                     (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)
                     (PABSimulator.SimulatorState
                        (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)))
                  Integer
getInt = do
    !numberSrt <- MonadIOClass.liftIO P.getLine
    case TextRead.readMaybe numberSrt of
        Just x ->
            if x >= 0 then
                return x
            else do
                PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Invalid input, try again"
                getInt
        Nothing -> do
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Invalid input, try again"
            getInt

-----------------------------------------------------------------------------------------

getStr :: MonadFreerInternal.Eff
                  (PABCore.PABEffects
                     (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)
                     (PABSimulator.SimulatorState
                        (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)))
                  P.String
getStr = do
    !srt <- MonadIOClass.liftIO P.getLine
    if length srt == 0
    then do
        PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Invalid input, try again"
        getStr
    else return srt

-----------------------------------------------------------------------------------------

getBool :: MonadFreerInternal.Eff
                  (PABCore.PABEffects
                     (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)
                     (PABSimulator.SimulatorState
                        (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)))
                 Bool
getBool = do
    !srt <- MonadIOClass.liftIO P.getLine
    if length srt == 0
    then getBool
    else
        case srt of
            "y" -> return True
            "n" -> return False
            _ -> do
                PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Invalid input, try again"
                getBool

-----------------------------------------------------------------------------------------

getFile :: P.String -> [P.String] -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) P.String
getFile path list = do

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Enter File number:"
    !numeroStr <- MonadIOClass.liftIO P.getLine
    case TextRead.readMaybe numeroStr of
        Just n -> do
            if n <= length list && n > 0 then do
                let !nombre = list!!(n-1)
                PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "File: " ++ nombre
                !exist <- MonadIOClass.liftIO $ SystemDirectory.doesFileExist (path SystemFilePathPosix.</> nombre SystemFilePathPosix.</> "PABPoolParams-HEX.json")
                if exist then do
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "ok"
                    return nombre
                else do
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Invalid input, try again"
                    getFile path list
            else do
                PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Invalid input, try again"
                getFile path list
        Nothing -> do
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Invalid input, try again"
            getFile path list

-----------------------------------------------------------------------------------------

isEqWallet :: WalletEmulator.Wallet -> WalletEmulator.Wallet -> Bool
isEqWallet w w' =
    TxBuiltinsInternal.BuiltinString (WalletEmulator.toBase16 $ WalletEmulator.getWalletId w) PlutusTxEq.== TxBuiltinsInternal.BuiltinString(WalletEmulator.toBase16 $ WalletEmulator.getWalletId w')

-----------------------------------------------------------------------------------------

fromWallet :: Integer -> WalletEmulator.Entity -> Bool
fromWallet numWallet entity =
    case entity of
        WalletEmulator.WalletEntity wallet  -> isEqWallet wallet (getWallet numWallet)
        _                                   -> False

-----------------------------------------------------------------------------------------

fromScript :: T.PABPoolParams -> WalletEmulator.Entity -> Bool
fromScript pabPoolParams entity =
    case entity of
        WalletEmulator.ScriptEntity scriptHast ->
            T.pppValidatorHash pabPoolParams == scriptHast
        _ -> False

-----------------------------------------------------------------------------------------

walletFromEntity :: WalletEmulator.Entity -> Maybe WalletEmulator.Wallet
walletFromEntity entity =
    case entity of
        WalletEmulator.WalletEntity wallet -> Just wallet
        _ -> Nothing

-----------------------------------------------------------------------------------------

balances :: (Maybe Integer, Integer) -> Maybe T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
balances (_, walletCount) pabPoolParams' _ = do

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Balances:"

    !balances' <- PABSimulator.currentBalances

    -- PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ("Balances:" ++ P.show balances')

    let
        -- (entity, value) <- DataMap.toList balances'
        !balanceList = DataMap.toList balances'
        formatWallets = concat [
            let
                fromWalletEntity walletNro' (entity, _) = fromWallet walletNro' entity
                entiyValue' = find (fromWalletEntity walletNro ) balanceList
            in
                case entiyValue' of
                    Nothing -> []
                    Just (_, value) ->
                        [
                            "----------------" ,
                            "#: " ++ P.show walletNro,
                            "Pk: " ++ P.show (walletPaymentPubKeyHash walletNro) ,
                            "Value: " ++ P.show value
                        ] | walletNro <- [1..walletCount]
            ]

    mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) formatWallets

    case pabPoolParams' of
        Just pabPoolParams ->
            mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))
                ["Script: " ++ P.show (T.pppValidatorHash pabPoolParams) ++ " " ++  P.show value | (entity, value) <- DataMap.toList balances', fromScript pabPoolParams entity ]

        _ ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""

    slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
    let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

------------------------------------------------------------------------------------------

selectUTxO :: LedgerValue.AssetClass -> [(Ledger.TxOutRef, Ledger.TxOut)] -> LedgerBlockchain.Blockchain -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)))  (Maybe (Integer, Ledger.TxOutRef))
selectUTxO unit_AC uTxOuts blockchain = do
    let
        !uTxOutsWithAC' =
            [ (txOutRef, txOut) | (txOutRef, txOut) <- uTxOuts, LedgerValue.assetClassValueOf (Ledger.txOutValue txOut) unit_AC > 0 ]

    case uTxOutsWithAC' of
        [] -> do
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "There is no UtxO to choose"
            return Nothing
        uTxOutsWithAC ->  do
            let
                datumFrom _ =
                    "TODO: get Datum"

                formatValues uTxORef = [P.show val   |  val <- LedgerValue.flattenValue $ Helpers.fromJust $ LedgerBlockchain.value blockchain uTxORef ]

                formatUTxOValues = concat [
                    "----------------" :
                    ("#: " ++ P.show ( 1 P.+  Helpers.fromJust(DataList.elemIndex (uTxORef, uTxOut) uTxOutsWithAC))) :
                    ("At: " ++ P.show uTxORef) :
                    ("Datum: " ++  datumFrom uTxOut) : formatValues uTxORef | (uTxORef, uTxOut) <- uTxOutsWithAC
                    ]

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "----------------"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose UtxO:"

            mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) formatUTxOValues

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "----------------"

            !opcionUTxO <- MonadIOClass.liftIO P.getLine

            case TextRead.readMaybe opcionUTxO :: Maybe Integer of
                Just x -> do
                    if x >= 1 && x <= length uTxOutsWithAC then do
                        let
                            !new = (x, fst $ uTxOutsWithAC!!(x-1))
                        return (Just new)
                    else
                        selectUTxO unit_AC uTxOuts blockchain
                _ ->
                    selectUTxO unit_AC uTxOuts blockchain

------------------------------------------------------------------------------------------

selectUTxOs :: LedgerValue.AssetClass -> [(Integer, Ledger.TxOutRef)] -> [(Ledger.TxOutRef, Ledger.TxOut)] -> LedgerBlockchain.Blockchain -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)))  [(Integer, Ledger.TxOutRef)]
selectUTxOs unit_AC opciones uTxOuts blockchain = do
    let

        !uTxOutsWithAC =
            [ (txOutRef, txOut) | (txOutRef, txOut) <- uTxOuts, LedgerValue.assetClassValueOf (Ledger.txOutValue txOut) unit_AC > 0 ]

        datumFrom _ =
            "TODO: get Datum"

        formatValues uTxORef = [P.show val   |  val <- LedgerValue.flattenValue $ Helpers.fromJust $ LedgerBlockchain.value blockchain uTxORef ]

        formatUTxOValues = concat [
            "----------------" :
            ("#: " ++ P.show ( 1 P.+  Helpers.fromJust(DataList.elemIndex (uTxORef, uTxOut) uTxOutsWithAC))) :
            ("At: " ++ P.show uTxORef) : ("Datum: " ++  datumFrom uTxOut) :
            formatValues uTxORef
            | (uTxORef, uTxOut) <- uTxOutsWithAC ]

        formatSelected :: [(Integer, Ledger.TxOutRef)] -> [P.String]
        formatSelected opciones' = concat [  ["----------------", P.show numOpcion, P.show uTxORef] | (numOpcion, uTxORef) <- opciones' ]

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "----------------"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose UtxO:"

    mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) formatUTxOValues

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "----------------"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Selected:"
    mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) (formatSelected opciones)

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "----------------"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Option (0 to finish):"

    opcionUTxO <- MonadIOClass.liftIO P.getLine

    case TextRead.readMaybe opcionUTxO :: Maybe Integer of
        Just 0 ->
            return opciones
        Just x -> do

            if x >= 1 && x <= length uTxOutsWithAC then do
                let
                    new = (x, fst $ uTxOutsWithAC!!(x-1))
                    news = new : filter (new/=) opciones
                selectUTxOs unit_AC news uTxOuts blockchain
            else
                selectUTxOs unit_AC opciones uTxOuts blockchain
        _ ->
            selectUTxOs unit_AC opciones uTxOuts blockchain

------------------------------------------------------------------------------------------

elegirMasterParaSendBack :: Integer -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)))  Integer
elegirMasterParaSendBack walletCount = do
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Master's Wallet for Send Back Fund"

    let
        formatWallets = concat [ [
                    "----------------" ,
                    "#: " ++ P.show walletNro,
                    "Pk: " ++ P.show (walletPaymentPubKeyHash walletNro)
                ] | walletNro <- [1..walletCount]
            ]
    mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) formatWallets

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Wallet:"
    numberSrt <- MonadIOClass.liftIO P.getLine
    case TextRead.readMaybe numberSrt :: Maybe Integer of
        Just x ->
            if x >= 1 && x <= walletCount then return x
             else
                elegirMasterParaSendBack walletCount
        _ ->
            elegirMasterParaSendBack walletCount

-----------------------------------------------------------------------------------------

elegirWalletsParaMasters :: Integer -> [Integer] -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) [Integer]
elegirWalletsParaMasters walletCount opciones = do
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Master's Wallet:"
    let
        formatWallets = concat [ [
                    "----------------" ,
                    "#: " ++ P.show walletNro,
                    "Pk: " ++ P.show (walletPaymentPubKeyHash walletNro)
                ] | walletNro <- [1..walletCount]
            ]

        formatSelected :: [Integer] -> [P.String]
        formatSelected opciones' =
            concat [  ["----------------", P.show walletNro] | walletNro <- opciones' ]

    mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) formatWallets
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "----------------"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Selected:"
    mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) (formatSelected opciones)
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "----------------"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Option (0 to finish):"
    numberSrt <- MonadIOClass.liftIO P.getLine
    case TextRead.readMaybe numberSrt :: Maybe Integer of
        Just 0 ->
            return opciones
        Just x ->
            if x >= 1 && x <= walletCount then
                let
                    new = x
                    news = new : filter (new/=) opciones
                in elegirWalletsParaMasters walletCount news
             else
                elegirWalletsParaMasters walletCount opciones
        _ ->
            elegirWalletsParaMasters walletCount opciones

-----------------------------------------------------------------------------------------

evaluate_Validator :: T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
evaluate_Validator pabParams = do
    let
        !pParams = T.pppPoolParams pabParams

        !txID_Master_Fund_CS = T.pppCurSymbol_TxID_Master_Fund pabParams
        !txID_Master_FundAndMerge_CS = T.pppCurSymbol_TxID_Master_FundAndMerge pabParams
        !txID_Master_SplitFund_CS = T.pppCurSymbol_TxID_Master_SplitFund pabParams
        !txID_Master_ClosePool_CS = T.pppCurSymbol_TxID_Master_ClosePool pabParams
        !txID_Master_TerminatePool_CS = T.pppCurSymbol_TxID_Master_TerminatePool pabParams
        !txID_Master_Emergency_CS = T.pppCurSymbol_TxID_Master_Emergency pabParams
        !txID_Master_DeleteFund_CS = T.pppCurSymbol_TxID_Master_DeleteFund pabParams
        !txID_Master_SendBackFund_CS = T.pppCurSymbol_TxID_Master_SendBackFund pabParams
        !txID_Master_SendBackDeposit_CS = T.pppCurSymbol_TxID_Master_SendBackDeposit pabParams
        !txID_Master_AddScripts_CS = T.pppCurSymbol_TxID_Master_AddScripts pabParams
        !txID_Master_DeleteScripts_CS = T.pppCurSymbol_TxID_Master_DeleteScripts pabParams
        !txID_User_Deposit_CS = T.pppCurSymbol_TxID_User_Deposit pabParams
        !txID_User_Harvest_CS = T.pppCurSymbol_TxID_User_Harvest pabParams
        !txID_User_Withdraw_CS = T.pppCurSymbol_TxID_User_Withdraw pabParams

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Generating Main Validator Script..."
    let
        validator = OnChain.codeValidator pParams txID_Master_Fund_CS txID_Master_FundAndMerge_CS txID_Master_SplitFund_CS txID_Master_ClosePool_CS txID_Master_TerminatePool_CS txID_Master_Emergency_CS txID_Master_DeleteFund_CS txID_Master_SendBackFund_CS txID_Master_SendBackDeposit_CS txID_Master_AddScripts_CS txID_Master_DeleteScripts_CS txID_User_Deposit_CS txID_User_Harvest_CS  txID_User_Withdraw_CS
        hash = Utils.hashValidator validator
        address = Utils.addressValidator hash

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "address: " ++ P.show address

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Evaluating Validator Script..."
    let

        !staking_CS = T.ppStaking_CS pParams
        !staking_AC = LedgerValue.AssetClass (staking_CS, T.ppStaking_TN pParams)
        !stakingIsAda = LedgerApiV2.adaSymbol == staking_CS
        !stakingIsWithoutTokenName = not stakingIsAda && T.ppStaking_TN pParams == LedgerApiV2.TokenName emptyByteString
        !harvest_CS =  T.ppHarvest_CS pParams
        !harvest_AC = LedgerValue.AssetClass (harvest_CS, T.ppHarvest_TN pParams)
        !harvestIsAda = LedgerApiV2.adaSymbol == harvest_CS
        !haverstIsWithoutTokenName = not harvestIsAda && T.ppHarvest_TN pParams == LedgerApiV2.TokenName emptyByteString

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "staking_AC: " ++ P.show staking_AC
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "stakingIsWithoutTokenName: " ++ P.show stakingIsWithoutTokenName
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "harvest_AC: " ++ P.show harvest_AC
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "haverstIsWithoutTokenName: " ++ P.show haverstIsWithoutTokenName

    let
        !txID_User_Harvest_AC = LedgerValue.AssetClass (txID_User_Harvest_CS, T.txID_User_Harvest_TN)
        !value_For_Mint_TxID_User_Harvest = LedgerValue.assetClassValue txID_User_Harvest_AC 1

        --------------------------------

        !now = LedgerApiV2.POSIXTime 1000000

        --------------------------------

        !claimAmount = 100
        !claimAt = now

        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange   = Ledger.interval ( now - intervalOffset1 ) (now + intervalOffset2)

        --------------------------------

        !masterFunders = []
        !fundCount = 0
        !isClosedAt = Nothing
        !isTerminated = T.poolDatum_NotTerminated
        !isEmergency = T.poolDatum_NotEmergency
        !totalCashedOut = 0
        !minAda_For_PoolDatum = 1000
        !poolDatum_In = T.mkPoolDatumTypo masterFunders fundCount totalCashedOut isClosedAt isTerminated isEmergency minAda_For_PoolDatum
        !poolDatum_In_Plutus = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.PoolDatum poolDatum_In

        !poolID_CS = T.ppPoolID_CS pParams
        !poolID_AC = LedgerValue.AssetClass (poolID_CS, T.poolID_TN)
        !value_PoolID    = LedgerValue.assetClassValue poolID_AC 1

        !value_PoolDatum_In    = LedgerAda.lovelaceValueOf minAda_For_PoolDatum <> value_PoolID

        --------------------------------

        !fundAmount = 100000
        !minAda_For_FundDatum = 1000
        !cashedOut = 0

        !fundDatum_In = T.mkFundDatumTypo fundAmount cashedOut minAda_For_FundDatum
        !fundDatum_In_Plutus = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.FundDatum fundDatum_In

        !fundID_CS = T.pppCurSymbol_TxID_Master_Fund pabParams
        !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)
        !value_FundID    = LedgerValue.assetClassValue fundID_AC 1

        !value_FundAmount = LedgerValue.assetClassValue harvest_AC fundAmount
        !value_FundDatum_In    = LedgerAda.lovelaceValueOf minAda_For_FundDatum <> value_FundID <> value_FundAmount

        --- 

        !fundDatum_Out = Helpers.mkUpdated_FundDatum_With_NewClaimRewards fundDatum_In claimAmount
        !fundDatum_Out_Plutus = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.FundDatum fundDatum_Out

        !valueToSubstract = LedgerValue.assetClassValue harvest_AC claimAmount
        !value_FundDatum_Out = value_FundDatum_In <> negate valueToSubstract

        --------------------------------

        pdClosedAt = T.pdClosedAt poolDatum_In

        !user = T.exampleUser
        !investAmount = 20
        !createdAt = 0
        !minAda_For_UserDatum = 1000

        !userDatum_In = T.mkUserDatumTypo user investAmount createdAt 0 0 DataMaybe.Nothing minAda_For_UserDatum
        !userDatum_In_Plutus = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.UserDatum userDatum_In

        !value_InvestAmount =
            if stakingIsWithoutTokenName then
                let
                    !value_Tk1     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui1")) 1
                    !value_Tk2     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui2")) 1
                    !value_Tk3     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui3")) 1
                    !value_Tk4     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui4")) 1
                    !value_Tk5     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui5")) 1
                    !value_Tk6     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui6")) 1
                    !value_Tk7     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui7")) 1
                    !value_Tk8     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui8")) 1
                    !value_Tk9     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui9")) 1

                    !value_Tk11    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui11")) 1
                    !value_Tk21    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui21")) 1
                    !value_Tk31    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui31")) 1
                    !value_Tk41    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui41")) 1
                    !value_Tk51    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui51")) 1
                    !value_Tk61    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui61")) 1
                    !value_Tk71    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui71")) 1
                    !value_Tk81    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui81")) 1
                    !value_Tk91    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui91")) 1


                    !value_Tk111   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui111")) 10
                    !value_Tk211   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui211")) 10
                    !value_Tk311   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui311")) 10
                    !value_Tk411   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui411")) 10
                    !value_Tk511   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui511")) 10
                    !value_Tk611   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui611")) 10
                    !value_Tk711   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui711")) 10
                    !value_Tk811   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui811")) 10
                    !value_Tk911   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui911")) 10

                    !value_In_User_Wallet = value_Tk1 <> value_Tk2 <> value_Tk3 <> value_Tk4 <> value_Tk5 <> value_Tk6 <> value_Tk7 <> value_Tk8 <> value_Tk9 <> value_Tk11 <> value_Tk21 <> value_Tk31 <> value_Tk41 <> value_Tk51 <> value_Tk61 <> value_Tk71 <> value_Tk81 <> value_Tk91 <> value_Tk111 <> value_Tk211 <> value_Tk311 <> value_Tk411 <> value_Tk511 <> value_Tk611 <> value_Tk711 <> value_Tk811 <> value_Tk911
                in
                    -- value_Tk1 <> value_Tk2 <> value_Tk3 <> value_Tk4 <> value_Tk5 <> value_Tk6 <> value_Tk7 <> value_Tk8 <> value_Tk9 <> value_Tk11 <> value_Tk21 <> value_Tk31 <> value_Tk41 <> value_Tk51 <> value_Tk61 <> value_Tk71 <> value_Tk81 <> value_Tk91
                    Helpers.createValueAddingTokensOfCurrencySymbol staking_AC staking_CS stakingIsWithoutTokenName value_In_User_Wallet investAmount
           else
                LedgerValue.assetClassValue staking_AC investAmount

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "value_InvestAmount: " ++ P.show value_InvestAmount
    let
        !userID_CS = T.pppCurSymbol_TxID_User_Deposit pabParams
        !userID_AC = LedgerValue.AssetClass (userID_CS, T.userID_TN)
        !value_UserID    = LedgerValue.assetClassValue userID_AC 1

        !value_UserDatum_In    =  LedgerAda.lovelaceValueOf minAda_For_UserDatum <> value_UserID <> value_InvestAmount

        ---

        !rewards = Helpers.getRewardsPerInvest (T.ppDeadline pParams) pdClosedAt  (T.ppInterestRates pParams) (T.udLastClaimAt userDatum_In) claimAt (T.udCreatedAt userDatum_In) (T.udInvest userDatum_In) (T.udRewardsNotClaimed userDatum_In)
        !totalNewRewards = rewards  + T.udRewardsNotClaimed userDatum_In
        !rewardsNotClaimed = totalNewRewards - claimAmount
        !totalRewardsCashedOut = T.udCashedOut userDatum_In + claimAmount

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Claiming: " ++ P.show claimAmount
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "New Rewards: " ++ P.show rewards
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "RewardsNotClaimed: " ++ P.show (T.udRewardsNotClaimed userDatum_In)
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "TotalNewRewards: " ++ P.show totalNewRewards
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "RewardsNotClaimed after claim: " ++ P.show rewardsNotClaimed
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "TotalRewardsCashedOut: " ++ P.show totalRewardsCashedOut

    let

        !userDatum_Out = T.mkUserDatum
            user
            (T.udInvest userDatum_In)
            (T.udCreatedAt userDatum_In)
            totalRewardsCashedOut
            rewardsNotClaimed
            (Just claimAt)
            (T.udMinAda userDatum_In)
        !userDatum_Out_Plutus = LedgerApiV2.Datum $ PlutusTx.toBuiltinData userDatum_Out

        !value_Tk41133   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui411")) 10

        !value_UserDatum_Out    =  value_UserDatum_In <> value_For_Mint_TxID_User_Harvest

        --------------------------------

        !redeemer_For_Consuming_Validator_Datum = T.mkRedeemerUserHarvest user claimAmount claimAt
        !redeemer_For_Consuming_Validator_Datum_Plutus = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData redeemer_For_Consuming_Validator_Datum

        !redeemer_For_Mint_TxID_User_Harvest = T.mkRedeemerMint_TxID redeemer_For_Consuming_Validator_Datum

        --------------------------------

        mockInputWithPoolDatum :: LedgerApiV2.TxInInfo
        mockInputWithPoolDatum =
            LedgerApiV2.TxInInfo
                T.exampleTxOutRef
                (LedgerApiV2.TxOut
                    address -- txOutAddress :: Address	 
                    value_PoolDatum_In -- txOutValue :: Value	 
                    (LedgerApiV2.OutputDatum poolDatum_In_Plutus) -- txOutDatum :: OutputDatum	
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )

        mockInputWitFundDatum :: LedgerApiV2.TxInInfo
        mockInputWitFundDatum =
            LedgerApiV2.TxInInfo
                T.exampleTxOutRef
                (LedgerApiV2.TxOut
                    address -- txOutAddress :: Address	 
                    value_FundDatum_In -- txOutValue :: Value	 
                    (LedgerApiV2.OutputDatum fundDatum_In_Plutus) -- txOutDatum :: OutputDatum	
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )

        mockOutputWitFundDatum :: LedgerApiV2.TxOut
        mockOutputWitFundDatum =
            LedgerApiV2.TxOut
                address -- txOutAddress :: Address	 
                value_FundDatum_Out -- txOutValue :: Value	 
                (LedgerApiV2.OutputDatum fundDatum_Out_Plutus) -- txOutDatum :: OutputDatum	
                DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash


        mockInputWitUserDatum :: LedgerApiV2.TxInInfo
        mockInputWitUserDatum =
            LedgerApiV2.TxInInfo
                T.exampleTxOutRef
                -- T.exampleTxOutRef1
                (LedgerApiV2.TxOut
                    T.exampleAddress -- txOutAddress :: Address	 
                    value_UserDatum_In -- txOutValue :: Value	 
                    (LedgerApiV2.OutputDatum userDatum_In_Plutus) -- txOutDatum :: OutputDatum	
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )

        mockOutputWitUserDatum :: LedgerApiV2.TxOut
        mockOutputWitUserDatum =
            LedgerApiV2.TxOut
                address -- txOutAddress :: Address	 
                value_UserDatum_Out -- txOutValue :: Value	 
                (LedgerApiV2.OutputDatum userDatum_Out_Plutus) -- txOutDatum :: OutputDatum	
                DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash

        --------------------------------

        mockTxInfoInputs :: [LedgerApiV2.TxInInfo]
        -- mockTxInfoInputs = [ mockInputWitUserDatum, mockInputWitFundDatum ]
        -- mockTxInfoInputs = [ mockInputWitUserDatum ]
        -- mockTxInfoInputs = [ mockInputWithPoolDatum ]
        mockTxInfoInputs  = [ mockInputWitUserDatum ]

        mockTxInfoReferenceInputs :: [LedgerApiV2.TxInInfo]
        -- mockTxInfoReferenceInputs = [ mockInputWithPoolDatum ]
        mockTxInfoReferenceInputs = [  ]

        mockTxInfoOutputs :: [LedgerApiV2.TxOut]
        -- mockTxInfoOutputs = [ mockOutputWitUserDatum, mockOutputWitFundDatum ]
        mockTxInfoOutputs = [ ]

        mockTxInfoFee :: LedgerValue.Value
        mockTxInfoFee = LedgerAda.lovelaceValueOf 50000

        mockTxInfoMint :: LedgerValue.Value
        mockTxInfoMint = value_For_Mint_TxID_User_Harvest

        mockTxInfoSignatories :: [LedgerApiV2.PubKeyHash]
        mockTxInfoSignatories = [user]

        -- mockScriptPurposeMint :: LedgerApiV2.ScriptPurpose    
        -- mockScriptPurposeMint = LedgerApiV2.Minting txID_User_Harvest_CS

        mockScriptPurposeSpent :: LedgerApiV2.ScriptPurpose
        mockScriptPurposeSpent = LedgerApiV2.Spending T.exampleTxOutRef

        -- mockRedeemerMint :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)  
        -- mockRedeemerMint = (mockScriptPurpose, redeemer_For_Mint_TxID_User_Harvest)

        mockRedeemerSpent :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerSpent = (mockScriptPurposeSpent, redeemer_For_Consuming_Validator_Datum_Plutus)

        mockTxInfoRedeemers :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers = LedgerApiV2.fromList
            [
                mockRedeemerSpent --,
                -- mockRedeemerMint
            ]

        mockTxInfoData :: LedgerApiV2.Map LedgerApiV2.DatumHash LedgerApiV2.Datum
        mockTxInfoData = LedgerApiV2.fromList [(LedgerApiV2.DatumHash "aaaaaa", userDatum_In_Plutus)]

        mockCtx :: LedgerApiV2.ScriptContext
        mockCtx =
            LedgerApiV2.ScriptContext
                (
                LedgerApiV2.TxInfo
                    mockTxInfoInputs -- txInfoInputs :: [TxInInfo]	
                    mockTxInfoReferenceInputs -- txInfoReferenceInputs :: [TxInInfo]
                    mockTxInfoOutputs -- txInfoOutputs :: [TxOut]	
                    mockTxInfoFee -- txInfoFee :: Value	
                    mockTxInfoMint -- txInfoMint :: Value	
                    [] -- txInfoDCert :: [DCert]	
                    (LedgerApiV2.fromList []) -- txInfoWdrl :: Map StakingCredential Integer	
                    validityRange -- txInfoValidRange :: POSIXTimeRange	
                    mockTxInfoSignatories -- txInfoSignatories :: [PubKeyHash]	
                    mockTxInfoRedeemers -- txInfoRedeemers :: Map ScriptPurpose Redeemer	
                    mockTxInfoData  -- txInfoData :: Map DatumHash Datum	
                    (LedgerApiV2.TxId "555") -- txInfoId :: TxId 
                )
                mockScriptPurposeSpent -- scriptContextPurpose :: ScriptPurpose

        !datas = [ LedgerApiV2.toData (), LedgerApiV2.toData redeemer_For_Consuming_Validator_Datum_Plutus, LedgerApiV2.toData mockCtx]

        (logout, e, size) =  Utils.evaluateScriptValidator validator datas

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Log output: " ++  P.show logout
    case e of
        Left evalErr ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Eval Error: " ++  P.show evalErr
        Right exbudget -> do
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Ex Budget: " ++  P.show exbudget
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Script size: " ++  P.show size

----------------------------------------------------------------------------------------------------

evaluate_Policy_TxID_User_Harvest :: T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
evaluate_Policy_TxID_User_Harvest pabParams = do
    let
        !pParams = T.pppPoolParams pabParams

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Generating 'User Harvest' Minting Script..."
    let
        !txID_Master_Fund_CS = T.pppCurSymbol_TxID_Master_Fund pabParams
        !txID_User_Deposit_CS = T.pppCurSymbol_TxID_User_Deposit pabParams

        !policy_TxID_User_Harvest = OnChainNFT.policy_TxID_User_Harvest pParams txID_Master_Fund_CS txID_User_Deposit_CS
        !txID_User_Harvest_CS = Utils.getCurSymbolOfPolicy policy_TxID_User_Harvest

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "txID_Master_Fund_CS: " ++ P.show txID_Master_Fund_CS
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "txID_User_Deposit_CS: " ++ P.show txID_User_Deposit_CS
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "txID_User_Harvest_CS: " ++ P.show txID_User_Harvest_CS

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Evaluating Minting Script..."
    let

        !staking_CS = T.ppStaking_CS pParams
        !staking_AC = LedgerValue.AssetClass (staking_CS, T.ppStaking_TN pParams)
        !stakingIsAda = LedgerApiV2.adaSymbol == staking_CS
        !stakingIsWithoutTokenName = not stakingIsAda && T.ppStaking_TN pParams == LedgerApiV2.TokenName emptyByteString

        !harvest_CS =  T.ppHarvest_CS pParams
        !harvest_AC = LedgerValue.AssetClass (harvest_CS, T.ppHarvest_TN pParams)
        !harvestIsAda = LedgerApiV2.adaSymbol == harvest_CS
        !haverstIsWithoutTokenName = not harvestIsAda && T.ppHarvest_TN pParams == LedgerApiV2.TokenName emptyByteString

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "staking_AC: " ++ P.show staking_AC
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "stakingIsWithoutTokenName: " ++ P.show stakingIsWithoutTokenName

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "harvest_AC: " ++ P.show harvest_AC
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "haverstIsWithoutTokenName: " ++ P.show haverstIsWithoutTokenName

    let
        -- TX DE TRANSACCION
        -- !txID_User_Harvest_CS = T.pppCurSymbol_TxID_User_Harvest pabParams
        !txID_User_Harvest_AC = LedgerValue.AssetClass (txID_User_Harvest_CS, T.txID_User_Harvest_TN)

        !value_For_Mint_TxID_User_Harvest = LedgerValue.assetClassValue txID_User_Harvest_AC 1

        !now = LedgerApiV2.POSIXTime 1000000

        !claimAmount = 10
        !claimAt = now

        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange   = Ledger.interval ( now - intervalOffset1 ) (now + intervalOffset2)

        --------------------------------

        !masterFunders = []
        !fundCount = 2
        !isClosedAt = Nothing
        !isTerminated = T.poolDatum_NotTerminated
        !isEmergency = T.poolDatum_NotEmergency
        !totalCashedOut = 0
        !minAda_For_PoolDatum = 1000
        !poolDatum_In = T.mkPoolDatumTypo masterFunders fundCount totalCashedOut isClosedAt isTerminated isEmergency minAda_For_PoolDatum
        !poolDatum_In_Plutus = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.PoolDatum poolDatum_In

        !poolID_CS = T.ppPoolID_CS pParams
        !poolID_AC = LedgerValue.AssetClass (poolID_CS, T.poolID_TN)
        !value_PoolID    = LedgerValue.assetClassValue poolID_AC 1

        !value_PoolDatum_In    = LedgerAda.lovelaceValueOf minAda_For_PoolDatum <> value_PoolID

        --------------------------------

        !fundID_CS = T.pppCurSymbol_TxID_Master_Fund pabParams
        !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)
        !value_FundID    = LedgerValue.assetClassValue fundID_AC 1

        !fundAmount1 = 5
        !minAda_For_FundDatum1 = 1000
        !cashedOut1 = 0

        !fundDatum_In1 = T.mkFundDatumTypo fundAmount1 cashedOut1 minAda_For_FundDatum1
        !fundDatum_In_Plutus1 = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.FundDatum fundDatum_In1

        !value_FundAmount1 = LedgerValue.assetClassValue harvest_AC fundAmount1
        !value_FundDatum_In1    = LedgerAda.lovelaceValueOf minAda_For_FundDatum1 <> value_FundID <> value_FundAmount1

        --- 

        !fundAmount2 = 5
        !minAda_For_FundDatum2 = 2000
        !cashedOut2 = 0

        !fundDatum_In2 = T.mkFundDatumTypo fundAmount2 cashedOut2 minAda_For_FundDatum2
        !fundDatum_In_Plutus2 = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.FundDatum fundDatum_In2

        !value_FundAmount2 = LedgerValue.assetClassValue harvest_AC fundAmount2
        !value_FundDatum_In2    = LedgerAda.lovelaceValueOf minAda_For_FundDatum2 <> value_FundID <> value_FundAmount2

        --- 

        !use1 = 5 -- !claimAmount = 10

        !fundDatum_Out1 = Helpers.mkUpdated_FundDatum_With_NewClaimRewards fundDatum_In1 use1
        !fundDatum_Out_Plutus1 = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.FundDatum fundDatum_Out1

        !valueToSubstract1 = LedgerValue.assetClassValue harvest_AC use1
        !value_FundDatum_Out1 = value_FundDatum_In1 <> negate valueToSubstract1

        --- 

        !use2 = 5 -- !claimAmount = 10

        !fundDatum_Out2 = Helpers.mkUpdated_FundDatum_With_NewClaimRewards fundDatum_In2 use2
        !fundDatum_Out_Plutus2 = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.FundDatum fundDatum_Out2

        !valueToSubstract2 = LedgerValue.assetClassValue harvest_AC use2
        !value_FundDatum_Out2 = value_FundDatum_In2 <> negate valueToSubstract2

        --------------------------------

        pdClosedAt = T.pdClosedAt poolDatum_In

        !user = T.exampleUser
        !investAmount = 20
        !createdAt = 0
        !minAda_For_UserDatum = 1000

        !userDatum_In = T.mkUserDatumTypo user investAmount createdAt 0 0 DataMaybe.Nothing minAda_For_UserDatum
        !userDatum_In_Plutus = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.UserDatum userDatum_In

        !value_InvestAmount =
            if stakingIsWithoutTokenName then
                let
                    !value_Tk1     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui1")) 1
                    !value_Tk2     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui2")) 1
                    !value_Tk3     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui3")) 1
                    !value_Tk4     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui4")) 1
                    !value_Tk5     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui5")) 1
                    !value_Tk6     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui6")) 1
                    !value_Tk7     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui7")) 1
                    !value_Tk8     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui8")) 1
                    !value_Tk9     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui9")) 1

                    !value_Tk11    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui11")) 1
                    !value_Tk21    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui21")) 1
                    !value_Tk31    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui31")) 1
                    !value_Tk41    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui41")) 1
                    !value_Tk51    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui51")) 1
                    !value_Tk61    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui61")) 1
                    !value_Tk71    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui71")) 1
                    !value_Tk81    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui81")) 1
                    !value_Tk91    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui91")) 1


                    !value_Tk111   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui111")) 1
                    !value_Tk211   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui211")) 1
                    !value_Tk311   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui311")) 1
                    !value_Tk411   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui411")) 1
                    !value_Tk511   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui511")) 1
                    !value_Tk611   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui611")) 1
                    !value_Tk711   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui711")) 1
                    !value_Tk811   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui811")) 1
                    !value_Tk911   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui911")) 1

                    !value_In_User_Wallet = value_Tk1 <> value_Tk2 <> value_Tk3 <> value_Tk4 <> value_Tk5 <> value_Tk6 <> value_Tk7 <> value_Tk8 <> value_Tk9 <> value_Tk11 <> value_Tk21 <> value_Tk31 <> value_Tk41 <> value_Tk51 <> value_Tk61 <> value_Tk71 <> value_Tk81 <> value_Tk91 <> value_Tk111 <> value_Tk211 <> value_Tk311 <> value_Tk411 <> value_Tk511 <> value_Tk611 <> value_Tk711 <> value_Tk811 <> value_Tk911
                in
                    -- value_Tk1 <> value_Tk2 <> value_Tk3 <> value_Tk4 <> value_Tk5 <> value_Tk6 <> value_Tk7 <> value_Tk8 <> value_Tk9 <> value_Tk11 <> value_Tk21 <> value_Tk31 <> value_Tk41 <> value_Tk51 <> value_Tk61 <> value_Tk71 <> value_Tk81 <> value_Tk91
                    Helpers.createValueAddingTokensOfCurrencySymbol staking_AC staking_CS stakingIsWithoutTokenName value_In_User_Wallet investAmount
           else
                LedgerValue.assetClassValue staking_AC investAmount

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "value_InvestAmount: " ++ P.show value_InvestAmount
    let
        !userID_CS = T.pppCurSymbol_TxID_User_Deposit pabParams
        !userID_AC = LedgerValue.AssetClass (userID_CS, T.userID_TN)
        !value_UserID    = LedgerValue.assetClassValue userID_AC 1

        !value_UserDatum_In    =  LedgerAda.lovelaceValueOf minAda_For_UserDatum <> value_UserID <> value_InvestAmount

        ---

        !rewards = Helpers.getRewardsPerInvest (T.ppDeadline pParams) pdClosedAt  (T.ppInterestRates pParams) (T.udLastClaimAt userDatum_In) claimAt (T.udCreatedAt userDatum_In) (T.udInvest userDatum_In) (T.udRewardsNotClaimed userDatum_In)
        !totalNewRewards = rewards  + T.udRewardsNotClaimed userDatum_In
        !rewardsNotClaimed = totalNewRewards - claimAmount
        !totalRewardsCashedOut = T.udCashedOut userDatum_In + claimAmount

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Claiming: " ++ P.show claimAmount
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "New Rewards: " ++ P.show rewards
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "RewardsNotClaimed: " ++ P.show (T.udRewardsNotClaimed userDatum_In)
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "TotalNewRewards: " ++ P.show totalNewRewards
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "RewardsNotClaimed after claim: " ++ P.show rewardsNotClaimed
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "TotalRewardsCashedOut: " ++ P.show totalRewardsCashedOut

    let

        !userDatum_Out = T.mkUserDatum
            user
            (T.udInvest userDatum_In)
            (T.udCreatedAt userDatum_In)
            totalRewardsCashedOut
            rewardsNotClaimed
            (Just claimAt)
            (T.udMinAda userDatum_In)
        !userDatum_Out_Plutus = LedgerApiV2.Datum $ PlutusTx.toBuiltinData userDatum_Out

        !value_Tk41133   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui411")) 10

        !value_UserDatum_Out    =  value_UserDatum_In <> value_For_Mint_TxID_User_Harvest

        --------------------------------

        !redeemer_For_Consuming_Validator_Datum = T.mkRedeemerUserHarvest user claimAmount claimAt
        !redeemer_For_Consuming_Validator_Datum_Plutus = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData redeemer_For_Consuming_Validator_Datum

        !redeemer_For_Consuming_Validator_Datum1 = T.mkRedeemerUserWithdraw user 
        !redeemer_For_Consuming_Validator_Datum_Plutus1 = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData redeemer_For_Consuming_Validator_Datum1

        !redeemer_For_Mint_TxID_User_Harvest = T.mkRedeemerMint_TxID redeemer_For_Consuming_Validator_Datum

        --------------------------------

        mockInputWithPoolDatum :: LedgerApiV2.TxInInfo
        mockInputWithPoolDatum =
            LedgerApiV2.TxInInfo
                T.exampleTxOutRef
                (LedgerApiV2.TxOut
                    T.exampleAddress -- txOutAddress :: Address	 
                    value_PoolDatum_In -- txOutValue :: Value	 
                    (LedgerApiV2.OutputDatum poolDatum_In_Plutus) -- txOutDatum :: OutputDatum	
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )

        mockInputWitFundDatum1 :: LedgerApiV2.TxInInfo
        mockInputWitFundDatum1 =
            LedgerApiV2.TxInInfo
                T.exampleTxOutRef
                (LedgerApiV2.TxOut
                    T.exampleAddress -- txOutAddress :: Address	 
                    value_FundDatum_In1 -- txOutValue :: Value	 
                    (LedgerApiV2.OutputDatum fundDatum_In_Plutus1) -- txOutDatum :: OutputDatum	
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )

        mockInputWitFundDatum2 :: LedgerApiV2.TxInInfo
        mockInputWitFundDatum2 =
            LedgerApiV2.TxInInfo
                T.exampleTxOutRef
                (LedgerApiV2.TxOut
                    T.exampleAddress -- txOutAddress :: Address	 
                    value_FundDatum_In2 -- txOutValue :: Value	 
                    (LedgerApiV2.OutputDatum fundDatum_In_Plutus2) -- txOutDatum :: OutputDatum	
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )

        mockOutputWitFundDatum1 :: LedgerApiV2.TxOut
        mockOutputWitFundDatum1 =
            LedgerApiV2.TxOut
                T.exampleAddress -- txOutAddress :: Address	 
                value_FundDatum_Out1 -- txOutValue :: Value	 
                (LedgerApiV2.OutputDatum fundDatum_Out_Plutus1) -- txOutDatum :: OutputDatum	
                DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash

        mockOutputWitFundDatum2 :: LedgerApiV2.TxOut
        mockOutputWitFundDatum2 =
            LedgerApiV2.TxOut
                T.exampleAddress -- txOutAddress :: Address	 
                value_FundDatum_Out2 -- txOutValue :: Value	 
                (LedgerApiV2.OutputDatum fundDatum_Out_Plutus2) -- txOutDatum :: OutputDatum	
                DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash

        mockInputWitUserDatum :: LedgerApiV2.TxInInfo
        mockInputWitUserDatum =
            LedgerApiV2.TxInInfo
                T.exampleTxOutRef
                (LedgerApiV2.TxOut
                    T.exampleAddress -- txOutAddress :: Address	 
                    value_UserDatum_In -- txOutValue :: Value	 
                    (LedgerApiV2.OutputDatum userDatum_In_Plutus) -- txOutDatum :: OutputDatum	
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )

        mockOutputWitUserDatum :: LedgerApiV2.TxOut
        mockOutputWitUserDatum =
            LedgerApiV2.TxOut
                T.exampleAddress -- txOutAddress :: Address	 
                value_UserDatum_Out -- txOutValue :: Value	 
                (LedgerApiV2.OutputDatum userDatum_Out_Plutus) -- txOutDatum :: OutputDatum	
                DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash

        --------------------------------

        mockTxInfoInputs :: [LedgerApiV2.TxInInfo]
        mockTxInfoInputs = [ mockInputWitUserDatum, mockInputWitFundDatum1, mockInputWitFundDatum2 ]

        mockTxInfoReferenceInputs :: [LedgerApiV2.TxInInfo]
        mockTxInfoReferenceInputs = [ mockInputWithPoolDatum ]

        mockTxInfoOutputs :: [LedgerApiV2.TxOut]
        mockTxInfoOutputs = [ mockOutputWitUserDatum, mockOutputWitFundDatum1, mockOutputWitFundDatum2]

        mockTxInfoFee :: LedgerValue.Value
        mockTxInfoFee = LedgerAda.lovelaceValueOf 50000

        mockTxInfoMint :: LedgerValue.Value
        mockTxInfoMint = value_For_Mint_TxID_User_Harvest

        mockTxInfoSignatories :: [LedgerApiV2.PubKeyHash]
        mockTxInfoSignatories = [user]

        mockScriptPurposeMint :: LedgerApiV2.ScriptPurpose    
        mockScriptPurposeMint = LedgerApiV2.Minting txID_User_Harvest_CS

        mockScriptPurposeSpent :: LedgerApiV2.ScriptPurpose
        mockScriptPurposeSpent = LedgerApiV2.Spending T.exampleTxOutRef

        mockRedeemerMint :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerMint = (mockScriptPurposeMint, redeemer_For_Mint_TxID_User_Harvest)

        mockRedeemerSpent :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerSpent = (mockScriptPurposeSpent, redeemer_For_Consuming_Validator_Datum_Plutus)

        mockRedeemerSpent1 :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerSpent1 = (mockScriptPurposeSpent, redeemer_For_Consuming_Validator_Datum_Plutus1)

        mockTxInfoRedeemers :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers = LedgerApiV2.fromList
            [
                mockRedeemerSpent,
                mockRedeemerSpent1,
                mockRedeemerMint
            ]

        mockTxInfoData :: LedgerApiV2.Map LedgerApiV2.DatumHash LedgerApiV2.Datum
        mockTxInfoData = LedgerApiV2.fromList [(LedgerApiV2.DatumHash "aaaaaa", userDatum_In_Plutus)]

        mockCtx :: LedgerApiV2.ScriptContext
        mockCtx =
            LedgerApiV2.ScriptContext
                (
                LedgerApiV2.TxInfo
                    mockTxInfoInputs -- txInfoInputs :: [TxInInfo]	
                    mockTxInfoReferenceInputs -- txInfoReferenceInputs :: [TxInInfo]
                    mockTxInfoOutputs -- txInfoOutputs :: [TxOut]	
                    mockTxInfoFee -- txInfoFee :: Value	
                    mockTxInfoMint -- txInfoMint :: Value	
                    [] -- txInfoDCert :: [DCert]	
                    (LedgerApiV2.fromList []) -- txInfoWdrl :: Map StakingCredential Integer	
                    validityRange -- txInfoValidRange :: POSIXTimeRange	
                    mockTxInfoSignatories -- txInfoSignatories :: [PubKeyHash]	
                    mockTxInfoRedeemers -- txInfoRedeemers :: Map ScriptPurpose Redeemer	
                    mockTxInfoData  -- txInfoData :: Map DatumHash Datum	
                    (LedgerApiV2.TxId "555") -- txInfoId :: TxId 
                )
                mockScriptPurposeMint -- scriptContextPurpose :: ScriptPurpose

        !datas = [ LedgerApiV2.toData redeemer_For_Mint_TxID_User_Harvest, LedgerApiV2.toData mockCtx]

        (logout, e, size) = Utils.evaluateScriptMint policy_TxID_User_Harvest datas

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Log output: " ++  P.show logout
    case e of
        Left evalErr ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Eval Error: " ++  P.show evalErr
        Right exbudget -> do
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Ex Budget: " ++  P.show exbudget
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Script size: " ++  P.show size

----------------------------------------------------------------------------------------------------

evaluate_Policy_TxID_User_Withdraw :: T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
evaluate_Policy_TxID_User_Withdraw pabParams = do
    let
        !pParams = T.pppPoolParams pabParams

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Generating 'User Withdraw' Minting Script..."
    let
        !txID_Master_Fund_CS = T.pppCurSymbol_TxID_Master_Fund pabParams
        !txID_User_Deposit_CS = T.pppCurSymbol_TxID_User_Deposit pabParams
        !txID_User_Harvest_CS = T.pppCurSymbol_TxID_User_Harvest pabParams

        !policy_TxID_User_Withdraw = OnChainNFT.policy_TxID_User_Withdraw pParams txID_Master_Fund_CS txID_User_Deposit_CS txID_User_Harvest_CS
        !txID_User_Withdraw_CS = Utils.getCurSymbolOfPolicy policy_TxID_User_Withdraw

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "txID_Master_Fund_CS: " ++ P.show txID_Master_Fund_CS
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "txID_User_Deposit_CS: " ++ P.show txID_User_Deposit_CS
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "txID_User_Harvest_CS: " ++ P.show txID_User_Harvest_CS
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "txID_User_Withdraw_CS: " ++ P.show txID_User_Withdraw_CS

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Evaluating Minting Script..."
    let

        !staking_CS = T.ppStaking_CS pParams
        !staking_AC = LedgerValue.AssetClass (staking_CS, T.ppStaking_TN pParams)
        !stakingIsAda = LedgerApiV2.adaSymbol == staking_CS
        !stakingIsWithoutTokenName = not stakingIsAda && T.ppStaking_TN pParams == LedgerApiV2.TokenName emptyByteString

        !harvest_CS =  T.ppHarvest_CS pParams
        !harvest_AC = LedgerValue.AssetClass (harvest_CS, T.ppHarvest_TN pParams)
        !harvestIsAda = LedgerApiV2.adaSymbol == harvest_CS
        !haverstIsWithoutTokenName = not harvestIsAda && T.ppHarvest_TN pParams == LedgerApiV2.TokenName emptyByteString

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "staking_AC: " ++ P.show staking_AC
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "stakingIsWithoutTokenName: " ++ P.show stakingIsWithoutTokenName

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "harvest_AC: " ++ P.show harvest_AC
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "haverstIsWithoutTokenName: " ++ P.show haverstIsWithoutTokenName

    let
        -- TX DE TRANSACCION
        -- !txID_User_Withdraw_CS = T.pppCurSymbol_TxID_User_Withdraw pabParams
        !txID_User_Withdraw_AC = LedgerValue.AssetClass (txID_User_Withdraw_CS, T.txID_User_Withdraw_TN)

        !value_For_Mint_TxID_User_Withdraw = LedgerValue.assetClassValue txID_User_Withdraw_AC 1

        !now = LedgerApiV2.POSIXTime 1000000

        -- !claimAmount = 10
        -- !claimAt = now

        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange   = Ledger.interval ( now - intervalOffset1 ) (now + intervalOffset2)

        --------------------------------

        !masterFunders = []
        !fundCount = 2
        !isClosedAt = Nothing
        !isTerminated = T.poolDatum_Terminated -- T.poolDatum_NotTerminated
        !isEmergency = T.poolDatum_NotEmergency
        !totalCashedOut = 0
        !minAda_For_PoolDatum = 1000
        !poolDatum_In = T.mkPoolDatumTypo masterFunders fundCount totalCashedOut isClosedAt isTerminated isEmergency minAda_For_PoolDatum
        !poolDatum_In_Plutus = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.PoolDatum poolDatum_In

        !poolID_CS = T.ppPoolID_CS pParams
        !poolID_AC = LedgerValue.AssetClass (poolID_CS, T.poolID_TN)
        !value_PoolID    = LedgerValue.assetClassValue poolID_AC 1

        !value_PoolDatum_In    = LedgerAda.lovelaceValueOf minAda_For_PoolDatum <> value_PoolID

        --------------------------------

        !fundID_CS = T.pppCurSymbol_TxID_Master_Fund pabParams
        !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)
        !value_FundID    = LedgerValue.assetClassValue fundID_AC 1

        !fundAmount1 = 5
        !minAda_For_FundDatum1 = 1000
        !cashedOut1 = 0

        !fundDatum_In1 = T.mkFundDatumTypo fundAmount1 cashedOut1 minAda_For_FundDatum1
        !fundDatum_In_Plutus1 = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.FundDatum fundDatum_In1

        !value_FundAmount1 = LedgerValue.assetClassValue harvest_AC fundAmount1
        !value_FundDatum_In1    = LedgerAda.lovelaceValueOf minAda_For_FundDatum1 <> value_FundID <> value_FundAmount1

        --- 

        -- !fundAmount2 = 5 
        -- !minAda_For_FundDatum2 = 2000
        -- !cashedOut2 = 0

        -- !fundDatum_In2 = T.mkFundDatumTypo fundAmount2 cashedOut2 minAda_For_FundDatum2
        -- !fundDatum_In_Plutus2 = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.FundDatum fundDatum_In2

        -- !value_FundAmount2 = LedgerValue.assetClassValue harvest_AC fundAmount2
        -- !value_FundDatum_In2    = LedgerAda.lovelaceValueOf minAda_For_FundDatum2 <> value_FundID <> value_FundAmount2    

        --- 

        -- !use1 = 5 -- !claimAmount = 10

        !fundDatum_Out1 = fundDatum_In1
        !fundDatum_Out_Plutus1 = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.FundDatum fundDatum_Out1

        -- !valueToSubstract1 = LedgerValue.assetClassValue harvest_AC use1
        !value_FundDatum_Out1 = value_FundDatum_In1 <> value_For_Mint_TxID_User_Withdraw -- <> negate valueToSubstract1

        --- 

        -- !use2 = 5 -- !claimAmount = 10

        -- !fundDatum_Out2 = Helpers.mkUpdated_FundDatum_With_NewClaimRewards fundDatum_In2 use2
        -- !fundDatum_Out_Plutus2 = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.FundDatum fundDatum_Out2

        -- !valueToSubstract2 = LedgerValue.assetClassValue harvest_AC use2
        -- !value_FundDatum_Out2 = value_FundDatum_In2 <> negate valueToSubstract2

        --------------------------------

        pdClosedAt = T.pdClosedAt poolDatum_In

        !user = T.exampleUser
        !investAmount = 20
        !createdAt = 0
        !minAda_For_UserDatum = 1000

        !userDatum_In = T.mkUserDatumTypo user investAmount createdAt 0 0 DataMaybe.Nothing minAda_For_UserDatum
        !userDatum_In_Plutus = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.UserDatum userDatum_In

        !value_InvestAmount =
            if stakingIsWithoutTokenName then
                let
                    !value_Tk1     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui1")) 1
                    !value_Tk2     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui2")) 1
                    !value_Tk3     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui3")) 1
                    !value_Tk4     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui4")) 1
                    !value_Tk5     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui5")) 1
                    !value_Tk6     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui6")) 1
                    !value_Tk7     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui7")) 1
                    !value_Tk8     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui8")) 1
                    !value_Tk9     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui9")) 1

                    !value_Tk11    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui11")) 1
                    !value_Tk21    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui21")) 1
                    !value_Tk31    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui31")) 1
                    !value_Tk41    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui41")) 1
                    !value_Tk51    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui51")) 1
                    !value_Tk61    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui61")) 1
                    !value_Tk71    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui71")) 1
                    !value_Tk81    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui81")) 1
                    !value_Tk91    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui91")) 1


                    !value_Tk111   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui111")) 1
                    !value_Tk211   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui211")) 1
                    !value_Tk311   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui311")) 1
                    !value_Tk411   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui411")) 1
                    !value_Tk511   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui511")) 1
                    !value_Tk611   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui611")) 1
                    !value_Tk711   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui711")) 1
                    !value_Tk811   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui811")) 1
                    !value_Tk911   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui911")) 1

                    !value_In_User_Wallet = value_Tk1 <> value_Tk2 <> value_Tk3 <> value_Tk4 <> value_Tk5 <> value_Tk6 <> value_Tk7 <> value_Tk8 <> value_Tk9 <> value_Tk11 <> value_Tk21 <> value_Tk31 <> value_Tk41 <> value_Tk51 <> value_Tk61 <> value_Tk71 <> value_Tk81 <> value_Tk91 <> value_Tk111 <> value_Tk211 <> value_Tk311 <> value_Tk411 <> value_Tk511 <> value_Tk611 <> value_Tk711 <> value_Tk811 <> value_Tk911
                in
                    -- value_Tk1 <> value_Tk2 <> value_Tk3 <> value_Tk4 <> value_Tk5 <> value_Tk6 <> value_Tk7 <> value_Tk8 <> value_Tk9 <> value_Tk11 <> value_Tk21 <> value_Tk31 <> value_Tk41 <> value_Tk51 <> value_Tk61 <> value_Tk71 <> value_Tk81 <> value_Tk91
                    Helpers.createValueAddingTokensOfCurrencySymbol staking_AC staking_CS stakingIsWithoutTokenName value_In_User_Wallet investAmount
           else
                LedgerValue.assetClassValue staking_AC investAmount

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "value_InvestAmount: " ++ P.show value_InvestAmount
    let
        !userID_CS = T.pppCurSymbol_TxID_User_Deposit pabParams
        !userID_AC = LedgerValue.AssetClass (userID_CS, T.userID_TN)
        !value_UserID    = LedgerValue.assetClassValue userID_AC 1

        !value_UserDatum_In    =  LedgerAda.lovelaceValueOf minAda_For_UserDatum <> value_UserID <> value_InvestAmount

        ---

    --     !rewards = Helpers.getRewardsPerInvest (T.ppDeadline pParams) pdClosedAt  (T.ppInterestRates pParams) (T.udLastClaimAt userDatum_In) claimAt (T.udCreatedAt userDatum_In) (T.udInvest userDatum_In) (T.udRewardsNotClaimed userDatum_In)
    --     !totalNewRewards = rewards  + T.udRewardsNotClaimed userDatum_In
    --     !rewardsNotClaimed = totalNewRewards - claimAmount
    --     !totalRewardsCashedOut = T.udCashedOut userDatum_In + claimAmount

    -- PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Claiming: " ++ P.show claimAmount
    -- PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "New Rewards: " ++ P.show rewards
    -- PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "RewardsNotClaimed: " ++ P.show (T.udRewardsNotClaimed userDatum_In)
    -- PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "TotalNewRewards: " ++ P.show totalNewRewards
    -- PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "RewardsNotClaimed after claim: " ++ P.show rewardsNotClaimed
    -- PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "TotalRewardsCashedOut: " ++ P.show totalRewardsCashedOut

    -- let 

    --     !userDatum_Out = T.mkUserDatum 
    --         user
    --         (T.udInvest userDatum_In)
    --         (T.udCreatedAt userDatum_In)
    --         totalRewardsCashedOut
    --         rewardsNotClaimed
    --         (Just claimAt)
    --         (T.udMinAda userDatum_In)
    --     !userDatum_Out_Plutus = LedgerApiV2.Datum $ PlutusTx.toBuiltinData userDatum_Out

    --     !value_Tk41133   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui411")) 10

    --     !value_UserDatum_Out    =  value_UserDatum_In <> value_For_Mint_TxID_User_Harvest

        --------------------------------

        !redeemer_For_Consuming_Validator_Datum = T.mkRedeemerUserWithdraw user
        !redeemer_For_Consuming_Validator_Datum_Plutus = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData redeemer_For_Consuming_Validator_Datum

        !redeemer_For_Mint_TxID_User_Withdraw = T.mkRedeemerMint_TxID redeemer_For_Consuming_Validator_Datum

        --------------------------------

        mockInputWithPoolDatum :: LedgerApiV2.TxInInfo
        mockInputWithPoolDatum =
            LedgerApiV2.TxInInfo
                T.exampleTxOutRef
                (LedgerApiV2.TxOut
                    T.exampleAddress -- txOutAddress :: Address	 
                    value_PoolDatum_In -- txOutValue :: Value	 
                    (LedgerApiV2.OutputDatum poolDatum_In_Plutus) -- txOutDatum :: OutputDatum	
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )

        mockInputWitFundDatum1 :: LedgerApiV2.TxInInfo
        mockInputWitFundDatum1 =
            LedgerApiV2.TxInInfo
                T.exampleTxOutRef
                (LedgerApiV2.TxOut
                    T.exampleAddress -- txOutAddress :: Address	 
                    value_FundDatum_In1 -- txOutValue :: Value	 
                    (LedgerApiV2.OutputDatum fundDatum_In_Plutus1) -- txOutDatum :: OutputDatum	
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )

        -- mockInputWitFundDatum2 :: LedgerApiV2.TxInInfo
        -- mockInputWitFundDatum2 = 
        --     LedgerApiV2.TxInInfo
        --         T.exampleTxOutRef
        --         (LedgerApiV2.TxOut
        --             T.exampleAddress -- txOutAddress :: Address	 
        --             value_FundDatum_In2 -- txOutValue :: Value	 
        --             (LedgerApiV2.OutputDatum fundDatum_In_Plutus2) -- txOutDatum :: OutputDatum	
        --             DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
        --         )

        mockOutputWitFundDatum1 :: LedgerApiV2.TxOut
        mockOutputWitFundDatum1 =
            LedgerApiV2.TxOut
                T.exampleAddress -- txOutAddress :: Address	 
                value_FundDatum_Out1 -- txOutValue :: Value	 
                (LedgerApiV2.OutputDatum fundDatum_Out_Plutus1) -- txOutDatum :: OutputDatum	
                DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash

        -- mockOutputWitFundDatum2 :: LedgerApiV2.TxOut
        -- mockOutputWitFundDatum2 = 
        --     LedgerApiV2.TxOut
        --         T.exampleAddress -- txOutAddress :: Address	 
        --         value_FundDatum_Out2 -- txOutValue :: Value	 
        --         (LedgerApiV2.OutputDatum fundDatum_Out_Plutus2) -- txOutDatum :: OutputDatum	
        --         DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash

        mockInputWitUserDatum :: LedgerApiV2.TxInInfo
        mockInputWitUserDatum =
            LedgerApiV2.TxInInfo
                T.exampleTxOutRef
                (LedgerApiV2.TxOut
                    T.exampleAddress -- txOutAddress :: Address	 
                    value_UserDatum_In -- txOutValue :: Value	 
                    (LedgerApiV2.OutputDatum userDatum_In_Plutus) -- txOutDatum :: OutputDatum	
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )

        -- mockOutputWitUserDatum :: LedgerApiV2.TxOut
        -- mockOutputWitUserDatum = 
        --     LedgerApiV2.TxOut
        --         T.exampleAddress -- txOutAddress :: Address	 
        --         value_UserDatum_Out -- txOutValue :: Value	 
        --         (LedgerApiV2.OutputDatum userDatum_Out_Plutus) -- txOutDatum :: OutputDatum	
        --         DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash

        --------------------------------

        mockTxInfoReferenceInputs :: [LedgerApiV2.TxInInfo]
        mockTxInfoReferenceInputs = [ mockInputWithPoolDatum ]

        mockTxInfoInputs :: [LedgerApiV2.TxInInfo]
        mockTxInfoInputs = [ mockInputWitUserDatum, mockInputWitFundDatum1 ]

        mockTxInfoOutputs :: [LedgerApiV2.TxOut]
        mockTxInfoOutputs = [ mockOutputWitFundDatum1 ]

        mockTxInfoFee :: LedgerValue.Value
        mockTxInfoFee = LedgerAda.lovelaceValueOf 50000

        mockTxInfoMint :: LedgerValue.Value
        mockTxInfoMint = value_For_Mint_TxID_User_Withdraw

        mockTxInfoSignatories :: [LedgerApiV2.PubKeyHash]
        mockTxInfoSignatories = [user]

        mockScriptPurposeMint :: LedgerApiV2.ScriptPurpose    
        mockScriptPurposeMint = LedgerApiV2.Minting txID_User_Withdraw_CS

        mockScriptPurposeSpent :: LedgerApiV2.ScriptPurpose
        mockScriptPurposeSpent = LedgerApiV2.Spending T.exampleTxOutRef

        mockRedeemerMint :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerMint = (mockScriptPurposeMint, redeemer_For_Mint_TxID_User_Withdraw)

        mockRedeemerSpent :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerSpent = (mockScriptPurposeSpent, redeemer_For_Consuming_Validator_Datum_Plutus)

        mockTxInfoRedeemers :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers = LedgerApiV2.fromList
            [
                mockRedeemerSpent,
                mockRedeemerMint
            ]

        mockTxInfoData :: LedgerApiV2.Map LedgerApiV2.DatumHash LedgerApiV2.Datum
        mockTxInfoData = LedgerApiV2.fromList [(LedgerApiV2.DatumHash "aaaaaa", userDatum_In_Plutus)]

        mockCtx :: LedgerApiV2.ScriptContext
        mockCtx =
            LedgerApiV2.ScriptContext
                (
                LedgerApiV2.TxInfo
                    mockTxInfoInputs -- txInfoInputs :: [TxInInfo]	
                    mockTxInfoReferenceInputs -- txInfoReferenceInputs :: [TxInInfo]
                    mockTxInfoOutputs -- txInfoOutputs :: [TxOut]	
                    mockTxInfoFee -- txInfoFee :: Value	
                    mockTxInfoMint -- txInfoMint :: Value	
                    [] -- txInfoDCert :: [DCert]	
                    (LedgerApiV2.fromList []) -- txInfoWdrl :: Map StakingCredential Integer	
                    validityRange -- txInfoValidRange :: POSIXTimeRange	
                    mockTxInfoSignatories -- txInfoSignatories :: [PubKeyHash]	
                    mockTxInfoRedeemers -- txInfoRedeemers :: Map ScriptPurpose Redeemer	
                    mockTxInfoData  -- txInfoData :: Map DatumHash Datum	
                    (LedgerApiV2.TxId "555") -- txInfoId :: TxId 
                )
                mockScriptPurposeMint -- scriptContextPurpose :: ScriptPurpose

        !datas = [ LedgerApiV2.toData redeemer_For_Mint_TxID_User_Withdraw, LedgerApiV2.toData mockCtx]

        (logout, e, size) = Utils.evaluateScriptMint policy_TxID_User_Withdraw datas

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Log output: " ++  P.show logout
    case e of
        Left evalErr ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Eval Error: " ++  P.show evalErr
        Right exbudget -> do
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Ex Budget: " ++  P.show exbudget
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Script size: " ++  P.show size

----------------------------------------------------------------------------------------------------

evaluate_Policy_TxID_Master_Emergency :: T.PABPoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
evaluate_Policy_TxID_Master_Emergency pabParams = do
    let
        !pParams = T.pppPoolParams pabParams

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Generating 'Master Emergency' Minting Script..."
    let
        !policy_TxID_Master_Emergency = OnChainNFT.policy_TxID_Master_Emergency pParams
        !txID_Master_Emergency_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_Emergency

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Evaluating Minting Script..."
    let

        !staking_CS = T.ppStaking_CS pParams
        !staking_AC = LedgerValue.AssetClass (staking_CS, T.ppStaking_TN pParams)
        !stakingIsAda = LedgerApiV2.adaSymbol == staking_CS
        !stakingIsWithoutTokenName = not stakingIsAda && T.ppStaking_TN pParams == LedgerApiV2.TokenName emptyByteString

        !harvest_CS =  T.ppHarvest_CS pParams
        !harvest_AC = LedgerValue.AssetClass (harvest_CS, T.ppHarvest_TN pParams)
        !harvestIsAda = LedgerApiV2.adaSymbol == harvest_CS
        !haverstIsWithoutTokenName = not harvestIsAda && T.ppHarvest_TN pParams == LedgerApiV2.TokenName emptyByteString

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "staking_AC: " ++ P.show staking_AC
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "stakingIsWithoutTokenName: " ++ P.show stakingIsWithoutTokenName

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "harvest_AC: " ++ P.show harvest_AC
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "haverstIsWithoutTokenName: " ++ P.show haverstIsWithoutTokenName

    let
        -- TX DE TRANSACCION
        !txID_Master_Emergency_AC = LedgerValue.AssetClass (txID_Master_Emergency_CS, T.txID_Master_Emergency_TN)
        !value_For_Mint_TxID_Master_Emergency = LedgerValue.assetClassValue txID_Master_Emergency_AC 1

        -----------------------------

        -- !master = T.exampleMaster
        !master = T.ppMasters pParams !! 0
        !masters = T.ppMasters pParams 

        -----------------------------

        !now = LedgerApiV2.POSIXTime 1000000

        !claimAmount = 10
        !claimAt = now

        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange   = Ledger.interval ( now - intervalOffset1 ) (now + intervalOffset2)

        --------------------------------

        !masterFunders = []
        !fundCount = 2
        !isClosedAt = Nothing
        !isTerminated = T.poolDatum_NotTerminated
        !isEmergency = T.poolDatum_NotEmergency
        !totalCashedOut = 0
        !minAda_For_PoolDatum = 1000
        !poolDatum_In = T.mkPoolDatumTypo masterFunders fundCount totalCashedOut isClosedAt isTerminated isEmergency minAda_For_PoolDatum
        !poolDatum_In_Plutus = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.PoolDatum poolDatum_In

        !poolID_CS = T.ppPoolID_CS pParams
        !poolID_AC = LedgerValue.AssetClass (poolID_CS, T.poolID_TN)
        !value_PoolID    = LedgerValue.assetClassValue poolID_AC 1

        !value_PoolDatum_In    = LedgerAda.lovelaceValueOf minAda_For_PoolDatum <> value_PoolID

        --------------------------------

        !fundID_CS = T.pppCurSymbol_TxID_Master_Fund pabParams
        !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)
        !value_FundID    = LedgerValue.assetClassValue fundID_AC 1

        !fundAmount1 = 5
        !minAda_For_FundDatum1 = 1000
        !cashedOut1 = 0

        !fundDatum_In1 = T.mkFundDatumTypo fundAmount1 cashedOut1 minAda_For_FundDatum1
        !fundDatum_In_Plutus1 = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.FundDatum fundDatum_In1

        !value_FundAmount1 = LedgerValue.assetClassValue harvest_AC fundAmount1
        !value_FundDatum_In1    = LedgerAda.lovelaceValueOf minAda_For_FundDatum1 <> value_FundID <> value_FundAmount1

        --- 

        !fundAmount2 = 5
        !minAda_For_FundDatum2 = 2000
        !cashedOut2 = 0

        !fundDatum_In2 = T.mkFundDatumTypo fundAmount2 cashedOut2 minAda_For_FundDatum2
        !fundDatum_In_Plutus2 = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.FundDatum fundDatum_In2

        !value_FundAmount2 = LedgerValue.assetClassValue harvest_AC fundAmount2
        !value_FundDatum_In2    = LedgerAda.lovelaceValueOf minAda_For_FundDatum2 <> value_FundID <> value_FundAmount2

        --- 

        !use1 = 5 -- !claimAmount = 10

        !fundDatum_Out1 = Helpers.mkUpdated_FundDatum_With_NewClaimRewards fundDatum_In1 use1
        !fundDatum_Out_Plutus1 = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.FundDatum fundDatum_Out1

        !valueToSubstract1 = LedgerValue.assetClassValue harvest_AC use1
        !value_FundDatum_Out1 = value_FundDatum_In1 <> negate valueToSubstract1

        --- 

        !use2 = 5 -- !claimAmount = 10

        !fundDatum_Out2 = Helpers.mkUpdated_FundDatum_With_NewClaimRewards fundDatum_In2 use2
        !fundDatum_Out_Plutus2 = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.FundDatum fundDatum_Out2

        !valueToSubstract2 = LedgerValue.assetClassValue harvest_AC use2
        !value_FundDatum_Out2 = value_FundDatum_In2 <> negate valueToSubstract2

        --------------------------------

        pdClosedAt = T.pdClosedAt poolDatum_In

        !user = T.exampleUser
        !investAmount = 20
        !createdAt = 0
        !minAda_For_UserDatum = 1000

        !userDatum_In = T.mkUserDatumTypo user investAmount createdAt 0 0 DataMaybe.Nothing minAda_For_UserDatum
        !userDatum_In_Plutus = LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.UserDatum userDatum_In

        !value_InvestAmount =
            if stakingIsWithoutTokenName then
                let
                    !value_Tk1     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui1")) 1
                    !value_Tk2     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui2")) 1
                    !value_Tk3     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui3")) 1
                    !value_Tk4     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui4")) 1
                    !value_Tk5     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui5")) 1
                    !value_Tk6     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui6")) 1
                    !value_Tk7     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui7")) 1
                    !value_Tk8     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui8")) 1
                    !value_Tk9     = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui9")) 1

                    !value_Tk11    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui11")) 1
                    !value_Tk21    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui21")) 1
                    !value_Tk31    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui31")) 1
                    !value_Tk41    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui41")) 1
                    !value_Tk51    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui51")) 1
                    !value_Tk61    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui61")) 1
                    !value_Tk71    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui71")) 1
                    !value_Tk81    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui81")) 1
                    !value_Tk91    = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui91")) 1


                    !value_Tk111   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui111")) 1
                    !value_Tk211   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui211")) 1
                    !value_Tk311   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui311")) 1
                    !value_Tk411   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui411")) 1
                    !value_Tk511   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui511")) 1
                    !value_Tk611   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui611")) 1
                    !value_Tk711   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui711")) 1
                    !value_Tk811   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui811")) 1
                    !value_Tk911   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui911")) 1

                    !value_In_User_Wallet = value_Tk1 <> value_Tk2 <> value_Tk3 <> value_Tk4 <> value_Tk5 <> value_Tk6 <> value_Tk7 <> value_Tk8 <> value_Tk9 <> value_Tk11 <> value_Tk21 <> value_Tk31 <> value_Tk41 <> value_Tk51 <> value_Tk61 <> value_Tk71 <> value_Tk81 <> value_Tk91 <> value_Tk111 <> value_Tk211 <> value_Tk311 <> value_Tk411 <> value_Tk511 <> value_Tk611 <> value_Tk711 <> value_Tk811 <> value_Tk911
                in
                    -- value_Tk1 <> value_Tk2 <> value_Tk3 <> value_Tk4 <> value_Tk5 <> value_Tk6 <> value_Tk7 <> value_Tk8 <> value_Tk9 <> value_Tk11 <> value_Tk21 <> value_Tk31 <> value_Tk41 <> value_Tk51 <> value_Tk61 <> value_Tk71 <> value_Tk81 <> value_Tk91
                    Helpers.createValueAddingTokensOfCurrencySymbol staking_AC staking_CS stakingIsWithoutTokenName value_In_User_Wallet investAmount
           else
                LedgerValue.assetClassValue staking_AC investAmount

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "value_InvestAmount: " ++ P.show value_InvestAmount
    let
        !userID_CS = T.pppCurSymbol_TxID_User_Deposit pabParams
        !userID_AC = LedgerValue.AssetClass (userID_CS, T.userID_TN)
        !value_UserID    = LedgerValue.assetClassValue userID_AC 1

        !value_UserDatum_In    =  LedgerAda.lovelaceValueOf minAda_For_UserDatum <> value_UserID <> value_InvestAmount

        ---

        !rewards = Helpers.getRewardsPerInvest (T.ppDeadline pParams) pdClosedAt  (T.ppInterestRates pParams) (T.udLastClaimAt userDatum_In) claimAt (T.udCreatedAt userDatum_In) (T.udInvest userDatum_In) (T.udRewardsNotClaimed userDatum_In)
        !totalNewRewards = rewards  + T.udRewardsNotClaimed userDatum_In
        !rewardsNotClaimed = totalNewRewards - claimAmount
        !totalRewardsCashedOut = T.udCashedOut userDatum_In + claimAmount

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Claiming: " ++ P.show claimAmount
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "New Rewards: " ++ P.show rewards
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "RewardsNotClaimed: " ++ P.show (T.udRewardsNotClaimed userDatum_In)
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "TotalNewRewards: " ++ P.show totalNewRewards
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "RewardsNotClaimed after claim: " ++ P.show rewardsNotClaimed
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "TotalRewardsCashedOut: " ++ P.show totalRewardsCashedOut

    let

        !userDatum_Out = T.mkUserDatum
            user
            (T.udInvest userDatum_In)
            (T.udCreatedAt userDatum_In)
            totalRewardsCashedOut
            rewardsNotClaimed
            (Just claimAt)
            (T.udMinAda userDatum_In)
        !userDatum_Out_Plutus = LedgerApiV2.Datum $ PlutusTx.toBuiltinData userDatum_Out

        !value_Tk41133   = LedgerValue.assetClassValue (LedgerValue.AssetClass (staking_CS, LedgerApiV2.TokenName "qwertyui411")) 10

        !value_UserDatum_Out    =  value_UserDatum_In

        --------------------------------

        !redeemer_For_Consuming_Validator_Datum = T.mkRedeemerMasterEmergency master 
        !redeemer_For_Consuming_Validator_Datum_Plutus = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData redeemer_For_Consuming_Validator_Datum

        !redeemer_For_Mint_TxID_Master_Emergency = T.mkRedeemerMint_TxID redeemer_For_Consuming_Validator_Datum

        --------------------------------

        mockInputWithPoolDatum :: LedgerApiV2.TxInInfo
        mockInputWithPoolDatum =
            LedgerApiV2.TxInInfo
                T.exampleTxOutRef
                (LedgerApiV2.TxOut
                    T.exampleAddress -- txOutAddress :: Address	 
                    value_PoolDatum_In -- txOutValue :: Value	 
                    (LedgerApiV2.OutputDatum poolDatum_In_Plutus) -- txOutDatum :: OutputDatum	
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )

        mockInputWitFundDatum1 :: LedgerApiV2.TxInInfo
        mockInputWitFundDatum1 =
            LedgerApiV2.TxInInfo
                T.exampleTxOutRef
                (LedgerApiV2.TxOut
                    T.exampleAddress -- txOutAddress :: Address	 
                    value_FundDatum_In1 -- txOutValue :: Value	 
                    (LedgerApiV2.OutputDatum fundDatum_In_Plutus1) -- txOutDatum :: OutputDatum	
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )

        mockInputWitFundDatum2 :: LedgerApiV2.TxInInfo
        mockInputWitFundDatum2 =
            LedgerApiV2.TxInInfo
                T.exampleTxOutRef
                (LedgerApiV2.TxOut
                    T.exampleAddress -- txOutAddress :: Address	 
                    value_FundDatum_In2 -- txOutValue :: Value	 
                    (LedgerApiV2.OutputDatum fundDatum_In_Plutus2) -- txOutDatum :: OutputDatum	
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )

        mockOutputWitFundDatum1 :: LedgerApiV2.TxOut
        mockOutputWitFundDatum1 =
            LedgerApiV2.TxOut
                T.exampleAddress -- txOutAddress :: Address	 
                value_FundDatum_Out1 -- txOutValue :: Value	 
                (LedgerApiV2.OutputDatum fundDatum_Out_Plutus1) -- txOutDatum :: OutputDatum	
                DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash

        mockOutputWitFundDatum2 :: LedgerApiV2.TxOut
        mockOutputWitFundDatum2 =
            LedgerApiV2.TxOut
                T.exampleAddress -- txOutAddress :: Address	 
                value_FundDatum_Out2 -- txOutValue :: Value	 
                (LedgerApiV2.OutputDatum fundDatum_Out_Plutus2) -- txOutDatum :: OutputDatum	
                DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash

        mockInputWitUserDatum :: LedgerApiV2.TxInInfo
        mockInputWitUserDatum =
            LedgerApiV2.TxInInfo
                T.exampleTxOutRef
                (LedgerApiV2.TxOut
                    T.exampleAddress -- txOutAddress :: Address	 
                    value_UserDatum_In -- txOutValue :: Value	 
                    (LedgerApiV2.OutputDatum userDatum_In_Plutus) -- txOutDatum :: OutputDatum	
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )

        mockOutputWitUserDatum :: LedgerApiV2.TxOut
        mockOutputWitUserDatum =
            LedgerApiV2.TxOut
                T.exampleAddress -- txOutAddress :: Address	 
                value_UserDatum_Out -- txOutValue :: Value	 
                (LedgerApiV2.OutputDatum userDatum_Out_Plutus) -- txOutDatum :: OutputDatum	
                DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash

        --------------------------------

        mockTxInfoInputs :: [LedgerApiV2.TxInInfo]
        mockTxInfoInputs = [ mockInputWitUserDatum, mockInputWitFundDatum1, mockInputWitFundDatum2 ]

        mockTxInfoReferenceInputs :: [LedgerApiV2.TxInInfo]
        mockTxInfoReferenceInputs = [ mockInputWithPoolDatum ]

        mockTxInfoOutputs :: [LedgerApiV2.TxOut]
        mockTxInfoOutputs = [ mockOutputWitUserDatum, mockOutputWitFundDatum1, mockOutputWitFundDatum2]

        mockTxInfoFee :: LedgerValue.Value
        mockTxInfoFee = LedgerAda.lovelaceValueOf 50000

        mockTxInfoMint :: LedgerValue.Value
        mockTxInfoMint = value_For_Mint_TxID_Master_Emergency

        mockTxInfoSignatories :: [LedgerApiV2.PubKeyHash]
        mockTxInfoSignatories = masters --[master]

        mockScriptPurposeMint :: LedgerApiV2.ScriptPurpose    
        mockScriptPurposeMint = LedgerApiV2.Minting txID_Master_Emergency_CS

        mockScriptPurposeSpent :: LedgerApiV2.ScriptPurpose
        mockScriptPurposeSpent = LedgerApiV2.Spending T.exampleTxOutRef

        mockRedeemerMint :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerMint = (mockScriptPurposeMint, redeemer_For_Mint_TxID_Master_Emergency)

        mockRedeemerSpent :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerSpent = (mockScriptPurposeSpent, redeemer_For_Consuming_Validator_Datum_Plutus)

        mockTxInfoRedeemers :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers = LedgerApiV2.fromList
            [
                mockRedeemerSpent,
                mockRedeemerMint
            ]

        mockTxInfoData :: LedgerApiV2.Map LedgerApiV2.DatumHash LedgerApiV2.Datum
        mockTxInfoData = LedgerApiV2.fromList [(LedgerApiV2.DatumHash "aaaaaa", userDatum_In_Plutus)]

        mockCtx :: LedgerApiV2.ScriptContext
        mockCtx =
            LedgerApiV2.ScriptContext
                (
                LedgerApiV2.TxInfo
                    mockTxInfoInputs -- txInfoInputs :: [TxInInfo]	
                    mockTxInfoReferenceInputs -- txInfoReferenceInputs :: [TxInInfo]
                    mockTxInfoOutputs -- txInfoOutputs :: [TxOut]	
                    mockTxInfoFee -- txInfoFee :: Value	
                    mockTxInfoMint -- txInfoMint :: Value	
                    [] -- txInfoDCert :: [DCert]	
                    (LedgerApiV2.fromList []) -- txInfoWdrl :: Map StakingCredential Integer	
                    validityRange -- txInfoValidRange :: POSIXTimeRange	
                    mockTxInfoSignatories -- txInfoSignatories :: [PubKeyHash]	
                    mockTxInfoRedeemers -- txInfoRedeemers :: Map ScriptPurpose Redeemer	
                    mockTxInfoData  -- txInfoData :: Map DatumHash Datum	
                    (LedgerApiV2.TxId "555") -- txInfoId :: TxId 
                )
                mockScriptPurposeMint -- scriptContextPurpose :: ScriptPurpose

        !datas = [ LedgerApiV2.toData redeemer_For_Mint_TxID_Master_Emergency, LedgerApiV2.toData mockCtx]

        (logout, e, size) = Utils.evaluateScriptMint policy_TxID_Master_Emergency datas

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Log output: " ++  P.show logout
    case e of
        Left evalErr ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Eval Error: " ++  P.show evalErr
        Right exbudget -> do
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Ex Budget: " ++  P.show exbudget
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "Script size: " ++  P.show size

----------------------------------------------------------------------------------------------------
