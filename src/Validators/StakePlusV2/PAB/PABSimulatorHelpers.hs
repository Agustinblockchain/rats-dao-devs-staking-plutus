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
-- {-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE TupleSections              #-}
-- {-# LANGUAGE AllowAmbiguousTypes        #-}
-- {-# LANGUAGE NumericUnderscores         #-}
-- {-# LANGUAGE QuasiQuotes                #-}
{- HLINT ignore "Use camelCase" -}
-- {-# LANGUAGE BangPatterns #-}
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
import qualified Data.Time.Clock                                        as DataTimeClock (secondsToNominalDiffTime)
import qualified Data.Time.Clock.POSIX                                  as DataTimeClockPOSIX (posixSecondsToUTCTime)
import qualified Data.Time.Format                                       as DataTimeFormat (defaultTimeLocale, formatTime)
import qualified Ledger
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
import qualified Validators.StakePlusV2.Helpers                         as Helpers
import qualified Validators.StakePlusV2.PAB.PAB                             as PAB
import qualified Validators.StakePlusV2.Types.PABParams                 as T
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
        !uTxOs = [(txOutRef, txOut)  | (txOutRef, txOut)    <- DataMap.toList  unspentOutputList, Ledger.txOutAddress  txOut == addr]
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

getAmount :: P.String -> Ledger.AssetClass -> Integer -> MonadFreerInternal.Eff
                  (PABCore.PABEffects
                     (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)
                     (PABSimulator.SimulatorState
                        (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)))
                  Integer
getAmount unit_UI unit_AC minAmount = do
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
        -- (entity, value) <-  DataMap.toList balances'
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
                        ] | walletNro <-  [1..walletCount]
            ]

    mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) formatWallets

    case pabPoolParams' of
        Just pabPoolParams ->
            mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))
                ["Script: " ++ P.show (T.pppValidatorHash pabPoolParams) ++ " " ++  P.show value | (entity, value) <-  DataMap.toList balances', fromScript pabPoolParams entity ]

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
                    ("Datum: " ++  datumFrom uTxOut) : formatValues uTxORef | (uTxORef, uTxOut) <-  uTxOutsWithAC
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
            | (uTxORef, uTxOut) <-  uTxOutsWithAC ]

        formatSelected :: [(Integer, Ledger.TxOutRef)] -> [P.String]
        formatSelected opciones' = concat [  ["----------------", P.show numOpcion, P.show uTxORef] | (numOpcion, uTxORef) <-  opciones' ]

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
                ] | walletNro <-  [1..walletCount]
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
                ] | walletNro <-  [1..walletCount]
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