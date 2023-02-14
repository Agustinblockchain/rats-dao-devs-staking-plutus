{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
{- HLINT ignore "Use camelCase" -}
------------------------------------------------------------------------------------------
module Deploy where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Cardano.Api as CardanoApi
-- import qualified Cardano.Api.Shelley                                 as ApiShelley
-- import qualified Cardano.Ledger.Alonzo                             as CardanoLedgerAlonzo (AlonzoEra)
-- import qualified Cardano.Ledger.Alonzo.Language                as CardanoLedgerAlonzoLanguage    (Language (..))
-- import qualified Cardano.Ledger.Alonzo.Scripts             as CardanoLedgerAlonzoScripts (ExUnits (..), Script (..))
-- import qualified Cardano.Crypto.Hash.Class                     as CryptoHashClass (hashToBytes)
-- import qualified Cardano.Ledger.BaseTypes                        as LedgerBaseTypes (certIxToInt, txIxToInt)
-- import qualified Cardano.Ledger.Credential                     as LedgerCredential
-- import qualified Cardano.Ledger.Crypto                             as LedgerCrypto (StandardCrypto)
-- import qualified Cardano.Ledger.Hashes                             as LedgerHashes (ScriptHash (..))
-- import qualified Cardano.Ledger.Keys                                 as LedgerKeys (KeyHash (..))
-- import qualified Codec.Serialise                  as CodecSerialise (serialise)
-- import qualified Data.Aeson                             as DataAeson (decode) --, encode
-- import qualified Data.ByteString                         as DataByteString
-- import qualified Data.ByteString.Lazy                                as DataByteStringLazy
-- import qualified Data.ByteString.Short                             as SBS
-- import qualified Data.ByteString.Char8                             as DataByteStringChar8
import qualified Data.Maybe                                  as DataMaybe (fromJust) --,fromMaybe,
-- import qualified Data.List                                     as DataList
import qualified Data.List.Split                         as DataListSplit
-- import qualified Data.Map                                 as DataMap
-- import qualified Data.String                          as DataString (IsString (fromString))
-- import qualified Data.Text                              as DataText (pack, Text) --,
-- import qualified Data.Text.Internal.Search                     as DataTextSearch
import qualified Data.Time                                     as DataTime
import qualified Data.Time.Clock.POSIX             as DataTimeClockPOSIX (getPOSIXTime)
-- import qualified Ledger                                                            
import qualified Ledger.Address                          as LedgerAddress (Address)
-- import qualified Ledger.Bytes                         as LedgerBytes (LedgerBytes(LedgerBytes), fromHex)
import qualified Ledger.Value                              as LedgerValue
-- import qualified Network.Curl                         as NetworkCurl
-- import qualified Network.Curl.Aeson             as NetworkCurlAeson
-- import qualified Plutus.V1.Ledger.Scripts                             as LedgerScriptsV1
import qualified Plutus.V2.Ledger.Api              as LedgerApiV2
-- import qualified Plutus.V2.Ledger.Credential                 as LedgerCredentialV2
-- import qualified Plutus.V2.Ledger.Crypto                         as LedgerCryptoV2
--import qualified Plutus.V2.Ledger.EvaluationContext              as LedgerEvaluationContextV2
-- import qualified Plutus.V2.Ledger.Scripts                        as LedgerScriptsV2
-- import qualified Plutus.V2.Ledger.Value                            as LedgerValueV2 (TokenName (..))
-- import qualified PlutusTx                                                            
import qualified PlutusTx.Builtins.Class                                as TxBuiltinsClass
-- import qualified PlutusTx.Builtins              as TxBuiltins
-- import qualified PlutusTx.Builtins.Internal                    as TxBuiltinsInternal (BuiltinByteString (..))
import                     PlutusTx.Prelude                      hiding (unless)
import qualified System.Directory                      as SystemDirectory
-- import qualified System.Environment             as SystemEnvironment (lookupEnv)
import qualified System.FilePath.Posix             as SystemFilePathPosix
-- import qualified Text.RE.Replace                  as TextREReplace
-- import qualified Text.RE.TDFA.String                                 as TextRETDFAString
import qualified Text.Read                                     as TextRead (readMaybe)
import qualified Text.Hex                                      as TextHex
-- import qualified Wallet.Emulator.Wallet                            as WalletEmulator            (WalletId (..)) --, Wallet (..)
-- import qualified Wallet.Types                         as WalletTypes (ContractInstanceId (..))
import qualified Prelude                                         as P
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
-- import qualified Validators.StakePlusV2.Helpers                               as Helpers
import qualified Validators.StakePlusV2.OnChain.Core.Validator                       as OnChain
-- import qualified Validators.StakePlusV2.OnChain.TestInputsOutputs.Policy              as OnChainTestInputsOutputs
import qualified Validators.StakePlusV2.OnChain.Tokens.PoolID.Policy                        as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.Fund                        as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.FundAndMerge        as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.SplitFund                     as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.ClosePool                     as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.TerminatePool             as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.Emergency             as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.DeleteFund                    as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.SendBackFund        as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.SendBackDeposit    as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.AddScripts            as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.DeleteScripts     as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.UserActions.Deposit                        as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.UserActions.Harvest                as OnChainNFT
import qualified Validators.StakePlusV2.OnChain.Tokens.TxID.UserActions.Withdraw         as OnChainNFT
import qualified Validators.StakePlusV2.Types.Constants                                                                     as T
import qualified Validators.StakePlusV2.Types.DatumsValidator                                                         as T
import qualified Validators.StakePlusV2.Types.Examples                                                                        as T
import qualified Validators.StakePlusV2.Types.PABParams                                                                     as T
import qualified Validators.StakePlusV2.Types.RedeemersMint                                                             as T
import qualified Validators.StakePlusV2.Types.RedeemersValidator                                                    as T
import qualified Validators.StakePlusV2.Types.Types                                                                             as T
import qualified Utils
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
-- lee archivos exportados con Utils.writeEncodedToFile
-- readFileDecodedAsDatumValidator "/home/manuelpadilla/source/copyRepos/RATS-DAO/cardano-devs-scripts/files/validators/V2/StakePlusV2/FundDatum-HEX.json"
-- contents of file: {"getDatum":"d87a9fd8799fd8799f581c9925b87688572c90085d2a6bcaa7e8f4d1e631fc18a4439ea998ce3f5820d793c0edc6b088fbd76e749658a27a7cad242ec4fb63f9547bffccf2bfcbda41ff9fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e1a002dc96600ffff800000ffff"}
readFileDecodedAsDatumValidator :: P.String -> P.IO T.DatumValidator
readFileDecodedAsDatumValidator filepath = do
    !raw <- Utils.readFileDecodedAsDatum filepath
    P.putStrLn $ "Raw: " ++ P.show raw
    let !datumValidator = LedgerApiV2.unsafeFromBuiltinData @T.DatumValidator (LedgerApiV2.getDatum raw)
    P.putStrLn $ "Result: " ++ P.show datumValidator
    return datumValidator

-- lee archivos exportados con Utils.writePlutusDataToFile
-- readFileToPlutusDataAsDatumValidator "/home/manuelpadilla/source/copyRepos/RATS-DAO/cardano-devs-scripts/files/validators/V2/StakePlusV2/FundDatum.json"
readFileToPlutusDataAsDatumValidator :: P.String -> P.IO T.DatumValidator
readFileToPlutusDataAsDatumValidator filepath = do
    !raw <- Utils.readFileToPlutusData filepath
    P.putStrLn $ "Raw: " ++ P.show raw
    let !datumValidator = LedgerApiV2.unsafeFromBuiltinData @T.DatumValidator (LedgerApiV2.BuiltinData raw)
    P.putStrLn $ "Result: " ++ P.show datumValidator
    return datumValidator

----------------------------------------------------------------------------------------

-- lee strings cbor
-- readEncodedStringAsDatumOrRedemeer "DatumValidator" "{\"getDatum\":\"d8799fd8799f9fd8799f424353444d616e75ffffffff\"}"
-- readEncodedStringAsDatumOrRedemeer "DatumValidator" "{\"getDatum\":\"d87a9fd8799fd8799f424353444d616e75ff9fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e1a002dc6c01a002dc6c0ffff9fd8799f424353444d616e75ffff1a01c9c38000ffff\"}"
-- readEncodedStringAsDatumOrRedemeer "DatumValidator" "{\"getDatum\":\"d87a9fd8799fd8799f581c9925b87688572c90085d2a6bcaa7e8f4d1e631fc18a4439ea998ce3f5820d793c0edc6b088fbd76e749658a27a7cad242ec4fb63f9547bffccf2bfcbda41ff9fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e1a002dc96600ffff800000ffff\"}"
-- readEncodedStringAsDatumOrRedemeer "DatumValidator" "{\"getDatum\":\"d87b9fd8799fd8799f424353444d616e75ff581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e1a01c9c3801b0000018212c5d3f81a01c9c3801a01c9c380d8799f1b0000018212c5d3f8ffffff\"}"

-- readEncodedStringAsDatumOrRedemeer "RedeemerValidator" "{\"getRedeemer\":\"d87a9fd8799fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16ed8799fd8799f43aaccffff0affffffff\"}"
-- readEncodedStringAsDatumOrRedemeer "Redeemer_TxID" "{\"getRedeemer\":\"d8799fd8799fd8799fd8799f424353444d616e75ff9f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16eff1b0000018212c5d3f89fd8799fd8799f185aff01ffd8799fd8799f18b4ff02ffd8799fd87a8003ffff1a01c9c380424353ffd87a9fd8799fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16ed8799fd8799f43aaccffff0affffffffffff\"}"
-- readEncodedStringAsDatumOrRedemeer "RedeemerNFTFundAndUserID" "{\"getRedeemer\":\"d8799fd8799f40d8799fd8799f43aaafffff00ffffff\"}"
-- readEncodedStringAsDatumOrRedemeer "RedeemerNFTFundAndUserID" "{\"getRedeemer\":\"d87a9fd8799f444d616e75ffff\"}"

-- readEncodedStringAsDatumOrRedemeer "Redeemer_TxID" "{\"getRedeemer\":\"d8799fd8799fd8799fd8799f581c47d39eec62a0069c145d691757d2dec20d879a949e0f603b9f3035a346506f6f6c4944ff9f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16eff1b000001835d3453f09fd8799f185a01ffd8799f18b402ffd8799f19270f03ffff1a02faf080581c14f792644b7ce8e294ef44826e6017aee704f9d5f2bfa8b83ce9d38effd8799fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16ed8799fd8799f582025d3b9007a5ab81ef517aea2448dccddd6b494d8404ff000cd23c508e8762c79ff00ffffffffff\"}"

readEncodedStringAsDatumOrRedemeer :: P.String -> P.String -> P.IO ()
readEncodedStringAsDatumOrRedemeer typeToDecode stringCbor = do
    case typeToDecode of
        "DatumValidator" -> do
            !raw <- Utils.readStringDecodedAsDatum stringCbor
            P.putStrLn $ "Raw: " ++ P.show raw
            let !result = LedgerApiV2.unsafeFromBuiltinData @T.DatumValidator (LedgerApiV2.getDatum raw)
            P.putStrLn $ "Result: " ++ P.show result
        _ -> do
            !raw <- Utils.readStringDecodedAsRedeemer stringCbor
            P.putStrLn $ "Raw: " ++ P.show raw
            case typeToDecode of
                "RedeemerValidator" -> do
                    let !result = LedgerApiV2.unsafeFromBuiltinData @T.RedeemerValidator (LedgerApiV2.getRedeemer raw)
                    P.putStrLn $ "Result: " ++ P.show result
                "Redeemer_TxID" -> do
                    let !result = LedgerApiV2.unsafeFromBuiltinData @T.Redeemer_TxID (LedgerApiV2.getRedeemer raw)
                    P.putStrLn $ "Result: " ++ P.show result
                _ -> P.putStrLn "Invalid type to decode"
    return ()

---

readEncodedStringAsDatum :: P.String -> P.IO T.DatumValidator
readEncodedStringAsDatum    stringCbor = do
    !raw <- Utils.readStringDecodedAsDatum stringCbor
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @T.DatumValidator (LedgerApiV2.getDatum raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

------------------------------------------------------------------------------------------

writeValidator :: LedgerApiV2.Validator -> P.String -> P.String -> P.IO (Either (CardanoApi.FileError ()) ())
writeValidator validator path file    = do
    Utils.writeValidator path (file ++ ".plutus") validator

writeValidatorHash :: LedgerApiV2.ValidatorHash -> P.String -> P.String    -> P.IO ()
writeValidatorHash hash path file    = do
    Utils.writePlutusDataToFile (path SystemFilePathPosix.</> file ++ ".hash") hash

writeValidatorAddress :: LedgerAddress.Address -> P.String -> P.String    -> P.IO ()
writeValidatorAddress address path file    = do
    _ <- Utils.writeEncodedToFile (path SystemFilePathPosix.</> file ++ "-HEX.addr") address
    _ <- Utils.writeFile (path SystemFilePathPosix.</> file ++ "-Mainnet.addr") $ Utils.validatorAddrToAddrBech32Mainnet address
    _ <- Utils.writeFile (path SystemFilePathPosix.</> file ++ "-Testnet.addr") $ Utils.validatorAddrToAddrBech32Testnet address
    P.putStrLn $ "Addr: " ++ P.show address

----------------------------------------------------------------------------------------------------

writeMintingPolicy :: LedgerApiV2.MintingPolicy -> LedgerApiV2.CurrencySymbol -> P.String -> P.String -> P.IO ()
writeMintingPolicy policy curSymbol path file    = do
    _ <- Utils.writeMintingPolicy path (file ++ ".plutus") policy
    Utils.writePlutusDataToFile (path SystemFilePathPosix.</> file ++ ".symbol") curSymbol


----------------------------------------------------------------------------------------------------

-- evaluateScriptStakePlus :: P.IO ()
-- evaluateScriptStakePlus = do
--     let
--         pParams = T.examplePoolParams
--         codeValidator = OnChain.codeValidator pParams
--         !datas = []
--         (logout, e, size) = Utils.evaluateScriptValidator codeValidator datas

--     P.print ("Log output" :: P.String) >> P.print logout
--     case e of
--         Left evalErr -> P.print ("Eval Error" :: P.String) >> P.print evalErr
--         Right exbudget -> do
--             P.print ("Ex Budget" :: P.String) >> P.print exbudget
--             P.print ("Script size " :: P.String) >> P.print size

----------------------------------------------------------------------------------------------------

evaluateScriptMint_Master_Fund :: P.IO ()
evaluateScriptMint_Master_Fund = do
    let
        !pParams = T.examplePoolParams
        !mintingPolicy = OnChainNFT.policy_TxID_Master_Fund pParams
        !datas = []
        (logout, e, size) =  Utils.evaluateScriptMint mintingPolicy datas

    P.print ("Log output" :: P.String) >> P.print logout
    case e of
        Left evalErr -> P.print ("Eval Error" :: P.String) >> P.print evalErr
        Right exbudget -> do
            P.print ("Ex Budget" :: P.String) >> P.print exbudget
            P.print ("Script size " :: P.String) >> P.print size

------------------------------------------------------------------------------------------

writeEstado :: P.String -> P.String -> P.String -> P.IO ()
writeEstado basePathFiles nombrePool estadoStr = do
    let
        !estado = T.Estado { T.getEstado = estadoStr }
    Utils.writeEncodedToFile (basePathFiles SystemFilePathPosix.</> nombrePool SystemFilePathPosix.</> "estado.json") estado

------------------------------------------------------------------------------------------

-- exportarPoolParamsYScripts "RATSA3ss" "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e,44567883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e" "350bf24c053e3dc7d0a64f5fc56b6d7bebfefc647955633fdbb0013411f0fc48#6" "1666329787226" "1666761787000" "lovelace" "lovelace" "525600525600" "/home/manuelpadilla/source/copyRepos/RATS-DAO/rats-dao-devs-staking-frontend-v2.2/public/scripts"
exportarPoolParamsYScripts :: P.String -> P.String -> P.String -> P.String -> P.String -> P.String -> P.String -> P.String -> P.String -> P.String -> P.String -> P.String ->    P.String ->    P.FilePath -> P.IO ()
exportarPoolParamsYScripts nombrePool mastersStr uTxOutRefStr beginAtPoolStr deadlinePoolStr graceTimeStr staking_UI staking_CS_Str staking_TN_Str harvest_UI harvest_CS_Str harvest_TN_Str interestStr basePathFiles = do

    SystemDirectory.removePathForcibly (basePathFiles SystemFilePathPosix.</> nombrePool )
    SystemDirectory.createDirectoryIfMissing True (basePathFiles SystemFilePathPosix.</> nombrePool )

    writeEstado basePathFiles nombrePool "Starting..."
    P.putStrLn "Starting..."

    let
        isHexOk hex =
            case hex of
                    Nothing -> False
                    _             -> True

    let
        !mastersString = DataListSplit.splitOn "," mastersStr

        getMaster_Str_Ok masterStr' =
            if length masterStr' P.== 56 then
                 masterStr'
            else
                P.error $ "Invalid Master Lenght: " ++ P.show masterStr'

        !mastersStrOK = getMaster_Str_Ok <$> mastersString

        !mastersHex = TextHex.decodeHex . Utils.stringToStrictText <$> mastersStrOK

        getMaster_Hex_Ok masterHex' =
            if isHexOk masterHex' then
                DataMaybe.fromJust masterHex'
            else
                P.error $ "Invalid Master Hex: " ++ P.show masterHex'

        !mastersHexOK = getMaster_Hex_Ok <$> mastersHex
        -- masterHexOK = [DataMaybe.fromJust masterHex | masterHex <- mastersHex, isHexOk masterHex ]
        !masters =    LedgerApiV2.PubKeyHash . TxBuiltinsClass.toBuiltin <$> mastersHexOK
    P.putStrLn $ "Masters: " ++ P.show masters

    !now' <- DataTimeClockPOSIX.getPOSIXTime
    let
        !convertPosixTime    = P.floor . (1e3 P.*) .    DataTime.nominalDiffTimeToSeconds 
        !now = convertPosixTime now' :: Integer
    P.putStrLn $ "Now: " ++ P.show now

    let

        !beginAtPool =
                case TextRead.readMaybe beginAtPoolStr :: Maybe Integer of
                    Just x ->
                        -- if x >= now then
                            LedgerApiV2.POSIXTime x
                        -- else
                        --     P.error $ "Invalid Begin At, must be greater than Now: " ++ P.show now ++ ", Begin At: " ++ P.show x
                    _ -> P.error $ "Invalid Begin At, must be a number: " ++ P.show beginAtPoolStr

        !deadlinePool =
                case TextRead.readMaybe deadlinePoolStr :: Maybe Integer of
                        Just x ->
                            if LedgerApiV2.POSIXTime x >= beginAtPool then
                                LedgerApiV2.POSIXTime x
                            else
                                P.error $ "Invalid Deadline, must be greater than Begin At: " ++ P.show beginAtPool ++ ", Deadline: " ++ P.show x
                        _ -> P.error $ "Invalid Deadline, must be a number: " ++ P.show deadlinePoolStr

        !graceTime =
                case TextRead.readMaybe graceTimeStr :: Maybe Integer of
                        Just x -> 
                            if x >=0 then 
                                LedgerApiV2.POSIXTime x 
                            else 
                                P.error $ "Invalid GraceTime, must be greater than 0: " ++ P.show x
                        _ -> P.error $ "Invalid GraceTime, must be a number: " ++ P.show graceTimeStr

        getStaking_CS_Str_Ok cs_Str' =
            if length cs_Str' P.== 56 then
                 cs_Str'
            else
                P.error $ "Invalid Staking Currency Symbol Lenght: " ++ P.show cs_Str'

        getStaking_CS_Hex_Ok cs' =
            if isHexOk cs' then
                DataMaybe.fromJust cs'
            else
                P.error $ "Invalid Staking Currency Symbol Hex: " ++ P.show cs'

        getHarvest_CS_Str_Ok cs_Str' =
            if length cs_Str' P.== 56 then
                 cs_Str'
            else
                P.error $ "Invalid Harvest Currency Symbol Lenght: " ++ P.show cs_Str'

        getHarvest_CS_Hex_Ok cs' =
            if isHexOk cs' then
                DataMaybe.fromJust cs'
            else
                P.error $ "Invalid Harvest Currency Symbol Hex: " ++ P.show cs'

        !(staking_CS, staking_TN)    =
            case staking_CS_Str of
                    "lovelace"    ->    (LedgerApiV2.adaSymbol,    LedgerApiV2.adaToken)
                    ""                    ->    (LedgerApiV2.adaSymbol,    LedgerApiV2.adaToken)
                    _                     ->
                        let
                            staking_CS_Str_Ok = getStaking_CS_Str_Ok staking_CS_Str
                            staking_CS_Hex = TextHex.decodeHex $ Utils.stringToStrictText staking_CS_Str_Ok
                            staking_CS' = LedgerApiV2.CurrencySymbol $ TxBuiltinsClass.toBuiltin $ getStaking_CS_Hex_Ok staking_CS_Hex

                            staking_TN_Hex = TextHex.decodeHex $ Utils.stringToStrictText staking_TN_Str
                            staking_TN' = 
                                if isHexOk staking_TN_Hex then 
                                    LedgerApiV2.TokenName $ TxBuiltinsClass.toBuiltin $ DataMaybe.fromJust staking_TN_Hex 
                                else 
                                    P.error $ "Invalid Staking TokenName Hex: " ++ P.show staking_TN_Hex
                        in
                            ( staking_CS' , staking_TN' )

    P.putStrLn $ "Staking UI: " ++ P.show staking_UI
    P.putStrLn $ "Staking CS: " ++ P.show staking_CS
    P.putStrLn $ "Staking TN: " ++ P.show staking_TN

    let
        !(harvest_CS, harvest_TN)    =
            case harvest_CS_Str of
                    "lovelace"    ->    (LedgerApiV2.adaSymbol, LedgerApiV2.adaToken)
                    ""                    ->    (LedgerApiV2.adaSymbol, LedgerApiV2.adaToken)
                    _                     ->
                        let
                            harvest_CS_Str_Ok = getHarvest_CS_Str_Ok harvest_CS_Str
                            harvest_CS_Hex = TextHex.decodeHex $ Utils.stringToStrictText harvest_CS_Str_Ok
                            harvest_CS' = LedgerApiV2.CurrencySymbol $ TxBuiltinsClass.toBuiltin $ getHarvest_CS_Hex_Ok harvest_CS_Hex

                            harvest_TN_Str_Ok = 
                                if length harvest_TN_Str P.> 0 then
                                    harvest_TN_Str
                                else
                                   P.error "Invalid Harvest TokenName Lenght, can't be empty"

                            harvest_TN_Hex = TextHex.decodeHex $ Utils.stringToStrictText harvest_TN_Str_Ok
                            harvest_TN' =
                                if isHexOk harvest_TN_Hex then
                                    LedgerApiV2.TokenName $ TxBuiltinsClass.toBuiltin $ DataMaybe.fromJust harvest_TN_Hex
                                else
                                    P.error "Invalid Harvest TokenName Hex"
                        in
                            ( harvest_CS' , harvest_TN' )

    P.putStrLn $ "Harvest UI: " ++ P.show harvest_UI
    P.putStrLn $ "Harvest CS: " ++ P.show harvest_CS
    P.putStrLn $ "Harvest TN: " ++ P.show harvest_TN

    let
        !interest =
                    case TextRead.readMaybe interestStr :: Maybe Integer of
                            Just x -> if x >=0 then x else P.error "Invalid Interest"
                            _ -> P.error "Invalid Interest"

    let
        !poolID_TxOutRef = Utils.unsafeReadTxOutRef uTxOutRefStr

    writeEstado basePathFiles nombrePool "Creating Minting Policy PoolID..."
    P.putStrLn "Creating Minting Policy PoolID..."
    let
        !policy_PoolID = OnChainNFT.policy_PoolID masters poolID_TxOutRef
        !curSymbol_PoolID = Utils.getCurSymbolOfPolicy policy_PoolID
    _ <- writeMintingPolicy policy_PoolID curSymbol_PoolID (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_PoolID"

    let
        !poolID_CS = curSymbol_PoolID
        !poolID_AC = LedgerValue.AssetClass ( poolID_CS, T.poolID_TN)

        !pParams =
            T.PoolParams
                {
                    T.ppPoolID_CS = poolID_CS,
                    T.ppMasters = masters,
                    T.ppBeginAt = beginAtPool,
                    T.ppDeadline = deadlinePool,
                    T.ppGraceTime = graceTime,
                    T.ppStaking_CS = staking_CS,
                    T.ppStaking_TN = staking_TN,
                    T.ppHarvest_CS = harvest_CS,
                    T.ppHarvest_TN = harvest_TN,
                    --T.ppInterestRates = [T.InterestRate {T.iMinDays = Just 90, T.iPercentage = 1}, T.InterestRate {T.iMinDays = Just 180, T.iPercentage = 2}, T.InterestRate {T.iMinDays = Nothing, T.iPercentage = 3}],
                    T.ppInterestRates = [T.InterestRate {T.iMinDays = Nothing, T.iPercentage = interest}]
                }

    P.putStrLn $ "PoolID TxOutRef: " ++ P.show poolID_TxOutRef
    P.putStrLn $ "PoolID TN: " ++ P.show T.poolID_TN
    P.putStrLn $ "PoolID CS: " ++ P.show poolID_CS
    P.putStrLn $ "PoolID AC: " ++ P.show poolID_AC

    writeEstado basePathFiles nombrePool "Generating 'Master Fund' Minting Script..."
    P.putStrLn "Generating 'Master Fund' Minting Script..."
    let
        !policy_TxID_Master_Fund = OnChainNFT.policy_TxID_Master_Fund pParams
        !txID_Master_Fund_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_Fund
    _ <- writeMintingPolicy policy_TxID_Master_Fund txID_Master_Fund_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_Fund"

    writeEstado basePathFiles nombrePool "Generating 'User Deposit' Minting Script..."
    P.putStrLn "Generating 'User Deposit' Minting Script..."
    let
        !policy_TxID_User_Deposit = OnChainNFT.policy_TxID_User_Deposit pParams
        !txID_User_Deposit_CS = Utils.getCurSymbolOfPolicy policy_TxID_User_Deposit
    _ <- writeMintingPolicy policy_TxID_User_Deposit txID_User_Deposit_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_User_Deposit"
    
    writeEstado basePathFiles nombrePool "Generating 'User Harvest' Minting Script..."
    P.putStrLn "Generating 'User Harvest' Minting Script..."
    let
        !policy_TxID_User_Harvest = OnChainNFT.policy_TxID_User_Harvest pParams txID_Master_Fund_CS txID_User_Deposit_CS
        !txID_User_Harvest_CS = Utils.getCurSymbolOfPolicy policy_TxID_User_Harvest
    _ <- writeMintingPolicy policy_TxID_User_Harvest txID_User_Harvest_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_User_Harvest"

    writeEstado basePathFiles nombrePool "Generating 'Master Fund And Merge' Minting Script..."
    P.putStrLn "Generating 'Master Fund And Merge' Minting Script..."
    let
        !policy_TxID_Master_FundAndMerge = OnChainNFT.policy_TxID_Master_FundAndMerge pParams txID_Master_Fund_CS
        !txID_Master_FundAndMerge_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_FundAndMerge
    _ <- writeMintingPolicy policy_TxID_Master_FundAndMerge txID_Master_FundAndMerge_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_FundAndMerge"

    writeEstado basePathFiles nombrePool "Generating 'Master Split Fund' Minting Script..."
    P.putStrLn "Generating 'Master Split Fund' Minting Script..."
    let
        !policy_TxID_Master_SplitFund = OnChainNFT.policy_TxID_Master_SplitFund pParams txID_Master_Fund_CS
        !txID_Master_SplitFund_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_SplitFund
    _ <- writeMintingPolicy policy_TxID_Master_SplitFund txID_Master_SplitFund_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_SplitFund"

    writeEstado basePathFiles nombrePool "Generating 'Master Close Pool' Minting Script..."
    P.putStrLn "Generating 'Master Close Pool' Minting Script..."
    let
        !policy_TxID_Master_ClosePool = OnChainNFT.policy_TxID_Master_ClosePool pParams
        !txID_Master_ClosePool_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_ClosePool
    _ <- writeMintingPolicy policy_TxID_Master_ClosePool txID_Master_ClosePool_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_ClosePool"

    writeEstado basePathFiles nombrePool "Generating 'Master Terminate Pool' Minting Script..."
    P.putStrLn "Generating 'Master Terminate Pool' Minting Script..."
    let
        !policy_TxID_Master_TerminatePool = OnChainNFT.policy_TxID_Master_TerminatePool pParams
        !txID_Master_TerminatePool_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_TerminatePool
    _ <- writeMintingPolicy policy_TxID_Master_TerminatePool txID_Master_TerminatePool_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_TerminatePool"

    writeEstado basePathFiles nombrePool "Generating 'Master Emergency' Minting Script..."
    P.putStrLn "Generating 'Master Emergency' Minting Script..."
    let
        !policy_TxID_Master_Emergency = OnChainNFT.policy_TxID_Master_Emergency pParams
        !txID_Master_Emergency_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_Emergency
    _ <- writeMintingPolicy policy_TxID_Master_Emergency txID_Master_Emergency_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_Emergency"

    writeEstado basePathFiles nombrePool "Generating 'Master Delete Fund' Minting Script..."
    P.putStrLn "Generating 'Master Delete Fund' Minting Script..."
    let
        !policy_TxID_Master_DeleteFund = OnChainNFT.policy_TxID_Master_DeleteFund pParams txID_Master_Fund_CS
        !txID_Master_DeleteFund_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_DeleteFund
    _ <- writeMintingPolicy policy_TxID_Master_DeleteFund txID_Master_DeleteFund_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_DeleteFund"

    writeEstado basePathFiles nombrePool "Generating 'Master Send Back Fund' Minting Script..."
    P.putStrLn "Generating 'Master Send Back Fund' Minting Script..."
    let
        !policy_TxID_Master_SendBackFund = OnChainNFT.policy_TxID_Master_SendBackFund pParams
        !txID_Master_SendBackFund_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_SendBackFund
    _ <- writeMintingPolicy policy_TxID_Master_SendBackFund txID_Master_SendBackFund_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_SendBackFund"

    writeEstado basePathFiles nombrePool "Generating 'Master Send Back Deposit' Minting Script..."
    P.putStrLn "Generating 'Master Send Back Deposit' Minting Script..."
    let
        !policy_TxID_Master_SendBackDeposit = OnChainNFT.policy_TxID_Master_SendBackDeposit pParams txID_Master_Fund_CS txID_User_Deposit_CS txID_User_Harvest_CS
        !txID_Master_SendBackDeposit_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_SendBackDeposit
    _ <- writeMintingPolicy policy_TxID_Master_SendBackDeposit txID_Master_SendBackDeposit_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_SendBackDeposit"

    writeEstado basePathFiles nombrePool "Generating 'Master Add Scripts' Minting Script..."
    P.putStrLn "Generating 'Master Add Scripts' Minting Script..."
    let
        !policy_TxID_Master_AddScripts = OnChainNFT.policy_TxID_Master_AddScripts pParams
        !txID_Master_AddScripts_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_AddScripts
    _ <- writeMintingPolicy policy_TxID_Master_AddScripts txID_Master_AddScripts_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_AddScripts"

    writeEstado basePathFiles nombrePool "Generating 'Master Delete Scripts' Minting Script..."
    P.putStrLn "Generating 'Master Delete Scripts' Minting Script..."
    let
        !policy_TxID_Master_DeleteScripts = OnChainNFT.policy_TxID_Master_DeleteScripts pParams txID_Master_AddScripts_CS
        !txID_Master_DeleteScripts_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_DeleteScripts
    _ <- writeMintingPolicy policy_TxID_Master_DeleteScripts txID_Master_DeleteScripts_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_DeleteScripts"

    writeEstado basePathFiles nombrePool "Generating 'User Withdraw' Minting Script..."
    P.putStrLn "Generating 'User Withdraw' Minting Script..."
    let
        !policy_TxID_User_Withdraw = OnChainNFT.policy_TxID_User_Withdraw pParams txID_Master_Fund_CS txID_User_Deposit_CS txID_User_Harvest_CS
        !txID_User_Withdraw_CS = Utils.getCurSymbolOfPolicy policy_TxID_User_Withdraw
    _ <- writeMintingPolicy policy_TxID_User_Withdraw txID_User_Withdraw_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_User_Withdraw"

    -- writeEstado basePathFiles nombrePool "Creating Minting Policy Test..." 
    -- P.putStrLn "Creating Minting Policy Test..."

    -- let
    --     !p1 = OnChainTestInputsOutputs.policyTestInputs
    --     !p1Ref = OnChainTestInputsOutputs.policyTestInputsRef
    --     !p2 = OnChainTestInputsOutputs.policyTestOutputs

    --     !p1CS = Utils.getCurSymbolOfPolicy p1
    --     !p1RefCS= Utils.getCurSymbolOfPolicy p1Ref
    --     !p2CS = Utils.getCurSymbolOfPolicy p2

    -- _ <- writeMintingPolicy p1 p1CS             (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_Test-Inputs"
    -- _ <- writeMintingPolicy p1Ref p1RefCS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_Test-InputsRef"
    -- _ <- writeMintingPolicy p2 p2CS             (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_Test-Outputs"

    -- P.putStrLn "Creating Minting Policy PoolID"

    -- _ <- writeMintingPolicy policy_PoolID curSymbol_PoolID (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_NFT_PoolID"

    writeEstado basePathFiles nombrePool "Generating Main Validator Script..."
    P.putStrLn "Generating Main Validator Script..."

    let
            validator = OnChain.codeValidator pParams txID_Master_Fund_CS txID_Master_FundAndMerge_CS txID_Master_SplitFund_CS txID_Master_ClosePool_CS txID_Master_TerminatePool_CS txID_Master_Emergency_CS txID_Master_DeleteFund_CS txID_Master_SendBackFund_CS txID_Master_SendBackDeposit_CS txID_Master_AddScripts_CS txID_Master_DeleteScripts_CS txID_User_Deposit_CS txID_User_Harvest_CS txID_User_Withdraw_CS
            hash = Utils.hashValidator validator
            address = Utils.addressValidator hash
    _ <- writeValidator validator (basePathFiles SystemFilePathPosix.</> nombrePool) "Validator"
    _ <- writeValidatorHash hash (basePathFiles SystemFilePathPosix.</> nombrePool) "Validator"
    _ <- writeValidatorAddress address (basePathFiles SystemFilePathPosix.</> nombrePool) "Validator"

    writeEstado basePathFiles nombrePool "Creating PAB Pool Params File..."
    P.putStrLn "Creating PAB Pool Params File..."

    let
        !pabPoolParams =
            T.PABPoolParams
                {
                    T.pppPoolParams = pParams,

                    T.pppStaking_UI = staking_UI,
                    T.pppHarvest_UI = harvest_UI,

                    T.pppPoolID_TxOutRef = poolID_TxOutRef,

                    T.pppValidator = validator,
                    T.pppValidatorHash = hash,
                    T.pppValidatorAddress = address,

                    T.pppPolicy_PoolID    = policy_PoolID,
                    T.pppCurSymbol_PoolID = poolID_CS,

                    T.pppPolicy_TxID_Master_Fund    = policy_TxID_Master_Fund,
                    T.pppPolicy_TxID_Master_FundAndMerge    = policy_TxID_Master_FundAndMerge,
                    T.pppPolicy_TxID_Master_SplitFund    = policy_TxID_Master_SplitFund,
                    T.pppPolicy_TxID_Master_ClosePool    = policy_TxID_Master_ClosePool,
                    T.pppPolicy_TxID_Master_TerminatePool    = policy_TxID_Master_TerminatePool,
                    T.pppPolicy_TxID_Master_Emergency    = policy_TxID_Master_Emergency,
                    T.pppPolicy_TxID_Master_DeleteFund    = policy_TxID_Master_DeleteFund,
                    T.pppPolicy_TxID_Master_SendBackFund    = policy_TxID_Master_SendBackFund,
                    T.pppPolicy_TxID_Master_SendBackDeposit    = policy_TxID_Master_SendBackDeposit,
                    T.pppPolicy_TxID_Master_AddScripts    = policy_TxID_Master_AddScripts,
                    T.pppPolicy_TxID_Master_DeleteScripts    = policy_TxID_Master_DeleteScripts,

                    T.pppPolicy_TxID_User_Deposit    = policy_TxID_User_Deposit,
                    T.pppPolicy_TxID_User_Harvest    = policy_TxID_User_Harvest,
                    T.pppPolicy_TxID_User_Withdraw    = policy_TxID_User_Withdraw,

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

    Utils.writeEncodedToFile (basePathFiles SystemFilePathPosix.</> nombrePool SystemFilePathPosix.</> "PABPoolParams-HEX.json") pabPoolParams
    _ <- P.putStrLn $ "Saved PAB Pool Params in :" ++ P.show (basePathFiles SystemFilePathPosix.</> nombrePool SystemFilePathPosix.</> "PABPoolParams-HEX.json")

    writeEstado basePathFiles nombrePool "Done!"
    P.putStrLn "Done!"

-- exportarPoolParamsYScriptsIndividual :: P.String -> P.String -> P.String -> P.String -> P.String -> P.String -> P.String -> P.String -> P.String -> P.String -> P.String -> P.String -> P.String ->    P.String ->    P.FilePath -> P.IO ()
-- exportarPoolParamsYScriptsIndividual scriptsStr nombrePool mastersStr uTxOutRefStr beginAtPoolStr deadlinePoolStr graceTimeStr staking_UI staking_CS_Str staking_TN_Str harvest_UI harvest_CS_Str harvest_TN_Str interestStr basePathFiles = do

--     SystemDirectory.createDirectoryIfMissing True (basePathFiles SystemFilePathPosix.</> nombrePool )

--     writeEstado basePathFiles nombrePool "Starting..."
--     P.putStrLn "Starting..."

--     let 
--         scripts = DataListSplit.splitOn "," scriptsStr

--     let
--         isHexOk hex =
--             case hex of
--                     Nothing -> False
--                     _             -> True

--     let
--         mastersString = DataListSplit.splitOn "," mastersStr
--         mastersHex = TextHex.decodeHex . Utils.stringToStrictText <$> mastersString

--         getMaster masterHex' =
--             if isHexOk masterHex' then
--                 let masterHex'' = DataMaybe.fromJust masterHex'
--                 in
--                     if DataByteString.length masterHex'' P.== 56 then
--                         masterHex''
--                     else
--                         P.error "Invalid Master"
--             else
--                 P.error "Invalid Master"

--         mastersHexOK = getMaster <$> mastersHex
--         -- masterHexOK = [DataMaybe.fromJust masterHex | masterHex <- mastersHex, isHexOk masterHex ]
--         masters =    LedgerApiV2.PubKeyHash . TxBuiltinsClass.toBuiltin <$> mastersHexOK
--     P.putStrLn $ "Masters: " ++ P.show masters

--     now' <- DataTimeClockPOSIX.getPOSIXTime
--     let
--         convertPosixTime    = P.floor . (1e9 P.*) .    DataTime.nominalDiffTimeToSeconds 
--         now = convertPosixTime now'

--     let

--         beginAtPool =
--                 case TextRead.readMaybe beginAtPoolStr :: Maybe Integer of
--                     Just x ->
--                         if x >= now then
--                             LedgerApiV2.POSIXTime x
--                         else
--                             P.error "Invalid Begin At"
--                     _ -> P.error "Invalid Begin At"

--         deadlinePool =
--                 case TextRead.readMaybe deadlinePoolStr :: Maybe Integer of
--                         Just x ->
--                             if LedgerApiV2.POSIXTime x >= beginAtPool then
--                                 LedgerApiV2.POSIXTime x
--                             else
--                                 P.error "Invalid Deadline"
--                         _ -> P.error "Invalid Deadline"

--         graceTime =
--                 case TextRead.readMaybe graceTimeStr :: Maybe Integer of
--                         Just x -> if x >=0 then LedgerApiV2.POSIXTime x else P.error "Invalid GraceTime"
--                         _ -> P.error "Invalid GraceTime"

--         getStaking_CS cs' =
--             if isHexOk cs' then
--                 let cs'' = DataMaybe.fromJust cs'
--                 in
--                     if DataByteString.length cs'' P.== 56 then
--                         cs''
--                     else
--                         P.error "Invalid Staking Currency Symbol"
--             else
--                 P.error "Invalid Staking Currency Symbol"

--         getHarvest_CS cs' =
--             if isHexOk cs' then
--                 let cs'' = DataMaybe.fromJust cs'
--                 in
--                     if DataByteString.length cs'' P.== 56 then
--                         cs''
--                     else
--                         P.error "Invalid Harvest Currency Symbol"
--             else
--                 P.error "Invalid Harvest Currency Symbol"

--         (staking_CS, staking_TN)    =
--             case staking_CS_Str of
--                     "lovelace"    ->    (LedgerApiV2.adaSymbol,    LedgerApiV2.adaToken)
--                     ""                    ->    (LedgerApiV2.adaSymbol,    LedgerApiV2.adaToken)
--                     _                     ->
--                         let
--                             staking_CS_Hex = TextHex.decodeHex $ Utils.stringToStrictText staking_CS_Str
--                             staking_CS' = LedgerApiV2.CurrencySymbol $ TxBuiltinsClass.toBuiltin $ getStaking_CS staking_CS_Hex

--                             staking_TN_Hex = TextHex.decodeHex $ Utils.stringToStrictText staking_TN_Str
--                             staking_TN' = if isHexOk staking_TN_Hex then LedgerApiV2.TokenName $ TxBuiltinsClass.toBuiltin $ DataMaybe.fromJust staking_TN_Hex else P.error "Invalid Staking TokenName"
--                         in
--                             ( staking_CS' , staking_TN' )

--     P.putStrLn $ "Staking UI: " ++ P.show staking_UI
--     P.putStrLn $ "Staking CS: " ++ P.show staking_CS
--     P.putStrLn $ "Staking TN: " ++ P.show staking_TN

--     let
--         (harvest_CS, harvest_TN)    =
--             case harvest_CS_Str of
--                     "lovelace"    ->    (LedgerApiV2.adaSymbol, LedgerApiV2.adaToken)
--                     ""                    ->    (LedgerApiV2.adaSymbol, LedgerApiV2.adaToken)
--                     _                     ->
--                         let
--                             harvest_CS_Hex = TextHex.decodeHex $ Utils.stringToStrictText harvest_CS_Str
--                             harvest_CS' = LedgerApiV2.CurrencySymbol $ TxBuiltinsClass.toBuiltin $ getHarvest_CS harvest_CS_Hex

--                             harvest_TN_Hex = TextHex.decodeHex $ Utils.stringToStrictText harvest_TN_Str
--                             harvest_TN' =
--                                 if isHexOk harvest_TN_Hex then
--                                     let harvest_TN_Hex' = DataMaybe.fromJust harvest_TN_Hex
--                                     in
--                                         if DataByteString.length harvest_TN_Hex' P.> 0 then
--                                             LedgerApiV2.TokenName $ TxBuiltinsClass.toBuiltin harvest_TN_Hex'
--                                         else
--                                             P.error "Invalid Harvest TokenName, can't be empty"
--                                 else
--                                     P.error "Invalid Harvest TokenName"
--                         in
--                             ( harvest_CS' , harvest_TN' )

--     P.putStrLn $ "Harvest UI: " ++ P.show harvest_UI
--     P.putStrLn $ "Harvest CS: " ++ P.show harvest_CS
--     P.putStrLn $ "Harvest TN: " ++ P.show harvest_TN

--     let
--         interest =
--                     case TextRead.readMaybe interestStr :: Maybe Integer of
--                             Just x -> if x >=0 then x else P.error "Invalid Interest"
--                             _ -> P.error "Invalid Interest"

--     let
--         poolID_TxOutRef = Utils.unsafeReadTxOutRef uTxOutRefStr
--         poolID_TN = T.poolID_TN

--     writeEstado basePathFiles nombrePool "Creating Minting Policy PoolID..."
--     P.putStrLn "Creating Minting Policy PoolID..."
--     let
--         !policy_PoolID = OnChainNFT.policy_PoolID masters poolID_TxOutRef
--         !curSymbol_PoolID = Utils.getCurSymbolOfPolicy policy_PoolID
--     _ <- writeMintingPolicy policy_PoolID curSymbol_PoolID (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_PoolID"

--     let
--         poolID_CS = curSymbol_PoolID
--         poolID_AC = LedgerValue.AssetClass ( poolID_CS, poolID_TN)

--         pParams =
--             T.PoolParams
--                 {
--                     T.ppPoolID_CS = poolID_CS,
--                     T.ppMasters = masters,
--                     T.ppBeginAt = beginAtPool,
--                     T.ppDeadline = deadlinePool,
--                     T.ppGraceTime = graceTime,
--                     T.ppStaking_CS = staking_CS,
--                     T.ppStaking_TN = staking_TN,
--                     T.ppHarvest_CS = harvest_CS,
--                     T.ppHarvest_TN = harvest_TN,
--                     --T.ppInterestRates = [T.InterestRate {T.iMinDays = Just 90, T.iPercentage = 1}, T.InterestRate {T.iMinDays = Just 180, T.iPercentage = 2}, T.InterestRate {T.iMinDays = Nothing, T.iPercentage = 3}],
--                     T.ppInterestRates = [T.InterestRate {T.iMinDays = Nothing, T.iPercentage = interest}]
--                 }

--     P.putStrLn $ "PoolID TxOutRef: " ++ P.show poolID_TxOutRef
--     P.putStrLn $ "PoolID TN: " ++ P.show poolID_TN
--     P.putStrLn $ "PoolID CS: " ++ P.show poolID_CS
--     P.putStrLn $ "PoolID AC: " ++ P.show poolID_AC

--     let
--         isElement element list = P.not (P.null (DataList.elemIndices element list))

--     writeEstado basePathFiles nombrePool "Generating 'Master Fund' Minting Script..."
--     P.putStrLn "Generating 'Master Fund' Minting Script..."
--     let
--         !policy_TxID_Master_Fund = OnChainNFT.policy_TxID_Master_Fund pParams
--         !txID_Master_Fund_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_Fund
--     _ <- writeMintingPolicy policy_TxID_Master_Fund txID_Master_Fund_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_Fund"

--     writeEstado basePathFiles nombrePool "Generating 'User Deposit' Minting Script..."
--     P.putStrLn "Generating 'User Deposit' Minting Script..."
--     let
--         !policy_TxID_User_Deposit = OnChainNFT.policy_TxID_User_Deposit pParams
--         !txID_User_Deposit_CS = Utils.getCurSymbolOfPolicy policy_TxID_User_Deposit
--     _ <- writeMintingPolicy policy_TxID_User_Deposit txID_User_Deposit_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_User_Deposit"

--     if isElement "Master_FundAndMerge" scripts then do
--         writeEstado basePathFiles nombrePool "Generating 'Master Fund And Merge' Minting Script..."
--         P.putStrLn "Generating 'Master Fund And Merge' Minting Script..."
--         let
--             !policy_TxID_Master_FundAndMerge = OnChainNFT.policy_TxID_Master_FundAndMerge pParams txID_Master_Fund_CS
--             !txID_Master_FundAndMerge_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_FundAndMerge
--         writeMintingPolicy policy_TxID_Master_FundAndMerge txID_Master_FundAndMerge_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_FundAndMerge"
--     else
--         P.putStrLn "..."

--     if isElement "Master_SplitFund" scripts then do
--         writeEstado basePathFiles nombrePool "Generating 'Master Split Fund' Minting Script..."
--         P.putStrLn "Generating 'Master Split Fund' Minting Script..."
--         let
--             !policy_TxID_Master_SplitFund = OnChainNFT.policy_TxID_Master_SplitFund pParams txID_Master_Fund_CS
--             !txID_Master_SplitFund_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_SplitFund
--         writeMintingPolicy policy_TxID_Master_SplitFund txID_Master_SplitFund_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_SplitFund"
--     else
--         P.putStrLn "..."

--     if isElement "Master_ClosePool" scripts then do
--         writeEstado basePathFiles nombrePool "Generating 'Master Close Pool' Minting Script..."
--         P.putStrLn "Generating 'Master Close Pool' Minting Script..."
--         let
--             !policy_TxID_Master_ClosePool = OnChainNFT.policy_TxID_Master_ClosePool pParams
--             !txID_Master_ClosePool_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_ClosePool
--         writeMintingPolicy policy_TxID_Master_ClosePool txID_Master_ClosePool_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_ClosePool"
--     else
--         P.putStrLn "..."

--     if isElement "Master_TerminatePool" scripts then do
--         writeEstado basePathFiles nombrePool "Generating 'Master Terminate Pool' Minting Script..."
--         P.putStrLn "Generating 'Master Terminate Pool' Minting Script..."
--         let
--             !policy_TxID_Master_TerminatePool = OnChainNFT.policy_TxID_Master_TerminatePool pParams
--             !txID_Master_TerminatePool_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_TerminatePool
--         writeMintingPolicy policy_TxID_Master_TerminatePool txID_Master_TerminatePool_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_TerminatePool"
--     else
--         P.putStrLn "..."

--     if isElement "Master_DeleteFund" scripts then do
--         writeEstado basePathFiles nombrePool "Generating 'Master Delete Fund' Minting Script..."
--         P.putStrLn "Generating 'Master Delete Fund' Minting Script..."
--         let
--             !policy_TxID_Master_DeleteFund = OnChainNFT.policy_TxID_Master_DeleteFund pParams txID_Master_Fund_CS
--             !txID_Master_DeleteFund_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_DeleteFund
--         writeMintingPolicy policy_TxID_Master_DeleteFund txID_Master_DeleteFund_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_DeleteFund"
--     else
--         P.putStrLn "..."

--     if isElement "Master_SendBackFund" scripts then do
--         writeEstado basePathFiles nombrePool "Generating 'Master Send Back Fund' Minting Script..."
--         P.putStrLn "Generating 'Master Send Back Fund' Minting Script..."
--         let
--             !policy_TxID_Master_SendBackFund = OnChainNFT.policy_TxID_Master_SendBackFund pParams
--             !txID_Master_SendBackFund_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_SendBackFund
--         writeMintingPolicy policy_TxID_Master_SendBackFund txID_Master_SendBackFund_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_SendBackFund"
--     else
--         P.putStrLn "..."

--     if isElement "Master_SendBackDeposit" scripts then do
--         writeEstado basePathFiles nombrePool "Generating 'Master Send Back Deposit' Minting Script..."
--         P.putStrLn "Generating 'Master Send Back Deposit' Minting Script..."
--         let
--             !policy_TxID_Master_SendBackDeposit = OnChainNFT.policy_TxID_Master_SendBackDeposit pParams txID_Master_Fund_CS txID_User_Deposit_CS
--             !txID_Master_SendBackDeposit_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_SendBackDeposit
--         writeMintingPolicy policy_TxID_Master_SendBackDeposit txID_Master_SendBackDeposit_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_SendBackDeposit"
--     else
--         P.putStrLn "..."

--     writeEstado basePathFiles nombrePool "Generating 'Master Add Scripts' Minting Script..."
--     P.putStrLn "Generating 'Master Add Scripts' Minting Script..."
--     let
--         !policy_TxID_Master_AddScripts = OnChainNFT.policy_TxID_Master_AddScripts pParams
--         !txID_Master_AddScripts_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_AddScripts
--     writeMintingPolicy policy_TxID_Master_AddScripts txID_Master_AddScripts_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_AddScripts"

--     if isElement "Master_DeleteScripts" scripts then do
--         writeEstado basePathFiles nombrePool "Generating 'Master Delete Scripts' Minting Script..."
--         P.putStrLn "Generating 'Master Delete Scripts' Minting Script..."
--         let
--             !policy_TxID_Master_DeleteScripts = OnChainNFT.policy_TxID_Master_DeleteScripts pParams txID_Master_AddScripts_CS
--             !txID_Master_DeleteScripts_CS = Utils.getCurSymbolOfPolicy policy_TxID_Master_DeleteScripts
--         writeMintingPolicy policy_TxID_Master_DeleteScripts txID_Master_DeleteScripts_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_Master_DeleteScripts"
--     else
--         P.putStrLn "..."

--     if isElement "User_Harvest" scripts then do
--         writeEstado basePathFiles nombrePool "Generating 'User Harvest' Minting Script..."
--         P.putStrLn "Generating 'User Harvest' Minting Script..."
--         let
--             !policy_TxID_User_Harvest = OnChainNFT.policy_TxID_User_Harvest pParams txID_Master_Fund_CS txID_User_Deposit_CS
--             !txID_User_Harvest_CS = Utils.getCurSymbolOfPolicy policy_TxID_User_Harvest
--         writeMintingPolicy policy_TxID_User_Harvest txID_User_Harvest_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_User_Harvest"
--     else
--         P.putStrLn "..."

--     if isElement "User_Withdraw" scripts then do
--         writeEstado basePathFiles nombrePool "Generating 'User Withdraw' Minting Script..."
--         P.putStrLn "Generating 'User Withdraw' Minting Script..."
--         let
--             !policy_TxID_User_Withdraw = OnChainNFT.policy_TxID_User_Withdraw pParams txID_Master_Fund_CS txID_User_Deposit_CS
--             !txID_User_Withdraw_CS = Utils.getCurSymbolOfPolicy policy_TxID_User_Withdraw
--         writeMintingPolicy policy_TxID_User_Withdraw txID_User_Withdraw_CS (basePathFiles SystemFilePathPosix.</> nombrePool) "Mint_TxID_User_Withdraw"
--     else
--         P.putStrLn "..."

--     writeEstado basePathFiles nombrePool "Done!"
--     P.putStrLn "Done!"

------------------------------------------------------------------------------------------