{-# LANGUAGE DataKinds                                    #-}
-- {-# LANGUAGE DeriveAnyClass                         #-}
-- {-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE DerivingStrategies                 #-}
{-# LANGUAGE FlexibleContexts                     #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase                                 #-}
{-# LANGUAGE MultiParamTypeClasses            #-}
{-# LANGUAGE NoImplicitPrelude                    #-}
{-# LANGUAGE OverloadedStrings                    #-}
-- {-# LANGUAGE RecordWildCards                        #-}
{-# LANGUAGE ScopedTypeVariables                #-}
-- {-# LANGUAGE TemplateHaskell                        #-}
-- {-# LANGUAGE TypeApplications                     #-}
{-# LANGUAGE TypeFamilies                             #-}
-- {-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE RankNTypes                                 #-}
-- {-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE AllowAmbiguousTypes                #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
-- {-# LANGUAGE NumericUnderscores                 #-}
{- HLINT ignore "Use camelCase" -}
------------------------------------------------------------------------------------------
module Utils where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Cardano.Api                                                        as CardanoApi
import qualified Cardano.Api.Shelley                                        as ApiShelley
-- import qualified Cardano.Ledger.Alonzo                             as CardanoLedgerAlonzo (AlonzoEra)
-- import qualified Cardano.Ledger.Alonzo.Language            as CardanoLedgerAlonzoLanguage    (Language (..))
-- import qualified Cardano.Ledger.Alonzo.Scripts             as CardanoLedgerAlonzoScripts (ExUnits (..), Script (..)) 
import qualified Cardano.Crypto.Hash.Class                            as CryptoHashClass (hashToBytes)
import qualified Cardano.Codec.Bech32.Prefixes                    as Bench32Prefixes (addr_test, addr)
import qualified Cardano.Ledger.BaseTypes                             as LedgerBaseTypes (certIxToInt, txIxToInt)
import qualified Cardano.Ledger.Credential                            as LedgerCredential
import qualified Cardano.Ledger.Crypto                                    as LedgerCrypto (StandardCrypto)
import qualified Cardano.Ledger.Hashes                                    as LedgerHashes (ScriptHash (..))
import qualified Cardano.Ledger.Keys                                        as LedgerKeys (KeyHash (..))
import qualified Codec.Binary.Bech32                                        as CodecBinaryBech32
import qualified Codec.Serialise                                                as CodecSerialise (serialise)
import qualified Data.Aeson                                                         as DataAeson (decode, encode)
import qualified Data.ByteString                                                as DataByteString
import qualified Data.ByteString.Base16                                 as DataByteStringBase16
import qualified Data.ByteString.Char8                                    as DataByteStringChar8
import qualified Data.ByteString.Lazy                                     as DataByteStringLazy
import qualified Data.ByteString.Short                                    as DataByteStringShort
import qualified Data.ByteString.UTF8                                     as DataByteStringUTF8
import qualified Data.Maybe                                                         as DataMaybe (fromJust, fromMaybe)
-- import qualified Data.Map                                                        as DataMap
import qualified Data.String                                                        as DataString
import qualified Data.Text                                                            as DataText
import qualified Data.Text.Encoding                                         as DataTextEncoding
import qualified Data.Text.Lazy                                                 as DataTextLazy
import qualified Data.Text.Lazy.Encoding                                as DataTextLazyEncoding
import qualified Ledger
import qualified Ledger.Address                                                 as LedgerAddress (Address)
import qualified Ledger.Bytes                                                     as LedgerBytes (LedgerBytes(LedgerBytes), fromHex) --getLedgerBytes
import qualified Plutus.Script.Utils.V2.Scripts                 as UtilsScriptsV2
import qualified Plutus.V1.Ledger.Api                                     as LedgerApiV1
import qualified Plutus.V1.Ledger.Credential                        as LedgerCredentialV1
-- import qualified Plutus.V1.Ledger.Crypto                         as LedgerCryptoV1 
-- import qualified Plutus.V1.Ledger.EvaluationContext    as LedgerEvaluationContextV1
-- import qualified Plutus.V1.Ledger.Bytes                            as LedgerBytesV1
import qualified Plutus.V1.Ledger.Value                                 as LedgerValueV1 (TokenName (..))
import qualified Plutus.V1.Ledger.Scripts                             as LedgerScriptsV1
import qualified Plutus.V2.Ledger.Api                                     as LedgerApiV2
-- import qualified Plutus.V2.Ledger.Credential                 as LedgerCredentialV2
-- import qualified Plutus.V2.Ledger.Crypto                         as LedgerCryptoV2 
import qualified Plutus.V2.Ledger.EvaluationContext         as LedgerEvaluationContextV2
-- import qualified Plutus.V2.Ledger.Value                            as LedgerValueV2 (TokenName (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins                                            as TxBuiltins (toBuiltin)
import qualified PlutusTx.Builtins.Class                                as TxBuiltinsClass
import qualified PlutusTx.Builtins.Internal                         as TxBuiltinsInternal (BuiltinByteString (..)) --decodeUtf8
--decodeUtf8
import                     PlutusTx.Prelude                                             ( return, Bool(True), Maybe(..), Either(..), BuiltinByteString, Semigroup((<>)), sha2_256, ($), (.), fst, (<$>), (++), maybe, Integer )
import qualified Prelude                                                                as P
import qualified System.Directory                                             as SystemDirectory
import qualified System.FilePath.Posix                                    as SystemFilePathPosix
import qualified Wallet.Emulator.Wallet                                 as WalletEmulator            (WalletId (..)) --, Wallet (..)
import qualified Wallet.Types                                                     as WalletTypes (ContractInstanceId (..))
------------------------------------------------------------------------------------------
-- Modulo 
------------------------------------------------------------------------------------------

stringToStrictByteString                        :: P.String -> DataByteString.ByteString
stringToStrictByteString                        = DataTextEncoding.encodeUtf8 . DataText.pack
stringToLazyByteString                            :: P.String -> DataByteStringLazy.ByteString
stringToLazyByteString                            = DataTextLazyEncoding.encodeUtf8 . DataTextLazy.pack
stringToStrictText                                    :: P.String -> DataText.Text
stringToStrictText                                    = DataText.pack
stringToLazyText                                        :: P.String -> DataTextLazy.Text
stringToLazyText                                        = DataTextLazy.pack

------------------------------------------------------------------------------------------

strictByteStringToString                        :: DataByteString.ByteString -> P.String
strictByteStringToString                        = DataText.unpack . DataTextEncoding.decodeUtf8
strictByteStringToLazyByteString        :: DataByteString.ByteString -> DataByteStringLazy.ByteString
strictByteStringToLazyByteString        = DataByteStringLazy.fromChunks . return
strictByteStringToStrictText                :: DataByteString.ByteString -> DataText.Text
strictByteStringToStrictText                = DataTextEncoding.decodeUtf8
strictByteStringToLazyText                    :: DataByteString.ByteString -> DataTextLazy.Text
strictByteStringToLazyText                    = DataTextLazy.fromStrict . DataTextEncoding.decodeUtf8

------------------------------------------------------------------------------------------

lazyByteStringToString                            :: DataByteStringLazy.ByteString -> P.String
lazyByteStringToString                            = DataTextLazy.unpack . DataTextLazyEncoding.decodeUtf8
lazyByteStringToStrictByteString        :: DataByteStringLazy.ByteString -> DataByteString.ByteString
lazyByteStringToStrictByteString        = DataByteString.concat . DataByteStringLazy.toChunks
lazyByteStringToStrictText                    :: DataByteStringLazy.ByteString -> DataText.Text
lazyByteStringToStrictText                    = DataTextEncoding.decodeUtf8 . DataByteString.concat . DataByteStringLazy.toChunks
lazyByteStringToLazyText                        :: DataByteStringLazy.ByteString -> DataTextLazy.Text
lazyByteStringToLazyText                        = DataTextLazyEncoding.decodeUtf8

------------------------------------------------------------------------------------------

strictTextToString                                    :: DataText.Text -> P.String
strictTextToString                                    = DataText.unpack
strictTextToStrictByteString                :: DataText.Text -> DataByteString.ByteString
strictTextToStrictByteString                = DataTextEncoding.encodeUtf8
strictTextToLazyByteString                    :: DataText.Text -> DataByteStringLazy.ByteString
strictTextToLazyByteString                    = DataByteStringLazy.fromChunks . return . DataTextEncoding.encodeUtf8
strictTextToLazyText                                :: DataText.Text -> DataTextLazy.Text
strictTextToLazyText                                = DataTextLazy.fromStrict

------------------------------------------------------------------------------------------

lazyTextToString                                        :: DataTextLazy.Text -> P.String
lazyTextToString                                        = DataTextLazy.unpack
lazyTextTostrictByteString                    :: DataTextLazy.Text -> DataByteString.ByteString
lazyTextTostrictByteString                    = DataTextEncoding.encodeUtf8 . DataTextLazy.toStrict
lazyTextToLazyByteString                        :: DataTextLazy.Text -> DataByteStringLazy.ByteString
lazyTextToLazyByteString                        = DataTextLazyEncoding.encodeUtf8
lazyTextToStrictText                                :: DataTextLazy.Text -> DataText.Text
lazyTextToStrictText                                = DataTextLazy.toStrict

------------------------------------------------------------------------------------------

stringToBuiltinByteString :: P.String -> TxBuiltinsInternal.BuiltinByteString
stringToBuiltinByteString    = TxBuiltinsInternal.BuiltinByteString . stringToStrictByteString 

------------------------------------------------------------------------------------------

dataToScriptData :: PlutusTx.Data -> CardanoApi.ScriptData
dataToScriptData (PlutusTx.Constr n xs) = CardanoApi.ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (PlutusTx.Map xs)            = CardanoApi.ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (PlutusTx.List xs)         = CardanoApi.ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (PlutusTx.I n)                 = CardanoApi.ScriptDataNumber n
dataToScriptData (PlutusTx.B bs)                = CardanoApi.ScriptDataBytes bs

scriptDataToData :: CardanoApi.ScriptData -> PlutusTx.Data
scriptDataToData (CardanoApi.ScriptDataConstructor int xs) = PlutusTx.Constr int [ scriptDataToData x | x <- xs ]
scriptDataToData (CardanoApi.ScriptDataMap    kvs) = PlutusTx.Map    [ (scriptDataToData k, scriptDataToData v) | (k,v) <- kvs ]
scriptDataToData (CardanoApi.ScriptDataList    xs) = PlutusTx.List [ scriptDataToData x | x <- xs ]
scriptDataToData (CardanoApi.ScriptDataNumber n) = PlutusTx.I n
scriptDataToData (CardanoApi.ScriptDataBytes bs) = PlutusTx.B bs

----------------------------------------------------------------------------------------

writeFile :: P.FilePath -> DataByteStringLazy.ByteString -> P.IO ()
writeFile = DataByteStringLazy.writeFile

readFile :: P.String -> P.IO DataByteStringLazy.ByteString
readFile = DataByteStringLazy.readFile

----------------------------------------------------------------------------------------

writeEncodedToFile :: ApiShelley.ToJSON a => P.FilePath -> a -> P.IO ()
writeEncodedToFile filepath dataToWrite =
    writeFile filepath (DataAeson.encode dataToWrite)

readFileDecodedAsRedeemer :: P.String -> P.IO LedgerApiV1.Redeemer
readFileDecodedAsRedeemer filepath = do
    !file <- Utils.readFile filepath
    -- P.putStrLn $ "file: " ++ P.show file
    -- case DataAeson.decode file :: Maybe LedgerApiV1.Redeemer of
    --     Nothing             -> P.error "Could not decode from file"
    --     Just decoded    -> return decoded
    readStringDecodedAsRedeemer (lazyByteStringToString file)

readFileDecodedAsDatum :: P.String -> P.IO LedgerApiV1.Datum
readFileDecodedAsDatum filepath = do
    !file <- Utils.readFile filepath
    -- P.putStrLn $ "file: " ++ P.show file
    -- case DataAeson.decode file :: Maybe LedgerApiV1.Datum of
    --     Nothing             -> P.error "Could not decode from file"
    --     Just decoded    -> return decoded
    readStringDecodedAsDatum (lazyByteStringToString file)

----------------------------------------------------------------------------------------

readStringDecodedAsRedeemer :: P.String -> P.IO LedgerApiV1.Redeemer
readStringDecodedAsRedeemer encoded = do
    P.putStrLn $ "encoded: " ++ P.show encoded
    case DataAeson.decode (stringToLazyByteString encoded) :: Maybe LedgerApiV1.Redeemer of
        Nothing             -> P.error $ "Could not decode As Redeemer " ++ encoded
        Just decoded    -> return decoded


readStringDecodedAsDatum :: P.String -> P.IO LedgerApiV1.Datum
readStringDecodedAsDatum encoded = do
    P.putStrLn $ "encoded: " ++ P.show encoded
    case DataAeson.decode (stringToLazyByteString encoded) :: Maybe LedgerApiV1.Datum of
        Nothing             -> P.error $ "Could not decode As Datum " ++ encoded
        Just decoded    -> return decoded

----------------------------------------------------------------------------------------

writePlutusDataToFile :: PlutusTx.ToData a => P.FilePath -> a -> P.IO ()
writePlutusDataToFile filepath dataToWrite = do
    let !scriptData = (CardanoApi.scriptDataToJson CardanoApi.ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData) dataToWrite
    writeFile filepath (DataAeson.encode scriptData)

readFileToPlutusData :: P.String -> P.IO PlutusTx.Data
readFileToPlutusData filepath = do
    !file <- readFile filepath
    case DataAeson.decode file of
        Nothing             -> P.error "Could not decode from file"
        Just decoded    -> case CardanoApi.scriptDataFromJson CardanoApi.ScriptDataJsonDetailedSchema decoded of
            Right scriptData    -> return $ scriptDataToData scriptData
            _                                 -> P.error "Could not scriptDataFromJson"

----------------------------------------------------------------------------------------

writeUnit :: P.String -> P.IO ()
writeUnit path = writePlutusDataToFile (path ++ "/unit.json") ()

----------------------------------------------------------------------------------------

credentialLedgerToPlutus :: LedgerCredential.Credential a LedgerCrypto.StandardCrypto -> LedgerCredentialV1.Credential
credentialLedgerToPlutus (LedgerCredential.ScriptHashObj (LedgerHashes.ScriptHash h)) = LedgerApiV1.ScriptCredential $ LedgerApiV1.ValidatorHash $ TxBuiltins.toBuiltin $ CryptoHashClass.hashToBytes h
credentialLedgerToPlutus (LedgerCredential.KeyHashObj (LedgerKeys.KeyHash h))             = LedgerApiV1.PubKeyCredential $ LedgerApiV1.PubKeyHash $ TxBuiltins.toBuiltin $ CryptoHashClass.hashToBytes h

----------------------------------------------------------------------------------------

stakeReferenceLedgerToPlutus :: LedgerCredential.StakeReference LedgerCrypto.StandardCrypto -> Maybe LedgerCredentialV1.StakingCredential
stakeReferenceLedgerToPlutus (LedgerCredential.StakeRefBase x)                                     = Just $ LedgerApiV1.StakingHash $ credentialLedgerToPlutus x
stakeReferenceLedgerToPlutus (LedgerCredential.StakeRefPtr (LedgerCredential.Ptr (CardanoApi.SlotNo x) txIx certIx)) =
    let
        !txIxInteger = P.toInteger (LedgerBaseTypes.txIxToInt txIx)
        !certIxInteger = P.toInteger (LedgerBaseTypes.certIxToInt certIx)
    in Just $ LedgerApiV1.StakingPtr (P.fromIntegral x) txIxInteger    certIxInteger
stakeReferenceLedgerToPlutus LedgerCredential.StakeRefNull                                             = Nothing

----------------------------------------------------------------------------------------

tryReadAddress :: P.String -> Maybe LedgerApiV1.Address
tryReadAddress x = case CardanoApi.deserialiseAddress CardanoApi.AsAddressAny $ DataText.pack x of
    Nothing                                                             -> Nothing
    Just (ApiShelley.AddressByron _)                                    -> Nothing
    Just (ApiShelley.AddressShelley (ApiShelley.ShelleyAddress _ p s))  -> 
        Just LedgerApiV1.Address
            {
                LedgerApiV1.addressCredential                = credentialLedgerToPlutus p,
                LedgerApiV1.addressStakingCredential = stakeReferenceLedgerToPlutus s
            }

----------------------------------------------------------------------------------------

tryReadWalletId :: P.String -> Maybe WalletEmulator.WalletId
tryReadWalletId = DataAeson.decode . DataAeson.encode

----------------------------------------------------------------------------------------

unsafeReadWalletId :: P.String -> WalletEmulator.WalletId
unsafeReadWalletId s = DataMaybe.fromMaybe (P.error $ "can't parse " ++ s ++ " as a WalletId") $ tryReadWalletId s

----------------------------------------------------------------------------------------

unsafeReadAddress :: P.String -> LedgerApiV1.Address
unsafeReadAddress s = DataMaybe.fromMaybe (P.error $ "can't parse " ++ s ++ " as an address") $ tryReadAddress s

----------------------------------------------------------------------------------------

unsafeReadTxOutRef :: P.String -> LedgerApiV1.TxOutRef
unsafeReadTxOutRef s =
    let
        !(x, _ : y) = P.span (P./= '#') s
    in
        LedgerApiV1.TxOutRef
            {
                LedgerApiV1.txOutRefId    = DataString.fromString x,
                LedgerApiV1.txOutRefIdx = P.read y
            }

----------------------------------------------------------------------------------------

getCredentials :: LedgerApiV1.Address -> Maybe (Ledger.PaymentPubKeyHash, Maybe Ledger.StakePubKeyHash)
getCredentials (LedgerApiV1.Address x y) = case x of
    LedgerApiV1.ScriptCredential _    -> Nothing
    LedgerApiV1.PubKeyCredential pkh ->
        let
            !ppkh = Ledger.PaymentPubKeyHash pkh
        in
            case y of
                    Nothing                                                             -> Just (ppkh, Nothing)
                    Just LedgerApiV1.StakingPtr {}                -> Nothing
                    Just (LedgerApiV1.StakingHash h)            -> case h of
                            LedgerApiV1.ScriptCredential _        -> Nothing
                            LedgerApiV1.PubKeyCredential pkh' -> Just (ppkh, Just $ Ledger.StakePubKeyHash pkh')

----------------------------------------------------------------------------------------

unsafePaymentPubKeyHash :: LedgerApiV1.Address -> Ledger.PaymentPubKeyHash
unsafePaymentPubKeyHash addr = maybe (P.error $ "script address " ++ P.show addr ++ " does not contain a payment key") fst $ getCredentials addr

unsafeStakePubKeyHash :: LedgerApiV1.Address -> Ledger.StakePubKeyHash
unsafeStakePubKeyHash addr = 
    case getCredentials addr of
        Nothing                    -> P.error $ "unexpected script address " ++ P.show addr
        Just (_, Nothing) -> P.error $ "addres " ++ P.show addr ++ " contains no stake component"
        Just (_, Just x) -> x

----------------------------------------------------------------------------------------

cidToString :: WalletTypes.ContractInstanceId -> P.String
cidToString = P.show . WalletTypes.unContractInstanceId

----------------------------------------------------------------------------------------

unsafeTokenNameToHex :: LedgerValueV1.TokenName -> P.String
unsafeTokenNameToHex = DataByteStringChar8.unpack . CardanoApi.serialiseToRawBytesHex . DataMaybe.fromJust . CardanoApi.deserialiseFromRawBytes CardanoApi.AsAssetName . getByteString . LedgerValueV1.unTokenName
    where
        getByteString (TxBuiltinsInternal.BuiltinByteString bs) = bs

----------------------------------------------------------------------------------------

-- for testing the sha_256 hash in the tokenName and comparting with the one i was making in Javascript
concatenateBBS :: P.String -> P.String -> BuiltinByteString
concatenateBBS bbs1 bbs2 =    TxBuiltinsClass.stringToBuiltinByteString bbs1 <> TxBuiltinsClass.stringToBuiltinByteString bbs2

----------------------------------------------------------------------------------------

-- for testing the sha_256 hash in the tokenName and comparting with the one i was making in Javascript
sha2_256Str :: P.String -> BuiltinByteString
sha2_256Str bbs1 = sha2_256 $ TxBuiltinsClass.stringToBuiltinByteString bbs1

----------------------------------------------------------------------------------------

-- for testing the sha_256 hash in the tokenName and comparting with the one i was making in Javascript
sha2_256HexStr :: P.String -> BuiltinByteString
sha2_256HexStr bbs1 =
    let Right r1 = LedgerBytes.fromHex $ DataByteStringUTF8.fromString bbs1
    in sha2_256 $ LedgerApiV1.getLedgerBytes r1
----------------------------------------------------------------------------------------

-- for testing the sha_256 hash in the tokenName and comparting with the one i was making in Javascript
sha2_256BBS :: BuiltinByteString -> BuiltinByteString
sha2_256BBS = sha2_256

----------------------------------------------------------------------------------------

-- for testing the sha_256 hash in the tokenName and comparting with the one i was making in Javascript
sha2_256HexBBS :: BuiltinByteString -> BuiltinByteString
sha2_256HexBBS bbs1 =
    let Right r1 = LedgerBytes.fromHex $ DataByteStringUTF8.fromString $ P.show bbs1
    in sha2_256 $ LedgerApiV1.getLedgerBytes r1

----------------------------------------------------------------------------------------

-- for testing the sha_256 hash in the tokenName and comparting with the one i was making in Javascript
bbsToTokenName :: BuiltinByteString -> LedgerApiV1.TokenName
bbsToTokenName = LedgerApiV1.TokenName

----------------------------------------------------------------------------------------

pkhFromStr :: P.String -> LedgerApiV1.PubKeyHash
pkhFromStr s =
    case LedgerBytes.fromHex (DataString.fromString s) of
        Right (LedgerBytes.LedgerBytes bytes) -> LedgerApiV1.PubKeyHash bytes
        Left msg -> P.error $ "Could not convert from hex to bytes: " ++ P.show msg

----------------------------------------------------------------------------------------

hashValidator :: LedgerApiV2.Validator -> LedgerApiV2.ValidatorHash
hashValidator = UtilsScriptsV2.validatorHash

hashScriptValidator :: LedgerApiV2.Validator -> LedgerApiV2.ScriptHash
hashScriptValidator = UtilsScriptsV2.scriptHash . LedgerApiV2.getValidator 

addressValidator :: LedgerApiV2.ValidatorHash -> LedgerAddress.Address
addressValidator = Ledger.scriptHashAddress

------------------------------------------------------------------------------------------

getCurSymbolOfPolicy :: LedgerApiV2.MintingPolicy -> LedgerApiV2.CurrencySymbol
getCurSymbolOfPolicy = UtilsScriptsV2.scriptCurrencySymbol 

hashScriptMinting :: LedgerApiV2.MintingPolicy -> LedgerApiV2.ScriptHash
hashScriptMinting = UtilsScriptsV2.scriptHash . LedgerApiV2.getMintingPolicy    

getPolicyScript :: LedgerApiV2.MintingPolicy -> LedgerApiV2.Script
getPolicyScript = LedgerApiV2.unMintingPolicyScript

------------------------------------------------------------------------------------------

getScriptUnValidatorV1 :: LedgerScriptsV1.Validator -> LedgerScriptsV1.Script
getScriptUnValidatorV1    = LedgerScriptsV1.unValidatorScript

getScriptShortBsV1 :: LedgerScriptsV1.Script -> DataByteStringShort.ShortByteString
getScriptShortBsV1 = DataByteStringShort.toShort . DataByteStringLazy.toStrict . CodecSerialise.serialise

getScriptSerialisedV1 :: DataByteStringShort.ShortByteString -> ApiShelley.PlutusScript ApiShelley.PlutusScriptV1
getScriptSerialisedV1 = ApiShelley.PlutusScriptSerialised

writeValidatorV1 :: P.String -> P.String -> LedgerScriptsV1.Validator -> P.IO (Either (CardanoApi.FileError ()) ())
writeValidatorV1 path file codeValidator = do
    let
        --v1dir = "V1"
        !scriptUnValidatorV1 = getScriptUnValidatorV1 codeValidator
        !scriptShortBsV1 = getScriptShortBsV1 scriptUnValidatorV1
        !scriptSerialisedV1 = getScriptSerialisedV1 scriptShortBsV1
    SystemDirectory.createDirectoryIfMissing True path --SystemFilePathPosix.</> v1dir
    CardanoApi.writeFileTextEnvelope (path SystemFilePathPosix.</> file) Nothing scriptSerialisedV1

----------------------------------------------------------------------------------------

getScriptUnValidator :: LedgerApiV2.Validator -> LedgerApiV2.Script
getScriptUnValidator    = LedgerApiV2.unValidatorScript

getScriptShortBs :: LedgerApiV2.Script -> DataByteStringShort.ShortByteString
getScriptShortBs = DataByteStringShort.toShort . DataByteStringLazy.toStrict . CodecSerialise.serialise

getScriptSerialised :: DataByteStringShort.ShortByteString -> ApiShelley.PlutusScript ApiShelley.PlutusScriptV2
getScriptSerialised = ApiShelley.PlutusScriptSerialised

writeValidator :: P.String -> P.String -> LedgerScriptsV1.Validator -> P.IO (Either (CardanoApi.FileError ()) ())
writeValidator path file codeValidator = do
    let
        --v1dir = "V2"
        !scriptUnValidatorV2 = getScriptUnValidator codeValidator
        !scriptShortBsV2 = getScriptShortBs scriptUnValidatorV2
        !scriptSerialisedV2 = getScriptSerialised scriptShortBsV2
    SystemDirectory.createDirectoryIfMissing True path --SystemFilePathPosix.</> V2dir
    CardanoApi.writeFileTextEnvelope (path SystemFilePathPosix.</> file) Nothing scriptSerialisedV2

----------------------------------------------------------------------------------------

getScriptMintingPolicyV1 :: LedgerScriptsV1.MintingPolicy -> LedgerScriptsV1.Script
getScriptMintingPolicyV1    = LedgerScriptsV1.getMintingPolicy

writeMintingPolicyV1 :: P.String -> P.String -> LedgerScriptsV1.MintingPolicy -> P.IO (Either (CardanoApi.FileError ()) ())
writeMintingPolicyV1 path file policy = do
    let
        --v1dir = "V1"
        !scriptMintingPolicyV1 = getScriptMintingPolicyV1 policy
        !scriptShortBsV1 = getScriptShortBsV1 scriptMintingPolicyV1
        !scriptSerialisedV1 = getScriptSerialisedV1 scriptShortBsV1
    SystemDirectory.createDirectoryIfMissing True path --SystemFilePathPosix.</> v1dir
    CardanoApi.writeFileTextEnvelope (path SystemFilePathPosix.</> file) Nothing scriptSerialisedV1

----------------------------------------------------------------------------------------

getScriptMintingPolicy :: LedgerApiV2.MintingPolicy -> LedgerApiV2.Script
getScriptMintingPolicy    = LedgerApiV2.getMintingPolicy

writeMintingPolicy :: P.String -> P.String -> LedgerApiV2.MintingPolicy -> P.IO (Either (CardanoApi.FileError ()) ())
writeMintingPolicy path file policy = do
    let
        --v1dir = "V1"
        !scriptMintingPolicyV2 = getScriptMintingPolicy policy
        !scriptShortBsV2 = getScriptShortBs scriptMintingPolicyV2
        !scriptSerialisedV2 = getScriptSerialised scriptShortBsV2
    SystemDirectory.createDirectoryIfMissing True path --SystemFilePathPosix.</> v1dir
    CardanoApi.writeFileTextEnvelope (path SystemFilePathPosix.</> file) Nothing scriptSerialisedV2

----------------------------------------------------------------------------------------
-- Create Script Address
------------------------------------------------------------------------------------------

{-
Header for Testnet is "70" and for Mainnet "71"
To work out why you need to go read CIP19:
https://cips.cardano.org/cips/cip19/#shelley-addresses

And then look up the
- Header Bits '011100000' for Testnet or 
- Header Bits '011100001' for Mainnet
In a binary to Hex table, such as here:
https://www.rapidtables.com/convert/number/binary-to-ascii.html

The header is in Base16 format (aka Hexadecimal)
-}

addrBech32AddHeader :: P.String -> P.String -> DataText.Text
addrBech32AddHeader h v = DataText.pack (h P.++ v)

addrBech32HeaderTestnet :: P.String
addrBech32HeaderTestnet = "70"

addrBech32HeaderMainnet:: P.String
addrBech32HeaderMainnet = "71"

validatorHashToHex :: LedgerApiV2.ValidatorHash -> P.String
validatorHashToHex = P.show

validatorHashHexWithHeader ::    P.String ->    LedgerApiV2.ValidatorHash -> DataByteString.ByteString
validatorHashHexWithHeader headerNetworkTag vhash = DataTextEncoding.encodeUtf8 (addrBech32AddHeader headerNetworkTag (validatorHashToHex vhash))

validatorHashToBinary :: P.String -> LedgerApiV2.ValidatorHash -> DataByteString.ByteString
validatorHashToBinary headerNetworkTag vhash = do
    let Right decoded = DataByteStringBase16.decode (validatorHashHexWithHeader headerNetworkTag vhash)
    decoded

{-
Prefix for Testnet: "addr_test" and Mainnet: "addr"
These prefixes are imported from Cardano.Codec.Bech32.Prefixes
-}

addrBech32DataPart :: P.String -> LedgerApiV2.ValidatorHash -> CodecBinaryBech32.DataPart
addrBech32DataPart headerNetworkTag vhash = CodecBinaryBech32.dataPartFromBytes (validatorHashToBinary headerNetworkTag vhash)

validatorAddrToHash :: LedgerAddress.Address -> LedgerApiV2.ValidatorHash
validatorAddrToHash addr = DataMaybe.fromJust (Ledger.toValidatorHash addr)

{-
The bech32 representation is derived in 2 steps:
- bech32 = prefix + dataPart
- and where dataPart = header + validatorHash
-}

validatorAddrToAddrBech32Testnet :: LedgerAddress.Address -> DataByteStringLazy.ByteString
validatorAddrToAddrBech32Testnet addr = do
    let
        !vhash = validatorAddrToHash addr
        !headerNetworkTag = addrBech32HeaderTestnet
        Right encoded = CodecBinaryBech32.encode Bench32Prefixes.addr_test (addrBech32DataPart headerNetworkTag vhash)
    DataByteStringLazy.fromStrict $ DataTextEncoding.encodeUtf8 encoded

validatorAddrToAddrBech32Mainnet :: LedgerAddress.Address -> DataByteStringLazy.ByteString
validatorAddrToAddrBech32Mainnet addr = do
    let
        !vhash = validatorAddrToHash addr
        !headerNetworkTag = addrBech32HeaderMainnet
        Right encoded = CodecBinaryBech32.encode Bench32Prefixes.addr (addrBech32DataPart headerNetworkTag vhash)
    DataByteStringLazy.fromStrict $ DataTextEncoding.encodeUtf8 encoded

----------------------------------------------------------------------------------------

evaluateScriptValidator :: LedgerApiV2.Validator -> [PlutusTx.Data] -> (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, Integer)
evaluateScriptValidator validator datas =
    let
        !pv = LedgerApiV2.ProtocolVersion 6 0

        !scriptUnValidatorV2 = Utils.getScriptUnValidator validator

        !scriptShortBsV2 = Utils.getScriptShortBs scriptUnValidatorV2
        --scriptSerialisedV2 = Utils.getScriptSerialised scriptShortBsV2

        !(logout, e) = LedgerApiV2.evaluateScriptCounting pv LedgerApiV2.Verbose LedgerEvaluationContextV2.evalCtxForTesting scriptShortBsV2 datas

        !size = LedgerScriptsV1.scriptSize scriptUnValidatorV2

    in 
        (logout, e, size)
  

----------------------------------------------------------------------------------------

evaluateScriptMint :: LedgerApiV2.MintingPolicy -> [PlutusTx.Data] -> (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, Integer)
evaluateScriptMint policy datas =
    let
        !pv = LedgerApiV2.ProtocolVersion 6 0

        !scriptMintingPolicyV2 = Utils.getScriptMintingPolicy policy

        !scriptShortBsV2 = Utils.getScriptShortBs scriptMintingPolicyV2
        --scriptSerialisedV1 = Utils.getScriptSerialisedV1 scriptShortBsV1
        
        exBudget :: LedgerApiV2.ExBudget
        exBudget = LedgerApiV2.ExBudget 10000000000 14000000
    
        -- !(logout, e) = Ledge rApiV2.evaluateScriptCounting pv LedgerApiV2.Verbose LedgerEvaluationContextV2.evalCtxForTesting scriptShortBsV2 datas
        !(logout, e) = LedgerApiV2.evaluateScriptRestricting pv LedgerApiV2.Verbose LedgerEvaluationContextV2.evalCtxForTesting exBudget scriptShortBsV2 datas

        !size = LedgerScriptsV1.scriptSize scriptMintingPolicyV2
   in 
        (logout, e, size)
  
----------------------------------------------------------------------------------------
