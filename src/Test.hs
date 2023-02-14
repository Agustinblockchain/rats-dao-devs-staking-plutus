{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE BangPatterns #-}
{- HLINT ignore "Use camelCase" -}


module Test where

import qualified Data.ByteString.Short                         as DataByteStringShort
import qualified Data.ByteString.Lazy                          as DataByteStringLazy
import qualified Codec.Serialise                               as CodecSerialise
import qualified Data.Maybe                                    as DataMaybe
import qualified Ledger
import qualified Ledger.Ada                                    as LedgerAda
import qualified Ledger.Address                             as LedgerAddress
import qualified Ledger.Value                                  as LedgerValue
import qualified Plutonomy
import qualified Plutus.Script.Utils.V2.Scripts                as UtilsScriptsV2
import qualified Plutus.V1.Ledger.Scripts                      as LedgerScriptsV1
import qualified Plutus.V2.Ledger.Api                          as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts                     as LedgerContextsV2
import qualified Plutus.V2.Ledger.EvaluationContext            as LedgerEvaluationContextV2
import qualified PlutusTx
import qualified PlutusTx.AssocMap                                 as TxAssocMap
import           PlutusTx.Prelude
import qualified Prelude                                       as P

---------------------------------------------------

{-# INLINABLE flattenValueWithoutZeros #-}
flattenValueWithoutZeros :: LedgerValue.Value -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
-- flattenValueWithoutZeros v =
flattenValueWithoutZeros (LedgerValue.Value !mp) =
    let
        !f1 = TxAssocMap.toList mp
        !f2 = [ ( cs , TxAssocMap.toList mp') | (cs, mp') <- f1 ]
        !f3 = [ (cs , tn, amt) | (cs, f4) <- f2, (tn, amt) <- f4, amt /= 0 ]

    --     !f1 = LedgerValue.flattenValue v
    --     !f3 = [ (cs , tn, amt) | (cs , tn, amt) <- f1, amt /= 0 ]
    in
        f3

{-# INLINABLE valueEqualsValue #-}
valueEqualsValue :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue !value1 !value2 =
    let
        !value1' = flattenValueWithoutZeros value1
        !value2' = flattenValueWithoutZeros value2
    in
        length value1' == length value2' &&
        all (\(cs, tn, amount) ->
                let
                    !ac = LedgerValue.AssetClass (cs, tn)
                in
                    LedgerValue.assetClassValueOf value2 ac == amount
            ) value1'

---------------------------------------------------

{-# INLINABLE mkPolicy1 #-}
mkPolicy1 :: BuiltinData -> BuiltinData -> ()
mkPolicy1 !mintRedeemerRaw !ctxRaw  =
    let
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !inputs = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        !outputs = LedgerApiV2.txInfoOutputs (LedgerContextsV2.scriptContextTxInfo ctx)
        !inputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- inputs ]
        !outputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- outputs ]

        !value1 = inputs_Values!!0
        !value2 = inputs_Values!!1
        !value3_Control = value1 <> value2
        !value3_Real = outputs_Values!!0

    in
        if value3_Control == value3_Real
        then ()
        else error ()

{-# INLINABLE mkPolicy2 #-}
mkPolicy2 :: BuiltinData -> BuiltinData -> ()
mkPolicy2 !mintRedeemerRaw !ctxRaw  =
    let
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !inputs = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        !outputs = LedgerApiV2.txInfoOutputs (LedgerContextsV2.scriptContextTxInfo ctx)
        !inputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- inputs ]
        !outputs_Values = [ LedgerApiV2.txOutValue txtout | txtout <- outputs ]

        !value1 = inputs_Values!!0
        !value2 = inputs_Values!!1
        !value3_Control = value1 <> value2
        !value3_Real = outputs_Values!!0

    in
        if valueEqualsValue value3_Control value3_Real
        then ()
        else error ()

---------------------------------------------------

{-# INLINEABLE policy1 #-}
policy1 :: LedgerApiV2.MintingPolicy
policy1 = LedgerApiV2.mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicy1 ||])

{-# INLINEABLE policy2 #-}
policy2 :: LedgerApiV2.MintingPolicy
policy2 = LedgerApiV2.mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicy2 ||])

---------------------------------------------------

{-# INLINEABLE policy1_fromPlutonomy #-}
policy1_fromPlutonomy :: LedgerApiV2.MintingPolicy
policy1_fromPlutonomy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ Plutonomy.mkMintingPolicyScript
    $$(PlutusTx.compile [|| mkPolicy1 ||])

{-# INLINEABLE policy2_fromPlutonomy #-}
policy2_fromPlutonomy :: LedgerApiV2.MintingPolicy
policy2_fromPlutonomy = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ Plutonomy.mkMintingPolicyScript
    $$(PlutusTx.compile [|| mkPolicy2 ||])

---------------------------------------------------

curSymbol :: LedgerApiV2.MintingPolicy -> LedgerApiV2.CurrencySymbol
curSymbol = UtilsScriptsV2.scriptCurrencySymbol

---------------------------------------------------

evaluate :: P.IO ()
evaluate =
    let

        !value_Tk1     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui1")) 1
        !value_Tk2     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui2")) 1
        !value_Tk3     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui3")) 1
        !value_Tk4     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui4")) 1
        !value_Tk5     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui5")) 1
        !value_Tk6     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui6")) 1
        !value_Tk7     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui7")) 1
        !value_Tk8     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui8")) 1
        !value_Tk9     = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "AADSDDSERRDFFqwertyui9")) 1
        !value_Tk11    = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "qwertyui11")) 1
        !value_Tk21    = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "qwertyui21")) 1
        !value_Tk31    = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "qwertyui31")) 1
        !value_Tk41    = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "qwertyui41")) 1
        !value_Tk51    = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "qwertyui51")) 1
        !value_Tk61    = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "qwertyui61")) 1
        !value_Tk71    = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "qwertyui71")) 1
        !value_Tk81    = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "qwertyui81")) 1
        !value_Tk91    = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "qwertyui91")) 1
        !value_Tk111   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "qwertyui111")) 1
        !value_Tk211   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "qwertyui211")) 1
        !value_Tk311   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "qwertyui311")) 1
        !value_Tk411   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "qwertyui411")) 1
        !value_Tk511   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "qwertyui511")) 1
        !value_Tk611   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "qwertyui611")) 1
        !value_Tk711   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "qwertyui711")) 1
        !value_Tk811   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "qwertyui811")) 1
        !value_Tk911   = LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol "aa111", LedgerApiV2.TokenName "qwertyui911")) 1

        !testValuesList = [value_Tk1, value_Tk2, value_Tk3, value_Tk4, value_Tk5, value_Tk6, value_Tk7, value_Tk8, value_Tk9, value_Tk11, value_Tk21, value_Tk31, value_Tk41, value_Tk51, value_Tk61, value_Tk71, value_Tk81, value_Tk91, value_Tk111, value_Tk211, value_Tk311, value_Tk411, value_Tk511, value_Tk611, value_Tk711, value_Tk811, value_Tk911]

        !testValues =  [ foldl (<>) (LedgerAda.lovelaceValueOf 0) (P.take i testValuesList) | i <- [0..19] ]
       
        testCases :: LedgerValue.Value -> P.IO ()
        testCases = do
            evaluateCase

    in
        mapM_ testCases testValues

---------------------------------------------------

evaluateScriptMint :: LedgerApiV2.MintingPolicy -> [PlutusTx.Data] -> (P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, Integer)
evaluateScriptMint p datas =
    let
        getScriptMintingPolicy :: LedgerApiV2.MintingPolicy -> LedgerApiV2.Script
        getScriptMintingPolicy    = LedgerApiV2.getMintingPolicy

        getScriptShortBs :: LedgerApiV2.Script -> DataByteStringShort.ShortByteString
        getScriptShortBs = DataByteStringShort.toShort . DataByteStringLazy.toStrict . CodecSerialise.serialise

        exBudget :: LedgerApiV2.ExBudget
        exBudget = LedgerApiV2.ExBudget 10000000000 14000000

        !pv = LedgerApiV2.ProtocolVersion 6 0
        !scriptMintingPolicyV2 = getScriptMintingPolicy p
        !scriptShortBsV2 = getScriptShortBs scriptMintingPolicyV2
        !(_, e) = LedgerApiV2.evaluateScriptRestricting pv LedgerApiV2.Verbose LedgerEvaluationContextV2.evalCtxForTesting exBudget scriptShortBsV2 datas
        !size = LedgerScriptsV1.scriptSize scriptMintingPolicyV2
   in
        (e, size)

---------------------------------------------------

evaluateCase :: LedgerValue.Value -> P.IO ()
evaluateCase v = 
    let
        !curSymbol1 = curSymbol policy1
        !curSymbol2 = curSymbol policy2

        exampleTxOutRef :: LedgerApiV2.TxOutRef
        exampleTxOutRef = LedgerApiV2.TxOutRef {
            LedgerApiV2.txOutRefId = "aaccff",
            LedgerApiV2.txOutRefIdx = 10
        }

        exampleAddress :: LedgerAddress.Address
        exampleAddress =  LedgerAddress.Address {LedgerApiV2.addressCredential =  LedgerApiV2.PubKeyCredential $ LedgerApiV2.PubKeyHash  "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e" , LedgerApiV2.addressStakingCredential =  Nothing }

        caseValue = length $ LedgerValue.flattenValue v

        value1 = v
        value2 = v
        value3 = value1 <> value2

        mockInput1 :: LedgerApiV2.TxInInfo
        mockInput1 =
            LedgerApiV2.TxInInfo
                exampleTxOutRef
                (LedgerApiV2.TxOut
                    exampleAddress -- txOutAddress :: Address	 
                    value1 -- txOutValue :: Value	 
                    (LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData ()) -- txOutDatum :: OutputDatum	
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )

        mockInput2 :: LedgerApiV2.TxInInfo
        mockInput2 =
            LedgerApiV2.TxInInfo
                exampleTxOutRef
                (LedgerApiV2.TxOut
                    exampleAddress -- txOutAddress :: Address	 
                    value2 -- txOutValue :: Value	 
                    (LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData ()) -- txOutDatum :: OutputDatum	
                    DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash
                )        

        mockOutput1 :: LedgerApiV2.TxOut
        mockOutput1 =
            LedgerApiV2.TxOut
                exampleAddress -- txOutAddress :: Address	 
                value3 -- txOutValue :: Value	 
                (LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData ()) -- txOutDatum :: OutputDatum	
                DataMaybe.Nothing -- txOutReferenceScript :: Maybe ScriptHash

        --------------------------------

        redeemer_For_Mint = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData ()

        mockTxInfoInputs :: [LedgerApiV2.TxInInfo]
        mockTxInfoInputs = [ mockInput1, mockInput2 ]

        mockTxInfoOutputs :: [LedgerApiV2.TxOut]
        mockTxInfoOutputs = [ mockOutput1 ]

        mockTxInfoMint :: LedgerValue.Value
        mockTxInfoMint = LedgerAda.lovelaceValueOf 0

        mockScriptPurposeMint1 :: LedgerApiV2.ScriptPurpose    
        mockScriptPurposeMint1 = LedgerApiV2.Minting curSymbol1

        mockScriptPurposeMint2 :: LedgerApiV2.ScriptPurpose    
        mockScriptPurposeMint2 = LedgerApiV2.Minting curSymbol2

        mockRedeemerMint1 :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerMint1 = (mockScriptPurposeMint1, redeemer_For_Mint)

        mockRedeemerMint2 :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)
        mockRedeemerMint2 = (mockScriptPurposeMint2, redeemer_For_Mint)

        mockTxInfoRedeemers1 :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers1 = LedgerApiV2.fromList
            [
                mockRedeemerMint1
            ]

        mockTxInfoRedeemers2 :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers2 = LedgerApiV2.fromList
            [
                mockRedeemerMint2
            ]

        !now = LedgerApiV2.POSIXTime 1000000
        !intervalOffset1 = 1000
        !intervalOffset2 = 15000
        !validityRange   = Ledger.interval ( now - intervalOffset1 ) (now + intervalOffset2)

        mockCtx1 :: LedgerApiV2.ScriptContext
        mockCtx1 =
            LedgerApiV2.ScriptContext
                (
                LedgerApiV2.TxInfo
                    mockTxInfoInputs -- txInfoInputs :: [TxInInfo]	
                    [ ] -- txInfoReferenceInputs :: [TxInInfo]
                    mockTxInfoOutputs -- txInfoOutputs :: [TxOut]	
                    (LedgerAda.lovelaceValueOf 50000) -- txInfoFee :: Value	
                    mockTxInfoMint -- txInfoMint :: Value	
                    [] -- txInfoDCert :: [DCert]	
                    (LedgerApiV2.fromList []) -- txInfoWdrl :: Map StakingCredential Integer	
                    validityRange -- txInfoValidRange :: POSIXTimeRange	
                    []  -- txInfoSignatories :: [PubKeyHash]	
                    mockTxInfoRedeemers1 -- txInfoRedeemers :: Map ScriptPurpose Redeemer	
                    (LedgerApiV2.fromList [])  -- txInfoData :: Map DatumHash Datum	
                    (LedgerApiV2.TxId "555") -- txInfoId :: TxId 
                )
                mockScriptPurposeMint1 -- scriptContextPurpose :: ScriptPurpose

        mockCtx2 :: LedgerApiV2.ScriptContext
        mockCtx2 =
            LedgerApiV2.ScriptContext
                (
                LedgerApiV2.TxInfo
                    mockTxInfoInputs -- txInfoInputs :: [TxInInfo]	
                    [ ] -- txInfoReferenceInputs :: [TxInInfo]
                    mockTxInfoOutputs -- txInfoOutputs :: [TxOut]	
                    (LedgerAda.lovelaceValueOf 50000) -- txInfoFee :: Value	
                    mockTxInfoMint -- txInfoMint :: Value	
                    [] -- txInfoDCert :: [DCert]	
                    (LedgerApiV2.fromList []) -- txInfoWdrl :: Map StakingCredential Integer	
                    validityRange -- txInfoValidRange :: POSIXTimeRange	
                    []  -- txInfoSignatories :: [PubKeyHash]	
                    mockTxInfoRedeemers2 -- txInfoRedeemers :: Map ScriptPurpose Redeemer	
                    (LedgerApiV2.fromList [])  -- txInfoData :: Map DatumHash Datum	
                    (LedgerApiV2.TxId "555") -- txInfoId :: TxId 
                )
                mockScriptPurposeMint2 -- scriptContextPurpose :: ScriptPurpose

        !datas1 = [ LedgerApiV2.toData redeemer_For_Mint, LedgerApiV2.toData mockCtx1]
        !datas2 = [ LedgerApiV2.toData redeemer_For_Mint, LedgerApiV2.toData mockCtx2]

        (e1, size1) = evaluateScriptMint policy1 datas1
        (e2, size2) = evaluateScriptMint policy2 datas2
        (e3, size3) = evaluateScriptMint policy1_fromPlutonomy datas1
        (e4, size4) = evaluateScriptMint policy2_fromPlutonomy datas2

    in do
        P.putStrLn "-----"
        P.putStrLn $ "Case: " ++ P.show caseValue 
        case e1 of
            Left evalErr -> do
                P.putStrLn "With (==)"
                P.putStrLn $ "Eval Error: " ++  P.show evalErr
            Right exbudget -> do 
                P.putStrLn "With (==)"
                P.putStrLn $ "Ex Budget: " ++  P.show exbudget ++ " - Script size: " ++  P.show size1

        case e2 of
            Left evalErr -> do
                P.putStrLn "With valueEqualsValue"
                P.putStrLn $ "Eval Error: " ++ P.show evalErr
            Right exbudget -> do 
                P.putStrLn "With valueEqualsValue"
                P.putStrLn $ "Ex Budget: " ++  P.show exbudget ++ " - Script size: " ++  P.show size2

        case e3 of
            Left evalErr -> do
                P.putStrLn "With (== and Plutonomy)"
                P.putStrLn $ "Eval Error: " ++ P.show evalErr
            Right exbudget -> do 
                P.putStrLn "With (== and Plutonomy)"
                P.putStrLn $ "Ex Budget: " ++  P.show exbudget ++ " - Script size: " ++  P.show size3

        case e4 of
            Left evalErr -> do
                P.putStrLn "With valueEqualsValue and Plutonomy" 
                P.putStrLn $ "Eval Error: " ++ P.show evalErr
            Right exbudget -> do 
                P.putStrLn "With valueEqualsValue and Plutonomy"
                P.putStrLn $ "Ex Budget: " ++  P.show exbudget ++ " - Script size: " ++  P.show size4


--------------------------------------------------------------------------------
