{-# LANGUAGE DataKinds                  #-}
-- {-# LANGUAGE DeriveAnyClass             #-}
-- {-# LANGUAGE DeriveGeneric              #-}
-- {-# LANGUAGE DerivingStrategies         #-}
-- {-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase                 #-}
-- {-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
-- {-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
--{-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE TupleSections              #-}
-- {-# LANGUAGE AllowAmbiguousTypes        #-}
-- {-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}
{- HLINT ignore "Use camelCase" -}
------------------------------------------------------------------------------------------
module Validators.StakePlusV2.OnChain.TestInputsOutputs.Policy where
------------------------------------------------------------------------------------------
-- Import Externos
--------------------------------------------------------------------------------------------
import qualified Plutonomy  
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (txInfoOutputs, unsafeFromBuiltinData, txInfoInputs, txInfoReferenceInputs, txInInfoResolved, MintingPolicy)
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ScriptContext, scriptContextTxInfo)
import qualified PlutusTx                                                   (compile)
import           PlutusTx.Prelude                                           ( BuiltinData, error, ($) )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (tracetxOuts)
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicyTestInputs #-}
mkPolicyTestInputs :: BuiltinData -> BuiltinData -> ()
mkPolicyTestInputs _ ctxRaw = 
    let
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !txOutsInputs = [  LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        !result = OnChainHelpers.tracetxOuts txOutsInputs ctx
    in
        if result
        then () 
        else error ()        

--------------------------------------------------------------------------------

{-# INLINABLE mkPolicyTestInputsRef #-}
mkPolicyTestInputsRef :: BuiltinData -> BuiltinData -> ()
mkPolicyTestInputsRef _ ctxRaw = 
    let
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !txOutsInputsRef = [  LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoReferenceInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        !result = OnChainHelpers.tracetxOuts txOutsInputsRef ctx
    in
        if result
        then () 
        else error ()   

--------------------------------------------------------------------------------

{-# INLINABLE mkPolicyTestOutputs #-}
mkPolicyTestOutputs :: BuiltinData -> BuiltinData -> ()
mkPolicyTestOutputs _ ctxRaw = 
    let
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !result = OnChainHelpers.tracetxOuts (LedgerApiV2.txInfoOutputs (LedgerContextsV2.scriptContextTxInfo ctx)) ctx
    in
        if result
        then () 
        else error ()     

--------------------------------------------------------------------------------

policyTestInputs :: LedgerApiV2.MintingPolicy
policyTestInputs = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus original_policyTestInputs

original_policyTestInputs :: Plutonomy.MintingPolicy
original_policyTestInputs =
  Plutonomy.mkMintingPolicyScript
    $$(PlutusTx.compile [||  mkPolicyTestInputs ||]) 

--------------------------------------------------------------------------------

policyTestInputsRef :: LedgerApiV2.MintingPolicy
policyTestInputsRef = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus original_policyTestInputsRef

original_policyTestInputsRef :: Plutonomy.MintingPolicy
original_policyTestInputsRef =
  Plutonomy.mkMintingPolicyScript
    $$(PlutusTx.compile [||  mkPolicyTestInputsRef ||]) 

--------------------------------------------------------------------------------

policyTestOutputs :: LedgerApiV2.MintingPolicy
policyTestOutputs = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus original_policyOutputs

original_policyOutputs :: Plutonomy.MintingPolicy
original_policyOutputs =
  Plutonomy.mkMintingPolicyScript
    $$(PlutusTx.compile [||  mkPolicyTestOutputs ||])  

--------------------------------------------------------------------------------
