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
{-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE TupleSections              #-}
-- {-# LANGUAGE AllowAmbiguousTypes        #-}
-- {-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}
{- HLINT ignore "Use camelCase" -}
------------------------------------------------------------------------------------------
module Validators.StakePlusV2.OnChain.Tokens.PoolID.Policy
(
    policy_PoolID 
)
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ScriptContext, TxInfo, scriptContextTxInfo) 
import qualified PlutusTx   
import           PlutusTx.Prelude                                           ( Bool(False), BuiltinData, Ord((<), (>)), (&&), (||), error, ($), all, traceIfFalse )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (signedByMasters)
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (getOwnMintedTokenNameAndAmt, hasInputUTxO, validateMint_NFT_Own_CS_Any_TN, validateBurn_NFT_Own_CS_Any_TN)
import qualified Validators.StakePlusV2.Types.Types                         as T (Master)
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicyPoolID #-}
mkPolicyPoolID :: [T.Master] -> LedgerApiV2.TxOutRef -> BuiltinData -> BuiltinData -> ()
mkPolicyPoolID !masters !txOutRef _ !ctxRaw =
    let
        ------------------

        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw

        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx

        ------------------

        checkMinting:: Bool
        !checkMinting =
            case OnChainNFTHelpers.getOwnMintedTokenNameAndAmt ctx  of
                []  -> False
                x   -> all (\(_, amt) -> amt > 0) x
            -- Just (_, _, amt)   -> amt > 0
            -- _                   -> False

        ------------------

        checkBurning:: Bool
        !checkBurning =
            case OnChainNFTHelpers.getOwnMintedTokenNameAndAmt ctx  of
                []  -> False
                x   -> all (\(_, amt) -> amt < 0) x
            -- Just (_, _, amt)   -> amt < 0
            -- _                   -> False

    in
        if
            traceIfFalse "PPMSM"  (OnChainHelpers.signedByMasters masters info) && --  "Minting PoolID: Master's signature missing!!!"
            traceIfFalse "UTXO" ((checkMinting && OnChainNFTHelpers.hasInputUTxO txOutRef info) || checkBurning) && -- "Minting PoolID: UTxO not consumed"
            (
                (checkMinting && OnChainNFTHelpers.validateMint_NFT_Own_CS_Any_TN ctx ) ||
                (checkBurning && OnChainNFTHelpers.validateBurn_NFT_Own_CS_Any_TN ctx)
            )

        then ()

        else error ()

-- --------------------------------------------------------------------------------

{-# INLINEABLE policy_PoolID #-}
policy_PoolID :: [T.Master] -> LedgerApiV2.TxOutRef -> LedgerApiV2.MintingPolicy
policy_PoolID masters txOutRef  = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy masters txOutRef

{-# INLINEABLE original_policy #-}
original_policy :: [T.Master] -> LedgerApiV2.TxOutRef -> Plutonomy.MintingPolicy
original_policy masters txOutRef  =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkPolicyPoolID ||])
    `PlutusTx.applyCode` PlutusTx.liftCode masters
    `PlutusTx.applyCode` PlutusTx.liftCode txOutRef

--------------------------------------------------------------------------------
