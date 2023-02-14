{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE LambdaCase                 #-}
-- {-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
-- {-# LANGUAGE OverloadedStrings          #-}
--{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- {-# LANGUAGE TemplateHaskell            #-}
-- {-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
--{-# LANGUAGE RankNTypes                 #-}
--{-# LANGUAGE TupleSections              #-}
--{-# LANGUAGE AllowAmbiguousTypes        #-}
-- {-# LANGUAGE NumericUnderscores         #-}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Use camelCase" -}

------------------------------------------------------------------------------------------
module Validators.StakePlusV2.Types.PABParams where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Ledger.Address                      as LedgerAddress
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import           PlutusTx.Prelude                    (Integer, BuiltinByteString)
import qualified Prelude                             as P
import qualified Schema                              
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import Validators.StakePlusV2.Types.Types           ( Master, PoolParams )               
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------

instance Schema.ToSchema LedgerApiV2.Validator where
  toSchema = Schema.FormSchemaUnit

instance Schema.ToSchema  LedgerAddress.Address where
  toSchema = Schema.FormSchemaUnit

instance Schema.ToSchema   LedgerApiV2.MintingPolicy where
  toSchema = Schema.FormSchemaUnit

data PABPoolParams = PABPoolParams
    {   
        pppPoolParams :: PoolParams,

        pppStaking_UI :: P.String,
        pppHarvest_UI :: P.String,

        pppPoolID_TxOutRef :: LedgerApiV2.TxOutRef,

        pppValidator :: LedgerApiV2.Validator,
        pppValidatorAddress :: LedgerAddress.Address,
        pppValidatorHash :: LedgerApiV2.ValidatorHash,

        pppPolicy_PoolID :: LedgerApiV2.MintingPolicy,
        pppCurSymbol_PoolID :: LedgerApiV2.CurrencySymbol,

        pppPolicy_TxID_Master_Fund :: LedgerApiV2.MintingPolicy,
        pppPolicy_TxID_Master_FundAndMerge :: LedgerApiV2.MintingPolicy,
        pppPolicy_TxID_Master_SplitFund :: LedgerApiV2.MintingPolicy,
        pppPolicy_TxID_Master_ClosePool :: LedgerApiV2.MintingPolicy,
        pppPolicy_TxID_Master_TerminatePool :: LedgerApiV2.MintingPolicy,
        pppPolicy_TxID_Master_Emergency :: LedgerApiV2.MintingPolicy,
        pppPolicy_TxID_Master_DeleteFund :: LedgerApiV2.MintingPolicy,
        pppPolicy_TxID_Master_SendBackFund :: LedgerApiV2.MintingPolicy,
        pppPolicy_TxID_Master_SendBackDeposit :: LedgerApiV2.MintingPolicy,
        pppPolicy_TxID_Master_AddScripts :: LedgerApiV2.MintingPolicy,
        pppPolicy_TxID_Master_DeleteScripts :: LedgerApiV2.MintingPolicy,

        pppPolicy_TxID_User_Deposit :: LedgerApiV2.MintingPolicy,
        pppPolicy_TxID_User_Withdraw :: LedgerApiV2.MintingPolicy,
        pppPolicy_TxID_User_Harvest :: LedgerApiV2.MintingPolicy,

        pppCurSymbol_TxID_Master_Fund :: LedgerApiV2.CurrencySymbol,
        pppCurSymbol_TxID_Master_FundAndMerge :: LedgerApiV2.CurrencySymbol,
        pppCurSymbol_TxID_Master_SplitFund :: LedgerApiV2.CurrencySymbol,
        pppCurSymbol_TxID_Master_ClosePool :: LedgerApiV2.CurrencySymbol,
        pppCurSymbol_TxID_Master_TerminatePool :: LedgerApiV2.CurrencySymbol,
        pppCurSymbol_TxID_Master_Emergency :: LedgerApiV2.CurrencySymbol,
        pppCurSymbol_TxID_Master_DeleteFund :: LedgerApiV2.CurrencySymbol,
        pppCurSymbol_TxID_Master_SendBackFund :: LedgerApiV2.CurrencySymbol,
        pppCurSymbol_TxID_Master_SendBackDeposit :: LedgerApiV2.CurrencySymbol,
        pppCurSymbol_TxID_Master_AddScripts :: LedgerApiV2.CurrencySymbol,
        pppCurSymbol_TxID_Master_DeleteScripts :: LedgerApiV2.CurrencySymbol,

        pppCurSymbol_TxID_User_Deposit :: LedgerApiV2.CurrencySymbol,
        pppCurSymbol_TxID_User_Withdraw :: LedgerApiV2.CurrencySymbol,
        pppCurSymbol_TxID_User_Harvest :: LedgerApiV2.CurrencySymbol

    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

newtype PABBalanceAtScriptParams = PABBalanceAtScriptParams
    { 
        pbPABPoolParams:: PABPoolParams
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

newtype PABBalanceAtScriptFullParams = PABBalanceAtScriptFullParams
    { 
        pbfPABPoolParams:: PABPoolParams
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

newtype PABSplitUtxOParams = PABSplitUtxOParams
    { 
        psuSplitAmount :: Integer
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

data PABMasterMintFreeParams = PABMasterMintFreeParams
    { 
        -- pmmfPABPoolParams:: PABPoolParams, 
        pmmfMintPolicyNum :: Integer,
        pmmfMintTokenNameBase :: BuiltinByteString,
        pmmfMintDiifTokenNameCount :: Integer,
        pmmfMintAmount :: Integer
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

data PABMasterPreparePoolParams = PABMasterPreparePoolParams
    { 
        pmcpPABPoolParams:: PABPoolParams, 
        pmcpPoolID_TxOutRef :: LedgerApiV2.TxOutRef
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

data PABMasterFundParams = PABMasterFundParams
    { 
        pmfpPABPoolParams :: PABPoolParams,
        pmfpFundAmount    :: Integer
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

data PABMasterFundAndMergeParams = PABMasterFundAndMergeParams
    { 
        pmfampPABPoolParams :: PABPoolParams, 
        pmfampFundIDs_TxOutRefs :: [LedgerApiV2.TxOutRef],
        pmfampFundAmount    :: Integer
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

data PABMasterSplitFundParams = PABMasterSplitFundParams
    { 
        pmsPABPoolParams :: PABPoolParams,
        pmsFundID_TxOutRef :: LedgerApiV2.TxOutRef,
        pmsSplitFundAmount    :: Integer
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

newtype PABMasterClosePoolParams = PABMasterClosePoolParams
    { 
        pmcPABPoolParams :: PABPoolParams
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

newtype PABMasterTerminatePoolParams = PABMasterTerminatePoolParams
    { 
        pmtPABPoolParams :: PABPoolParams
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

newtype PABMasterEmergencyParams = PABMasterEmergencyParams
    { 
        pmePABPoolParams :: PABPoolParams
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

data PABMasterDeleteFundParams = PABMasterDeleteFundParams
    { 
        pmdPABPoolParams :: PABPoolParams,
        pmdFundIDs_TxOutRefs :: [LedgerApiV2.TxOutRef]
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

data PABMasterSendBackFundParams = PABMasterSendBackFundParams
    { 
        pmsbfPABPoolParams :: PABPoolParams, 
        pmsbfMasterToSendBack    :: Master
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

data PABMasterSendBackDepositParams = PABMasterSendBackDepositParams
    { 
        pmsbiPABPoolParams :: PABPoolParams, 
        pmsbiUserID_TxOutRef :: LedgerApiV2.TxOutRef
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

newtype PABMasterAddScriptsParams = PABMasterAddScriptsParams
    { 
        pmasPABPoolParams :: PABPoolParams
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

newtype PABMasterDeleteScriptsParams = PABMasterDeleteScriptsParams
    { 
        pmdsPABPoolParams :: PABPoolParams
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

data PABUserDepositParams = PABUserDepositParams
    { 
        puiPABPoolParams :: PABPoolParams, 
        puiInvestAmount    :: Integer
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

data PABUserHarvestParams = PABUserHarvestParams
    { 
        pugrPABPoolParams :: PABPoolParams, 
        pugrUserID_TxOutRef :: LedgerApiV2.TxOutRef,
        pugrClaimAmount    :: Integer 
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------


data PABUserWithdrawParams = PABUserWithdrawParams
    { 
        pugbiPABPoolParams :: PABPoolParams, 
        pugbiUserID_TxOutRef :: LedgerApiV2.TxOutRef
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------
