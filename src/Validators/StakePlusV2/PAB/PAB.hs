-- {-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
-- {-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase                 #-}
-- {-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
-- {-# LANGUAGE OverloadedStrings          #-}
-- {-# LANGUAGE RecordWildCards            #-}
-- {-# LANGUAGE ScopedTypeVariables        #-}
-- {-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
-- {-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE TupleSections              #-}
-- {-# LANGUAGE AllowAmbiguousTypes        #-}
-- {-# LANGUAGE NumericUnderscores         #-}
-- {- HLINT ignore "Use camelCase" -}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}
------------------------------------------------------------------------------------------
module Validators.StakePlusV2.PAB.PAB where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Data.Aeson                                         as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                                as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics                                       as GHCGenerics (Generic)
import qualified Plutus.PAB.Effects.Contract.Builtin                as PABEffectsContractBuiltin (Empty, HasDefinitions (..), SomeBuiltin (..), endpointsToSchemas)
import           PlutusTx.Prelude                                   hiding (unless)
import qualified Prelude                                            as P
import qualified Prettyprinter                                      (Pretty (..), viaShow)
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.OffChain.EndPoints          as OffChain
import qualified Validators.StakePlusV2.OffChain.EndPointsMaster1   as OffChain
import qualified Validators.StakePlusV2.OffChain.EndPointsMaster2   as OffChain
import qualified Validators.StakePlusV2.OffChain.EndPointsUser      as OffChain
import qualified Validators.StakePlusV2.Types.Examples              as T
import qualified Validators.StakePlusV2.Types.PABParams             as T
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------

data ValidatorContracts = 
    BalanceAtScript T.PABBalanceAtScriptParams  |
    BalanceAtScriptFull T.PABBalanceAtScriptFullParams  |
    SplitUtxO T.PABSplitUtxOParams  |
    MasterMintFree T.PABMasterMintFreeParams  |
    MasterPreparePool T.PABMasterPreparePoolParams  |
    MasterFund T.PABMasterFundParams |
    MasterFundAndMerge T.PABMasterFundAndMergeParams  |
    MasterSplitFund T.PABMasterSplitFundParams |
    MasterClosePool T.PABMasterClosePoolParams |
    MasterTerminatePool T.PABMasterTerminatePoolParams |
    MasterEmergency T.PABMasterEmergencyParams |
    MasterDeleteFund T.PABMasterDeleteFundParams |
    MasterSendBackFund T.PABMasterSendBackFundParams |
    MasterSendBackDeposit T.PABMasterSendBackDepositParams |
    MasterAddScripts T.PABMasterAddScriptsParams |
    MasterDeleteScripts T.PABMasterDeleteScriptsParams |
    UserDeposit T.PABUserDepositParams  |
    UserHarvest T.PABUserHarvestParams |
    UserWithdraw T.PABUserWithdrawParams
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema) 

instance Prettyprinter.Pretty ValidatorContracts where
    pretty = Prettyprinter.viaShow

instance PABEffectsContractBuiltin.HasDefinitions ValidatorContracts where

    getDefinitions        = [
            BalanceAtScript T.examplePABBalanceAtScriptParams,
            BalanceAtScriptFull T.examplePABBalanceAtScriptFullParams,
            SplitUtxO T.examplePABSplitUtxOParams,
            MasterMintFree T.examplePABMasterMintFreeParams,
            MasterPreparePool T.examplePABMasterPreparePoolParams,
            MasterFund T.examplePABMasterFundParams,
            MasterFundAndMerge T.examplePABMasterFundAndMergeParams,
            MasterSplitFund T.examplePABMasterSplitFundParams,
            MasterClosePool T.examplePABMasterClosePoolParams,
            MasterTerminatePool T.examplePABMasterTerminatePoolParams,
            MasterEmergency T.examplePABMasterEmergencyParams,
            MasterDeleteFund T.examplePABMasterDeleteFundParams,
            MasterSendBackFund T.examplePABMasterSendBackFundParams,
            MasterSendBackDeposit T.examplePABMasterSendBackDepositParams,
            MasterAddScripts T.examplePABMasterAddScriptsParams,
            MasterDeleteScripts T.examplePABMasterDeleteScriptsParams,
            UserDeposit T.examplePABUserDepositParams,
            UserHarvest T.examplePABUserHarvestParams,
            UserWithdraw T.examplePABUserWithdrawParams 

        ]

    getContract (BalanceAtScript mbParams)=              PABEffectsContractBuiltin.SomeBuiltin $ OffChain.balanceAtScript @() @PABEffectsContractBuiltin.Empty mbParams
    getContract (BalanceAtScriptFull mbParams)=          PABEffectsContractBuiltin.SomeBuiltin $ OffChain.balanceAtScriptFull @() @PABEffectsContractBuiltin.Empty mbParams
    getContract (SplitUtxO mcParams)=                    PABEffectsContractBuiltin.SomeBuiltin $ OffChain.splitUtxO @() @PABEffectsContractBuiltin.Empty mcParams
    getContract (MasterMintFree mcParams)=               PABEffectsContractBuiltin.SomeBuiltin $ OffChain.masterMintFree @() @PABEffectsContractBuiltin.Empty mcParams
    getContract (MasterPreparePool mcParams)=                 PABEffectsContractBuiltin.SomeBuiltin $ OffChain.masterPreparePool @() @PABEffectsContractBuiltin.Empty mcParams
    getContract (MasterFund mfpParams) =                 PABEffectsContractBuiltin.SomeBuiltin $ OffChain.masterFund @() @PABEffectsContractBuiltin.Empty mfpParams
    getContract (MasterFundAndMerge mfampParams) =       PABEffectsContractBuiltin.SomeBuiltin $ OffChain.masterFundAndMerge @() @PABEffectsContractBuiltin.Empty mfampParams
    getContract (MasterSplitFund mcpParams)=                 PABEffectsContractBuiltin.SomeBuiltin $ OffChain.masterSplitFund @() @PABEffectsContractBuiltin.Empty mcpParams
    getContract (MasterClosePool mcpParams)=                 PABEffectsContractBuiltin.SomeBuiltin $ OffChain.masterClosePool @() @PABEffectsContractBuiltin.Empty mcpParams
    getContract (MasterTerminatePool mcpParams)=             PABEffectsContractBuiltin.SomeBuiltin $ OffChain.masterTerminatePool @() @PABEffectsContractBuiltin.Empty mcpParams
    getContract (MasterEmergency mcpParams)=             PABEffectsContractBuiltin.SomeBuiltin $ OffChain.masterEmergency @() @PABEffectsContractBuiltin.Empty mcpParams
    getContract (MasterDeleteFund mcpParams)=                PABEffectsContractBuiltin.SomeBuiltin $ OffChain.masterDeleteFund @() @PABEffectsContractBuiltin.Empty mcpParams
    getContract (MasterSendBackFund mgbfParams)=         PABEffectsContractBuiltin.SomeBuiltin $ OffChain.masterSendBackFund @() @PABEffectsContractBuiltin.Empty mgbfParams
    getContract (MasterSendBackDeposit mgbfParams)=       PABEffectsContractBuiltin.SomeBuiltin $ OffChain.masterSendBackDeposit @() @PABEffectsContractBuiltin.Empty mgbfParams
    getContract (MasterAddScripts mcpParams)=            PABEffectsContractBuiltin.SomeBuiltin $ OffChain.masterAddScripts @() @PABEffectsContractBuiltin.Empty mcpParams
    getContract (MasterDeleteScripts mcpParams)=         PABEffectsContractBuiltin.SomeBuiltin $ OffChain.masterDeleteScripts @() @PABEffectsContractBuiltin.Empty mcpParams
    getContract (UserDeposit uiParams)=                   PABEffectsContractBuiltin.SomeBuiltin $ OffChain.userDeposit @() @PABEffectsContractBuiltin.Empty uiParams
    getContract (UserHarvest ugrParams)=              PABEffectsContractBuiltin.SomeBuiltin $ OffChain.userHarvest @() @PABEffectsContractBuiltin.Empty ugrParams
    getContract (UserWithdraw ugbiParams)=          PABEffectsContractBuiltin.SomeBuiltin $ OffChain.userWithdraw @() @PABEffectsContractBuiltin.Empty ugbiParams

    getSchema = const $ PABEffectsContractBuiltin.endpointsToSchemas @PABEffectsContractBuiltin.Empty
    