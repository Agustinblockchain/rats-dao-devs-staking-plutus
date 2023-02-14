{-# LANGUAGE DataKinds                  #-}
--{-# LANGUAGE DeriveAnyClass             #-}
--{-# LANGUAGE DeriveGeneric              #-}
--{-# LANGUAGE DerivingStrategies         #-}
--{-# LANGUAGE FlexibleContexts           #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
--{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
--{-# LANGUAGE TypeApplications           #-}
--{-# LANGUAGE TypeFamilies               #-}
--{-# LANGUAGE TypeOperators              #-}
--{-# LANGUAGE RankNTypes                 #-}
--{-# LANGUAGE TupleSections              #-}
--{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE NumericUnderscores         #-}
{- HLINT ignore "Use camelCase" -}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}
-----------------------------------------------------------------------------------------
module Validators.StakePlusV2.Types.Examples where
-----------------------------------------------------------------------------------------
-- Import Externos
-----------------------------------------------------------------------------------------
import qualified Ledger.Address                             as LedgerAddress
import qualified Plutonomy      
import qualified Plutus.V2.Ledger.Api                       as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude                                 
import qualified Prelude                                    as P
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import Validators.StakePlusV2.OnChain.Tokens.PoolID.Policy
import Validators.StakePlusV2.Types.Constants                
import Validators.StakePlusV2.Types.DatumsValidator      
import Validators.StakePlusV2.Types.PABParams           
import Validators.StakePlusV2.Types.RedeemersMint         
import Validators.StakePlusV2.Types.RedeemersValidator    
import Validators.StakePlusV2.Types.Types                
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------

examplePOSIXTime :: LedgerApiV2.POSIXTime
examplePOSIXTime = 1658172331000

exampleTxOutRef :: LedgerApiV2.TxOutRef
exampleTxOutRef = LedgerApiV2.TxOutRef {
            LedgerApiV2.txOutRefId = "aaccff",
            LedgerApiV2.txOutRefIdx = 10
        }

exampleTxOutRef1 :: LedgerApiV2.TxOutRef
exampleTxOutRef1 = LedgerApiV2.TxOutRef {
            LedgerApiV2.txOutRefId = "aacc11",
            LedgerApiV2.txOutRefIdx = 11
        }
        
exampleAddress :: LedgerAddress.Address
exampleAddress =  LedgerAddress.Address {LedgerApiV2.addressCredential =  LedgerApiV2.PubKeyCredential $ LedgerApiV2.PubKeyHash  "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e" , LedgerApiV2.addressStakingCredential =  Nothing }

exampleAddress1 :: LedgerAddress.Address
exampleAddress1 =  LedgerAddress.Address {LedgerApiV2.addressCredential =  LedgerApiV2.PubKeyCredential $ LedgerApiV2.PubKeyHash  "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c1ff" , LedgerApiV2.addressStakingCredential =  Nothing }

exampleHash :: LedgerApiV2.ValidatorHash 
exampleHash = LedgerApiV2.ValidatorHash "aaccff"

exampleMkValidator :: PlutusTx.BuiltinData  -> PlutusTx.BuiltinData  -> PlutusTx.BuiltinData  -> ()
exampleMkValidator _ _ _ = ()

exampleValidator :: LedgerApiV2.Validator
exampleValidator  =
    Plutonomy.optimizeUPLC $
      Plutonomy.validatorToPlutus $
        Plutonomy.mkValidatorScript $$(PlutusTx.compile [||exampleMkValidator||])

exampleMintingPolicy :: LedgerApiV2.MintingPolicy
exampleMintingPolicy = policy_PoolID [] exampleTxOutRef

exampleCS :: LedgerApiV2.CurrencySymbol 
exampleCS =  "CS"

exampleTN :: LedgerApiV2.TokenName
exampleTN =  "TN"

exampleMaster :: Master
exampleMaster = "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e"

exampleUser :: User
exampleUser = "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e"

exampleBBS :: LedgerApiV2.BuiltinByteString
exampleBBS = "aaccff"

exampleBool :: Bool
exampleBool = True

exampleInteger :: Integer
exampleInteger = 3_000_000

exampleString :: P.String
exampleString = "aaccff"

------------------------------------------------------------------------------------------

exampleMasterFunder :: MasterFunder
exampleMasterFunder = MasterFunder {
        mfMaster = exampleMaster,
        mfFundAmount = exampleInteger,
        mfClaimedFund = poolDatum_NotClaimedFund,
        mfMinAda = exampleInteger
    }

examplePoolDatum :: DatumValidator
examplePoolDatum = PoolDatum PoolDatumTypo { 
        pdMasterFunders = [exampleMasterFunder],
        pdFundCount = 0,
        pdTotalCashedOut = 0,
        pdClosedAt = Just examplePOSIXTime,
        pdIsTerminated = poolDatum_NotTerminated,
        pdIsEmergency = poolDatum_NotEmergency,
        pdMinAda = exampleInteger
    }

exampleFundDatum :: DatumValidator
exampleFundDatum = FundDatum FundDatumTypo { 
        fdFundAmount = exampleInteger,
        fdCashedOut = exampleInteger,
        fdMinAda  = exampleInteger
    }

exampleUserDatum :: DatumValidator
exampleUserDatum = UserDatum UserDatumTypo { 
        udUser = exampleUser, 
        udInvest = exampleInteger,
        udCreatedAt = examplePOSIXTime,
        udCashedOut = exampleInteger,
        udRewardsNotClaimed = exampleInteger,
        udLastClaimAt = Just examplePOSIXTime,
        udMinAda = exampleInteger
    }

------------------------------------------------------------------------------------------

exampleRedeemerMasterFund  :: RedeemerValidator
exampleRedeemerMasterFund  = RedeemerMasterFund RedeemerMasterFundTypo { 
        rmfMaster = exampleMaster,
        rmfFundAmount = exampleInteger,
        rmfMinAda = exampleInteger
    } 

------------------------------------------------------------------------------------------

exampleRedeemerMint_TxID :: Redeemer_TxID
exampleRedeemerMint_TxID = RedeemerMint_TxID RedeemerMint_TxIDTypo { 
        mrRedeemerValidator = exampleRedeemerMasterFund
    } 

exampleRedeemerBurn_TxID :: Redeemer_TxID
exampleRedeemerBurn_TxID = RedeemerBurn_TxID RedeemerBurn_TxIDTypo  { 
    } 

------------------------------------------------------------------------------------------

examplePoolParams :: PoolParams
examplePoolParams = PoolParams
    { 
        ppPoolID_CS  = exampleCS,

        ppMasters = [exampleMaster],   

        ppBeginAt = examplePOSIXTime,

        ppDeadline = examplePOSIXTime + 100000000,

        ppGraceTime = 100000000,

        ppStaking_CS = exampleCS,
        ppStaking_TN = exampleTN,

        ppHarvest_CS = exampleCS,
        ppHarvest_TN = exampleTN,

        ppInterestRates = [InterestRate { iMinDays = Just 90, iPercentage = 1 }, InterestRate { iMinDays = Just 180, iPercentage = 2 }, InterestRate { iMinDays = Nothing, iPercentage = 3 }]

    }

------------------------------------------------------------------------------------------

examplePABPoolParams :: PABPoolParams
examplePABPoolParams = PABPoolParams
    {   
        pppPoolParams = examplePoolParams,

        pppStaking_UI = exampleString,
        pppHarvest_UI = exampleString,

        pppPoolID_TxOutRef = exampleTxOutRef,

        pppValidator =  exampleValidator,
        pppValidatorAddress = exampleAddress,
        pppValidatorHash = exampleHash,

        pppPolicy_PoolID = exampleMintingPolicy,
        pppCurSymbol_PoolID = exampleCS,

        pppPolicy_TxID_Master_Fund = exampleMintingPolicy,
        pppPolicy_TxID_Master_FundAndMerge = exampleMintingPolicy,
        pppPolicy_TxID_Master_SplitFund = exampleMintingPolicy,
        pppPolicy_TxID_Master_ClosePool = exampleMintingPolicy,
        pppPolicy_TxID_Master_TerminatePool = exampleMintingPolicy,
        pppPolicy_TxID_Master_Emergency = exampleMintingPolicy,
        pppPolicy_TxID_Master_DeleteFund = exampleMintingPolicy,
        pppPolicy_TxID_Master_SendBackFund = exampleMintingPolicy,
        pppPolicy_TxID_Master_SendBackDeposit = exampleMintingPolicy,
        pppPolicy_TxID_Master_AddScripts = exampleMintingPolicy,
        pppPolicy_TxID_Master_DeleteScripts = exampleMintingPolicy,

        pppPolicy_TxID_User_Deposit = exampleMintingPolicy,
        pppPolicy_TxID_User_Harvest = exampleMintingPolicy,
        pppPolicy_TxID_User_Withdraw = exampleMintingPolicy,

        pppCurSymbol_TxID_Master_Fund = exampleCS,
        pppCurSymbol_TxID_Master_FundAndMerge = exampleCS,
        pppCurSymbol_TxID_Master_SplitFund = exampleCS,
        pppCurSymbol_TxID_Master_ClosePool = exampleCS,
        pppCurSymbol_TxID_Master_TerminatePool = exampleCS,
        pppCurSymbol_TxID_Master_Emergency = exampleCS,
        pppCurSymbol_TxID_Master_DeleteFund = exampleCS,
        pppCurSymbol_TxID_Master_SendBackFund = exampleCS,
        pppCurSymbol_TxID_Master_SendBackDeposit= exampleCS,
        pppCurSymbol_TxID_Master_AddScripts = exampleCS,
        pppCurSymbol_TxID_Master_DeleteScripts = exampleCS,

        pppCurSymbol_TxID_User_Deposit = exampleCS,
        pppCurSymbol_TxID_User_Harvest = exampleCS,
        pppCurSymbol_TxID_User_Withdraw = exampleCS
    } 

------------------------------------------------------------------------------------------


examplePABBalanceAtScriptParams :: PABBalanceAtScriptParams 
examplePABBalanceAtScriptParams = PABBalanceAtScriptParams
    { 
        pbPABPoolParams = examplePABPoolParams
    } 

examplePABBalanceAtScriptFullParams :: PABBalanceAtScriptFullParams 
examplePABBalanceAtScriptFullParams = PABBalanceAtScriptFullParams
    { 
        pbfPABPoolParams = examplePABPoolParams
    } 

examplePABSplitUtxOParams :: PABSplitUtxOParams 
examplePABSplitUtxOParams = PABSplitUtxOParams
    { 
        psuSplitAmount = exampleInteger
    } 


examplePABMasterMintFreeParams :: PABMasterMintFreeParams
examplePABMasterMintFreeParams = PABMasterMintFreeParams
    { 
        -- pmmfPABPoolParams = examplePABPoolParams, 
        pmmfMintPolicyNum = exampleInteger,
        pmmfMintTokenNameBase = exampleBBS,
        pmmfMintDiifTokenNameCount = exampleInteger,
        pmmfMintAmount  = exampleInteger
    } 

examplePABMasterPreparePoolParams :: PABMasterPreparePoolParams
examplePABMasterPreparePoolParams = PABMasterPreparePoolParams
    { 
        pmcpPABPoolParams = examplePABPoolParams, 
        pmcpPoolID_TxOutRef = exampleTxOutRef
    } 

examplePABMasterFundParams :: PABMasterFundParams
examplePABMasterFundParams = PABMasterFundParams
    { 
        pmfpPABPoolParams = examplePABPoolParams, 
        pmfpFundAmount    = exampleInteger
    } 

examplePABMasterFundAndMergeParams :: PABMasterFundAndMergeParams
examplePABMasterFundAndMergeParams = PABMasterFundAndMergeParams
    { 
        pmfampPABPoolParams = examplePABPoolParams, 
        pmfampFundIDs_TxOutRefs = [exampleTxOutRef],
        pmfampFundAmount    = exampleInteger
    } 

examplePABMasterSplitFundParams :: PABMasterSplitFundParams
examplePABMasterSplitFundParams = PABMasterSplitFundParams
    { 
        pmsPABPoolParams = examplePABPoolParams,
        pmsFundID_TxOutRef = exampleTxOutRef,
        pmsSplitFundAmount = exampleInteger
    } 

examplePABMasterClosePoolParams :: PABMasterClosePoolParams
examplePABMasterClosePoolParams = PABMasterClosePoolParams
    { 
        pmcPABPoolParams = examplePABPoolParams
    } 

examplePABMasterTerminatePoolParams :: PABMasterTerminatePoolParams
examplePABMasterTerminatePoolParams = PABMasterTerminatePoolParams
    { 
        pmtPABPoolParams = examplePABPoolParams
    } 

examplePABMasterEmergencyParams :: PABMasterEmergencyParams
examplePABMasterEmergencyParams = PABMasterEmergencyParams
    { 
        pmePABPoolParams = examplePABPoolParams
    } 

examplePABMasterDeleteFundParams :: PABMasterDeleteFundParams
examplePABMasterDeleteFundParams = PABMasterDeleteFundParams
    { 
        pmdPABPoolParams = examplePABPoolParams,
        pmdFundIDs_TxOutRefs = [exampleTxOutRef]
    } 

examplePABMasterSendBackFundParams :: PABMasterSendBackFundParams
examplePABMasterSendBackFundParams = PABMasterSendBackFundParams
    { 
        pmsbfPABPoolParams = examplePABPoolParams,
        pmsbfMasterToSendBack = exampleMaster
    } 

examplePABMasterSendBackDepositParams :: PABMasterSendBackDepositParams
examplePABMasterSendBackDepositParams = PABMasterSendBackDepositParams
    { 
        pmsbiPABPoolParams = examplePABPoolParams,
        pmsbiUserID_TxOutRef = exampleTxOutRef
    } 

examplePABMasterAddScriptsParams :: PABMasterAddScriptsParams
examplePABMasterAddScriptsParams = PABMasterAddScriptsParams
    { 
        pmasPABPoolParams = examplePABPoolParams
    } 

examplePABMasterDeleteScriptsParams :: PABMasterDeleteScriptsParams
examplePABMasterDeleteScriptsParams = PABMasterDeleteScriptsParams
    { 
        pmdsPABPoolParams = examplePABPoolParams
    } 

examplePABUserDepositParams :: PABUserDepositParams
examplePABUserDepositParams = PABUserDepositParams
    { 
        puiPABPoolParams = examplePABPoolParams, 
        puiInvestAmount    = exampleInteger
    } 

examplePABUserHarvestParams :: PABUserHarvestParams
examplePABUserHarvestParams = PABUserHarvestParams
    { 
        pugrPABPoolParams = examplePABPoolParams, 
        pugrUserID_TxOutRef = exampleTxOutRef,
        pugrClaimAmount   = exampleInteger
    } 

examplePABUserWithdrawParams :: PABUserWithdrawParams
examplePABUserWithdrawParams = PABUserWithdrawParams
    { 
        pugbiPABPoolParams = examplePABPoolParams,
        pugbiUserID_TxOutRef = exampleTxOutRef
    } 
