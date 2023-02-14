{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE NumericUnderscores         #-}
-- {-# LANGUAGE MonoLocalBinds     #-}

{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}
------------------------------------------------------------------------------------------
module Validators.StakePlusV2.OffChain.EndPoints where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
-- import qualified Cardano.Api                                                                     as CardanoApi
-- import qualified Cardano.Api.Shelley                                                             as ApiShelley
import qualified Control.Monad.Error.Lens                                                           as ControlMonadErrorLens
-- import qualified Data.Map                                                                        as DataMap
import qualified Data.Text                                                                          as DataText ( Text)
import qualified Data.Void                                                                          as DataVoid (Void)
import qualified Ledger                                                                             (getCardanoTxId, pubKeyHashAddress)
import qualified Ledger.Ada                                                                         as LedgerAda
-- import qualified Ledger.Scripts                                                                  as LedgerScripts
import qualified Ledger.Value                                                                       as LedgerValue
import qualified Ledger.Constraints                                                                 as LedgerConstraints
-- import qualified Ledger.Constraints.TxConstraints                                                as LedgerTxConstraints
-- import qualified Ledger.Tx                                                                       as LedgerTx 
--import qualified Ledger.Tx.Constraints                                                            as LedgerTxConstraints
import qualified Playground.Contract                                                                as PlaygroundContract (mkSchemaDefinitions)
import qualified Plutus.Contract                                                                    as PlutusContract
-- import qualified Plutus.Script.Utils.Scripts                                                     as UtilsScripts
-- import qualified Plutus.Script.Utils.V2.Scripts                                                  as UtilsScriptsV2
-- import qualified Plutus.V1.Ledger.Interval                                                       as LedgerIntervalV1 (interval)
-- import qualified Plutus.V2.Ledger.Api                                                            as LedgerApiV2
-- import qualified Plutus.V2.Ledger.Address                                                        as LedgerAddressV2
-- import qualified Plutus.V2.Ledger.Value                                                          as LedgerValueV2
-- import qualified PlutusTx                                               
-- import qualified PlutusTx.Builtins.Class                                                         as TxBuiltinsClass
import           PlutusTx.Prelude                                                                   hiding (unless)
-- import qualified PlutusTx.Ratio                                                                  as TxRatio
import qualified Prelude                                                                            as P
import qualified Text.Printf                                                                        as TextPrintf (printf)
-- import qualified Wallet.Emulator.Chain                                                              as WalletEmulatorChain

------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                                                     as Helpers
import qualified Validators.StakePlusV2.OffChain.EndPointsUser                                      as EndPoints
import qualified Validators.StakePlusV2.OffChain.EndPointsMaster1                                   as EndPoints
import qualified Validators.StakePlusV2.OffChain.EndPointsMaster2                                   as EndPoints
import qualified Validators.StakePlusV2.OffChain.OffChainHelpers                                    as OffChainHelpers
import qualified Validators.StakePlusV2.Types.Constants                                             as T
import qualified Validators.StakePlusV2.Types.DatumsValidator                                       as T
import qualified Validators.StakePlusV2.Types.PABParams                                             as T
import qualified Validators.StakePlusV2.Types.Types                                                 as T 
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------

handleContractError :: PlutusContract.ContractError -> PlutusContract.Contract w s e ()
handleContractError err = do
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------- ERROR ----------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "%s" (P.show err)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"

    -- entonces dentro de un contrato deberia poner esto antes de todo y el handlker se ocuparia del error
    -- ControlMonadErrorLens.handling PlutusContract._ContractError handleContractError $ do

--------------------------------------------------------------------------------------------

balanceAtScript :: T.PABBalanceAtScriptParams -> PlutusContract.Contract w s DataText.Text ()
balanceAtScript T.PABBalanceAtScriptParams {..} = do
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Balance At Script : Init ------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    (now,_) <- PlutusContract.currentNodeClientTimeRange 
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    ---------------------
    let
        !pabParams = pbPABPoolParams
        !pParams = T.pppPoolParams pabParams
        !validatorAddress = T.pppValidatorAddress pabParams
    ----------------------
        !poolID_CS = T.ppPoolID_CS pParams
        !poolID_AC = LedgerValue.AssetClass (poolID_CS, T.poolID_TN)
    ----------------------
        !fundID_CS = T.pppCurSymbol_TxID_Master_Fund pabParams
        !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)
    ----------------------
        !userID_CS = T.pppCurSymbol_TxID_User_Deposit pabParams
        !userID_AC = LedgerValue.AssetClass (userID_CS, T.userID_TN) 
    ---------------------
    !uTxOsAtValidator <- PlutusContract.utxosAt validatorAddress
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----------------"
    ---------------------
    !uTxOAtScript_With_PoolDatum' <- OffChainHelpers.getUTxO_With_PoolDatum poolID_AC uTxOsAtValidator
    ---------------------
    case uTxOAtScript_With_PoolDatum' of
        Nothing -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Can't find any uTxO with PoolDatum"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        Just uTxOAtScript_With_PoolDatum -> do
            let
                !poolDatum = Helpers.getPoolDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_PoolDatum
                !valuePoolDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_PoolDatum
            PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum UTxO: %s" (P.show $ fst uTxOAtScript_With_PoolDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum: %s" (P.show poolDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Value: %s" (P.show valuePoolDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
            ---------------------
            let
                !poolDatum_In = Helpers.getPoolDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_PoolDatum
                !masterFundersAll = T.pdMasterFunders poolDatum_In
            ---------------------
            if  T.pdFundCount poolDatum_In == 0 then do
                PlutusContract.logInfo @P.String "Fund Count is zero"
                PlutusContract.logInfo @P.String "----"
                ---------------------
                let
                    formatMasterFunderClaimedPrint list = concat  [ (\mf' ->
                        let
                            !(getBackFundAmountForMaster, getBackminAda_ForMaster) = Helpers.getFundAmountsRemains_ForMaster poolDatum_In (T.mfMaster mf)
                        in
                            [
                                "Master: " ++ P.show (T.mfMaster mf'),
                                "FundAmount: " ++ P.show (T.mfFundAmount mf'),
                                "ClaimedFund: Master Funder didn't claim fund",
                                "MinAda: " ++ P.show (T.mfMinAda mf'),
                                "----",
                                "FundAmountToGetBack: " ++ P.show getBackFundAmountForMaster,
                                "MinAdaToGetBack: " ++ P.show getBackminAda_ForMaster,
                                "----"
                        ]
                        ) mf |  mf <- list, T.mfClaimedFund mf == T.poolDatum_NotClaimedFund ]
                ---------------------
                    formatMasterFunderNotClaimedPrint list = concat  [ [
                        "Master: " ++ P.show (T.mfMaster mf),
                        "FundAmount: " ++ P.show (T.mfFundAmount mf),
                        "ClaimedFund: Master Funder already Claimed Fund",
                        "MinAda: " ++ P.show (T.mfMinAda mf),
                        "----"
                        ] | mf <- list, T.mfClaimedFund mf == T.poolDatum_ClaimedFund ]
                ---------------------
                mapM_ (PlutusContract.logInfo @P.String) (formatMasterFunderNotClaimedPrint masterFundersAll)
                mapM_ (PlutusContract.logInfo @P.String) (formatMasterFunderClaimedPrint masterFundersAll)
                ---------------------
            else do
                PlutusContract.logInfo @P.String "Fund Count is not zero"
                PlutusContract.logInfo @P.String "----"
                ---------------------
                let
                    formatMasterFunderPrint list = concat  [ [
                        "Master: " ++ P.show (T.mfMaster mf),
                        "FundAmount: " ++ P.show (T.mfFundAmount mf),
                        "ClaimedFund: " ++ P.show (T.mfClaimedFund mf),
                        "MinAda: " ++ P.show (T.mfMinAda mf),
                        "----"
                        ] | mf <- list]

                ---------------------
                mapM_ (PlutusContract.logInfo @P.String) (formatMasterFunderPrint masterFundersAll)
                ---------------------
    uTxOsAtScript_With_FundDatums <- OffChainHelpers.getUTxOs_With_FundDatum fundID_AC uTxOsAtValidator
    ---------------------
    let
        !fundDatumListAndValues =
            case uTxOsAtScript_With_FundDatums of
                [] -> []
                _ ->
                    P.map (\uTxOAtScript_With_FundDatum -> (Helpers.getFundDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_FundDatum, OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_FundDatum)) uTxOsAtScript_With_FundDatums
    ---------------------
    uTxOsAtScript_With_UserDatums <- OffChainHelpers.getUTxOs_With_UserDatum userID_AC uTxOsAtValidator
    ---------------------
    let
        !userDatumListAndValues =
            case uTxOsAtScript_With_UserDatums of
                [] -> []
                _ ->
                    P.map (\uTxOAtScript_With_UserDatum -> (Helpers.getUserDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_UserDatum, OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_UserDatum)) uTxOsAtScript_With_UserDatums
    ---------------------
    let
        formatFundDatumPrint list = concat  [ ["FundDatum: " ++ P.show datum, "Value: " ++ P.show value, "------------"] | (datum, value) <- list]

        formatUserDatumPrint list = 
            case uTxOAtScript_With_PoolDatum' of
                Nothing -> concat  [ [ "UserDatum: " ++ P.show datum, "Value: " ++ P.show value, "------------"] | (datum, value) <- list]
                Just uTxOAtScript_With_PoolDatum ->
                    concat  [ (\(userDatum, value') ->
                    let 
                        !claimAt = now
                        !closedAt = T.pdClosedAt $ Helpers.getPoolDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_PoolDatum
                        !rewards = Helpers.getRewardsPerInvest (T.ppDeadline pParams) closedAt  (T.ppInterestRates pParams) (T.udLastClaimAt userDatum) claimAt (T.udCreatedAt userDatum) (T.udInvest userDatum) (T.udRewardsNotClaimed userDatum)
                        !rewardsNotClaimed = T.udRewardsNotClaimed userDatum
                        !cashedOut = T.udCashedOut userDatum
                    ---------------------
                    in
                        [
                            "UserDatum: " ++ P.show userDatum, 
                            "----",
                            "New Rewards: " ++ P.show rewards,
                            "RewardsNotClaimed: " ++ P.show rewardsNotClaimed,
                            "RewardsCashedOut: " ++ P.show cashedOut,
                            "----",
                            "Value: " ++ P.show value', 
                            "------------"
                        ]) (datum, value)
                    | (datum, value) <- list]
                    ---------------------
    PlutusContract.logInfo @P.String "--------------------------------"
    mapM_ (PlutusContract.logInfo @P.String  ) (formatFundDatumPrint fundDatumListAndValues)
    PlutusContract.logInfo @P.String "--------------------------------"
    mapM_ (PlutusContract.logInfo @P.String  ) (formatUserDatumPrint userDatumListAndValues)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"

balanceAtScriptFull :: T.PABBalanceAtScriptFullParams -> PlutusContract.Contract w s DataText.Text ()
balanceAtScriptFull T.PABBalanceAtScriptFullParams {..} = do
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Balance At Script Full : Init ------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    let
        !pabParams = pbfPABPoolParams
        !pParams = T.pppPoolParams pabParams
        !validatorAddress = T.pppValidatorAddress pabParams
    ----------------------
        !poolID_CS = T.ppPoolID_CS pParams
        !poolID_AC = LedgerValue.AssetClass (poolID_CS, T.poolID_TN)
    ----------------------
        !fundID_CS = T.pppCurSymbol_TxID_Master_Fund pabParams
        !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)
    ----------------------
        !userID_CS = T.pppCurSymbol_TxID_User_Deposit pabParams
        !userID_AC = LedgerValue.AssetClass (userID_CS, T.userID_TN) 
    ---------------------
        !scriptID_CS =  T.pppCurSymbol_TxID_Master_AddScripts pabParams
        !scriptID_AC = LedgerValue.AssetClass (scriptID_CS, T.scriptID_TN)
    ---------------------
        !scriptID_Validator_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Validator_TN)
        !scriptID_Master_Fund_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_Fund_TN)
        !scriptID_Master_FundAndMerge_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_FundAndMerge_TN)
        !scriptID_Master_SplitFund_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_SplitFund_TN )
        !scriptID_Master_ClosePool_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_ClosePool_TN)
        !scriptID_Master_TerminatePool_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_TerminatePool_TN )
        !scriptID_Master_Emergency_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_Emergency_TN )
        !scriptID_Master_DeleteFund_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_DeleteFund_TN)
        !scriptID_Master_SendBackFund_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_SendBackFund_TN)
        !scriptID_Master_SendBackDeposit_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_SendBackDeposit_TN)
        !scriptID_Master_AddScripts_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_AddScripts_TN)
        !scriptID_Master_DeleteScripts_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_DeleteScripts_TN)
        !scriptID_User_Deposit_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_User_Deposit_TN)
        !scriptID_User_Harvest_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_User_Harvest_TN)
        !scriptID_User_Withdraw_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_User_Withdraw_TN)
    !uTxOsAtValidator <- PlutusContract.utxosAt validatorAddress
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Lista de UTxOs: %s" (P.show  uTxOsAtValidator)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    ---------------------
    !uTxOAtScript_With_ScriptDatum' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Validator_AC uTxOsAtValidator
    ---------------------
    case uTxOAtScript_With_ScriptDatum' of
        Nothing -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Can't find any uTxO with Main Validator Script"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        Just uTxOAtScript_With_ScriptDatum -> do
            let
                !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_ScriptDatum
                !valueScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_ScriptDatum
            PlutusContract.logInfo @P.String $ TextPrintf.printf "ScriptDatum UTxO: %s" (P.show $ fst uTxOAtScript_With_ScriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "ScriptDatum: %s" (P.show scriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "ScriptDatum Value: %s" (P.show valueScriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    ---------------------
    !uTxOAtScript_With_Script_Master_Fund_Datum' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_Fund_AC uTxOsAtValidator
    ---------------------
    case uTxOAtScript_With_Script_Master_Fund_Datum' of
        Nothing -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Can't find any uTxO with 'Master Fund' Minting Script"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        Just uTxOAtScript_With_Script_Master_Fund_Datum -> do
            let
                !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_Fund_Datum
                !valueScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_Fund_Datum
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Fund' Minting Script UTxO: %s" (P.show $ fst uTxOAtScript_With_Script_Master_Fund_Datum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Fund' Minting Script Datum: %s" (P.show scriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Fund' Minting Script Value: %s" (P.show valueScriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    ---------------------
    !uTxOAtScript_With_Script_Master_FundAndMerge_Datum' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_FundAndMerge_AC uTxOsAtValidator
    ---------------------
    case uTxOAtScript_With_Script_Master_FundAndMerge_Datum' of
        Nothing -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Can't find any uTxO with 'Master Fund And Merge' Minting Script"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        Just uTxOAtScript_With_Script_Master_FundAndMerge_Datum -> do
            let
                !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_FundAndMerge_Datum
                !valueScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_FundAndMerge_Datum
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Fund And Merge' Minting Script UTxO: %s" (P.show $ fst uTxOAtScript_With_Script_Master_FundAndMerge_Datum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Fund And Merge' Minting Script Datum: %s" (P.show scriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Fund And Merge' Minting Script Value: %s" (P.show valueScriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    ---------------------
    !uTxOAtScript_With_Script_Master_SplitFund_Datum' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_SplitFund_AC uTxOsAtValidator
    ---------------------
    case uTxOAtScript_With_Script_Master_SplitFund_Datum' of
        Nothing -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Can't find any uTxO with 'Master Split Fund' Minting Script"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        Just uTxOAtScript_With_Script_Master_SplitFund_Datum -> do
            let
                !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_SplitFund_Datum
                !valueScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_SplitFund_Datum
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Split Fund' Minting Script UTxO: %s" (P.show $ fst uTxOAtScript_With_Script_Master_SplitFund_Datum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Split Fund' Minting Script Datum: %s" (P.show scriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Split Fund' Minting Script Value: %s" (P.show valueScriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    ---------------------
    !uTxOAtScript_With_Script_Master_ClosePool_Datum' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_ClosePool_AC uTxOsAtValidator
    ---------------------
    case uTxOAtScript_With_Script_Master_ClosePool_Datum' of
        Nothing -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Can't find any uTxO with 'Master Close Pool' Minting Script"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        Just uTxOAtScript_With_Script_Master_ClosePool_Datum -> do
            let
                !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_ClosePool_Datum
                !valueScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_ClosePool_Datum
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Close Pool' Minting Script UTxO: %s" (P.show $ fst uTxOAtScript_With_Script_Master_ClosePool_Datum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Close Pool' Minting Script Datum: %s" (P.show scriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Close Pool' Minting Script Value: %s" (P.show valueScriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    ---------------------
    !uTxOAtScript_With_Script_Master_TerminatePool_Datum' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_TerminatePool_AC uTxOsAtValidator
    ---------------------
    case uTxOAtScript_With_Script_Master_TerminatePool_Datum' of
        Nothing -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Can't find any uTxO with 'Master Terminate Pool' Minting Script"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        Just uTxOAtScript_With_Script_Master_TerminatePool_Datum -> do
            let
                !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_TerminatePool_Datum
                !valueScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_TerminatePool_Datum
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Terminate Pool' Minting Script UTxO: %s" (P.show $ fst uTxOAtScript_With_Script_Master_TerminatePool_Datum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Terminate Pool' Minting Script Datum: %s" (P.show scriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Terminate Pool' Minting Script Value: %s" (P.show valueScriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    ---------------------
    !uTxOAtScript_With_Script_Master_Emergency_Datum' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_Emergency_AC uTxOsAtValidator
    ---------------------
    case uTxOAtScript_With_Script_Master_Emergency_Datum' of
        Nothing -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Can't find any uTxO with 'Master Emergency' Minting Script"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        Just uTxOAtScript_With_Script_Master_Emergency_Datum -> do
            let
                !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_Emergency_Datum
                !valueScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_Emergency_Datum
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Emergency' Minting Script UTxO: %s" (P.show $ fst uTxOAtScript_With_Script_Master_Emergency_Datum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Emergency' Minting Script Datum: %s" (P.show scriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Emergency' Minting Script Value: %s" (P.show valueScriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    ---------------------
    !uTxOAtScript_With_Script_Master_DeleteFund_Datum' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_DeleteFund_AC uTxOsAtValidator
    ---------------------
    case uTxOAtScript_With_Script_Master_DeleteFund_Datum' of
        Nothing -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Can't find any uTxO with 'Master Delete Fund' Minting Script"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        Just uTxOAtScript_With_Script_Master_DeleteFund_Datum -> do
            let
                !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_DeleteFund_Datum
                !valueScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_DeleteFund_Datum
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Delete Fund' Minting Script UTxO: %s" (P.show $ fst uTxOAtScript_With_Script_Master_DeleteFund_Datum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Delete Fund' Minting Script Datum: %s" (P.show scriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Delete Fund' Minting Script Value: %s" (P.show valueScriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    ---------------------
    !uTxOAtScript_With_Script_Master_SendBackFund_Datum' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_SendBackFund_AC uTxOsAtValidator
    ---------------------
    case uTxOAtScript_With_Script_Master_SendBackFund_Datum' of
        Nothing -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Can't find any uTxO with 'Master Send Back Fund' Minting Script"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        Just uTxOAtScript_With_Script_Master_SendBackFund_Datum -> do
            let
                !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_SendBackFund_Datum
                !valueScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_SendBackFund_Datum
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Send Back Fund' Minting Script UTxO: %s" (P.show $ fst uTxOAtScript_With_Script_Master_SendBackFund_Datum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Send Back Fund' Minting Script Datum: %s" (P.show scriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Send Back Fund' Minting Script Value: %s" (P.show valueScriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    ---------------------
    !uTxOAtScript_With_Script_Master_SendBackDeposit_Datum' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_SendBackDeposit_AC uTxOsAtValidator
    ---------------------
    case uTxOAtScript_With_Script_Master_SendBackDeposit_Datum' of
        Nothing -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Can't find any uTxO with 'Master Send Back Deposit' Minting Script"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        Just uTxOAtScript_With_Script_Master_SendBackDeposit_Datum -> do
            let
                !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_SendBackDeposit_Datum
                !valueScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_SendBackDeposit_Datum
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Send Back Deposit' Minting Script UTxO: %s" (P.show $ fst uTxOAtScript_With_Script_Master_SendBackDeposit_Datum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Send Back Deposit' Minting Script Datum: %s" (P.show scriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Send Back Deposit' Minting Script Value: %s" (P.show valueScriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    ---------------------
    !uTxOAtScript_With_Script_Master_AddScripts_Datum' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_AddScripts_AC uTxOsAtValidator
    ---------------------
    case uTxOAtScript_With_Script_Master_AddScripts_Datum' of
        Nothing -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Can't find any uTxO with 'Master Add Scripts' Minting Script"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        Just uTxOAtScript_With_Script_Master_AddScripts_Datum -> do
            let
                !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_AddScripts_Datum
                !valueScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_AddScripts_Datum
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Add Scripts' Minting Script UTxO: %s" (P.show $ fst uTxOAtScript_With_Script_Master_AddScripts_Datum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Add Scripts' Minting Script Datum: %s" (P.show scriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Add Scripts' Minting Script Value: %s" (P.show valueScriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    ---------------------
    !uTxOAtScript_With_Script_Master_DeleteScripts_Datum' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_DeleteScripts_AC uTxOsAtValidator
    ---------------------
    case uTxOAtScript_With_Script_Master_DeleteScripts_Datum' of
        Nothing -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Can't find any uTxO with 'Master Delete Scripts' Minting Script"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        Just uTxOAtScript_With_Script_Master_DeleteScripts_Datum -> do
            let
                !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_DeleteScripts_Datum
                !valueScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_Script_Master_DeleteScripts_Datum
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Delete Scripts' Minting Script UTxO: %s" (P.show $ fst uTxOAtScript_With_Script_Master_DeleteScripts_Datum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Delete Scripts' Minting Script Datum: %s" (P.show scriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'Master Delete Scripts' Minting Script Value: %s" (P.show valueScriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    ---------------------
    !uTxOAtScript_With_Script_User_Deposit_Datum' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_User_Deposit_AC uTxOsAtValidator
    ---------------------
    case uTxOAtScript_With_Script_User_Deposit_Datum' of
        Nothing -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Can't find any uTxO with 'User Deposit' Minting Script"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        Just uTxOAtScript_With_Script_User_Deposit_Datum -> do
            let
                !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_Script_User_Deposit_Datum
                !valueScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_Script_User_Deposit_Datum
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'User Deposit' Minting Script UTxO: %s" (P.show $ fst uTxOAtScript_With_Script_User_Deposit_Datum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'User Deposit' Minting Script Datum: %s" (P.show scriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'User Deposit' Minting Script Value: %s" (P.show valueScriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    ---------------------
    !uTxOAtScript_With_Script_User_Harvest_Datum' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_User_Harvest_AC uTxOsAtValidator
    ---------------------
    case uTxOAtScript_With_Script_User_Harvest_Datum' of
        Nothing -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Can't find any uTxO with 'User Harvest' Minting Script"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        Just uTxOAtScript_With_Script_User_Harvest_Datum -> do
            let
                !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_Script_User_Harvest_Datum
                !valueScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_Script_User_Harvest_Datum
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'User Harvest' Minting Script UTxO: %s" (P.show $ fst uTxOAtScript_With_Script_User_Harvest_Datum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'User Harvest' Minting Script Datum: %s" (P.show scriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'User Harvest' Minting Script Value: %s" (P.show valueScriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    ---------------------
    !uTxOAtScript_With_Script_User_Withdraw_Datum' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_User_Withdraw_AC uTxOsAtValidator
    ---------------------
    case uTxOAtScript_With_Script_User_Withdraw_Datum' of
        Nothing -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Can't find any uTxO with 'User Withdraw' Minting Script"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        Just uTxOAtScript_With_Script_User_Withdraw_Datum -> do
            let
                !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_Script_User_Withdraw_Datum
                !valueScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_Script_User_Withdraw_Datum
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'User Withdraw' Minting Script UTxO: %s" (P.show $ fst uTxOAtScript_With_Script_User_Withdraw_Datum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'User Withdraw' Minting Script Datum: %s" (P.show scriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "'User Withdraw' Minting Script Value: %s" (P.show valueScriptDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    ---------------------
    !uTxOAtScript_With_PoolDatum' <- OffChainHelpers.getUTxO_With_PoolDatum poolID_AC uTxOsAtValidator
    ---------------------
    case uTxOAtScript_With_PoolDatum' of
        Nothing -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Can't find any uTxO with PoolDatum"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        Just uTxOAtScript_With_PoolDatum -> do
            let
                !poolDatum = Helpers.getPoolDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_PoolDatum
                !valuePoolDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_PoolDatum
            PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum UTxO: %s" (P.show $ fst uTxOAtScript_With_PoolDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum: %s" (P.show poolDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Value: %s" (P.show valuePoolDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    ---------------------
    uTxOsAtScript_With_FundDatums <- OffChainHelpers.getUTxOs_With_FundDatum fundID_AC uTxOsAtValidator
    ---------------------
    let
        !fundDatumListAndValues =
            case uTxOsAtScript_With_FundDatums of
                [] -> []
                _ ->
                    P.map (\uTxOAtScript_With_FundDatum -> (Helpers.getFundDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_FundDatum, OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_FundDatum)) uTxOsAtScript_With_FundDatums
    ---------------------
    uTxOsAtScript_With_UserDatums <- OffChainHelpers.getUTxOs_With_UserDatum userID_AC uTxOsAtValidator
    ---------------------
    let
        !userDatumListAndValues =
            case uTxOsAtScript_With_UserDatums of
                [] -> []
                _ ->
                    P.map (\uTxOAtScript_With_UserDatum -> (Helpers.getUserDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxOAtScript_With_UserDatum, OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_UserDatum)) uTxOsAtScript_With_UserDatums
    ---------------------
    let
        formatFundDatumPrint list = concat  [ ["FundDatum: " ++ P.show datum, "Value: " ++ P.show value, "----"] | (datum, value) <- list]
        formatUserDatumPrint list = concat  [ ["UserDatum: " ++ P.show datum, "Value: " ++ P.show value, "----"] | (datum, value) <- list]
    mapM_ (PlutusContract.logInfo @P.String  ) (formatFundDatumPrint fundDatumListAndValues)
    mapM_ (PlutusContract.logInfo @P.String  ) (formatUserDatumPrint userDatumListAndValues)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"

------------------------------------------------------------------------------------------

splitUtxO :: PlutusContract.AsContractError DataText.Text => T.PABSplitUtxOParams -> PlutusContract.Contract w s DataText.Text ()
splitUtxO T.PABSplitUtxOParams {..} = do
    ControlMonadErrorLens.handling PlutusContract._ContractError handleContractError $ do
        ---------------------
        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------------------------- Split UTxO : Init ------------------------------------"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
        ---------------------
        (now,_) <- PlutusContract.currentNodeClientTimeRange
        PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
        ---------------------
        masterPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
        let
            !masterAdds = Ledger.pubKeyHashAddress masterPPKH Nothing
        uTxOsAtMaster <- PlutusContract.utxosAt masterAdds
        ---------------------
        let
            !splitAmount  = psuSplitAmount
        ---------------------
            !value_For_SplitAmount  = LedgerAda.lovelaceValueOf splitAmount
        ---------------------
            lookupsTx =
                LedgerConstraints.unspentOutputs uTxOsAtMaster 
            tx =
                LedgerConstraints.mustPayToPubKey masterPPKH value_For_SplitAmount P.<> 
                LedgerConstraints.mustPayToPubKey masterPPKH value_For_SplitAmount P.<> 
                LedgerConstraints.mustBeSignedBy masterPPKH
        ------------------------
        submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
        txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
        PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Split Fund UTxO (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)

------------------------------------------------------------------------------------------

type ValidatorSchema =
        PlutusContract.Endpoint "balanceAtScript" T.PABBalanceAtScriptParams PlutusContract..\/
        PlutusContract.Endpoint "balanceAtScriptFull" T.PABBalanceAtScriptFullParams PlutusContract..\/
        PlutusContract.Endpoint "splitUtxO" T.PABSplitUtxOParams PlutusContract..\/
        PlutusContract.Endpoint "masterMintFree" T.PABMasterMintFreeParams PlutusContract..\/
        PlutusContract.Endpoint "masterPreparePool" T.PABMasterPreparePoolParams PlutusContract..\/
        PlutusContract.Endpoint "masterFund" T.PABMasterFundParams PlutusContract..\/
        PlutusContract.Endpoint "masterFundAndMerge" T.PABMasterFundAndMergeParams PlutusContract..\/
        PlutusContract.Endpoint "masterSplitFund" T.PABMasterSplitFundParams PlutusContract..\/
        PlutusContract.Endpoint "masterClosePool" T.PABMasterClosePoolParams PlutusContract..\/
        PlutusContract.Endpoint "masterTerminatePool" T.PABMasterTerminatePoolParams PlutusContract..\/
        PlutusContract.Endpoint "masterEmergency" T.PABMasterEmergencyParams PlutusContract..\/
        PlutusContract.Endpoint "masterDeleteFund" T.PABMasterDeleteFundParams PlutusContract..\/
        PlutusContract.Endpoint "masterSendBackFund" T.PABMasterSendBackFundParams PlutusContract..\/
        PlutusContract.Endpoint "masterSendBackDeposit" T.PABMasterSendBackDepositParams PlutusContract..\/
        PlutusContract.Endpoint "userDeposit" T.PABUserDepositParams PlutusContract..\/
        PlutusContract.Endpoint "userWithdraw" T.PABUserWithdrawParams PlutusContract..\/
        PlutusContract.Endpoint "userHarvest" T.PABUserHarvestParams 

endpoints :: PlutusContract.Contract () ValidatorSchema DataText.Text ()
endpoints = PlutusContract.awaitPromise (
        balanceAtScript' `PlutusContract.select`
        balanceAtScriptFull' `PlutusContract.select`
        splitUtxO' `PlutusContract.select`
        masterPreparePool' `PlutusContract.select`
        masterFund' `PlutusContract.select`
        masterFundAndMerge' `PlutusContract.select`
        masterSplitFund' `PlutusContract.select`
        masterClosePool' `PlutusContract.select`
        masterTerminatePool' `PlutusContract.select`
        masterEmergency' `PlutusContract.select`
        masterDeleteFund' `PlutusContract.select`
        masterSendBackFund' `PlutusContract.select`
        masterSendBackDeposit' `PlutusContract.select`
        userDeposit' `PlutusContract.select`
        userWithdraw' `PlutusContract.select`
        userHarvest' 
    ) >> endpoints
  where
    balanceAtScript' = PlutusContract.endpoint @"balanceAtScript" balanceAtScript
    balanceAtScriptFull' = PlutusContract.endpoint @"balanceAtScriptFull" balanceAtScriptFull
    splitUtxO' = PlutusContract.endpoint @"splitUtxO" splitUtxO
    masterPreparePool' = PlutusContract.endpoint @"masterPreparePool" EndPoints.masterPreparePool
    masterFund' = PlutusContract.endpoint @"masterFund" EndPoints.masterFund
    masterFundAndMerge' = PlutusContract.endpoint @"masterFundAndMerge" EndPoints.masterFundAndMerge
    masterSplitFund' = PlutusContract.endpoint @"masterSplitFund" EndPoints.masterSplitFund
    masterClosePool' = PlutusContract.endpoint @"masterClosePool" EndPoints.masterClosePool
    masterTerminatePool' = PlutusContract.endpoint @"masterTerminatePool" EndPoints.masterTerminatePool
    masterEmergency' = PlutusContract.endpoint @"masterEmergency" EndPoints.masterEmergency
    masterDeleteFund' = PlutusContract.endpoint @"masterDeleteFund" EndPoints.masterDeleteFund
    masterSendBackFund' = PlutusContract.endpoint @"masterSendBackFund" EndPoints.masterSendBackFund
    masterSendBackDeposit' = PlutusContract.endpoint @"masterSendBackDeposit" EndPoints.masterSendBackDeposit
    userDeposit' = PlutusContract.endpoint @"userDeposit" EndPoints.userDeposit
    userWithdraw' = PlutusContract.endpoint @"userWithdraw" EndPoints.userWithdraw
    userHarvest' = PlutusContract.endpoint @"userHarvest" EndPoints.userHarvest

PlaygroundContract.mkSchemaDefinitions ''ValidatorSchema
