{-# LANGUAGE DataKinds                  #-}
-- {-# LANGUAGE DeriveAnyClass             #-}
-- {-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- {-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE TupleSections              #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
-- {-# LANGUAGE NumericUnderscores         #-}
-- {-# LANGUAGE MonoLocalBinds     #-}

{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}
------------------------------------------------------------------------------------------
module Validators.StakePlusV2.OffChain.EndPointsMaster2 where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Data.Map                                                   as DataMap
import qualified Data.Text                                                  as DataText ( Text)
import qualified Data.Void                                                  as DataVoid (Void)
import qualified Ledger                                                     (getCardanoTxId, pubKeyHashAddress, unPaymentPubKeyHash, PaymentPubKeyHash(..) )
import qualified Ledger.Ada                                                 as LedgerAda
import qualified Ledger.Value                                               as LedgerValue
import qualified Ledger.Constraints                                         as LedgerConstraints
import qualified Ledger.Constraints.TxConstraints                           as LedgerTxConstraints
import qualified Plutus.Contract                                            as PlutusContract
import qualified Plutus.V1.Ledger.Interval                                  as LedgerIntervalV1 (interval)
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                                                    as P
import qualified Text.Printf                                                as TextPrintf (printf)
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers
-- import qualified Validators.StakePlusV2.OnChain.Tokens.Free.Policy          as FreePolicy
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers
import qualified Validators.StakePlusV2.OffChain.OffChainHelpers            as OffChainHelpers
import qualified Validators.StakePlusV2.Types.Constants                     as T 
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T 
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T 
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T 
import qualified Validators.StakePlusV2.Types.PABParams                     as T 
import qualified Validators.StakePlusV2.Types.Types                         as T 
import qualified Utils

------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------

masterDeleteFund :: T.PABMasterDeleteFundParams -> PlutusContract.Contract w s DataText.Text ()
masterDeleteFund T.PABMasterDeleteFundParams{..} = do
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Delete Fund : Init ------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    (now,_) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    ---------------------
    masterPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let
        !master = Ledger.unPaymentPubKeyHash masterPPKH
        !masterAdds = Ledger.pubKeyHashAddress masterPPKH Nothing
    uTxOsAtMaster <- PlutusContract.utxosAt masterAdds
    ---------------------
    let
        !pabParams = pmdPABPoolParams
        !pParams = T.pppPoolParams pabParams
        !validatorHash = T.pppValidatorHash pabParams
        !validatorAddress = T.pppValidatorAddress pabParams
    ---------------------
        -- !policy_TxID_Master_DeleteFund = T.pppPolicy_TxID_Master_DeleteFund pabParams
    ---------------------
        !poolID_CS = T.ppPoolID_CS pParams
        !poolID_AC = LedgerValue.AssetClass (poolID_CS, T.poolID_TN)
    ----------------------
        !fundID_CS = T.pppCurSymbol_TxID_Master_Fund pabParams
        !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)
    ---------------------
        !scriptID_CS =  T.pppCurSymbol_TxID_Master_AddScripts pabParams
        !scriptID_AC = LedgerValue.AssetClass (scriptID_CS, T.scriptID_TN)
    ---------------------
        !scriptID_Validator_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Validator_TN)
        !scriptID_Master_DeleteFund_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_DeleteFund_TN)
        !scriptID_Master_Fund_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_Fund_TN)
    ---------------------
        !txID_Master_DeleteFund_CS =  T.pppCurSymbol_TxID_Master_DeleteFund pabParams
        !txID_Master_DeleteFund_AC = LedgerValue.AssetClass (txID_Master_DeleteFund_CS, T.txID_Master_DeleteFund_TN)
    ---------------------
        !uTxOToDelete = pmdFundIDs_TxOutRefs
    ---------------------
    !checkCollateral <- OffChainHelpers.checkIfThereIsUTxOFreeForCollateral uTxOsAtMaster
    if checkCollateral then
        PlutusContract.logInfo @P.String $ TextPrintf.printf "There is UTxO free for Collateral"
    else
        PlutusContract.throwError "There is NOT UTxO free for Collateral"
    ---------------------
    uTxOsAtValidator <- PlutusContract.utxosAt validatorAddress
    ---------------------
    !uTxO_With_ScriptDatum' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Validator_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_ScriptDatum <-
        case uTxO_With_ScriptDatum' of
            Nothing ->
                PlutusContract.throwError "Can't find any uTxO with Main Validator Script"
            Just uTxO_With_ScriptDatum -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with Main Validator Script: %s" (P.show $ fst uTxO_With_ScriptDatum)
                return uTxO_With_ScriptDatum
    ---------------------
    !uTxO_With_Script_Master_Fund' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_Fund_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_Fund <-
        case uTxO_With_Script_Master_Fund' of
            Nothing ->
                PlutusContract.throwError "Can't find any uTxO with 'Master Fund' Minting Script"
            Just uTxO_With_Script_Master_Fund -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with 'Master Fund' Minting Script: %s" (P.show $ fst uTxO_With_Script_Master_Fund)
                return uTxO_With_Script_Master_Fund
    ---------------------
    !uTxO_With_Script_Master_DeleteFund' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_DeleteFund_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_DeleteFund <-
        case uTxO_With_Script_Master_DeleteFund' of
            Nothing ->
                PlutusContract.throwError "Can't find any uTxO with 'Master Delete Fund' Minting Script"
            Just uTxO_With_Script_Master_DeleteFund -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with 'Master Delete Fund' Minting Script: %s" (P.show $ fst uTxO_With_Script_Master_DeleteFund)
                return uTxO_With_Script_Master_DeleteFund
    ---------------------
    !uTxO_With_PoolDatum' <- OffChainHelpers.getUTxO_With_PoolDatum poolID_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_PoolDatum <-
        case uTxO_With_PoolDatum' of
            Nothing ->
                PlutusContract.throwError "Can't find any uTxO with PoolDatum"
            Just uTxO_With_PoolDatum -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with PoolDatum: %s" (P.show $ fst uTxO_With_PoolDatum)
                return uTxO_With_PoolDatum
    ---------------------
    uTxOs_With_FundDatums' <- OffChainHelpers.getUTxOs_With_FundDatum fundID_AC uTxOsAtValidator
    ---------------------
    !uTxOs_With_FundDatums <-
        case uTxOs_With_FundDatums' of
            [] ->
                PlutusContract.throwError "Can't find any uTxO with FundDatum"
            x -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxOs List with FundDatum: %s" (P.show $ fst <$> x )
                return x
    ---------------------
    let !uTxOs_With_FundDatums_To_Delete' = [ (txOutRef, chainIndexTxOut)  |  (txOutRef, chainIndexTxOut) <- uTxOs_With_FundDatums, txOutRef `elem` uTxOToDelete]
    ---------------------
    !uTxOs_With_FundDatums_To_Delete <-
        case uTxOs_With_FundDatums_To_Delete' of
            [] ->
                PlutusContract.throwError "Can't find any uTxO with FundDatum at the chossen uTxOs to Delete"
            x -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxOs List with FundDatum to Delete: %s" (P.show $ fst <$> x )
                return x
    ---------------------
    let
    ---------------------
        !fundDatums_To_Delete = Helpers.getFundDatumTypo_FromMaybeDatum . OffChainHelpers.getDatumFromDecoratedTxOut . snd  <$> uTxOs_With_FundDatums_To_Delete
    ---------------------
        !mergingCount = length uTxOs_With_FundDatums_To_Delete
        !mergingCashedOut = sum [ T.fdCashedOut fundDatumTypo | fundDatumTypo <- fundDatums_To_Delete ]
    ---------------------
        !value_For_Mint_TxID_Master_DeleteFund = LedgerValue.assetClassValue txID_Master_DeleteFund_AC 1
    ---------------------
        !value_In_FundDatum_To_Delete = foldl (<>) (LedgerAda.lovelaceValueOf 0) (OffChainHelpers.getValueFromDecoratedTxOut . snd <$> uTxOs_With_FundDatums_To_Delete)
    ---------------------
        !fundID_To_Burn_Amount = LedgerValue.assetClassValueOf value_In_FundDatum_To_Delete fundID_AC
        !value_For_Burn_TxID_FundID = LedgerValue.assetClassValue fundID_AC (negate fundID_To_Burn_Amount)
    ---------------------
        !value_In_PoolDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_PoolDatum
        !value_For_PoolDatum = value_In_PoolDatum <> value_In_FundDatum_To_Delete <> value_For_Mint_TxID_Master_DeleteFund <> value_For_Burn_TxID_FundID
    ---------------------
        !poolDatum_In = Helpers.getPoolDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_PoolDatum
        !poolDatum_Out = T.PoolDatum $ Helpers.mkUpdated_PoolDatum_With_DeletingFunds poolDatum_In mergingCount mergingCashedOut
    ---------------------
        !redeemer_For_Consuming_Validator_Datum = T.mkRedeemerMasterDeleteFund master
        !redeemer_For_Mint_TxID_Master_DeleteFund = T.mkRedeemerMint_TxID redeemer_For_Consuming_Validator_Datum
        !redeemer_For_Burn_TxID = T.mkRedeemerBurn_TxID
    ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Delete Fund : Ending ---------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In: %s" (P.show poolDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In Value: %s" (P.show value_In_PoolDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out: %s" (P.show poolDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out Value: %s" (P.show value_For_PoolDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum To Delete: %s" (P.show fundDatums_To_Delete)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum To Delete Value: %s" (P.show value_In_FundDatum_To_Delete)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeemer For Consuming Validator Datum: %s" (P.show redeemer_For_Consuming_Validator_Datum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    let
        (lookupsTx_Mint_TxID_Master_DeleteFund, tx_Mint_TxID_Master_DeleteFund) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMaster uTxO_With_Script_Master_DeleteFund (Just redeemer_For_Mint_TxID_Master_DeleteFund) value_For_Mint_TxID_Master_DeleteFund validityRange masterPPKH
        (lookupsTx_Burn_FundID, tx_Burn_FundID) = OffChainHelpers.burntToken_With_RefPolicy uTxOsAtMaster uTxO_With_Script_Master_Fund (Just redeemer_For_Burn_TxID) value_For_Burn_TxID_FundID validityRange masterPPKH
        lookupsTx =
            lookupsTx_Mint_TxID_Master_DeleteFund P.<>
            lookupsTx_Burn_FundID P.<>
            LedgerConstraints.unspentOutputs uTxOsAtMaster P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_ScriptDatum]) P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum])    P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList uTxOs_With_FundDatums_To_Delete )
        tx =
            tx_Mint_TxID_Master_DeleteFund P.<>
            tx_Burn_FundID P.<>
            LedgerConstraints.mustSpendScriptOutputWithReference (fst uTxO_With_PoolDatum) (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum)  (fst uTxO_With_ScriptDatum) P.<>
            mconcat [LedgerConstraints.mustSpendScriptOutputWithReference txOutRef (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum)  (fst uTxO_With_ScriptDatum) | txOutRef <- fst <$> uTxOs_With_FundDatums_To_Delete] P.<>
            LedgerConstraints.mustPayToOtherScriptWithInlineDatum validatorHash  (LedgerApiV2.Datum $ PlutusTx.toBuiltinData poolDatum_Out) value_For_PoolDatum P.<>
            LedgerConstraints.mustValidateIn validityRange P.<>
            LedgerConstraints.mustBeSignedBy masterPPKH
    ---------------------
    submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
    txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Delete Fund (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)

--------------------------------------------------------------------------------------------

masterSendBackFund :: T.PABMasterSendBackFundParams -> PlutusContract.Contract w s DataText.Text ()
masterSendBackFund T.PABMasterSendBackFundParams{..} = do
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Send Back Fund : Init ----------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    (now,_) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    ---------------------
    masterPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let
        !master = Ledger.unPaymentPubKeyHash masterPPKH
        !masterAdds = Ledger.pubKeyHashAddress masterPPKH Nothing
    uTxOsAtMaster <- PlutusContract.utxosAt masterAdds
    ---------------------
    let
        !pabParams = pmsbfPABPoolParams
        !pParams = T.pppPoolParams pabParams
        !validatorHash = T.pppValidatorHash pabParams
        !validatorAddress = T.pppValidatorAddress pabParams
    ---------------------
        -- !policy_TxID_Master_SendBackFund = T.pppPolicy_TxID_Master_SendBackFund pabParams
    ---------------------
        !harvest_CS = T.ppHarvest_CS pParams
        !harvest_AC = LedgerValue.AssetClass (harvest_CS, T.ppHarvest_TN pParams)
        !harvestIsAda = LedgerApiV2.adaSymbol == harvest_CS
        !haverstIsWithoutTokenName = not harvestIsAda && T.ppHarvest_TN pParams == LedgerApiV2.TokenName emptyByteString
    ------------------------------------------
        !poolID_CS = T.ppPoolID_CS pParams
        !poolID_AC = LedgerValue.AssetClass (poolID_CS, T.poolID_TN)
    ---------------------
        !scriptID_CS =  T.pppCurSymbol_TxID_Master_AddScripts pabParams
        !scriptID_AC = LedgerValue.AssetClass (scriptID_CS, T.scriptID_TN)
    ---------------------
        !scriptID_Validator_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Validator_TN)
        !scriptID_Master_SendBackFund_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_SendBackFund_TN)
    ---------------------
        !txID_Master_SendBackFund_CS =  T.pppCurSymbol_TxID_Master_SendBackFund pabParams
        !txID_Master_SendBackFund_AC = LedgerValue.AssetClass (txID_Master_SendBackFund_CS, T.txID_Master_SendBackFund_TN)
    ---------------------
        !master_To_SendBack = pmsbfMasterToSendBack
    ---------------------
    !checkCollateral <- OffChainHelpers.checkIfThereIsUTxOFreeForCollateral uTxOsAtMaster
    if checkCollateral then
        PlutusContract.logInfo @P.String $ TextPrintf.printf "There is UTxO free for Collateral"
    else
        PlutusContract.throwError "There is NOT UTxO free for Collateral"
    ---------------------
    uTxOsAtValidator <- PlutusContract.utxosAt validatorAddress
    ---------------------
    !uTxO_With_ScriptDatum' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Validator_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_ScriptDatum <-
        case uTxO_With_ScriptDatum' of
            Nothing ->
                PlutusContract.throwError "Can't find any uTxO with Main Validator Script"
            Just uTxO_With_ScriptDatum -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with Main Validator Script: %s" (P.show $ fst uTxO_With_ScriptDatum)
                return uTxO_With_ScriptDatum
    ---------------------
    !uTxO_With_Script_Master_SendBackFund' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_SendBackFund_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_SendBackFund <-
        case uTxO_With_Script_Master_SendBackFund' of
            Nothing ->
                PlutusContract.throwError "Can't find any uTxO with 'Master Send Back Fund' Minting Script"
            Just uTxO_With_Script_Master_SendBackFund -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with 'Master Send Back Fund' Minting Script: %s" (P.show $ fst uTxO_With_Script_Master_SendBackFund)
                return uTxO_With_Script_Master_SendBackFund
    ---------------------
    !uTxO_With_PoolDatum' <- OffChainHelpers.getUTxO_With_PoolDatum poolID_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_PoolDatum <-
        case uTxO_With_PoolDatum' of
            Nothing ->
                PlutusContract.throwError "Can't find any uTxO with PoolDatum"
            Just uTxO_With_PoolDatum -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with PoolDatum: %s" (P.show $ fst uTxO_With_PoolDatum)
                return uTxO_With_PoolDatum
    ---------------------
    let
        !poolDatum_In = Helpers.getPoolDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_PoolDatum
    ---------------------
    _ <- if  T.pdFundCount poolDatum_In /= 0 then
            PlutusContract.throwError "You must Merge and Delete All Funds to calculate remaining Funds"
        else
            PlutusContract.logInfo @P.String "Fund Count is zero"
    ---------------------
    let
        !masterFundersAll = T.pdMasterFunders poolDatum_In
        !masterFunder = find  (\mF' -> T.mfMaster mF' == master_To_SendBack) masterFundersAll
    case masterFunder of
        Nothing -> PlutusContract.throwError "Can't find Master Funder"
        Just mf ->
            if T.mfClaimedFund mf == T.poolDatum_ClaimedFund then
                PlutusContract.throwError "Master Funder already Claimed Fund"
            else
                PlutusContract.logInfo @P.String "Master Funder Found!"
    ---------------------
    let
        !value_For_Mint_TxID_Master_SendBackFund = LedgerValue.assetClassValue txID_Master_SendBackFund_AC 1
    ---------------------
        !value_In_PoolDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_PoolDatum
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Calculating Fund To Send Back"
    let
        !(sendBackFundAmount, sendBackMinAda) = Helpers.getFundAmountsRemains_ForMaster poolDatum_In master_To_SendBack
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Calculated Fund To Send Back: %s, minAda : %s" (P.show sendBackFundAmount) (P.show sendBackMinAda)
    ---------------------
    !value_For_SendBackFundAmount <- OffChainHelpers.createValueAddingTokensOfCurrencySymbol harvest_AC harvest_CS haverstIsWithoutTokenName value_In_PoolDatum sendBackFundAmount
    ---------------------
    let
        !value_For_SendBackMinAda = LedgerAda.lovelaceValueOf sendBackMinAda
    ---------------------
        !value_For_SendBackFundAmountPlusAda = value_For_SendBackFundAmount <> value_For_SendBackMinAda
    ---------------------
        !value_For_SendBackFund_To_Master = value_For_SendBackFundAmountPlusAda
    ---------------------
        !value_For_PoolDatum = value_In_PoolDatum <> value_For_Mint_TxID_Master_SendBackFund <> negate value_For_SendBackFund_To_Master
    ---------------------
        !poolDatum_Out = T.PoolDatum $ Helpers.mkUpdated_PoolDatum_With_SendBackFund poolDatum_In master_To_SendBack
    ---------------------
        !redeemer_For_Consuming_Validator_Datum = T.mkRedeemerMasterSendBackFund master master_To_SendBack
        !redeemer_For_Mint_TxID_Master_SendBackFund = T.mkRedeemerMint_TxID redeemer_For_Consuming_Validator_Datum
    ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Send Back Fund : Ending ---------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In: %s" (P.show poolDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In Value: %s" (P.show value_In_PoolDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out: %s" (P.show poolDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out Value: %s" (P.show value_For_PoolDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Master To Send Back: %s" (P.show master_To_SendBack)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Value For Master: %s" (P.show value_For_SendBackFund_To_Master)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeemer For Consuming Validator Datum: %s" (P.show redeemer_For_Consuming_Validator_Datum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    let
        (lookupsTx_Mint_TxID_Master_SendBackFund, tx_Mint_TxID_Master_SendBackFund) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMaster uTxO_With_Script_Master_SendBackFund (Just redeemer_For_Mint_TxID_Master_SendBackFund) value_For_Mint_TxID_Master_SendBackFund validityRange masterPPKH
        lookupsTx =
            lookupsTx_Mint_TxID_Master_SendBackFund P.<>
            LedgerConstraints.unspentOutputs uTxOsAtMaster P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_ScriptDatum]) P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum] )

        tx =
            tx_Mint_TxID_Master_SendBackFund P.<>
            LedgerConstraints.mustSpendScriptOutputWithReference (fst uTxO_With_PoolDatum) (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum)  (fst uTxO_With_ScriptDatum) P.<>
            LedgerConstraints.mustPayToOtherScriptWithInlineDatum validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData poolDatum_Out) value_For_PoolDatum P.<>
            LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash master_To_SendBack) value_For_SendBackFund_To_Master  P.<>
            LedgerConstraints.mustValidateIn validityRange P.<>
            LedgerConstraints.mustBeSignedBy masterPPKH
    ---------------------
    submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void  lookupsTx tx
    txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Send Back Fund (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)

--------------------------------------------------------------------------------------------

masterSendBackDeposit :: T.PABMasterSendBackDepositParams -> PlutusContract.Contract w s DataText.Text ()
masterSendBackDeposit T.PABMasterSendBackDepositParams{..} = do
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Send Back Deposit : Init ----------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    (now,_) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    ---------------------
    masterPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let
        !master = Ledger.unPaymentPubKeyHash masterPPKH
        !masterAdds = Ledger.pubKeyHashAddress masterPPKH Nothing
    uTxOsAtMaster <- PlutusContract.utxosAt masterAdds
    ---------------------
    let
        !pabParams = pmsbiPABPoolParams
        !pParams = T.pppPoolParams pabParams
        !validatorHash = T.pppValidatorHash pabParams
        !validatorAddress = T.pppValidatorAddress pabParams
    ---------------------
        -- !policy_TxID_Master_SendBackDeposit = T.pppPolicy_TxID_Master_SendBackDeposit pabParams
    ---------------------
        -- !policy_TxID_User_Deposit = T.pppPolicy_TxID_User_Deposit pabParams
        -- !policy_TxID_User_Harvest = T.pppPolicy_TxID_User_Harvest pabParams
    ---------------------
        !staking_CS = T.ppStaking_CS pParams
        !staking_AC = LedgerValue.AssetClass (staking_CS, T.ppStaking_TN pParams)
        !stakingIsAda = LedgerApiV2.adaSymbol == staking_CS
        !stakingIsWithoutTokenName = not stakingIsAda && T.ppStaking_TN pParams == LedgerApiV2.TokenName emptyByteString
    ------------------------------------------
        !poolID_CS = T.ppPoolID_CS pParams
        !poolID_AC = LedgerValue.AssetClass (poolID_CS, T.poolID_TN)
    ---------------------
        !fundID_CS = T.pppCurSymbol_TxID_Master_Fund pabParams
        !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)
    ---------------------
        !userID_CS = T.pppCurSymbol_TxID_User_Deposit pabParams
        !userID_AC = LedgerValue.AssetClass (userID_CS, T.userID_TN) 
    ---------------------
        !scriptID_CS =  T.pppCurSymbol_TxID_Master_AddScripts pabParams
        !scriptID_AC = LedgerValue.AssetClass (scriptID_CS, T.scriptID_TN)
        ---------------------
        !scriptID_Validator_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Validator_TN)
        !scriptID_Master_SendBackDeposit_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_SendBackDeposit_TN)
        !scriptID_User_Deposit_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_User_Deposit_TN)
    ---------------------
        !txID_Master_SendBackDeposit_CS =  T.pppCurSymbol_TxID_Master_SendBackDeposit pabParams
        !txID_Master_SendBackDeposit_AC = LedgerValue.AssetClass (txID_Master_SendBackDeposit_CS, T.txID_Master_SendBackDeposit_TN)
    ---------------------
        -- !txID_User_Harvest_CS = T.pppCurSymbol_TxID_User_Harvest pabParams
    ---------------------
        !uTxOToSendBack = pmsbiUserID_TxOutRef
    ---------------------
    !checkCollateral <- OffChainHelpers.checkIfThereIsUTxOFreeForCollateral uTxOsAtMaster
    if checkCollateral then
        PlutusContract.logInfo @P.String $ TextPrintf.printf "There is UTxO free for Collateral"
    else
        PlutusContract.throwError "There is NOT UTxO free for Collateral"
    ---------------------
    uTxOsAtValidator <- PlutusContract.utxosAt validatorAddress
    ---------------------
    !uTxO_With_ScriptDatum' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Validator_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_ScriptDatum <-
        case uTxO_With_ScriptDatum' of
            Nothing ->
                PlutusContract.throwError "Can't find any uTxO with Main Validator Script"
            Just uTxO_With_ScriptDatum -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with Main Validator Script: %s" (P.show $ fst uTxO_With_ScriptDatum)
                return uTxO_With_ScriptDatum
    ---------------------
    !uTxO_With_Script_Master_SendBackDeposit' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_SendBackDeposit_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_SendBackDeposit <-
        case uTxO_With_Script_Master_SendBackDeposit' of
            Nothing ->
                PlutusContract.throwError "Can't find any uTxO with 'Master Send Back Deposit' Minting Script"
            Just uTxO_With_Script_Master_SendBackDeposit -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with 'Master Send Back Deposit' Minting Script: %s" (P.show $ fst uTxO_With_Script_Master_SendBackDeposit)
                return uTxO_With_Script_Master_SendBackDeposit
    ---------------------
    !uTxO_With_Script_User_Deposit' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_User_Deposit_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_User_Deposit <-
        case uTxO_With_Script_User_Deposit' of
            Nothing ->
                PlutusContract.throwError "Can't find any uTxO with 'User Deposit' Minting Script"
            Just uTxO_With_Script_User_Deposit -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with 'User Deposit' Minting Script: %s" (P.show $ fst uTxO_With_Script_User_Deposit)
                return uTxO_With_Script_User_Deposit
    ---------------------
    !uTxO_With_PoolDatum' <- OffChainHelpers.getUTxO_With_PoolDatum poolID_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_PoolDatum <-
        case uTxO_With_PoolDatum' of
            Nothing ->
                PlutusContract.throwError "Can't find any uTxO with PoolDatum"
            Just uTxO_With_PoolDatum -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with PoolDatum: %s" (P.show $ fst uTxO_With_PoolDatum)
                return uTxO_With_PoolDatum
    ---------------------
    !uTxOs_With_UserDatums' <- OffChainHelpers.getUTxOs_With_UserDatum userID_AC uTxOsAtValidator
    ---------------------
    !uTxOs_With_UserDatums <-
        case uTxOs_With_UserDatums' of
            [] ->
                PlutusContract.throwError "Can't find any uTxO with UserDatum"
            x -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxOs List with UserDatum: %s" (P.show $ fst <$> x )
                return x
    ---------------------
    let !uTxO_With_UserDatum' = find (\(txOutRef, _) -> txOutRef == uTxOToSendBack) uTxOs_With_UserDatums
    !uTxO_With_UserDatum <-
        case uTxO_With_UserDatum' of
            Nothing ->
                PlutusContract.throwError "Can't find any uTxO with UserDatum at the chossen uTxO to Send Back Invest"
            Just x -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with UserDatum to Send Back Invest: %s" (P.show $ fst x  )
                return x
    ---------------------
    let
        !poolDatum_In = Helpers.getPoolDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_PoolDatum
        !isFundCountZero = T.pdFundCount poolDatum_In == 0
    ---------------------
        !value_For_Mint_TxID_Master_SendBackDeposit = LedgerValue.assetClassValue txID_Master_SendBackDeposit_AC 1
    ---------------------
        !userDatum_IN = Helpers.getUserDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_UserDatum
    ---------------------
        !userToSendBack = T.udUser userDatum_IN
        !investAmount = T.udInvest userDatum_IN
        !mindAda = T.udMinAda userDatum_IN
    ---------------------
        !value_In_UserDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_UserDatum
    ---------------------
    !value_InvestAmount <- OffChainHelpers.createValueAddingTokensOfCurrencySymbol staking_AC staking_CS stakingIsWithoutTokenName value_In_UserDatum investAmount
    ---------------------
    let
        !value_MinAda_In_UserDatum = LedgerAda.lovelaceValueOf mindAda
    ---------------------
        !value_InvestAmountPlusAda = value_InvestAmount <> value_MinAda_In_UserDatum
    ---------------------
        !value_For_SendBackDeposit_To_User = value_InvestAmountPlusAda
    ---------------------
        -- !value_ForKeepTxID_User_Harvest = Helpers.getValueOfCurrencySymbol value_In_UserDatum txID_User_Harvest_CS
    ---------------------
        !value_For_Burn_UserID = LedgerValue.assetClassValue userID_AC (-1)
    ---------------------
        !redeemer_For_Consuming_Validator_Datum = T.mkRedeemerMasterSendBackDeposit master userToSendBack
        !redeemer_For_Mint_TxID_Master_SendBackDeposit = T.mkRedeemerMint_TxID redeemer_For_Consuming_Validator_Datum
        !redeemer_For_Burn_TxID = T.mkRedeemerBurn_TxID
    ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
    ---------------------
        (lookupsTx_Mint_TxID_Master_SendBackDeposit, tx_Mint_TxID_Master_SendBackDeposit) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMaster uTxO_With_Script_Master_SendBackDeposit (Just redeemer_For_Mint_TxID_Master_SendBackDeposit) value_For_Mint_TxID_Master_SendBackDeposit validityRange masterPPKH
        (lookupsTx_Burn_UserID, tx_Burn_UserID) = OffChainHelpers.burntToken_With_RefPolicy uTxOsAtMaster uTxO_With_Script_User_Deposit (Just redeemer_For_Burn_TxID) value_For_Burn_UserID validityRange masterPPKH
    ---------------------
    if isFundCountZero then do
        let
            !poolDatum_Out = T.PoolDatum poolDatum_In
        ---------------------
            !value_In_PoolDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_PoolDatum
            !value_For_PoolDatum = value_In_PoolDatum <> value_For_Mint_TxID_Master_SendBackDeposit <> value_In_UserDatum <> value_For_Burn_UserID <> negate value_For_SendBackDeposit_To_User
        ----------------------------------
        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Send Back Deposit : Ending ---------------------------------------"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In: %s" (P.show poolDatum_In)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In Value: %s" (P.show value_In_PoolDatum)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "User To Send Back Deposit: %s" (P.show userToSendBack)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "Value For User: %s" (P.show value_For_SendBackDeposit_To_User)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out: %s" (P.show poolDatum_Out)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out Value: %s" (P.show value_For_PoolDatum)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeemer For Consuming Validator Datum: %s" (P.show redeemer_For_Consuming_Validator_Datum)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
        ----------------------------------
        let
            lookupsTx =
                lookupsTx_Mint_TxID_Master_SendBackDeposit P.<>
                lookupsTx_Burn_UserID P.<>
                LedgerConstraints.unspentOutputs uTxOsAtMaster P.<>
                LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_ScriptDatum]) P.<>
                LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum]) P.<>
                LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_UserDatum])

            tx =
                tx_Mint_TxID_Master_SendBackDeposit P.<>
                tx_Burn_UserID P.<>
                LedgerConstraints.mustSpendScriptOutputWithReference (fst uTxO_With_PoolDatum) (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum) (fst uTxO_With_ScriptDatum)  P.<>
                LedgerConstraints.mustPayToOtherScriptWithInlineDatum validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData poolDatum_Out) value_For_PoolDatum P.<>
                LedgerConstraints.mustSpendScriptOutputWithReference (fst uTxO_With_UserDatum) (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum) (fst uTxO_With_ScriptDatum) P.<>
                LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash userToSendBack) value_For_SendBackDeposit_To_User  P.<>
                LedgerConstraints.mustValidateIn validityRange P.<>
                LedgerConstraints.mustBeSignedBy masterPPKH
        ---------------------
        submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
        txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
        PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Send Back Deposit (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
    else do
        uTxOs_With_FundDatums' <- OffChainHelpers.getUTxOs_With_FundDatum fundID_AC uTxOsAtValidator
        ---------------------
        !uTxOs_With_FundDatums <-
            case uTxOs_With_FundDatums' of
                [] ->
                    PlutusContract.throwError "Can't find any uTxO with FundDatum"
                x -> do
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxOs List with FundDatum: %s" (P.show $ fst <$> x )
                    return x
        let
            !uTxO_With_FundDatum = head uTxOs_With_FundDatums
        ---------------------
            !fundDatum_In = Helpers.getFundDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_FundDatum
            !fundDatum_Out = T.FundDatum fundDatum_In
        ---------------------
            !value_In_FundDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_FundDatum
            !value_For_FundDatum = value_In_FundDatum <> value_For_Mint_TxID_Master_SendBackDeposit <> value_In_UserDatum <> value_For_Burn_UserID <> negate value_For_SendBackDeposit_To_User
        ---------------------
        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Send Back Deposit : Ending ---------------------------------------"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum In: %s" (P.show fundDatum_In)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum In Value: %s" (P.show value_In_FundDatum)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum Out: %s" (P.show fundDatum_Out)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum Out Value: %s" (P.show value_For_FundDatum)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "UserDatum In: %s" (P.show userDatum_IN)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "UserDatum In Value: %s" (P.show value_In_UserDatum)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "User To Send Back Deposit: %s" (P.show userToSendBack)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "Value For User: %s" (P.show value_For_SendBackDeposit_To_User)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeemer For Consuming Validator Datum: %s" (P.show redeemer_For_Consuming_Validator_Datum)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
        ---------------------
        let
            lookupsTx =
                lookupsTx_Mint_TxID_Master_SendBackDeposit P.<>
                lookupsTx_Burn_UserID P.<>
                LedgerConstraints.unspentOutputs uTxOsAtMaster P.<>
                LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_ScriptDatum]) P.<>
                LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum] ) P.<>
                LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_FundDatum] ) P.<>
                LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_UserDatum])

            tx =
                tx_Mint_TxID_Master_SendBackDeposit P.<>
                tx_Burn_UserID P.<>
                LedgerConstraints.mustReferenceOutput (fst uTxO_With_PoolDatum) P.<>
                LedgerConstraints.mustSpendScriptOutputWithReference (fst uTxO_With_FundDatum) (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum)  (fst uTxO_With_ScriptDatum) P.<>
                LedgerConstraints.mustPayToOtherScriptWithDatumInTx validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundDatum_Out) value_For_FundDatum P.<>
                LedgerConstraints.mustSpendScriptOutputWithReference (fst uTxO_With_UserDatum) (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum) (fst uTxO_With_ScriptDatum) P.<>
                LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash userToSendBack) value_For_SendBackDeposit_To_User  P.<>
                LedgerConstraints.mustValidateIn validityRange P.<>
                LedgerConstraints.mustBeSignedBy masterPPKH
        ---------------------
        submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void  lookupsTx tx
        txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
        PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Send Back Deposit (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
--------------------------------------------------------------------------------------------

masterAddScripts :: T.PABMasterAddScriptsParams -> PlutusContract.Contract w s DataText.Text ()
masterAddScripts T.PABMasterAddScriptsParams{..} = do
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Add Scripts : Init -------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    (now,_) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    ---------------------
    masterPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let
        !master = Ledger.unPaymentPubKeyHash masterPPKH
        !masterAdds = Ledger.pubKeyHashAddress masterPPKH Nothing
    -- uTxOsAtMaster <- PlutusContract.utxosAt masterAdds
    ---------------------
    let
        !pabParams = pmasPABPoolParams
        !pParams = T.pppPoolParams pabParams
        -- !policy_PoolID = T.pppPolicy_PoolID pabParams
        -- !validatorHash = T.pppValidatorHash pabParams
        !validatorAddress = T.pppValidatorAddress  pabParams
    ---------------------
        !poolID_CS = T.ppPoolID_CS pParams
        !poolID_AC = LedgerValue.AssetClass (poolID_CS, T.poolID_TN)
    ---------------------
    uTxOsAtValidator <- PlutusContract.utxosAt validatorAddress
    ---------------------
    !uTxO_With_PoolDatum' <- OffChainHelpers.getUTxO_With_PoolDatum poolID_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_PoolDatum <-
        case uTxO_With_PoolDatum' of
            Nothing -> do
                PlutusContract.throwError "Can't find any uTxO with PoolDatum"
            Just uTxO_With_PoolDatum -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with PoolDatum: %s" (P.show $ fst uTxO_With_PoolDatum)
                return uTxO_With_PoolDatum
    ---------------------
    let
        !policy_TxID_Master_Fund = T.pppPolicy_TxID_Master_Fund pabParams
        !policy_TxID_Master_FundAndMerge = T.pppPolicy_TxID_Master_FundAndMerge pabParams
        !policy_TxID_Master_SplitFund = T.pppPolicy_TxID_Master_SplitFund pabParams
        !policy_TxID_Master_ClosePool = T.pppPolicy_TxID_Master_ClosePool pabParams
        !policy_TxID_Master_TerminatePool = T.pppPolicy_TxID_Master_TerminatePool pabParams
        !policy_TxID_Master_Emergency = T.pppPolicy_TxID_Master_Emergency pabParams
        !policy_TxID_Master_DeleteFund = T.pppPolicy_TxID_Master_DeleteFund pabParams
        !policy_TxID_Master_SendBackFund = T.pppPolicy_TxID_Master_SendBackFund pabParams
        !policy_TxID_Master_SendBackDeposit = T.pppPolicy_TxID_Master_SendBackDeposit pabParams
        -- !policy_TxID_Master_AddScripts = T.pppPolicy_TxID_Master_AddScripts pabParams
        !policy_TxID_Master_DeleteScripts = T.pppPolicy_TxID_Master_DeleteScripts pabParams
        !policy_TxID_User_Deposit = T.pppPolicy_TxID_User_Deposit pabParams
        !policy_TxID_User_Harvest = T.pppPolicy_TxID_User_Harvest pabParams
        !policy_TxID_User_Withdraw = T.pppPolicy_TxID_User_Withdraw pabParams
    ---------------------
        !scriptMintingHash_TxID_Master_Fund = Utils.hashScriptMinting policy_TxID_Master_Fund
        !scriptMintingHash_TxID_Master_FundAndMerge = Utils.hashScriptMinting policy_TxID_Master_FundAndMerge
        !scriptMintingHash_TxID_Master_SplitFund = Utils.hashScriptMinting policy_TxID_Master_SplitFund
        !scriptMintingHash_TxID_Master_ClosePool = Utils.hashScriptMinting policy_TxID_Master_ClosePool
        !scriptMintingHash_TxID_Master_TerminatePool = Utils.hashScriptMinting policy_TxID_Master_TerminatePool
        !scriptMintingHash_TxID_Master_Emergency = Utils.hashScriptMinting policy_TxID_Master_Emergency
        !scriptMintingHash_TxID_Master_DeleteFund = Utils.hashScriptMinting policy_TxID_Master_DeleteFund
        !scriptMintingHash_TxID_Master_SendBackFund = Utils.hashScriptMinting policy_TxID_Master_SendBackFund
        !scriptMintingHash_TxID_Master_SendBackDeposit = Utils.hashScriptMinting policy_TxID_Master_SendBackDeposit
        -- !scriptMintingHash_TxID_Master_AddScripts = Utils.hashScriptMinting policy_TxID_Master_AddScripts
        !scriptMintingHash_TxID_Master_DeleteScripts = Utils.hashScriptMinting policy_TxID_Master_DeleteScripts
        !scriptMintingHash_TxID_User_Deposit = Utils.hashScriptMinting policy_TxID_User_Deposit
        !scriptMintingHash_TxID_User_Harvest = Utils.hashScriptMinting policy_TxID_User_Harvest
        !scriptMintingHash_TxID_User_Withdraw = Utils.hashScriptMinting policy_TxID_User_Withdraw
    ---------------------
        !scriptID_CS =  T.pppCurSymbol_TxID_Master_AddScripts pabParams
        !scriptID_AC = LedgerValue.AssetClass (scriptID_CS, T.scriptID_TN)
    ---------------------
        -- !scriptID_Validator_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Validator_TN)
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
    let
    ---------------------
        !scriptDatum_Out = T.ScriptDatum T.ScriptDatumTypo {T.sdMaster = master}
    ---------------------
    !uTxO_With_Script_Master_Fund' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_Fund_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_FundAndMerge' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_FundAndMerge_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_SplitFund' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_SplitFund_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_ClosePool' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_ClosePool_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_TerminatePool' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_TerminatePool_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_Emergency' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_Emergency_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_DeleteFund' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_DeleteFund_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_SendBackFund' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_SendBackFund_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_SendBackDeposit' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_SendBackDeposit_AC uTxOsAtValidator
    ---------------------
    -- !uTxO_With_Script_Master_AddScripts' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_AddScripts_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_DeleteScripts' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_DeleteScripts_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_User_Deposit' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_User_Deposit_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_User_Harvest' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_User_Harvest_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_User_Withdraw' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_User_Withdraw_AC uTxOsAtValidator
    ---------------------
    let
        !value_For_Mint_TxID_Master_AddScripts = LedgerValue.assetClassValue scriptID_AC 1
    ---------------------
        !redeemer_For_Consuming_Validator_Datum = T.mkRedeemerMasterAddScripts master
        !redeemer_For_Mint_TxID_Master_AddScripts = T.mkRedeemerMint_TxID redeemer_For_Consuming_Validator_Datum
    ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Add Scripts : Ending ------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    !uTxO_With_Script_Master_AddScripts' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_AddScripts_AC uTxOsAtValidator
    ------------------------
    !uTxO_With_Script_Master_AddScripts <-
        case uTxO_With_Script_Master_AddScripts' of
            Nothing -> do
                PlutusContract.throwError "Can't find any uTxO with 'Master Add Scripts' Minting Script"
            Just uTxO_With_Script_Master_AddScripts -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with 'Master Add Scripts' Minting Script: %s" (P.show $ fst uTxO_With_Script_Master_AddScripts)
                return uTxO_With_Script_Master_AddScripts
    ------------------------
    do
        case uTxO_With_Script_Master_Fund' of
            Nothing -> do
                uTxOsAtMasterFor_TxID_Master_Fund_Datum <- PlutusContract.utxosAt masterAdds
                let
                    !value_For_Mint_ScriptID_Master_Fund = LedgerValue.assetClassValue scriptID_Master_Fund_AC 1
                    !value_For_Mint = value_For_Mint_TxID_Master_AddScripts <> value_For_Mint_ScriptID_Master_Fund
                    !value_For_Script_Master_Fund = LedgerAda.lovelaceValueOf 100000000 <> value_For_Mint_ScriptID_Master_Fund <> value_For_Mint_TxID_Master_AddScripts
                    (lookupsTx_Mint, tx_Mint) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMasterFor_TxID_Master_Fund_Datum uTxO_With_Script_Master_AddScripts (Just redeemer_For_Mint_TxID_Master_AddScripts) value_For_Mint validityRange masterPPKH
                    lookupsTx =
                        lookupsTx_Mint P.<>
                        LedgerConstraints.unspentOutputs uTxOsAtMasterFor_TxID_Master_Fund_Datum P.<>
                        LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum]) P.<>
                        LedgerConstraints.plutusV2MintingPolicy policy_TxID_Master_Fund
                    tx =
                        tx_Mint P.<>
                        LedgerConstraints.mustReferenceOutput (fst uTxO_With_PoolDatum) P.<>
                        LedgerConstraints.mustPayToAddressWithReferenceScript validatorAddress scriptMintingHash_TxID_Master_Fund (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData scriptDatum_Out) value_For_Script_Master_Fund  P.<>
                        LedgerConstraints.mustValidateIn validityRange P.<>
                        LedgerConstraints.mustBeSignedBy masterPPKH
                ------------------------
                submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
                txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
                PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Add Scripts (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
                ------------------------
            _ -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "Already have 'Master Fund' Minting Script"
    do
        case uTxO_With_Script_Master_FundAndMerge' of
            Nothing -> do
                uTxOsAtMasterFor_TxID_Master_FundAndMerge_Datum <- PlutusContract.utxosAt masterAdds
                let
                    !value_For_Mint_ScriptID_Master_FundAndMerge = LedgerValue.assetClassValue scriptID_Master_FundAndMerge_AC 1
                    !value_For_Mint = value_For_Mint_TxID_Master_AddScripts <> value_For_Mint_ScriptID_Master_FundAndMerge
                    !value_For_Script_Master_FundAndMerge = LedgerAda.lovelaceValueOf 100000000 <> value_For_Mint_ScriptID_Master_FundAndMerge <> value_For_Mint_TxID_Master_AddScripts
                    (lookupsTx_Mint, tx_Mint) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMasterFor_TxID_Master_FundAndMerge_Datum uTxO_With_Script_Master_AddScripts (Just redeemer_For_Mint_TxID_Master_AddScripts) value_For_Mint validityRange masterPPKH
                    lookupsTx =
                        lookupsTx_Mint P.<>
                        LedgerConstraints.unspentOutputs uTxOsAtMasterFor_TxID_Master_FundAndMerge_Datum P.<>
                        LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum]) P.<>
                        LedgerConstraints.plutusV2MintingPolicy policy_TxID_Master_FundAndMerge
                    tx =
                        tx_Mint P.<>
                        LedgerConstraints.mustReferenceOutput (fst uTxO_With_PoolDatum) P.<>
                        LedgerConstraints.mustPayToAddressWithReferenceScript validatorAddress scriptMintingHash_TxID_Master_FundAndMerge (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData scriptDatum_Out) value_For_Script_Master_FundAndMerge  P.<>
                        LedgerConstraints.mustValidateIn validityRange P.<>
                        LedgerConstraints.mustBeSignedBy masterPPKH
                ------------------------
                submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
                txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
                PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Add Scripts (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
                ------------------------
            _ -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "Already have 'Master Fund And Merge' Minting Script"
    do
        case uTxO_With_Script_Master_SplitFund' of
            Nothing -> do
                uTxOsAtMasterFor_TxID_Master_SplitFund_Datum <- PlutusContract.utxosAt masterAdds
                let
                    !value_For_Mint_ScriptID_Master_SplitFund = LedgerValue.assetClassValue scriptID_Master_SplitFund_AC 1
                    !value_For_Mint = value_For_Mint_TxID_Master_AddScripts <> value_For_Mint_ScriptID_Master_SplitFund
                    !value_For_Script_Master_SplitFund = LedgerAda.lovelaceValueOf 100000000 <> value_For_Mint_ScriptID_Master_SplitFund <> value_For_Mint_TxID_Master_AddScripts
                    (lookupsTx_Mint, tx_Mint) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMasterFor_TxID_Master_SplitFund_Datum uTxO_With_Script_Master_AddScripts (Just redeemer_For_Mint_TxID_Master_AddScripts) value_For_Mint validityRange masterPPKH
                    lookupsTx =
                        lookupsTx_Mint P.<>
                        LedgerConstraints.unspentOutputs uTxOsAtMasterFor_TxID_Master_SplitFund_Datum P.<>
                        LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum]) P.<>
                        LedgerConstraints.plutusV2MintingPolicy policy_TxID_Master_SplitFund
                    tx =
                        tx_Mint P.<>
                        LedgerConstraints.mustReferenceOutput (fst uTxO_With_PoolDatum) P.<>
                        LedgerConstraints.mustPayToAddressWithReferenceScript validatorAddress scriptMintingHash_TxID_Master_SplitFund (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData scriptDatum_Out) value_For_Script_Master_SplitFund  P.<>
                        LedgerConstraints.mustValidateIn validityRange P.<>
                        LedgerConstraints.mustBeSignedBy masterPPKH
                ------------------------
                submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
                txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
                PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Add Scripts (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
                ------------------------
            _ -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "Already have 'Master Split Fund' Minting Script"
    do
        case uTxO_With_Script_Master_ClosePool' of
            Nothing -> do
                uTxOsAtMasterFor_TxID_Master_ClosePool_Datum <- PlutusContract.utxosAt masterAdds
                let
                    !value_For_Mint_ScriptID_Master_ClosePool = LedgerValue.assetClassValue scriptID_Master_ClosePool_AC 1
                    !value_For_Mint = value_For_Mint_TxID_Master_AddScripts <> value_For_Mint_ScriptID_Master_ClosePool
                    !value_For_Script_Master_ClosePool = LedgerAda.lovelaceValueOf 100000000 <> value_For_Mint_ScriptID_Master_ClosePool <> value_For_Mint_TxID_Master_AddScripts
                    (lookupsTx_Mint, tx_Mint) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMasterFor_TxID_Master_ClosePool_Datum uTxO_With_Script_Master_AddScripts (Just redeemer_For_Mint_TxID_Master_AddScripts) value_For_Mint validityRange masterPPKH
                    lookupsTx =
                        lookupsTx_Mint P.<>
                        LedgerConstraints.unspentOutputs uTxOsAtMasterFor_TxID_Master_ClosePool_Datum P.<>
                        LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum]) P.<>
                        LedgerConstraints.plutusV2MintingPolicy policy_TxID_Master_ClosePool
                    tx =
                        tx_Mint P.<>
                        LedgerConstraints.mustReferenceOutput (fst uTxO_With_PoolDatum) P.<>
                        LedgerConstraints.mustPayToAddressWithReferenceScript validatorAddress scriptMintingHash_TxID_Master_ClosePool (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData scriptDatum_Out) value_For_Script_Master_ClosePool  P.<>
                        LedgerConstraints.mustValidateIn validityRange P.<>
                        LedgerConstraints.mustBeSignedBy masterPPKH
                ------------------------
                submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
                txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
                PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Add Scripts (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
                ------------------------
            _ -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "Already have 'Master Close Pool' Minting Script"
    do
        case uTxO_With_Script_Master_TerminatePool' of
            Nothing -> do
                uTxOsAtMasterFor_TxID_Master_TerminatePool_Datum <- PlutusContract.utxosAt masterAdds
                let
                    !value_For_Mint_ScriptID_Master_TerminatePool = LedgerValue.assetClassValue scriptID_Master_TerminatePool_AC 1
                    !value_For_Mint = value_For_Mint_TxID_Master_AddScripts <> value_For_Mint_ScriptID_Master_TerminatePool
                    !value_For_Script_Master_TerminatePool = LedgerAda.lovelaceValueOf 100000000 <> value_For_Mint_ScriptID_Master_TerminatePool <> value_For_Mint_TxID_Master_AddScripts
                    (lookupsTx_Mint, tx_Mint) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMasterFor_TxID_Master_TerminatePool_Datum uTxO_With_Script_Master_AddScripts (Just redeemer_For_Mint_TxID_Master_AddScripts) value_For_Mint validityRange masterPPKH
                    lookupsTx =
                        lookupsTx_Mint P.<>
                        LedgerConstraints.unspentOutputs uTxOsAtMasterFor_TxID_Master_TerminatePool_Datum P.<>
                        LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum]) P.<>
                        LedgerConstraints.plutusV2MintingPolicy policy_TxID_Master_TerminatePool
                    tx =
                        tx_Mint P.<>
                        LedgerConstraints.mustReferenceOutput (fst uTxO_With_PoolDatum) P.<>
                        LedgerConstraints.mustPayToAddressWithReferenceScript validatorAddress scriptMintingHash_TxID_Master_TerminatePool (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData scriptDatum_Out) value_For_Script_Master_TerminatePool  P.<>
                        LedgerConstraints.mustValidateIn validityRange P.<>
                        LedgerConstraints.mustBeSignedBy masterPPKH
                ------------------------
                submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
                txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
                PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Add Scripts (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
                ------------------------
            _ -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "Already have 'Master Terminate Pool' Minting Script"
    do
        case uTxO_With_Script_Master_Emergency' of
            Nothing -> do
                uTxOsAtMasterFor_TxID_Master_Emergency_Datum <- PlutusContract.utxosAt masterAdds
                let
                    !value_For_Mint_ScriptID_Master_Emergency = LedgerValue.assetClassValue scriptID_Master_Emergency_AC 1
                    !value_For_Mint = value_For_Mint_TxID_Master_AddScripts <> value_For_Mint_ScriptID_Master_Emergency
                    !value_For_Script_Master_Emergency = LedgerAda.lovelaceValueOf 100000000 <> value_For_Mint_ScriptID_Master_Emergency <> value_For_Mint_TxID_Master_AddScripts
                    (lookupsTx_Mint, tx_Mint) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMasterFor_TxID_Master_Emergency_Datum uTxO_With_Script_Master_AddScripts (Just redeemer_For_Mint_TxID_Master_AddScripts) value_For_Mint validityRange masterPPKH
                    lookupsTx =
                        lookupsTx_Mint P.<>
                        LedgerConstraints.unspentOutputs uTxOsAtMasterFor_TxID_Master_Emergency_Datum P.<>
                        LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum]) P.<>
                        LedgerConstraints.plutusV2MintingPolicy policy_TxID_Master_Emergency
                    tx =
                        tx_Mint P.<>
                        LedgerConstraints.mustReferenceOutput (fst uTxO_With_PoolDatum) P.<>
                        LedgerConstraints.mustPayToAddressWithReferenceScript validatorAddress scriptMintingHash_TxID_Master_Emergency (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData scriptDatum_Out) value_For_Script_Master_Emergency  P.<>
                        LedgerConstraints.mustValidateIn validityRange P.<>
                        LedgerConstraints.mustBeSignedBy masterPPKH
                ------------------------
                submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
                txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
                PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Add Scripts (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
                ------------------------
            _ -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "Already have 'Master Emergency' Minting Script"
    do
        case uTxO_With_Script_Master_DeleteFund' of
            Nothing -> do
                uTxOsAtMasterFor_TxID_Master_DeleteFund_Datum <- PlutusContract.utxosAt masterAdds
                let
                    !value_For_Mint_ScriptID_Master_DeleteFund = LedgerValue.assetClassValue scriptID_Master_DeleteFund_AC 1
                    !value_For_Mint = value_For_Mint_TxID_Master_AddScripts <> value_For_Mint_ScriptID_Master_DeleteFund
                    !value_For_Script_Master_DeleteFund = LedgerAda.lovelaceValueOf 100000000 <> value_For_Mint_ScriptID_Master_DeleteFund <> value_For_Mint_TxID_Master_AddScripts
                    (lookupsTx_Mint, tx_Mint) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMasterFor_TxID_Master_DeleteFund_Datum uTxO_With_Script_Master_AddScripts (Just redeemer_For_Mint_TxID_Master_AddScripts) value_For_Mint validityRange masterPPKH
                    lookupsTx =
                        lookupsTx_Mint P.<>
                        LedgerConstraints.unspentOutputs uTxOsAtMasterFor_TxID_Master_DeleteFund_Datum P.<>
                        LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum]) P.<>
                        LedgerConstraints.plutusV2MintingPolicy policy_TxID_Master_DeleteFund
                    tx =
                        tx_Mint P.<>
                        LedgerConstraints.mustReferenceOutput (fst uTxO_With_PoolDatum) P.<>
                        LedgerConstraints.mustPayToAddressWithReferenceScript validatorAddress scriptMintingHash_TxID_Master_DeleteFund (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData scriptDatum_Out) value_For_Script_Master_DeleteFund  P.<>
                        LedgerConstraints.mustValidateIn validityRange P.<>
                        LedgerConstraints.mustBeSignedBy masterPPKH
                ------------------------
                submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
                txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
                PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Add Scripts (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
                ------------------------
            _ -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "Already have 'Master Delete Fund' Minting Script"
    do
        case uTxO_With_Script_Master_SendBackFund' of
            Nothing -> do
                uTxOsAtMasterFor_TxID_Master_SendBackFund_Datum <- PlutusContract.utxosAt masterAdds
                let
                    !value_For_Mint_ScriptID_Master_SendBackFund = LedgerValue.assetClassValue scriptID_Master_SendBackFund_AC 1
                    !value_For_Mint = value_For_Mint_TxID_Master_AddScripts <> value_For_Mint_ScriptID_Master_SendBackFund
                    !value_For_Script_Master_SendBackFund = LedgerAda.lovelaceValueOf 100000000 <> value_For_Mint_ScriptID_Master_SendBackFund <> value_For_Mint_TxID_Master_AddScripts
                    (lookupsTx_Mint, tx_Mint) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMasterFor_TxID_Master_SendBackFund_Datum uTxO_With_Script_Master_AddScripts (Just redeemer_For_Mint_TxID_Master_AddScripts) value_For_Mint validityRange masterPPKH
                    lookupsTx =
                        lookupsTx_Mint P.<>
                        LedgerConstraints.unspentOutputs uTxOsAtMasterFor_TxID_Master_SendBackFund_Datum P.<>
                        LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum]) P.<>
                        LedgerConstraints.plutusV2MintingPolicy policy_TxID_Master_SendBackFund
                    tx =
                        tx_Mint P.<>
                        LedgerConstraints.mustReferenceOutput (fst uTxO_With_PoolDatum) P.<>
                        LedgerConstraints.mustPayToAddressWithReferenceScript validatorAddress scriptMintingHash_TxID_Master_SendBackFund (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData scriptDatum_Out) value_For_Script_Master_SendBackFund  P.<>
                        LedgerConstraints.mustValidateIn validityRange P.<>
                        LedgerConstraints.mustBeSignedBy masterPPKH
                ------------------------
                submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
                txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
                PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Add Scripts (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
                ------------------------
            _ -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "Already have 'Master Send Back Fund' Minting Script"
    do
        case uTxO_With_Script_Master_SendBackDeposit' of
            Nothing -> do
                uTxOsAtMasterFor_TxID_Master_SendBackDeposit_Datum <- PlutusContract.utxosAt masterAdds
                let
                    !value_For_Mint_ScriptID_Master_SendBackDeposit = LedgerValue.assetClassValue scriptID_Master_SendBackDeposit_AC 1
                    !value_For_Mint = value_For_Mint_TxID_Master_AddScripts <> value_For_Mint_ScriptID_Master_SendBackDeposit
                    !value_For_Script_Master_SendBackDeposit = LedgerAda.lovelaceValueOf 100000000 <> value_For_Mint_ScriptID_Master_SendBackDeposit <> value_For_Mint_TxID_Master_AddScripts
                    (lookupsTx_Mint, tx_Mint) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMasterFor_TxID_Master_SendBackDeposit_Datum uTxO_With_Script_Master_AddScripts (Just redeemer_For_Mint_TxID_Master_AddScripts) value_For_Mint validityRange masterPPKH
                    lookupsTx =
                        lookupsTx_Mint P.<>
                        LedgerConstraints.unspentOutputs uTxOsAtMasterFor_TxID_Master_SendBackDeposit_Datum P.<>
                        LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum]) P.<>
                        LedgerConstraints.plutusV2MintingPolicy policy_TxID_Master_SendBackDeposit
                    tx =
                        tx_Mint P.<>
                        LedgerConstraints.mustReferenceOutput (fst uTxO_With_PoolDatum) P.<>
                        LedgerConstraints.mustPayToAddressWithReferenceScript validatorAddress scriptMintingHash_TxID_Master_SendBackDeposit (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData scriptDatum_Out) value_For_Script_Master_SendBackDeposit  P.<>
                        LedgerConstraints.mustValidateIn validityRange P.<>
                        LedgerConstraints.mustBeSignedBy masterPPKH
                ------------------------
                submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
                txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
                PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Add Scripts (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
                ------------------------
            _ -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "Already have 'Master Send Back Deposit' Minting Script"
    do
        case uTxO_With_Script_Master_DeleteScripts' of
            Nothing -> do
                uTxOsAtMasterFor_TxID_Master_DeleteScripts_Datum <- PlutusContract.utxosAt masterAdds
                let
                    !value_For_Mint_ScriptID_Master_DeleteScripts = LedgerValue.assetClassValue scriptID_Master_DeleteScripts_AC 1
                    !value_For_Mint = value_For_Mint_TxID_Master_AddScripts <> value_For_Mint_ScriptID_Master_DeleteScripts
                    !value_For_Script_Master_DeleteScripts = LedgerAda.lovelaceValueOf 100000000 <> value_For_Mint_ScriptID_Master_DeleteScripts <> value_For_Mint_TxID_Master_AddScripts
                    (lookupsTx_Mint, tx_Mint) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMasterFor_TxID_Master_DeleteScripts_Datum uTxO_With_Script_Master_AddScripts (Just redeemer_For_Mint_TxID_Master_AddScripts) value_For_Mint validityRange masterPPKH
                    lookupsTx =
                        lookupsTx_Mint P.<>
                        LedgerConstraints.unspentOutputs uTxOsAtMasterFor_TxID_Master_DeleteScripts_Datum P.<>
                        LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum]) P.<>
                        LedgerConstraints.plutusV2MintingPolicy policy_TxID_Master_DeleteScripts
                    tx =
                        tx_Mint P.<>
                        LedgerConstraints.mustReferenceOutput (fst uTxO_With_PoolDatum) P.<>
                        LedgerConstraints.mustPayToAddressWithReferenceScript validatorAddress scriptMintingHash_TxID_Master_DeleteScripts (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData scriptDatum_Out) value_For_Script_Master_DeleteScripts  P.<>
                        LedgerConstraints.mustValidateIn validityRange P.<>
                        LedgerConstraints.mustBeSignedBy masterPPKH
                ------------------------
                submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
                txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
                PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Add Scripts (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
                ------------------------
            _ -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "Already have 'Master Delete Scripts' Minting Script"
    do
        case uTxO_With_Script_User_Deposit' of
            Nothing -> do
                uTxOsAtMasterFor_TxID_User_Deposit_Datum <- PlutusContract.utxosAt masterAdds
                let
                    !value_For_Mint_ScriptID_User_Deposit = LedgerValue.assetClassValue scriptID_User_Deposit_AC 1
                    !value_For_Mint = value_For_Mint_TxID_Master_AddScripts <> value_For_Mint_ScriptID_User_Deposit
                    !value_For_Script_User_Deposit = LedgerAda.lovelaceValueOf 100000000 <> value_For_Mint_ScriptID_User_Deposit <> value_For_Mint_TxID_Master_AddScripts
                    (lookupsTx_Mint, tx_Mint) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMasterFor_TxID_User_Deposit_Datum uTxO_With_Script_Master_AddScripts (Just redeemer_For_Mint_TxID_Master_AddScripts) value_For_Mint validityRange masterPPKH
                    lookupsTx =
                        lookupsTx_Mint P.<>
                        LedgerConstraints.unspentOutputs uTxOsAtMasterFor_TxID_User_Deposit_Datum P.<>
                        LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum]) P.<>
                        LedgerConstraints.plutusV2MintingPolicy policy_TxID_User_Deposit
                    tx =
                        tx_Mint P.<>
                        LedgerConstraints.mustReferenceOutput (fst uTxO_With_PoolDatum) P.<>
                        LedgerConstraints.mustPayToAddressWithReferenceScript validatorAddress scriptMintingHash_TxID_User_Deposit (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData scriptDatum_Out) value_For_Script_User_Deposit  P.<>
                        LedgerConstraints.mustValidateIn validityRange P.<>
                        LedgerConstraints.mustBeSignedBy masterPPKH
                ------------------------
                submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
                txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
                PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Add Scripts (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
                ------------------------
            _ -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "Already have 'User Deposit' Minting Script"
    do
        case uTxO_With_Script_User_Harvest' of
            Nothing -> do
                uTxOsAtMasterFor_TxID_User_Harvest_Datum <- PlutusContract.utxosAt masterAdds
                let
                    !value_For_Mint_ScriptID_User_Harvest = LedgerValue.assetClassValue scriptID_User_Harvest_AC 1
                    !value_For_Mint = value_For_Mint_TxID_Master_AddScripts <> value_For_Mint_ScriptID_User_Harvest
                    !value_For_Script_User_Harvest = LedgerAda.lovelaceValueOf 100000000 <> value_For_Mint_ScriptID_User_Harvest <> value_For_Mint_TxID_Master_AddScripts
                    (lookupsTx_Mint, tx_Mint) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMasterFor_TxID_User_Harvest_Datum uTxO_With_Script_Master_AddScripts (Just redeemer_For_Mint_TxID_Master_AddScripts) value_For_Mint validityRange masterPPKH
                    lookupsTx =
                        lookupsTx_Mint P.<>
                        LedgerConstraints.unspentOutputs uTxOsAtMasterFor_TxID_User_Harvest_Datum P.<>
                        LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum]) P.<>
                        LedgerConstraints.plutusV2MintingPolicy policy_TxID_User_Harvest
                    tx =
                        tx_Mint P.<>
                        LedgerConstraints.mustReferenceOutput (fst uTxO_With_PoolDatum) P.<>
                        LedgerConstraints.mustPayToAddressWithReferenceScript validatorAddress scriptMintingHash_TxID_User_Harvest (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData scriptDatum_Out) value_For_Script_User_Harvest  P.<>
                        LedgerConstraints.mustValidateIn validityRange P.<>
                        LedgerConstraints.mustBeSignedBy masterPPKH
                ------------------------
                submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
                txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
                PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Add Scripts (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
                ------------------------
            _ -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "Already have 'User Harvest' Minting Script"
    do
        case uTxO_With_Script_User_Withdraw' of
            Nothing -> do
                uTxOsAtMasterFor_TxID_User_Withdraw_Datum <- PlutusContract.utxosAt masterAdds
                let
                    !value_For_Mint_ScriptID_User_Withdraw = LedgerValue.assetClassValue scriptID_User_Withdraw_AC 1
                    !value_For_Mint = value_For_Mint_TxID_Master_AddScripts <> value_For_Mint_ScriptID_User_Withdraw
                    !value_For_Script_User_Withdraw = LedgerAda.lovelaceValueOf 100000000 <> value_For_Mint_ScriptID_User_Withdraw <> value_For_Mint_TxID_Master_AddScripts
                    (lookupsTx_Mint, tx_Mint) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMasterFor_TxID_User_Withdraw_Datum uTxO_With_Script_Master_AddScripts (Just redeemer_For_Mint_TxID_Master_AddScripts) value_For_Mint validityRange masterPPKH
                    lookupsTx =
                        lookupsTx_Mint P.<>
                        LedgerConstraints.unspentOutputs uTxOsAtMasterFor_TxID_User_Withdraw_Datum P.<>
                        LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum]) P.<>
                        LedgerConstraints.plutusV2MintingPolicy policy_TxID_User_Withdraw
                    tx =
                        tx_Mint P.<>
                        LedgerConstraints.mustReferenceOutput (fst uTxO_With_PoolDatum) P.<>
                        LedgerConstraints.mustPayToAddressWithReferenceScript validatorAddress scriptMintingHash_TxID_User_Withdraw (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData scriptDatum_Out) value_For_Script_User_Withdraw  P.<>
                        LedgerConstraints.mustValidateIn validityRange P.<>
                        LedgerConstraints.mustBeSignedBy masterPPKH
                ------------------------
                submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
                txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
                PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Add Scripts (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
                ------------------------
            _ -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "Already have 'User Withdraw' Minting Script"

--------------------------------------------------------------------------------------------

masterDeleteScripts :: T.PABMasterDeleteScriptsParams -> PlutusContract.Contract w s DataText.Text ()
masterDeleteScripts T.PABMasterDeleteScriptsParams{..} = do
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Delete Scripts : Init -------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    (now,_) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    ---------------------
    masterPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let
        !master = Ledger.unPaymentPubKeyHash masterPPKH
        !masterAdds = Ledger.pubKeyHashAddress masterPPKH Nothing
    uTxOsAtMaster <- PlutusContract.utxosAt masterAdds
    ---------------------
    let
        !pabParams = pmdsPABPoolParams
        !pParams = T.pppPoolParams pabParams
        !validatorHash = T.pppValidatorHash pabParams
        !validatorAddress = T.pppValidatorAddress pabParams
    ---------------------
        -- !policy_TxID_Master_DeleteScripts = T.pppPolicy_TxID_Master_DeleteScripts pabParams
    ---------------------
        !poolID_CS = T.ppPoolID_CS pParams
        !poolID_AC = LedgerValue.AssetClass (poolID_CS, T.poolID_TN)
    ---------------------
        !txID_Master_DeleteScripts_CS =  T.pppCurSymbol_TxID_Master_DeleteScripts pabParams
        !txID_Master_DeleteScripts_AC = LedgerValue.AssetClass (txID_Master_DeleteScripts_CS, T.txID_Master_DeleteScripts_TN)
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
    ---------------------
    !checkCollateral <- OffChainHelpers.checkIfThereIsUTxOFreeForCollateral uTxOsAtMaster
    if checkCollateral then
        PlutusContract.logInfo @P.String $ TextPrintf.printf "There is UTxO free for Collateral"
    else
        PlutusContract.throwError "There is NOT UTxO free for Collateral"
    ---------------------
    uTxOsAtValidator <- PlutusContract.utxosAt validatorAddress
    ---------------------
    !uTxO_With_ScriptDatum' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Validator_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_ScriptDatum <- 
        case uTxO_With_ScriptDatum' of
            Nothing -> do
                PlutusContract.throwError "Can't find any uTxO with Main Validator Script"
            Just uTxO_With_ScriptDatum -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with Main Validator Script: %s" (P.show $ fst uTxO_With_ScriptDatum)
                return uTxO_With_ScriptDatum
    ---------------------
    !uTxO_With_Script_Master_Fund' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_Fund_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_FundAndMerge' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_FundAndMerge_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_SplitFund' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_SplitFund_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_ClosePool' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_ClosePool_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_TerminatePool' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_TerminatePool_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_Emergency' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_Emergency_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_DeleteFund' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_DeleteFund_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_SendBackFund' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_SendBackFund_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_SendBackDeposit' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_SendBackDeposit_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_AddScripts' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_AddScripts_AC uTxOsAtValidator
    ------------------------
    !uTxO_With_Script_Master_AddScripts <-
        case uTxO_With_Script_Master_AddScripts' of
            Nothing -> do
                PlutusContract.throwError "Can't find any uTxO with 'Master Add Scripts' Minting Script"
            Just uTxO_With_Script_Master_AddScripts -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with 'Master Add Scripts' Minting Script: %s" (P.show $ fst uTxO_With_Script_Master_AddScripts)
                return uTxO_With_Script_Master_AddScripts
    ------------------------
    !uTxO_With_Script_Master_DeleteScripts' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_DeleteScripts_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_DeleteScripts <- 
        case uTxO_With_Script_Master_DeleteScripts' of
            Nothing -> do
                PlutusContract.throwError "Can't find any uTxO with 'Master Delete Scripts' Minting Script"
            Just uTxO_With_Script_Master_DeleteScripts -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with 'Master Delete Scripts' Minting Script: %s" (P.show $ fst uTxO_With_Script_Master_DeleteScripts)
                return uTxO_With_Script_Master_DeleteScripts   
    ---------------------
    !uTxO_With_Script_User_Deposit' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_User_Deposit_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_User_Harvest' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_User_Harvest_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_User_Withdraw' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_User_Withdraw_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_PoolDatum' <- OffChainHelpers.getUTxO_With_PoolDatum poolID_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_PoolDatum <-
        case uTxO_With_PoolDatum' of
            Nothing ->
                PlutusContract.throwError "Can't find any uTxO with PoolDatum"
            Just uTxO_With_PoolDatum -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with PoolDatum: %s" (P.show $ fst uTxO_With_PoolDatum)
                return uTxO_With_PoolDatum
    let     
        ---------------------
        !value_For_Burn_Each_TxID_Master_AddScripts  = LedgerValue.assetClassValue scriptID_AC (-1)
        ---------------------
        !uTxOsWithDatumAndValue' = concat
            [
                case uTxO_With_Script_Master_Fund' of
                    Nothing -> []
                    Just uTxO_With_Script_Master_Fund -> 
                        let
                            !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_Script_Master_Fund
                            !value_In_ScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_Script_Master_Fund
                            !value_For_Burn_ScriptID  = LedgerValue.assetClassValue scriptID_Master_Fund_AC (-1)
                            !value_For_Master = value_In_ScriptDatum <> value_For_Burn_Each_TxID_Master_AddScripts <> value_For_Burn_ScriptID
                            !master' = T.sdMaster scriptDatum
                        in 
                            [(uTxO_With_Script_Master_Fund, master', value_For_Master, value_For_Burn_ScriptID)]
            ,
               case uTxO_With_Script_Master_FundAndMerge' of
                    Nothing -> []
                    Just uTxO_With_Script_Master_FundAndMerge -> 
                        let
                            !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_Script_Master_FundAndMerge
                            !value_In_ScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_Script_Master_FundAndMerge
                            !value_For_Burn_ScriptID  = LedgerValue.assetClassValue scriptID_Master_FundAndMerge_AC (-1)
                            !value_For_Master = value_In_ScriptDatum <> value_For_Burn_Each_TxID_Master_AddScripts <> value_For_Burn_ScriptID
                            !master' = T.sdMaster scriptDatum
                        in
                            [(uTxO_With_Script_Master_FundAndMerge, master', value_For_Master, value_For_Burn_ScriptID)]
            ,    
                case uTxO_With_Script_Master_SplitFund' of
                    Nothing -> []
                    Just uTxO_With_Script_Master_SplitFund ->
                        let
                            !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_Script_Master_SplitFund
                            !value_In_ScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_Script_Master_SplitFund
                            !value_For_Burn_ScriptID  = LedgerValue.assetClassValue scriptID_Master_SplitFund_AC (-1)
                            !value_For_Master = value_In_ScriptDatum <> value_For_Burn_Each_TxID_Master_AddScripts <> value_For_Burn_ScriptID
                            !master' = T.sdMaster scriptDatum
                        in
                            [(uTxO_With_Script_Master_SplitFund, master', value_For_Master, value_For_Burn_ScriptID)]
            ,            
                case uTxO_With_Script_Master_ClosePool' of
                    Nothing -> []
                    Just uTxO_With_Script_Master_ClosePool ->
                        let
                            !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_Script_Master_ClosePool
                            !value_In_ScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_Script_Master_ClosePool
                            !value_For_Burn_ScriptID  = LedgerValue.assetClassValue scriptID_Master_ClosePool_AC (-1)
                            !value_For_Master = value_In_ScriptDatum <> value_For_Burn_Each_TxID_Master_AddScripts <> value_For_Burn_ScriptID
                            !master' = T.sdMaster scriptDatum
                        in
                            [(uTxO_With_Script_Master_ClosePool, master', value_For_Master, value_For_Burn_ScriptID)]
            ,    
                case uTxO_With_Script_Master_TerminatePool' of
                    Nothing -> []
                    Just uTxO_With_Script_Master_TerminatePool ->
                        let
                            !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_Script_Master_TerminatePool
                            !value_In_ScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_Script_Master_TerminatePool
                            !value_For_Burn_ScriptID  = LedgerValue.assetClassValue scriptID_Master_TerminatePool_AC (-1)
                            !value_For_Master = value_In_ScriptDatum <> value_For_Burn_Each_TxID_Master_AddScripts <> value_For_Burn_ScriptID
                            !master' = T.sdMaster scriptDatum
                        in
                            [(uTxO_With_Script_Master_TerminatePool, master', value_For_Master, value_For_Burn_ScriptID)]
            ,    
                case uTxO_With_Script_Master_Emergency' of
                    Nothing -> []
                    Just uTxO_With_Script_Master_Emergency ->
                        let
                            !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_Script_Master_Emergency
                            !value_In_ScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_Script_Master_Emergency
                            !value_For_Burn_ScriptID  = LedgerValue.assetClassValue scriptID_Master_Emergency_AC (-1)
                            !value_For_Master = value_In_ScriptDatum <> value_For_Burn_Each_TxID_Master_AddScripts <> value_For_Burn_ScriptID
                            !master' = T.sdMaster scriptDatum
                        in
                            [(uTxO_With_Script_Master_Emergency, master', value_For_Master, value_For_Burn_ScriptID)]
            ,    
                case uTxO_With_Script_Master_DeleteFund' of
                    Nothing -> []
                    Just uTxO_With_Script_Master_DeleteFund ->
                        let
                            !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_Script_Master_DeleteFund
                            !value_In_ScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_Script_Master_DeleteFund
                            !value_For_Burn_ScriptID  = LedgerValue.assetClassValue scriptID_Master_DeleteFund_AC (-1)
                            !value_For_Master = value_In_ScriptDatum <> value_For_Burn_Each_TxID_Master_AddScripts <> value_For_Burn_ScriptID
                            !master' = T.sdMaster scriptDatum
                        in
                            [(uTxO_With_Script_Master_DeleteFund, master', value_For_Master, value_For_Burn_ScriptID)]
            ,    
                case uTxO_With_Script_Master_SendBackFund' of
                    Nothing -> []
                    Just uTxO_With_Script_Master_SendBackFund ->
                        let
                            !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_Script_Master_SendBackFund
                            !value_In_ScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_Script_Master_SendBackFund
                            !value_For_Burn_ScriptID  = LedgerValue.assetClassValue scriptID_Master_SendBackFund_AC (-1)
                            !value_For_Master = value_In_ScriptDatum <> value_For_Burn_Each_TxID_Master_AddScripts <> value_For_Burn_ScriptID
                            !master' = T.sdMaster scriptDatum
                        in
                            [(uTxO_With_Script_Master_SendBackFund, master', value_For_Master, value_For_Burn_ScriptID)]
            ,    
                case uTxO_With_Script_Master_SendBackDeposit' of
                    Nothing -> []
                    Just uTxO_With_Script_Master_SendBackDeposit ->
                        let
                            !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_Script_Master_SendBackDeposit
                            !value_In_ScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_Script_Master_SendBackDeposit
                            !value_For_Burn_ScriptID  = LedgerValue.assetClassValue scriptID_Master_SendBackDeposit_AC (-1)
                            !value_For_Master = value_In_ScriptDatum <> value_For_Burn_Each_TxID_Master_AddScripts <> value_For_Burn_ScriptID
                            !master' = T.sdMaster scriptDatum
                        in
                            [(uTxO_With_Script_Master_SendBackDeposit, master', value_For_Master, value_For_Burn_ScriptID)]
            ,    
                case uTxO_With_Script_User_Deposit' of
                    Nothing -> []
                    Just uTxO_With_Script_User_Deposit ->
                        let
                            !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_Script_User_Deposit
                            !value_In_ScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_Script_User_Deposit
                            !value_For_Burn_ScriptID  = LedgerValue.assetClassValue scriptID_User_Deposit_AC (-1)
                            !value_For_Master = value_In_ScriptDatum <> value_For_Burn_Each_TxID_Master_AddScripts <> value_For_Burn_ScriptID
                            !master' = T.sdMaster scriptDatum
                        in
                            [(uTxO_With_Script_User_Deposit, master', value_For_Master, value_For_Burn_ScriptID)]
            ,    
                case uTxO_With_Script_User_Harvest' of
                    Nothing -> []
                    Just uTxO_With_Script_User_Harvest ->
                        let
                            !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_Script_User_Harvest
                            !value_In_ScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_Script_User_Harvest
                            !value_For_Burn_ScriptID  = LedgerValue.assetClassValue scriptID_User_Harvest_AC (-1)
                            !value_For_Master = value_In_ScriptDatum <> value_For_Burn_Each_TxID_Master_AddScripts <> value_For_Burn_ScriptID
                            !master' = T.sdMaster scriptDatum
                        in
                            [(uTxO_With_Script_User_Harvest, master', value_For_Master, value_For_Burn_ScriptID)]
            ,    
                case uTxO_With_Script_User_Withdraw' of
                    Nothing -> []
                    Just uTxO_With_Script_User_Withdraw ->
                        let
                            !scriptDatum = Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_Script_User_Withdraw
                            !value_In_ScriptDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_Script_User_Withdraw
                            !value_For_Burn_ScriptID  = LedgerValue.assetClassValue scriptID_User_Withdraw_AC (-1)
                            !value_For_Master = value_In_ScriptDatum <> value_For_Burn_Each_TxID_Master_AddScripts <> value_For_Burn_ScriptID
                            !master' = T.sdMaster scriptDatum
                        in
                            [(uTxO_With_Script_User_Withdraw, master', value_For_Master, value_For_Burn_ScriptID)]
            ]
    ---------------------
    !uTxOsWithDatumAndValue <-
        if length uTxOsWithDatumAndValue' > 0 then
            return $ take 2 uTxOsWithDatumAndValue'
        else    
            PlutusContract.throwError "Can't find any uTxO with ScriptDatum To Delete"
    ---------------------
    let 
        !value_For_Mint_TxID_Master_DeleteScripts = LedgerValue.assetClassValue txID_Master_DeleteScripts_AC 1
    ---------------------
        !value_For_Burn_All_TxID_Master_AddScripts  = negate $ LedgerValue.assetClassValue scriptID_AC (length uTxOsWithDatumAndValue)
        !value_For_Burn_All_ScriptID =  foldl (<>) (LedgerAda.lovelaceValueOf 0) [ value | (_, _, _, value) <- uTxOsWithDatumAndValue]
        !value_For_Burn = value_For_Burn_All_TxID_Master_AddScripts <> value_For_Burn_All_ScriptID
    ---------------------
        !value_In_PoolDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_PoolDatum
        !value_For_PoolDatum = value_In_PoolDatum <> value_For_Mint_TxID_Master_DeleteScripts 
    ---------------------
        !poolDatum_In = Helpers.getPoolDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_PoolDatum
        !poolDatum_Out = T.PoolDatum poolDatum_In
    ---------------------
        redeemer_For_Consuming_Validator_Datum = T.mkRedeemerMasterDeleteScripts master
    ---------------------
        !redeemer_For_Mint_TxID_Master_DeleteScripts = T.mkRedeemerMint_TxID redeemer_For_Consuming_Validator_Datum
        !redeemer_For_Burn_TxID = T.mkRedeemerBurn_TxID 
    ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Delete Scripts : Ending ---------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In: %s" (P.show poolDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In Value: %s" (P.show value_In_PoolDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out: %s" (P.show poolDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out Value: %s" (P.show value_For_PoolDatum)
    let
        formatScriptList = concat [  
            [
                "----", 
                "Master To Send Back: " ++ P.show master', 
                "Value For Master: " ++ P.show value
            ] 
            | (_, master', value, _) <- uTxOsWithDatumAndValue]
    
    mapM_ (PlutusContract.logInfo @P.String  ) formatScriptList

    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeemer For Consuming Validator Datum: %s" (P.show redeemer_For_Consuming_Validator_Datum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    let

        input_TxOut_Value_And_PoolDatum :: T.TxOut_Value_And_PoolDatum
        !input_TxOut_Value_And_PoolDatum =
            let
                c2 = snd uTxO_With_PoolDatum
            in
                (OffChainHelpers.getValueFromDecoratedTxOut c2 , Helpers.getPoolDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut c2) 
        

        inputs_TxOuts_Values_And_ScriptDatums :: [T.TxOut_Value_And_ScriptDatum]
        !inputs_TxOuts_Values_And_ScriptDatums =
            [(OffChainHelpers.getValueFromDecoratedTxOut ci , Helpers.getScriptDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut ci) | ((_, ci), _, _, _) <- uTxOsWithDatumAndValue]
        
        !correctOutput_PoolDatum_Value_WithTokens =
            let
                !value_In_PoolDatum' = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_PoolDatum
            ---------------------
                !value_For_Mint_TxID_Master_DeleteScripts' = LedgerValue.assetClassValue txID_Master_DeleteScripts_AC 1
            ---------------------
                !value_For_PoolDatum_Control = value_In_PoolDatum' <> value_For_Mint_TxID_Master_DeleteScripts'
                !value_For_PoolDatum_Real = value_For_PoolDatum
            in  
                value_For_PoolDatum_Real == value_For_PoolDatum_Control

        !correctFundAmount_SendBackToMaster = do
            let 
               
                joinSameMaster :: [(T.Master, LedgerApiV2.Value)] -> [(T.Master, LedgerApiV2.Value, LedgerApiV2.Value)]
                joinSameMaster list = joinSameMasterHelper [] list
                    where 
                        joinSameMasterHelper :: [(T.Master, LedgerApiV2.Value, LedgerApiV2.Value)] -> [(T.Master, LedgerApiV2.Value)] -> [(T.Master, LedgerApiV2.Value, LedgerApiV2.Value)]
                        joinSameMasterHelper seen [] = seen
                        joinSameMasterHelper seen ((master_To_SendBack', value_In_ScriptDatum'):xs) =
                            let
                                !master' = find (\(m, _, _) -> m == master_To_SendBack') seen
                            in
                                case master' of
                                    Nothing -> 
                                        let 
                                            !value_For_Master_Real = LedgerAda.lovelaceValueOf (100000000 * length inputs_TxOuts_Values_And_ScriptDatums)
                                            !elemet = (master_To_SendBack', value_In_ScriptDatum', value_For_Master_Real)
                                        in 
                                            joinSameMasterHelper (elemet:seen) xs
                                    Just (_, v1, v2) -> 
                                        let 
                                            !elemet = (master_To_SendBack', v1 <> value_In_ScriptDatum', v2)
                                            !seen_filter = filter (\(m', _, _) -> m' /= master_To_SendBack') seen
                                        in
                                            joinSameMasterHelper (elemet:seen_filter) xs

                !values_For_Each_Master = [ 
                        let 
                            !scriptDatum_In = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_ScriptDatum
                            !master_To_SendBack = T.sdMaster scriptDatum_In
                            !value_In_ScriptDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_ScriptDatum
                            !value_For_Burn'  = negate $ Helpers.getValueOfCurrencySymbol value_In_ScriptDatum scriptID_CS
                            !value_For_Master = value_In_ScriptDatum <> value_For_Burn'
                        in
                            (master_To_SendBack, value_For_Master)
                        | input_TxOut_Value_And_ScriptDatum <- inputs_TxOuts_Values_And_ScriptDatums
                    ]

                !values_For_Each_Master_Accumulated = joinSameMaster values_For_Each_Master

            PlutusContract.logInfo @P.String $ TextPrintf.printf "values_For_Each_Master_Accumulated: %s" (P.show values_For_Each_Master_Accumulated)
            return $ all (\(_, v1, v2) -> Helpers.valueIncludesValue v2 v1) values_For_Each_Master_Accumulated
       
    ---------------------
    correctFundAmount_SendBackToMaster' <- correctFundAmount_SendBackToMaster 
    ---------------------
    let
        formatList list = concat [  ["----", "Datum': " ++ P.show datum, "value: " ++ P.show value] | (datum, value) <- list]
    PlutusContract.logInfo @P.String $ TextPrintf.printf "inputs_TxOuts_Values_And_ScriptDatums"
    mapM_ (PlutusContract.logInfo @P.String  ) (formatList inputs_TxOuts_Values_And_ScriptDatums)
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "correctOutput_PoolDatum_Value_WithTokens: %s" (P.show correctOutput_PoolDatum_Value_WithTokens)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "correctFundAmount_SendBackToMaster: %s" (P.show correctFundAmount_SendBackToMaster')
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------

    let
        (lookupsTx_Mint_TxID_Master_DeleteScripts, tx_Mint_TxID_Master_DeleteScripts) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMaster uTxO_With_Script_Master_DeleteScripts (Just redeemer_For_Mint_TxID_Master_DeleteScripts) value_For_Mint_TxID_Master_DeleteScripts validityRange masterPPKH
        (lookupsTx_Burn, tx_Burn) = OffChainHelpers.burntToken_With_RefPolicy uTxOsAtMaster uTxO_With_Script_Master_AddScripts (Just redeemer_For_Burn_TxID) value_For_Burn validityRange masterPPKH

        lookupsTx =
            lookupsTx_Mint_TxID_Master_DeleteScripts P.<>
            lookupsTx_Burn P.<>
            LedgerConstraints.unspentOutputs uTxOsAtMaster P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_ScriptDatum]) P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum] ) P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [ uTxO | (uTxO, _, _, _) <- uTxOsWithDatumAndValue] )

        tx =
            tx_Mint_TxID_Master_DeleteScripts P.<>
            tx_Burn P.<>
            LedgerConstraints.mustSpendScriptOutputWithReference (fst uTxO_With_PoolDatum) (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum)  (fst uTxO_With_ScriptDatum) P.<>
            LedgerConstraints.mustPayToOtherScriptWithInlineDatum validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData poolDatum_Out) value_For_PoolDatum P.<>
            mconcat [ LedgerConstraints.mustSpendScriptOutputWithReference (fst uTxO) (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum) (fst uTxO_With_ScriptDatum) | (uTxO, _, _, _) <- uTxOsWithDatumAndValue ] P.<>
            mconcat [ LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash master') value | (_, master', value, _) <- uTxOsWithDatumAndValue ] P.<>
            LedgerConstraints.mustValidateIn validityRange P.<>
            LedgerConstraints.mustBeSignedBy masterPPKH
    ---------------------
    submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void  lookupsTx tx
    txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Delete Scripts (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)





--------------------------------------------------------------------------------------------
