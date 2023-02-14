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
module Validators.StakePlusV2.OffChain.EndPointsMaster1 where
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
import qualified Validators.StakePlusV2.OnChain.Tokens.Free.Policy          as FreePolicy
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

masterMintFree :: T.PABMasterMintFreeParams -> PlutusContract.Contract w s DataText.Text ()
masterMintFree T.PABMasterMintFreeParams{..} = do
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Mint : Init ---------------------------------------"
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
        !mintPolicyNum  = pmmfMintPolicyNum
        !mintTokenNameBase  = pmmfMintTokenNameBase
        !mintDiifTokenNameCount  = pmmfMintDiifTokenNameCount
        !mintAmount  = pmmfMintAmount
    ---------------------
        !policy_MintFree = FreePolicy.policy_Free mintPolicyNum
    ---------------------
        !mintFree_CS = Utils.getCurSymbolOfPolicy policy_MintFree
    ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
    ---------------------
        mintFree_TNS =
            if mintDiifTokenNameCount > 1 then do
                [ LedgerApiV2.TokenName (mintTokenNameBase <> Helpers.intToBBS num)  | num <- [1..mintDiifTokenNameCount] :: [Integer]]
            else
                [ LedgerApiV2.TokenName mintTokenNameBase ]
    PlutusContract.logInfo @P.String $ TextPrintf.printf "mintFree TNS: %s" (P.show mintFree_TNS)
    ---------------------
    let
        !value_For_Mint_MintFree = foldl (<>) (LedgerAda.lovelaceValueOf 0) ([
            let
                !mintFree_AC = LedgerValue.AssetClass (mintFree_CS, mintFree_TN')
            in
                LedgerValue.assetClassValue mintFree_AC mintAmount | mintFree_TN' <- mintFree_TNS
            ])

    --     -- let
    --     --     mintDo :: LedgerApiV2.TokenName -> PlutusContract.Contract w s DataText.Text ()
    --     --     mintDo mintFree_TN'= do
    --     --         uTxOsAtMasterForMint <- PlutusContract.utxosAt masterAdds
    --     --         let
    --     --         ---------------------
    --     --             !mintFree_AC = LedgerValue.AssetClass (mintFree_CS, mintFree_TN')
    --     --         ---------------------
    --     --             !value_For_Mint_MintFree = LedgerValue.assetClassValue mintFree_AC 1
    --     --         ---------------------
    --     --             lookupsTx =
    --     --                 LedgerConstraints.unspentOutputs uTxOsAtMasterForMint P.<>
    --     --                 LedgerConstraints.plutusV2MintingPolicy policy_MintFree
    --     --             tx =
    --     --                 LedgerConstraints.mustMintValue value_For_Mint_MintFree P.<>
    --     --                 LedgerConstraints.mustValidateIn validityRange P.<>
    --     --                 LedgerConstraints.mustBeSignedBy masterPPKH
    --     --         ------------------------
    --     --         submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
    --     --         txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
    --     --         PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master MintFree Pool (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)

    --     -- mapM_ mintDo mintFree_TNS

    -- else do
    --     let
    --         !mintFree_TN = LedgerApiV2.TokenName mintTokenNameBase
    --     ---------------------
    --         !mintFree_AC = LedgerValue.AssetClass (mintFree_CS, mintFree_TN)
    --     ---------------------
    --         !value_For_Mint_MintFree = LedgerValue.assetClassValue mintFree_AC mintAmount
        ---------------------
        lookupsTx =
            LedgerConstraints.unspentOutputs uTxOsAtMaster P.<>
            LedgerConstraints.plutusV2MintingPolicy policy_MintFree
        tx =
            LedgerConstraints.mustMintValue value_For_Mint_MintFree P.<>
            LedgerConstraints.mustValidateIn validityRange P.<>
            LedgerConstraints.mustBeSignedBy masterPPKH
    ------------------------
    submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
    txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Mint Free (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)

--------------------------------------------------------------------------------------------

masterPreparePool :: T.PABMasterPreparePoolParams -> PlutusContract.Contract w s DataText.Text ()
masterPreparePool T.PABMasterPreparePoolParams{..} = do
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Prepare Pool : Init ---------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    (now,_) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    ---------------------
    !masterPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let
        !master = Ledger.unPaymentPubKeyHash masterPPKH
        !masterAdds = Ledger.pubKeyHashAddress masterPPKH Nothing
    !uTxOsAtMaster <- PlutusContract.utxosAt masterAdds
    ---------------------
    let
        !pabParams = pmcpPABPoolParams
        !pParams = T.pppPoolParams pabParams
        !policy_PoolID = T.pppPolicy_PoolID pabParams
        !validator = T.pppValidator pabParams
        !validatorHash = T.pppValidatorHash pabParams
        !validatorAddress = T.pppValidatorAddress  pabParams
    ---------------------
        !policy_TxID_Master_AddScripts = T.pppPolicy_TxID_Master_AddScripts pabParams
    ---------------------
        !scriptValidatorHash = Utils.hashScriptValidator validator
        !scriptMintingHash_TxID_Master_AddScripts = Utils.hashScriptMinting policy_TxID_Master_AddScripts
    ---------------------
        !poolID_CS = T.ppPoolID_CS pParams
        !poolID_AC = LedgerValue.AssetClass (poolID_CS, T.poolID_TN)
    ---------------------
        !scriptID_CS =  T.pppCurSymbol_TxID_Master_AddScripts pabParams
        !scriptID_AC = LedgerValue.AssetClass (scriptID_CS, T.scriptID_TN)
    ---------------------
        !scriptID_Validator_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Validator_TN)
        !scriptID_Master_AddScripts_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_AddScripts_TN)
    ---------------------
    !poolID_TxOutRef <-
        case find (\(txOutRef, _) -> txOutRef == pmcpPoolID_TxOutRef) (DataMap.toList uTxOsAtMaster) of
            Nothing -> PlutusContract.throwError "Master Prepare Pool : Can't find uTxO for mint poolID"
            Just (txOutRef, _) -> do
                PlutusContract.logInfo @P.String "Master Prepare Pool : uTxO for mint poolID found!"
                return txOutRef
    ---------------------
    let
        !value_For_Mint_PoolID    = LedgerValue.assetClassValue poolID_AC 1
    ---------------------
        !value_For_PoolDatum' =  value_For_Mint_PoolID
        -- !value_Dummy = LedgerValue.assetClassValue T.dummy1_AC 1 <>  LedgerValue.assetClassValue T.dummy2_AC 1 <> LedgerValue.assetClassValue T.dummy3_AC 1 <> LedgerValue.assetClassValue T.dummy4_AC 1
        -- !minAda_For_PoolDatum = Helpers.calculateMinAda (value_For_PoolDatum' <> value_Dummy) True
        !minAda_For_PoolDatum_Normal = Helpers.calculateMinAdaOfValue value_For_PoolDatum' True
        !minAda_For_PoolDatum_ExtraTokens = Helpers.calculateMinAda T.maxDiffTokensForPoolAndFundDatum (T.tokenNameLenght*T.maxDiffTokensForPoolAndFundDatum) T.maxDiffTokensForPoolAndFundDatum True
        !minAda_For_PoolDatum = minAda_For_PoolDatum_Normal + minAda_For_PoolDatum_ExtraTokens
        !value_MinAda_For_PoolDatum = LedgerAda.lovelaceValueOf minAda_For_PoolDatum
        !value_For_PoolDatum = value_For_PoolDatum' <> value_MinAda_For_PoolDatum
    ---------------------
        !masterFunders = []
        !fundCount = 0
        !isClosedAt = Nothing
        !isTerminated = T.poolDatum_NotTerminated
        !isEmergency = T.poolDatum_NotEmergency
        !totalCashedOut = 0
        !minAda = minAda_For_PoolDatum
        !poolDatum_Out = T.mkPoolDatum masterFunders fundCount totalCashedOut isClosedAt isTerminated isEmergency minAda
    ---------------------
        !scriptDatum_Out = T.ScriptDatum T.ScriptDatumTypo {T.sdMaster = master}
    ---------------------
        -- !value_For_Mint_TxID_Master_AddScripts = LedgerValue.assetClassValue scriptID_AC 2
        !value_For_Each_TxID_Master_AddScripts = LedgerValue.assetClassValue scriptID_AC 1
        !value_For_Mint_ScriptID_Validator = LedgerValue.assetClassValue scriptID_Validator_AC 1
        !value_For_Mint_ScriptID_Master_AddScripts = LedgerValue.assetClassValue scriptID_Master_AddScripts_AC 1
    ---------------------
        !value_For_ScriptDatum = LedgerAda.lovelaceValueOf 100000000 <> value_For_Mint_ScriptID_Validator <> value_For_Each_TxID_Master_AddScripts
        !value_For_Script_Master_AddScripts_Datum = LedgerAda.lovelaceValueOf 100000000 <> value_For_Mint_ScriptID_Master_AddScripts <> value_For_Each_TxID_Master_AddScripts
        -- !value_For_Mint = value_For_Mint_TxID_Master_AddScripts <> value_For_Mint_ScriptID_Validator <> value_For_Mint_ScriptID_Master_AddScripts
    ---------------------
        !redeemer_For_Consuming_Validator_Datum = T.mkRedeemerMasterAddScripts master
        !redeemer_For_Mint_TxID_Master_AddScripts = T.mkRedeemerMint_TxID redeemer_For_Consuming_Validator_Datum
    ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Prepare Pool : Ending ------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out: %s" (P.show poolDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out Value: %s" (P.show value_For_PoolDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Script Validator Datum Out: %s" (P.show scriptDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Script Validator Datum Out Value: %s" (P.show value_For_ScriptDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Script TxID_Master_AddScripts Datum Out: %s" (P.show scriptDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Script TxID_Master_AddScripts Datum Out Value: %s" (P.show value_For_Script_Master_AddScripts_Datum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeemer For Consuming Validator Datum: %s" (P.show redeemer_For_Consuming_Validator_Datum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    -- do
    --     uTxOsAtMasterFor_TxID_Master_AddScripts_Datum <- PlutusContract.utxosAt masterAdds
    --     let
    --         (lookupsTx_Mint_TxID_Master_AddScripts, tx_Mint_TxID_Master_AddScripts) = OffChainHelpers.mintToken_With_Policy uTxOsAtMasterFor_TxID_Master_AddScripts_Datum policy_TxID_Master_AddScripts (Just redeemer_For_Mint_TxID_Master_AddScripts) value_For_Mint_TxID_Master_AddScripts validityRange masterPPKH
    --         lookupsTx =
    --             lookupsTx_Mint_TxID_Master_AddScripts P.<>
    --             LedgerConstraints.unspentOutputs uTxOsAtMasterFor_TxID_Master_AddScripts_Datum
    --         tx =
    --             tx_Mint_TxID_Master_AddScripts  P.<>
    --             LedgerConstraints.mustPayToAddressWithReferenceScript validatorAddress scriptMintingHash_TxID_Master_AddScripts (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData script_Master_AddScripts_Datum_Out) value_For_ScriptDatum  P.<>
    --             LedgerConstraints.mustValidateIn validityRange P.<>
    --             LedgerConstraints.mustBeSignedBy masterPPKH
    --     ------------------------
    --     submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
    --     txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
    --     PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Prepare Pool - Add Script 'Mster Add Script' (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
    -- ---------------------
    -- !uTxOsAtValidator <- PlutusContract.utxosAt validatorAddress
    -- ---------------------
    -- !uTxO_With_Script_Master_AddScripts' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_AddScripts_AC uTxOsAtValidator
    -- ------------------------
    -- !uTxO_With_Script_Master_AddScripts <-
    --     case uTxO_With_Script_Master_AddScripts' of
    --         Nothing -> do
    --             PlutusContract.throwError "Can't find any uTxO with 'Master Add Scripts' Minting Script"
    --         Just uTxO_With_Script_Master_AddScripts -> do
    --             PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with 'Master Add Scripts' Minting Script: %s" (P.show $ fst uTxO_With_Script_Master_AddScripts)
    --             return uTxO_With_Script_Master_AddScripts
    -- ------------------------
    -- !uTxOsAtMasterForPreparePool <- PlutusContract.utxosAt masterAdds
    ------------------------
    do
        let
            (lookupsTx_Mint_PoolID, tx_Mint_PoolID) = OffChainHelpers.mintNFT_With_TxOut uTxOsAtMaster policy_PoolID poolID_TxOutRef (Just redeemer_For_Mint_TxID_Master_AddScripts)  value_For_Mint_PoolID validityRange masterPPKH
            -- (lookupsTx_Mint_TxID_Master_AddScripts, tx_Mint_TxID_Master_AddScripts) = OffChainHelpers.mintToken_With_Policy uTxOsAtMaster policy_TxID_Master_AddScripts (Just redeemer_For_Mint_TxID_Master_AddScripts) value_For_Mint validityRange masterPPKH
            -- (lookupsTx_Mint_TxID_Master_AddScripts, tx_Mint_TxID_Master_AddScripts) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMasterForPreparePool uTxO_With_Script_Master_AddScripts (Just redeemer_For_Mint_TxID_Master_AddScripts) value_For_Mint_TxID_Master_AddScripts validityRange masterPPKH
        ---------------------    
        let
            lookupsTx =
                lookupsTx_Mint_PoolID P.<>
                -- lookupsTx_Mint_TxID_Master_AddScripts P.<>
                LedgerConstraints.unspentOutputs uTxOsAtMaster P.<>
                LedgerConstraints.plutusV2OtherScript validator
            tx =
                tx_Mint_PoolID  P.<>
                -- tx_Mint_TxID_Master_AddScripts  P.<>
                -- LedgerConstraints.mustPayToAddressWithReferenceScript validatorAddress scriptValidatorHash (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData scriptDatum_Out) value_For_ScriptDatum  P.<>
                -- LedgerConstraints.mustPayToAddressWithReferenceScript validatorAddress scriptMintingHash_TxID_Master_AddScripts (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData scriptDatum_Out) value_For_Script_Master_AddScripts_Datum  P.<>
                LedgerConstraints.mustPayToOtherScriptWithInlineDatum validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData poolDatum_Out) value_For_PoolDatum  P.<>
                LedgerConstraints.mustValidateIn validityRange P.<>
                LedgerConstraints.mustBeSignedBy masterPPKH
        ------------------------
        submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
        txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
        PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Prepare Pool (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
        ---------------------    
   
    do
        !uTxOsAtMasterForPreparePool <- PlutusContract.utxosAt masterAdds
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
        let
            -- value = value_For_Each_TxID_Master_AddScripts <>  value_For_Mint_ScriptID_Validator 
            value_For_Mint = value_For_Each_TxID_Master_AddScripts <> value_For_Mint_ScriptID_Master_AddScripts
            (lookupsTx_Mint_TxID_Master_AddScripts, tx_Mint_TxID_Master_AddScripts) = OffChainHelpers.mintToken_With_Policy uTxOsAtMasterForPreparePool policy_TxID_Master_AddScripts (Just redeemer_For_Mint_TxID_Master_AddScripts) value_For_Mint validityRange masterPPKH
        ---------------------    
        let
            lookupsTx =
                --lookupsTx_Mint_PoolID P.<>
                lookupsTx_Mint_TxID_Master_AddScripts P.<>
                LedgerConstraints.unspentOutputs uTxOsAtMasterForPreparePool P.<>
                LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum]) P.<>
                LedgerConstraints.plutusV2OtherScript validator
            tx =
                --tx_Mint_PoolID  P.<>
                tx_Mint_TxID_Master_AddScripts  P.<>
                LedgerConstraints.mustReferenceOutput (fst uTxO_With_PoolDatum) P.<>
                --LedgerConstraints.mustPayToAddressWithReferenceScript validatorAddress scriptValidatorHash (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData scriptDatum_Out) value_For_ScriptDatum  P.<>
                LedgerConstraints.mustPayToAddressWithReferenceScript validatorAddress scriptMintingHash_TxID_Master_AddScripts (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData scriptDatum_Out) value_For_Script_Master_AddScripts_Datum  P.<>
                --LedgerConstraints.mustPayToOtherScriptWithInlineDatum validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData poolDatum_Out) value_For_PoolDatum  P.<>
                LedgerConstraints.mustValidateIn validityRange P.<>
                LedgerConstraints.mustBeSignedBy masterPPKH
        ------------------------
        submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
        txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
        PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Prepare Pool (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
    ---------------------
    do
        !uTxOsAtMasterForPreparePool <- PlutusContract.utxosAt masterAdds
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
        let
            -- value = value_For_Each_TxID_Master_AddScripts <>  value_For_Mint_ScriptID_Validator 
            value_For_Mint = value_For_Each_TxID_Master_AddScripts <> value_For_Mint_ScriptID_Validator
            (lookupsTx_Mint_TxID_Master_AddScripts, tx_Mint_TxID_Master_AddScripts) = OffChainHelpers.mintToken_With_Policy uTxOsAtMasterForPreparePool policy_TxID_Master_AddScripts (Just redeemer_For_Mint_TxID_Master_AddScripts) value_For_Mint validityRange masterPPKH
        ---------------------    
        let
            lookupsTx =
                --lookupsTx_Mint_PoolID P.<>
                lookupsTx_Mint_TxID_Master_AddScripts P.<>
                LedgerConstraints.unspentOutputs uTxOsAtMasterForPreparePool P.<>
                LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum]) P.<>
                LedgerConstraints.plutusV2OtherScript validator
            tx =
                --tx_Mint_PoolID  P.<>
                tx_Mint_TxID_Master_AddScripts  P.<>
                LedgerConstraints.mustReferenceOutput (fst uTxO_With_PoolDatum) P.<>
                LedgerConstraints.mustPayToAddressWithReferenceScript validatorAddress scriptValidatorHash (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData scriptDatum_Out) value_For_ScriptDatum  P.<>
                --LedgerConstraints.mustPayToAddressWithReferenceScript validatorAddress scriptMintingHash_TxID_Master_AddScripts (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData scriptDatum_Out) value_For_Script_Master_AddScripts_Datum  P.<>
                --LedgerConstraints.mustPayToOtherScriptWithInlineDatum validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData poolDatum_Out) value_For_PoolDatum  P.<>
                LedgerConstraints.mustValidateIn validityRange P.<>
                LedgerConstraints.mustBeSignedBy masterPPKH
        ------------------------
        submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
        txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
        PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Prepare Pool (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
    ------------------------
    -- txUnBalanced <- PlutusContract.mkTxConstraints @DataVoid.Void lookupsTx tx    
    -- balanceTx <- PlutusContract.balanceTx txUnBalanced
    -- slot <- PlutusContract.currentNodeClientSlot
    -- let 
    -- --    (EmulatorTx tx2) = balanceTx
    -- --    (EmulatorTx  tx2) = balanceTx 
    -- --    (CardanoApiTx SomeCardanoApiTx	 
    --     (Both tx2 someCardanoApiTx2) = balanceTx
    -- --     validate = P.pure $ Ledger.validateTransactionOffChain tx2 
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "txUnBalanced: %s" (P.show txUnBalanced) 
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "balanceTx: %s" (P.show balanceTx) 
    -- let 
    --     validate = WalletEmulatorChain.validateL slot someCardanoApiTx2
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "validate: %s" (P.show validate) 
    
--------------------------------------------------------------------------------------------

masterFund :: PlutusContract.AsContractError DataText.Text => T.PABMasterFundParams -> PlutusContract.Contract w s DataText.Text ()
masterFund T.PABMasterFundParams{..} = do
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master New Fund : Init ----------------------------------------"
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
        !pabParams = pmfpPABPoolParams
        !pParams = T.pppPoolParams pabParams
        !validatorHash = T.pppValidatorHash pabParams
        !validatorAddress = T.pppValidatorAddress pabParams
    ---------------------
        -- !policy_TxID_Master_Fund = T.pppPolicy_TxID_Master_Fund pabParams
    ---------------------
        !harvest_CS = T.ppHarvest_CS pParams
        !harvest_AC = LedgerValue.AssetClass (harvest_CS, T.ppHarvest_TN pParams)
        !harvestIsAda = LedgerApiV2.adaSymbol == harvest_CS
        !haverstIsWithoutTokenName = not harvestIsAda && T.ppHarvest_TN pParams == LedgerApiV2.TokenName emptyByteString
    ---------------------
        !poolID_CS = T.ppPoolID_CS pParams
        !poolID_AC = LedgerValue.AssetClass (poolID_CS, T.poolID_TN)
    ---------------------
        !fundID_CS = T.pppCurSymbol_TxID_Master_Fund pabParams
        !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)
    ---------------------
        !scriptID_CS =  T.pppCurSymbol_TxID_Master_AddScripts pabParams
        !scriptID_AC = LedgerValue.AssetClass (scriptID_CS, T.scriptID_TN)
    ---------------------
        !scriptID_Validator_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Validator_TN)
        !scriptID_Master_Fund_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_Fund_TN)
    ---------------------
        !value_In_Master_Wallet = foldl (<>) (LedgerAda.lovelaceValueOf 0) (OffChainHelpers.getValueFromDecoratedTxOut . snd  <$> DataMap.toList uTxOsAtMaster)
    ---------------------
        !fundAmount = pmfpFundAmount
    ---------------------
    !value_FundAmount <- OffChainHelpers.createValueAddingTokensOfCurrencySymbol harvest_AC harvest_CS haverstIsWithoutTokenName value_In_Master_Wallet fundAmount
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
    -------------------
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
        !value_In_PoolDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_PoolDatum
        !value_For_PoolDatum = value_In_PoolDatum
    ---------------------
        !value_For_Mint_FundID     = LedgerValue.assetClassValue fundID_AC 1
    ---------------------
        !value_For_FundDatum' =  value_FundAmount <> value_For_Mint_FundID
        -- !value_Dummy = LedgerValue.assetClassValue T.dummy1_AC 1 <>  LedgerValue.assetClassValue T.dummy2_AC 1 <> LedgerValue.assetClassValue T.dummy3_AC 1 <> LedgerValue.assetClassValue T.dummy4_AC 1
        -- !minAda_For_FundDatum =  Helpers.calculateMinAda (value_For_FundDatum' <> value_Dummy) True
        !minAda_For_FundDatum_Normal = Helpers.calculateMinAdaOfValue value_For_FundDatum' True
        !minAda_For_FundDatum_ExtraTokens = Helpers.calculateMinAda T.maxDiffTokensForPoolAndFundDatum (T.tokenNameLenght*T.maxDiffTokensForPoolAndFundDatum) T.maxDiffTokensForPoolAndFundDatum True
        !minAda_For_FundDatum = minAda_For_FundDatum_Normal + minAda_For_FundDatum_ExtraTokens
        !value_MinAda_For_FundDatum = LedgerAda.lovelaceValueOf minAda_For_FundDatum
        !value_For_FundDatum = value_For_FundDatum' <> value_MinAda_For_FundDatum
    ---------------------
        !poolDatum_In = Helpers.getPoolDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_PoolDatum
        !poolDatum_Out = T.PoolDatum $ Helpers.mkUpdated_PoolDatum_With_NewFund poolDatum_In master fundAmount minAda_For_FundDatum
    ---------------------
        !cashedOut = 0
        !fundDatum_Out = T.mkFundDatum fundAmount cashedOut minAda_For_FundDatum
    ---------------------
        !redeemer_For_Consuming_Validator_Datum = T.mkRedeemerMasterFund master fundAmount minAda_For_FundDatum
        !redeemer_For_Mint_TxID_Master_Fund = T.mkRedeemerMint_TxID redeemer_For_Consuming_Validator_Datum
    ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master New Fund : Ending ---------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In: %s" (P.show poolDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In Value: %s" (P.show value_In_PoolDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out: %s" (P.show poolDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out Value: %s" (P.show value_For_PoolDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Fund Amount Value: %s" (P.show value_FundAmount)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum Out: %s" (P.show fundDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum Out Value: %s" (P.show value_For_FundDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeemer For Consuming Validator Datum: %s" (P.show redeemer_For_Consuming_Validator_Datum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    let
        (lookupsTx_Mint_FundID, tx_Mint_FundID) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMaster uTxO_With_Script_Master_Fund (Just redeemer_For_Mint_TxID_Master_Fund) value_For_Mint_FundID validityRange masterPPKH
    ---------------------
        lookupsTx =
            lookupsTx_Mint_FundID P.<>
            LedgerConstraints.unspentOutputs uTxOsAtMaster P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_ScriptDatum])  P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum] )
        tx =
            tx_Mint_FundID P.<>
            LedgerConstraints.mustSpendScriptOutputWithReference (fst uTxO_With_PoolDatum) (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum) (fst uTxO_With_ScriptDatum) P.<>
            LedgerConstraints.mustPayToOtherScriptWithInlineDatum validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData poolDatum_Out) value_For_PoolDatum P.<>
            LedgerConstraints.mustPayToOtherScriptWithDatumInTx validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundDatum_Out) value_For_FundDatum P.<>
            LedgerConstraints.mustValidateIn validityRange P.<>
            LedgerConstraints.mustBeSignedBy masterPPKH
    ---------------------
    submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void  lookupsTx tx
    txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Fund (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)

--------------------------------------------------------------------------------------------

masterFundAndMerge :: T.PABMasterFundAndMergeParams -> PlutusContract.Contract w s DataText.Text ()
masterFundAndMerge T.PABMasterFundAndMergeParams{..} = do
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Fund And Merge : Init ------------------------------"
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
        !pabParams = pmfampPABPoolParams
        !pParams = T.pppPoolParams pabParams
        !validatorHash = T.pppValidatorHash pabParams
        !validatorAddress = T.pppValidatorAddress pabParams
    ---------------------
        -- !policy_TxID_Master_FundAndMerge = T.pppPolicy_TxID_Master_FundAndMerge pabParams
    ---------------------
        !harvest_CS = T.ppHarvest_CS pParams
        !harvest_AC = LedgerValue.AssetClass (harvest_CS, T.ppHarvest_TN pParams)
        !harvestIsAda = LedgerApiV2.adaSymbol == harvest_CS
        !haverstIsWithoutTokenName = not harvestIsAda && T.ppHarvest_TN pParams == LedgerApiV2.TokenName emptyByteString
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
        !scriptID_Master_FundAndMerge_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_FundAndMerge_TN)
    ---------------------
        !txID_Master_FundAndMerge_CS =  T.pppCurSymbol_TxID_Master_FundAndMerge pabParams
        !txID_Master_FundAndMerge_AC = LedgerValue.AssetClass (txID_Master_FundAndMerge_CS, T.txID_Master_FundAndMerge_TN)
    ---------------------
        !value_In_Master_Wallet = foldl (<>) (LedgerAda.lovelaceValueOf 0) (OffChainHelpers.getValueFromDecoratedTxOut . snd  <$> DataMap.toList uTxOsAtMaster)
    ---------------------
        !fundAmount = pmfampFundAmount
    ---------------------
    !value_FundAmount <- OffChainHelpers.createValueAddingTokensOfCurrencySymbol harvest_AC harvest_CS haverstIsWithoutTokenName value_In_Master_Wallet fundAmount
    ---------------------
    let
        !uTxOToMerge = pmfampFundIDs_TxOutRefs
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
    -------------------
    !uTxO_With_Script_Master_FundAndMerge' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_FundAndMerge_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_FundAndMerge <-
        case uTxO_With_Script_Master_FundAndMerge' of
            Nothing ->
                PlutusContract.throwError "Can't find any uTxO with 'Master Fund And Merge' Minting Script"
            Just uTxO_With_Script_Master_FundAndMerge -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with 'Master Fund And Merge' Minting Script: %s" (P.show $ fst uTxO_With_Script_Master_FundAndMerge)
                return uTxO_With_Script_Master_FundAndMerge
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
    let !uTxOs_With_FundDatums_To_Merge' = [ (txOutRef, chainIndexTxOut)  |  (txOutRef, chainIndexTxOut) <- uTxOs_With_FundDatums, txOutRef `elem` uTxOToMerge]
    ---------------------
    !uTxOs_With_FundDatums_To_Merge <-
        case uTxOs_With_FundDatums_To_Merge' of
            [] ->
                PlutusContract.throwError "Can't find any uTxO with FundDatum at the chossen uTxOs to Merge"
            x -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxOs List with FundDatum to Merge: %s" (P.show $ fst <$> x )
                return x
    ---------------------
    let
    ---------------------
        !value_In_PoolDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_PoolDatum
        !value_For_PoolDatum = value_In_PoolDatum
    ---------------------
        !value_For_Mint_TxID_Master_FundAndMerge = LedgerValue.assetClassValue txID_Master_FundAndMerge_AC 1
    ---------------------
        !value_In_FundDatum_To_Merge = foldl (<>) (LedgerAda.lovelaceValueOf 0) (OffChainHelpers.getValueFromDecoratedTxOut . snd <$> uTxOs_With_FundDatums_To_Merge)
    ---------------------
        !value_For_FundDatum = value_FundAmount <> value_In_FundDatum_To_Merge <> value_For_Mint_TxID_Master_FundAndMerge
    ---------------------
        !mergingCount = length uTxOs_With_FundDatums_To_Merge
    ---------------------
        !poolDatum_In = Helpers.getPoolDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_PoolDatum
        !poolDatum_Out = T.PoolDatum $ Helpers.mkUpdated_PoolDatum_With_NewFundAmountAndMerging poolDatum_In master fundAmount mergingCount
    ---------------------
        !fundDatums_To_Merge = Helpers.getFundDatumTypo_FromMaybeDatum . OffChainHelpers.getDatumFromDecoratedTxOut . snd  <$> uTxOs_With_FundDatums_To_Merge
        !fundDatum_Out = T.FundDatum $ Helpers.mkUpdated_FundDatum_WithNewFundAmountAndMerging fundDatums_To_Merge fundAmount
    ---------------------
        !redeemer_For_Consuming_Validator_Datum = T.mkRedeemerMasterFundAndMerge master fundAmount
        !redeemer_For_Mint_TxID_Master_FundAndMerge = T.mkRedeemerMint_TxID redeemer_For_Consuming_Validator_Datum
    ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Fund And Merge : Ending ---------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In: %s" (P.show poolDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In Value: %s" (P.show value_In_PoolDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out: %s" (P.show poolDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out Value: %s" (P.show value_For_PoolDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum To Merge: %s" (P.show fundDatums_To_Merge)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum To Merge Value: %s" (P.show value_In_FundDatum_To_Merge)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Fund Amount Value: %s" (P.show value_FundAmount)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum Out: %s" (P.show fundDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum Out Value: %s" (P.show value_For_FundDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeemer For Consuming Validator Datum: %s" (P.show redeemer_For_Consuming_Validator_Datum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    let
        (lookupsTx_Mint_TxID_Master_FundAndMerge, tx_Mint_TxID_Master_FundAndMerge) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMaster uTxO_With_Script_Master_FundAndMerge (Just redeemer_For_Mint_TxID_Master_FundAndMerge) value_For_Mint_TxID_Master_FundAndMerge validityRange masterPPKH
    ---------------------
        lookupsTx =
            lookupsTx_Mint_TxID_Master_FundAndMerge P.<>
            LedgerConstraints.unspentOutputs uTxOsAtMaster P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_ScriptDatum])  P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum])     P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList uTxOs_With_FundDatums_To_Merge )
        tx =
            tx_Mint_TxID_Master_FundAndMerge P.<>
            LedgerConstraints.mustSpendScriptOutputWithReference (fst uTxO_With_PoolDatum) (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum) (fst uTxO_With_ScriptDatum) P.<>
            mconcat [LedgerConstraints.mustSpendScriptOutputWithReference txOutRef (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum) (fst uTxO_With_ScriptDatum) | txOutRef <- fst <$> uTxOs_With_FundDatums_To_Merge] P.<>
            LedgerConstraints.mustPayToOtherScriptWithInlineDatum validatorHash  (LedgerApiV2.Datum $ PlutusTx.toBuiltinData poolDatum_Out) value_For_PoolDatum P.<>
            LedgerConstraints.mustPayToOtherScriptWithDatumInTx validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundDatum_Out) value_For_FundDatum P.<>
            LedgerConstraints.mustValidateIn validityRange P.<>
            LedgerConstraints.mustBeSignedBy masterPPKH
    ---------------------
    submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
    txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Fund And Merge (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)

--------------------------------------------------------------------------------------------

masterSplitFund :: T.PABMasterSplitFundParams -> PlutusContract.Contract w s DataText.Text ()
masterSplitFund T.PABMasterSplitFundParams{..} = do
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Split Fund : Init ----------------------------------------"
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
        !pabParams = pmsPABPoolParams
        !pParams = T.pppPoolParams pabParams
        !validatorHash = T.pppValidatorHash pabParams
        !validatorAddress = T.pppValidatorAddress pabParams
    ---------------------
        -- !policy_TxID_Master_SplitFund = T.pppPolicy_TxID_Master_SplitFund pabParams
    ---------------------
        !harvest_CS = T.ppHarvest_CS pParams
        !harvest_AC = LedgerValue.AssetClass (harvest_CS, T.ppHarvest_TN pParams)
        !harvestIsAda = LedgerApiV2.adaSymbol == harvest_CS
        !haverstIsWithoutTokenName = not harvestIsAda && T.ppHarvest_TN pParams == LedgerApiV2.TokenName emptyByteString
    ---------------------
        !poolID_CS = T.ppPoolID_CS pParams
        !poolID_AC = LedgerValue.AssetClass (poolID_CS, T.poolID_TN)
    ---------------------
        !fundID_CS = T.pppCurSymbol_TxID_Master_Fund pabParams
        !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)
    ---------------------
        !scriptID_CS =  T.pppCurSymbol_TxID_Master_AddScripts pabParams
        !scriptID_AC = LedgerValue.AssetClass (scriptID_CS, T.scriptID_TN)
    ---------------------
        !scriptID_Validator_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Validator_TN)
        !scriptID_Master_Fund_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_Fund_TN )
        !scriptID_Master_SplitFund_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_SplitFund_TN )
    ---------------------
        !txID_Master_SplitFund_CS =  T.pppCurSymbol_TxID_Master_SplitFund pabParams
        !txID_Master_SplitFund_AC = LedgerValue.AssetClass (txID_Master_SplitFund_CS, T.txID_Master_SplitFund_TN)
    ---------------------
        !splitFundAmount = pmsSplitFundAmount
    ---------------------
        !uTxOToSplit = pmsFundID_TxOutRef
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
    -------------------
    !uTxO_With_Script_Master_SplitFund' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_SplitFund_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_SplitFund <-
        case uTxO_With_Script_Master_SplitFund' of
            Nothing ->
                PlutusContract.throwError "Can't find any uTxO with 'Master Split Fund' Minting Script"
            Just uTxO_With_Script_Master_SplitFund -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with 'Master Split Fund' Minting Script: %s" (P.show $ fst uTxO_With_Script_Master_SplitFund)
                return uTxO_With_Script_Master_SplitFund
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
    let !uTxO_With_FundDatumToSplit' =  find (\(txOutRef, _) -> txOutRef == uTxOToSplit) uTxOs_With_FundDatums
    ---------------------
    !uTxO_With_FundDatumToSplit <-
        case uTxO_With_FundDatumToSplit' of
            Nothing ->
                PlutusContract.throwError "Can't find any uTxO with FundDatum at the chossen uTxOs to Split"
            Just x -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with FundDatum to Split: %s" (P.show $ fst x )
                return x
    ---------------------
    let
        !fundDatum_To_Split = Helpers.getFundDatumTypo_FromMaybeDatum . OffChainHelpers.getDatumFromDecoratedTxOut . snd  $ uTxO_With_FundDatumToSplit
    ---------------------
        !maxFundAmount_ToSplit = Helpers.getFundAmountCanUse_in_FundDatum fundDatum_To_Split
    ---------------------
    if  splitFundAmount >= maxFundAmount_ToSplit then
        PlutusContract.throwError "Wrong Split Fund Amount"
    else pure ()
    ---------------------
    let
        !value_For_Mint_FundID     = LedgerValue.assetClassValue fundID_AC 1
    ---------------------
        !value_For_Mint_TxID_Master_SplitFund = LedgerValue.assetClassValue txID_Master_SplitFund_AC 1
    ---------------------
        !value_In_PoolDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_PoolDatum
        !value_For_PoolDatum = value_In_PoolDatum
    ---------------------
        !value_In_fundDatum_To_Split = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_FundDatumToSplit
    ---------------------
    !value_SplitFundAmount <- OffChainHelpers.createValueAddingTokensOfCurrencySymbol harvest_AC harvest_CS haverstIsWithoutTokenName value_In_fundDatum_To_Split splitFundAmount
    ---------------------
    let
        !value_For_FundDatum_New' =  value_SplitFundAmount <> value_For_Mint_FundID <> value_For_Mint_TxID_Master_SplitFund
        -- !value_Dummy_New = LedgerValue.assetClassValue T.dummy1_AC 1 <>  LedgerValue.assetClassValue T.dummy2_AC 1 <> LedgerValue.assetClassValue T.dummy3_AC 1 <> LedgerValue.assetClassValue T.dummy4_AC 1
        -- !minAda_For_FundDatum_New =  Helpers.calculateMinAda (value_For_FundDatum_New' <> value_Dummy_New) True
        !minAda_For_FundDatum_New_Normal = Helpers.calculateMinAdaOfValue value_For_FundDatum_New' True
        !minAda_For_FundDatum_New_ExtraTokens = Helpers.calculateMinAda T.maxDiffTokensForPoolAndFundDatum (T.tokenNameLenght*T.maxDiffTokensForPoolAndFundDatum) T.maxDiffTokensForPoolAndFundDatum True
        !minAda_For_FundDatum_New = minAda_For_FundDatum_New_Normal + minAda_For_FundDatum_New_ExtraTokens
        !value_MinAda_For_FundDatum_New = LedgerAda.lovelaceValueOf minAda_For_FundDatum_New
        !value_For_FundDatum_New = value_For_FundDatum_New' <> value_MinAda_For_FundDatum_New
    ---------------------
        !value_For_FundDatum_Split =  value_In_fundDatum_To_Split <> negate value_SplitFundAmount
    ---------------------
        !poolDatum_In = Helpers.getPoolDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_PoolDatum
        !poolDatum_Out = T.PoolDatum $ Helpers.mkUpdated_PoolDatum_With_SplitFundAmount poolDatum_In master minAda_For_FundDatum_New
    ---------------------
        !fundDatum_Split = T.FundDatum $ Helpers.mkUpdated_FundDatum_With_WithSplitFund fundDatum_To_Split splitFundAmount
    ---------------------
        !cashedOut = 0
        !fundDatum_New = T.mkFundDatum splitFundAmount cashedOut minAda_For_FundDatum_New
    ---------------------
        !redeemer_For_Consuming_Validator_Datum = T.mkRedeemerMasterSplitFund master splitFundAmount minAda_For_FundDatum_New
        !redeemer_For_Mint_TxID_Master_Fund = T.mkRedeemerMint_TxID redeemer_For_Consuming_Validator_Datum
        !redeemer_For_Mint_TxID_Master_SplitFund = T.mkRedeemerMint_TxID redeemer_For_Consuming_Validator_Datum
    ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Split Fund : Ending ---------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In: %s" (P.show poolDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In Value: %s" (P.show value_In_PoolDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out: %s" (P.show poolDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out Value: %s" (P.show value_For_PoolDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum To Split: %s" (P.show fundDatum_To_Split)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum To Split Value: %s" (P.show value_In_fundDatum_To_Split)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum Split: %s" (P.show fundDatum_Split)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum Split Value: %s" (P.show value_For_FundDatum_Split)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum New: %s" (P.show fundDatum_New)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum New Value: %s" (P.show value_For_FundDatum_New)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeemer For Consuming Validator Datum: %s" (P.show redeemer_For_Consuming_Validator_Datum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    let
        (lookupsTx_Mint_FundID, tx_Mint_FundID) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMaster uTxO_With_Script_Master_Fund (Just redeemer_For_Mint_TxID_Master_Fund) value_For_Mint_FundID validityRange masterPPKH
        (lookupsTx_Mint_TxID_Master_SplitFund, tx_Mint_TxID_Master_SplitFund) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMaster uTxO_With_Script_Master_SplitFund (Just redeemer_For_Mint_TxID_Master_SplitFund) value_For_Mint_TxID_Master_SplitFund validityRange masterPPKH

        lookupsTx =
            lookupsTx_Mint_FundID P.<>
            lookupsTx_Mint_TxID_Master_SplitFund P.<>
            LedgerConstraints.unspentOutputs uTxOsAtMaster P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_ScriptDatum])  P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum] )   P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_FundDatumToSplit] )
        tx =
            tx_Mint_FundID P.<>
            tx_Mint_TxID_Master_SplitFund P.<>
            LedgerConstraints.mustSpendScriptOutputWithReference (fst uTxO_With_PoolDatum) (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum)  (fst uTxO_With_ScriptDatum) P.<>
            LedgerConstraints.mustSpendScriptOutputWithReference (fst uTxO_With_FundDatumToSplit) (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum)  (fst uTxO_With_ScriptDatum) P.<>
            LedgerConstraints.mustPayToOtherScriptWithInlineDatum validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData poolDatum_Out) value_For_PoolDatum P.<>
            LedgerConstraints.mustPayToOtherScriptWithDatumInTx validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundDatum_Split) value_For_FundDatum_Split P.<>
            LedgerConstraints.mustPayToOtherScriptWithDatumInTx validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundDatum_New) value_For_FundDatum_New P.<>
            LedgerConstraints.mustValidateIn validityRange P.<>
            LedgerConstraints.mustBeSignedBy masterPPKH
    ---------------------
    submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void  lookupsTx tx
    txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Split Fund (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)

--------------------------------------------------------------------------------------------

masterClosePool :: T.PABMasterClosePoolParams -> PlutusContract.Contract w s DataText.Text ()
masterClosePool T.PABMasterClosePoolParams{..} = do
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Close Pool : Init ----------------------------------------"
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
        !pabParams = pmcPABPoolParams
        !pParams = T.pppPoolParams pabParams
        !validatorHash = T.pppValidatorHash pabParams
        !validatorAddress = T.pppValidatorAddress pabParams
    ---------------------
        -- !policy_TxID_Master_ClosePool = T.pppPolicy_TxID_Master_ClosePool pabParams
    ---------------------
        !poolID_CS = T.ppPoolID_CS pParams
        !poolID_AC = LedgerValue.AssetClass (poolID_CS, T.poolID_TN)
    ---------------------
        !scriptID_CS =  T.pppCurSymbol_TxID_Master_AddScripts pabParams
        !scriptID_AC = LedgerValue.AssetClass (scriptID_CS, T.scriptID_TN)
    ---------------------
        !scriptID_Validator_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Validator_TN)
        !scriptID_Master_ClosePool_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_ClosePool_TN)
    ---------------------
        !txID_Master_ClosePool_CS =  T.pppCurSymbol_TxID_Master_ClosePool pabParams
        !txID_Master_ClosePool_AC = LedgerValue.AssetClass (txID_Master_ClosePool_CS, T.txID_Master_ClosePool_TN)
    ---------------------
        closedAt = now
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
    !uTxO_With_Script_Master_ClosePool' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_ClosePool_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_ClosePool <-
        case uTxO_With_Script_Master_ClosePool' of
            Nothing ->
                PlutusContract.throwError "Can't find any uTxO with 'Master Close Pool' Minting Script"
            Just uTxO_With_Script_Master_ClosePool -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with 'Master Close Pool' Minting Script: %s" (P.show $ fst uTxO_With_Script_Master_ClosePool)
                return uTxO_With_Script_Master_ClosePool
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
        !value_For_Mint_TxID_Master_ClosePool = LedgerValue.assetClassValue txID_Master_ClosePool_AC 1
    ---------------------
        !value_In_PoolDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_PoolDatum
        !value_For_PoolDatum = value_In_PoolDatum <> value_For_Mint_TxID_Master_ClosePool
    ---------------------
        !poolDatum_In = Helpers.getPoolDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_PoolDatum
        !poolDatum_Out = T.PoolDatum $ Helpers.mkUpdated_PoolDatum_With_ClosedAt poolDatum_In closedAt
    ---------------------
        !redeemer_For_Consuming_Validator_Datum = T.mkRedeemerMasterClosePool master closedAt
        !redeemer_For_Mint_TxID_Master_ClosePool = T.mkRedeemerMint_TxID redeemer_For_Consuming_Validator_Datum
    ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Close Pool : Ending ---------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In: %s" (P.show poolDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In Value: %s" (P.show value_In_PoolDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out: %s" (P.show poolDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out Value: %s" (P.show value_For_PoolDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeemer For Consuming Validator Datum: %s" (P.show redeemer_For_Consuming_Validator_Datum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    let
        (lookupsTx_Mint_TxID_Master_ClosePool, tx_Mint_TxID_Master_ClosePool) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMaster uTxO_With_Script_Master_ClosePool (Just redeemer_For_Mint_TxID_Master_ClosePool) value_For_Mint_TxID_Master_ClosePool validityRange masterPPKH
        lookupsTx =
            lookupsTx_Mint_TxID_Master_ClosePool P.<>
            LedgerConstraints.unspentOutputs uTxOsAtMaster P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_ScriptDatum]) P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum] )

        tx =
            tx_Mint_TxID_Master_ClosePool P.<>
            LedgerConstraints.mustSpendScriptOutputWithReference (fst uTxO_With_PoolDatum) (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum)  (fst uTxO_With_ScriptDatum) P.<>
            LedgerConstraints.mustPayToOtherScriptWithInlineDatum validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData poolDatum_Out) value_For_PoolDatum P.<>
            LedgerConstraints.mustValidateIn validityRange P.<>
            LedgerConstraints.mustBeSignedBy masterPPKH
    ---------------------
    submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void  lookupsTx tx
    txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Close Pool (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)

--------------------------------------------------------------------------------------------

masterTerminatePool :: T.PABMasterTerminatePoolParams -> PlutusContract.Contract w s DataText.Text ()
masterTerminatePool T.PABMasterTerminatePoolParams{..} = do
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Terminate Pool : Init ----------------------------------------"
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
        !pabParams = pmtPABPoolParams
        !pParams = T.pppPoolParams pabParams
        !validatorHash = T.pppValidatorHash pabParams
        !validatorAddress = T.pppValidatorAddress pabParams
    ---------------------
        -- !policy_TxID_Master_TerminatePool = T.pppPolicy_TxID_Master_TerminatePool pabParams
    ---------------------
        !poolID_CS = T.ppPoolID_CS pParams
        !poolID_AC = LedgerValue.AssetClass (poolID_CS, T.poolID_TN)
    ---------------------
        !scriptID_CS =  T.pppCurSymbol_TxID_Master_AddScripts pabParams
        !scriptID_AC = LedgerValue.AssetClass (scriptID_CS, T.scriptID_TN)
    ---------------------
        !scriptID_Validator_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Validator_TN)
        !scriptID_Master_TerminatePool_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_TerminatePool_TN )
    ---------------------
        !txID_Master_TerminatePool_CS =  T.pppCurSymbol_TxID_Master_TerminatePool pabParams
        !txID_Master_TerminatePool_AC = LedgerValue.AssetClass (txID_Master_TerminatePool_CS, T.txID_Master_TerminatePool_TN)
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
    !uTxO_With_Script_Master_TerminatePool' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_TerminatePool_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_TerminatePool <-
        case uTxO_With_Script_Master_TerminatePool' of
            Nothing ->
                PlutusContract.throwError "Can't find any uTxO with 'Master Terminate Pool' Minting Script"
            Just uTxO_With_Script_Master_TerminatePool -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with 'Master Terminate Pool' Minting Script: %s" (P.show $ fst uTxO_With_Script_Master_TerminatePool)
                return uTxO_With_Script_Master_TerminatePool
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
    ---------------------
        !value_For_Mint_TxID_Master_TerminatePool = LedgerValue.assetClassValue txID_Master_TerminatePool_AC 1
    ---------------------
        !value_In_PoolDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_PoolDatum
        !value_For_PoolDatum = value_In_PoolDatum <> value_For_Mint_TxID_Master_TerminatePool
    ---------------------
        !poolDatum_In = Helpers.getPoolDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_PoolDatum
        !poolDatum_Out = T.PoolDatum $ Helpers.mkUpdated_PoolDatum_With_Terminated poolDatum_In
    ---------------------
        !redeemer_For_Consuming_Validator_Datum = T.mkRedeemerMasterTerminatePool master
        !redeemer_For_Mint_TxID_Master_TerminatePool = T.mkRedeemerMint_TxID redeemer_For_Consuming_Validator_Datum
    ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Terminate Pool : Ending ---------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In: %s" (P.show poolDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In Value: %s" (P.show value_In_PoolDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out: %s" (P.show poolDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out Value: %s" (P.show value_For_PoolDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeemer For Consuming Validator Datum: %s" (P.show redeemer_For_Consuming_Validator_Datum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    let
        (lookupsTx_Mint_TxID_Master_TerminatePool, tx_Mint_TxID_Master_TerminatePool) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMaster uTxO_With_Script_Master_TerminatePool (Just redeemer_For_Mint_TxID_Master_TerminatePool) value_For_Mint_TxID_Master_TerminatePool validityRange masterPPKH
        lookupsTx =
            lookupsTx_Mint_TxID_Master_TerminatePool P.<>
            LedgerConstraints.unspentOutputs uTxOsAtMaster P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_ScriptDatum]) P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum] )
        tx =
            tx_Mint_TxID_Master_TerminatePool P.<>
            LedgerConstraints.mustSpendScriptOutputWithReference (fst uTxO_With_PoolDatum) (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum)  (fst uTxO_With_ScriptDatum) P.<>
            LedgerConstraints.mustPayToOtherScriptWithInlineDatum validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData poolDatum_Out) value_For_PoolDatum P.<>
            LedgerConstraints.mustValidateIn validityRange P.<>
            LedgerConstraints.mustBeSignedBy masterPPKH
    ---------------------
    submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void  lookupsTx tx
    txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Terminate Pool (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)

--------------------------------------------------------------------------------------------

masterEmergency :: T.PABMasterEmergencyParams -> PlutusContract.Contract w s DataText.Text ()
masterEmergency T.PABMasterEmergencyParams{..} = do
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Emergency : Init ----------------------------------------"
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
        !pabParams = pmePABPoolParams
        !pParams = T.pppPoolParams pabParams
        !validatorHash = T.pppValidatorHash pabParams
        !validatorAddress = T.pppValidatorAddress pabParams
    ---------------------
        -- !policy_TxID_Master_Emergency = T.pppPolicy_TxID_Master_Emergency pabParams
    ---------------------
        !poolID_CS = T.ppPoolID_CS pParams
        !poolID_AC = LedgerValue.AssetClass (poolID_CS, T.poolID_TN)
    ---------------------
        !scriptID_CS =  T.pppCurSymbol_TxID_Master_AddScripts pabParams
        !scriptID_AC = LedgerValue.AssetClass (scriptID_CS, T.scriptID_TN)
    ---------------------
        !scriptID_Validator_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Validator_TN)
        !scriptID_Master_Emergency_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Master_Emergency_TN )
    ---------------------
        !txID_Master_Emergency_CS =  T.pppCurSymbol_TxID_Master_Emergency pabParams
        !txID_Master_Emergency_AC = LedgerValue.AssetClass (txID_Master_Emergency_CS, T.txID_Master_Emergency_TN)
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
    !uTxO_With_Script_Master_Emergency' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_Master_Emergency_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_Master_Emergency <-
        case uTxO_With_Script_Master_Emergency' of
            Nothing ->
                PlutusContract.throwError "Can't find any uTxO with 'Master Emergency' Minting Script"
            Just uTxO_With_Script_Master_Emergency -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with 'Master Emergency' Minting Script: %s" (P.show $ fst uTxO_With_Script_Master_Emergency)
                return uTxO_With_Script_Master_Emergency
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
    ---------------------
        !value_For_Mint_TxID_Master_Emergency = LedgerValue.assetClassValue txID_Master_Emergency_AC 1
    ---------------------
        !value_In_PoolDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_PoolDatum
        !value_For_PoolDatum = value_In_PoolDatum <> value_For_Mint_TxID_Master_Emergency
    ---------------------
        !poolDatum_In = Helpers.getPoolDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_PoolDatum
        !isEmergency = negate $ T.pdIsEmergency poolDatum_In
        !poolDatum_Out = T.PoolDatum $ Helpers.mkUpdated_PoolDatum_With_Emergency poolDatum_In isEmergency
    ---------------------
        !redeemer_For_Consuming_Validator_Datum = T.mkRedeemerMasterEmergency master 
        !redeemer_For_Mint_TxID_Master_Emergency = T.mkRedeemerMint_TxID redeemer_For_Consuming_Validator_Datum
    ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Emergency : Ending ---------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In: %s" (P.show poolDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In Value: %s" (P.show value_In_PoolDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out: %s" (P.show poolDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum Out Value: %s" (P.show value_For_PoolDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeemer For Consuming Validator Datum: %s" (P.show redeemer_For_Consuming_Validator_Datum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    let
        (lookupsTx_Mint_TxID_Master_Emergency, tx_Mint_TxID_Master_Emergency) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtMaster uTxO_With_Script_Master_Emergency (Just redeemer_For_Mint_TxID_Master_Emergency) value_For_Mint_TxID_Master_Emergency validityRange masterPPKH
        lookupsTx =
            lookupsTx_Mint_TxID_Master_Emergency P.<>
            LedgerConstraints.unspentOutputs uTxOsAtMaster P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_ScriptDatum]) P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum] )
        tx =
            tx_Mint_TxID_Master_Emergency P.<>
            LedgerConstraints.mustSpendScriptOutputWithReference (fst uTxO_With_PoolDatum) (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum)  (fst uTxO_With_ScriptDatum) P.<>
            LedgerConstraints.mustPayToOtherScriptWithInlineDatum validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData poolDatum_Out) value_For_PoolDatum P.<>
            LedgerConstraints.mustValidateIn validityRange P.<>
            LedgerConstraints.mustBeSignedBy masterPPKH
    ---------------------
    submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void  lookupsTx tx
    txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Master Emergency (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)

--------------------------------------------------------------------------------------------
