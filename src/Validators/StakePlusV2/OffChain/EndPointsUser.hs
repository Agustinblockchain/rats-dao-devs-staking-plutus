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
module Validators.StakePlusV2.OffChain.EndPointsUser where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Data.Map                                                   as DataMap
import qualified Data.Text                                                  as DataText ( Text)
import qualified Data.Void                                                  as DataVoid (Void)
import qualified Ledger                                                     (getCardanoTxId, pubKeyHashAddress, unPaymentPubKeyHash, PaymentPubKeyHash(..))
import qualified Ledger.Ada                                                 as LedgerAda
import qualified Ledger.Value                                               as LedgerValue
import qualified Ledger.Constraints                                         as LedgerConstraints
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
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers
import qualified Validators.StakePlusV2.OffChain.OffChainHelpers            as OffChainHelpers
import qualified Validators.StakePlusV2.Types.Constants                     as T 
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T 
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T 
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T 
import qualified Validators.StakePlusV2.Types.PABParams                     as T 
import qualified Validators.StakePlusV2.Types.Types                         as T
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------

userDeposit :: T.PABUserDepositParams -> PlutusContract.Contract w s DataText.Text ()
userDeposit T.PABUserDepositParams{..} = do
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User Deposit : Init ----------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    (now,_) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    ---------------------
    userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let
        !user = Ledger.unPaymentPubKeyHash userPPKH
        !userAdds = Ledger.pubKeyHashAddress userPPKH Nothing
    uTxOsAtUser <- PlutusContract.utxosAt userAdds
    ---------------------
    let
        !pabParams = puiPABPoolParams
        !pParams = T.pppPoolParams pabParams
        !validatorHash = T.pppValidatorHash pabParams
        !validatorAddress = T.pppValidatorAddress pabParams
    ---------------------
        -- !policy_TxID_User_Deposit = T.pppPolicy_TxID_User_Deposit pabParams
    ---------------------
        !staking_CS = T.ppStaking_CS pParams
        !staking_AC = LedgerValue.AssetClass (staking_CS, T.ppStaking_TN pParams)
        !stakingIsAda = LedgerApiV2.adaSymbol == staking_CS
        !stakingIsWithoutTokenName = not stakingIsAda && T.ppStaking_TN pParams == LedgerApiV2.TokenName emptyByteString
    ---------------------
        !poolID_CS = T.ppPoolID_CS pParams
        !poolID_AC = LedgerValue.AssetClass (poolID_CS, T.poolID_TN)
    ---------------------
        !userID_CS = T.pppCurSymbol_TxID_User_Deposit pabParams
        !userID_AC = LedgerValue.AssetClass (userID_CS, T.userID_TN)
    ---------------------
        !scriptID_CS =  T.pppCurSymbol_TxID_Master_AddScripts pabParams
        !scriptID_AC = LedgerValue.AssetClass (scriptID_CS, T.scriptID_TN)
    ---------------------
        !scriptID_Validator_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Validator_TN)
        !scriptID_User_Deposit_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_User_Deposit_TN)
    ---------------------
        !userDeposit_CS =  userID_CS
        !userDeposit_AC = LedgerValue.AssetClass (userDeposit_CS, T.userDeposit_TN)
    ---------------------
        !value_In_User_Wallet = foldl (<>) (LedgerAda.lovelaceValueOf 0) (OffChainHelpers.getValueFromDecoratedTxOut . snd  <$> DataMap.toList uTxOsAtUser)
    ---------------------
        !investAmount = puiInvestAmount
    ---------------------
    !value_InvestAmount <- OffChainHelpers.createValueAddingTokensOfCurrencySymbol staking_AC staking_CS stakingIsWithoutTokenName value_In_User_Wallet investAmount
    PlutusContract.logInfo @P.String $ TextPrintf.printf "staking_AC: %s" (P.show staking_AC)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "staking_CS: %s" (P.show staking_CS)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "stakingIsWithoutTokenName: %s" (P.show stakingIsWithoutTokenName)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "value_In_User_Wallet: %s" (P.show value_In_User_Wallet)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "value_InvestAmount: %s" (P.show value_InvestAmount)
    ---------------------
    let
        !createdAt = now
    ---------------------
    !checkCollateral <- OffChainHelpers.checkIfThereIsUTxOFreeForCollateral uTxOsAtUser
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
    !uTxO_With_Script_User_Deposit' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_User_Deposit_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_User_Deposit <-
        case uTxO_With_Script_User_Deposit' of
            Nothing -> do
                PlutusContract.throwError "Can't find any uTxO with 'User Deposit' Minting Script"
            Just uTxO_With_Script_User_Deposit -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with 'User Deposit' Minting Script: %s" (P.show $ fst uTxO_With_Script_User_Deposit)
                return uTxO_With_Script_User_Deposit
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
        !poolDatum_In = Helpers.getPoolDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_PoolDatum
    ---------------------
        !value_For_Mint_UserDeposit = LedgerValue.assetClassValue userDeposit_AC investAmount
    ---------------------
        !value_For_Mint_UserID = LedgerValue.assetClassValue userID_AC 1
    ---------------------
        !value_For_UserDatum' = value_InvestAmount <> value_For_Mint_UserID
        !minAda_For_UserDatum_Normal = Helpers.calculateMinAdaOfValue value_For_UserDatum' True
        !minAda_For_UserDatum_ExtraTokens = Helpers.calculateMinAda T.maxDiffTokensForUserDatum (T.tokenNameLenght*T.maxDiffTokensForUserDatum) T.maxDiffTokensForUserDatum True
        !minAda_For_UserDatum = minAda_For_UserDatum_Normal + minAda_For_UserDatum_ExtraTokens
        !value_MinAda_For_UserDatum = LedgerAda.lovelaceValueOf minAda_For_UserDatum
        !value_For_UserDatum = value_For_UserDatum' <> value_MinAda_For_UserDatum
    ---------------------
        !userDatum_Out = T.mkUserDatum user investAmount createdAt 0 0 Nothing minAda_For_UserDatum
    ---------------------
        !redeemer_For_Consuming_Validator_Datum = T.mkRedeemerUserDeposit user investAmount createdAt minAda_For_UserDatum
        !redeemer_For_Mint_UserDeposit = T.mkRedeemerMint_TxID redeemer_For_Consuming_Validator_Datum
    ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User Deposit : Ending ---------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In: %s" (P.show poolDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Invest Amount Value: %s" (P.show value_InvestAmount)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "UserDatum Out: %s" (P.show userDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "UserDatum Out Value: %s" (P.show value_For_UserDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeemer For Consuming Validator Datum: %s" (P.show redeemer_For_Consuming_Validator_Datum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    let
        (lookupsTx_MintUserID, txMintUserID) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtUser uTxO_With_Script_User_Deposit (Just redeemer_For_Mint_UserDeposit) value_For_Mint_UserID validityRange userPPKH
        (lookupsTx_Mint_TxID_User_Deposit, tx_Mint_TxID_User_Deposit) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtUser uTxO_With_Script_User_Deposit (Just redeemer_For_Mint_UserDeposit) value_For_Mint_UserDeposit validityRange userPPKH
        lookupsTx =
            lookupsTx_MintUserID P.<>
            lookupsTx_Mint_TxID_User_Deposit P.<>
            LedgerConstraints.unspentOutputs uTxOsAtUser P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_ScriptDatum]) P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum])

        tx =
            txMintUserID P.<>
            tx_Mint_TxID_User_Deposit P.<>
            LedgerConstraints.mustReferenceOutput (fst uTxO_With_PoolDatum) P.<>
            LedgerConstraints.mustPayToOtherScriptWithDatumInTx validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData userDatum_Out) value_For_UserDatum P.<>
            LedgerConstraints.mustValidateIn validityRange P.<>
            LedgerConstraints.mustBeSignedBy userPPKH
    ---------------------
    submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
    txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus User Deposit (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)

--------------------------------------------------------------------------------------------

userHarvest :: T.PABUserHarvestParams -> PlutusContract.Contract w s DataText.Text ()
userHarvest T.PABUserHarvestParams{..} = do
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User Harvest : Init ----------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    (now,_) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    ---------------------
    userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let
        !user = Ledger.unPaymentPubKeyHash userPPKH
        !userAdds = Ledger.pubKeyHashAddress userPPKH Nothing
    uTxOsAtUser <- PlutusContract.utxosAt userAdds
    ---------------------
    let
        !pabParams = pugrPABPoolParams
        !pParams = T.pppPoolParams pabParams
        !validatorHash = T.pppValidatorHash pabParams
        !validatorAddress = T.pppValidatorAddress pabParams
    ---------------------
        -- !policy_TxID_User_Harvest = T.pppPolicy_TxID_User_Harvest pabParams
    ---------------------
        !harvest_CS =  T.ppHarvest_CS pParams
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
        !userID_CS = T.pppCurSymbol_TxID_User_Deposit pabParams
        !userID_AC = LedgerValue.AssetClass (userID_CS, T.userID_TN)
    ---------------------
        !scriptID_CS =  T.pppCurSymbol_TxID_Master_AddScripts pabParams
        !scriptID_AC = LedgerValue.AssetClass (scriptID_CS, T.scriptID_TN)
    ---------------------
        !scriptID_Validator_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_Validator_TN)
        !scriptID_User_Harvest_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_User_Harvest_TN)
    ---------------------
        !txID_User_Harvest_CS = T.pppCurSymbol_TxID_User_Harvest pabParams
        !txID_User_Harvest_AC = LedgerValue.AssetClass (txID_User_Harvest_CS, T.txID_User_Harvest_TN)
    ---------------------
        !claimAmount = pugrClaimAmount
    ---------------------
        !claimAt = now
    ---------------------
        !uTxOToClaim = pugrUserID_TxOutRef
    ---------------------
    !checkCollateral <- OffChainHelpers.checkIfThereIsUTxOFreeForCollateral uTxOsAtUser
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
    !uTxO_With_Script_User_Harvest' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_User_Harvest_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_User_Harvest <-
        case uTxO_With_Script_User_Harvest' of
            Nothing -> do
                PlutusContract.throwError "Can't find any uTxO with 'User Harvest' Minting Script"
            Just uTxO_With_Script_User_Harvest -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with 'User Harvest' Minting Script: %s" (P.show $ fst uTxO_With_Script_User_Harvest)
                return uTxO_With_Script_User_Harvest
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
    uTxOs_With_FundDatums' <- OffChainHelpers.getUTxOs_With_FundDatum fundID_AC uTxOsAtValidator
    ---------------------
    !uTxOs_With_FundDatums <-
        case uTxOs_With_FundDatums' of
            [] -> do
                PlutusContract.throwError "Can't find any uTxO with FundDatum"
            x -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxOs List with FundDatum: %s" (P.show $ fst <$> x )
                return x
    ---------------------
    !uTxOs_With_UserDatums' <- OffChainHelpers.getUTxOs_With_UserDatum userID_AC uTxOsAtValidator
    ---------------------
    !uTxOs_With_UserDatums <-
        case uTxOs_With_UserDatums' of
            [] -> do
                PlutusContract.throwError "Can't find any uTxO with UserDatum"
            x -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxOs List with UserDatum: %s" (P.show $ fst <$> x )
                return x
    ---------------------
    let !uTxO_With_UserDatum' = find (\(txOutRef, _) -> txOutRef == uTxOToClaim) uTxOs_With_UserDatums
    !uTxO_With_UserDatum <-
        case uTxO_With_UserDatum' of
            Nothing -> do
                PlutusContract.throwError "Can't find any uTxO with UserDatum at the chossen uTxO to Claim"
            Just x -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with UserDatum to Claim: %s" (P.show $ fst x  )
                return x
    ---------------------
    let
        maxValueToClaim = OffChainHelpers.getMaxToClaimInUxtoList uTxOs_With_FundDatums
    PlutusContract.logInfo @P.String $ TextPrintf.printf "maxValueToClaim: %s" (P.show maxValueToClaim)
    ---------------------
    if claimAmount > maxValueToClaim then do
        PlutusContract.throwError "There is not enough funds in the uTxOs with FundDatum to cover the claim"
    else pure ()
    ---------------------
    let
        !poolDatum_In = Helpers.getPoolDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_PoolDatum
    ---------------------
        !userDatum_In = Helpers.getUserDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_UserDatum
    ---------------------
        !pdClosedAt = T.pdClosedAt poolDatum_In
    ---------------------
        !rewards = Helpers.getRewardsPerInvest (T.ppDeadline pParams) pdClosedAt  (T.ppInterestRates pParams) (T.udLastClaimAt userDatum_In) claimAt (T.udCreatedAt userDatum_In) (T.udInvest userDatum_In) (T.udRewardsNotClaimed userDatum_In)
        !totalNewRewards = rewards  + T.udRewardsNotClaimed userDatum_In
        !rewardsNotClaimed = totalNewRewards - claimAmount
        !totalRewardsCashedOut = T.udCashedOut userDatum_In + claimAmount
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Claiming: " ++ P.show claimAmount
    PlutusContract.logInfo @P.String $ TextPrintf.printf "New Rewards: " ++ P.show rewards
    PlutusContract.logInfo @P.String $ TextPrintf.printf "RewardsNotClaimed: " ++ P.show (T.udRewardsNotClaimed userDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "TotalNewRewards: " ++ P.show totalNewRewards
    PlutusContract.logInfo @P.String $ TextPrintf.printf "RewardsNotClaimed after claim: " ++ P.show rewardsNotClaimed
    PlutusContract.logInfo @P.String $ TextPrintf.printf "TotalRewardsCashedOut: " ++ P.show totalRewardsCashedOut
    ---------------------
    let
        !userDatum_Out = T.mkUserDatum user
            (T.udInvest userDatum_In)
            (T.udCreatedAt userDatum_In)
            totalRewardsCashedOut
            rewardsNotClaimed
            (Just now)
            (T.udMinAda userDatum_In)
    ---------------------
        !value_For_Mint_TxID_User_Harvest = LedgerValue.assetClassValue txID_User_Harvest_AC 1
    ---------------------
        !value_In_UserDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_UserDatum
        !value_For_UserDatum = value_In_UserDatum <> value_For_Mint_TxID_User_Harvest
    ---------------------
    if  claimAmount > totalNewRewards then do
        PlutusContract.throwError "Trying to get too many rewards... wait some time"
    else pure ()
    ---------------------
    let
        !uTxOs_With_FundDatums_Ordered = sortBy OffChainHelpers.sort_Value_And_FundDatum uTxOs_With_FundDatums
    ---------------------
        !uTxOs_With_FundDatums_WithEnoughValueToClaim = OffChainHelpers.getUTxOListWithEnoughValueToClaim uTxOs_With_FundDatums_Ordered claimAmount
    PlutusContract.logInfo @P.String $ TextPrintf.printf "uTxOs_With_FundDatums_WithEnoughValueToClaim List: %s" (P.show $ fst <$> uTxOs_With_FundDatums_WithEnoughValueToClaim)
    ---------------------
    -- for all the listo of chossen uTxO I need to create a list of FundDatum to sit with each one of them, with the cashedOut Value updated.
    fundDatumListWithNewValues <- OffChainHelpers.getFundDatumListWithNewValues harvest_AC harvest_CS haverstIsWithoutTokenName uTxOs_With_FundDatums_WithEnoughValueToClaim claimAmount
    ---------------------
    let
        !value_FundDatums_Ins = foldl (<>) (LedgerAda.lovelaceValueOf 0) (OffChainHelpers.getValueFromDecoratedTxOut . snd  <$> uTxOs_With_FundDatums_WithEnoughValueToClaim)
        !value_FundDatums_Outs = foldl (<>) (LedgerAda.lovelaceValueOf 0) [value | (_, value) <- fundDatumListWithNewValues]
    ---------------------
        !value_ClaimAmount = value_FundDatums_Ins <> negate value_FundDatums_Outs
        !value_For_User = value_ClaimAmount
    ---------------------
        !redeemer_For_Consuming_Validator_Datum = T.mkRedeemerUserHarvest user claimAmount claimAt
    ---------------------
        !redeemer_For_Mint_TxID_User_Harvest = T.mkRedeemerMint_TxID redeemer_For_Consuming_Validator_Datum
    ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
    ----------------------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User Harvest : Ending ---------------------------------------"
    let
        formatFundDatumOutputs = concat [  ["----", "FundDatum: " ++ P.show  fundDatum, "FundDatum Value: " ++ P.show value_For_FundDatum] | (fundDatum, value_For_FundDatum) <- fundDatumListWithNewValues]
    mapM_ (PlutusContract.logInfo @P.String  ) formatFundDatumOutputs
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "UserDatum In: %s" (P.show userDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "UserDatum In Value: %s" (P.show value_In_UserDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Value For User: %s" (P.show value_For_User)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "UserDatum Out: %s" (P.show userDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "UserDatum Out Value: %s" (P.show value_For_UserDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeemer For Consuming Validator Datum: %s" (P.show redeemer_For_Consuming_Validator_Datum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ----------------------------------
    let

        inputs_TxOut_Values_And_Datums :: [T.TxOut_Value_And_Datum]
        inputs_TxOut_Values_And_Datums =
            [(OffChainHelpers.getValueFromDecoratedTxOut ci , Helpers.fromJust $ OffChainHelpers.getDatumFromDecoratedTxOut ci) | (_, ci) <- [uTxO_With_PoolDatum]]
            ++
            [(OffChainHelpers.getValueFromDecoratedTxOut ci , Helpers.fromJust $ OffChainHelpers.getDatumFromDecoratedTxOut ci) | (_, ci) <- uTxOs_With_FundDatums_WithEnoughValueToClaim]
            ++
            [(OffChainHelpers.getValueFromDecoratedTxOut ci , Helpers.fromJust $ OffChainHelpers.getDatumFromDecoratedTxOut ci) | (_, ci) <- [uTxO_With_UserDatum]]

        outputs_TxOut_Values_And_Datums :: [T.TxOut_Value_And_Datum]
        outputs_TxOut_Values_And_Datums =
            [(value , T.FundDatum fd) | (fd, value) <- fundDatumListWithNewValues]
            ++
            [(value_For_UserDatum, userDatum_Out)]

        ---------------------------------
        !input_TxOut_Value_And_UserDatum =
            case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum userID_AC Helpers.getUserDatumTypo_FromDatum inputs_TxOut_Values_And_Datums of
                Nothing -> traceError "IUD"
                Just x  -> x
        ------------------
        !output_TxOut_Value_And_UserDatum =
            case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum userID_AC Helpers.getUserDatumTypo_FromDatum outputs_TxOut_Values_And_Datums of
                Nothing -> traceError "OUD" 
                Just x  -> x
        ---------------------------------

        inputs_TxOuts_Values_And_FundDatums :: [T.TxOut_Value_And_FundDatum]
        inputs_TxOuts_Values_And_FundDatums =
            [(OffChainHelpers.getValueFromDecoratedTxOut ci , Helpers.getFundDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut ci) | (_, ci) <- uTxOs_With_FundDatums_WithEnoughValueToClaim]
        outputs_TxOuts_Values_And_FundDatums :: [T.TxOut_Value_And_FundDatum]
        outputs_TxOuts_Values_And_FundDatums =
            [(value , fd) | (fd, value) <- fundDatumListWithNewValues]

        ---------------------------------

        !inputs_TxOuts_Values_And_FundDatums_Ordered =
            sortBy OnChainNFTHelpers.sort_Value_And_FundDatum inputs_TxOuts_Values_And_FundDatums
        !outputs_TxOuts_Values_And_FundDatums_Ordered =
            sortBy OnChainNFTHelpers.sort_Value_And_FundDatum outputs_TxOuts_Values_And_FundDatums
        !calculated_TxOuts_Values_And_FundDatums = OnChainNFTHelpers.getFundDatumListWithNewValues harvest_AC harvest_CS haverstIsWithoutTokenName inputs_TxOuts_Values_And_FundDatums_Ordered claimAmount
        !calculated_TxOuts_Values_And_FundDatums_Ordered =
            sortBy OnChainNFTHelpers.sort_Value_And_FundDatum calculated_TxOuts_Values_And_FundDatums

        correctInputs_FundDatumsQty :: Bool
        !correctInputs_FundDatumsQty =
            length calculated_TxOuts_Values_And_FundDatums == length inputs_TxOuts_Values_And_FundDatums

        correctOutputs_FundsDatums_And_Values_WithClaim :: Bool
        !correctOutputs_FundsDatums_And_Values_WithClaim =
            outputs_TxOuts_Values_And_FundDatums_Ordered == calculated_TxOuts_Values_And_FundDatums_Ordered

        correctOutput_UserDatum_Value_WithTokens :: Bool
        correctOutput_UserDatum_Value_WithTokens =
            let
                !value_In_UserDatum_ = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_UserDatum
                !value_For_Mint_TxID_User_Harvest_ = LedgerValue.assetClassValue txID_User_Harvest_AC 1
                !value_For_UserDatum_Control = value_In_UserDatum_ <> value_For_Mint_TxID_User_Harvest_
            ------------------
                !value_For_UserDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_UserDatum
            in
                value_For_UserDatum_Real ==  value_For_UserDatum_Control


        formatFundDatumOutputsList list = concat [  ["----", "FundDatum': " ++ P.show  fundDatum, "FundDatum Value: " ++ P.show value_For_FundDatum_] | (fundDatum, value_For_FundDatum_) <- list]

        -- formatUserDatumOutputsList list listWithValues = concat [  ["----", "UserDatum': " ++ P.show  userDatum, "UserDatum Value: " ++ P.show (listWithValues!!index)] | (index, userDatum) <- list]
        formatUserDatumOutputsList list = concat [  ["----", "UserDatum': " ++ P.show  userDatum, "UserDatum Value: " ++ P.show value_For_UserDatum_] | (userDatum, value_For_UserDatum_)  <- list]


    -- format_TxOut_Values_And_Datums list = concat [  ["----", "Datum: " ++ P.show  datum, "Value: " ++ P.show value] | (value, datum) <- list]
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "inputs_TxOut_Values_And_Datums"
    -- mapM_ (PlutusContract.logInfo @P.String  ) (format_TxOut_Values_And_Datums inputs_TxOut_Values_And_Datums)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "----"

    PlutusContract.logInfo @P.String $ TextPrintf.printf "input_TxOut_Value_And_UserDatum"
    mapM_ (PlutusContract.logInfo @P.String  ) (formatUserDatumOutputsList [input_TxOut_Value_And_UserDatum] )
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"

    PlutusContract.logInfo @P.String $ TextPrintf.printf "output_TxOut_Value_And_UserDatum"
    mapM_ (PlutusContract.logInfo @P.String  ) (formatUserDatumOutputsList [output_TxOut_Value_And_UserDatum] )
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"

    PlutusContract.logInfo @P.String $ TextPrintf.printf "correctOutput_UserDatum_Value_WithTokens: %s" (P.show correctOutput_UserDatum_Value_WithTokens)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"


    PlutusContract.logInfo @P.String $ TextPrintf.printf "inputs_TxOuts_Values_And_FundDatums"
    mapM_ (PlutusContract.logInfo @P.String  ) (formatFundDatumOutputsList inputs_TxOuts_Values_And_FundDatums)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "inputs_TxOuts_Values_And_FundDatums_Ordered"
    mapM_ (PlutusContract.logInfo @P.String  ) (formatFundDatumOutputsList inputs_TxOuts_Values_And_FundDatums_Ordered)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "outputs_TxOuts_Values_And_FundDatums"
    mapM_ (PlutusContract.logInfo @P.String  ) (formatFundDatumOutputsList outputs_TxOuts_Values_And_FundDatums)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "outputs_TxOuts_Values_And_FundDatums_Ordered"
    mapM_ (PlutusContract.logInfo @P.String  ) (formatFundDatumOutputsList outputs_TxOuts_Values_And_FundDatums_Ordered)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "calculated_TxOuts_Values_And_FundDatums"
    mapM_ (PlutusContract.logInfo @P.String  ) (formatFundDatumOutputsList calculated_TxOuts_Values_And_FundDatums)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "calculated_TxOuts_Values_And_FundDatums_Ordered"
    mapM_ (PlutusContract.logInfo @P.String  ) (formatFundDatumOutputsList calculated_TxOuts_Values_And_FundDatums_Ordered)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "correctInputs_FundDatumsQty: %s" (P.show correctInputs_FundDatumsQty)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "correctOutputs_FundsDatums_And_Values_WithClaim: %s" (P.show correctOutputs_FundsDatums_And_Values_WithClaim)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ----------------------------------
    let
        (lookupsTx_Mint_TxID_User_Harvest, tx_Mint_TxID_User_Harvest) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtUser uTxO_With_Script_User_Harvest (Just redeemer_For_Mint_TxID_User_Harvest) value_For_Mint_TxID_User_Harvest validityRange userPPKH

        lookupsTx =
            lookupsTx_Mint_TxID_User_Harvest P.<>
            LedgerConstraints.unspentOutputs uTxOsAtUser P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_ScriptDatum]) P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum]) P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList uTxOs_With_FundDatums_WithEnoughValueToClaim)  P.<>
            LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_UserDatum])
        tx =
            tx_Mint_TxID_User_Harvest P.<>
            LedgerConstraints.mustReferenceOutput (fst uTxO_With_PoolDatum) P.<>
            mconcat [LedgerConstraints.mustSpendScriptOutputWithReference txOutRef (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum) (fst uTxO_With_ScriptDatum) | txOutRef <- fst <$> uTxOs_With_FundDatums_WithEnoughValueToClaim] P.<>
            mconcat [LedgerConstraints.mustPayToOtherScriptWithDatumInTx validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData $ T.FundDatum fundDatum_Out) value_For_FundDatum | (fundDatum_Out, value_For_FundDatum) <- fundDatumListWithNewValues] P.<>
            LedgerConstraints.mustSpendScriptOutputWithReference (fst uTxO_With_UserDatum) (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum) (fst uTxO_With_ScriptDatum) P.<>
            LedgerConstraints.mustPayToOtherScriptWithDatumInTx validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData userDatum_Out) value_For_UserDatum P.<>
            -- LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash user) value_For_User  P.<> 
            LedgerConstraints.mustValidateIn validityRange P.<>
            LedgerConstraints.mustBeSignedBy userPPKH
    ---------------------
    submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
    txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus User Harvest (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)

--------------------------------------------------------------------------------------------

userWithdraw :: T.PABUserWithdrawParams -> PlutusContract.Contract w s DataText.Text ()
userWithdraw T.PABUserWithdrawParams{..} = do
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User Withdraw : Init ----------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    (now,_) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    ---------------------
    userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let
        !user = Ledger.unPaymentPubKeyHash userPPKH
        !userAdds = Ledger.pubKeyHashAddress userPPKH Nothing
    uTxOsAtUser <- PlutusContract.utxosAt userAdds
    ---------------------
    let
        !pabParams = pugbiPABPoolParams
        !pParams = T.pppPoolParams pabParams
        !validatorHash = T.pppValidatorHash pabParams
        !validatorAddress = T.pppValidatorAddress pabParams
    ---------------------
        -- !policy_TxID_User_Deposit = T.pppPolicy_TxID_User_Deposit pabParams
        -- !policy_TxID_User_Withdraw = T.pppPolicy_TxID_User_Withdraw pabParams
    ---------------------
        !staking_CS = T.ppStaking_CS pParams
        !staking_AC = LedgerValue.AssetClass (staking_CS, T.ppStaking_TN pParams)
        !stakingIsAda = LedgerApiV2.adaSymbol == staking_CS
        !stakingIsWithoutTokenName = not stakingIsAda && T.ppStaking_TN pParams == LedgerApiV2.TokenName emptyByteString
    ---------------------
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
        !scriptID_User_Deposit_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_User_Deposit_TN)
        !scriptID_User_Withdraw_AC =  LedgerValue.AssetClass (scriptID_CS, T.scriptID_User_Withdraw_TN)
    ---------------------
        !userDeposit_AC =  LedgerValue.AssetClass (userID_CS, T.userDeposit_TN)
    ---------------------
        !txID_User_Harvest_CS = T.pppCurSymbol_TxID_User_Harvest pabParams
        !txID_User_Harvest_AC = LedgerValue.AssetClass (txID_User_Harvest_CS, T.txID_User_Harvest_TN)
    ---------------------
        !txID_User_Withdraw_CS = T.pppCurSymbol_TxID_User_Withdraw pabParams
        !txID_User_Withdraw_AC = LedgerValue.AssetClass (txID_User_Withdraw_CS, T.txID_User_Withdraw_TN)
    ---------------------
        !uTxOToClaim = pugbiUserID_TxOutRef
    ---------------------
    !checkCollateral <- OffChainHelpers.checkIfThereIsUTxOFreeForCollateral uTxOsAtUser
    if checkCollateral then
        PlutusContract.logInfo @P.String $ TextPrintf.printf "There is UTxO free for Collateral"
    else
        PlutusContract.throwError "There is NOT UTxO free for Collateral"
    ---------------------
    !uTxOsAtValidator <- PlutusContract.utxosAt validatorAddress
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
    !uTxO_With_Script_User_Deposit' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_User_Deposit_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_User_Deposit <-
        case uTxO_With_Script_User_Deposit' of
            Nothing -> do
                PlutusContract.throwError "Can't find any uTxO with 'User Deposit' Minting Script"
            Just uTxO_With_Script_User_Deposit -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with 'User Deposit' Minting Script: %s" (P.show $ fst uTxO_With_Script_User_Deposit)
                return uTxO_With_Script_User_Deposit
    ---------------------
    !uTxO_With_Script_User_Withdraw' <- OffChainHelpers.getUTxO_With_ScriptDatum scriptID_AC scriptID_User_Withdraw_AC uTxOsAtValidator
    ---------------------
    !uTxO_With_Script_User_Withdraw <-
        case uTxO_With_Script_User_Withdraw' of
            Nothing -> do
                PlutusContract.throwError "Can't find any uTxO with 'User Withdraw' Minting Script"
            Just uTxO_With_Script_User_Withdraw -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with 'User Withdraw' Minting Script: %s" (P.show $ fst uTxO_With_Script_User_Withdraw)
                return uTxO_With_Script_User_Withdraw
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
    !uTxOs_With_UserDatums' <- OffChainHelpers.getUTxOs_With_UserDatum userID_AC uTxOsAtValidator
    ---------------------
    !uTxOs_With_UserDatums <-
        case uTxOs_With_UserDatums' of
            [] -> do
                PlutusContract.throwError "Can't find any uTxO with UserDatum"
            x -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxOs List with UserDatum: %s" (P.show $ fst <$> x )
                return x
    ---------------------
    let !uTxO_With_UserDatum' = find (\(txOutRef, _) -> txOutRef == uTxOToClaim) uTxOs_With_UserDatums
    !uTxO_With_UserDatum <-
        case uTxO_With_UserDatum' of
            Nothing -> do
                PlutusContract.throwError "Can't find any uTxO with UserDatum at the chossen uTxO to Withdraw"
            Just x -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxO with UserDatum to Withdraw: %s" (P.show $ fst x  )
                return x
    ---------------------
    let
        !poolDatum_In = Helpers.getPoolDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_PoolDatum
        !isFundCountZero = T.pdFundCount poolDatum_In == 0
    ---------------------
        !userDatum_In = Helpers.getUserDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_UserDatum
    ---------------------
        !value_In_UserDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_UserDatum
    --------------------
        !investAmount = T.udInvest userDatum_In
    ---------------------
    !value_InvestAmount <- OffChainHelpers.createValueAddingTokensOfCurrencySymbol staking_AC staking_CS stakingIsWithoutTokenName value_In_UserDatum investAmount
    ---------------------
    let
        !mindAda_In_UserDatum = T.udMinAda userDatum_In
        !value_MinAda_In_UserDatum = LedgerAda.lovelaceValueOf mindAda_In_UserDatum
    ---------------------
        !value_InvestAmountPlusAda = value_InvestAmount <> value_MinAda_In_UserDatum
    ---------------------
        !value_For_User = value_InvestAmountPlusAda
    ---------------------
        !value_For_Mint_TxID_User_Withdraw = LedgerValue.assetClassValue txID_User_Withdraw_AC 1
    ---------------------
        !value_For_Burn_TxID_User_Deposit = LedgerValue.assetClassValue userDeposit_AC (-investAmount)
        !value_For_Burn_UserID = LedgerValue.assetClassValue userID_AC (-1)
    ---------------------
        redeemer_For_Consuming_Validator_Datum = T.mkRedeemerUserWithdraw user
    ---------------------
        !redeemer_For_Mint_TxID_User_Withdraw = T.mkRedeemerMint_TxID redeemer_For_Consuming_Validator_Datum
        !redeemer_For_Burn_TxID = T.mkRedeemerBurn_TxID
    ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
    ---------------------
        (lookupsTx_Mint_TxID_User_Withdraw, tx_Mint_TxID_User_Withdraw) = OffChainHelpers.mintToken_With_RefPolicy uTxOsAtUser uTxO_With_Script_User_Withdraw (Just redeemer_For_Mint_TxID_User_Withdraw) value_For_Mint_TxID_User_Withdraw validityRange userPPKH
        (lookupsTx_Burn_UserID, tx_Burn_UserID) = OffChainHelpers.burntToken_With_RefPolicy uTxOsAtUser uTxO_With_Script_User_Deposit (Just redeemer_For_Burn_TxID) value_For_Burn_UserID validityRange userPPKH
        (lookupsTx_Burn_TxID_User_Deposit, tx_Burn_TxID_User_Deposit) = OffChainHelpers.burntToken_With_RefPolicy uTxOsAtUser uTxO_With_Script_User_Deposit (Just redeemer_For_Burn_TxID) value_For_Burn_TxID_User_Deposit validityRange userPPKH
    ---------------------
    if isFundCountZero then do
        let
            !poolDatum_Out = T.PoolDatum poolDatum_In
        ---------------------
            !value_In_PoolDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_PoolDatum
            !value_For_PoolDatum = value_In_PoolDatum <> value_For_Mint_TxID_User_Withdraw <> value_In_UserDatum <> value_For_Burn_UserID <> negate value_For_User
        ----------------------------------
        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User Withdraw : Ending ---------------------------------------"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In: %s" (P.show poolDatum_In)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolDatum In Value: %s" (P.show value_In_PoolDatum)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "Value For User: %s" (P.show value_For_User)
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
                lookupsTx_Mint_TxID_User_Withdraw P.<>
                lookupsTx_Burn_UserID P.<>
                lookupsTx_Burn_TxID_User_Deposit P.<>
                LedgerConstraints.unspentOutputs uTxOsAtUser P.<>
                LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum]) P.<>
                LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_UserDatum]) P.<>
                LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_ScriptDatum])

            tx =
                tx_Mint_TxID_User_Withdraw P.<>
                tx_Burn_UserID P.<>
                tx_Burn_TxID_User_Deposit P.<>
                LedgerConstraints.mustSpendScriptOutputWithReference (fst uTxO_With_PoolDatum) (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum) (fst uTxO_With_ScriptDatum)  P.<>
                LedgerConstraints.mustPayToOtherScriptWithInlineDatum validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData poolDatum_Out) value_For_PoolDatum P.<>
                LedgerConstraints.mustSpendScriptOutputWithReference (fst uTxO_With_UserDatum) (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum) (fst uTxO_With_ScriptDatum) P.<>
                -- LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash user) value_InvestAmount  P.<> 
                LedgerConstraints.mustValidateIn validityRange P.<>
                LedgerConstraints.mustBeSignedBy userPPKH
        ---------------------
        submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
        txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
        PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus User Get Back Deposit (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
    else do
        uTxOs_With_FundDatums' <- OffChainHelpers.getUTxOs_With_FundDatum fundID_AC uTxOsAtValidator
        ---------------------
        !uTxOs_With_FundDatums <-
            case uTxOs_With_FundDatums' of
                [] -> do
                    PlutusContract.throwError "Can't find any uTxO with FundDatum"
                x -> do
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxOs List with FundDatum: %s" (P.show $ fst <$> x )
                    return x
        ---------------------
        let
            !uTxO_With_FundDatum = head uTxOs_With_FundDatums
        ---------------------
            !fundDatum_In = Helpers.getFundDatumTypo_FromMaybeDatum $ OffChainHelpers.getDatumFromDecoratedTxOut $ snd uTxO_With_FundDatum
            !fundDatum_Out = T.FundDatum fundDatum_In
        ---------------------
            !value_In_FundDatum = OffChainHelpers.getValueFromDecoratedTxOut $ snd uTxO_With_FundDatum
            !value_Of_TxID_User_Harvest = Helpers.getValueOfAC value_In_UserDatum txID_User_Harvest_AC
            !value_For_FundDatum = value_In_FundDatum <> value_For_Mint_TxID_User_Withdraw <> value_Of_TxID_User_Harvest  -- <> value_In_UserDatum <> value_For_Burn_UserID <> negate value_For_User
        ---------------------
        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User Withdraw : Ending ---------------------------------------"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum In: %s" (P.show fundDatum_In)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum In Value: %s" (P.show value_In_FundDatum)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "Value For User: %s" (P.show value_For_User)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "value_Of_TxID_User_Harvest: %s" (P.show value_Of_TxID_User_Harvest)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum Out: %s" (P.show fundDatum_Out)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "FundDatum Out Value: %s" (P.show value_For_FundDatum)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeemer For Consuming Validator Datum: %s" (P.show redeemer_For_Consuming_Validator_Datum)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
        ----------------------------------

        let

            inputs_TxOut_Values_And_Datums :: [T.TxOut_Value_And_Datum]
            inputs_TxOut_Values_And_Datums =
                [(OffChainHelpers.getValueFromDecoratedTxOut ci , Helpers.fromJust $ OffChainHelpers.getDatumFromDecoratedTxOut ci) | (_, ci) <- [uTxO_With_PoolDatum]]
                ++
                [(OffChainHelpers.getValueFromDecoratedTxOut ci , Helpers.fromJust $ OffChainHelpers.getDatumFromDecoratedTxOut ci) | (_, ci) <- [uTxO_With_FundDatum]]
                ++
                [(OffChainHelpers.getValueFromDecoratedTxOut ci , Helpers.fromJust $ OffChainHelpers.getDatumFromDecoratedTxOut ci) | (_, ci) <- [uTxO_With_UserDatum]]

            outputs_TxOut_Values_And_Datums :: [T.TxOut_Value_And_Datum]
            outputs_TxOut_Values_And_Datums =
                [(value_For_FundDatum , fundDatum_Out)]


            !input_TxOut_Value_And_UserDatum =
                case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum userID_AC Helpers.getUserDatumTypo_FromDatum inputs_TxOut_Values_And_Datums of
                    Nothing -> traceError "IUD" 
                    Just x  -> x    

            !input_TxOut_Value_And_FundDatum =
                case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum fundID_AC Helpers.getFundDatumTypo_FromDatum inputs_TxOut_Values_And_Datums of
                    Nothing -> traceError "IFD" 
                    Just x  -> x

            !output_TxOut_Value_And_FundDatum =
                case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum fundID_AC Helpers.getFundDatumTypo_FromDatum outputs_TxOut_Values_And_Datums of
                    Nothing -> traceError "OFD"
                    Just x  -> x

            correctOutputFundDatum_Value_WithTokens :: Bool
            !correctOutputFundDatum_Value_WithTokens =
                let
                    !value_In_UserDatum' = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_UserDatum
                ---------------------
                    !txID_User_Harvest_AC' = LedgerValue.AssetClass (txID_User_Harvest_CS, T.txID_User_Harvest_TN)
                ---------------------
                    !value_Of_TxID_User_Harvest' = Helpers.getValueOfAC value_In_UserDatum' txID_User_Harvest_AC'
                ---------------------
                    !value_For_Mint_TxID_User_Withdraw' = LedgerValue.assetClassValue txID_User_Withdraw_AC 1
                ---------------------
                    !value_In_FundDatum' = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_FundDatum
                    -- !value_For_FundDatum_Control = value_In_FundDatum <> value_For_Mint_TxID_User_Withdraw <> value_In_UserDatum <> value_For_Burn_UserID <> negate value_For_User
                    !value_For_FundDatum_Control = value_In_FundDatum' <> value_For_Mint_TxID_User_Withdraw' <> value_Of_TxID_User_Harvest'
                ---------------------
                    !value_For_FundDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_FundDatum
                in
                    Helpers.valueEqualsValue value_For_FundDatum_Real value_For_FundDatum_Control


            formatFundDatumOutputsList list = concat [  ["----", "FundDatum': " ++ P.show  fundDatum, "FundDatum Value: " ++ P.show value_For_FundDatum_] | (fundDatum, value_For_FundDatum_) <- list]

            -- formatUserDatumOutputsList list listWithValues = concat [  ["----", "UserDatum': " ++ P.show  userDatum, "UserDatum Value: " ++ P.show (listWithValues!!index)] | (index, userDatum) <- list]
            formatUserDatumOutputsList list = concat [  ["----", "UserDatum': " ++ P.show  userDatum, "UserDatum Value: " ++ P.show value_For_UserDatum_] | (userDatum, value_For_UserDatum_)  <- list]


        PlutusContract.logInfo @P.String $ TextPrintf.printf "input_TxOut_Value_And_UserDatum"
        mapM_ (PlutusContract.logInfo @P.String  ) (formatUserDatumOutputsList [input_TxOut_Value_And_UserDatum] )
        PlutusContract.logInfo @P.String $ TextPrintf.printf "----"

        PlutusContract.logInfo @P.String $ TextPrintf.printf "inputs_TxOuts_Values_And_FundDatums"
        mapM_ (PlutusContract.logInfo @P.String  ) (formatFundDatumOutputsList [input_TxOut_Value_And_FundDatum])
        PlutusContract.logInfo @P.String $ TextPrintf.printf "----"

        PlutusContract.logInfo @P.String $ TextPrintf.printf "outputs_TxOuts_Values_And_FundDatums"
        mapM_ (PlutusContract.logInfo @P.String  ) (formatFundDatumOutputsList [output_TxOut_Value_And_FundDatum])
        PlutusContract.logInfo @P.String $ TextPrintf.printf "----"

        PlutusContract.logInfo @P.String $ TextPrintf.printf "correctOutputFundDatum_Value_WithTokens: %s" (P.show correctOutputFundDatum_Value_WithTokens)

        let
            lookupsTx =
                lookupsTx_Mint_TxID_User_Withdraw P.<>
                lookupsTx_Burn_UserID P.<>
                lookupsTx_Burn_TxID_User_Deposit P.<>
                LedgerConstraints.unspentOutputs uTxOsAtUser P.<>
                LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_PoolDatum]) P.<>
                LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_FundDatum]) P.<>
                LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_UserDatum]) P.<>
                LedgerConstraints.unspentOutputs (DataMap.fromList [uTxO_With_ScriptDatum])

            tx =
                tx_Mint_TxID_User_Withdraw P.<>
                tx_Burn_UserID P.<>
                tx_Burn_TxID_User_Deposit P.<>
                LedgerConstraints.mustReferenceOutput (fst uTxO_With_PoolDatum) P.<>
                LedgerConstraints.mustSpendScriptOutputWithReference (fst uTxO_With_FundDatum) (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum) (fst uTxO_With_ScriptDatum)  P.<>
                LedgerConstraints.mustPayToOtherScriptWithDatumInTx validatorHash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundDatum_Out) value_For_FundDatum P.<>
                LedgerConstraints.mustSpendScriptOutputWithReference (fst uTxO_With_UserDatum) (T.redeemerValidatorToBuiltinData redeemer_For_Consuming_Validator_Datum) (fst uTxO_With_ScriptDatum) P.<>
                -- LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash user) value_InvestAmount  P.<> 
                LedgerConstraints.mustValidateIn validityRange P.<>
                LedgerConstraints.mustBeSignedBy userPPKH
        ---------------------
        submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
        txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
        PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus User Get Back Deposit (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)

--------------------------------------------------------------------------------------------
