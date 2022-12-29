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
{- HLINT ignore "Use camelCase" -}
------------------------------------------------------------------------------------------
module Validators.StakePlusV2.OnChain.Tokens.TxID.UserActions.Withdraw 
(
    policy_TxID_User_Withdraw 
) 
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Ada                                                 as LedgerAda
import qualified Ledger.Value                                               as LedgerValue (assetClassValue, AssetClass (AssetClass), assetClass)
import qualified Plutonomy  
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, txOutValue, MintingPolicy, CurrencySymbol, adaSymbol, TokenName (TokenName)) 
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo) 
import qualified PlutusTx                                                   (compile, applyCode, liftCode)
import           PlutusTx.Prelude                                           ( Bool(..), Maybe(Nothing, Just), Eq((==)), BuiltinData, Semigroup((<>)), (&&), not, (||), error, traceError, ($), (++), negate, traceIfFalse, emptyByteString )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (fromJust, createValueAddingTokensOfCurrencySymbol)
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (getInputsWithDatum, getReferenceInputsWithDatum, getOutputsWithDatum, validateUserAction, isNFT_Minted_With_AC, isClosed, isToken_Minted_With_AC_AndAmt)
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (validateBurn_Token_Own_CS_Any_TN, checkIfAllAreFromSameAddress, checkIfAllSpendRedeemersAreEqual, getTxOut_Value_And_PoolDatum, getTxOuts_Values_And_UserDatums, getTxOut_Datum, getTxOut_Value, getTxOut_Value_And_PoolDatum, getTxOut_Datum, getTxOut_Value, getTxOuts_Values_And_FundDatums, getTxOut_Value_And_PoolDatum, getTxOut_Datum, getTxOut_Value)
import qualified Validators.StakePlusV2.Types.Constants                     as T (poolID_TN, userID_TN, fundID_TN, txID_User_Withdraw_TN, txID_User_Deposit_For_User_TN)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, UserDatumTypo (..), PoolDatumTypo (..), DatumValidator (PoolDatum, FundDatum))
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator (RedeemerUserWithdraw), RedeemerUserWithdrawTypo (..))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_User_Withdraw #-}
mkPolicy_TxID_User_Withdraw :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol ->  BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_User_Withdraw pParams curSymbol_TxID_Master_Fund curSymbol_TxID_User_Deposit mintRedeemerRaw ctxRaw  = 
    let
        !mintRedeemer = LedgerApiV2.unsafeFromBuiltinData @T.Redeemer_TxID mintRedeemerRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
    in
        if case mintRedeemer of 
            
            (T.RedeemerBurn_TxID T.RedeemerBurn_TxIDTypo) -> OnChainNFTHelpers.validateBurn_Token_Own_CS_Any_TN ctx
            (T.RedeemerMint_TxID T.RedeemerMint_TxIDTypo{..}) -> 
                let
                    !redeemer' = mrRedeemerValidator

                    !inputs_WithDatum = OnChainHelpers.getInputsWithDatum ctx
                    !inputsReference_WithDatum = OnChainHelpers.getReferenceInputsWithDatum ctx
                    !inputs_Normal_And_Refs = inputs_WithDatum ++ inputsReference_WithDatum
                    !outputs_WithDatum = OnChainHelpers.getOutputsWithDatum ctx

                    !inputs_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- inputs_WithDatum ]
                    !inputsReference_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- inputsReference_WithDatum]
                    !outputs_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- outputs_WithDatum ]
                in
                    traceIfFalse "INVIO" (OnChainNFTHelpers.checkIfAllAreFromSameAddress inputs_Normal_And_Refs outputs_WithDatum) && 
                    traceIfFalse "INVR" (OnChainNFTHelpers.checkIfAllSpendRedeemersAreEqual ctx redeemer') && 
                
                    case redeemer' of 
                        (T.RedeemerUserWithdraw redeemer) -> 
                            validateUserWithdraw pParams curSymbol_TxID_Master_Fund curSymbol_TxID_User_Deposit ctx redeemer inputs_TxOut_Values_And_Datums inputsReference_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
                        _ -> traceError "INVOP"-- "Minting TxID: Invalid Operation"
        then () 

        else error ()
        
--------------------------------------------------------------------------------

{-# INLINABLE validateUserWithdraw #-}
validateUserWithdraw :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerContextsV2.ScriptContext -> T.RedeemerUserWithdrawTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] ->[T.TxOut_Value_And_Datum] ->  Bool
validateUserWithdraw pParams curSymbol_TxID_Master_Fund curSymbol_TxID_User_Deposit ctx redeemer inputs_TxOut_Values_And_Datums inputsReference_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums  =
        
        -- Generic validation for User actions 
        OnChainHelpers.validateUserAction pParams info user user_UserDatum_In && -- OnChainNFT.curSymbol_UserID 

        -- Check if the txID_User_Harvest_AC minted is there
        traceIfFalse "UGI" (OnChainHelpers.isNFT_Minted_With_AC txID_User_Withdraw_AC  info ) &&  -- "Can't Find Valid TxID" 
        
        -- no hay chekeo de tiempos, puede recuperar su inversion/depostio siempre
        -- pero si lo hace antes de que el pool este closed tiene que entregar los tokens de user investAmount que se le dieron
        -- para evitar que se vuelta a registrar y obtenga nuevos tokens que pueden ser funcionales en algun optro sitio
        traceIfFalse "UIGB" isGivingBackTheUserDepositTokens && -- "User is not giving back the User Deposit Tokens"

        correctOutput_PoolDatum_Or_FunDatum

    where
        ------------------
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        user = T.ruwUser redeemer
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        !userID_CS = curSymbol_TxID_User_Deposit
        !userID_TN = T.userID_TN
        !userID_AC = LedgerValue.assetClass userID_CS userID_TN
        ------------------
        !txID_User_Withdraw_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !txID_User_Withdraw_AC =  LedgerValue.AssetClass (txID_User_Withdraw_CS, T.txID_User_Withdraw_TN)
        ------------------
        !inputNormalAndReference = inputs_TxOut_Values_And_Datums ++ inputsReference_TxOut_Values_And_Datums
        ------------------
        !inputNormalOrReference_TxOut_Value_And_PoolDatum' = OnChainNFTHelpers.getTxOut_Value_And_PoolDatum poolID_AC inputNormalAndReference
        !inputNormalOrReference_TxOut_Value_And_PoolDatum = 
            case inputNormalOrReference_TxOut_Value_And_PoolDatum' of
                Nothing -> traceError "INRPD" -- "Error. Can't find single reference input with PoolDatun"
                _       -> Helpers.fromJust inputNormalOrReference_TxOut_Value_And_PoolDatum'
        ------------------
        !input_TxOut_Value_And_UserDatum' = 
            case OnChainNFTHelpers.getTxOuts_Values_And_UserDatums userID_AC inputs_TxOut_Values_And_Datums of
                [x] -> Just x
                _ -> Nothing
        !input_TxOut_Value_And_UserDatum = 
            case input_TxOut_Value_And_UserDatum' of
                Nothing -> traceError "IUD" -- "Error. Can't find input with UserDatum" 
                _       -> Helpers.fromJust input_TxOut_Value_And_UserDatum'
        ------------------
        !poolDatum_In = OnChainNFTHelpers.getTxOut_Datum inputNormalOrReference_TxOut_Value_And_PoolDatum
        ------------------
        !isFundCountZero = T.pdFundCount poolDatum_In == 0
        ------------------
        !userDatum_In = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_UserDatum
        ------------------
        !user_UserDatum_In = T.udUser userDatum_In
        ------------------
        !investAmount = T.udInvest userDatum_In
        ------------------
        isGivingBackTheUserDepositTokens :: Bool
        !isGivingBackTheUserDepositTokens =
            let 
                !txID_User_Deposit_For_User_AC =  LedgerValue.AssetClass (userID_CS, T.txID_User_Deposit_For_User_TN)
            ------------------
                !isClosed = OnChainHelpers.isClosed pParams info poolDatum_In 
                !isTokensBurning = OnChainHelpers.isToken_Minted_With_AC_AndAmt txID_User_Deposit_For_User_AC (negate investAmount) info 
            in
                isClosed || (not isClosed && isTokensBurning) 
        ------------------
        correctOutput_PoolDatum_Or_FunDatum :: Bool
        !correctOutput_PoolDatum_Or_FunDatum =
            let 
                !value_For_Mint_TxID_User_Withdraw = LedgerValue.assetClassValue txID_User_Withdraw_AC 1
                !value_For_Burn_UserID = LedgerValue.assetClassValue userID_AC (-1)
            ------------------
                !value_In_UserDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_UserDatum
            ---------------------
                !staking_CS = T.ppStaking_CS pParams
                !staking_AC = LedgerValue.AssetClass (staking_CS, T.ppStaking_TN pParams)
                !stakingIsAda = LedgerApiV2.adaSymbol == staking_CS
                !stakingIsWithoutTokenName = not stakingIsAda && T.ppStaking_TN pParams == LedgerApiV2.TokenName emptyByteString
            ---------------------
                !value_InvestAmount = Helpers.createValueAddingTokensOfCurrencySymbol staking_AC staking_CS stakingIsWithoutTokenName value_In_UserDatum investAmount
            ---------------------
                !mindAda_In_UserDatum = T.udMinAda userDatum_In
                !value_MinAda_In_UserDatum = LedgerAda.lovelaceValueOf mindAda_In_UserDatum
            ---------------------
                !value_InvestAmountPlusAda = value_InvestAmount <> value_MinAda_In_UserDatum
                !value_For_User = value_InvestAmountPlusAda
            ------------------
            in
                if isFundCountZero then
                    let 
                        ------------------
                        !output_TxOut_Value_And_PoolDatum' = OnChainNFTHelpers.getTxOut_Value_And_PoolDatum poolID_AC outputs_TxOut_Values_And_Datums
                        !output_TxOut_Value_And_PoolDatum =
                            case output_TxOut_Value_And_PoolDatum' of
                                Nothing -> traceError "OPD" -- "Error. Can't find output with PoolDatum"
                                _       -> Helpers.fromJust output_TxOut_Value_And_PoolDatum'
                        ------------------
                        correctOutput_PoolDatum_NotChanged :: Bool
                        !correctOutput_PoolDatum_NotChanged =
                            let
                                !poolDatum_Out_Control = T.PoolDatum $ OnChainNFTHelpers.getTxOut_Datum inputNormalOrReference_TxOut_Value_And_PoolDatum
                                !poolDatum_Out_Real = T.PoolDatum $ OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_PoolDatum
                            in
                                poolDatum_Out_Real == poolDatum_Out_Control
                        ------------------
                        correctOutput_PoolDatum_Value_WithTokens :: Bool
                        !correctOutput_PoolDatum_Value_WithTokens =
                            let
                                !value_In_PoolDatum = OnChainNFTHelpers.getTxOut_Value inputNormalOrReference_TxOut_Value_And_PoolDatum
                                !value_For_PoolDatum_Control = value_In_PoolDatum <> value_For_Mint_TxID_User_Withdraw <> value_In_UserDatum <> value_For_Burn_UserID <> negate value_For_User     
                            ---------------------
                                !value_For_PoolDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_PoolDatum
                            in
                                value_For_PoolDatum_Real == value_For_PoolDatum_Control
                    in 
                        -- Without the UserID
                        traceIfFalse "PD" correctOutput_PoolDatum_NotChanged && --Wrong Updated FundDatum

                        -- Must be the same than the input.
                        traceIfFalse "PDV" correctOutput_PoolDatum_Value_WithTokens --Wrong FundDatum Value
                else
                    let 
                        !fundID_CS = curSymbol_TxID_Master_Fund
                        !fundID_TN = T.fundID_TN
                        !fundID_AC = LedgerValue.assetClass fundID_CS fundID_TN
                        ------------------
                        !input_TxOut_Value_And_FundDatum' =
                            case OnChainNFTHelpers.getTxOuts_Values_And_FundDatums fundID_AC inputs_TxOut_Values_And_Datums of
                                [x] -> Just x
                                _   -> Nothing
                        !input_TxOut_Value_And_FundDatum =
                            case input_TxOut_Value_And_FundDatum' of
                                Nothing -> traceError "IFD" -- "Error. Can't find inputs with FundDatum"
                                _       -> Helpers.fromJust input_TxOut_Value_And_FundDatum'
                        ------------------
                        !output_TxOut_Value_And_FundDatum' =
                            case OnChainNFTHelpers.getTxOuts_Values_And_FundDatums fundID_AC outputs_TxOut_Values_And_Datums of
                                [x] -> Just x
                                _   -> Nothing
                        !output_TxOut_Value_And_FundDatum =
                            case output_TxOut_Value_And_FundDatum' of
                                Nothing -> traceError "OFD" -- "Error. Can't find output with FundDatum"
                                _       -> Helpers.fromJust output_TxOut_Value_And_FundDatum'
                        ------------------      
                        isNotPresentPoolDatumAsRef :: Bool
                        !isNotPresentPoolDatumAsRef = 
                            let 
                                !inputNormal_TxOut_Value_And_PoolDatum' = OnChainNFTHelpers.getTxOut_Value_And_PoolDatum poolID_AC inputs_TxOut_Values_And_Datums
                            in
                                case inputNormal_TxOut_Value_And_PoolDatum' of
                                    Nothing -> True
                                    _       -> False       
                        ------------------      
                        correctOutput_FundDatum_NotChanged :: Bool
                        !correctOutput_FundDatum_NotChanged =
                            let
                                !fundDatum_Out_Control = T.FundDatum $ OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_FundDatum
                                !fundDatum_Out_Real = T.FundDatum $ OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_FundDatum
                            in
                                fundDatum_Out_Real == fundDatum_Out_Control
                        ------------------
                        correctOutputFundDatum_Value_WithTokens :: Bool
                        !correctOutputFundDatum_Value_WithTokens =
                            let
                                !value_In_FundDatum = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_FundDatum
                                !value_For_FundDatum_Control = value_In_FundDatum <> value_For_Mint_TxID_User_Withdraw <> value_In_UserDatum <> value_For_Burn_UserID <> negate value_For_User
                            ---------------------
                                !value_For_FundDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_FundDatum
                            in
                                value_For_FundDatum_Real == value_For_FundDatum_Control
                    in 
                        traceIfFalse "IRPDE" isNotPresentPoolDatumAsRef &&  

                        -- Without the UserID
                        traceIfFalse "FD" correctOutput_FundDatum_NotChanged && --Wrong Updated FundDatum

                        -- Must be the same than the input.
                        traceIfFalse "FDV" correctOutputFundDatum_Value_WithTokens --Wrong FundDatum Value

--------------------------------------------------------------------------------

policy_TxID_User_Withdraw :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.MintingPolicy
policy_TxID_User_Withdraw pParams curSymbol_TxID_Master_Fund curSymbol_TxID_User_Deposit  = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams curSymbol_TxID_Master_Fund curSymbol_TxID_User_Deposit 

original_policy :: T.PoolParams -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.CurrencySymbol -> Plutonomy.MintingPolicy
original_policy pParams curSymbol_TxID_Master_Fund curSymbol_TxID_User_Deposit  =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkPolicy_TxID_User_Withdraw ||])
    `PlutusTx.applyCode` PlutusTx.liftCode pParams
    `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_Master_Fund
    `PlutusTx.applyCode` PlutusTx.liftCode curSymbol_TxID_User_Deposit
    
--------------------------------------------------------------------------------
