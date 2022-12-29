{-# LANGUAGE DataKinds                  #-}
-- {-# LANGUAGE DeriveAnyClass             #-}
-- {-# LANGUAGE DeriveGeneric              #-}
-- {-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
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
module Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.Fund
(
    policy_TxID_Master_Fund 
)
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Ada                                                 as LedgerAda (lovelaceValueOf)
import qualified Ledger.Value                                               as LedgerValue (assetClassValue, AssetClass (AssetClass), assetClass)
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, txOutValue, MintingPolicy) 
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo, ownCurrencySymbol) 
import qualified PlutusTx                                                   (compile, applyCode, liftCode)
import           PlutusTx.Prelude                                           ( Bool(True), Maybe(Nothing, Just), Eq((==)), BuiltinData, Semigroup((<>)), (&&), error, traceError, ($), traceIfFalse )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (fromJust, mkUpdated_PoolDatum_With_NewFund)
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (getInputsWithDatum, getOutputsWithDatum, validateMasterAction, isNFT_Minted_With_AC, isNotTerminated) 
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (getTxOuts_Values_And_FundDatums, getTxOut_Datum, getTxOut_Value, getTxOut_Value_And_PoolDatum, validateBurn_Token_Own_CS_Any_TN, checkIfAllAreFromSameAddress, checkIfAllSpendRedeemersAreEqual) 
import qualified Validators.StakePlusV2.Types.Constants                     as T (poolID_TN, fundID_TN)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, DatumValidator (PoolDatum, FundDatum), mkFundDatum)
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator (RedeemerMasterFund, RedeemerMasterSplitFund), RedeemerMasterFundTypo (..))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_Master_Fund #-}
mkPolicy_TxID_Master_Fund :: T.PoolParams -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_Master_Fund pParams mintRedeemerRaw ctxRaw  =
    let
        !mintRedeemer = LedgerApiV2.unsafeFromBuiltinData @T.Redeemer_TxID mintRedeemerRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw

    in
        if
            case mintRedeemer of
            (T.RedeemerBurn_TxID T.RedeemerBurn_TxIDTypo) -> OnChainNFTHelpers.validateBurn_Token_Own_CS_Any_TN ctx
            (T.RedeemerMint_TxID T.RedeemerMint_TxIDTypo {..}) ->
                let
                    !redeemer' = mrRedeemerValidator

                    !inputs_WithDatum = OnChainHelpers.getInputsWithDatum ctx
                    !outputs_WithDatum = OnChainHelpers.getOutputsWithDatum ctx

                    !inputs_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- inputs_WithDatum ]
                    !outputs_TxOut_Values_And_Datums = [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- outputs_WithDatum ]
                in
                    traceIfFalse "INVIO" (OnChainNFTHelpers.checkIfAllAreFromSameAddress inputs_WithDatum outputs_WithDatum) && 
                    traceIfFalse "INVR" (OnChainNFTHelpers.checkIfAllSpendRedeemersAreEqual ctx redeemer') && 
                     
                    case redeemer' of
                        (T.RedeemerMasterFund redeemer) ->
                            validateMasterFund pParams ctx redeemer inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
                        (T.RedeemerMasterSplitFund _) ->
                            True
                        _ -> traceError "INVOP"-- "Minting TxID: Invalid Operation"

        then ()

        else error ()


--------------------------------------------------------------------------------

{-# INLINABLE validateMasterFund #-}
validateMasterFund :: T.PoolParams -> LedgerContextsV2.ScriptContext -> T.RedeemerMasterFundTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> Bool --LedgerValue.Value -> 
validateMasterFund pParams ctx redeemer inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums  =
        
        traceIfFalse "TERMINATED" (OnChainHelpers.isNotTerminated pParams info poolDatum_In) &&

        -- Basic validations for Master Actions 
        OnChainHelpers.validateMasterAction pParams info master &&

        -- Check if the FundID minted is there
        traceIfFalse "FundID" (OnChainHelpers.isNFT_Minted_With_AC fundID_AC info ) &&  -- "Can't Find Valid FundID" 

        -- With new FundID.
        traceIfFalse "PD" correctOutput_PoolDatum_Updated_With_NewFund && -- "Wrong Updated PoolDatum"

        -- Must be the same than the input.
        traceIfFalse "PDV" correctOutput_PoolDatum_Value_NotChanged && -- "Wrong PoolDatum Value"

        -- The new FundDatum must be initialized in the right way
        traceIfFalse "FD" correctOutput_FundDatum_New && -- "Wrong New FundDatum"

        -- Must be the new fund plus the FundID 
        traceIfFalse "FDV" correctOutput_FundDatum_Value_WithFunds -- "Wrong FundDatum Value"
    where
        
        ------------------
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !master = T.rmfMaster redeemer
        !fundAmount = T.rmfFundAmount redeemer
        !minAda = T.rmfMinAda redeemer
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        !fundID_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !fundID_TN = T.fundID_TN
        !fundID_AC = LedgerValue.assetClass fundID_CS fundID_TN
        ------------------
        !input_TxOut_Value_And_PoolDatum' = OnChainNFTHelpers.getTxOut_Value_And_PoolDatum poolID_AC inputs_TxOut_Values_And_Datums
        !input_TxOut_Value_And_PoolDatum =
            case input_TxOut_Value_And_PoolDatum' of
                Nothing -> traceError "IPD" -- "Error. Can't find input with PoolDatum" 
                _       -> Helpers.fromJust input_TxOut_Value_And_PoolDatum'
        ------------------
        !output_TxOut_Value_And_PoolDatum' = OnChainNFTHelpers.getTxOut_Value_And_PoolDatum poolID_AC outputs_TxOut_Values_And_Datums
        !output_TxOut_Value_And_PoolDatum =
            case output_TxOut_Value_And_PoolDatum' of
                Nothing -> traceError "OPD" -- "Error. Can't find output with PoolDatum"  
                _       -> Helpers.fromJust output_TxOut_Value_And_PoolDatum'
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
        !poolDatum_In = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_PoolDatum
        ------------------
        correctOutput_PoolDatum_Updated_With_NewFund :: Bool
        !correctOutput_PoolDatum_Updated_With_NewFund =
            let
                !poolDatum_Out_Control = Helpers.mkUpdated_PoolDatum_With_NewFund poolDatum_In master fundAmount minAda
                !poolDatum_Out_Real = T.PoolDatum $ OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_PoolDatum
            in  
                poolDatum_Out_Real == poolDatum_Out_Control
        ------------------
        correctOutput_PoolDatum_Value_NotChanged :: Bool
        !correctOutput_PoolDatum_Value_NotChanged =
            let
                !output_TxOut_PoolDatum_Value = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_PoolDatum
            in  
                output_TxOut_PoolDatum_Value == OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_PoolDatum
        ------------------
        correctOutput_FundDatum_New :: Bool
        !correctOutput_FundDatum_New =
            let
                !fdFundAmount = fundAmount 
                !cashedOut = 0 
                !fdMinAda = minAda
            ------------------
                !fundDatum_Out_Control = T.mkFundDatum fdFundAmount cashedOut fdMinAda
                !fundDatum_Out_Real = T.FundDatum $ OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_FundDatum
            in
                fundDatum_Out_Real == fundDatum_Out_Control
        ------------------
        correctOutput_FundDatum_Value_WithFunds :: Bool
        !correctOutput_FundDatum_Value_WithFunds =
            let
                !value_For_FundDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_FundDatum
            ------------------
                !value_For_Mint_FundID = LedgerValue.assetClassValue fundID_AC 1
            ------------------
                !valueMinAda = LedgerAda.lovelaceValueOf minAda
            ------------------
                -- !harvest_CS =  T.ppHarvest_CS pParams
                -- !harvestIsAda = LedgerApiV2.adaSymbol == harvest_CS
                -- !haverstIsWithoutTokenName = not harvestIsAda && T.ppHarvest_TN pParams == LedgerApiV2.TokenName emptyByteString
            -- in        
                -- if haverstIsWithoutTokenName then
                --     -- si la unidad de harvest es un token, pero no tiene nombre, 
                --     -- significa que estoy usando tokens con misma currency symbol pero diferente token name.
                --     -- en este caso, primero calculo el valor que existe en la output de esos tokens
                --     -- y ese valor primero lo comparo con el FundAmount
                --     -- si coinciden, entonces uso ese valor de tokens para calcular value_For_FundDatum_Control
                --     let 
                --         !value_For_FundDatum_Real_FromCurrencySymbol = Helpers.valueFromCurrencySymbol value_For_FundDatum_Real harvest_CS
                --     ------------------
                --         !value_FundAmount = value_For_FundDatum_Real_FromCurrencySymbol
                --         !fundAmount' = Helpers.valueOfCurrencySymbol value_For_FundDatum_Real_FromCurrencySymbol harvest_CS
                --     ------------------
                --         !value_For_FundDatum_Control =  value_FundAmount <> value_For_Mint_FundID <> valueMinAda
                --     in
                --         fundAmount == fundAmount' && value_For_FundDatum_Real == value_For_FundDatum_Control
                -- else
                --     -- si la unidad de harvest es ada, o es un token con nombre, 
                --     -- calculo el harvest asset class y el valor de ese asset class segun el fund amount
                --     -- y lo comparo con el valor de la output
                --     -- min ada tienen que ser simpre sumados. Si uso lovelace, al menons min ada para el  Mint FundID
                --     -- si uso otra moneda, sera eso más lo que se necesite. 
                --     -- La variable valueForFundDatumWithoutMinAda es todo lo que se necesita para calcular el min ada
                -- let
                !harvest_AC = LedgerValue.AssetClass (T.ppHarvest_CS pParams, T.ppHarvest_TN pParams)
                !value_FundAmount = LedgerValue.assetClassValue harvest_AC fundAmount
            ------------------
                !value_For_FundDatum_Control =  value_FundAmount <> value_For_Mint_FundID  <> valueMinAda
            in
                value_For_FundDatum_Real == value_For_FundDatum_Control

--------------------------------------------------------------------------------

{-# INLINEABLE policy_TxID_Master_Fund #-}
policy_TxID_Master_Fund :: T.PoolParams -> LedgerApiV2.MintingPolicy
policy_TxID_Master_Fund pParams = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams

{-# INLINEABLE original_policy #-}
original_policy :: T.PoolParams -> Plutonomy.MintingPolicy
original_policy pParams =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [||  mkPolicy_TxID_Master_Fund ||])
    `PlutusTx.applyCode` PlutusTx.liftCode pParams

--------------------------------------------------------------------------------

