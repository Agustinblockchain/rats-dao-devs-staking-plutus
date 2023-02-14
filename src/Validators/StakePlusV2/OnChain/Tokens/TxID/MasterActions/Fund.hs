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
-- -- {-# LANGUAGE Strict #-}
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
import qualified Ledger.Value                                               as LedgerValue (assetClassValue, AssetClass (AssetClass))
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, txOutValue, MintingPolicy) 
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo, ownCurrencySymbol) 
import qualified PlutusTx                                                   (compile, applyCode, liftCode)
import           PlutusTx.Prelude                                           ( Bool, Maybe(Nothing, Just), Eq((==)), BuiltinData, Semigroup((<>)), (&&), error, traceError, ($), traceIfFalse, length, AdditiveSemigroup ((+)), any )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (mkUpdated_PoolDatum_With_NewFund, valueEqualsValue, getPoolDatumTypo_FromDatum, getFundDatumTypo_FromDatum, isNFT_With_AC_InValue)
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (getInputsWithDatum, getOutputsWithDatum, validateMasterAction, isNFT_Minted_With_AC, isNotTerminated) 
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (getTxOut_Datum, getTxOut_Value, validateBurn_Token_Own_CS_Any_TN, checkIfAllAreFromSameAddress, checkIfAllSpendRedeemersAreEqual, getTxOut_Value_And_SomeDatum) 
import qualified Validators.StakePlusV2.Types.Constants                     as T (poolID_TN, fundID_TN, const_1_PD, const_1_FD)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, mkFundDatumTypo)
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator (RedeemerMasterFund, RedeemerMasterSplitFund), RedeemerMasterFundTypo (..), RedeemerMasterSplitFundTypo)
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_Master_Fund #-}
mkPolicy_TxID_Master_Fund :: T.PoolParams -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_Master_Fund !pParams !mintRedeemerRaw !ctxRaw  = 
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
                            (T.RedeemerMasterSplitFund redeemer) -> 
                                validateMasterSplitFund pParams ctx redeemer inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums 
                            _ -> traceError "INVOP"   

        then ()

        else error ()


--------------------------------------------------------------------------------

{-# INLINABLE validateMasterFund #-}
validateMasterFund :: T.PoolParams -> LedgerContextsV2.ScriptContext -> T.RedeemerMasterFundTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> Bool 
validateMasterFund !pParams !ctx !redeemer !inputs_TxOut_Values_And_Datums !outputs_TxOut_Values_And_Datums  =
        OnChainHelpers.validateMasterAction pParams info master &&
        traceIfFalse "WIO" correctIO &&
        traceIfFalse "FundID" (OnChainHelpers.isNFT_Minted_With_AC fundID_AC info ) &&          
        
        traceIfFalse "TERMINATED" (OnChainHelpers.isNotTerminated pParams info poolDatum_In) &&
        
        traceIfFalse "PD" correctOutput_PoolDatum_Updated_With_NewFund &&
        traceIfFalse "PDV" correctOutput_PoolDatum_Value_NotChanged &&
        traceIfFalse "FD" correctOutput_FundDatum_New &&
        traceIfFalse "FDV" correctOutput_FundDatum_Value_WithFunds
    where
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
        !fundID_AC = LedgerValue.AssetClass (fundID_CS, T.fundID_TN)
        ------------------
        !input_TxOut_Value_And_PoolDatum =
            case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum poolID_AC Helpers.getPoolDatumTypo_FromDatum inputs_TxOut_Values_And_Datums of
                Nothing -> traceError "IPD"
                Just x  -> x        
        ------------------
        !output_TxOut_Value_And_PoolDatum =
            case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum poolID_AC Helpers.getPoolDatumTypo_FromDatum outputs_TxOut_Values_And_Datums of
                Nothing -> traceError "OPD"
                Just x  -> x
        ------------------
        !output_TxOut_Value_And_FundDatum =
            case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum fundID_AC Helpers.getFundDatumTypo_FromDatum outputs_TxOut_Values_And_Datums of
                Nothing -> traceError "OFD"
                Just x  -> x
        ------------------
        correctIO :: Bool
        correctIO =
            -- caseIO # : REF INPUTS , NORMAL INPUTS      / OUTPUTS
            -- caseIO 1 : 0 R        , 1 (1 PD)           / 2 (1 PD, 1 FD)
            (length inputs_TxOut_Values_And_Datums  == T.const_1_PD                ) &&
            (length outputs_TxOut_Values_And_Datums == T.const_1_PD + T.const_1_FD )
        ------------------
        !poolDatum_In = OnChainNFTHelpers.getTxOut_Datum input_TxOut_Value_And_PoolDatum
        ------------------
        correctOutput_PoolDatum_Updated_With_NewFund :: Bool
        !correctOutput_PoolDatum_Updated_With_NewFund =
            let
                !poolDatum_Out_Control = Helpers.mkUpdated_PoolDatum_With_NewFund poolDatum_In master fundAmount minAda
                !poolDatum_Out_Real = OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_PoolDatum
            in  
                poolDatum_Out_Real == poolDatum_Out_Control
        ------------------
        correctOutput_PoolDatum_Value_NotChanged :: Bool
        !correctOutput_PoolDatum_Value_NotChanged =
            let
                !value_For_PoolDatum_Control = OnChainNFTHelpers.getTxOut_Value input_TxOut_Value_And_PoolDatum
                !value_For_PoolDatum_Real = OnChainNFTHelpers.getTxOut_Value output_TxOut_Value_And_PoolDatum
            in  
                Helpers.valueEqualsValue value_For_PoolDatum_Real value_For_PoolDatum_Control
        ------------------
        correctOutput_FundDatum_New :: Bool
        !correctOutput_FundDatum_New =
            let
                !fdFundAmount = fundAmount 
                !cashedOut = 0 
                !fdMinAda = minAda
            ------------------
                !fundDatum_Out_Control = T.mkFundDatumTypo fdFundAmount cashedOut fdMinAda
                !fundDatum_Out_Real = OnChainNFTHelpers.getTxOut_Datum output_TxOut_Value_And_FundDatum
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
                -- HARVEST UNIT VA A TENER SIEMPRE TOKEN NAME... NO HACE FALTA IR POR EL CAMINM DE LOS TOKENS SIN TOKEN NAME. 
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
                --         !value_For_FundDatum_Real_FromCurrencySymbol = Helpers.getValueOfCurrencySymbol value_For_FundDatum_Real harvest_CS
                --     ------------------
                --         !value_FundAmount = value_For_FundDatum_Real_FromCurrencySymbol
                --         !fundAmount' = Helpers.getAmtOfCurrencySymbol value_For_FundDatum_Real_FromCurrencySymbol harvest_CS
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
                Helpers.valueEqualsValue value_For_FundDatum_Real value_For_FundDatum_Control

--------------------------------------------------------------------------------

{-# INLINABLE validateMasterSplitFund #-}
validateMasterSplitFund :: T.PoolParams -> LedgerContextsV2.ScriptContext -> T.RedeemerMasterSplitFundTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> Bool 
validateMasterSplitFund !pParams _ _ !inputs_TxOut_Values_And_Datums _ =
    -- si el redeemer es split, no hago mas control 
    -- para que todo funcione, debo asegurarme que haya algun redeemer spent y que sea split tambien
    -- si hay spend significa que el validador se va a ejecutar
    -- alli el validador controlará si el redeemer es split que este el tx split
    -- la poliza tx split es la que hace el control completo.
    -- para asegurarme de que se ejecuta el validador correcto, debo asegurarme que haya un redeemer spent, pero que sea de la misma address del validador
    -- eso me lo garantizo al buscar un pool datum en las imputs.
        traceIfFalse "IPD" inputPD         
    where
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        !inputPD = any  (\(v, _) -> Helpers.isNFT_With_AC_InValue v poolID_AC ) inputs_TxOut_Values_And_Datums

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

