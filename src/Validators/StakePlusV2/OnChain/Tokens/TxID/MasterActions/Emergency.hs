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
-- -- {-# LANGUAGE Strict #-}
{- HLINT ignore "Use camelCase" -}
------------------------------------------------------------------------------------------
module Validators.StakePlusV2.OnChain.Tokens.TxID.MasterActions.Emergency 
(
    policy_TxID_Master_Emergency
) 
where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Value                                               as LedgerValue (AssetClass (AssetClass), assetClassValue)
import qualified Plutonomy  
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2 (unsafeFromBuiltinData, txOutValue, MintingPolicy)
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ownCurrencySymbol, ScriptContext, TxInfo, scriptContextTxInfo)
import qualified PlutusTx                                                   (compile, applyCode, liftCode)
import           PlutusTx.Prelude                                           ( Bool, Maybe(Nothing, Just), Eq((==)), BuiltinData, Semigroup((<>)), (&&), error, traceError, ($), traceIfFalse, length, (++), (||) )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (mkUpdated_PoolDatum_With_ClosedAt, valueEqualsValue, getPoolDatumTypo_FromDatum) 
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers         as OnChainHelpers (getInputsWithDatum, getOutputsWithDatum, validateMasterAction, isNFT_Minted_With_AC, isDateInRange, isNotClosed, getReferenceInputsWithDatum, isEmergency) 
import qualified Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers    as OnChainNFTHelpers (getTxOut_Datum, validateBurn_Token_Own_CS_Any_TN, getTxOut_Value, checkIfAllSpendRedeemersAreEqual, checkIfAllAreFromSameAddress, getTxOut_Value_And_SomeDatum) 
import qualified Validators.StakePlusV2.Types.Constants                     as T (poolID_TN, txID_Master_Emergency_TN, const_1_PD)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum)
import qualified Validators.StakePlusV2.Types.RedeemersMint                 as T (Redeemer_TxID (..), RedeemerBurn_TxIDTypo (..), RedeemerMint_TxIDTypo(..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator (RedeemerMasterEmergency), RedeemerMasterEmergencyTypo (..))
import qualified Validators.StakePlusV2.Types.Types                         as T (PoolParams (..))
import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers as OnChainNFTHelpers
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicy_TxID_Master_Emergency #-}
mkPolicy_TxID_Master_Emergency :: T.PoolParams -> BuiltinData -> BuiltinData -> ()
mkPolicy_TxID_Master_Emergency !pParams !mintRedeemerRaw !ctxRaw  = 
   let
        !mintRedeemer = LedgerApiV2.unsafeFromBuiltinData @T.Redeemer_TxID mintRedeemerRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
    in
        if
            case mintRedeemer of
                (T.RedeemerBurn_TxID T.RedeemerBurn_TxIDTypo) -> OnChainNFTHelpers.validateBurn_Token_Own_CS_Any_TN ctx
                (T.RedeemerMint_TxID T.RedeemerMint_TxIDTypo {..}) ->
                        traceIfFalse "APPMSM" (OnChainNFTHelpers.signedByAllPoolParamMaster pParams info)
                    || 
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
                                (T.RedeemerMasterEmergency redeemer) ->
                                    validateMasterEmergency pParams ctx redeemer inputsReference_TxOut_Values_And_Datums inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
                                _ -> traceError "INVOP"
        then ()
        else error ()

--------------------------------------------------------------------------------

{-# INLINABLE validateMasterEmergency #-}
validateMasterEmergency :: T.PoolParams -> LedgerContextsV2.ScriptContext -> T.RedeemerMasterEmergencyTypo -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> Bool 
validateMasterEmergency !pParams !ctx !redeemer !inputsReference_TxOut_Values_And_Datums !inputs_TxOut_Values_And_Datums _ =
        OnChainHelpers.validateMasterAction pParams info master &&
        traceIfFalse "MCE" (OnChainHelpers.isNFT_Minted_With_AC txID_Master_Emergency_AC info ) &&
        traceIfFalse "NOTSOS" (OnChainHelpers.isEmergency poolDatum_In ) 
    where
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !master = T.rmeMaster redeemer
        -- !isEmergency = T.rmeIsEmergency redeemer
        ------------------
        !txID_Master_Emergency_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !txID_Master_Emergency_AC =  LedgerValue.AssetClass (txID_Master_Emergency_CS, T.txID_Master_Emergency_TN)
        ------------------
        !poolID_AC = LedgerValue.AssetClass (T.ppPoolID_CS pParams, T.poolID_TN)
        ------------------
        -- - FIRMARON TODOS:
        --     si esta firmado por todos los masters permito el minteo sin más control.
        --     puede consumir cualquier utxo como input
        --     - CON POOLDATUM NORMAL:
        --         si esta el pooldatum, puedo actualizar el IsEmergency a True.
        --         puedo hacer lo que quiera en realidad, romper el pooldatum incluso.
        --         no controlo nada, dejo que lo hagan como quieran.
        --         es por que quiero que el metodo sea lo menos complejo posible para que sea un mecanismo de emergencia real 
        -- - NO FIRMARON TODOS:
        --     si no firmaron todos, valido que al menos este firmando algun master y que este en emergecy.
        --     la unica forma de poner en emergency es que este firmado por todos los masters.
        --     como necesita el pool datum, con eso me aseguro que estoy usando la address del contrato
        --     y estoy verificando que la addreess sea igual en todas las utxo que quiero consumir.
        --     - CON POOLDATUM REF:
        --         - IsEmergency Actual es True
        --             permito el minteo por cualquier master
        --             puede consumir cualquier utxo como input
        --         - IsEmergency Actual es False
        --             NO PERMITO MINTEAR
        --     - CON POOLDATUM NORMAL:
        --         - IsEmergency Actual es True
        --             permito el minteo por cualquier master
        --             puede consumir cualquier utxo como input
        --         - IsEmergency Actual es False
        --             NO PERMITO MINTEAR
        ------------------
        !inputNormalAndReference = inputs_TxOut_Values_And_Datums ++ inputsReference_TxOut_Values_And_Datums
        ------------------
        !inputNormalOrReference_TxOut_Value_And_PoolDatum =
            case OnChainNFTHelpers.getTxOut_Value_And_SomeDatum poolID_AC Helpers.getPoolDatumTypo_FromDatum inputNormalAndReference of
                Nothing -> traceError "INRPD"
                Just x  -> x
         ------------------
        !poolDatum_In = OnChainNFTHelpers.getTxOut_Datum inputNormalOrReference_TxOut_Value_And_PoolDatum
        
       

--------------------------------------------------------------------------------

{-# INLINEABLE policy_TxID_Master_Emergency #-}
policy_TxID_Master_Emergency :: T.PoolParams -> LedgerApiV2.MintingPolicy
policy_TxID_Master_Emergency pParams = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy pParams

{-# INLINEABLE original_policy #-}
original_policy :: T.PoolParams -> Plutonomy.MintingPolicy
original_policy pParams =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkPolicy_TxID_Master_Emergency ||])
    `PlutusTx.applyCode` PlutusTx.liftCode pParams

--------------------------------------------------------------------------------
