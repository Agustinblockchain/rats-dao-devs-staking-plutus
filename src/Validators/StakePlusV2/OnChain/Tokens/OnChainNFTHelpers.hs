-- {-# LANGUAGE DataKinds                  #-}
-- {-# LANGUAGE DeriveAnyClass             #-}
-- {-# LANGUAGE DeriveGeneric              #-}
-- {-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase                 #-}
-- {-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
-- {-# LANGUAGE RecordWildCards            #-}
-- {-# LANGUAGE ScopedTypeVariables        #-}
-- {-# LANGUAGE TemplateHaskell            #-}
-- {-# LANGUAGE TypeApplications           #-}
-- {-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE TupleSections              #-}
-- {-# LANGUAGE AllowAmbiguousTypes        #-}
-- {-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{- HLINT ignore "Use camelCase" -}
------------------------------------------------------------------------------------------
module Validators.StakePlusV2.OnChain.Tokens.OnChainNFTHelpers where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Ledger.Ada                                                 as LedgerAda
import qualified Ledger.Value                                               as LedgerValue
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ScriptContext (scriptContextTxInfo), TxInfo, ownCurrencySymbol)
import qualified PlutusTx.AssocMap                                          as TxAssocMap
-- import qualified PlutusTx.Foldable                                          as TxFold
import           PlutusTx.Prelude                                           ( Bool(..), Integer, Maybe(..), Ordering(GT, LT), Eq((==)), Ord((>=), (<), (>)), AdditiveGroup((-)), Semigroup((<>)), (&&), not, ($), fst, snd, all, any, null, head, tail, negate, traceIfFalse, traceError, (++) )
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.StakePlusV2.Helpers                             as Helpers (fromJust, isToken_With_AC_InValue, getFundAmountCanUse_in_FundDatum, mkUpdated_FundDatum_With_NewClaimRewards)
import qualified Validators.StakePlusV2.Types.DatumsValidator               as T (TxOut_Value_And_Datum, TxOut_With_Datum, TxOut_Value_And_FundDatum, DatumValidator, FundDatumTypo (..))
import qualified Validators.StakePlusV2.Types.RedeemersValidator            as T (RedeemerValidator)
--import qualified Validators.StakePlusV2.OnChain.Core.OnChainHelpers       as OnChainHelpers
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE hasInputUTxO #-}
hasInputUTxO :: LedgerApiV2.TxOutRef -> LedgerContextsV2.TxInfo -> Bool
hasInputUTxO !txOutRef !info = any (\i -> LedgerApiV2.txInInfoOutRef i == txOutRef) $ LedgerApiV2.txInfoInputs info
-------------------------------------------------------------------------------------------  

{-# INLINABLE getOwnMintedTokenNameAndAmt  #-}
getOwnMintedTokenNameAndAmt :: LedgerContextsV2.ScriptContext -> [(LedgerApiV2.TokenName, Integer)]
getOwnMintedTokenNameAndAmt !ctx =
    let
        !cs = LedgerContextsV2.ownCurrencySymbol ctx

        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx

        !flatten = TxAssocMap.lookup cs (LedgerApiV2.getValue $ LedgerApiV2.txInfoMint info)
    in
        TxAssocMap.toList $ Helpers.fromJust flatten

        -- case TxAssocMap.toList $ Helpers.fromJust flatten of
        --     [(tn, amt)] -> Just (cs, tn, amt)
        --     _           -> Nothing

-------------------------------------------------------------------------------------------  

{-# INLINABLE validateMint_NFT_Own_CS_Any_TN #-}
validateMint_NFT_Own_CS_Any_TN :: LedgerContextsV2.ScriptContext -> Bool
validateMint_NFT_Own_CS_Any_TN !ctx  =
    traceIfFalse "MAMT" checkNFTMintedAmountV2  -- "Minting NFT: Wrong Mint Amount"
    where
        !checkNFTMintedAmountV2 =
            case getOwnMintedTokenNameAndAmt ctx of
                []  -> False
                x   -> all (\(_, amt) -> amt == 1) x
                -- [(_, amt)] -> amt == 1
                -- _           -> False
                -- Just (_, _, amt)   -> amt == 1
                -- _                  -> False

-------------------------------------------------------------------------------------------

{-# INLINABLE validateBurn_NFT_Own_CS_Any_TN #-}
validateBurn_NFT_Own_CS_Any_TN :: LedgerContextsV2.ScriptContext -> Bool
validateBurn_NFT_Own_CS_Any_TN !ctx  =
    traceIfFalse "BNFTAMT" checkNFTBurnedAmountV2 -- "Minting NFT: Wrong Burn Amount"
    where
        !checkNFTBurnedAmountV2 =
            case getOwnMintedTokenNameAndAmt ctx of
                []  -> False
                x   -> all (\(_, amt) -> amt == negate 1) x
                -- [(_, amt)] -> amt == negate 1
                -- _           -> False
                -- Just (_, _, amt)    -> amt == negate 1
                -- _                   -> False

-------------------------------------------------------------------------------------------

{-# INLINABLE validateBurn_Token_Own_CS_Any_TN #-}
validateBurn_Token_Own_CS_Any_TN ::  LedgerContextsV2.ScriptContext -> Bool
validateBurn_Token_Own_CS_Any_TN !ctx  =
    traceIfFalse "BTAMT" checkAnyBurnedAmountV2 -- "Minting Token: Wrong Burn Amount"
    where
        !checkAnyBurnedAmountV2 =
            case getOwnMintedTokenNameAndAmt ctx of
                    []  -> False
                    x   -> all (\(_, amt) -> amt < 0) x
                    -- [(_, amt)] -> amt == negate 1
                    -- _        -> False
                -- Just (_, _, amt)    -> amt < 0
                -- _                   -> False

-------------------------------------------------------------------------------------------  
-- Helpers para la validacion de la tx
-------------------------------------------------------------------------------------------  

{-# INLINABLE getTxOut_Value #-}
getTxOut_Value :: (x, y) -> x
getTxOut_Value = fst

-- {-# INLINABLE getTxOut_ValueEX #-}
-- getTxOut_ValueEX :: (Integer, d1) -> [(v,d2)] -> v
-- getTxOut_ValueEX (index, _) z = fst (z!!index)

{-# INLINABLE getTxOut_Datum #-}
getTxOut_Datum :: (x, y) -> y
getTxOut_Datum = snd

-------------------------------------------------------------------------------------------

-- {-# INLINABLE getInputs_TxOuts_Values_And_FundDatums_Ordered #-}
-- getInputs_TxOuts_Values_And_FundDatums_Ordered :: LedgerValue.AssetClass -> LedgerContextsV2.ScriptContext -> [T.TxOut_Value_And_FundDatum]
-- getInputs_TxOuts_Values_And_FundDatums_Ordered fundID_AC ctx =
--     let
--         !inputs_TxOut_Values_And_Datums =
--             let
--                 !inputs_WithDatum = OnChainHelpers.getInputsWithDatum ctx
--             in
--                 [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- inputs_WithDatum ]

--         !inputs_TxOuts_Values_And_FundDatums =
--             let
--                 !inputs_TxOuts_Values_And_FundDatums' =
--                     case getTxOuts_Values_And_FundDatums fundID_AC inputs_TxOut_Values_And_Datums of
--                         []  -> Nothing
--                         x   -> Just x
--             in
--                 case inputs_TxOuts_Values_And_FundDatums' of
--                     Nothing -> traceError "IFDS"
--                     _       -> Helpers.fromJust inputs_TxOuts_Values_And_FundDatums'

--     in
--         sortBy sort_Value_And_FundDatum inputs_TxOuts_Values_And_FundDatums

-- -------------------------------------------------------------------------------------------

-- {-# INLINABLE getOutputs_TxOuts_Values_And_FundDatums_Ordered #-}
-- getOutputs_TxOuts_Values_And_FundDatums_Ordered :: LedgerValue.AssetClass -> LedgerContextsV2.ScriptContext -> [T.TxOut_Value_And_FundDatum]
-- getOutputs_TxOuts_Values_And_FundDatums_Ordered fundID_AC ctx =
--     let
--         !outputs_TxOut_Values_And_Datums =
--             let
--                 !outputs_WithDatum = OnChainHelpers.getOutputsWithDatum ctx
--             in
--                 [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- outputs_WithDatum ]

--         !outputs_TxOuts_Values_And_FundDatums =
--             let
--                 !outputs_TxOuts_Values_And_FundDatums' =
--                     case getTxOuts_Values_And_FundDatums fundID_AC outputs_TxOut_Values_And_Datums of
--                         []  -> Nothing
--                         x   -> Just x
--             in
--                 case outputs_TxOuts_Values_And_FundDatums' of
--                     Nothing -> traceError "OFDS"
--                     _       -> Helpers.fromJust outputs_TxOuts_Values_And_FundDatums'
--     in
--         sortBy sort_Value_And_FundDatum outputs_TxOuts_Values_And_FundDatums

-- -------------------------------------------------------------------------------------------

-- {-# INLINABLE getInput_TxOut_Value_And_UserDatum #-}
-- getInput_TxOut_Value_And_UserDatum :: LedgerValue.AssetClass -> LedgerContextsV2.ScriptContext -> T.TxOut_Value_And_UserDatum
-- getInput_TxOut_Value_And_UserDatum userID_AC ctx =
--     let
--         !input_TxOut_Value_And_UserDatum' =
--             let
--                 !inputs_TxOut_Values_And_Datums =
--                     let
--                         !inputs_WithDatum = OnChainHelpers.getInputsWithDatum ctx
--                     in
--                         [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- inputs_WithDatum ]

--             in
--                 case getTxOuts_Values_And_UserDatums userID_AC inputs_TxOut_Values_And_Datums of
--                     [x] -> Just x
--                     _ -> Nothing
--     in
--         case input_TxOut_Value_And_UserDatum' of
--             Nothing -> traceError "IUD"
--             _       -> Helpers.fromJust input_TxOut_Value_And_UserDatum'

-- -------------------------------------------------------------------------------------------

-- {-# INLINABLE getOutput_TxOut_Value_And_UserDatum #-}
-- getOutput_TxOut_Value_And_UserDatum :: LedgerValue.AssetClass -> LedgerContextsV2.ScriptContext -> T.TxOut_Value_And_UserDatum
-- getOutput_TxOut_Value_And_UserDatum userID_AC ctx =
--     let
--         !output_TxOut_Value_And_UserDatum' =
--             let
--                 !outputs_TxOut_Values_And_Datums =
--                     let
--                         !outputs_WithDatum = OnChainHelpers.getOutputsWithDatum ctx
--                     in
--                         [ (LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- outputs_WithDatum ]
--             in
--                 case getTxOuts_Values_And_UserDatums userID_AC outputs_TxOut_Values_And_Datums of
--                     [x] -> Just x
--                     _ -> Nothing
--     in
--         case output_TxOut_Value_And_UserDatum' of
--             Nothing -> traceError "OUD" 
--             _       -> Helpers.fromJust output_TxOut_Value_And_UserDatum'



-------------------------------------------------------------------------------------------

-- {-# INLINABLE getTxOut_Value_And_PoolDatum #-}
-- getTxOut_Value_And_PoolDatum :: LedgerValue.AssetClass -> [T.TxOut_Value_And_Datum] -> Maybe T.TxOut_Value_And_PoolDatum
-- getTxOut_Value_And_PoolDatum poolID_AC txOut_Values_And_Datums =
--     let
--         !txIDInOutWithPoolDatum' =
--             let
--                 !txIDInOutWithPoolDatum = [ (value, datum) | (value, datum) <- txOut_Values_And_Datums, Helpers.isNFT_With_AC_InValue value poolID_AC]
--             in
--                 [(value, Helpers.getPoolDatumTypo_FromDatum datum) | (value, datum) <- txIDInOutWithPoolDatum ]
--     in
--         case txIDInOutWithPoolDatum' of
--             [x] -> Just x
--             _   -> Nothing


-------------------------------------------------------------------------------------------

-- {-# INLINABLE getTxOut_Value_And_FundDatum #-}
-- getTxOut_Value_And_FundDatum :: LedgerValue.AssetClass -> [T.TxOut_Value_And_Datum] -> Maybe T.TxOut_Value_And_FundDatum
-- getTxOut_Value_And_FundDatum fundID_AC txOuts_Value_And_Datum =
--     let
--         !txIDInOut =
--             let
--                 !txIDInOutWithFundDatum = [ (value, datum) | (value, datum) <- txOuts_Value_And_Datum, Helpers.isToken_With_AC_InValue value fundID_AC]
--             in
--                 [(value, Helpers.getFundDatumTypo_FromDatum datum) | (value, datum) <- txIDInOutWithFundDatum]
--     in
--         case txIDInOut of
--             [x] -> Just x
--             _   -> Nothing

-- {-# INLINABLE getTxOuts_Values_And_FundDatums #-}
-- getTxOuts_Values_And_FundDatums :: LedgerValue.AssetClass -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_FundDatum]
-- getTxOuts_Values_And_FundDatums fundID_AC txOuts_Value_And_Datum =
--     let
--         !txIDInOutWithFundDatum = [ (value, datum) | (value, datum) <- txOuts_Value_And_Datum, Helpers.isToken_With_AC_InValue value fundID_AC]
--     in
--         [(value, Helpers.getFundDatumTypo_FromDatum datum) | (value, datum) <- txIDInOutWithFundDatum]

-------------------------------------------------------------------------------------------  

-- {-# INLINABLE getUnsafe_TxOut_Value_And_SomeDatum #-}
-- getUnsafe_TxOut_Value_And_SomeDatum :: LedgerValue.AssetClass -> (T.DatumValidator -> a) -> [T.TxOut_Value_And_Datum] -> (LedgerApiV2.Value, a)
-- getUnsafe_TxOut_Value_And_SomeDatum ac getSomeDatumTypo txOuts_Value_And_Datum =
--     let
--         !txIDInOut =
--             let
--                 !txIDInOutWithSomeDatum = [ (value, datum) | (value, datum) <- txOuts_Value_And_Datum, Helpers.isToken_With_AC_InValue value ac]
--             in
--                 [(value, getSomeDatumTypo datum) | (value, datum) <- txIDInOutWithSomeDatum]
--     in
--         case txIDInOut of
--             [x] -> x
--             _   -> traceError "GD1"

{-# INLINABLE getTxOut_Value_And_SomeDatum #-}
getTxOut_Value_And_SomeDatum :: LedgerValue.AssetClass -> (T.DatumValidator -> a) -> [T.TxOut_Value_And_Datum] -> Maybe (LedgerApiV2.Value, a)
getTxOut_Value_And_SomeDatum !ac !getSomeDatumTypo !txOuts_Value_And_Datum =
    let
        !txIDInOut =
            let
                !txIDInOutWithSomeDatum = [ (value, datum) | (value, datum) <- txOuts_Value_And_Datum, Helpers.isToken_With_AC_InValue value ac]
            in
                [(value, getSomeDatumTypo datum) | (value, datum) <- txIDInOutWithSomeDatum]
    in
        case txIDInOut of
            [x] -> Just x
            _   -> Nothing


{-# INLINABLE getTxOuts_Values_And_SomeDatums #-}
getTxOuts_Values_And_SomeDatums :: LedgerValue.AssetClass -> (T.DatumValidator -> a) -> [T.TxOut_Value_And_Datum] -> Maybe [(LedgerApiV2.Value, a)]
getTxOuts_Values_And_SomeDatums !ac !getSomeDatumTypo !txOuts_Value_And_Datum =
    let
        !txIDInOut =
            let
                !txIDInOutWithSomeDatum = [ (value, datum) | (value, datum) <- txOuts_Value_And_Datum, Helpers.isToken_With_AC_InValue value ac]
            in
                [(value, getSomeDatumTypo datum) | (value, datum) <- txIDInOutWithSomeDatum]
    in
        case txIDInOut of
            []  -> Nothing
            x   -> Just x
    
-------------------------------------------------------------------------------------------  

-- {-# INLINABLE getTxOut_Value_And_UserDatum #-}
-- getTxOut_Value_And_UserDatum :: LedgerValue.AssetClass -> [T.TxOut_Value_And_Datum] -> T.TxOut_Value_And_UserDatum
-- getTxOut_Value_And_UserDatum userID_AC txOuts_Value_And_Datum =
--     case getTxOut_Value_And_SomeDatum userID_AC Helpers.getUserDatumTypo_FromDatum txOuts_Value_And_Datum of
--         Nothing -> traceError "IUD" 
--         Just x  -> x

-- {-# INLINABLE getTxOut_Value_And_UserDatum #-}
-- getTxOut_Value_And_UserDatum :: LedgerValue.AssetClass -> [T.TxOut_Value_And_Datum] -> Maybe T.TxOut_Value_And_UserDatum
-- getTxOut_Value_And_UserDatum userID_AC txOuts_Value_And_Datum =
--     let
--         !txIDInOut =
--             let
--                 !txIDInOutWithUserDatum = [ (value, datum) | (value, datum) <- txOuts_Value_And_Datum, Helpers.isNFT_With_AC_InValue value userID_AC]
--             in
--                 [(value, Helpers.getUserDatumTypo_FromDatum datum) | (value, datum) <- txIDInOutWithUserDatum]
--     in
--         case txIDInOut of
--             [x] -> Just x
--             _   -> Nothing


-- {-# INLINABLE getTxOuts_Values_And_UserDatums #-}
-- getTxOuts_Values_And_UserDatums :: LedgerValue.AssetClass -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_UserDatum]
-- getTxOuts_Values_And_UserDatums userID_AC txOuts_Value_And_Datum =
--     let
--         !txIDInOutWithUserDatum = [ (value, datum) | (value, datum) <- txOuts_Value_And_Datum, Helpers.isNFT_With_AC_InValue value userID_AC]
--     in
--         [(value, Helpers.getUserDatumTypo_FromDatum datum) | (value, datum) <- txIDInOutWithUserDatum]


-------------------------------------------------------------------------------------------  

-- {-# INLINABLE getTxOuts_Values_And_ScriptDatums #-}
-- getTxOuts_Values_And_ScriptDatums :: LedgerValue.AssetClass -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_ScriptDatum]
-- getTxOuts_Values_And_ScriptDatums scriptID_AC txOuts_Value_And_Datum =
--     let
--         !txIDInOutWithScriptDatum = [ (value, datum) | (value, datum) <- txOuts_Value_And_Datum, Helpers.isToken_With_AC_InValue value scriptID_AC]
--     in
--         [(value, Helpers.getScriptDatumTypo_FromDatum datum) | (value, datum) <- txIDInOutWithScriptDatum ]

-------------------------------------------------------------------------------------------  

-- for ordering a list of uTxO 
{-# INLINABLE sort_Value_And_FundDatum #-}
sort_Value_And_FundDatum :: T.TxOut_Value_And_FundDatum -> T.TxOut_Value_And_FundDatum -> Ordering
sort_Value_And_FundDatum !uTxO1 !uTxO2 =
    let
        !fundDatum1 = getTxOut_Datum uTxO1
        !fundDatum2 = getTxOut_Datum uTxO2
        !getFundAmountCanUse_in_FundDatum1 = Helpers.getFundAmountCanUse_in_FundDatum fundDatum1
        !getFundAmountCanUse_in_FundDatum2 = Helpers.getFundAmountCanUse_in_FundDatum fundDatum2
    in
        if getFundAmountCanUse_in_FundDatum1 > getFundAmountCanUse_in_FundDatum2 then
            LT
        else
            if getFundAmountCanUse_in_FundDatum1 < getFundAmountCanUse_in_FundDatum2 then
                GT
            else
                let
                    !fdFundAmount1 = T.fdFundAmount fundDatum1
                    !fdFundAmount2 = T.fdFundAmount fundDatum2
                in
                    if fdFundAmount1 > fdFundAmount2 then
                        LT
                    else
                        if fdFundAmount1 < fdFundAmount2 then
                            GT
                        else
                            let
                                !fdCashedOut1 = T.fdCashedOut fundDatum1
                                !fdCashedOut2 = T.fdCashedOut fundDatum2
                            in
                                if fdCashedOut1 > fdCashedOut2 then
                                    LT
                                else
                                    if fdCashedOut1 < fdCashedOut2 then
                                        GT
                                    else
                                       let
                                            !value1 = getTxOut_Value uTxO1
                                            !value2 = getTxOut_Value uTxO2
                                            !ada1 = LedgerAda.fromValue value1
                                            !ada2 = LedgerAda.fromValue value2
                                        in
                                            if ada1 > ada2 then
                                                LT
                                            else
                                                if ada1 < ada2 then
                                                    GT
                                                else
                                                    if LedgerValue.gt value1 value2 then
                                                        LT
                                                    else
                                                        GT

-------------------------------------------------------------------------------------------

-- it creates the FundDatum, hash and value to each of the uTxO selected
{-# INLINABLE getFundDatumListWithNewValues #-}
getFundDatumListWithNewValues :: LedgerValue.AssetClass -> LedgerApiV2.CurrencySymbol -> Bool -> [T.TxOut_Value_And_FundDatum] -> Integer -> [T.TxOut_Value_And_FundDatum]
getFundDatumListWithNewValues !harvest_AC !harvest_CS !haverstIsWithoutTokenName !txOuts_Values_And_FundDatums_WithEnoughValueToClaim !claim =
    if (claim > 0) && not (null txOuts_Values_And_FundDatums_WithEnoughValueToClaim)  then
        let
            ---------------
            !uTxO = head txOuts_Values_And_FundDatums_WithEnoughValueToClaim
            !fundDatum' = getTxOut_Datum uTxO
            ---------------
            !value = getTxOut_Value uTxO
            !amountCanUse   = Helpers.getFundAmountCanUse_in_FundDatum fundDatum'
            ---------------
        in
            ---------------
            if amountCanUse - claim >= 0 then
                -- this means that with this uTxO i cover all the claim, dont need to keep adding uTxO
                let
                    !valueToSubstract =
                        -- if haverstIsWithoutTokenName then
                        --     -- si la unidad de harvest es un token, pero no tiene nombre, 
                        --     -- significa que estoy usando tokens con misma currency symbol pero diferente token name.
                        --     -- voy a tomar aleatoriamente claim cantidad de tokens de la currency symbol sin importar el token name
                        -- Helpers.createValueAddingTokensOfCurrencySymbol harvest_AC harvest_CS haverstIsWithoutTokenName value claim
                        -- else
                        -- si la unidad de harvest es ada, o es un token con nombre, 
                        LedgerValue.assetClassValue harvest_AC claim
                    ---------------
                    !newValue = value <> negate valueToSubstract
                    ---------------
                    !newFundDatumTypo = Helpers.mkUpdated_FundDatum_With_NewClaimRewards fundDatum' claim
                    ---------------
                    -- !(T.FundDatum newFundDatumTypo) = newFundDatum
                    ---------------
                in
                    [(newValue, newFundDatumTypo)]
            ---------------
            else
            ---------------
                -- this means that remaining claim is bigger than the value, so i need to keep adding uTxO
                -- ill take all i can from this uTxO
                let
                    !valueToSubstract =
                        -- if haverstIsWithoutTokenName then
                        --     -- si la unidad de harvest es un token, pero no tiene nombre, 
                        --     -- significa que estoy usando tokens con misma currency symbol pero diferente token name.
                        --     -- voy a tomar aleatoriamente harvestUnitFromValueCanUse cantidad de tokens de la currency symbol sin importar el token name
                        -- Helpers.createValueAddingTokensOfCurrencySymbol harvest_AC harvest_CS haverstIsWithoutTokenName value amountCanUse
                        -- else
                        -- si la unidad de harvest es ada, o es un token con nombre, 
                        LedgerValue.assetClassValue harvest_AC amountCanUse
                    ---------------
                    !newValue = value <> negate valueToSubstract
                    ---------------
                    !newFundDatumTypo = Helpers.mkUpdated_FundDatum_With_NewClaimRewards fundDatum' amountCanUse
                    ---------------
                    -- !(T.FundDatum newFundDatumTypo) = newFundDatum
                    ---------------
                    !newClaim = claim - amountCanUse
                    ---------------
                    !new = (newValue, newFundDatumTypo)
                    ---------------
                    !others = getFundDatumListWithNewValues harvest_AC harvest_CS haverstIsWithoutTokenName (tail txOuts_Values_And_FundDatums_WithEnoughValueToClaim) newClaim
                    ---------------
                in
                    new : others
    else
        -- si se acabo la lista, no hay mas utxos para cubrir el clamin deberia arrojar error, pero lo tengo controlado afuera
        -- al controlar si habia fondos suficientes en total.
        []


-------------------------------------------------------------------------------------------

-- {-# INLINABLE all2 #-}
-- all2 :: (a -> Bool) -> [a] -> Bool
-- all2 p xs = and (map p xs)
-- --all2 cond list = TxFold.foldl (&&) True [ cond elem | elem <- list ]

{-# INLINABLE checkIfAllAreFromSameAddress #-}
checkIfAllAreFromSameAddress :: [T.TxOut_With_Datum] -> [T.TxOut_With_Datum] -> Bool
checkIfAllAreFromSameAddress !inputs_Normal_And_Refs !outputs_WithDatum =
    let
        !inputsAddresses = [ LedgerApiV2.txOutAddress txtout | (txtout, _) <- inputs_Normal_And_Refs ]
        !outputsAddresses = [ LedgerApiV2.txOutAddress txtout | (txtout, _) <- outputs_WithDatum ]
        !inputsAndOutputs = inputsAddresses ++ outputsAddresses
        !address =
            case inputsAndOutputs of
                []  -> traceError "ADD"
                _   -> head inputsAndOutputs
    in
        ---all (== address) inputsAddresses && all (== address) outputsAddresses
        -- solo controlo la salida. 
        -- la entrada se supone que son validas por que tienen el token de Pool, Fund, User o Script ID.
        -- sería redundante controlar si vienen del contrato. No hay forma de que vengan de otro lado.
        -- esos ID se generan en condiciones controladas y siempre veo que la salida sea al contrato y no deben salir de alli.
        -- si no salen, no pueden entrar desde otro lado.
        -- A menos que haya tenido algun filtro y error en otra transaccion.
        all (== address) inputsAndOutputs 

-------------------------------------------------------------------------------------------

{-# INLINABLE checkIfAllSpendRedeemersAreEqual #-}
checkIfAllSpendRedeemersAreEqual :: LedgerContextsV2.ScriptContext -> T.RedeemerValidator -> Bool
checkIfAllSpendRedeemersAreEqual !ctx !redeemer =
    let
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        txInfoRedeemers :: [(LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)]
        !txInfoRedeemers = TxAssocMap.toList $ LedgerApiV2.txInfoRedeemers info
        isSpending :: LedgerApiV2.ScriptPurpose -> Bool
        isSpending !sp =
            case sp of
                LedgerApiV2.Spending _ -> True
                _ -> False
        spendRedeemers :: [LedgerApiV2.Redeemer]
        !spendRedeemers = [ red | (sp, red) <- txInfoRedeemers, isSpending sp ]
        validatorRedeemers :: [T.RedeemerValidator]
        !validatorRedeemers = [ LedgerApiV2.unsafeFromBuiltinData @T.RedeemerValidator $ LedgerApiV2.getRedeemer red | red <- spendRedeemers]
    in
        all (== redeemer) validatorRedeemers && not (null validatorRedeemers)

-------------------------------------------------------------------------------------------

{-# INLINABLE checkIfSpendRedeemersIsEmpty #-}
checkIfSpendRedeemersIsEmpty :: LedgerContextsV2.ScriptContext -> Bool
checkIfSpendRedeemersIsEmpty !ctx =
    let
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        txInfoRedeemers :: [(LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)]
        !txInfoRedeemers = TxAssocMap.toList $ LedgerApiV2.txInfoRedeemers info
        isSpending :: LedgerApiV2.ScriptPurpose -> Bool
        isSpending !sp =
            case sp of
                LedgerApiV2.Spending _ -> True
                _ -> False
        spendRedeemers :: [LedgerApiV2.Redeemer]
        !spendRedeemers = [ red | (sp, red) <- txInfoRedeemers, isSpending sp ]
    in
        null spendRedeemers
-------------------------------------------------------------------------------------------

-- {-# INLINABLE getCaseIO #-}
-- getCaseIO :: [(Integer,[(LedgerValue.AssetClass, Integer)], [(LedgerValue.AssetClass, Integer)])] -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> Integer
-- getCaseIO [] _ _ = traceError "WIO"    -- "wrong inputs or outputs"
-- getCaseIO ((i, b, c):xsCases) !inputs_TxOut_Values_And_Datums !outputs_TxOut_Values_And_Datums =
--     let
--         !lenI = length inputs_TxOut_Values_And_Datums
--         !lenO = length outputs_TxOut_Values_And_Datums
--         checkCaseIO :: Bool
--         checkCaseIO =
--             let
--                 checkList :: [(LedgerValue.AssetClass, Integer)] -> [T.TxOut_Value_And_Datum] -> Bool
--                 checkList [] [] = True
--                 checkList [] _ = False
--                 checkList ((!acABuscar, !countExpected):xsAssets) !list  =
--                     let
--                         -- obtengo la lista de todos los inputs que NO tengan el asset class que estoy buscando
--                         -- me sirve para enviar luego a la siguiente recursion y que haya menos elementos a buscar
--                         -- de forma indirecta calculo cuantos elementos SI son los assets que estoy buscando
--                         -- para eso resto a la longitud total lo que encontre, que no es lo que buscaba
--                         -- esa cantidad de lo que buscaba la comparo con la cantidad que espero de ese asset clasee
--                         -- a menos que la cantidad esperada sea -1, que significa que no importa cuantos sean, pero que al menos haya uno
--                         listofNOTAC = [ (value, datum) | (value, datum) <- list, not $ Helpers.isNFT_With_AC_InValue value acABuscar]
--                         len = length list - length listofNOTAC
--                     in
--                         ((len == countExpected && countExpected >= 0) || (len > 0 && countExpected == -1)) && checkList xsAssets listofNOTAC
--             in
--                 -- controlo que la cantidad de todos los tipos de assets coincidan con la longitud de la input
--                 -- a menos que haya alguna cantidad que sea -1, que significa muchos. 
--                 -- en ese caso solo controlo que la cantidad de elementos sea mayor a 0
--                 -- (TxFold.foldl (+) 0 [ c1 | (_, c1) <- b ] == lenI   || ((TxFold.foldl (||) False [ c1 == -1| (_, c1) <- b ]) && lenI > 0)) &&
--                 -- (TxFold.foldl (+) 0 [ c1 | (_, c1) <- c ] == lenO   || ((TxFold.foldl (||) False [ c1 == -1| (_, c1) <- c ]) && lenO > 0)) &&
--                 (( any  (\(_, c1) -> c1 == -1 ) b && lenI   > 0) || TxFold.foldl (+) 0 [ c1 | (_, c1) <- b ] == lenI  ) &&
--                 (( any  (\(_, c1) -> c1 == -1 ) c && lenO   > 0) || TxFold.foldl (+) 0 [ c1 | (_, c1) <- c ] == lenO  ) &&
--                 checkList b inputs_TxOut_Values_And_Datums && checkList c outputs_TxOut_Values_And_Datums
--     in
--         if checkCaseIO  then
--             i
--         else
--             getCaseIO xsCases inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums


------------------
-- !casesIO =
--     [
--         (1, []               , [(userID_AC, 1)]                      , []),
--         (2, [(poolID_AC, 1)] , [(userID_AC, 1)]                      , []),
--         (3, [(poolID_AC, 1)] , [(fundID_AC, 1), (userID_AC, 1)]      , [(fundID_AC, 1)]),
--         (4, []               , [(fundID_AC, 1), (userID_AC, 1)]      , [(fundID_AC, 1)]),
--         (5, []               , [(poolID_AC, 1), (userID_AC, 1)]      , [(poolID_AC, 1)])
--     ]
------------------
-- !currentCaseIO = OnChainNFTHelpers.getCaseIO casesIO inputsReference_TxOut_Values_And_Datums inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
------------------


-- {-# INLINABLE getCaseIO #-}
-- getCaseIO :: [(Integer, [(LedgerValue.AssetClass, Integer)], [(LedgerValue.AssetClass, Integer)], [(LedgerValue.AssetClass, Integer)])] -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> [T.TxOut_Value_And_Datum] -> Integer
-- getCaseIO [] _ _ _ = traceError "WIO"    -- "wrong inputs or outputs"
-- getCaseIO ((i, a, b, c):xsCases) !inputsReference_TxOut_Values_And_Datums !inputs_TxOut_Values_And_Datums !outputs_TxOut_Values_And_Datums =
--     let
--         !lenRef = length inputsReference_TxOut_Values_And_Datums
--         !lenI = length inputs_TxOut_Values_And_Datums
--         !lenO = length outputs_TxOut_Values_And_Datums
--         checkCaseIO :: Bool
--         checkCaseIO =
--             let
--                 checkList :: [(LedgerValue.AssetClass, Integer)] -> [T.TxOut_Value_And_Datum] -> Bool
--                 checkList [] [] = True
--                 checkList [] _ = False
--                 checkList ((!acABuscar, !countExpected):xsAssets) !list  =
--                     let
--                         -- obtengo la lista de todos los inputs que NO tengan el asset class que estoy buscando
--                         -- me sirve para enviar luego a la siguiente recursion y que haya menos elementos a buscar
--                         -- de forma indirecta calculo cuantos elementos SI son los assets que estoy buscando
--                         -- para eso resto a la longitud total lo que encontre, que no es lo que buscaba
--                         -- esa cantidad de lo que buscaba la comparo con la cantidad que espero de ese asset clasee
--                         -- a menos que la cantidad esperada sea -1, que significa que no importa cuantos sean, pero que al menos haya uno
--                         listofNOTAC = [ (value, datum) | (value, datum) <- list, not $ Helpers.isNFT_With_AC_InValue value acABuscar]
--                         len = length list - length listofNOTAC
--                     in
--                         ((len == countExpected && countExpected >= 0) || (len > 0 && countExpected == -1)) && checkList xsAssets listofNOTAC


--                 checkListRef :: [(LedgerValue.AssetClass, Integer)] -> [T.TxOut_Value_And_Datum] -> Bool
--                 checkListRef [] [] = True
--                 checkListRef [] _ = True -- en el caso de input ref, no importa que haya mas inputs de los que se esperan, son script datums
--                 checkListRef ((!acABuscar, !countExpected):xsAssets) !list  =
--                     let
--                         -- obtengo la lista de todos los inputs que NO tengan el asset class que estoy buscando
--                         -- me sirve para enviar luego a la siguiente recursion y que haya menos elementos a buscar
--                         -- de forma indirecta calculo cuantos elementos SI son los assets que estoy buscando
--                         -- para eso resto a la longitud total lo que encontre, que no es lo que buscaba
--                         -- esa cantidad de lo que buscaba la comparo con la cantidad que espero de ese asset clasee
--                         -- a menos que la cantidad esperada sea -1, que significa que no importa cuantos sean, pero que al menos haya uno
                        
--                         listofNOTAC = [ (value, datum) | (value, datum) <- list, not $ Helpers.isNFT_With_AC_InValue value acABuscar]
--                         len = length list - length listofNOTAC
--                     in
--                         ((len == countExpected && countExpected >= 0) || (len > 0 && countExpected == -1)) && checkList xsAssets listofNOTAC

--             in
--                 -- controlo que la cantidad de todos los tipos de assets coincidan con la longitud de la input
--                 -- a menos que haya alguna cantidad que sea -1, que significa muchos. 
--                 -- en ese caso solo controlo que la cantidad de elementos sea mayor a 0
--                 -- eso no lo controlo en input ref, por que alli van scripts datums como referencia y no quiero validarlos.
--                 -- los inputs como referencia no son tan cruciales, no se consumen, no me importan.
--                 -- (TxFold.foldl (+) 0 [ c1 | (_, c1) <- a ] == lenRef || ((TxFold.foldl (||) False [ c1 == -1| (_, c1) <- a ]) && lenRef > 0)) &&
--                 -- (TxFold.foldl (+) 0 [ c1 | (_, c1) <- b ] == lenI   || ((TxFold.foldl (||) False [ c1 == -1| (_, c1) <- b ]) && lenI > 0)) &&
--                 -- (TxFold.foldl (+) 0 [ c1 | (_, c1) <- c ] == lenO   || ((TxFold.foldl (||) False [ c1 == -1| (_, c1) <- c ]) && lenO > 0)) &&
--                 -- (( any  (\(_, c1) -> c1 == -1 ) a && lenRef > 0) || TxFold.foldl (+) 0 [ c1 | (_, c1) <- a ] == lenRef) &&
--                 (( any  (\(_, c1) -> c1 == -1 ) b && lenI   > 0) || TxFold.foldl (+) 0 [ c1 | (_, c1) <- b ] == lenI  ) &&
--                 (( any  (\(_, c1) -> c1 == -1 ) c && lenO   > 0) || TxFold.foldl (+) 0 [ c1 | (_, c1) <- c ] == lenO  ) &&
--                 checkListRef a inputsReference_TxOut_Values_And_Datums && checkList b inputs_TxOut_Values_And_Datums && checkList c outputs_TxOut_Values_And_Datums
--     in
--         if checkCaseIO  then
--             i
--         else
--             getCaseIO xsCases inputsReference_TxOut_Values_And_Datums inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums


------------------