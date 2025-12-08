{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module SmartTokens.Contracts.ProgrammableLogicBase (
  TokenProof (..),
  ProgrammableLogicGlobalRedeemer (..),
  mkProgrammableLogicBase,
  mkProgrammableLogicGlobal,
) where

import Generics.SOP qualified as SOP
import GHC.Generics (Generic)
import Plutarch.Builtin.Integer (pconstantInteger)
import Plutarch.Core.Context (paddressCredential, pscriptContextTxInfo,
                              ptxInInfoResolved, ptxOutDatum, ptxOutValue)
import Plutarch.Core.Integrity (pisRewardingScript)
import Plutarch.Core.Internal.Builtins (pmapData, ppairDataBuiltinRaw)
import Plutarch.Core.List
import Plutarch.Core.Utils
import Plutarch.Core.ValidationLogic hiding (pemptyLedgerValue, pvalueFromCred,
                                      pvalueToCred)
import Plutarch.Core.Value
import Plutarch.Internal.Lift
import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1.Value (Value)
import PlutusTx qualified
import SmartTokens.Types.ProtocolParams (PProgrammableLogicGlobalParams (..))
import SmartTokens.Types.PTokenDirectory (PDirectorySetNode (..))

pjustData :: Term s (PMaybeData a) -> Term s a
pjustData term =
  punsafeCoerce $ phead # (psndBuiltin # (pasConstr # pforgetData (pdata term)))

paddressStakingCredential :: Term s PAddress -> Term s PStakingCredential
paddressStakingCredential addr =
  pmatch addr $ \addr' ->
    pjustData $ paddress'stakingCredential addr'

-- TODO: Replace current corresponding input / output comparison (which compares address, reference script and datum) for multi-seize
-- with constructing the expected output from the input with this function and comparing it to the actual output.
-- Further optimize this with the optimization in the "Everything is possible" UPLC fest presentation.
-- pconstructExpectedOutputWithOutputDatum :: Term s PAddress -> Term s (PAsData (PValue 'Sorted 'Positive)) -> Term s POutputDatum -> Term s (PAsData PTxOut)
-- pconstructExpectedOutputWithOutputDatum address value datum =
--   pdata $ pcon $
--     PTxOut
--      { ptxOut'address = address
--      , ptxOut'value = value
--      , ptxOut'datum = datum
--      , ptxOut'referenceScript = pconstant Nothing
--      }

-- TODO:
-- The current implementation of the contracts in this module are not designed to be maximally efficient.
-- In the future, this should be optimized to use the redeemer indexing design pattern to not just index the directory nodes in the reference inputs,
-- but also to index the programmable inputs and outputs.
data TokenProof
  = TokenExists Integer
  | TokenDoesNotExist Integer
  deriving stock (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed ''TokenProof
  [('TokenExists, 0), ('TokenDoesNotExist, 1)]

data PTokenProof (s :: S)
  = PTokenExists { pnodeIdx :: Term s (PAsData PInteger)}
  | PTokenDoesNotExist { pnodeIdx :: Term s (PAsData PInteger)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTokenProof)

deriving via DeriveDataPLiftable PTokenProof TokenProof
  instance PLiftable PTokenProof

emptyValue :: Value
emptyValue = mempty

pemptyLedgerValue :: Term s (PValue 'Sorted 'Positive)
pemptyLedgerValue = punsafeCoerce $ pconstant @(PValue 'Unsorted 'NoGuarantees) emptyValue

pvalueFromCred :: Term s (PCredential :--> PBuiltinList (PAsData PPubKeyHash) :--> PBuiltinList (PAsData PByteString) :--> PBuiltinList (PAsData PTxInInfo) :--> PValue 'Sorted 'Positive)
pvalueFromCred = phoistAcyclic $ plam $ \cred sigs scripts inputs ->
  (pfix #$ plam $ \self acc ->
    pelimList
      (\txIn xs ->
        self
          # pmatch (ptxInInfoResolved $ pfromData txIn) (\(PTxOut {ptxOut'address, ptxOut'value}) ->
              plet ptxOut'address $ \addr ->
                pif (paddressCredential addr #== cred)
                    (
                      pmatch (paddressStakingCredential addr) $ \case
                        PStakingHash ownerCred ->
                          pmatch ownerCred $ \case
                            PPubKeyCredential pkh ->
                              pif (ptxSignedByPkh # pkh # sigs)
                                  (acc <> pfromData ptxOut'value)
                                  (ptraceInfoError "Missing required pk witness")
                            PScriptCredential scriptHash_ ->
                              pif (pelem # punsafeCoerce scriptHash_ # scripts)
                                  (acc <> pfromData ptxOut'value)
                                  (ptraceInfoError "Missing required script witness")
                        _ -> perror
                    )
                    acc
            )
          # xs
      )
      acc
  )
  # pemptyLedgerValue
  # inputs

pvalueToCred :: Term s (PCredential :--> PBuiltinList (PAsData PTxOut) :--> PValue 'Sorted 'Positive)
pvalueToCred = phoistAcyclic $ plam $ \cred inputs ->
  let value = (pfix #$ plam $ \self acc ->
                pelimList
                  (\txOut xs ->
                    self
                      # pmatch (pfromData txOut) (\(PTxOut {ptxOut'address, ptxOut'value}) ->
                          plet ptxOut'address $ \addr ->
                            pif (paddressCredential addr #== cred)
                                (acc <> pfromData ptxOut'value)
                                acc
                                )
                      # xs
                  )
                  acc
              )
              # pemptyLedgerValue
              # inputs
  in pstripAda # value

-- | Programmable logic base
-- This validator forwards its validation logic to the programmable logic stake script
-- using the withdraw-zero design pattern.
mkProgrammableLogicBase :: Term s (PAsData PCredential :--> PScriptContext :--> PUnit)
mkProgrammableLogicBase = plam $ \stakeCred ctx ->
  pmatch (pscriptContextTxInfo ctx) $ \txInfo ->
    let wdrls :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace)))
        wdrls = pto $ pfromData $ ptxInfo'wdrl txInfo
    in
      plet wdrls $ \withdrawals ->
        let firstWithdrawal :: Term _ (PAsData PCredential)
            firstWithdrawal = pfstBuiltin # (phead @PBuiltinList # withdrawals)
            hasCred =
              pif (firstWithdrawal #== stakeCred)
                  (pconstant True)
                  (
                    pcanFind @PBuiltinList
                      # plam (\withdrawPair -> pfstBuiltin # withdrawPair #== stakeCred)
                      # (ptail # withdrawals)
                  )
        in pvalidateConditions [ptraceInfoIfFalse "programmable global not invoked" hasCred]

-- | Traverse the currency symbols of the combined value of all programmable base inputs
-- (excluding the first currency symbol in `totalValue` which the ledger enforces must be Ada).
-- For each currency symbol, we check a proof that either:
-- 1. The currency symbol is in the directory (and thus is a programmable token)
--      - given that it is a programmable token, we check that associated transfer logic script is executed in the transaction
--        and add the value entry to the result.
-- 2. The currency symbol is not in the directory.
-- Return a Value containing only programmable tokens from `totalValue` by filtering out the non-programmable token entries.
pcheckTransferLogicAndGetProgrammableValue :: Term s (PAsData PCurrencySymbol :--> PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PAsData PTokenProof) :--> PBuiltinList (PAsData PByteString) :--> PValue 'Sorted 'Positive :--> PValue 'Sorted 'Positive)
pcheckTransferLogicAndGetProgrammableValue = plam $ \directoryNodeCS refInputs proofList scripts totalValue ->
  plet (pelemAtFast @PBuiltinList # refInputs) $ \patRefUTxOIdx ->
    let mapInnerList :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger))))
        mapInnerList = pto (pto totalValue)
        -- go :: Term _ (PBuiltinList (PAsData PTokenProof) :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger))) :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger))) :--> PValue 'Sorted 'Positive)
        go = pfix #$ plam $ \self proofs inputInnerValue actualProgrammableTokenValue ->
              pelimList
                (\csPair csPairs ->
                  let cs :: Term _ (PAsData PByteString)
                      cs = punsafeCoerce $ pfstBuiltin # csPair
                  in
                    pmatch (pfromData $ phead # proofs) $ \case
                      PTokenExists nodeIdx -> P.do
                        PTxOut {ptxOut'value=directoryNodeUTxOFValue, ptxOut'datum=directoryNodeUTxOFDatum} <- pmatch $ ptxInInfoResolved (pfromData $ patRefUTxOIdx # pfromData nodeIdx)
                        POutputDatum paramDat' <- pmatch directoryNodeUTxOFDatum
                        PDirectorySetNode {pkey=directoryNodeDatumFkey, ptransferLogicScript=directoryNodeDatumFTransferLogicScript} <- pmatch (pfromData $ punsafeCoerce @(PAsData PDirectorySetNode) (pto paramDat'))
                        let transferLogicScriptHash = punsafeCoerce @(PAsData PByteString) $ phead #$ psndBuiltin #$ pasConstr # pforgetData directoryNodeDatumFTransferLogicScript
                        -- validate that the directory entry for the currency symbol is referenced by the proof
                        -- and that the associated transfer logic script is executed in the transaction
                        let checks =
                              pand'List
                                [ ptraceInfoIfFalse "Missing required transfer script" $ pelem # transferLogicScriptHash # scripts
                                , ptraceInfoIfFalse "directory proof mismatch" $ punsafeCoerce directoryNodeDatumFkey #== cs
                                , ptraceInfoIfFalse "invalid dir node" $ phasDataCS # directoryNodeCS # pfromData directoryNodeUTxOFValue
                                ]
                        pif checks
                            (self # (ptail # proofs) # csPairs # (pcons # csPair # actualProgrammableTokenValue))
                            perror
                      PTokenDoesNotExist notExistNodeIdx -> P.do
                        PTxOut {ptxOut'value=prevNodeUTxOValue, ptxOut'datum=prevNodeUTxODatum} <- pmatch $ ptxInInfoResolved (pfromData $ patRefUTxOIdx # pfromData notExistNodeIdx)
                        POutputDatum prevNodeDat' <- pmatch prevNodeUTxODatum
                        PDirectorySetNode {pkey=nodeDatumKey, pnext=nodeDatumNext} <- pmatch (pfromData $ punsafeCoerce @(PAsData PDirectorySetNode) (pto prevNodeDat'))
                        currCS <- plet $ pasByteStr # pforgetData (pfstBuiltin # csPair)
                        nodeKey <- plet $ pasByteStr # pforgetData nodeDatumKey
                        nodeNext <- plet $ pasByteStr # pforgetData nodeDatumNext
                        let checks =
                              pand'List
                                [
                                -- the currency symbol is not in the directory
                                ptraceInfoIfFalse "dir neg-proof node must cover" $ nodeKey #< currCS
                                , ptraceInfoIfFalse "dir neg-proof node must cover" $ currCS #< nodeNext
                                -- both directory entries are legitimate, this is proven by the
                                -- presence of the directory node currency symbol.
                                , ptraceInfoIfFalse "invalid dir node n" $ phasDataCS # directoryNodeCS # pfromData prevNodeUTxOValue
                                ]
                        pif checks
                            (self # (ptail # proofs) # csPairs # actualProgrammableTokenValue)
                            perror
                )
                (pcon $ PValue $ pcon $ PMap actualProgrammableTokenValue)
                inputInnerValue
    -- drop the ada entry in the value before traversing the rest of the value entries
    in go # proofList # (ptail # mapInnerList) # pto (pto pemptyLedgerValue)


data ProgrammableLogicGlobalRedeemer
  = TransferAct [TokenProof]
  | SeizeAct {
        plgrDirectoryNodeIdx :: Integer
      , plgrInputIdxs :: [Integer]
      , plgrOutputsStartIdx :: Integer
      , plgrLengthInputIdxs :: Integer
      }
  deriving (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed ''ProgrammableLogicGlobalRedeemer
  [('TransferAct, 0), ('SeizeAct, 1)]

data PProgrammableLogicGlobalRedeemer (s :: S)
  = PTransferAct {pproofs :: Term s (PAsData (PBuiltinList (PAsData PTokenProof)))}
  -- The proofs are the list of proofs that the token exists in the directory
  | PSeizeAct
      { pdirectoryNodeIdx :: Term s (PAsData PInteger)
      , pinputIdxs :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
      , poutputsStartIdx :: Term s (PAsData PInteger)
      , plengthInputIdxs :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PProgrammableLogicGlobalRedeemer)

deriving via DeriveDataPLiftable PProgrammableLogicGlobalRedeemer ProgrammableLogicGlobalRedeemer
  instance PLiftable PProgrammableLogicGlobalRedeemer

mkProgrammableLogicGlobal :: Term s (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
mkProgrammableLogicGlobal = plam $ \protocolParamsCS ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <- pmatch ctx
  PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'signatories, ptxInfo'wdrl} <- pmatch pscriptContext'txInfo
  let red = pfromData $ punsafeCoerce @(PAsData PProgrammableLogicGlobalRedeemer) (pto pscriptContext'redeemer)
  referenceInputs <- plet $ pfromData ptxInfo'referenceInputs

  -- Extract protocol parameter UTxO
  ptraceInfo "Extracting protocol parameter UTxO"

  let paramUTxO =
        ptxInInfoResolved $ pfromData $
          pmustFind @PBuiltinList
            # plam (\txIn ->
                    let resolvedIn = ptxInInfoResolved $ pfromData txIn
                    in phasDataCS # protocolParamsCS # pfromData (ptxOutValue resolvedIn)
                  )
            # referenceInputs

  POutputDatum paramDat' <- pmatch $ ptxOutDatum paramUTxO
  PProgrammableLogicGlobalParams {pdirectoryNodeCS, pprogLogicCred} <- pmatch (pfromData $ punsafeCoerce @(PAsData PProgrammableLogicGlobalParams) (pto paramDat'))
  progLogicCred <- plet $ pfromData pprogLogicCred

  ptraceInfo "Extracting invoked scripts"
  invokedScripts <- plet $
        pmap @PBuiltinList
          # plam (\wdrlPair ->
                    let cred = pfstBuiltin # wdrlPair
                    in punsafeCoerce @(PAsData PByteString) $ phead #$ psndBuiltin #$ pasConstr # pforgetData cred
                )
          # pto (pfromData ptxInfo'wdrl)

  pmatch red $ \case
    PTransferAct proofs -> P.do
      -- TODO: Consider minted values, right now minted value can be smuggled out of the programmable assets mini-ledger.
      totalProgTokenValue <-
        plet $ pvalueFromCred
                # progLogicCred
                # pfromData ptxInfo'signatories
                # invokedScripts
                # pfromData ptxInfo'inputs
      totalProgTokenValue_ <-
        plet $ pcheckTransferLogicAndGetProgrammableValue
                # pdirectoryNodeCS
                # referenceInputs
                # pfromData proofs
                # invokedScripts
                # totalProgTokenValue

      pvalidateConditions
          [ pisRewardingScript (pdata pscriptContext'scriptInfo)
          , ptraceInfoIfFalse "prog tokens escape" $
              pvalueContains # (pvalueToCred # progLogicCred # pfromData ptxInfo'outputs) # totalProgTokenValue_
          ]
    PSeizeAct {pdirectoryNodeIdx, pinputIdxs, poutputsStartIdx, plengthInputIdxs} -> P.do
      inputIdxs <- plet $ pmap @PBuiltinList @(PAsData PInteger) # plam pfromData # pfromData pinputIdxs
      let remainingOutputs = pdropFast # pfromData poutputsStartIdx # pfromData ptxInfo'outputs
      let directoryNodeUTxO = pelemAtFast @PBuiltinList # referenceInputs # pfromData pdirectoryNodeIdx
      PTxOut {ptxOut'value=seizeDirectoryNodeValue, ptxOut'datum=seizeDirectoryNodeDatum} <- pmatch (ptxInInfoResolved $ pfromData directoryNodeUTxO)
      POutputDatum seizeDat' <- pmatch seizeDirectoryNodeDatum
      PDirectorySetNode
        { pkey=directoryNodeDatumFKey
        , pissuerLogicScript=directoryNodeDatumFIssuerLogicScript
        } <- pmatch (pfromData $ punsafeCoerce @(PAsData PDirectorySetNode) (pto seizeDat'))
      let issuerLogicScriptHash = punsafeCoerce @(PAsData PByteString) $ phead #$ psndBuiltin #$ pasConstr # pforgetData directoryNodeDatumFIssuerLogicScript
      let conditions =
              [ ptraceInfoIfFalse "mini-ledger invariants violated" $ processThirdPartyTransfer # directoryNodeDatumFKey # progLogicCred # pfromData ptxInfo'inputs # remainingOutputs # inputIdxs
              , ptraceInfoIfFalse "issuer logic script must be invoked" $ pelem # issuerLogicScriptHash # invokedScripts
              -- directory node is valid (presence of state token)
              , ptraceInfoIfFalse "directory node is not valid" $ phasDataCS # pdirectoryNodeCS # pfromData seizeDirectoryNodeValue
              -- input indexes are unique.
              , ptraceInfoIfFalse "input indexes are not unique" $ pisUniqueSet # pfromData plengthInputIdxs # inputIdxs
              ]
      pvalidateConditions conditions

punionTokens :: Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
punionTokens = pfix #$ plam $ \self tokensA tokensB ->
  pelimList
    (\tokenPairA tokensRestA ->
      plet (pfstBuiltin # tokenPairA) $ \tokenNameA ->
      pelimList
        (\tokenPairB tokensRestB ->
           pif (pfromData tokenNameA #== pfromData (pfstBuiltin # tokenPairB))
               ( -- both entries have the same token so we add quantities
                let quantityA = pfromData (psndBuiltin # tokenPairA)
                    quantityB = pfromData (psndBuiltin # tokenPairB)
                in pcons # (ppairDataBuiltin # tokenNameA # pdata (quantityA + quantityB)) # (self # tokensRestA # tokensRestB)
               )
               (
                pif (pfromData tokenNameA #< pfromData (pfstBuiltin # tokenPairB))
                    -- entry A has a token that entry B does not so we add the token and quantity from entry A.
                    (pcons # tokenPairA # (self # tokensRestA # tokensB))
                    -- entry B has a token that entry A does not so we add the token and quantity from entry B.
                    (pcons # tokenPairB # (self # tokensA # tokensRestB))
               )
        )
        pnil tokensB
    )
    pnil tokensA

processThirdPartyTransfer :: Term s
  (PAsData PCurrencySymbol
  :--> PCredential
  :--> PBuiltinList (PAsData PTxInInfo)
  :--> PBuiltinList (PAsData PTxOut)
  :--> PBuiltinList PInteger
  :--> PBool)
processThirdPartyTransfer = plam $ \programmableCS progLogicCred inputs progOutputs inputIdxs' ->
    plet (pvalueEqualsDeltaCurrencySymbol # pfromData programmableCS) $ \pvalueEqualsDeltaCurrencySymbol' ->
    plet (pelemAtFast @PBuiltinList # inputs) $ \patInputIdx ->
      let
          checkBalanceInvariant :: Term _ (PBuiltinList (PAsData PTxOut)) -> Term _ (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))) -> Term _ PBool
          checkBalanceInvariant remainingOutputs deltaAccumulatorResult =
            let outputAccumulatorResult = go2 # remainingOutputs
                deltaResultValue = punsafeCoerce @(PValue 'Sorted 'Positive) (pconsBuiltin # (ppairDataBuiltinRaw # pforgetData programmableCS # (pmapData # punsafeCoerce deltaAccumulatorResult)) # pnil)
            in pif (pvalueContains # outputAccumulatorResult # deltaResultValue)
                   (pconstant True)
                   perror

          go2 :: Term _ (PBuiltinList (PAsData PTxOut) :--> PValue 'Sorted 'Positive)
          go2 = pfix #$ plam $ \self programmableOutputs ->
            pelimList
              (\programmableOutput programmableOutputsRest ->
                pmatch (pfromData programmableOutput) $ \(PTxOut {ptxOut'address=programmableOutputAddress, ptxOut'value=programmableOutputValue}) ->
                  pif ( paddressCredential programmableOutputAddress #== progLogicCred )
                      ( pfromData programmableOutputValue #<> (self # programmableOutputsRest) )
                      pmempty
              )
              pmempty
              programmableOutputs

          go :: Term _ (PBuiltinList PInteger :--> PBuiltinList (PAsData PTxOut) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBool)
          go = pfix #$ plam $ \self inputIdxs programmableOutputs deltaAccumulator ->
                pelimList
                  (\txInIdx txInsIdxs ->
                    plet (patInputIdx # txInIdx) $ \programmableInput ->
                      pmatch (ptxInInfoResolved $ pfromData programmableInput) $ \(PTxOut {ptxOut'address=programmableInputAddress, ptxOut'value=programmableInputValue}) ->
                        pmatch (pfromData $ phead # programmableOutputs) $ \(PTxOut {ptxOut'address=programmableOutputAddress, ptxOut'value=programmableOutputValue}) ->
                          let conditions =
                                pand'List
                                  [ -- this credential check is likely not necessary as the fact that the value has a programmable CS is proof that that input must come from the programmable logic base script.
                                    -- otherwise that means programmable tokens have been smuggled out of the programmable assets mini-ledger.
                                    ptraceInfoIfFalse "input payment credential is not the programmable logic credential" $ paddressCredential programmableInputAddress #== progLogicCred
                                  , ptraceInfoIfFalse "corresponding output: address mismatch" $ programmableOutputAddress #== programmableInputAddress
                                  ]
                           in pif conditions
                              (
                                let delta = pvalueEqualsDeltaCurrencySymbol' # programmableInputValue # programmableOutputValue
                                in self # txInsIdxs # (ptail # programmableOutputs) # (punionTokens # delta # deltaAccumulator)
                              )
                              perror
                  )
                  (checkBalanceInvariant programmableOutputs deltaAccumulator)
                  inputIdxs
      in go # inputIdxs' # progOutputs # pnil


-------------------------------------------------------------------------------
-- Corresponding inputs and outputs from and to the programmable token spending script (mini-ledger where all programmable tokens live).
-- Example Inputs:
-- inputA = {
--   progCS: { Foo: 120, Bar: 80 },
--   ADA: { "": 3_000_000 },
--   usdCS: { USDT: 50 },
--   nftCS: { ArtNFT: 1 }
-- }

-- inputB = {
--   progCS: { Foo: 70 },
--   ADA: { "": 2_000_000 },
--   usdCS: { USDT: 10 }
-- }

-- inputC = {
--   progCS: { Foo: 40, Bar: 30 },
--   ADA: { "": 1_500_000 }
-- }

-------------------------------------------------------------------------------
-- Corresponding Outputs:
-- Corresponding outputs are the continuing outputs for their corresponding inputs. They must have the same address as their input (as indicated by their label as continuing outputs)
-- they must also have the same datum and the same reference script hash (if present) as their input.
-- Finally, they must also have the same value as their input except for the balance of tokens with the progCS currency symbol, for tokens of that currency symbol.

-- Example outputs:
-- correspondingOutputA = {
--   progCS: { Foo: 140, Bar: 60 },
--   ADA: { "": 3_000_000 },
--   usdCS: { USDT: 50 },
--   nftCS: { ArtNFT: 1 }
-- }

-- correspondingOutputB = {
--   progCS: { Foo: 20 },
--   ADA: { "": 2_000_000 },
--   usdCS: { USDT: 10 }
-- }

-- correspondingOutputC = {
--   progCS: { Foo: 10, Bar: 10 },
--   ADA: { "": 1_500_000 }
-- }

-------------------------------------------------------------------------------
-- Remaining outputs:
-- Remaining programmable token outputs - these are outputs to the programmable token spending script (mini-ledger where all programmable tokens live)
-- that are not corresponding to any inputs to the programmable token spending script. The accumulated value of these outputs must contain
-- the delta between the amount of programmable asset in the inputs and the amount of programmable asset in the corresponding outputs thus assuring all
-- programmable assets must stay within the programmable token spending script.
-- Example remaining outputs:
-- remainingOutputA = {
--   progCS: { Foo: 40, Bar: 25 },
--   ADA: { "": 2_000_000 }
-- }

-- remainingOutputB = {
--   progCS: { Foo: 20, Bar: 15 },
--   ADA: { "": 2_000_000 }
-- }

-------------------------------------------------------------------------------
-- The below calculation checks that the total amount of programmable tokens spent from the script is equal to the amount sent to the script,
-- and that each correspondingOutput is equal to it's input except for the balance of tokens with the progCS currency symbol, for tokens of that currency symbol
-- each corresponding output contains either more or less than the amount of the tokens in the input.

-- accumulatedValue = amount of programmable asset in input - amount of programmable asset in corresponding output

-- outputValueAccumulator = emptyValue
-- if accumulatedValue > 0
--   for each remainingOutput:
--     outputValueAccumulator = outputValueAccumulator <> remainingOutputValue

-- if (valueContains outputValueAccumulator accumulatedValue)
--    constant True

-- | Negates the quantity of each token in a list of token quantity pairs (ie. the inner map of a `PValue`).
-- Example:
-- pnegateTokens [("FooToken", 10), ("BarToken", 20)] = [("FooToken", -10), ("BarToken", -20)]
pnegateTokens :: Term _ (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
pnegateTokens = pfix #$ plam $ \self tokens ->
  pelimList
    (\tokenPair tokensRest ->
      let tokenName = pfstBuiltin # tokenPair
          tokenAmount = psndBuiltin # tokenPair
      in pcons # (ppairDataBuiltin # tokenName # pdata (pconstantInteger 0 - pfromData tokenAmount)) # (self # tokensRest)
    )
    pnil
    tokens

-- |
-- `pvalueEqualsDeltaCurrencySymbol # progCS # inputUTxOValue # outputUTxOValue` MUST check that inputUTxOValue is equal to outputUTxOValue for all tokens except those of currency symbol progCS.
-- The function should return a value consisting of only tokens with the currency symbol progCS, this value is as follows: For each token t of currency symbol progCS, the quantity of the token
-- in the return value rValue is the quantity of token t in inputUTxOValue minus the quantity of token t in outputUTxOValue.
-- for the purposes of the subtraction ie. if inputUTxOValue has 0 FooToken and outputUTxOValue has 10 FooToken then rValue should have 0 - 10 = -10 FooToken.
--
pvalueEqualsDeltaCurrencySymbol ::
  forall anyOrder anyAmount s.
  Term
    s
    ( PCurrencySymbol
        :--> PAsData (PValue anyOrder anyAmount)
        :--> PAsData (PValue anyOrder anyAmount)
        :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
    )
pvalueEqualsDeltaCurrencySymbol = plam $ \progCS inputUTxOValue outputUTxOValue ->
  let innerInputValue :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap anyOrder PTokenName PInteger))))
      innerInputValue = pto (pto $ pfromData inputUTxOValue)
      innerOutputValue :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap anyOrder PTokenName PInteger))))
      innerOutputValue = pto (pto $ pfromData outputUTxOValue)

      psubtractTokens ::
        Term _ (
          PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
            :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
            :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
        )
      psubtractTokens =
        pfix #$ plam $ \self inputTokens outputTokens ->
          pelimList
            (\inputPair inputRest ->
              plet (pfstBuiltin # inputPair) $ \inputTokenName ->
              let inputTokenAmount = psndBuiltin # inputPair
              in pelimList
                  (\outputPair outputRest ->
                    let outputTokenName   = pfstBuiltin # outputPair
                        outputTokenAmount = psndBuiltin # outputPair
                    in
                    pif (pfromData inputTokenName #<= pfromData outputTokenName)
                        ( -- inputTokenName <= outputTokenName
                          pif (inputTokenName #== outputTokenName)
                            ( -- names equal → diff = input − output; skip if zero
                              let diff = pfromData inputTokenAmount - pfromData outputTokenAmount
                              in pif (diff #== 0)
                                    (self # inputRest # outputRest)
                                    (pcons
                                        # (ppairDataBuiltin # inputTokenName # pdata diff)
                                        # (self # inputRest # outputRest))
                            )
                            ( -- outputTokenName > inputTokenName → token only in input (nonzero by invariant)
                              let diff = pfromData inputTokenAmount
                              in pcons
                                    # (ppairDataBuiltin # inputTokenName # pdata diff)
                                    # (self # inputRest # outputTokens)
                            )
                        )
                        ( -- outputTokenName < inputTokenName → token only in output (nonzero by invariant)
                          let diff = pconstantInteger 0 - pfromData outputTokenAmount
                          in pcons
                                # (ppairDataBuiltin # outputTokenName # pdata diff)
                                # (self # inputTokens # outputRest)
                        )
                  )
                  -- output exhausted → emit remaining input tokens as positive (nonzero by invariant)
                  inputRest
                  outputTokens
            )
            -- input exhausted → emit remaining output tokens as negative (nonzero by invariant)
            (pnegateTokens # outputTokens)
            inputTokens

      -- no need to check for progCs in "everything should be same" parts
      -- input  : |- everything should be same -| |-progCs-| |-everything should be same-|
      -- output : |- everything should be same -| |-progCs-| |-everything should be same-|
      goOuter ::
        Term _
          ( PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap anyOrder PTokenName PInteger)))
              :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap anyOrder PTokenName PInteger)))
              :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))  -- accumulator (delta for progCS)
              :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
          )
      goOuter = pfix #$ plam $ \self inputValuePairs outputValuePairs diffAccumulator ->
                  pelimList
                    (\inputValueEntry inputValueEntries ->
                      plet (pfstBuiltin # inputValueEntry) $ \inputValueEntryCS ->
                      pelimList
                        (\outputValueEntry outputValueEntries ->
                            pif (pfromData inputValueEntryCS #== pfromData (pfstBuiltin # outputValueEntry))
                              (pif (pfromData inputValueEntryCS #== progCS)
                                  (pif  (pmapData # punsafeCoerce outputValueEntries #== pmapData # punsafeCoerce inputValueEntries)
                                        (psubtractTokens # pto (pfromData (psndBuiltin # inputValueEntry)) # pto (pfromData @(PMap anyOrder PTokenName PInteger) (psndBuiltin # outputValueEntry)))
                                        perror
                                  )
                                  (pif (psndBuiltin # inputValueEntry #== psndBuiltin # outputValueEntry) (self # inputValueEntries # outputValueEntries # diffAccumulator) perror)
                              )
                              (pif (psndBuiltin # inputValueEntry #== psndBuiltin # outputValueEntry) diffAccumulator perror)

                        ) pnil outputValuePairs
                    ) pnil inputValuePairs
   in goOuter # innerInputValue # innerOutputValue # pnil
