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
import Plutarch.Core.Context (paddressCredential, pscriptContextTxInfo,
                              ptxInInfoResolved, ptxOutDatum, ptxOutValue)
import Plutarch.Core.Integrity (pisRewardingScript)
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

pconstructExpectedOutputWithOutputDatum :: Term s PAddress -> Term s (PAsData (PValue 'Sorted 'Positive)) -> Term s POutputDatum -> Term s (PAsData PTxOut)
pconstructExpectedOutputWithOutputDatum address value datum =
  pdata $ pcon $
    PTxOut
     { ptxOut'address = address
     , ptxOut'value = value
     , ptxOut'datum = datum
     , ptxOut'referenceScript = pconstant Nothing
     }


-- TODO:
-- The current implementation of the contracts in this module are not designed to be maximally efficient.
-- In the future, this should be optimized to use the redeemer indexing design pattern to identify and validate
-- the programmable inputs.
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
        plgrSeizeInputIdx :: Integer,
        plgrSeizeOutputIdx :: Integer,
        plgrDirectoryNodeIdx :: Integer
      }
  deriving (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed ''ProgrammableLogicGlobalRedeemer
  [('TransferAct, 0), ('SeizeAct, 1)]

data PProgrammableLogicGlobalRedeemer (s :: S)
  = PTransferAct {pproofs :: Term s (PAsData (PBuiltinList (PAsData PTokenProof)))}
  | PSeizeAct
      { pseizeInputIdx :: Term s (PAsData PInteger)
      , pseizeOutputIdx :: Term s (PAsData PInteger)
      , pdirectoryNodeIdx :: Term s (PAsData PInteger)
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
    PSeizeAct {pseizeInputIdx, pseizeOutputIdx, pdirectoryNodeIdx} -> P.do
      -- TODO:
      -- Possibly enforce that the seized assets must be seized to the programmable logic contract
      -- just under different ownership (staking credential changed)
      ptraceInfo "PSeizeAct"
      txInputs <- plet $ pfromData ptxInfo'inputs
      let seizeInput = ptxInInfoResolved $ pfromData (pelemAtFast @PBuiltinList # txInputs # pfromData pseizeInputIdx)
          seizeOutput = pelemAtFast @PBuiltinList # pfromData ptxInfo'outputs # pfromData pseizeOutputIdx
          directoryNodeUTxO = pelemAtFast @PBuiltinList # referenceInputs # pfromData pdirectoryNodeIdx
      PTxOut {ptxOut'value=seizeDirectoryNodeValue, ptxOut'datum=seizeDirectoryNodeDatum} <- pmatch (ptxInInfoResolved $ pfromData directoryNodeUTxO)
      POutputDatum seizeDat' <- pmatch seizeDirectoryNodeDatum
      PDirectorySetNode
        { pkey=directoryNodeDatumFKey
        , pissuerLogicScript=directoryNodeDatumFIssuerLogicScript
        } <- pmatch (pfromData $ punsafeCoerce @(PAsData PDirectorySetNode) (pto seizeDat'))
      PTxOut{ptxOut'address=seizeInputAddress', ptxOut'value=seizeInputValue', ptxOut'datum=seizeInputDatum} <- pmatch seizeInput
      seizeInputAddress <- plet seizeInputAddress'

      seizeInputValue <- plet $ pfromData seizeInputValue'
      expectedSeizeOutputValue <- plet $ pfilterCSFromValue # seizeInputValue # directoryNodeDatumFKey

      let expectedSeizeOutput =
           pconstructExpectedOutputWithOutputDatum
              seizeInputAddress
              (pdata expectedSeizeOutputValue)
              seizeInputDatum

      -- For ease of implementation of POC we only allow one UTxO to be seized per transaction.
      -- This can be easily modified to support seizure of multiple UTxOs.
      let issuerLogicScriptHash = punsafeCoerce @(PAsData PByteString) $ phead #$ psndBuiltin #$ pasConstr # pforgetData directoryNodeDatumFIssuerLogicScript
      pvalidateConditions
          [ pcountInputsFromCred # progLogicCred # txInputs #== pconstant 1
          , paddressCredential seizeInputAddress #== progLogicCred
          , seizeOutput #== expectedSeizeOutput
          , pelem # issuerLogicScriptHash # invokedScripts
          -- Prevent DDOS greifing attacks via the seize action
          -- i.e. the issuer logic script being used to spend a programmable token UTxO that does not have the given programmable token
          -- back to the mkProgrammableLogicBase script without modifying it (thus preventing any others from spending
          -- that UTxO in that block). Or using it to repeatedly spend a programmable token UTxO that does have the programmable token back back to
          -- the mkProgrammableLogicBase script without removing the programmable token associated with the `issuerLogicCredential`.
          , pnot # (pdata seizeInputValue #== pdata expectedSeizeOutputValue)
          -- seize node is valid (presence of state token)
          , phasDataCS # pdirectoryNodeCS # pfromData seizeDirectoryNodeValue
          ]



