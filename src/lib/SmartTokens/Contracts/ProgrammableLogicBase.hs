module SmartTokens.Contracts.ProgrammableLogicBase (
  mkProgrammableLogicBase,
  mkProgrammableLogicGlobal
) where

import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Builtin
import SmartTokens.Core.Utils
import Plutarch.Unsafe
import PlutusLedgerApi.V1.Value
import SmartTokens.Types.ProtocolParams (PProgrammableLogicGlobalParams)
import Plutarch.Extra.Record
import SmartTokens.Types.PTokenDirectory
-- import Plutarch.List

data PTokenProof (s :: S)
  = PTokenExists
      ( Term s ( PDataRecord '[ "nodeIdx" ':= PInteger ] ) )
  | PTokenDoesNotExist
      ( Term
          s
          ( PDataRecord
              '[ "nodeIdx" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PTokenProof where
  type DPTStrat _ = PlutusTypeData

emptyValue :: Value
emptyValue = mempty

pvalueFromCred :: Term s (PAsData PCredential :--> PBuiltinList (PAsData PPubKeyHash) :--> PBuiltinList (PAsData PByteString) :--> PBuiltinList (PAsData PTxInInfo) :--> PValue 'Sorted 'Positive)
pvalueFromCred = phoistAcyclic $ plam $ \cred sigs scripts inputs ->
  (pfix #$ plam $ \self acc ->
    pelimList
      (\txIn xs ->
        self
          # pletFields @'["address", "value"] (pfield @"resolved" # txIn) (\txInF ->
              plet txInF.address $ \addr ->
                pif (pfield @"credential" # addr #== cred)
                    (
                      pmatch (pfield @"stakingCredential" # addr) $ \case
                        PDJust ((pfield @"_0" #) -> stakingCred) ->
                          pmatch stakingCred $ \case
                            PStakingHash ((pfield @"_0" #) -> ownerCred) ->
                              pmatch ownerCred $ \case
                                PPubKeyCredential ((pfield @"_0" #) -> pkh) ->
                                  pif (ptxSignedByPkh # pkh # sigs)
                                      (acc <> pfromData txInF.value)
                                      perror
                                PScriptCredential ((pfield @"_0" #) -> scriptHash) ->
                                  pif (pelem # punsafeCoerce scriptHash # scripts)
                                      (acc <> pfromData txInF.value)
                                      perror
                            _ -> perror
                        PDNothing _ -> perror
                    )
                    acc
                    )
          # xs
      )
      acc
  )
  # punsafeCoerce (pconstant emptyValue)
  # inputs

pvalueToCred :: Term s (PAsData PCredential :--> PBuiltinList (PAsData PTxOut) :--> PValue 'Sorted 'Positive)
pvalueToCred = phoistAcyclic $ plam $ \cred inputs ->
  (pfix #$ plam $ \self acc ->
    pelimList
      (\txOut xs ->
        self
          # pletFields @'["address", "value"] txOut (\txOutF ->
              plet txOutF.address $ \addr ->
                pif (pfield @"credential" # addr #== cred)
                    (acc <> pfromData txOutF.value)
                    acc
                    )
          # xs
      )
      acc
  )
  # punsafeCoerce (pconstant emptyValue)
  # inputs

-- | Programmable logic base
-- This validator forwards its validation logic to the programmable logic stake script
-- using the withdraw-zero design pattern. 
mkProgrammableLogicBase :: ClosedTerm (PAsData PCredential :--> PScriptContext :--> PUnit)
mkProgrammableLogicBase = plam $ \stakeCred ctx ->
  let wdrls :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace)))
      wdrls = pto $ pfromData $ pfield @"wdrl" # (pfield @"txInfo" # ctx)
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
      in pvalidateConditions [hasCred]

pcheckTransferLogic :: Term s (PAsData PCurrencySymbol :--> PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PAsData PTokenProof) :--> PBuiltinList (PAsData PByteString) :--> PValue 'Sorted 'Positive :--> PBool)
pcheckTransferLogic = plam $ \directoryNodeCS refInputs proofList scripts totalValue ->
  plet (pelemAtFast @PBuiltinList # refInputs) $ \patRefUTxOIdx ->
    let mapInnerList = pto (pto totalValue)
        go = pfix #$ plam $ \self proofs innerValue ->
              pelimList
                (\csPair csPairs ->
                  let cs :: Term _ (PAsData PByteString)
                      cs = punsafeCoerce $ pfstBuiltin # csPair
                  in
                    pmatch (pfromData $ phead # proofs) $ \case
                      PTokenExists ((pfield @"nodeIdx" #) -> nodeIdx) -> P.do
                        directoryNodeUTxOF <- pletFields @'["value", "datum"] $ pfield @"resolved" # (patRefUTxOIdx # pfromData nodeIdx)
                        POutputDatum ((pfield @"outputDatum" #) -> paramDat') <- pmatch directoryNodeUTxOF.datum
                        directoryNodeDatumF <- pletFields @'["key", "next", "transferLogicScript"] (punsafeCoerce @_ @_ @PDirectorySetNode (pto paramDat'))
                        let transferLogicScriptHash = punsafeCoerce @_ @_ @(PAsData PByteString) $ phead #$ psndBuiltin #$ pasConstr # pforgetData directoryNodeDatumF.transferLogicScript
                        -- validate that the directory entry for the currency symbol is referenced by the proof 
                        -- and that the associated transfer logic script is executed in the transaction
                        let checks =
                              pand'List
                                [ pelem # transferLogicScriptHash # scripts
                                , punsafeCoerce directoryNodeDatumF.key #== cs
                                , phasDataCS # directoryNodeCS # pfromData directoryNodeUTxOF.value
                                ]
                        pif checks
                            (self # (ptail # proofs) # csPairs)
                            perror
                      PTokenDoesNotExist notExist -> P.do
                        notExistF <- pletFields @'["nodeIdx"] notExist
                        prevNodeUTxOF <- pletFields @'["value", "datum"] $ pfield @"resolved" # (patRefUTxOIdx # pfromData notExistF.nodeIdx)
                        POutputDatum ((pfield @"outputDatum" #) -> prevNodeDat') <- pmatch prevNodeUTxOF.datum
                        nodeDatumF <- pletFields @'["key", "next"] (punsafeCoerce @_ @_ @PDirectorySetNode (pto prevNodeDat'))
                        currCS <- plet $ pasByteStr # pforgetData (pfstBuiltin # csPair)
                        nodeKey <- plet $ pasByteStr # pforgetData nodeDatumF.key
                        nodeNext <- plet $ pasByteStr # pforgetData nodeDatumF.next
                        let checks =
                              pand'List
                                [
                                -- the currency symbol is not in the directory
                                nodeKey #< currCS
                                , currCS #< nodeNext #|| nodeNext #== pconstant ""
                                -- both directory entries are legitimate, this is proven by the 
                                -- presence of the directory node currency symbol.
                                , phasDataCS # directoryNodeCS # pfromData prevNodeUTxOF.value 
                                ]
                        pif checks
                            (self # (ptail # proofs) # csPairs)
                            perror
                )
                (pconstant True)
                innerValue
    in go # proofList # (ptail # mapInnerList)

-- type ProgrammableLogicGlobalRedeemer = PBuiltinList (PAsData PTokenProof)

data ProgrammableLogicGlobalRedeemer (s :: S)
  = PTransferAct
      ( Term s ( PDataRecord '[ "proofs" ':= PBuiltinList (PAsData PTokenProof) ] ) )
  | PSeizeAct
      ( Term
          s
          ( PDataRecord
              '[ "seizeInputIdx" ':= PInteger
               , "seizeOutputIdx" ':= PInteger
               , "directoryNodeIdx" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType ProgrammableLogicGlobalRedeemer where
  type DPTStrat _ = PlutusTypeData

mkProgrammableLogicGlobal :: ClosedTerm (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
mkProgrammableLogicGlobal = plam $ \protocolParamsCS ctx -> P.do
  ctxF <- pletFields @'["txInfo", "redeemer", "scriptInfo"] ctx
  infoF <- pletFields @'["inputs", "referenceInputs", "outputs", "signatories", "wdrl"] ctxF.txInfo
  let red = pfromData $ punsafeCoerce @_ @_ @(PAsData ProgrammableLogicGlobalRedeemer) (pto ctxF.redeemer)
  referenceInputs <- plet $ pfromData infoF.referenceInputs
  -- Extract protocol parameter UTxO
  ptraceInfo "Extracting protocol parameter UTxO"
  let paramUTxO =
        pfield @"resolved" #$
          pmustFind @PBuiltinList
            # plam (\txIn ->
                    let resolvedIn = pfield @"resolved" # txIn
                    in phasDataCS # protocolParamsCS # (pfield @"value" # resolvedIn)
                  )
            # referenceInputs

  POutputDatum ((pfield @"outputDatum" #) -> paramDat') <- pmatch $ pfield @"datum" # paramUTxO
  protocolParamsF <- pletFields @'["directoryNodeCS", "progLogicCred"] (punsafeCoerce @_ @_ @PProgrammableLogicGlobalParams (pto paramDat'))
  progLogicCred <- plet protocolParamsF.progLogicCred
  
  
  ptraceInfo "Extracting invoked scripts"
  let invokedScripts =
        pmap @PBuiltinList
          # plam (\wdrlPair ->
                    let cred = pfstBuiltin # wdrlPair
                    in punsafeCoerce @_ @_ @(PAsData PByteString) $ phead #$ psndBuiltin #$ pasConstr # pforgetData cred
                )
          # pto (pfromData infoF.wdrl)

  pmatch red $ \case
    PTransferAct ((pfield @"proofs" #) -> proofs) -> P.do 
      ptraceInfo "PTransferAct valueFromCred"
      totalProgTokenValue <- 
        plet $ pvalueFromCred
                # progLogicCred
                # infoF.signatories
                # invokedScripts
                # infoF.inputs
      ptraceInfo "PTransferAct validateConditions"
      pvalidateConditions
          [ pisRewarding ctxF.scriptInfo
          , pcheckTransferLogic 
              # protocolParamsF.directoryNodeCS 
              # referenceInputs 
              # pfromData proofs 
              # invokedScripts 
              # totalProgTokenValue
          -- For POC we enforce that all value spent from the programmable contracts must 
          -- return to the programmable contracts. We can easily extend this to allow 
          -- for non-programmable tokens to leave the programmable contract.
          , pvalueToCred # progLogicCred # infoF.outputs #== totalProgTokenValue
          ]
    PSeizeAct seizeAct -> P.do
      -- TODO:
      -- Possibly enforce that the seized assets must be seized to the programmable logic contract
      -- just under different ownership (staking credential changed)
      ptraceInfo "PSeizeAct"
      txInputs <- plet $ pfromData infoF.inputs
      seizeActF <- pletFields @'["seizeInputIdx", "seizeOutputIdx", "directoryNodeIdx"] seizeAct
      let seizeInput = pfield @"resolved" # (pelemAtFast @PBuiltinList # txInputs # seizeActF.seizeInputIdx)
          seizeOutput = pelemAtFast @PBuiltinList # infoF.outputs # seizeActF.seizeOutputIdx
          directoryNodeUTxO = pelemAtFast @PBuiltinList # referenceInputs # pfromData seizeActF.directoryNodeIdx
      seizeDirectoryNode <- pletFields @'["value", "datum"] (pfield @"resolved" # directoryNodeUTxO)
      POutputDatum ((pfield @"outputDatum" #) -> seizeDat') <- pmatch seizeDirectoryNode.datum
      directoryNodeDatumF <- pletFields @'["key", "next", "transferLogicScript", "issuerLogicScript"] (punsafeCoerce @_ @_ @PDirectorySetNode (pto seizeDat'))

      seizeInputF <- pletFields @'["address", "value", "datum"] seizeInput
      seizeInputAddress <- plet seizeInputF.address

      let expectedSeizeOutput =
            pdata $
              mkRecordConstr
                PTxOut
                ( #address
                    .= seizeInputF.address
                    .& #value
                    .= pdata (pfilterCSFromValue # pfromData seizeInputF.value # directoryNodeDatumF.key)
                    .& #datum
                    .= seizeInputF.datum
                    .& #referenceScript
                    .= pdata pdnothing
                )
      -- For ease of implementation of POC we only allow one UTxO to be seized per transaction. 
      -- This can be easily modified to support seizure of multiple UTxOs.
      let issuerLogicScriptHash = punsafeCoerce @_ @_ @(PAsData PByteString) $ phead #$ psndBuiltin #$ pasConstr # pforgetData directoryNodeDatumF.issuerLogicScript
      pvalidateConditions
          [ pcountInputsFromCred # progLogicCred # txInputs #== pconstant 1
          , pfield @"credential" # seizeInputAddress #== progLogicCred
          , seizeOutput #== expectedSeizeOutput
          , pelem # issuerLogicScriptHash # invokedScripts
          ]



