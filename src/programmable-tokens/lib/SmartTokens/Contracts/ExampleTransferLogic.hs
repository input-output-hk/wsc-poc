{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QualifiedDo          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE NamedFieldPuns       #-}

module SmartTokens.Contracts.ExampleTransferLogic (
  mkPermissionedTransfer,
  mkFreezeAndSeizeTransfer,
  BlacklistProof (..),
) where

import Generics.SOP qualified as SOP
import GHC.Generics (Generic)
import Plutarch.Core.Context
import Plutarch.Core.Integrity (pisRewardingScript)
import Plutarch.Core.List
import Plutarch.Core.Utils
import Plutarch.Core.ValidationLogic
import Plutarch.Core.Value
import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusTx qualified
import SmartTokens.Types.PTokenDirectory

-- >>> _printTerm $ unsafeEvalTerm NoTracing (pconstant $ NonmembershipProof 1)
-- "program 1.0.0 (Constr 0 [I 1])"
data BlacklistProof
  = NonmembershipProof Integer
  deriving stock (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed ''BlacklistProof
  [('NonmembershipProof, 0)]

-- >>> _printTerm $ unsafeEvalTerm NoTracing (mkRecordConstr PNonmembershipProof ( #nodeIdx .= pdata (pconstant 1)))
-- "program 1.0.0 (Constr 0 [I 1])"
data PBlacklistProof (s :: S)
  = PNonmembershipProof { pnodeIdx :: Term s (PAsData PInteger) }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via DeriveAsDataStruct PBlacklistProof

deriving via DeriveDataPLiftable PBlacklistProof BlacklistProof
  instance PLiftable PBlacklistProof
{-|
  The 'mkPermissionedTransfer' is a transfer logic script that enforces that all transactions which spend the
  associated programmable tokens must be signed by the specified permissioned credential.

  Parameters:
  - 'permissionedCred': The public key hash of the credential that is permitted to sign the transaction.
  - 'ctx': The transaction's script context.

  The script works as follows:
  1. Extracts the list of signatories from the transaction context.
  2. Validates that the transaction is signed by the specified permissioned credential.
  3. If the transaction is signed by the permissioned credential, the script succeeds. Otherwise, it fails.

  This ensures that only transactions signed by the specified permissioned credential can spend the associated programmable tokens.
-}
mkPermissionedTransfer :: ClosedTerm (PData :--> PAsData PPubKeyHash :--> PScriptContext :--> PUnit)
mkPermissionedTransfer = plam $ \_ permissionedCred ctx ->
  pvalidateConditions
    [ ptxSignedByPkh # permissionedCred # (pfromData . ptxInfoSignatories . pscriptContextTxInfo) ctx
    ]

{-|
  The 'pvalidateWitnesses' function validates that none of the transaction witnesses are in the blacklist.

  Parameters:
  - 'blacklistNodeCS': The currency symbol that identifies authentic blacklist nodes.
  - 'proofs': A list of blacklist proofs provided via the redeemer.
  - 'refInputs': A list of reference inputs in the transaction.
  - 'witnesses': A list of transaction witnesses.

  The function works as follows:
  1. Iterates over the list of witnesses and blacklist proofs.
  2. For each witness, it verifies the correctness of the corresponding proof.
  3. The proof must demonstrate that the associated witness is not in the blacklist.
  4. If all proofs are valid, the function returns 'True'. Otherwise, it returns 'False'.

  The function uses a recursive approach with 'pfix' and 'pelimList' to process the list of proofs and witnesses.
  It performs the following checks for each proof:
  - For 'PNonmembershipProof':
    - Ensures that the two nodes are adjacent in the blacklist.
    - Verifies that the witness key is not in the blacklist by checking that it is lexographically greater than the key of the
      first node and lexographically less than the key of the second node (and thus if it was in the blacklist those two nodes
      would not be adjacent).
    - Confirms the legitimacy of both directory entries by checking the presence of the directory node currency symbol.
  If any of the checks fail, the function throws an error.
-}
pvalidateWitnesses :: Term s (PAsData PCurrencySymbol :--> PBuiltinList (PAsData PBlacklistProof) :--> PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PAsData PByteString) :--> PBool)
pvalidateWitnesses = phoistAcyclic $ plam $ \blacklistNodeCS proofs refInputs witnesses ->
  plet (pelemAtFast @PBuiltinList # refInputs) $ \patRefUTxOIdx ->
    (pfix #$ plam $ \self remainingProofs txWits ->
      pelimList @PBuiltinList
        (\wit remainWits ->
          pmatch (pfromData $ phead # remainingProofs) $ \case
            PNonmembershipProof nonExistFNodeIdx -> P.do
              PTxOut {ptxOut'value=prevNodeVal, ptxOut'datum=prevNodeDat} <- pmatch $ ptxInInfoResolved $ pfromData (patRefUTxOIdx # pfromData nonExistFNodeIdx)
              POutputDatum prevNodeDat' <- pmatch prevNodeDat
              PBlacklistNode {pblnKey, pblnNext} <- pmatch $ pfromData $ punsafeCoerce @(PAsData PBlacklistNode) (pto prevNodeDat')
              witnessKey <- plet $ pasByteStr # pforgetData wit
              nodeKey <- plet $ pfromData pblnKey
              nodeNext <- plet $ pfromData pblnNext
              let checks =
                    pand'List
                      [
                      -- the witness is not in the blacklist
                      ptraceInfoIfFalse "witness is blacklisted" $ nodeKey #< witnessKey
                      , ptraceInfoIfFalse "witness is blacklisted" $ witnessKey #< nodeNext
                      -- directory entries are legitimate, this is proven by the
                      -- presence of the directory node currency symbol.
                      , ptraceInfoIfFalse "indexed invalid blacklist node" $ phasDataCS # blacklistNodeCS # pfromData prevNodeVal
                      ]
              pif checks
                  (self # (ptail # remainingProofs) # remainWits)
                  perror
        )
        (pconstant True)
        txWits
    ) # proofs # witnesses

pextractRequiredWitnesses :: Term s (PCredential :--> PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PAsData PByteString))
pextractRequiredWitnesses = phoistAcyclic $ plam $ \progBaseCred inputs ->
  (pfix #$ plam $ \self acc ->
    pelimList
      (\txIn xs ->
        self
          # plet (ptxOutAddress $ ptxInInfoResolved $ pfromData txIn) (\addr ->
                pif (paddressCredential addr #== progBaseCred)
                    (
                      let stakingCred = paddressStakingCredential addr
                          ownerCred = phead #$ psndBuiltin #$ pasConstr # pforgetData (pdata stakingCred)
                          credHash = punsafeCoerce @(PAsData PByteString) $ phead #$ psndBuiltin #$ pasConstr # ownerCred
                       in pcons # credHash # acc
                    )
                    acc
            )
          # xs
      )
      acc
  )
  # pnil
  # inputs

{-|
  The 'mkFreezeAndSeizeTransfer' is a transfer logic script that allows the associated programmable token
  to be frozen via a dynamic blacklist.

  Parameters:
  - 'blacklistNodeCS': The currency symbol that identifies authentic blacklist nodes.
  - 'ctx': The transaction's script context.

  The script works as follows:
  1. Constructs a list of transaction witnesses by iterating over the withdrawal list, extracting the script credentials and
     prepending them to the transaction required_signers set (txInfoSignatories).
  2. Enforces that the script is being invoked as a rewarding script (withdraw-zero trick)
  3. Validates that none of the tx witnesses are in the blacklist. This is done by iterating over the witnesses
     the blacklist proofs (provided via the redeemer) verifying the correctness of each proof
     (i.e. that the proof really does prove that the associated witness is not in the blacklist).
-}
mkFreezeAndSeizeTransfer :: ClosedTerm (PAsData PCredential :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
mkFreezeAndSeizeTransfer = plam $ \(pfromData -> programmableLogicBaseCred) blacklistNodeCS ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <- pmatch ctx
  PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs} <- pmatch pscriptContext'txInfo
  let red = pfromData $ punsafeCoerce @(PAsData (PBuiltinList (PAsData PBlacklistProof))) (pto pscriptContext'redeemer)
  let txWitnesses = pextractRequiredWitnesses # programmableLogicBaseCred # pfromData ptxInfo'inputs
  pvalidateConditions
    [ pisRewardingScript (pdata pscriptContext'scriptInfo)
    , pvalidateWitnesses # blacklistNodeCS # red # pfromData ptxInfo'referenceInputs # txWitnesses
    ]
