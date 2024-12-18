{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE QualifiedDo #-}
module SmartTokens.Contracts.ExampleTransferLogic (
  mkPermissionedTransfer,
  mkFreezeAndSeizeTransfer,
) where

import Plutarch.LedgerApi.V3
    ( PPubKeyHash,
      PCurrencySymbol,
      POutputDatum(POutputDatum),
      PScriptContext,
      PTxInInfo )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
    ( Generic,
      (#),
      (#$),
      perror,
      phoistAcyclic,
      plet,
      pfix,
      pto,
      pmatch,
      type (:-->),
      ClosedTerm,
      S,
      Term,
      plam,
      DerivePlutusType(..),
      PlutusType,
      pif,
      pfromData,
      pfstBuiltin,
      psndBuiltin,
      pfield,
      pletFields,
      pconstant,
      PBool,
      PEq(..),
      PPartialOrd((#<)),
      PAsData,
      PBuiltinList,
      PIsData,
      PByteString,
      PDataRecord,
      PLabeledType((:=)),
      PlutusTypeData,
      PInteger,
      PListLike(pcons, ptail, pelimList, phead),
      PUnit, (#||) )
import Plutarch.Builtin ( pasByteStr, pasConstr, pforgetData )
import Plutarch.Core.Utils
    ( pisRewarding,
      phasDataCS,
      pelemAtFast,
      ptxSignedByPkh,
      pand'List,
      pvalidateConditions )
import Plutarch.Unsafe ( punsafeCoerce )
import SmartTokens.Types.PTokenDirectory ( PBlacklistNode, pletFieldsBlacklistNode)

data PBlacklistProof (s :: S)
  = PNonmembershipProof
      ( Term
          s
          ( PDataRecord
              '[ "nodeIdx" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PBlacklistProof where
  type DPTStrat _ = PlutusTypeData

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
mkPermissionedTransfer :: ClosedTerm (PAsData PPubKeyHash :--> PScriptContext :--> PUnit)
mkPermissionedTransfer = plam $ \permissionedCred ctx ->
  pvalidateConditions
    [ ptxSignedByPkh # permissionedCred # (pfield @"signatories" # (pfield @"txInfo" # ctx))
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
  - For 'PNonmembershipProofTail':
    - Ensures that the witness key is greater than the tail node key in the blacklist.
    - Confirms the legitimacy of the directory entry by checking the presence of the directory node currency symbol.

  If any of the checks fail, the function throws an error.
-}
pvalidateWitnesses :: Term s (PAsData PCurrencySymbol :--> PBuiltinList (PAsData PBlacklistProof) :--> PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PAsData PByteString) :--> PBool)
pvalidateWitnesses = phoistAcyclic $ plam $ \blacklistNodeCS proofs refInputs witnesses ->
  plet (pelemAtFast @PBuiltinList # refInputs) $ \patRefUTxOIdx ->
    (pfix #$ plam $ \self remainingProofs txWits ->
      pelimList @PBuiltinList
        (\wit remainWits ->
          pmatch (pfromData $ phead # remainingProofs) $ \case
            PNonmembershipProof nonExist -> P.do
              notExistF <- pletFields @'["nodeIdx"] nonExist
              prevNodeUTxOF <- pletFields @'["value", "datum"] $ pfield @"resolved" # (patRefUTxOIdx # pfromData notExistF.nodeIdx)
              POutputDatum ((pfield @"outputDatum" #) -> prevNodeDat') <- pmatch prevNodeUTxOF.datum
              prevNodeDatumF <- pletFieldsBlacklistNode $ punsafeCoerce @_ @_ @(PAsData PBlacklistNode) (pto prevNodeDat')
              witnessKey <- plet $ pasByteStr # pforgetData wit
              nodeKey <- plet $ pfromData prevNodeDatumF.key
              nodeNext <- plet $ pfromData prevNodeDatumF.next -- pasByteStr # pforgetData (phead # (ptail # prevNodeDatumF))
              let checks =
                    pand'List
                      [
                      -- the currency symbol is not in the blacklist
                      nodeKey #< witnessKey
                      , witnessKey #< nodeNext #|| nodeNext #== pconstant ""
                      -- both directory entries are legitimate, this is proven by the 
                      -- presence of the directory node currency symbol.
                      , phasDataCS # blacklistNodeCS # pfromData prevNodeUTxOF.value
                      ]
              pif checks
                  (self # (ptail # remainingProofs) # remainWits)
                  perror
        )
        (pconstant True)
        txWits
    ) # proofs # witnesses


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
mkFreezeAndSeizeTransfer :: ClosedTerm (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
mkFreezeAndSeizeTransfer = plam $ \blacklistNodeCS ctx -> P.do
  ctxF <- pletFields @'["txInfo", "redeemer", "scriptInfo"] ctx
  infoF <- pletFields @'["inputs", "referenceInputs", "outputs", "signatories", "wdrl"] ctxF.txInfo
  let red = pfromData $ punsafeCoerce @_ @_ @(PAsData (PBuiltinList (PAsData PBlacklistProof))) (pto ctxF.redeemer)
  pkWits <- plet $ punsafeCoerce @_ @_ @(PBuiltinList (PAsData PByteString)) (pfromData infoF.signatories)
  let txWitnesses = pfix #
        plam (\self wdrlList ->
          pelimList @PBuiltinList
            (\wdrlPair rest ->
              let cred = pfstBuiltin # wdrlPair
                  script = punsafeCoerce @_ @_ @(PAsData PByteString) $ phead #$ psndBuiltin #$ pasConstr # pforgetData cred
              in pcons # script # (self # rest)
            )
            pkWits
            wdrlList
            ) # pto (pfromData infoF.wdrl)
  pvalidateConditions
    [ pisRewarding ctxF.scriptInfo
    , pvalidateWitnesses # blacklistNodeCS # red # infoF.referenceInputs # txWitnesses
    ]