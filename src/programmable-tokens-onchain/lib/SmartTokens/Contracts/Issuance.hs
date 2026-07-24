{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QualifiedDo          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Issuance minting policy — dual-arm custody (spec
-- doc/design-issuance-dual-arm-custody.md §5-§9).
--
-- The policy has two compile-time parameters: @protocolParamsCS@ (identifies the
-- immutable protocol instance; the directory / base / global / seize credentials
-- are read from its NFT-authenticated datum) and @mintingLogicHash@ (the
-- token-specific authorization script — MUST remain the LAST applied parameter so
-- the offchain issuance-cbor-hex derivation can split the compiled CBOR around
-- it). Custody of newly created tokens is enforced by exactly one of four
-- witnesses, chosen by the transaction and revalidated on-chain; a false witness
-- can only invalidate its own transaction.
module SmartTokens.Contracts.Issuance (
  mkProgrammableLogicMinting,
  MintRedeemer (..),
  RegistrationWitness (..),
) where

import Generics.SOP qualified as SOP
import GHC.Generics (Generic)
import Plutarch.Core.Context (paddressCredential, ptxInInfoResolved)
import Plutarch.Builtin.List (pdropList)
import Plutarch.Core.ValidationLogic (pvalidateConditions)
import Plutarch.Core.Value (phasCS, ptryLookupValue)
import Plutarch.LedgerApi.AssocMap (KeyGuarantees (Sorted))
import Plutarch.LedgerApi.V3 (AmountGuarantees (Positive), PCredential (..),
                              PCurrencySymbol, PScriptContext (..),
                              PScriptHash, PScriptInfo (PMintingScript),
                              PScriptPurpose (PRewarding),
                              PTokenName (PTokenName),
                              PTxInfo (PTxInfo, ptxInfo'mint, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'wdrl, ptxInfo'redeemers),
                              PTxOut (PTxOut, ptxOut'address, ptxOut'value),
                              PValue)
import Plutarch.LedgerApi.Value (pvalueOf)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusTx qualified
import SmartTokens.Contracts.ProgrammableLogicBase (PProgrammableLogicGlobalRedeemer (..),
                                                    pparamsAtRefIdx)
import SmartTokens.Types.ProtocolParams (PProgrammableLogicGlobalParams (..))

-- | How the minted policy proves it is registered in the directory (spec §7).
-- Constructor indices are frozen: @RegisteredByReferenceInput = 0@,
-- @RegisteredByOutput = 1@.
data RegistrationWitness
  = RegisteredByReferenceInput Integer
  | RegisteredByOutput Integer
  deriving stock (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed
  ''RegistrationWitness
  [('RegisteredByReferenceInput, 0), ('RegisteredByOutput, 1)]

-- | The custody witness (spec §6). Constructor indices are frozen:
-- @Local = 0@, @DelegateTransfer = 1@, @DelegateSeize = 2@, @BurnOnly = 3@.
-- Every index field is a self-validating hint; a wrong index makes the check it
-- feeds fail (or errors), so honesty is never a trust assumption.
data MintRedeemer
  = Local
      { mrMintingLogicWdrlIdx :: Integer
      , mrParamsRefIdx :: Integer
      , mrRegistration :: RegistrationWitness
      }
  | DelegateTransfer
      { mrMintingLogicWdrlIdx :: Integer
      , mrParamsRefIdx :: Integer
      , mrNodeRefIdx :: Integer
      , mrGlobalWdrlIdx :: Integer
      }
  | DelegateSeize
      { mrMintingLogicWdrlIdx :: Integer
      , mrParamsRefIdx :: Integer
      , mrNodeRefIdx :: Integer
      , mrSeizeRedeemerIdx :: Integer
      }
  | BurnOnly
      { mrMintingLogicWdrlIdx :: Integer
      }
  deriving stock (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed
  ''MintRedeemer
  [('Local, 0), ('DelegateTransfer, 1), ('DelegateSeize, 2), ('BurnOnly, 3)]

data PRegistrationWitness (s :: S)
  = PRegisteredByReferenceInput {pregRefIdx :: Term s (PAsData PInteger)}
  | PRegisteredByOutput {pregOutIdx :: Term s (PAsData PInteger)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PRegistrationWitness)

data PMintRedeemer (s :: S)
  = PLocal
      { plMintingLogicWdrlIdx :: Term s (PAsData PInteger)
      , plParamsRefIdx :: Term s (PAsData PInteger)
      , plRegistration :: Term s (PAsData PRegistrationWitness)
      }
  | PDelegateTransfer
      { pdtMintingLogicWdrlIdx :: Term s (PAsData PInteger)
      , pdtParamsRefIdx :: Term s (PAsData PInteger)
      , pdtNodeRefIdx :: Term s (PAsData PInteger)
      , pdtGlobalWdrlIdx :: Term s (PAsData PInteger)
      }
  | PDelegateSeize
      { pdsMintingLogicWdrlIdx :: Term s (PAsData PInteger)
      , pdsParamsRefIdx :: Term s (PAsData PInteger)
      , pdsNodeRefIdx :: Term s (PAsData PInteger)
      , pdsSeizeRedeemerIdx :: Term s (PAsData PInteger)
      }
  | PBurnOnly
      { pboMintingLogicWdrlIdx :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintRedeemer)

-- | Drop @idx@ elements, rejecting a negative index explicitly (spec §6 — must
-- not rely on budget exhaustion).
pcheckedDrop :: (PIsListLike PBuiltinList a) => Term s (PInteger :--> PBuiltinList a :--> PBuiltinList a)
pcheckedDrop = phoistAcyclic $ plam $ \idx xs ->
  -- The dropList builtin treats a negative count as zero, so the explicit
  -- guard stays: redeemer indices must be rejected, not silently clamped.
  pif (idx #< 0) (ptraceInfoError "negative index") (pdropList # idx # xs)

mkProgrammableLogicMinting :: Term s (PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
mkProgrammableLogicMinting = plam $ \protocolParamsCS mintingLogicHash' ctx -> P.do
  -- The credential the token's minting-logic withdrawal must carry (baked into
  -- the policy id, so this is a compile-time constant — hoist once).
  mintingLogicCred <- plet $ pdata $ pcon $ PScriptCredential mintingLogicHash'
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <- pmatch ctx
  PTxInfo {ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'mint, ptxInfo'wdrl, ptxInfo'redeemers} <- pmatch pscriptContext'txInfo
  PMintingScript ownCS' <- pmatch pscriptContext'scriptInfo
  ownCS <- plet $ pfromData ownCS'
  ownAsTokenName <- plet $ pcon $ PTokenName (pto ownCS)
  withdrawalEntries <- plet $ pto (pfromData ptxInfo'wdrl)
  referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
  outputs <- plet $ pfromData ptxInfo'outputs
  red <- plet $ pfromData (punsafeCoerce @(PAsData PMintRedeemer) (pto pscriptContext'redeemer))

  -- Common check C1 (§7): the token's minting-logic rewarding script ran, proven
  -- by its withdrawal appearing at the witnessed index. A valid ledger tx then
  -- guarantees that script executed.
  mintingLogicInvokedAt <- plet $ plam $ \wdrlIdx ->
    (pfstBuiltin # (phead # (pcheckedDrop # pfromData wdrlIdx # withdrawalEntries))) #== mintingLogicCred

  -- Registration NFT presence: a single directory NFT named `ownCS` under the
  -- directory currency symbol at the resolved value. No datum decode (§7) — the
  -- directory policy already binds NFT name == node key at mint time.
  hasNodeNFT <- plet $ plam $ \directoryNodeCS value ->
    pvalueOf # value # directoryNodeCS # ownAsTokenName #== 1

  pmatch red $ \case
    -- ===== Local: the policy proves custody itself (§8). =====
    PLocal wdrlIdx paramsRefIdx registration -> P.do
      PProgrammableLogicGlobalParams {pdirectoryNodeCS, pprogLogicCred} <-
        pmatch $ pparamsAtRefIdx (pfromData protocolParamsCS) referenceInputs (pfromData paramsRefIdx)
      directoryNodeCS <- plet $ pfromData pdirectoryNodeCS
      progLogicCred <- plet $ pfromData pprogLogicCred
      -- Registration: directory NFT named ownCS at a witnessed reference input OR
      -- output (the output arm is safe HERE only because Local does its own
      -- custody scan and never trusts the global's directory view).
      registrationOk <- plet $
        pmatch (pfromData registration) $ \case
          PRegisteredByReferenceInput idx ->
            pmatch (ptxInInfoResolved $ pfromData (phead # (pcheckedDrop # pfromData idx # referenceInputs))) $
              \(PTxOut {ptxOut'value}) -> hasNodeNFT # directoryNodeCS # pfromData ptxOut'value
          PRegisteredByOutput idx ->
            pmatch (pfromData (phead # (pcheckedDrop # pfromData idx # outputs))) $
              \(PTxOut {ptxOut'value}) -> hasNodeNFT # directoryNodeCS # pfromData ptxOut'value
      -- Custody: universal full scan. Every output whose payment credential is not
      -- the base must hold zero of ownCS. Delta- and input-agnostic. Uses raw
      -- field access (not a full `PTxOut` decode) so the datum and reference
      -- script of each output are never forced — the dominant per-output cost on
      -- busy transactions.
      progLogicCredData <- plet $ pforgetData pprogLogicCred
      noEscape <- plet $
        pall # plam (\o ->
          plet (psndBuiltin # (pasConstr # pforgetData o)) $ \txOutFields ->
            let addrData = phead # txOutFields
                valueData = phead # (ptail # txOutFields)
                paymentCredData = phead # (psndBuiltin # (pasConstr # addrData))
             in pif
                  (paymentCredData #== progLogicCredData)
                  (pconstant True)
                  (pnot # (phasCS # pfromData (punsafeCoerce @(PAsData (PValue 'Sorted 'Positive)) valueData) # ownCS))
        ) # outputs
      pvalidateConditions
        [ mintingLogicInvokedAt # wdrlIdx
        , registrationOk
        , noEscape
        ]

    -- ===== DelegateTransfer: the global transfer validator enforces custody (§9.1). =====
    PDelegateTransfer wdrlIdx paramsRefIdx nodeRefIdx globalWdrlIdx -> P.do
      PProgrammableLogicGlobalParams {pdirectoryNodeCS, pglobalLogicCred} <-
        pmatch $ pparamsAtRefIdx (pfromData protocolParamsCS) referenceInputs (pfromData paramsRefIdx)
      -- Registration by REFERENCE INPUT ONLY (F-1, normative): an output-side
      -- (freshly inserted) node would leave the global's ref-input directory view
      -- pre-insert, letting a NonMember proof exclude ownCS from containment.
      regByRefOk <- plet $
        pmatch (ptxInInfoResolved $ pfromData (phead # (pcheckedDrop # pfromData nodeRefIdx # referenceInputs))) $
          \(PTxOut {ptxOut'value}) -> hasNodeNFT # pfromData pdirectoryNodeCS # pfromData ptxOut'value
      -- The global transfer validator is running (its withdrawal is present at the
      -- witnessed index). It accepts only TransferAct, whose no-omission mint walk
      -- + containment then force ownCS's mint to the base credential.
      globalInvoked <- plet $
        (pfstBuiltin # (phead # (pcheckedDrop # pfromData globalWdrlIdx # withdrawalEntries))) #== pglobalLogicCred
      pvalidateConditions
        [ mintingLogicInvokedAt # wdrlIdx
        , regByRefOk
        , globalInvoked
        ]

    -- ===== DelegateSeize: the standalone seize validator enforces custody (§9.2). =====
    PDelegateSeize wdrlIdx paramsRefIdx nodeRefIdx seizeRedeemerIdx -> P.do
      PProgrammableLogicGlobalParams {pdirectoryNodeCS, pseizeLogicCred} <-
        pmatch $ pparamsAtRefIdx (pfromData protocolParamsCS) referenceInputs (pfromData paramsRefIdx)
      regByRefOk <- plet $
        pmatch (ptxInInfoResolved $ pfromData (phead # (pcheckedDrop # pfromData nodeRefIdx # referenceInputs))) $
          \(PTxOut {ptxOut'value}) -> hasNodeNFT # pfromData pdirectoryNodeCS # pfromData ptxOut'value
      -- The seize redeemer at the witnessed index must be a SeizeAct of the params
      -- seize credential whose directoryNodeIdx names the SAME node we proved is
      -- keyed ownCS — binding the (single-policy) seize scope to ownCS.
      seizeEntry <- plet $ phead # (pcheckedDrop # pfromData seizeRedeemerIdx # pto (pfromData ptxInfo'redeemers))
      seizeScopeOk <- plet $
        pmatch (pfromData (punsafeCoerce @(PAsData PScriptPurpose) (pfstBuiltin # seizeEntry))) $ \case
          PRewarding seizeCred ->
            pmatch (pfromData (punsafeCoerce @(PAsData PProgrammableLogicGlobalRedeemer) (psndBuiltin # seizeEntry))) $ \case
              PSeizeAct {pdirectoryNodeIdx} ->
                (pdata seizeCred #== pseizeLogicCred)
                  #&& (pfromData pdirectoryNodeIdx #== pfromData nodeRefIdx)
              _ -> pconstant False
          _ -> pconstant False
      pvalidateConditions
        [ mintingLogicInvokedAt # wdrlIdx
        , regByRefOk
        , seizeScopeOk
        ]

    -- ===== BurnOnly: no positive mint ⇒ no custody/registration/params proof (§3.4). =====
    PBurnOnly wdrlIdx -> P.do
      -- Scan the WHOLE ownCS mint token-map: any positive entry means this is not a
      -- pure burn and must use a custody arm. (Head-only would smuggle a mint.)
      ownTkPairs <- plet $ ptryLookupValue # ownCS' # pfromData ptxInfo'mint
      pvalidateConditions
        [ mintingLogicInvokedAt # wdrlIdx
        , pall # plam (\pair -> pfromData (psndBuiltin # pair) #<= 0) # ownTkPairs
        ]
