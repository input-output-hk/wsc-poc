{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QualifiedDo          #-}
{-# LANGUAGE UndecidableInstances #-}

module SmartTokens.Contracts.Issuance (
  mkProgrammableLogicMinting,
  SmartTokenMintingAction (..),
) where

import Generics.SOP qualified as SOP
import GHC.Generics (Generic)
import Plutarch.Core.Context (paddressCredential, ptxInInfoResolved)
import Plutarch.Core.List (pheadSingleton)
import Plutarch.Core.ValidationLogic (pvalidateConditions)
import Plutarch.Core.Value (ptryLookupValue)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3 (PCredential (..), PCurrencySymbol,
                              PMaybeData (PDJust, PDNothing),
                              PRedeemer, PScriptContext (..),
                              PScriptHash, PScriptInfo (PMintingScript),
                              PScriptPurpose (..),
                              PStakingCredential (PStakingHash),
                              PTokenName (PTokenName),
                              PTxInInfo, PTxInfo (PTxInfo, ptxInfo'mint, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'wdrl),
                              PTxOut (PTxOut, ptxOut'address, ptxOut'value),
                              ptxInfo'redeemers)
import Plutarch.LedgerApi.Value (pvalueOf)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V3
import PlutusTx qualified

newtype SmartTokenMintingAction = SmartTokenMintingAction Credential
  deriving stock (Show, Eq, Generic)
  deriving newtype
    ( PlutusTx.ToData
    , PlutusTx.FromData
    , PlutusTx.UnsafeFromData
    )

newtype PSmartTokenMintingAction (s :: S) = PSmartTokenMintingAction (Term s PCredential)
  deriving stock
    ( Generic
    )
  deriving anyclass
    ( SOP.Generic
    , PIsData
    , PEq
    , PShow
    )
  deriving
    (
      PlutusType
    )
    via (DeriveNewtypePlutusType PSmartTokenMintingAction)

{-| Minting Policy for Programmable Logic Tokens

This minting policy enables the creation and management of programmable tokens with
configurable transfer and issuer logic.

== Overview
The policy supports two primary actions:
1. Token Minting
2. Token Burning

Both actions are encompassed by the `PSmartTokenMintingAction` redeemer, which is used to mint or burn tokens of an already registered token.

== Registration Process
Before a token is minted from this policy, it should be registered in the directory.
For details on the registration process refer to the `SmartTokens.LinkedList.MintDirectory` module.

== Directory Node Structure
Each programmable token entry is represented in a directory with the following attributes:
- @key@: Currency symbol of the programmable token
- @next@: The currency symbol of the next programmable token identified by the next directory node (enables a linked list structure)
- @transferLogicScript@: Credential of the script that must validate all token transfers of the programmable token
- @issuerLogicScript@: Credential for issuer-specific actions (e.g., clawbacks) for the programmable token
- @globalStateCS@: The currency symbol of an NFT that uniquely identifies a UTxO that contains the global state associated with the programmable token.
    This is optionally and can be set to the empty currency symbol if not needed.

== Constraints
=== Minting/Burning Constraint
- The first transaction output must contain the minted tokens
- The output must be associated with the base programmable logic credential
- The minting logic script must be invoked in the transaction.

==== Burning
- Minting logic credential must be invoked in the transaction.
- No tokens can be minted.
- At-least one token must be burned.

@programmableLogicBase@ Script Credential of the programmable logic script
@mintingLogicCred@ Script Credential for the script which must be invoked to perform minting/burning operations
@ctx@ Script context containing transaction details
-}
-- The @directoryNodeCS@ parameter sits between the base credential and the
-- minting-logic hash so that the minting-logic hash remains the LAST applied
-- argument — the offchain issuance-cbor-hex derivation splits the compiled
-- script's CBOR around that placeholder, so its position must not change.
mkProgrammableLogicMinting :: Term s (PAsData PCredential :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
mkProgrammableLogicMinting = plam $ \(pfromData -> programmableLogicBase) directoryNodeCS mintingLogicCred' ctx -> P.do
  let mintingLogicCred = pdata $ pcon $ PScriptCredential mintingLogicCred'
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <- pmatch ctx
  PTxInfo {ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'mint, ptxInfo'wdrl, ptxInfo'redeemers} <- pmatch pscriptContext'txInfo

  PMintingScript ownCS' <- pmatch pscriptContext'scriptInfo
  ownCS <- plet ownCS'
  mintedValue <- plet $ pfromData ptxInfo'mint

  -- Registration binding (security S2, mirrors Aiken issuance_mint Finding 04):
  -- the minted policy MUST be registered in the directory. Without this, an
  -- issuer (or a compromised minting logic) could mint tokens for an
  -- UNregistered policy; those tokens start in the mini-ledger but can then
  -- escape it via a covering-node (`TokenDoesNotExist`) transfer proof — which
  -- exists precisely because the policy is unregistered — defeating
  -- freeze/seize control. Proof: a reference input holds the directory NFT named
  -- after `ownCS` under the trusted `directoryNodeCS`. That NFT can only be
  -- minted by the directory policy, which enforces token-name == node-key
  -- (parseNodeOutputUtxo), so its mere presence proves a node keyed on `ownCS`
  -- exists — no datum decode needed (cheaper than Aiken's node-datum parse).
  ownAsTokenName <- plet $ pcon $ PTokenName (pto (pfromData ownCS))
  nodeCSsym <- plet $ pfromData directoryNodeCS
  hasRegistryNode <- plet $ plam $ \txOut ->
        pmatch txOut $ \(PTxOut{ptxOut'value=nodeVal}) ->
          pvalueOf # pfromData nodeVal # nodeCSsym # ownAsTokenName #== 1
  -- Accept the node either as a reference input (already registered) OR as an
  -- output (registered in this same tx — Aiken's OutputIndex mode). The
  -- reference-input scan short-circuits first, so an ordinary mint of an
  -- already-registered policy never pays for the output scan.
  registrationProven <- plet $
    (pany # plam (\txIn -> hasRegistryNode # ptxInInfoResolved (pfromData txIn)) # pfromData ptxInfo'referenceInputs)
      #|| (pany # plam (\o -> hasRegistryNode # pfromData o) # pfromData ptxInfo'outputs)

  let ownTkPairs = ptryLookupValue # ownCS # mintedValue
  -- For ease of implementation of the POC we only allow one programmable token per instance of this minting policy.
  -- This can be easily changed later.
  ownTkPair <- plet (pheadSingleton # ownTkPairs)
  ownTokenName <- plet (pfstBuiltin # ownTkPair)
  ownNumMinted <- plet (pfromData $ psndBuiltin # ownTkPair)
  txOutputs <- plet $ pfromData ptxInfo'outputs
  -- For ease of implementation of the POC we enforce that the first output must contain the minted tokens.
  -- This can be easily changed later.
  PTxOut {ptxOut'address=mintingToOutputFAddress, ptxOut'value=mintingToOutputFValue} <- pmatch (pfromData $ phead # txOutputs)

  let invokedScripts =
        pmap @PBuiltinList
          # plam (pfstBuiltin #)
          # pto (pfromData ptxInfo'wdrl)
  red <- plet pscriptContext'redeemer
  -- All transfers of the token will be validated by either the transferLogicScript or the issuerLogicScript.
  -- Registration can only occurr once per instance of this minting policy since the directory contracts do not permit duplicate
  -- entries.
  -- Item 4: the minted-to base output must carry an INLINE stake credential.
  -- Minting to a base-cred output with no (or a pointer) staking credential would
  -- create a UTxO the transfer path can never attribute to an owner — permanently
  -- locked. Extract the address's staking credential via raw field access (mirrors
  -- `pvalueFromCred` in ProgrammableLogicBase).
  let mintOutputStakingCredMaybe =
        punsafeCoerce @(PMaybeData PStakingCredential)
          (phead # (ptail # (psndBuiltin # (pasConstr # pforgetData (pdata mintingToOutputFAddress)))))
      mintOutputHasInlineStake =
        pmatch mintOutputStakingCredMaybe $ \case
          PDJust scData ->
            pmatch (pfromData scData) $ \case
              PStakingHash _ -> pconstant True
              _ -> pconstant False
          PDNothing -> pconstant False

  pif (ownNumMinted #> 0)
      (
        -- This branch is for validating the minting of tokens
        pvalidateConditions
          [ pvalueOf # pfromData mintingToOutputFValue # pfromData ownCS # pfromData ownTokenName #== ownNumMinted
          , paddressCredential mintingToOutputFAddress #== programmableLogicBase
          , mintOutputHasInlineStake
          , registrationProven
          , pelem # mintingLogicCred # invokedScripts
          , punsafeCoerce @(PAsData PCredential) (pto red) #== mintingLogicCred
          , psingleMintWithCredential # pdata red # pfromData ptxInfo'redeemers
          ]
      )
      (
        -- This branch is for validating the burning of tokens
        pvalidateConditions
          [ registrationProven
          , pelem # mintingLogicCred # invokedScripts
          , psingleMintWithCredential # pdata pscriptContext'redeemer # pfromData ptxInfo'redeemers
          ]
      )

-- | Check that exactly one `PMinting` redeemer with the mintingLogic credential is present in the transaction.
psingleMintWithCredential :: Term (s :: S) (PAsData PRedeemer :--> AssocMap.PMap 'AssocMap.Unsorted PScriptPurpose PRedeemer :--> PBool)
psingleMintWithCredential =
  phoistAcyclic $ plam $ \rdmr redeemers ->
    -- skip spending purposes
    let go = pfix #$ plam $ \self ->
              pelimList
                (\x xs ->
                  let purposeConstrPair = pfstBuiltin # x
                      purposeConstrIdx = pfstBuiltin # (pasConstr # pforgetData purposeConstrPair)
                   in pif
                        (pnot # (purposeConstrIdx #== 0))
                        (self # xs)
                        ( pif (psndBuiltin # x #== rdmr)
                              (go2 # xs)
                              (go1 # xs)
                        )
                )
                (pconstant False)
        -- check that exactly one redeemer is equal to the mintingLogic credential
        go1 = pfix #$ plam $ \self ->
              pelimList
                (\x xs ->
                  let purposeConstrPair = pfstBuiltin # x
                      purposeConstrIdx = pfstBuiltin # (pasConstr # pforgetData purposeConstrPair)
                   in pif
                        (purposeConstrIdx #== 0)
                        ( pif (psndBuiltin # x #== rdmr)
                              (go2 # xs)
                              (self # xs)
                        )
                        (pconstant False)
                )
                (pconstant False)
        -- check that no other redeemer is equal to the mintingLogic credential
        go2 = pfix #$ plam $ \self ->
                pelimList
                  (\x xs ->
                    let purposeConstrPair = pfstBuiltin # x
                        purposeConstrIdx = pfstBuiltin # (pasConstr # pforgetData purposeConstrPair)
                    in pif
                          (purposeConstrIdx #== 0)
                          ( pif (psndBuiltin # x #== rdmr)
                                (pconstant False)
                                (self # xs)
                          )
                          (pconstant True)
                  )
                  (pconstant True)
     in go # pto redeemers
