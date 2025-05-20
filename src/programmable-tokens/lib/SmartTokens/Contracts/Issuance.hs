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
import Plutarch.Core.Context (paddressCredential)
import Plutarch.Core.List (pheadSingleton)
import Plutarch.Core.ValidationLogic (pvalidateConditions)
import Plutarch.Core.Value (ptryLookupValue)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3 (PCredential (..), PRedeemer, PScriptContext (..),
                              PScriptHash, PScriptInfo (PMintingScript),
                              PScriptPurpose (..),
                              PTxInfo (PTxInfo, ptxInfo'mint, ptxInfo'outputs, ptxInfo'wdrl),
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
mkProgrammableLogicMinting :: ClosedTerm (PAsData PCredential :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
mkProgrammableLogicMinting = plam $ \(pfromData -> programmableLogicBase) mintingLogicCred' ctx -> P.do
  let mintingLogicCred = pdata $ pcon $ PScriptCredential mintingLogicCred'
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <- pmatch ctx
  PTxInfo {ptxInfo'outputs, ptxInfo'mint, ptxInfo'wdrl, ptxInfo'redeemers} <- pmatch pscriptContext'txInfo

  PMintingScript ownCS' <- pmatch pscriptContext'scriptInfo
  ownCS <- plet ownCS'
  mintedValue <- plet $ pfromData ptxInfo'mint

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
  pif (ownNumMinted #> 0)
      (
        -- This branch is for validating the minting of tokens
        pvalidateConditions
          [ pvalueOf # pfromData mintingToOutputFValue # pfromData ownCS # pfromData ownTokenName #== ownNumMinted
          , paddressCredential mintingToOutputFAddress #== programmableLogicBase
          , pelem # mintingLogicCred # invokedScripts
          , punsafeCoerce @(PAsData PCredential) (pto red) #== mintingLogicCred
          , psingleMintWithCredential # pdata red # pfromData ptxInfo'redeemers
          ]
      )
      (
        -- This branch is for validating the burning of tokens
        pvalidateConditions
          [ pelem # mintingLogicCred # invokedScripts
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
