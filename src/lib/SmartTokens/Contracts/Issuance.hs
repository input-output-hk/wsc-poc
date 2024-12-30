{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QualifiedDo          #-}
{-# LANGUAGE UndecidableInstances #-}

module SmartTokens.Contracts.Issuance (
  mkProgrammableLogicMinting,
  SmartTokenMintingAction (..),
) where

import Plutarch.Builtin (pdataImpl, pfromDataImpl)
import Plutarch.Core.Utils (pand'List, pheadSingleton, ptryLookupValue,
                            pvalidateConditions, (#>))
import Plutarch.Internal.PlutusType (PlutusType (pcon', pmatch'))
import Plutarch.LedgerApi.V3 (PCredential, PScriptContext,
                              PScriptInfo (PMintingScript))
import Plutarch.LedgerApi.Value (PCurrencySymbol, pvalueOf)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusTx qualified

data SmartTokenMintingAction = RegisterPToken | MintPToken
  deriving stock (Show, Eq, Generic)

instance PlutusTx.ToData SmartTokenMintingAction where
  toBuiltinData RegisterPToken = PlutusTx.dataToBuiltinData (PlutusTx.I 0)
  toBuiltinData MintPToken = PlutusTx.dataToBuiltinData (PlutusTx.I 1)

data PSmartTokenMintingAction (s :: S) = PRegisterPToken | PMintPToken

{- |
  PSmartTokenMintingAction is encoded as an Enum, using values of PInteger
  internally.
-}
instance PlutusType PSmartTokenMintingAction where
  type PInner PSmartTokenMintingAction = PInteger

  pcon' PRegisterPToken = 0
  pcon' PMintPToken = 1

  -- redeemer data is untrusted and non-permanent so we can safely decide zero is
  -- PRegisterPToken and anything else we consider PMintPToken.
  pmatch' x f =
    pif (x #== 0) (f PRegisterPToken) (f PMintPToken)

instance PIsData PSmartTokenMintingAction where
    pfromDataImpl d =
        punsafeCoerce (pfromDataImpl @PInteger $ punsafeCoerce d)

    pdataImpl x =
        pdataImpl $ pto x

{-| Minting Policy for Programmable Logic Tokens

This minting policy enables the creation and management of programmable tokens with
configurable transfer and issuer logic.

== Overview
The policy supports two primary actions:
1. Token Registration (PRegisterPToken)
2. Token Minting/Burning (PMintPToken)

== Registration Process
When registering a new programmable token, the policy:
- Creates a directory entry for the token
- Associates the programmable token with specific transfer and issuer logic scripts
- Ensures one-time registration per minting policy instance

== Directory Node Structure
Each programmable token entry is represented in a directory with the following attributes:
- @key@: Currency symbol of the programmable token
- @next@: The currency symbol of the next programmable token identified by the next directory node (enables a linked list structure)
- @transferLogicScript@: Credential of the script that must validate all token transfers of the programmable token
- @issuerLogicScript@: Credential for issuer-specific actions (e.g., clawbacks) for the programmable token

== Constraints
=== Registration Constraints
- Only one token can be registered per minting policy instance
- The first transaction output must contain the minted tokens
- The output must be associated with the base programmable logic credential
- Exactly one node must be inserted into the directory
- The minting logic script must be invoked in the transaction.

=== Minting/Burning Constraints
==== Minting
- Number of tokens in the output must match the minted amount
- Output must be sent to the base programmable logic credential
- Minting logic credential must be invoked in the transaction.

==== Burning
- Minting logic credential must be invoked in the transaction.

@programmableLogicBase@ Script Credential of the programmable logic script
@mintingLogicCred@ Script Credential for the script which must be invoked to perform minting/burning operations
@nodeCS@ Currency symbol of the directory node
@ctx@ Script context containing transaction details
-}
mkProgrammableLogicMinting :: ClosedTerm (PAsData PCredential :--> PAsData PCurrencySymbol :--> PAsData PCredential :--> PScriptContext :--> PUnit)
mkProgrammableLogicMinting = plam $ \programmableLogicBase nodeCS mintingLogicCred ctx -> P.do
  ctxF <- pletFields @'["txInfo", "redeemer", "scriptInfo"] ctx
  infoF <- pletFields @'["referenceInputs", "outputs", "mint", "wdrl"] ctxF.txInfo
  let red = pfromData (punsafeCoerce @_ @_ @(PAsData PSmartTokenMintingAction) (pto ctxF.redeemer))
  PMintingScript scriptInfo <- pmatch ctxF.scriptInfo
  ownCS <- plet $ pfield @"_0" # scriptInfo
  mintedValue <- plet $ pfromData infoF.mint

  let ownTkPairs = ptryLookupValue # ownCS # mintedValue
  -- For ease of implementation of the POC we only allow one programmable token per instance of this minting policy.
  -- This can be easily changed later.
  ownTkPair <- plet (pheadSingleton # ownTkPairs)
  ownTokenName <- plet (pfstBuiltin # ownTkPair)
  ownNumMinted <- plet (pfromData $ psndBuiltin # ownTkPair)
  txOutputs <- plet $ pfromData infoF.outputs
  -- For ease of implementation of the POC we enforce that the first output must contain the minted tokens.
  -- This can be easily changed later.
  mintingToOutputF <- pletFields @'["value", "address"] (phead # txOutputs)

  let invokedScripts =
        pmap @PBuiltinList
          # plam (pfstBuiltin #)
          # pto (pfromData infoF.wdrl)

  pmatch red $ \case
    -- PRegisterPToken is used to register a new programmable token in the directory
    -- It creates a permanent association between the currency symbol with a transferLogicScript and issuerLogicScript.
    -- All transfers of the token will be validated by either the transferLogicScript or the issuerLogicScript.
    -- This redeemer can only be invoked once per instance of this minting policy since the directory contracts do not permit duplicate
    -- entries.
    PRegisterPToken -> P.do
      let nodeTkPairs = ptryLookupValue # nodeCS # mintedValue
      nodeTkPair <- plet (pheadSingleton # nodeTkPairs)
      _insertedName <- plet $ pfstBuiltin # nodeTkPair
      insertedAmount <- plet $ psndBuiltin # nodeTkPair

      let checks =
            pand'List
              [ pvalueOf # pfromData mintingToOutputF.value # pfromData ownCS # pfromData ownTokenName #== ownNumMinted
              , pfield @"credential" # mintingToOutputF.address #== programmableLogicBase
              -- The entry for this currency symbol is inserted into the programmable token directory
              , pfromData insertedAmount #== pconstant 1
              , pelem # mintingLogicCred # invokedScripts
              ]
      pif checks
          (pconstant ())
          perror
    PMintPToken ->
      pif (ownNumMinted #> 0)
          (
            pvalidateConditions
              [ pvalueOf # pfromData mintingToOutputF.value # pfromData ownCS # pfromData ownTokenName #== ownNumMinted
              , pfield @"credential" # mintingToOutputF.address #== programmableLogicBase
              , pelem # mintingLogicCred # invokedScripts
              ]
          )
          (
            -- This branch is for validating the burning of tokens
            pvalidateConditions [pelem # mintingLogicCred # invokedScripts]
          )
