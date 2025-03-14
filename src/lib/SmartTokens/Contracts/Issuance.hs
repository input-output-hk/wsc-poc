{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QualifiedDo          #-}
{-# LANGUAGE UndecidableInstances #-}

module SmartTokens.Contracts.Issuance (
  mkProgrammableLogicMinting,
  SmartTokenMintingAction (..),
) where

import GHC.Generics (Generic)
import Plutarch.Core.Context (paddressCredential)
import Plutarch.Core.List (pheadSingleton)
import Plutarch.Core.ValidationLogic (pvalidateConditions)
import Plutarch.Core.Value (ptryLookupValue)
import Plutarch.Internal.PlutusType (PlutusType (pcon', pmatch'))
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3 (PCredential, PRedeemer, PScriptContext (..),
                              PScriptInfo (PMintingScript), PScriptPurpose (..),
                              PTxInfo (PTxInfo, ptxInfo'mint, ptxInfo'outputs, ptxInfo'wdrl),
                              PTxOut (PTxOut, ptxOut'address, ptxOut'value))
import Plutarch.LedgerApi.Value (pvalueOf)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusTx qualified

data SmartTokenMintingAction = MintPToken
  deriving stock (Show, Eq, Generic)

instance PlutusTx.ToData SmartTokenMintingAction where
  toBuiltinData MintPToken = PlutusTx.dataToBuiltinData (PlutusTx.I 0)

data PSmartTokenMintingAction (s :: S) = PMintPToken

{- |
  PSmartTokenMintingAction is encoded as an Enum, using values of PInteger
  internally.
-}
instance PlutusType PSmartTokenMintingAction where
  type PInner PSmartTokenMintingAction = PInteger

  pcon' PMintPToken = 0

  -- redeemer data is untrusted and non-permanent so we can safely decide zero is
  -- PMintPToken
  pmatch' x f =
    pif (x #== 0) (f PMintPToken) perror

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
1. Token Registration
2. Token Minting/Burning

Both actions are encompassed by the `PMintPToken` redeemer, which is used to register a new programmable token and to mint or burn tokens of an already registered token.

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
- @globalStateCS@: The currency symbol of a NFT that uniquely identifies a UTxO that contains the global state associated with the programmable token.
    This is optionaly and can be set to the empty currency symbol if not needed.

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
mkProgrammableLogicMinting :: ClosedTerm (PAsData PCredential :--> PAsData PCredential :--> PScriptContext :--> PUnit)
mkProgrammableLogicMinting = plam $ \(pfromData -> programmableLogicBase) mintingLogicCred ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <- pmatch ctx
  PTxInfo {ptxInfo'outputs, ptxInfo'mint, ptxInfo'wdrl} <- pmatch pscriptContext'txInfo

  let red = pfromData (punsafeCoerce @(PAsData PSmartTokenMintingAction) (pto pscriptContext'redeemer))
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

  pmatch red $ \case
    -- PMintPToken is used to register a new programmable token in the directory and to mint the programmable token upon registration and
    -- in any subsequent transaction.
    -- At registration this creates a permanent association between the currency symbol with a transferLogicScript and issuerLogicScript.
    -- All transfers of the token will be validated by either the transferLogicScript or the issuerLogicScript.
    -- Registration can only occurr once per instance of this minting policy since the directory contracts do not permit duplicate
    -- entries.
    PMintPToken ->
      pif (ownNumMinted #> 0)
          (
            pvalidateConditions
              [ pvalueOf # pfromData mintingToOutputFValue # pfromData ownCS # pfromData ownTokenName #== ownNumMinted
              , paddressCredential mintingToOutputFAddress #== programmableLogicBase
              , pelem # mintingLogicCred # invokedScripts
              ]
          )
          (
            -- This branch is for validating the burning of tokens
            pvalidateConditions [pelem # mintingLogicCred # invokedScripts]
          )

-- TODO:
-- Add this check to the above validator
-- and adjust the offchain to provide the redeemer of the issuance minting policy set to the Credential of the withdraw-zero validator.
_psingleMintWithCredential :: Term (s :: S) (PAsData PRedeemer :--> AssocMap.PMap 'AssocMap.Unsorted PScriptPurpose PRedeemer :--> PBool)
_psingleMintWithCredential =
  phoistAcyclic $ plam $ \rdmr redeemers ->
    let go = pfix #$ plam $ \self ->
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
