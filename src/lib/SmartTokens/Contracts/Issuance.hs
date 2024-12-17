module SmartTokens.Contracts.Issuance (
  mkProgrammableLogicMinting,
) where

import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Builtin
import Plutarch.LedgerApi.Value
import Plutarch.Core.Utils
import Plutarch.Unsafe
import Plutarch.Internal.PlutusType (PlutusType(pcon', pmatch'))
--import SmartTokens.Types.PTokenDirectory (PDirectorySetNode)

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

mkProgrammableLogicMinting :: ClosedTerm (PAsData PCredential :--> PAsData PCredential :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
mkProgrammableLogicMinting = plam $ \programmableLogicBase mintingLogicCred nodeCS ctx -> P.do
  ctxF <- pletFields @'["txInfo", "redeemer", "scriptInfo"] ctx
  infoF <- pletFields @'["referenceInputs", "outputs", "mint", "wdrl"] ctxF.txInfo
  let red = punsafeCoerce @_ @_ @PSmartTokenMintingAction (pto ctxF.redeemer)
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
