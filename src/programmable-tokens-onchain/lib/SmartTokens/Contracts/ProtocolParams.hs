{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QualifiedDo         #-}
module SmartTokens.Contracts.ProtocolParams (
  mkProtocolParametersMinting,
  alwaysFailScript,
  mkPermissionedMinting,
) where

import Plutarch.Core.Context
import Plutarch.Core.List
import Plutarch.Core.Trace (pdebug)
import Plutarch.Core.Utils
import Plutarch.Core.ValidationLogic
import Plutarch.Core.Value
import Plutarch.LedgerApi.V3
import Plutarch.LedgerApi.Value (pvalueOf)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import SmartTokens.Types.Constants (pprotocolParamsTokenData)
import SmartTokens.Types.ProtocolParams (PProgrammableLogicGlobalParams (..))

-- | Protocol Parameters minting policy (spec §4.1 — hardened anchor).
--
-- The protocol-params NFT is the single root of trust every runtime validator
-- authenticates by the cheap first-non-Ada @phasCSH@ idiom and then reads by
-- RAW field access with no shape re-validation. That is only sound if this
-- policy guarantees, at mint time, that the NFT sits in a well-formed,
-- immutable UTxO. It therefore enforces:
--
--   1. the configured one-shot input is spent (symbol uniqueness);
--   2. exactly one NFT is minted under this policy, named @protocolParamsToken@;
--   3. the NFT output sits at the canonical always-fail spending address
--      (@paramsSpendScriptHash@) with NO stake credential — so the UTxO can
--      never be spent and the datum is immutable;
--   4. that output holds ONLY Ada plus the single NFT (no third asset), which
--      is exactly what licenses the runtime first-non-Ada authentication;
--   5. the output carries an inline datum that fully decodes to the 4-field
--      'PProgrammableLogicGlobalParams', with a canonical 28-byte currency
--      symbol and each of the three credential fields a @PScriptCredential@
--      wrapping a canonical 28-byte hash (a key credential in any script slot
--      makes the NFT unmintable); and
--   6. the output carries no reference script.
--
-- All checks are genesis-only (paid once), so they impose no runtime cost.
mkProtocolParametersMinting :: Term s (PAsData PScriptHash :--> PAsData PTxOutRef :--> PScriptContext :--> PUnit)
mkProtocolParametersMinting = plam $ \paramsSpendScriptHash oref ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatch ctx
  PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'mint} <- pmatch pscriptContext'txInfo
  PMintingScript ownCS' <- pmatch pscriptContext'scriptInfo
  ownCS <- plet ownCS'

  mintedValue <- plet $ pfromData ptxInfo'mint
  let ownTkPairs = ptryLookupValue # ownCS # mintedValue

  -- (2) Enforce that only a single token name, qty 1, is minted for this policy.
  ownTkPair <- plet (pheadSingleton # ownTkPairs)
  ownTokenName <- plet (pfstBuiltin # ownTkPair)
  ownNumMinted <- plet (psndBuiltin # ownTkPair)

  -- Locate the unique output carrying the NFT.
  anchorOut <- plet $ pfindAnchorOutput # ownCS # ownTokenName # pfromData ptxInfo'outputs
  PTxOut
    { ptxOut'address = anchorAddr
    , ptxOut'value = anchorValD
    , ptxOut'datum = anchorDatum
    , ptxOut'referenceScript = anchorRefScript
    } <- pmatch anchorOut
  anchorValue <- plet $ pfromData anchorValD

  -- (3) Canonical always-fail address, no stake credential.
  PAddress {paddress'credential = anchorCred, paddress'stakingCredential = anchorStake} <- pmatch anchorAddr

  -- (5) Inline datum fully decodes to the 4-field type; validate every field.
  POutputDatum anchorInlineDatum <- pmatch anchorDatum
  params <- plet $ pfromData (punsafeCoerce @(PAsData PProgrammableLogicGlobalParams) (pto anchorInlineDatum))
  PProgrammableLogicGlobalParams
    { pdirectoryNodeCS = pDirCS
    , pprogLogicCred = pBaseCred
    , pglobalLogicCred = pGlobalCred
    , pseizeLogicCred = pSeizeCred
    } <- pmatch params

  pvalidateConditions
    [ pdebug "minted tn must match protocolParamsToken" $ ownTokenName #== pprotocolParamsTokenData
    , pdebug "only single pp token must be minted" $ ownNumMinted #== pconstant 1
    , pdebug "must spend ppInitTxOutRef" $ phasUTxO # pfromData oref # pfromData ptxInfo'inputs
    -- (3) canonical immutable address, no stake credential
    , pdebug "anchor must sit at params-spend script cred" $
        anchorCred #== pcon (PScriptCredential paramsSpendScriptHash)
    , pdebug "anchor must have no stake credential" $
        anchorStake #== pcon PDNothing
    -- (4) only Ada + the single NFT (exactly two policies: Ada and ownCS), NFT qty 1
    , pdebug "anchor value must be ada + single NFT only" $
        (plength # pto (pto anchorValue)) #== 2
    , pdebug "anchor holds the NFT" $
        pvalueOf # anchorValue # pfromData ownCS # pfromData ownTokenName #== 1
    -- (5) canonical field lengths
    , pdebug "params CS must be 28 bytes" $ pisCanonicalCS # pDirCS
    , pdebug "base cred must be script/28" $ pisScriptCred28 # pBaseCred
    , pdebug "global cred must be script/28" $ pisScriptCred28 # pGlobalCred
    , pdebug "seize cred must be script/28" $ pisScriptCred28 # pSeizeCred
    -- (6) no reference script
    , pdebug "anchor must carry no reference script" $
        anchorRefScript #== pcon PDNothing
    ]

-- | Find the single transaction output holding one unit of @cs.tn@. Errors if
-- no such output exists (the mint would then be unusable). Because the mint is
-- a single unit (checked by the caller) and the anchor value check forbids
-- splitting, there is exactly one.
pfindAnchorOutput :: Term s (PAsData PCurrencySymbol :--> PAsData PTokenName :--> PBuiltinList (PAsData PTxOut) :--> PTxOut)
pfindAnchorOutput = phoistAcyclic $ plam $ \cs tn outputs ->
  pmatch (pfromData cs) $ \(PCurrencySymbol csb) ->
    let go = pfix #$ plam $ \self outs ->
          pelimList
            ( \out rest ->
                pmatch (pfromData out) $ \txout@(PTxOut{ptxOut'value}) ->
                  pif
                    (pvalueOf # pfromData ptxOut'value # pcon (PCurrencySymbol csb) # pfromData tn #== 1)
                    (pcon txout)
                    (self # rest)
            )
            (ptraceInfoError "protocol-params NFT output not found")
            outs
     in go # outputs

-- | A currency symbol whose underlying bytes are exactly 28 long.
pisCanonicalCS :: Term s (PAsData PCurrencySymbol :--> PBool)
pisCanonicalCS = phoistAcyclic $ plam $ \csD ->
  pmatch (pfromData csD) $ \(PCurrencySymbol csb) -> plengthBS # csb #== 28

-- | A credential that is a @PScriptCredential@ wrapping a canonical 28-byte
-- script hash. A @PPubKeyCredential@ (or a malformed hash) fails.
pisScriptCred28 :: Term s (PAsData PCredential :--> PBool)
pisScriptCred28 = phoistAcyclic $ plam $ \credD ->
  pmatch (pfromData credD) $ \case
    PScriptCredential sh ->
      pmatch (pfromData sh) $ \(PScriptHash shb) -> plengthBS # shb #== 28
    PPubKeyCredential _ -> pconstant False

-- | Permissioned Minting Policy
-- This minting policy checks for a given permissioned credential in the signatories of the transaction.
-- It allows minting of any number of tokens with any token name so long as the credential authorizes the transaction.
mkPermissionedMinting :: Term s (PData :--> PAsData PPubKeyHash :--> PScriptContext :--> PUnit)
mkPermissionedMinting = plam $ \_ permissionedCred ctx ->
  pvalidateConditions
    [ ptxSignedByPkh # permissionedCred # (pfromData . ptxInfoSignatories . pscriptContextTxInfo) ctx
    ]

-- | A nonced always fails script
-- The parameter is used to modify the script hash.
-- This is where the protocol parameters UTxO should reside.
alwaysFailScript :: Term s (PData :--> PScriptContext :--> PUnit)
alwaysFailScript = plam $ \_ _ctx -> perror
