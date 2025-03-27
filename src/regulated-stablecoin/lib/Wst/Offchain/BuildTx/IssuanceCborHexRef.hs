{-# LANGUAGE NamedFieldPuns #-}
module Wst.Offchain.BuildTx.IssuanceCborHexRef (
  mintIssuanceCborHexNFT,
  getCborHexInline,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx (MonadBuildTx, mintPlutus, prependTxOut,
                       spendPublicKeyOutput)
import Convex.Class (MonadBlockchain (..))
import Convex.PlutusLedger.V1 (unTransAssetName)
import Convex.Scripts (fromHashableScriptData, toHashableScriptData)
import Convex.Utils qualified as Utils
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Short qualified as SBS
import GHC.Exts (IsList (..))
import Plutarch.Evaluate (applyArguments)
import Plutarch.Internal.Term (Config (..), compile)
import Plutarch.Prelude (pconstant, (#))
import Plutarch.Script (Script (..))
import PlutusLedgerApi.Common (serialiseUPLC, toBuiltin, toData)
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import SmartTokens.Contracts.Issuance (mkProgrammableLogicMinting)
import SmartTokens.Contracts.IssuanceCborHex (IssuanceCborHex (IssuanceCborHex))
import SmartTokens.Types.Constants (issuanceCborHexToken)
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (..))
import Wst.Offchain.Env (DirectoryEnv (..), globalParams)
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Scripts (scriptPolicyIdV3)

issuerPrefixPostfixBytes :: V3.Credential -> (BS.ByteString, BS.ByteString)
issuerPrefixPostfixBytes progLogicCred =
  let
      dummyHex = BSC.pack "deadbeefcafebabe"  -- Use ByteString for the dummy hex
      placeholderMintingLogic = V3.ScriptHash $ stringToBuiltinByteStringHex "deadbeefcafebabe"
      issuerScriptBase =
        case compile NoTracing (mkProgrammableLogicMinting # pconstant progLogicCred) of
          Right compiledScript -> compiledScript
          Left err -> error $ "Failed to compile issuer script: " <> show err
      dummyIssuerInstanceCborHex = SBS.fromShort . serialiseUPLC . unScript $ applyArguments issuerScriptBase [toData placeholderMintingLogic]
   in breakCborHexBS dummyHex dummyIssuerInstanceCborHex

breakCborHexBS :: BS.ByteString -> BS.ByteString -> (BS.ByteString, BS.ByteString)
breakCborHexBS toSplitOn cborHex =
  case BSC.breakSubstring toSplitOn cborHex of
    (before, after)
      | not (BS.null after) -> (before, BS.drop (BS.length toSplitOn) after)
      | otherwise           -> error $ "breakCborHexBS: Failed to split on " <> show toSplitOn <> " in " <> show cborHex

issuanceCborHexTokenC :: C.AssetName
issuanceCborHexTokenC = unTransAssetName issuanceCborHexToken

{-| Mint the issuance cbor hex NFT and place it in the output locked by 'alwaysFailsScript'
-}
mintIssuanceCborHexNFT :: forall era env m. (MonadReader env m, Env.HasDirectoryEnv env, C.IsBabbageBasedEra era, MonadBuildTx era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBlockchain era m) => m ()
mintIssuanceCborHexNFT = Utils.inBabbage @era $ do
  txIn <- asks (Env.issuanceCborHexTxIn . Env.dsScriptRoot . Env.directoryEnv)
  netId <- queryNetworkId
  dir@DirectoryEnv{dsProtocolParamsMintingScript, dsProtocolParamsSpendingScript} <- asks Env.directoryEnv
  let ProgrammableLogicGlobalParams {progLogicCred} = globalParams dir
      (toBuiltin -> prefixCborHex, toBuiltin -> postfixCborHex) = issuerPrefixPostfixBytes progLogicCred
      issuanceCborHexDatum = IssuanceCborHex prefixCborHex postfixCborHex

  let policyId = scriptPolicyIdV3 dsProtocolParamsMintingScript

      val = C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toLedgerValue @era C.maryBasedEra
            $ fromList [(C.AssetId policyId issuanceCborHexTokenC, 1)]

      addr =
        C.makeShelleyAddressInEra
          C.shelleyBasedEra
          netId
          (C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 dsProtocolParamsSpendingScript)
          C.NoStakeAddress

      -- prefix and postfix bytes of issuance script.
      dat = C.TxOutDatumInline C.babbageBasedEra $ toHashableScriptData issuanceCborHexDatum

      output :: C.TxOut C.CtxTx era
      output = C.TxOut addr val dat C.ReferenceScriptNone

  spendPublicKeyOutput txIn
  mintPlutus dsProtocolParamsMintingScript () issuanceCborHexTokenC 1
  prependTxOut output

getCborHexInline :: C.InAnyCardanoEra (C.TxOut C.CtxTx) -> Maybe ProgrammableLogicGlobalParams
getCborHexInline (C.InAnyCardanoEra _ (C.TxOut _ _ dat _)) =
  case dat of
    C.TxOutDatumInline _era (fromHashableScriptData -> Just d) -> Just d
    _ -> Nothing
