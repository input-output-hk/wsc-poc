{-# LANGUAGE NamedFieldPuns #-}
module ProgrammableTokens.OffChain.BuildTx.IssuanceCborHexRef (
  mintIssuanceCborHexNFT,
  frackUTxOs,
  getCborHexInline,
) where

import Cardano.Api qualified as C
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx (MonadBuildTx, mintPlutus, prependTxOut,
                       spendPublicKeyOutput)
import Convex.Class (MonadBlockchain (..))
import Convex.PlutusLedger.V1 (transPolicyId, unTransAssetName)
import Convex.Scripts (fromHashableScriptData, toHashableScriptData)
import Convex.Utils qualified as Utils
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import GHC.Exts (IsList (..))
import Plutarch.Evaluate (applyArguments)
import Plutarch.Internal.Term (Config (..), compile)
import Plutarch.Prelude (pconstant, pdata, (#))
import Plutarch.Script (Script (..))
import PlutusLedgerApi.V1
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import ProgrammableTokens.OffChain.Env (DirectoryEnv (..))
import ProgrammableTokens.OffChain.Env qualified as Env
import ProgrammableTokens.OffChain.Scripts (scriptPolicyIdV3)
import SmartTokens.Contracts.Issuance (mkProgrammableLogicMinting)
import SmartTokens.Contracts.IssuanceCborHex (IssuanceCborHex (IssuanceCborHex))
import SmartTokens.Types.Constants (issuanceCborHexToken)
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (..))

-- | The @protocolParamsCS@ argument must be applied in the same position as
-- on-chain so the compiled prefix matches; the minting-logic hash stays the
-- last-applied argument and remains the CBOR split point.
issuerPrefixPostfixBytes :: CurrencySymbol -> (BS.ByteString, BS.ByteString)
issuerPrefixPostfixBytes protocolParamsCS =
  let
      placeholderMintingLogic = V3.ScriptHash $ stringToBuiltinByteStringHex "deadbeefcafebabedeadbeefcafebabedeadbeefcafebabedeadbeef"
      dummyHex = fromBuiltin $ BI.serialiseData $ PlutusTx.toBuiltinData placeholderMintingLogic
      -- Must mirror 'programmableLogicMintingScript' EXACTLY (same @pdata
      -- (pconstant …)@ wrapping, same single applied parameter) so the compiled
      -- prefix/postfix match the real issued policy id; otherwise the on-chain
      -- registration derivation (_papplyHashedParameter) reconstructs a different
      -- hash.
      issuerScriptBase =
        case compile NoTracing (mkProgrammableLogicMinting # pdata (pconstant protocolParamsCS)) of
          Right compiledScript -> compiledScript
          Left err -> error $ "Failed to compile issuer script: " <> show err
      dummyIssuerInstanceCborHex = SBS.fromShort . serialiseUPLC . unScript $ applyArguments issuerScriptBase [toData placeholderMintingLogic]
   in breakCborHexBS dummyHex dummyIssuerInstanceCborHex

breakCborHexBS :: BS.ByteString -> BS.ByteString -> (BS.ByteString, BS.ByteString)
breakCborHexBS toSplitOn cborHex =
  case BS.breakSubstring toSplitOn cborHex of
    (before_, after_)
      | not (BS.null after_) -> (before_, BS.drop (BS.length toSplitOn) after_)
      | otherwise           -> error $ "breakCborHexBS: Failed to split on " <> show toSplitOn <> " in " <> show cborHex

issuanceCborHexTokenC :: C.AssetName
issuanceCborHexTokenC = unTransAssetName issuanceCborHexToken

{-| Mint the issuance cbor hex NFT and place it in the output locked by 'alwaysFailsScript'
-}
mintIssuanceCborHexNFT :: forall era env m. (MonadReader env m, Env.HasDirectoryEnv env, C.IsBabbageBasedEra era, MonadBuildTx era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBlockchain era m) => m ()
mintIssuanceCborHexNFT = Utils.inBabbage @era $ do
  txIn <- asks (Env.srIssuanceCborHexTxIn . Env.dsScriptRoot . Env.directoryEnv)
  netId <- queryNetworkId
  dir@DirectoryEnv{dsIssuanceCborHexMintingScript, dsIssuanceCborHexSpendingScript} <- asks Env.directoryEnv
  let protocolParamsCS = transPolicyId (Env.protocolParamsPolicyId dir)
      (toBuiltin -> prefixCborHex, toBuiltin -> postfixCborHex) = issuerPrefixPostfixBytes protocolParamsCS
      issuanceCborHexDatum = IssuanceCborHex prefixCborHex postfixCborHex

  let policyId = scriptPolicyIdV3 dsIssuanceCborHexMintingScript

      val = C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toLedgerValue @era C.maryBasedEra
            $ fromList [(C.AssetId policyId issuanceCborHexTokenC, 1)]

      addr =
        C.makeShelleyAddressInEra
          C.shelleyBasedEra
          netId
          (C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 dsIssuanceCborHexSpendingScript)
          C.NoStakeAddress

      -- prefix and postfix bytes of issuance script.
      dat = C.TxOutDatumInline C.babbageBasedEra $ toHashableScriptData issuanceCborHexDatum

      output :: C.TxOut C.CtxTx era
      output = C.TxOut addr val dat C.ReferenceScriptNone

  spendPublicKeyOutput txIn
  mintPlutus dsIssuanceCborHexMintingScript () issuanceCborHexTokenC 1
  prependTxOut output

frackUTxOs :: forall era env m. (C.IsBabbageBasedEra era, MonadBuildTx era m, MonadBlockchain era m, MonadReader env m, Env.HasOperatorEnv era env) => m ()
frackUTxOs = Utils.inBabbage @era $ do
  Env.OperatorEnv{Env.bteOperator} <- asks (Env.operatorEnv @era)
  netId <- queryNetworkId
  let val = C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toLedgerValue @era C.maryBasedEra
            $ C.lovelaceToValue 150_000_000

      addr =
        C.makeShelleyAddressInEra
          C.shelleyBasedEra
          netId
          (C.PaymentCredentialByKey (fst bteOperator))
          (snd bteOperator)

      output :: C.TxOut C.CtxTx era
      output = C.TxOut addr val C.TxOutDatumNone C.ReferenceScriptNone

  prependTxOut output
  prependTxOut output
  prependTxOut output
  prependTxOut output

getCborHexInline :: C.InAnyCardanoEra (C.TxOut C.CtxTx) -> Maybe ProgrammableLogicGlobalParams
getCborHexInline (C.InAnyCardanoEra _ (C.TxOut _ _ dat _)) =
  case dat of
    C.TxOutDatumInline _era (fromHashableScriptData -> Just d) -> Just d
    _ -> Nothing
