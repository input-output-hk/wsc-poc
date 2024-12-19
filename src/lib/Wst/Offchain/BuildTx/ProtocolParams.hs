module Wst.Offchain.BuildTx.ProtocolParams (
  mintProtocolParams,
  getProtocolParamsGlobalInline
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Convex.BuildTx (MonadBuildTx, mintPlutus, prependTxOut,
                       spendPublicKeyOutput)
import Convex.Class (MonadBlockchain (..))
import Convex.PlutusLedger.V1 (unTransAssetName)
import Convex.Scripts (fromHashableScriptData, toHashableScriptData)
import Convex.Utils qualified as Utils
import GHC.Exts (IsList (..))
import SmartTokens.Types.Constants (protocolParamsToken)
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams)
import Wst.Offchain.Scripts (protocolParamsMintingScript,
                             protocolParamsSpendingScript, scriptPolicyIdV3)

protocolParamsTokenC :: C.AssetName
protocolParamsTokenC = unTransAssetName protocolParamsToken

{-| Mint the protocol parameters NFT. Returns NFT's policy ID.
-}
mintProtocolParams :: forall era m. (C.IsBabbageBasedEra era, MonadBuildTx era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBlockchain era m) => ProgrammableLogicGlobalParams -> C.TxIn -> m ()
mintProtocolParams params txIn = Utils.inBabbage @era $ do
  netId <- queryNetworkId
  let
      mintingScript = protocolParamsMintingScript txIn

      policyId = scriptPolicyIdV3 mintingScript

      val = C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toLedgerValue @era C.maryBasedEra
            $ fromList [(C.AssetId policyId protocolParamsTokenC, 1)]

      addr =
        C.makeShelleyAddressInEra
          C.shelleyBasedEra
          netId
          (C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 protocolParamsSpendingScript)
          C.NoStakeAddress

      -- Should contain directoryNodeCS and progLogicCred fields
      dat = C.TxOutDatumInline C.babbageBasedEra $ toHashableScriptData params

      output :: C.TxOut C.CtxTx era
      output = C.TxOut addr val dat C.ReferenceScriptNone

  spendPublicKeyOutput txIn
  mintPlutus mintingScript () protocolParamsTokenC 1
  prependTxOut output

getProtocolParamsGlobalInline :: C.InAnyCardanoEra (C.TxOut C.CtxTx) -> Maybe ProgrammableLogicGlobalParams
getProtocolParamsGlobalInline (C.InAnyCardanoEra _ (C.TxOut _ _ dat _)) =
  case dat of
    C.TxOutDatumInline _era (fromHashableScriptData -> Just d) -> Just d
    _ -> Nothing
