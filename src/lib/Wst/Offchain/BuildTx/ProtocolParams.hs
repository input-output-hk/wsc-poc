{-# LANGUAGE OverloadedStrings #-}

module Wst.Offchain.BuildTx.ProtocolParams (
  mintProtocolParams
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Convex.BuildTx (MonadBuildTx, mintPlutus, prependTxOut,
                       spendPublicKeyOutput)
import Convex.Class (MonadBlockchain (..))
import Convex.PlutusLedger.V1 (unTransAssetName)
import Convex.Scripts (toHashableScriptData)
import Convex.Utils qualified as Utils
import GHC.Exts (IsList (..))
import SmartTokens.Types.Constants (protocolParamsToken)
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams)
import Wst.Offchain.Scripts (protocolParamsMintingScript,
                             protocolParamsSpendingScript, scriptPolicyIdV3)

protocolParamsTokenC :: C.AssetName
protocolParamsTokenC = unTransAssetName protocolParamsToken

mintProtocolParams :: forall era m. (C.IsBabbageBasedEra era, MonadBuildTx era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBlockchain era m) => ProgrammableLogicGlobalParams -> C.TxIn -> m ()
mintProtocolParams params txIn = Utils.inBabbage @era $ do
  netId <- queryNetworkId
  let
      mintingScript = protocolParamsMintingScript txIn

      val = C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toLedgerValue @era C.maryBasedEra
            $ fromList [(C.AssetId (scriptPolicyIdV3 mintingScript) protocolParamsTokenC, 1)]

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
