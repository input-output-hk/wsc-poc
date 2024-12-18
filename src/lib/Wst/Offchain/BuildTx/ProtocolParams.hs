{-# LANGUAGE OverloadedStrings #-}

module Wst.Offchain.BuildTx.ProtocolParams (
  mintProtocolParams
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Lens (over)
import Convex.BuildTx (MonadBuildTx, addBtx, mintPlutus, spendPublicKeyOutput)
import Convex.CardanoApi.Lenses qualified as L
import Convex.PlutusLedger.V1 (unTransAssetName)
import Convex.Scripts (toHashableScriptData)
import GHC.Exts (IsList (..))
import PlutusLedgerApi.V3 qualified as P
import Wst.Offchain.Scripts (protocolParamsMintingScript,
                             protocolParamsSpendingScript, scriptPolicyIdV3)

protocolParamsToken :: C.AssetName
protocolParamsToken = unTransAssetName $ P.TokenName "ProtocolParamsNFT"

mintProtocolParams :: (MonadBuildTx C.ConwayEra m, P.ToData a) => C.NetworkId -> a -> C.TxIn -> m ()
mintProtocolParams netId d txIn = do
  let
      mintingScript = protocolParamsMintingScript txIn

      val = C.TxOutValueShelleyBased C.ShelleyBasedEraConway $ C.toLedgerValue C.MaryEraOnwardsBabbage
            $ fromList [(C.AssetId (scriptPolicyIdV3 mintingScript) protocolParamsToken, 1)]

      addr =
        C.makeShelleyAddressInEra
          C.ShelleyBasedEraConway
          netId
          (C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 protocolParamsSpendingScript)
          C.NoStakeAddress

      -- Should contain directoryNodeCS and progLogicCred fields
      dat = C.TxOutDatumInline C.BabbageEraOnwardsConway $ toHashableScriptData d

      output :: C.TxOut C.CtxTx C.ConwayEra
      output = C.TxOut addr val dat C.ReferenceScriptNone

  spendPublicKeyOutput txIn
  mintPlutus mintingScript () protocolParamsToken 1
  addBtx (over L.txOuts (output :))
