{-# LANGUAGE OverloadedStrings #-}

module Wst.Offchain.ProtocolParams (
  mintProtocolParams
) where

import Cardano.Api qualified as C
import PlutusLedgerApi.V3 qualified as P
import Convex.BuildTx (MonadBuildTx, mintPlutus, spendPublicKeyOutput, addBtx)
import Convex.PlutusLedger (unTransAssetName)
import qualified Cardano.Api.Shelley as C
import Convex.CardanoApi.Lenses qualified as L
import GHC.Exts (IsList(..))
import Convex.Scripts (toHashableScriptData)
import Control.Lens (over)


protocolParamsMintingScript :: C.PlutusScript C.PlutusScriptV3
protocolParamsMintingScript = undefined

protocolParamsToken :: C.AssetName
protocolParamsToken = unTransAssetName $ P.TokenName "ProtocolParamsNFT"

alwaysFailScript :: C.PlutusScript C.PlutusScriptV3
alwaysFailScript = undefined

paramScriptPolicy :: C.PolicyId
paramScriptPolicy = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV3 alwaysFailScript

mintProtocolParams :: (MonadBuildTx C.ConwayEra m, P.ToData a) => C.NetworkId -> a -> C.TxIn -> m ()
mintProtocolParams netId d txIn = do
  let 
      val = C.TxOutValueShelleyBased C.ShelleyBasedEraConway $ C.toLedgerValue C.MaryEraOnwardsBabbage 
            $ fromList [(C.AssetId paramScriptPolicy protocolParamsToken, 1)]

      addr = 
        C.makeShelleyAddressInEra 
          C.ShelleyBasedEraConway 
          netId 
          (C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 alwaysFailScript) 
          C.NoStakeAddress

      -- Should contain directoryNodeCS and progLogicCred fields
      dat = C.TxOutDatumInline C.BabbageEraOnwardsConway $ toHashableScriptData d

      output :: C.TxOut C.CtxTx C.ConwayEra
      output = C.TxOut addr val dat C.ReferenceScriptNone

  spendPublicKeyOutput txIn
  mintPlutus protocolParamsMintingScript () protocolParamsToken 1
  addBtx (over L.txOuts (output :))
