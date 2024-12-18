{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Wst.Offchain.BuildTx.DirectorySet (
  initDirectorySet,
  insertDirectoryNode,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Lens (over)
import Convex.BuildTx (MonadBuildTx, addBtx, mintPlutus)
import Convex.CardanoApi.Lenses qualified as L
import Convex.PlutusLedger.V1 (unTransAssetName)
import Convex.Scripts (fromHashableScriptData, toHashableScriptData)
import GHC.Exts (IsList (..))
import PlutusLedgerApi.V3 (Credential (..), CurrencySymbol (..))
import PlutusLedgerApi.V3 qualified as P
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (..))
import Wst.Offchain.Scripts (directoryNodeMintingScript,
                             directoryNodeSpendingScript, scriptPolicyIdV3)

-- TODO: Where should this go
directoryNodeToken :: C.AssetName
directoryNodeToken = unTransAssetName $ P.TokenName "DirectoryNodeNFT"

initDirectorySet :: (MonadBuildTx C.ConwayEra m) => C.NetworkId -> C.PolicyId -> C.TxIn -> m ()
initDirectorySet netId paramsPolicyId txIn = do

  let mintingScript = directoryNodeMintingScript txIn

  mintPlutus mintingScript () directoryNodeToken 1

  let
      val = C.TxOutValueShelleyBased C.ShelleyBasedEraConway $ C.toLedgerValue C.MaryEraOnwardsBabbage
            $ fromList [(C.AssetId (scriptPolicyIdV3 mintingScript) directoryNodeToken, 1)]

      addr =
        C.makeShelleyAddressInEra
          C.ShelleyBasedEraConway
          netId
          (C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 $ directoryNodeSpendingScript paramsPolicyId)
          C.NoStakeAddress

      d = DirectorySetNode (CurrencySymbol "") (CurrencySymbol "") (PubKeyCredential "") (PubKeyCredential "")
      dat = C.TxOutDatumInline C.BabbageEraOnwardsConway $ toHashableScriptData d

      output :: C.TxOut C.CtxTx C.ConwayEra
      output = C.TxOut addr val dat C.ReferenceScriptNone

  addBtx (over L.txOuts (output :))

insertDirectoryNode :: (MonadBuildTx C.ConwayEra m) => C.NetworkId -> C.PolicyId -> C.TxIn -> (C.TxIn, C.InAnyCardanoEra (C.TxOut ctx)) -> (CurrencySymbol, Credential, Credential) -> m ()
insertDirectoryNode netId paramsPolicyId initialTxIn (_, firstTxOut) (newKey, transferLogic, issuerLogic) = do

  let
      directoryMintingScript = directoryNodeMintingScript initialTxIn

      (firstTxVal :: C.TxOutValue C.ConwayEra, firstTxData :: DirectorySetNode) = case firstTxOut of
        C.InAnyCardanoEra _ (C.TxOut _ v (C.TxOutDatumInline C.BabbageEraOnwardsConway dat) _) -> case fromHashableScriptData @DirectorySetNode dat of
          Just d -> (v, d)
          Nothing -> error "insertDirectoryNode: invalid datum"
        _ -> error "insertDirectoryNode: invalid output"

      newVal = C.TxOutValueShelleyBased C.ShelleyBasedEraConway $ C.toLedgerValue C.MaryEraOnwardsBabbage
          $ fromList [(C.AssetId (scriptPolicyIdV3 directoryMintingScript) directoryNodeToken, 1)]

      addr =
        C.makeShelleyAddressInEra
          C.ShelleyBasedEraConway
          netId
          (C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 $ directoryNodeSpendingScript paramsPolicyId )
          C.NoStakeAddress

      x = DirectorySetNode
            { key = newKey
            , next = next firstTxData
            , transferLogicScript = transferLogic
            , issuerLogicScript = issuerLogic
            }
      newDat = C.TxOutDatumInline C.BabbageEraOnwardsConway $ toHashableScriptData x

      newOutput :: C.TxOut C.CtxTx C.ConwayEra
      newOutput = C.TxOut addr newVal newDat C.ReferenceScriptNone

      firstDat = firstTxData { next = newKey}
      firstOutput = C.TxOut addr firstTxVal (C.TxOutDatumInline C.BabbageEraOnwardsConway $ toHashableScriptData firstDat) C.ReferenceScriptNone

  mintPlutus directoryMintingScript () directoryNodeToken 1
  addBtx (over L.txOuts (newOutput :))
  addBtx (over L.txOuts (firstOutput :))

-- TODO: is this necessary? It will be for blacklist but not sure about
-- the directory set
removeDirectoryNode = undefined
