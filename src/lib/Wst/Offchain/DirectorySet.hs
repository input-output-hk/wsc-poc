{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Wst.Offchain.DirectorySet (
  initDirectorySet,
  insertDirectoryNode,
  DirectorySetNode (..)
) where

import Cardano.Api qualified as C
import PlutusLedgerApi.V3 qualified as P
import PlutusLedgerApi.V3 ( CurrencySymbol(..), Credential(..) )
import Convex.BuildTx (MonadBuildTx, mintPlutus, spendPublicKeyOutput, addBtx)
import Convex.PlutusLedger (unTransAssetName, transPolicyId)
import qualified Cardano.Api.Shelley as C
import Convex.CardanoApi.Lenses qualified as L
import GHC.Exts (IsList(..))
import Convex.Scripts (toHashableScriptData, fromHashableScriptData)
import Control.Lens (over)
import GHC.Generics (Generic)
import qualified PlutusTx


-- TODO: To be imported after merge with onchain
directoryNodeMintingScript :: C.PlutusScript C.PlutusScriptV3
directoryNodeMintingScript = undefined

directoryMintingPolicy :: C.PolicyId
directoryMintingPolicy = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV3 directoryNodeMintingScript

-- TODO: To be imported after merge with onchain
directoryNodeToken :: C.AssetName
directoryNodeToken = unTransAssetName $ P.TokenName "DirectoryNodeNFT"

-- TODO: There might be more appropriate script logic, this should suffice for
-- the time being
directoryNodeSpendingScript :: C.Hash C.PaymentKey -> C.SimpleScript
directoryNodeSpendingScript = C.RequireSignature

-- TODO: This is defined in onchain PR, to be replaced/imported
data DirectorySetNode = DirectorySetNode
  { key :: CurrencySymbol
  , next :: CurrencySymbol
  , transferLogicScript  :: Credential
  , issuerLogicScript :: Credential
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

initDirectorySet :: (MonadBuildTx C.ConwayEra m) => C.NetworkId -> C.Hash C.PaymentKey -> m ()
initDirectorySet netId operatorHash = do

  mintPlutus directoryNodeMintingScript () directoryNodeToken 1

  let
      val = C.TxOutValueShelleyBased C.ShelleyBasedEraConway $ C.toLedgerValue C.MaryEraOnwardsBabbage
            $ fromList [(C.AssetId directoryMintingPolicy directoryNodeToken, 1)]

      addr =
        C.makeShelleyAddressInEra
          C.ShelleyBasedEraConway
          netId
          (C.PaymentCredentialByScript $ C.hashScript $ C.SimpleScript $ directoryNodeSpendingScript operatorHash)
          C.NoStakeAddress

      d = DirectorySetNode (CurrencySymbol "") (CurrencySymbol "") (PubKeyCredential "") (PubKeyCredential "")
      dat = C.TxOutDatumInline C.BabbageEraOnwardsConway $ toHashableScriptData d

      output :: C.TxOut C.CtxTx C.ConwayEra
      output = C.TxOut addr val dat C.ReferenceScriptNone

  addBtx (over L.txOuts (output :))

insertDirectoryNode :: (MonadBuildTx C.ConwayEra m) => C.NetworkId -> C.Hash C.PaymentKey -> (C.TxIn, C.InAnyCardanoEra (C.TxOut ctx)) -> (CurrencySymbol, Credential, Credential) -> m ()
insertDirectoryNode netId operatorHash (firstTxIn, firstTxOut) (newKey, transferLogic, issuerLogic) = do

  let
      (firstTxVal :: C.TxOutValue C.ConwayEra, firstTxData :: DirectorySetNode) = case firstTxOut of
        C.InAnyCardanoEra _ (C.TxOut _ v (C.TxOutDatumInline C.BabbageEraOnwardsConway dat) _) -> case fromHashableScriptData @DirectorySetNode dat of
          Just d -> (v, d)
          Nothing -> error "insertDirectoryNode: invalid datum"
        _ -> error "insertDirectoryNode: invalid output"

  mintPlutus directoryNodeMintingScript () directoryNodeToken 1

  let
      newVal = C.TxOutValueShelleyBased C.ShelleyBasedEraConway $ C.toLedgerValue C.MaryEraOnwardsBabbage
          $ fromList [(C.AssetId directoryMintingPolicy directoryNodeToken, 1)]

      addr =
        C.makeShelleyAddressInEra
          C.ShelleyBasedEraConway
          netId
          (C.PaymentCredentialByScript $ C.hashScript $ C.SimpleScript $ directoryNodeSpendingScript operatorHash)
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

  addBtx (over L.txOuts (newOutput :))
  addBtx (over L.txOuts (firstOutput :))

-- TODO: is this even necessary? It will be for blacklist but not sure about
-- the directory set
removeDirectoryNode = undefined
