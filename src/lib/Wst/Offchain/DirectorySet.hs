{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Wst.Offchain.DirectorySet (
  initDirectorySet,
  insertDirectoryNode
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


directoryNodeMintingScript :: C.TxIn -> C.PlutusScript C.PlutusScriptV3
directoryNodeMintingScript txIn = undefined

directoryMintingPolicy :: C.TxIn -> C.PolicyId
directoryMintingPolicy = C.scriptPolicyId . C.PlutusScript C.PlutusScriptV3 . directoryNodeMintingScript


directoryNodeToken :: C.AssetName
directoryNodeToken = unTransAssetName $ P.TokenName "DirectoryNodeNFT"

directoryNodeSpendingScript :: C.Hash C.PaymentKey -> C.SimpleScript
directoryNodeSpendingScript = C.RequireSignature

data DirectorySetNode = DirectorySetNode
  { key :: CurrencySymbol
  , next :: CurrencySymbol
  , transferLogicScript  :: Credential
  , issuerLogicScript :: Credential
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

initDirectorySet :: (MonadBuildTx C.ConwayEra m) => C.NetworkId -> C.Hash C.PaymentKey -> C.TxIn -> m ()
initDirectorySet netId operatorHash txIn = do

  spendPublicKeyOutput txIn
  mintPlutus (directoryNodeMintingScript txIn) () directoryNodeToken 1

  let
      val = C.TxOutValueShelleyBased C.ShelleyBasedEraConway $ C.toLedgerValue C.MaryEraOnwardsBabbage
            $ fromList [(C.AssetId (directoryMintingPolicy txIn) directoryNodeToken, 1)]

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
insertDirectoryNode netId operatorHash (afterTxIn, afterTxOut) (newKey, transferLogic, issuerLogic) = do

  let
      (afterTxVal :: C.TxOutValue C.ConwayEra, afterTxData :: DirectorySetNode) = case afterTxOut of
        C.InAnyCardanoEra _ (C.TxOut _ v (C.TxOutDatumInline C.BabbageEraOnwardsConway dat) _) -> case fromHashableScriptData @DirectorySetNode dat of
          Just d -> (v, d)
          Nothing -> error "insertDirectoryNode: invalid datum"
        _ -> error "insertDirectoryNode: invalid output"

  spendPublicKeyOutput afterTxIn
  mintPlutus (directoryNodeMintingScript afterTxIn) () directoryNodeToken 1

  let
      newVal = C.TxOutValueShelleyBased C.ShelleyBasedEraConway $ C.toLedgerValue C.MaryEraOnwardsBabbage
          $ fromList [(C.AssetId (directoryMintingPolicy afterTxIn) directoryNodeToken, 1)]

      addr =
        C.makeShelleyAddressInEra
          C.ShelleyBasedEraConway
          netId
          (C.PaymentCredentialByScript $ C.hashScript $ C.SimpleScript $ directoryNodeSpendingScript operatorHash)
          C.NoStakeAddress

      x = DirectorySetNode
            { key = newKey
            , next = next afterTxData
            , transferLogicScript = transferLogic
            , issuerLogicScript = issuerLogic
            }
      newDat = C.TxOutDatumInline C.BabbageEraOnwardsConway $ toHashableScriptData x

      newOutput :: C.TxOut C.CtxTx C.ConwayEra
      newOutput = C.TxOut addr newVal newDat C.ReferenceScriptNone

      modifiedDat = afterTxData { next = transPolicyId $ directoryMintingPolicy afterTxIn }
      modifiedOutput = C.TxOut addr afterTxVal (C.TxOutDatumInline C.BabbageEraOnwardsConway $ toHashableScriptData modifiedDat) C.ReferenceScriptNone

  addBtx (over L.txOuts (newOutput :))
  addBtx (over L.txOuts (modifiedOutput :))
