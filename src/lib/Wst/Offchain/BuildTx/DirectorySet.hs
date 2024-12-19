{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Wst.Offchain.BuildTx.DirectorySet (
  initDirectorySet,
  insertDirectoryNode,
  -- * Values
  initialNode
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Lens (over)
import Convex.BuildTx (MonadBuildTx, addBtx, mintPlutus)
import Convex.CardanoApi.Lenses qualified as L
import Convex.Class (MonadBlockchain, queryNetworkId)
import Convex.PlutusLedger.V1 (transStakeCredential, unTransAssetName)
import Convex.Scripts (fromHashableScriptData, toHashableScriptData)
import Convex.Utils qualified as Utils
import GHC.Exts (IsList (..))
import PlutusLedgerApi.V3 (Credential (..), CurrencySymbol (..), TokenName (..))
import SmartTokens.LinkedList.MintDirectory (DirectoryNodeAction (..))
import SmartTokens.Types.Constants (directoryNodeToken)
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (..))
import Wst.Offchain.Scripts (directoryNodeMintingScript,
                             directoryNodeSpendingScript, scriptPolicyIdV3)

initialNode :: DirectorySetNode
initialNode = DirectorySetNode (CurrencySymbol "") (CurrencySymbol "") (PubKeyCredential "") (PubKeyCredential "")

initDirectorySet :: forall era m. (C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => C.PolicyId -> C.TxIn -> m ()
initDirectorySet paramsPolicyId txIn = Utils.inBabbage @era $ do
  netId <- queryNetworkId
  let mintingScript = directoryNodeMintingScript txIn

  mintPlutus mintingScript InitDirectory (unTransAssetName directoryNodeToken) 1

  let
      val = C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toLedgerValue @era C.maryBasedEra
            $ fromList [(C.AssetId (scriptPolicyIdV3 mintingScript) (unTransAssetName directoryNodeToken), 1)]

      addr =
        C.makeShelleyAddressInEra
          C.shelleyBasedEra
          netId
          (C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 $ directoryNodeSpendingScript paramsPolicyId)
          C.NoStakeAddress

      dat = C.TxOutDatumInline C.babbageBasedEra $ toHashableScriptData initialNode

      output :: C.TxOut C.CtxTx era
      output = C.TxOut addr val dat C.ReferenceScriptNone

  addBtx (over L.txOuts (output :))

insertDirectoryNode :: forall era m ctx. (C.IsBabbageBasedEra era, MonadBuildTx era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBlockchain era m) => C.PolicyId -> C.TxIn -> (C.TxIn, C.TxOut ctx era) -> (CurrencySymbol, C.StakeCredential, C.StakeCredential) -> m ()
insertDirectoryNode paramsPolicyId initialTxIn (_, firstTxOut) (newKey, transferLogic, issuerLogic) = Utils.inBabbage @era $ do
  netId <- queryNetworkId

  let
      directoryMintingScript = directoryNodeMintingScript initialTxIn

      firstTxVal :: C.TxOutValue era
      (firstTxVal, firstTxData) = case firstTxOut of
        (C.TxOut _ v (C.TxOutDatumInline _ dat) _) -> case fromHashableScriptData @DirectorySetNode dat of
          Just d -> (v, d)
          Nothing -> error "insertDirectoryNode: invalid datum"
        _ -> error "insertDirectoryNode: invalid output"

      newVal = C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toLedgerValue @era C.maryBasedEra
          $ fromList [(C.AssetId (scriptPolicyIdV3 directoryMintingScript) (unTransAssetName directoryNodeToken), 1)]

      addr =
        C.makeShelleyAddressInEra
          C.shelleyBasedEra
          netId
          (C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 $ directoryNodeSpendingScript paramsPolicyId )
          C.NoStakeAddress

      dsn = DirectorySetNode
            { key = newKey
            , next = next firstTxData
            , transferLogicScript = transStakeCredential transferLogic
            , issuerLogicScript = transStakeCredential issuerLogic
            }
      newDat = C.TxOutDatumInline C.babbageBasedEra $ toHashableScriptData dsn

      newOutput = C.TxOut addr newVal newDat C.ReferenceScriptNone

      firstDat = firstTxData { next = newKey}
      firstOutput = C.TxOut addr firstTxVal (C.TxOutDatumInline C.babbageBasedEra $ toHashableScriptData firstDat) C.ReferenceScriptNone

  mintPlutus directoryMintingScript (InsertDirectoryNode newKey) (unTransAssetName directoryNodeToken) 1
  addBtx (over L.txOuts (newOutput :))
  addBtx (over L.txOuts (firstOutput :))

-- TODO: is this necessary? It will be for blacklist but not sure about
-- the directory set
removeDirectoryNode = undefined
