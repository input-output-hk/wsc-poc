

module Wst.Offchain.BuildTx.LinkedList (
  initLinkedList
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Lens (over)
import Convex.BuildTx (MonadBuildTx, addBtx, mintPlutus, spendPublicKeyOutput)
import Convex.CardanoApi.Lenses qualified as L
import Convex.Scripts (toHashableScriptData)
import GHC.Exts (IsList (..))
import PlutusLedgerApi.Data.V3 (BuiltinByteString, CurrencySymbol)
import PlutusLedgerApi.V3 qualified as P


initLinkedList :: (MonadBuildTx C.ConwayEra m, P.ToData k) => C.NetworkId -> k -> m ()
initLinkedList netId key = do
  pure ()

insertLinkedList :: (MonadBuildTx C.ConwayEra m, P.ToData k) => C.NetworkId -> k -> m ()
insertLinkedList netId k = do
  pure ()
