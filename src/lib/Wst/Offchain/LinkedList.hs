

module Wst.Offchain.LinkedList (
  initLinkedList
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
import PlutusLedgerApi.Data.V3 (BuiltinByteString, CurrencySymbol)


initLinkedList :: (MonadBuildTx C.ConwayEra m, P.ToData k) => C.NetworkId -> k -> m ()
initLinkedList netId key = do
  pure ()

insertLinkedList :: (MonadBuildTx C.ConwayEra m, P.ToData k) => C.NetworkId -> k -> m ()
insertLinkedList netId k = do
  pure ()
