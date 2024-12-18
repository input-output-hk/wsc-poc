

module Wst.Offchain.BuildTx.LinkedList (
  initLinkedList,
  insertLinkedList
) where

import Cardano.Api qualified as C
import Convex.BuildTx (MonadBuildTx)


initLinkedList :: (MonadBuildTx C.ConwayEra m) => C.NetworkId -> k -> m ()
initLinkedList _netId _key = do
  pure ()

insertLinkedList :: (MonadBuildTx C.ConwayEra m) => C.NetworkId -> k -> m ()
insertLinkedList _netId _k = do
  pure ()
