{-| Look up outputs at script addresses
-}
module Wst.Offchain.Query(
  -- * Queries
  blacklistNodes,

  -- * UTxO with datum
  UTxODat(..),
  fromOutput
) where

import Cardano.Api qualified as C
import Control.Lens qualified as L
import Control.Monad.Reader (MonadReader, asks)
import Convex.CardanoApi.Lenses qualified as L
import Convex.Class (MonadUtxoQuery, utxosByPaymentCredential)
import GHC.Exts (IsList (..))
import ProgrammableTokens.OffChain.Orphans ()
import ProgrammableTokens.OffChain.UTxODat (UTxODat (..), extractUTxO,
                                            fromOutput)
import SmartTokens.Types.PTokenDirectory (BlacklistNode)
import Wst.Offchain.Env (BlacklistEnv (bleSpendingScript), HasBlacklistEnv (..),
                         blacklistNodePolicyId)

-- TODO: We should probably filter the UTxOs to check that they have the correct NFTs

{-| Find all UTxOs that make up the blacklist
-}
blacklistNodes :: forall era env m. (MonadReader env m, HasBlacklistEnv env, MonadUtxoQuery m, C.IsBabbageBasedEra era) => m [UTxODat era BlacklistNode]
blacklistNodes = do
  utxosAtBlacklistScript <- asks (C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 . bleSpendingScript . blacklistEnv) >>= fmap (extractUTxO @era) . utxosByPaymentCredential
  blacklistPolicy <- asks (blacklistNodePolicyId . blacklistEnv)
  pure $ filter (utxoHasPolicyId blacklistPolicy) utxosAtBlacklistScript

extractValue :: C.IsBabbageBasedEra era => C.TxOut C.CtxUTxO era -> C.Value
extractValue = L.view $ L._TxOut . L._2 . L._TxOutValue

hasPolicyId :: C.PolicyId -> C.Value -> Bool
hasPolicyId policyId val =
  let isPolicy :: (C.AssetId, C.Quantity) -> Bool
      isPolicy (C.AssetId pid _, _) = pid == policyId
      isPolicy _ = False
  in any isPolicy (toList val)

utxoHasPolicyId :: C.IsBabbageBasedEra era => C.PolicyId -> UTxODat era a -> Bool
utxoHasPolicyId policyId txoD = hasPolicyId policyId $ extractValue (uOut txoD)

