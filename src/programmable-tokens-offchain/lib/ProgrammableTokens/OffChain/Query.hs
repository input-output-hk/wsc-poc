module ProgrammableTokens.OffChain.Query(
  -- * CIP-143 registry queries
  registryNodes,
  registryNode

) where

import Cardano.Api qualified as C
import Control.Lens qualified as L
import Control.Monad.Error.Lens (throwing_)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, asks)
import Convex.CardanoApi.Lenses qualified as L
import Convex.Class (MonadUtxoQuery, utxosByPaymentCredential)
import Convex.PlutusLedger.V1 (transPolicyId)
import Data.List (sortOn)
import Data.Ord (Down (..))
import GHC.Exts (IsList (..))
import ProgrammableTokens.OffChain.Env (DirectoryEnv (..), HasDirectoryEnv (..),
                                        HasTransferLogicEnv (..),
                                        directoryNodePolicyId,
                                        programmableTokenMintingScript)
import ProgrammableTokens.OffChain.Error (AsProgrammableTokensError (..))
import ProgrammableTokens.OffChain.UTxODat (UTxODat (..), extractUTxO)
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (..))

{-| Find all UTxOs that make up the registry
-}
registryNodes :: forall era env m. (MonadReader env m, HasDirectoryEnv env, MonadUtxoQuery m, C.IsBabbageBasedEra era) => m [UTxODat era DirectorySetNode]
registryNodes = do
  utxosAtDirectoryScript <- asks (C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 . dsDirectorySpendingScript . directoryEnv) >>= fmap (extractUTxO @era) . utxosByPaymentCredential
  registryPolicy <- asks (directoryNodePolicyId . directoryEnv)
  pure $ filter (utxoHasPolicyId registryPolicy) utxosAtDirectoryScript

-- | Find the specific directory set node for the policy what we're working with.
registryNode :: forall era env err m. (MonadUtxoQuery m, C.IsBabbageBasedEra era, HasDirectoryEnv env, MonadReader env m, HasTransferLogicEnv env, MonadError err m, AsProgrammableTokensError err) => m (UTxODat era DirectorySetNode)
registryNode = do
  directoryList <- registryNodes @era
  dir <- asks directoryEnv
  inta <- asks transferLogicEnv

  let mintingScript = programmableTokenMintingScript dir inta
      issuedPolicyId = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV3 mintingScript
      issuedSymbol = transPolicyId issuedPolicyId
  let udats =
        sortOn (Down . key . uDatum)
        $
          filter ((<= issuedSymbol) . key . uDatum) directoryList -- TODO: Should this be equality instead of LEQ?
  case udats of
    [] -> throwing_ _DirectorySetNodeNotFound
    x : _ -> pure x

utxoHasPolicyId :: C.IsBabbageBasedEra era => C.PolicyId -> UTxODat era a -> Bool
utxoHasPolicyId policyId txoD = hasPolicyId policyId $ extractValue (uOut txoD)

hasPolicyId :: C.PolicyId -> C.Value -> Bool
hasPolicyId policyId val =
  let isPolicy :: (C.AssetId, C.Quantity) -> Bool
      isPolicy (C.AssetId pid _, _) = pid == policyId
      isPolicy _ = False
  in any isPolicy (toList val)

extractValue :: C.IsBabbageBasedEra era => C.TxOut C.CtxUTxO era -> C.Value
extractValue = L.view $ L._TxOut . L._2 . L._TxOutValue
