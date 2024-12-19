{-| Look up outputs at script addresses
-}
module Wst.Offchain.Endpoints.Query(
  registryNodes,
  globalParamsNode
) where

import Cardano.Api qualified as C
import Control.Monad.Reader (MonadReader, asks)
import Convex.Class (MonadUtxoQuery, utxosByPaymentCredential)
import Convex.Utxos (toApiUtxo)
import Data.Map qualified as Map
import Wst.Offchain.Endpoints.Deployment (DeploymentScripts (dsDirectorySpendingScript))
import Wst.Offchain.Scripts (protocolParamsSpendingScript)

-- TODO: We should probably filter the UTxOs to check that they have the correct NFTs,
--       and we should parse the inline datums

{-| Find all UTxOs that make up the registry
-}
registryNodes :: forall era m. (MonadReader DeploymentScripts m, MonadUtxoQuery m, C.IsBabbageBasedEra era) => m [(C.TxIn, C.TxOut C.CtxUTxO era)]
registryNodes =
  asks (C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 . dsDirectorySpendingScript)
    >>= fmap (Map.toList . C.unUTxO . toApiUtxo @era) . utxosByPaymentCredential

{-| Find the UTxO with the global params
-}
globalParamsNode :: forall era m. (MonadReader DeploymentScripts m, MonadUtxoQuery m, C.IsBabbageBasedEra era) => m [(C.TxIn, C.TxOut C.CtxUTxO era)]
globalParamsNode = do
  let cred = C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 $ protocolParamsSpendingScript
  fmap (Map.toList . C.unUTxO . toApiUtxo @era) $ utxosByPaymentCredential cred
