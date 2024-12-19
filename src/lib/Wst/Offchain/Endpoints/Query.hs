{-# LANGUAGE NamedFieldPuns #-}
{-| Look up outputs at script addresses
-}
module Wst.Offchain.Endpoints.Query(
  UTxO(..),
  registryNodes,
  globalParamsNode
) where

import Cardano.Api qualified as C
import Control.Lens qualified as L
import Control.Monad ((>=>))
import Control.Monad.Reader (MonadReader, asks)
import Convex.CardanoApi.Lenses qualified as L
import Convex.Class (MonadUtxoQuery, utxosByPaymentCredential)
import Convex.Scripts (fromHashableScriptData)
import Convex.Utxos (UtxoSet, toApiUtxo)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import PlutusTx qualified
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams)
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (..))
import Wst.Offchain.Endpoints.Deployment (DeploymentScripts (dsDirectorySpendingScript))
import Wst.Offchain.Scripts (protocolParamsSpendingScript)

-- TODO: We should probably filter the UTxOs to check that they have the correct NFTs

{-| Unspent transaction output with 'TxIn', 'TxOut' and an inline datum
-}
data UTxO era a =
  UTxO
    { uIn    :: C.TxIn
    , uOut   :: C.TxOut C.CtxUTxO era
    , uDatum :: a
    }

{-| Find all UTxOs that make up the registry
-}
registryNodes :: forall era m. (MonadReader DeploymentScripts m, MonadUtxoQuery m, C.IsBabbageBasedEra era) => m [UTxO era DirectorySetNode]
registryNodes =
  asks (C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 . dsDirectorySpendingScript)
    >>= fmap (extractUTxO @era) . utxosByPaymentCredential

{-| Find the UTxO with the global params
-}
globalParamsNode :: forall era m. (MonadReader DeploymentScripts m, MonadUtxoQuery m, C.IsBabbageBasedEra era) => m [UTxO era ProgrammableLogicGlobalParams]
globalParamsNode = do
  let cred = C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 $ protocolParamsSpendingScript
  fmap (extractUTxO @era) (utxosByPaymentCredential cred)

fromOutput :: (PlutusTx.FromData a, C.IsBabbageBasedEra era) => C.TxIn -> C.TxOut C.CtxUTxO era -> Maybe (UTxO era a)
fromOutput uIn uOut@(L.preview (L._TxOut . L._3 . L._TxOutDatumInline) >=> fromHashableScriptData -> Just uDatum) = Just UTxO{uIn, uOut, uDatum}
fromOutput _ _ = Nothing

extractUTxO :: forall era a b. (PlutusTx.FromData a, C.IsBabbageBasedEra era) => UtxoSet C.CtxUTxO b -> [UTxO era a]
extractUTxO = mapMaybe (uncurry fromOutput) . Map.toList . C.unUTxO . toApiUtxo @era
