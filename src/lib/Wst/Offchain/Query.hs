{-# LANGUAGE NamedFieldPuns #-}
{-| Look up outputs at script addresses
-}
module Wst.Offchain.Query(
  -- * Queries
  blacklistNodes,
  registryNodes,
  globalParamsNode,
  programmableLogicOutputs,

  -- * UTxO with datum
  UTxODat(..),
  fromOutput
) where

import Cardano.Api qualified as C
import Control.Lens qualified as L
import Control.Monad ((>=>))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, asks)
import Convex.CardanoApi.Lenses qualified as L
import Convex.Class (MonadUtxoQuery, utxosByPaymentCredential)
import Convex.Scripts (fromHashableScriptData)
import Convex.Utxos (UtxoSet, toApiUtxo)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import PlutusTx qualified
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams)
import SmartTokens.Types.PTokenDirectory (BlacklistNode, DirectorySetNode (..))
import Wst.AppError (AppError (GlobalParamsNodeNotFound))
import Wst.Offchain.Env (DirectoryEnv (dsDirectorySpendingScript, dsProgrammableLogicBaseScript),
                         HasDirectoryEnv (directoryEnv),
                         HasTransferLogicEnv (transferLogicEnv),
                         TransferLogicEnv (tleBlacklistSpendingScript))
import Wst.Offchain.Scripts (protocolParamsSpendingScript)

-- TODO: We should probably filter the UTxOs to check that they have the correct NFTs

{-| Unspent transaction output with 'TxIn', 'TxOut' and an inline datum
-}
data UTxODat era a =
  UTxODat
    { uIn    :: C.TxIn
    , uOut   :: C.TxOut C.CtxUTxO era
    , uDatum :: a
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

{-| Find all UTxOs that make up the registry
-}
registryNodes :: forall era env m. (MonadReader env m, HasDirectoryEnv env, MonadUtxoQuery m, C.IsBabbageBasedEra era) => m [UTxODat era DirectorySetNode]
registryNodes =
  asks (C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 . dsDirectorySpendingScript . directoryEnv)
    >>= fmap (extractUTxO @era) . utxosByPaymentCredential

{-| Find all UTxOs that make up the blacklist
-}
blacklistNodes :: forall era env m. (MonadReader env m, HasTransferLogicEnv env, MonadUtxoQuery m, C.IsBabbageBasedEra era) => m [UTxODat era BlacklistNode]
blacklistNodes =
  asks (C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 . tleBlacklistSpendingScript . transferLogicEnv)
    >>= fmap (extractUTxO @era) . utxosByPaymentCredential

{-| Find the UTxO with the global params
-}
globalParamsNode :: forall era m. (MonadUtxoQuery m, C.IsBabbageBasedEra era, MonadError (AppError era) m) => m (UTxODat era ProgrammableLogicGlobalParams)
globalParamsNode = do
  let cred = C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 $ protocolParamsSpendingScript
  utxosByPaymentCredential cred
    >>= maybe (throwError GlobalParamsNodeNotFound) pure . listToMaybe . extractUTxO @era

{-| Outputs that are locked by the programmable logic base script.
-}
programmableLogicOutputs :: forall era env m. (MonadReader env m, HasDirectoryEnv env, MonadUtxoQuery m, C.IsBabbageBasedEra era) => m [UTxODat era ()]
programmableLogicOutputs = do
  asks (C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 . dsProgrammableLogicBaseScript . directoryEnv)
    >>= fmap (extractUtxoNoDatum @era) . utxosByPaymentCredential

fromOutputNoDatum :: forall era. (C.IsBabbageBasedEra era) => C.TxIn -> C.TxOut C.CtxUTxO era -> Maybe (UTxODat era ())
fromOutputNoDatum uIn uOut@(L.preview (L._TxOut . L._3 . L._TxOutDatumInline) >=> fromHashableScriptData -> Just ()) = Just UTxODat{uIn, uOut, uDatum = ()}
fromOutputNoDatum uIn uOut = Just $ UTxODat{uIn, uOut, uDatum = ()}

extractUtxoNoDatum :: forall era b. (C.IsBabbageBasedEra era) => UtxoSet C.CtxUTxO b -> [UTxODat era ()]
extractUtxoNoDatum = mapMaybe (uncurry fromOutputNoDatum) . Map.toList . C.unUTxO . toApiUtxo @era

fromOutput :: forall era a. (PlutusTx.FromData a, C.IsBabbageBasedEra era) => C.TxIn -> C.TxOut C.CtxUTxO era -> Maybe (UTxODat era a)
fromOutput uIn uOut@(L.preview (L._TxOut . L._3 . L._TxOutDatumInline) >=> fromHashableScriptData -> Just uDatum) = Just UTxODat{uIn, uOut, uDatum}
fromOutput _ _ = Nothing

extractUTxO :: forall era a b. (PlutusTx.FromData a, C.IsBabbageBasedEra era) => UtxoSet C.CtxUTxO b -> [UTxODat era a]
extractUTxO = mapMaybe (uncurry fromOutput) . Map.toList . C.unUTxO . toApiUtxo @era
