{-# LANGUAGE NamedFieldPuns #-}
{-| Look up outputs at script addresses
-}
module Wst.Offchain.Query(
  -- * Queries
  blacklistNodes,
  registryNodes,
  registryNode,
  globalParamsNode,
  issuanceCborHexUTxO,
  programmableLogicOutputs,
  userProgrammableOutputs,

  -- * UTxO with datum
  UTxODat(..),
  fromOutput
) where

import Cardano.Api qualified as C
import Control.Lens qualified as L
import Control.Monad ((>=>))
import Control.Monad.Error.Lens (throwing_)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, asks)
import Convex.CardanoApi.Lenses qualified as L
import Convex.Class (MonadBlockchain (queryNetworkId), MonadUtxoQuery,
                     utxosByPaymentCredential)
import Convex.PlutusLedger.V1 (transCredential, transPolicyId,
                               unTransStakeCredential)
import Convex.Scripts (fromHashableScriptData)
import Convex.Utxos (UtxoSet, toApiUtxo)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.OpenApi.Schema (ToSchema (..))
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import Data.Ord (Down (..))
import Data.Typeable (Typeable)
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import PlutusTx qualified
import SmartTokens.Contracts.IssuanceCborHex (IssuanceCborHex (..))
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams)
import SmartTokens.Types.PTokenDirectory (BlacklistNode, DirectorySetNode (..))
import Wst.AppError (AsProgrammableTokensError (..))
import Wst.JSON.Utils qualified as JSON
import Wst.Offchain.Env (DirectoryEnv (..), HasDirectoryEnv (directoryEnv),
                         HasTransferLogicEnv (transferLogicEnv),
                         TransferLogicEnv (tleBlacklistSpendingScript),
                         blacklistNodePolicyId, directoryNodePolicyId,
                         issuanceCborHexPolicyId,
                         programmableTokenMintingScript, protocolParamsPolicyId)
import Wst.Orphans ()

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

-- | Aeson options for the UTxODat type. Used to derive JSON instances and ToSchema
utxoDatOptions :: JSON.Options
utxoDatOptions = JSON.customJsonOptions 1

instance (C.IsCardanoEra era, ToJSON a) => ToJSON (UTxODat era a) where
  toJSON = JSON.genericToJSON utxoDatOptions
  toEncoding = JSON.genericToEncoding utxoDatOptions

instance (C.IsCardanoEra era, FromJSON a, C.IsShelleyBasedEra era) => FromJSON (UTxODat era a) where
  parseJSON = JSON.genericParseJSON utxoDatOptions

instance (Typeable a, ToSchema a, Typeable era) => ToSchema (UTxODat era a) where
  declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions utxoDatOptions)

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

{-| Find all UTxOs that make up the blacklist
-}
blacklistNodes :: forall era env m. (MonadReader env m, HasTransferLogicEnv env, MonadUtxoQuery m, C.IsBabbageBasedEra era) => m [UTxODat era BlacklistNode]
blacklistNodes = do
  utxosAtBlacklistScript <- asks (C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 . tleBlacklistSpendingScript . transferLogicEnv) >>= fmap (extractUTxO @era) . utxosByPaymentCredential
  blacklistPolicy <- asks (blacklistNodePolicyId . transferLogicEnv)
  pure $ filter (utxoHasPolicyId blacklistPolicy) utxosAtBlacklistScript

userProgrammableOutputs :: forall era env m. (MonadReader env m, HasDirectoryEnv env, MonadUtxoQuery m, C.IsBabbageBasedEra era, MonadBlockchain era m) => C.PaymentCredential -> m [UTxODat era ()]
userProgrammableOutputs userCred = do
  nid <- queryNetworkId
  baseCred <- asks (C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 . dsProgrammableLogicBaseScript . directoryEnv)

  userStakeCred <- either (error . ("Could not unTrans credential: " <>) . show) pure $ unTransStakeCredential $ transCredential userCred
  let expectedAddress = C.makeShelleyAddressInEra C.shelleyBasedEra nid baseCred (C.StakeAddressByValue userStakeCred)
      isUserUtxo UTxODat{uOut=(C.TxOut addr _ _ _)} = addr == expectedAddress

  filter isUserUtxo <$> programmableLogicOutputs

{-| Find the UTxO with the issuance script cbor hex
-}
issuanceCborHexUTxO :: forall era env err m. (MonadReader env m, HasDirectoryEnv env, MonadUtxoQuery m, C.IsBabbageBasedEra era, MonadError err m, AsProgrammableTokensError err) => m (UTxODat era IssuanceCborHex)
issuanceCborHexUTxO = do
  env@DirectoryEnv{dsIssuanceCborHexSpendingScript} <- asks directoryEnv
  let cred   = C.PaymentCredentialByScript . C.hashScript $ C.PlutusScript C.PlutusScriptV3 dsIssuanceCborHexSpendingScript
      hasNft = utxoHasPolicyId (issuanceCborHexPolicyId env)
  utxosByPaymentCredential cred
    >>= maybe (throwing_ _IssuanceCborHexUTxONotFound) pure . listToMaybe . filter hasNft . extractUTxO @era

{-| Find the UTxO with the global params
-}
globalParamsNode :: forall era env err m. (MonadReader env m, HasDirectoryEnv env, MonadUtxoQuery m, C.IsBabbageBasedEra era, MonadError err m, AsProgrammableTokensError err) => m (UTxODat era ProgrammableLogicGlobalParams)
globalParamsNode = do
  env@DirectoryEnv{dsProtocolParamsSpendingScript} <- asks directoryEnv
  let cred   = C.PaymentCredentialByScript . C.hashScript $ C.PlutusScript C.PlutusScriptV3 dsProtocolParamsSpendingScript
      hasNft = utxoHasPolicyId (protocolParamsPolicyId env)
  utxosByPaymentCredential cred
    >>= maybe (throwing_ _GlobalParamsNodeNotFound) pure . listToMaybe . filter hasNft . extractUTxO @era

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

