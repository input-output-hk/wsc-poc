{-# LANGUAGE NamedFieldPuns #-}
module ProgrammableTokens.OffChain.Query(
  -- * CIP-143 registry queries
  registryNodes,

  NodeMatch(..),
  registryNode,
  registryNodeForInsertion,
  registryNodeForReference,
  registryNodeForReferenceOrInsertion,

  userProgrammableOutputs,
  issuanceCborHexUTxO,
  globalParamsNode,
  programmableLogicOutputs,
  selectProgammableOutputsFor,

  utxoHasPolicyId,
  hasPolicyId,
  extractValue,
  userTotalProgrammableValue,
) where

import Cardano.Api qualified as C
import Control.Lens qualified as L
import Control.Monad.Error.Lens (throwing, throwing_)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, asks)
import Convex.CardanoApi.Lenses qualified as L
import Convex.Class (MonadBlockchain (..), MonadUtxoQuery,
                     utxosByPaymentCredential)
import Convex.PlutusLedger.V1 (transCredential, transPolicyId, unTransPolicyId,
                               unTransStakeCredential)
import Convex.Utxos (UtxoSet (UtxoSet))
import Convex.Wallet (selectMixedInputsCovering)
import Data.List (sortOn)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import GHC.Exts (IsList (..))
import ProgrammableTokens.OffChain.Env.Directory (DirectoryEnv (..),
                                                  HasDirectoryEnv (..),
                                                  directoryNodePolicyId,
                                                  issuanceCborHexPolicyId,
                                                  protocolParamsPolicyId)
import ProgrammableTokens.OffChain.Env.TransferLogic (HasTransferLogicEnv (..),
                                                      programmableTokenMintingScript,
                                                      programmableTokenPolicyId)
import ProgrammableTokens.OffChain.Error (AsProgrammableTokensError (..))
import ProgrammableTokens.OffChain.UTxODat (UTxODat (..), extractUTxO,
                                            extractUtxoNoDatum)
import SmartTokens.Contracts.IssuanceCborHex (IssuanceCborHex (..))
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams)
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (..))

{-| Find all UTxOs that make up the registry
-}
registryNodes :: forall era env m. (MonadReader env m, HasDirectoryEnv env, MonadUtxoQuery m, C.IsBabbageBasedEra era) => m [UTxODat era DirectorySetNode]
registryNodes = do
  utxosAtDirectoryScript <- asks (C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 . dsDirectorySpendingScript . directoryEnv) >>= fmap (extractUTxO @era) . utxosByPaymentCredential
  registryPolicy <- asks (directoryNodePolicyId . directoryEnv)
  pure $ filter (utxoHasPolicyId registryPolicy) utxosAtDirectoryScript

-- |
data NodeMatch
  = MustBeEqual -- ^ The node's key must equal the issued symbol of the TransferLogicEnv
  | MustNotBeEqual -- ^ The node's key must not equal the issued symbol of the TransferLogicEnv.
  | GreaterThanOrEqual
  deriving stock (Eq, Ord, Show)

operator :: Ord a => NodeMatch -> a -> a -> Bool
operator = \case
  MustBeEqual -> (==)
  MustNotBeEqual -> (>)
  GreaterThanOrEqual -> (>=)

-- | Find the directory set node that we need to spend in order to insert a new node with our policy into the list
registryNodeForInsertion :: forall era env err m. (MonadUtxoQuery m, C.IsBabbageBasedEra era, HasDirectoryEnv env, MonadReader env m, HasTransferLogicEnv env, MonadError err m, AsProgrammableTokensError err) => m (UTxODat era DirectorySetNode)
registryNodeForInsertion = registryNode MustNotBeEqual

-- | Find the directory set node that has our policy
registryNodeForReference :: forall era env err m. (MonadUtxoQuery m, C.IsBabbageBasedEra era, HasDirectoryEnv env, MonadReader env m, HasTransferLogicEnv env, MonadError err m, AsProgrammableTokensError err) => m (UTxODat era DirectorySetNode)
registryNodeForReference = registryNode MustBeEqual

-- | Find the directory set node that has our policy
registryNodeForReferenceOrInsertion :: forall era env err m. (MonadUtxoQuery m, C.IsBabbageBasedEra era, HasDirectoryEnv env, MonadReader env m, HasTransferLogicEnv env, MonadError err m, AsProgrammableTokensError err) => m (UTxODat era DirectorySetNode)
registryNodeForReferenceOrInsertion = registryNode GreaterThanOrEqual

-- | Find the specific directory set node for the policy what we're working with.
registryNode :: forall era env err m. (MonadUtxoQuery m, C.IsBabbageBasedEra era, HasDirectoryEnv env, MonadReader env m, HasTransferLogicEnv env, MonadError err m, AsProgrammableTokensError err) => NodeMatch -> m (UTxODat era DirectorySetNode)
registryNode op' = do
  let op = operator op'
  directoryList <- registryNodes @era
  dir <- asks directoryEnv
  inta <- asks transferLogicEnv

  let mintingScript = programmableTokenMintingScript dir inta
      issuedPolicyId = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV3 mintingScript
      issuedSymbol = transPolicyId issuedPolicyId
  let udats =
        sortOn (Down . key . uDatum)
        $
          filter ((issuedSymbol `op`) . key . uDatum) directoryList
  case udats of
    [] -> throwing _DirectorySetNodeNotFound (show op')
    x : _ -> pure x

-- | CIP-143 outputs addressed to the given payment credential
userProgrammableOutputs :: forall era env m. (MonadReader env m, HasDirectoryEnv env, MonadUtxoQuery m, C.IsBabbageBasedEra era, MonadBlockchain era m) => (C.PaymentCredential, Maybe C.StakeCredential) -> m [UTxODat era ()]
userProgrammableOutputs (userPayCred, userStakeCred) = do
  nid <- queryNetworkId
  baseCred <- asks (C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 . dsProgrammableLogicBaseScript . directoryEnv)

  userPayCredAsStakeCred <- either (error . ("Could not unTrans credential: " <>) . show) pure $ unTransStakeCredential $ transCredential userPayCred
  let expectedAddress = C.makeShelleyAddressInEra C.shelleyBasedEra nid baseCred (C.StakeAddressByValue userPayCredAsStakeCred)
      isUserUtxo UTxODat{uOut=(C.TxOut addr _ _ _)} = addr == expectedAddress
        || maybe False (\stakeCred -> addr == C.makeShelleyAddressInEra C.shelleyBasedEra nid baseCred (C.StakeAddressByValue stakeCred)) userStakeCred

  filter isUserUtxo <$> programmableLogicOutputs

-- | Get the total of a user's programmable tokens
userTotalProgrammableValue :: forall era env m. (MonadReader env m, HasDirectoryEnv env, MonadUtxoQuery m, C.IsBabbageBasedEra era, MonadBlockchain era m) => (C.PaymentCredential, Maybe C.StakeCredential) -> m C.Value
userTotalProgrammableValue (userCred, userStakeCred) = do
  baseCred <- asks (C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 . dsProgrammableLogicBaseScript . directoryEnv)
  nid <- queryNetworkId
  userPayCredAsStakeCred <- either (error . ("Could not unTrans credential: " <>) . show) pure $ unTransStakeCredential $ transCredential userCred
  let programmableAddressByPayCred = C.makeShelleyAddressInEra C.shelleyBasedEra nid baseCred (C.StakeAddressByValue userPayCredAsStakeCred)
      isUserUtxo UTxODat{uOut=(C.TxOut addr _ _ _)} =
        addr == programmableAddressByPayCred
        || maybe False (\stakeCred -> addr == C.makeShelleyAddressInEra C.shelleyBasedEra nid baseCred (C.StakeAddressByValue stakeCred)) userStakeCred
  plOutputs <- filter isUserUtxo <$> programmableLogicOutputs @era @env

  directoryPolicyIds <- mapMaybe (either (const Nothing) Just . unTransPolicyId . key . uDatum) <$> registryNodes @era
  pure $ foldMap (filterValueByPolicyIds directoryPolicyIds . extractValue . uOut) plOutputs
  where
    filterValueByPolicyIds :: [C.PolicyId] -> C.Value -> C.Value
    filterValueByPolicyIds policyIds val =
      let isPolicy (C.AssetId pid _, _) = pid `elem` policyIds
          isPolicy _ = False
      in fromList $ filter isPolicy (toList val)


{-| Select enough programmable outputs to cover the desired amount
of the token. Returns the 'TxIn's and the leftover (change) quantity
-}
selectProgammableOutputsFor :: forall era env m.
  ( MonadReader env m
  , HasDirectoryEnv env
  , MonadUtxoQuery m
  , C.IsBabbageBasedEra era
  , MonadBlockchain era m
  , HasTransferLogicEnv env
  )
  => (C.PaymentCredential, Maybe C.StakeCredential)
  -> C.AssetName
  -> C.Quantity
  -> m ([C.TxIn], C.Quantity)
selectProgammableOutputsFor (owner, ownerStakeCred) assetname quantity = do
  userOutputs <- userProgrammableOutputs (owner, ownerStakeCred)
  policyId <- programmableTokenPolicyId
  -- Find sufficient inputs to cover the transfer
  let assetId = C.AssetId policyId assetname
  let userOutputsMap = fromList $ map (\UTxODat {uIn, uOut, uDatum} -> (uIn, (C.inAnyCardanoEra (C.cardanoEra @era) uOut, uDatum))) userOutputs
  (totalVal, txins) <- maybe (error "insufficient funds for transfer") pure $ selectMixedInputsCovering (UtxoSet userOutputsMap) [(assetId, quantity)]
  let quantityAvailable = C.selectAsset totalVal assetId
  pure (txins, quantityAvailable - quantity)

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
