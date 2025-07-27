{-# LANGUAGE NamedFieldPuns #-}
module ProgrammableTokens.OffChain.Query(
  -- * CIP-143 registry queries
  registryNodes,
  registryNode,

  userProgrammableOutputs,
  issuanceCborHexUTxO,
  globalParamsNode,
  programmableLogicOutputs

) where

import Cardano.Api qualified as C
import Control.Lens qualified as L
import Control.Monad.Error.Lens (throwing_)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, asks)
import Convex.CardanoApi.Lenses qualified as L
import Convex.Class (MonadBlockchain (..), MonadUtxoQuery,
                     utxosByPaymentCredential)
import Convex.PlutusLedger.V1 (transCredential, transPolicyId,
                               unTransStakeCredential)
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import GHC.Exts (IsList (..))
import ProgrammableTokens.OffChain.Env.Directory (DirectoryEnv (..),
                                                  HasDirectoryEnv (..),
                                                  directoryNodePolicyId,
                                                  issuanceCborHexPolicyId,
                                                  protocolParamsPolicyId)
import ProgrammableTokens.OffChain.Env.TransferLogic (HasTransferLogicEnv (..),
                                                      programmableTokenMintingScript)
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

-- | CIP-143 outputs addressed to the given payment credential
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
