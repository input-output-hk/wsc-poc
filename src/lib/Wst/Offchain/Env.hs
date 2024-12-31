{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

{-| Transaction building environment
-}
module Wst.Offchain.Env(
  -- * Operator environment
  HasOperatorEnv(..),
  OperatorEnv(..),
  loadOperatorEnv,
  loadOperatorEnvFromAddress,
  operatorPaymentCredential,

  -- ** Using the operator environment
  selectOperatorOutput,
  balanceTxEnv,
  balanceTxEnv_,

  -- * Directory environment
  HasDirectoryEnv(..),
  DirectoryEnv(..),
  mkDirectoryEnv,
  programmableLogicStakeCredential,
  programmableLogicBaseCredential,
  directoryNodePolicyId,
  protocolParamsPolicyId,
  globalParams,
  getGlobalParams,

  -- * Transfer logic environment
  TransferLogicEnv(..),
  HasTransferLogicEnv(..),
  mkTransferLogicEnv,
  addTransferEnv,
  withTransfer,
  withTransferFor,
  withTransferFromOperator,

  -- * Runtime data
  RuntimeEnv(..),
  HasRuntimeEnv(..),
  loadRuntimeEnv,

  -- * Combined environment
  CombinedEnv(..),
  empty,
  withEnv,
  addDirectoryEnvFor,
  addDirectoryEnv,
  withDirectory,
  withDirectoryFor,
  addRuntimeEnv,
  withRuntime,
  addOperatorEnv,
  withOperator,
  blacklistNodePolicyId
) where

import Blammo.Logging (Logger)
import Blammo.Logging.Logger (HasLogger (..), newLogger)
import Blammo.Logging.LogSettings.Env qualified as LogSettingsEnv
import Blockfrost.Auth (mkProject)
import Blockfrost.Client.Auth qualified as Blockfrost
import Cardano.Api (PlutusScript, PlutusScriptV3, UTxO)
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Lens (makeLensesFor)
import Control.Lens qualified as L
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Convex.BuildTx (BuildTxT)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain, MonadUtxoQuery (..),
                     queryProtocolParameters, utxosByPaymentCredential)
import Convex.CoinSelection qualified as CoinSelection
import Convex.PlutusLedger.V1 (transCredential, transPolicyId)
import Convex.Utils (mapError)
import Convex.Utxos (BalanceChanges)
import Convex.Utxos qualified as Utxos
import Convex.Wallet.Operator (returnOutputFor)
import Data.Functor.Identity (Identity (..))
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (..))
import System.Environment qualified
import Wst.AppError (AppError (..))
import Wst.Offchain.Scripts (blacklistMintingScript, blacklistSpendingScript,
                             directoryNodeMintingScript,
                             directoryNodeSpendingScript, freezeTransferScript,
                             permissionedTransferScript,
                             programmableLogicBaseScript,
                             programmableLogicGlobalScript,
                             protocolParamsMintingScript, scriptPolicyIdV3)

{-| Environments that have an 'OperatorEnv'
-}
class HasOperatorEnv era e | e -> era where
  operatorEnv :: e -> OperatorEnv era

instance HasOperatorEnv era (OperatorEnv era) where
  operatorEnv = id

{-| Information needed to build transactions
-}
data OperatorEnv era =
  OperatorEnv
    { bteOperator      :: (C.Hash C.PaymentKey, C.StakeAddressReference) -- ^ Payment and stake credential, used for generating return outputs
    , bteOperatorUtxos :: UTxO era -- ^ UTxOs owned by the operator, available for spending
    }

{-| Get the operator's payment credential from the 'env'
-}
operatorPaymentCredential :: (MonadReader env m, HasOperatorEnv era env) => m C.PaymentCredential
operatorPaymentCredential = asks (C.PaymentCredentialByKey . fst . bteOperator . operatorEnv)

{-| Populate the 'OperatorEnv' with UTxOs locked by the payment credential
-}
loadOperatorEnv :: (MonadUtxoQuery m, C.IsBabbageBasedEra era) => C.Hash C.PaymentKey -> C.StakeAddressReference -> m (OperatorEnv era)
loadOperatorEnv paymentCredential stakeCredential = do
  let bteOperator = (paymentCredential, stakeCredential)
  bteOperatorUtxos <- Utxos.toApiUtxo <$> utxosByPaymentCredential (C.PaymentCredentialByKey paymentCredential)
  pure OperatorEnv{bteOperator, bteOperatorUtxos}

loadOperatorEnvFromAddress :: (MonadUtxoQuery m, C.IsBabbageBasedEra era) => C.Address C.ShelleyAddr -> m (OperatorEnv era)
loadOperatorEnvFromAddress = \case
  (C.ShelleyAddress _ntw (C.fromShelleyPaymentCredential -> C.PaymentCredentialByKey pmt) stakeRef) ->
    loadOperatorEnv pmt (C.fromShelleyStakeReference stakeRef)
  _ -> error "Expected public key address" -- FIXME: proper error

{-| Select an output owned by the operator
-}
selectOperatorOutput :: (MonadReader env m, HasOperatorEnv era env, MonadError (AppError era) m) => m (C.TxIn, C.TxOut C.CtxUTxO era)
selectOperatorOutput = asks (listToMaybe . Map.toList . C.unUTxO . bteOperatorUtxos . operatorEnv) >>= \case
  Nothing -> throwError OperatorNoUTxOs
  Just k -> pure k

{-| Balance a transaction using the operator's funds and return output
-}
balanceTxEnv_ :: forall era env a m. (MonadBlockchain era m, MonadReader env m, HasOperatorEnv era env, MonadError (AppError era) m, C.IsBabbageBasedEra era) => BuildTxT era m a -> m (C.BalancedTxBody era, BalanceChanges)
balanceTxEnv_ btx = do
  OperatorEnv{bteOperatorUtxos, bteOperator} <- asks operatorEnv
  params <- queryProtocolParameters
  txBuilder <- BuildTx.execBuildTxT $ btx >> BuildTx.setMinAdaDepositAll params
  -- TODO: change returnOutputFor to consider the stake address reference
  -- (needs to be done in sc-tools)
  output <- returnOutputFor (C.PaymentCredentialByKey $ fst bteOperator)
  mapError BalancingError (CoinSelection.balanceTx mempty output (Utxos.fromApiUtxo bteOperatorUtxos) txBuilder CoinSelection.TrailingChange)

{-| Balance a transaction using the operator's funds and return output
-}
balanceTxEnv :: forall era env a m. (MonadBlockchain era m, MonadReader env m, HasOperatorEnv era env, MonadError (AppError era) m, C.IsBabbageBasedEra era) => BuildTxT era m a -> m ((C.BalancedTxBody era, BalanceChanges), a)
balanceTxEnv btx = do
  OperatorEnv{bteOperatorUtxos, bteOperator} <- asks operatorEnv
  params <- queryProtocolParameters
  (r, txBuilder) <- BuildTx.runBuildTxT $ btx <* BuildTx.setMinAdaDepositAll params
  -- TODO: change returnOutputFor to consider the stake address reference
  -- (needs to be done in sc-tools)
  output <- returnOutputFor (C.PaymentCredentialByKey $ fst bteOperator)
  (balBody, balChanges) <- mapError BalancingError (CoinSelection.balanceTx mempty output (Utxos.fromApiUtxo bteOperatorUtxos) txBuilder CoinSelection.TrailingChange)
  pure ((balBody, balChanges), r)


class HasDirectoryEnv e where
  directoryEnv :: e -> DirectoryEnv

instance HasDirectoryEnv DirectoryEnv where
  directoryEnv = id

{-| Scripts related to managing the token policy directory.
All of the scripts and their hashes are determined by the 'TxIn'.
-}
data DirectoryEnv =
  DirectoryEnv
    { dsTxIn :: C.TxIn -- ^ The 'txIn' that we spend when deploying the protocol params and directory set
    , dsDirectoryMintingScript         :: PlutusScript PlutusScriptV3
    , dsDirectorySpendingScript        :: PlutusScript PlutusScriptV3
    , dsProtocolParamsMintingScript    :: PlutusScript PlutusScriptV3
    , dsProgrammableLogicBaseScript    :: PlutusScript PlutusScriptV3
    , dsProgrammableLogicGlobalScript  :: PlutusScript PlutusScriptV3
    }

mkDirectoryEnv :: C.TxIn -> DirectoryEnv
mkDirectoryEnv dsTxIn =
  let dsDirectoryMintingScript        = directoryNodeMintingScript dsTxIn
      dsProtocolParamsMintingScript   = protocolParamsMintingScript dsTxIn
      dsDirectorySpendingScript       = directoryNodeSpendingScript (protocolParamsPolicyId result)
      dsProgrammableLogicBaseScript   = programmableLogicBaseScript (programmableLogicStakeCredential result) -- Parameterized by the stake cred of the global script
      dsProgrammableLogicGlobalScript = programmableLogicGlobalScript (protocolParamsPolicyId result) -- Parameterized by the CS holding protocol params datum
      result = DirectoryEnv
                { dsTxIn
                , dsDirectoryMintingScript
                , dsProtocolParamsMintingScript
                , dsProgrammableLogicBaseScript
                , dsProgrammableLogicGlobalScript
                , dsDirectorySpendingScript
                }
  in result

programmableLogicStakeCredential :: DirectoryEnv -> C.StakeCredential
programmableLogicStakeCredential =
  C.StakeCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 . dsProgrammableLogicGlobalScript

programmableLogicBaseCredential :: DirectoryEnv -> C.PaymentCredential
programmableLogicBaseCredential =
  C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 . dsProgrammableLogicBaseScript

directoryNodePolicyId :: DirectoryEnv -> C.PolicyId
directoryNodePolicyId = scriptPolicyIdV3 . dsDirectoryMintingScript

protocolParamsPolicyId :: DirectoryEnv -> C.PolicyId
protocolParamsPolicyId = scriptPolicyIdV3 . dsProtocolParamsMintingScript

globalParams :: DirectoryEnv -> ProgrammableLogicGlobalParams
globalParams scripts =
  ProgrammableLogicGlobalParams
    { directoryNodeCS = transPolicyId (directoryNodePolicyId scripts)
    , progLogicCred   = transCredential (programmableLogicBaseCredential scripts) -- its the script hash of the programmable base spending script
    }

getGlobalParams :: (MonadReader e m, HasDirectoryEnv e) => m ProgrammableLogicGlobalParams
getGlobalParams = asks (globalParams . directoryEnv)

{-| Scripts related to managing the specific transfer logic
-}

data TransferLogicEnv =
  TransferLogicEnv
    { tleBlacklistMintingScript :: PlutusScript PlutusScriptV3
    , tleBlacklistSpendingScript :: PlutusScript PlutusScriptV3
    , tleMintingScript :: PlutusScript PlutusScriptV3
    , tleTransferScript :: PlutusScript PlutusScriptV3
    , tleIssuerScript :: PlutusScript PlutusScriptV3
    }

class HasTransferLogicEnv e where
  transferLogicEnv :: e -> TransferLogicEnv

instance HasTransferLogicEnv TransferLogicEnv where
  transferLogicEnv = id

{-| The 'TransferLogicEnv' with scripts that allow the given payment credential
to manage the blacklist and issue / burn tokens
-}
mkTransferLogicEnv :: C.PaymentCredential -> C.Hash C.PaymentKey -> TransferLogicEnv
mkTransferLogicEnv progLogicBaseCred cred =
  let blacklistMinting = blacklistMintingScript cred
      blacklistPolicy = scriptPolicyIdV3 blacklistMinting
  in
  TransferLogicEnv
    { tleBlacklistMintingScript = blacklistMinting
    , tleBlacklistSpendingScript = blacklistSpendingScript cred
    , tleMintingScript =  permissionedTransferScript cred
    , tleTransferScript = freezeTransferScript progLogicBaseCred blacklistPolicy
    , tleIssuerScript = permissionedTransferScript cred
    }

blacklistNodePolicyId :: TransferLogicEnv -> C.PolicyId
blacklistNodePolicyId = scriptPolicyIdV3 . tleBlacklistMintingScript

data RuntimeEnv
  = RuntimeEnv
      { envLogger      :: Logger
      , envBlockfrost  :: Blockfrost.Project
      }

makeLensesFor
  [ ("envLogger", "logger")
  , ("envBlockfrostProject", "blockfrostProject")
  ]
  'RuntimeEnv

instance HasLogger RuntimeEnv where
  loggerL = logger

-- | Load the 'RuntimeEnv' from environment variables
loadRuntimeEnv :: IO RuntimeEnv
loadRuntimeEnv =
  RuntimeEnv
    <$> (LogSettingsEnv.parse >>= newLogger)
    <*> fmap (mkProject . Text.pack) (System.Environment.getEnv "WST_BLOCKFROST_TOKEN")

class HasRuntimeEnv e where
  runtimeEnv :: e -> RuntimeEnv

instance HasRuntimeEnv RuntimeEnv where
  runtimeEnv = id

data CombinedEnv operatorF directoryF transferF runtimeF era =
  CombinedEnv
    { ceOperator  :: operatorF (OperatorEnv era)
    , ceDirectory :: directoryF DirectoryEnv
    , ceTransfer  :: transferF TransferLogicEnv
    , ceRuntime   :: runtimeF RuntimeEnv
    }

makeLensesFor
  [("ceRuntime", "runtime")]
  ''CombinedEnv

{-| 'CombinedEnv' with no values
-}
empty :: forall era. CombinedEnv Proxy Proxy Proxy Proxy era
empty =
  CombinedEnv
    { ceOperator = Proxy
    , ceDirectory = Proxy
    , ceTransfer = Proxy
    , ceRuntime = Proxy
    }

instance HasOperatorEnv era (CombinedEnv Identity d t r era) where
  operatorEnv = runIdentity . ceOperator

instance HasDirectoryEnv (CombinedEnv o Identity t r era) where
  directoryEnv = runIdentity . ceDirectory

instance HasTransferLogicEnv (CombinedEnv o d Identity r era) where
  transferLogicEnv = runIdentity . ceTransfer

instance HasRuntimeEnv (CombinedEnv o d t Identity era) where
  runtimeEnv = runIdentity . ceRuntime

_Identity :: L.Iso' (Identity a) a
_Identity = L.iso runIdentity Identity

instance HasLogger (CombinedEnv o d t Identity era) where
  loggerL = runtime . _Identity . loggerL

{-| Add a 'DirectoryEnv' for the 'C.TxIn' in to the environment
-}
addDirectoryEnvFor :: C.TxIn -> CombinedEnv o d t r era -> CombinedEnv o Identity t r era
addDirectoryEnvFor txi = addDirectoryEnv (mkDirectoryEnv txi)

{-| Add a 'DirectoryEnv' for the 'C.TxIn' in to the environment
-}
addDirectoryEnv :: DirectoryEnv -> CombinedEnv o d t r era -> CombinedEnv o Identity t r era
addDirectoryEnv de env =
  env{ceDirectory = Identity de }

withDirectory :: MonadReader (CombinedEnv o d t r era) m => DirectoryEnv -> ReaderT (CombinedEnv o Identity t r era) m a -> m a
withDirectory dir action = do
  asks (addDirectoryEnv dir)
    >>= runReaderT action

withDirectoryFor :: MonadReader (CombinedEnv o d t r era) m => C.TxIn -> ReaderT (CombinedEnv o Identity t r era) m a -> m a
withDirectoryFor txi = withDirectory (mkDirectoryEnv txi)

{-| Add a 'TransferLogicEnv' for the 'C.Hash C.PaymentKey' corresponding to the
   admin hash
 -}
addTransferEnv :: TransferLogicEnv -> CombinedEnv o d t r era -> CombinedEnv o d Identity r era
addTransferEnv de env =
  env{ceTransfer = Identity de }

withTransfer :: MonadReader (CombinedEnv o Identity t r era) m => TransferLogicEnv -> ReaderT (CombinedEnv o Identity Identity r era) m a -> m a
withTransfer dir action = do
  asks (addTransferEnv dir)
    >>= runReaderT action

withTransferFor :: MonadReader (CombinedEnv o Identity t r era) m => C.PaymentCredential -> C.Hash C.PaymentKey -> ReaderT (CombinedEnv o Identity Identity r era) m a -> m a
withTransferFor plbBaseCred opPKH = withTransfer $ mkTransferLogicEnv plbBaseCred opPKH

withTransferFromOperator :: (MonadReader (CombinedEnv Identity Identity t r era) m) => ReaderT (CombinedEnv Identity Identity Identity r era) m a -> m a
withTransferFromOperator action = do
  env <- ask
  let opPkh = fst . bteOperator . operatorEnv $ env
      programmableBaseLogicCred = programmableLogicBaseCredential . directoryEnv $ env
  runReaderT action (addTransferEnv (mkTransferLogicEnv programmableBaseLogicCred opPkh) env)

{-| Add a 'DirectoryEnv' for the 'C.TxIn' in to the environment and run the
action with the modified environment
-}
withEnv :: forall era m a. ReaderT (CombinedEnv Proxy Proxy Proxy Proxy era) m a -> m a
withEnv = flip runReaderT empty

{-| Add a 'RuntimeEnv' to the environment
-}
addRuntimeEnv :: RuntimeEnv -> CombinedEnv o d t r era -> CombinedEnv o d t Identity era
addRuntimeEnv env e =
  e{ceRuntime = Identity env }

withRuntime :: MonadReader (CombinedEnv o d t r era) m => RuntimeEnv -> ReaderT (CombinedEnv o d t Identity era) m a -> m a
withRuntime runtime_ action =
  asks (addRuntimeEnv runtime_)
    >>= runReaderT action

{-| Add an 'OperatorEnv' to the environment
-}
addOperatorEnv :: OperatorEnv era -> CombinedEnv o d t r era2 -> CombinedEnv Identity d t r era
addOperatorEnv op e =
  e{ceOperator = Identity op }

withOperator :: MonadReader (CombinedEnv o d t r era1) m => OperatorEnv era -> ReaderT (CombinedEnv Identity d t r era) m a -> m a
withOperator op action = asks (addOperatorEnv op) >>= runReaderT action
