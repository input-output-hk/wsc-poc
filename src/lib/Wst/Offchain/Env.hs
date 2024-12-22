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

  -- ** Using the operator environment
  selectOperatorOutput,
  balanceTxEnv,

  -- * Directory environment
  HasDirectoryEnv(..),
  DirectoryEnv(..),
  mkDirectoryEnv,
  programmableLogicStakeCredential,
  programmableLogicBaseCredential,
  directoryNodePolicyId,
  protocolParamsPolicyId,
  globalParams,


  -- * Transfer logic environment
  TransferLogicEnv(..),
  HasTransferLogicEnv(..),

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
  withOperator
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
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
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
                             directoryNodeSpendingScript,
                             freezeAndSezieTransferScript,
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
balanceTxEnv :: forall era env a m. (MonadBlockchain era m, MonadReader env m, HasOperatorEnv era env, MonadError (AppError era) m, C.IsBabbageBasedEra era) => BuildTxT era m a -> m (C.BalancedTxBody era, BalanceChanges)
balanceTxEnv btx = do
  OperatorEnv{bteOperatorUtxos, bteOperator} <- asks operatorEnv
  params <- queryProtocolParameters
  txBuilder <- BuildTx.execBuildTxT $ btx >> BuildTx.setMinAdaDepositAll params
  -- TODO: change returnOutputFor to consider the stake address reference
  -- (needs to be done in sc-tools)
  output <- returnOutputFor (C.PaymentCredentialByKey $ fst bteOperator)
  mapError BalancingError (CoinSelection.balanceTx mempty output (Utxos.fromApiUtxo bteOperatorUtxos) txBuilder CoinSelection.TrailingChange)

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
      dsProgrammableLogicGlobalScript = programmableLogicGlobalScript (directoryNodePolicyId result) -- Parameterized by the CS holding protocol params datum
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

{-| Scripts related to managing the specific transfer logic
-}

data TransferLogicEnv =
  TransferLogicEnv
    { tleBlacklistPolicy :: C.PolicyId
    , tleBlacklistMintingScript :: PlutusScript PlutusScriptV3
    , tleBlacklistSpendingScript :: PlutusScript PlutusScriptV3
    , tleMintingScript :: PlutusScript PlutusScriptV3
    , tleTransferScript :: PlutusScript PlutusScriptV3
    , tleIssuerScript :: PlutusScript PlutusScriptV3
    }

class HasTransferLogicEnv e where
  transferLogicEnv :: e -> TransferLogicEnv

instance HasTransferLogicEnv TransferLogicEnv where
  transferLogicEnv = id

mkTransferLogicEnv :: C.Hash C.PaymentKey -> TransferLogicEnv
mkTransferLogicEnv cred =
  let blacklistMinting = blacklistMintingScript cred
      blacklistPolicy = scriptPolicyIdV3 blacklistMinting
  in
  TransferLogicEnv
    { tleBlacklistPolicy = blacklistPolicy
    , tleBlacklistMintingScript = blacklistMinting
    , tleBlacklistSpendingScript = blacklistSpendingScript cred
    , tleMintingScript =  permissionedTransferScript cred
    , tleTransferScript = freezeAndSezieTransferScript blacklistPolicy
    , tleIssuerScript = permissionedTransferScript cred
    }

data RuntimeEnv
  = RuntimeEnv
      { envLogger     :: Logger
      , envBlockfrost :: Blockfrost.Project
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

data CombinedEnv operatorF directoryF runtimeF era =
  CombinedEnv
    { ceOperator  :: operatorF (OperatorEnv era)
    , ceDirectory :: directoryF DirectoryEnv
    , ceRuntime :: runtimeF RuntimeEnv
    }

makeLensesFor
  [("ceRuntime", "runtime")]
  ''CombinedEnv

{-| 'CombinedEnv' with no values
-}
empty :: forall era. CombinedEnv Proxy Proxy Proxy era
empty =
  CombinedEnv
    { ceOperator = Proxy
    , ceDirectory = Proxy
    , ceRuntime = Proxy
    }

instance HasOperatorEnv era (CombinedEnv Identity d r era) where
  operatorEnv = runIdentity . ceOperator

instance HasDirectoryEnv (CombinedEnv o Identity r era) where
  directoryEnv = runIdentity . ceDirectory

instance HasTransferLogicEnv (CombinedEnv Identity d r era) where
  transferLogicEnv = mkTransferLogicEnv . fst . bteOperator . operatorEnv

instance HasRuntimeEnv (CombinedEnv o d Identity era) where
  runtimeEnv = runIdentity . ceRuntime

_Identity :: L.Iso' (Identity a) a
_Identity = L.iso runIdentity Identity

instance HasLogger (CombinedEnv o d Identity era) where
  loggerL = runtime . _Identity . loggerL

{-| Add a 'DirectoryEnv' for the 'C.TxIn' in to the environment
-}
addDirectoryEnvFor :: C.TxIn -> CombinedEnv o d r era -> CombinedEnv o Identity r era
addDirectoryEnvFor txi = addDirectoryEnv (mkDirectoryEnv txi)

{-| Add a 'DirectoryEnv' for the 'C.TxIn' in to the environment
-}
addDirectoryEnv :: DirectoryEnv -> CombinedEnv o d r era -> CombinedEnv o Identity r era
addDirectoryEnv de env =
  env{ceDirectory = Identity de }

withDirectory :: MonadReader (CombinedEnv o d r era) m => DirectoryEnv -> ReaderT (CombinedEnv o Identity r era) m a -> m a
withDirectory dir action = do
  asks (addDirectoryEnv dir)
    >>= runReaderT action

withDirectoryFor :: MonadReader (CombinedEnv o d r era) m => C.TxIn -> ReaderT (CombinedEnv o Identity r era) m a -> m a
withDirectoryFor txi = withDirectory (mkDirectoryEnv txi)

{-| Add a 'DirectoryEnv' for the 'C.TxIn' in to the environment and run the
action with the modified environment
-}
withEnv :: forall era m a. ReaderT (CombinedEnv Proxy Proxy Proxy era) m a -> m a
withEnv = flip runReaderT empty

{-| Add a 'RuntimeEnv' to the environment
-}
addRuntimeEnv :: RuntimeEnv -> CombinedEnv o d r era -> CombinedEnv o d Identity era
addRuntimeEnv env e =
  e{ceRuntime = Identity env }

withRuntime :: MonadReader (CombinedEnv o d r era) m => RuntimeEnv -> ReaderT (CombinedEnv o d Identity era) m a -> m a
withRuntime runtime action =
  asks (addRuntimeEnv runtime)
    >>= runReaderT action

{-| Add an 'OperatorEnv' to the environment
-}
addOperatorEnv :: OperatorEnv era -> CombinedEnv o d r era2 -> CombinedEnv Identity d r era
addOperatorEnv op e =
  e{ceOperator = Identity op }

withOperator :: MonadReader (CombinedEnv o d r era1) m => OperatorEnv era -> ReaderT (CombinedEnv Identity d r era) m a -> m a
withOperator op action = asks (addOperatorEnv op) >>= runReaderT action
