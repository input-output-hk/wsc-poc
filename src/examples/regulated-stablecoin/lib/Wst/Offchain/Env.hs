{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

{-| Transaction building environment
-}
module Wst.Offchain.Env(

  -- ** Using the operator environment
  balanceDeployTxEnv_,

  -- * On-chain scripts

  -- ** Directory environment
  DirectoryScriptRoot(..),
  mkDirectoryEnv,
  HasDirectoryEnv(..),
  DirectoryEnv(..),
  programmableLogicStakeCredential,
  programmableLogicBaseCredential,
  directoryNodePolicyId,
  protocolParamsPolicyId,
  issuanceCborHexPolicyId,
  globalParams,
  getGlobalParams,

  -- ** Blacklist environment
  BlacklistEnv(..),
  HasBlacklistEnv(..),
  alwaysSucceedsBlacklistEnv,
  mkBlacklistEnv,

  -- ** Transfer logic environment
  BlacklistTransferLogicScriptRoot(..),
  mkTransferLogicEnv,
  TransferLogicEnv(..),
  transferLogicForDirectory,
  alwaysSucceedsTransferLogic,
  HasTransferLogicEnv(..),
  addTransferEnv,
  withTransfer,
  withTransferFor,
  withTransferFromOperator,

  -- ** Minting tokens
  programmableTokenMintingScript,
  programmableTokenAssetId,
  programmableTokenReceivingAddress,

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
  blacklistNodePolicyId,
  withBlacklist,
  withBlacklistFor,
  addBlacklistEnv
) where

import Blammo.Logging (Logger)
import Blammo.Logging.Logger (HasLogger (..), newLogger)
import Blammo.Logging.LogSettings.Env qualified as LogSettingsEnv
import Blockfrost.Auth (mkProject)
import Blockfrost.Client.Auth qualified as Blockfrost
import Cardano.Api (PlutusScript, PlutusScriptV3)
import Cardano.Api qualified as C
import Control.Lens (makeLensesFor)
import Control.Lens qualified as L
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import ProgrammableTokens.OffChain.Env (DirectoryEnv (..),
                                        DirectoryScriptRoot (..),
                                        HasDirectoryEnv (..),
                                        HasOperatorEnv (..),
                                        HasTransferLogicEnv (..),
                                        OperatorEnv (..), TransferLogicEnv (..),
                                        alwaysSucceedsTransferLogic,
                                        balanceDeployTxEnv_,
                                        directoryNodePolicyId, getGlobalParams,
                                        globalParams, issuanceCborHexPolicyId,
                                        mkDirectoryEnv,
                                        programmableLogicBaseCredential,
                                        programmableLogicStakeCredential,
                                        programmableTokenMintingScript,
                                        programmableTokenReceivingAddress,
                                        protocolParamsPolicyId)
import ProgrammableTokens.OffChain.Scripts (alwaysSucceedsScript,
                                            scriptPolicyIdV3)
import SmartTokens.Core.Scripts (ScriptTarget)
import System.Environment qualified
import Wst.Offchain.Scripts (blacklistMintingScript, blacklistSpendingScript,
                             freezeTransferScript, permissionedMintingScript,
                             permissionedSpendingScript)

data BlacklistEnv =
  BlacklistEnv
    { bleMintingScript  :: PlutusScript PlutusScriptV3
    , bleSpendingScript :: PlutusScript PlutusScriptV3
    }

alwaysSucceedsBlacklistEnv :: ScriptTarget -> BlacklistEnv
alwaysSucceedsBlacklistEnv target =
  BlacklistEnv
    { bleMintingScript = alwaysSucceedsScript target
    , bleSpendingScript = alwaysSucceedsScript target
    }

class HasBlacklistEnv e where
  blacklistEnv :: e -> BlacklistEnv

instance HasBlacklistEnv BlacklistEnv where
  blacklistEnv = id

mkBlacklistEnv :: BlacklistTransferLogicScriptRoot -> BlacklistEnv
mkBlacklistEnv BlacklistTransferLogicScriptRoot{tlrTarget, tlrIssuer} =
  let blacklistMinting = blacklistMintingScript tlrTarget tlrIssuer
  in BlacklistEnv
      { bleMintingScript = blacklistMinting
      , bleSpendingScript = blacklistSpendingScript tlrTarget tlrIssuer
      }

{-| Data that completely determines the on-chain scripts of the blacklist
transfer logic, and their hashes. Any information that results in different
script hashes should go in here. We should be able to write a function
'BlacklistTransferLogicScriptRoot -> script' for all of the blacklist transfer
logic scripts.
-}
data BlacklistTransferLogicScriptRoot =
  BlacklistTransferLogicScriptRoot
    { tlrTarget :: ScriptTarget
    , tlrDirEnv :: DirectoryEnv
    , tlrIssuer :: C.Hash C.PaymentKey
    }

mkTransferLogicEnv :: BlacklistTransferLogicScriptRoot -> TransferLogicEnv
mkTransferLogicEnv BlacklistTransferLogicScriptRoot{tlrTarget, tlrDirEnv, tlrIssuer} =
  let blacklistMinting = blacklistMintingScript tlrTarget tlrIssuer
      blacklistPolicy = scriptPolicyIdV3 blacklistMinting
      progLogicBaseCred = programmableLogicBaseCredential tlrDirEnv
  in
  TransferLogicEnv
    { tleMintingScript =  permissionedMintingScript tlrTarget tlrIssuer
    , tleTransferScript = freezeTransferScript tlrTarget progLogicBaseCred blacklistPolicy
    , tleIssuerScript = permissionedSpendingScript tlrTarget tlrIssuer
    , tleGlobalParamsNft = Nothing
    }

blacklistNodePolicyId :: BlacklistEnv -> C.PolicyId
blacklistNodePolicyId = scriptPolicyIdV3 . bleMintingScript

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

data CombinedEnv operatorF directoryF transferF runtimeF blacklistF era =
  CombinedEnv
    { ceOperator  :: operatorF (OperatorEnv era)
    , ceDirectory :: directoryF DirectoryEnv
    , ceTransfer  :: transferF TransferLogicEnv
    , ceRuntime   :: runtimeF RuntimeEnv
    , ceBlacklist :: blacklistF BlacklistEnv
    }

makeLensesFor
  [("ceRuntime", "runtime")]
  ''CombinedEnv

{-| 'CombinedEnv' with no values
-}
empty :: forall era. CombinedEnv Proxy Proxy Proxy Proxy Proxy era
empty =
  CombinedEnv
    { ceOperator = Proxy
    , ceDirectory = Proxy
    , ceTransfer = Proxy
    , ceRuntime = Proxy
    , ceBlacklist = Proxy
    }

instance HasOperatorEnv era (CombinedEnv Identity d t r b era) where
  operatorEnv = runIdentity . ceOperator

instance HasDirectoryEnv (CombinedEnv o Identity t r b era) where
  directoryEnv = runIdentity . ceDirectory

instance HasTransferLogicEnv (CombinedEnv o d Identity r b era) where
  transferLogicEnv = runIdentity . ceTransfer

instance HasRuntimeEnv (CombinedEnv o d t Identity b era) where
  runtimeEnv = runIdentity . ceRuntime

instance HasBlacklistEnv (CombinedEnv o d t r Identity era) where
  blacklistEnv = runIdentity . ceBlacklist

_Identity :: L.Iso' (Identity a) a
_Identity = L.iso runIdentity Identity

instance HasLogger (CombinedEnv o d t Identity b era) where
  loggerL = runtime . _Identity . loggerL

{-| Add a 'DirectoryEnv' for the 'C.TxIn' in to the environment
-}
addDirectoryEnvFor :: DirectoryScriptRoot -> CombinedEnv o d t r b era -> CombinedEnv o Identity t r b era
addDirectoryEnvFor = addDirectoryEnv . mkDirectoryEnv

{-| Add a 'DirectoryEnv' for the 'C.TxIn' in to the environment
-}
addDirectoryEnv :: DirectoryEnv -> CombinedEnv o d t r b era -> CombinedEnv o Identity t r b era
addDirectoryEnv de env =
  env{ceDirectory = Identity de }

withDirectory :: MonadReader (CombinedEnv o d t r b era) m => DirectoryEnv -> ReaderT (CombinedEnv o Identity t r b era) m a -> m a
withDirectory dir action = do
  asks (addDirectoryEnv dir)
    >>= runReaderT action

withDirectoryFor :: MonadReader (CombinedEnv o d t r b era) m => DirectoryScriptRoot -> ReaderT (CombinedEnv o Identity t r b era) m a -> m a
withDirectoryFor = withDirectory . mkDirectoryEnv

{-| Add a 'TransferLogicEnv' for the 'C.Hash C.PaymentKey' corresponding to the
   admin hash
 -}
addTransferEnv :: TransferLogicEnv -> CombinedEnv o d t r b era -> CombinedEnv o d Identity r b era
addTransferEnv de env =
  env{ceTransfer = Identity de }

withTransfer :: MonadReader (CombinedEnv o d t r b era) m => TransferLogicEnv -> ReaderT (CombinedEnv o d Identity r b era) m a -> m a
withTransfer dir action = do
  asks (addTransferEnv dir)
    >>= runReaderT action

withTransferFor :: MonadReader (CombinedEnv o Identity t r b era) m => BlacklistTransferLogicScriptRoot -> ReaderT (CombinedEnv o Identity Identity r b era) m a -> m a
withTransferFor = withTransfer . mkTransferLogicEnv

{-| Transfer logic scripts for the blacklist managed by the given 'C.PaymentKey' hash
-}
transferLogicForDirectory :: (HasDirectoryEnv env, MonadReader env m) => C.Hash C.PaymentKey -> m (TransferLogicEnv, BlacklistEnv)
transferLogicForDirectory pkh = do
  env <- ask
  let dirEnv = directoryEnv env
      sr     = BlacklistTransferLogicScriptRoot (srTarget $ dsScriptRoot dirEnv) dirEnv pkh
  pure (mkTransferLogicEnv sr, mkBlacklistEnv sr)

withTransferFromOperator :: (MonadReader (CombinedEnv Identity Identity t r b era) m) => ReaderT (CombinedEnv Identity Identity Identity r Identity era) m a -> m a
withTransferFromOperator action = do
  env <- ask
  let opPkh = fst . bteOperator . operatorEnv $ env
  (transferEnv,  ble) <- transferLogicForDirectory opPkh
  runReaderT action (addTransferEnv transferEnv $ addBlacklistEnv ble env)

addBlacklistEnv :: BlacklistEnv -> CombinedEnv o d t r b era -> CombinedEnv o d t r Identity era
addBlacklistEnv be env =
  env{ceBlacklist = Identity be}

withBlacklist :: MonadReader (CombinedEnv o d t r b era) m => BlacklistEnv -> ReaderT (CombinedEnv o d t r Identity era) m a -> m a
withBlacklist env action =
  asks (addBlacklistEnv env)
    >>= runReaderT action

withBlacklistFor :: MonadReader (CombinedEnv o d t r b era) m => BlacklistTransferLogicScriptRoot -> ReaderT (CombinedEnv o d t r Identity era) m a -> m a
withBlacklistFor = withBlacklist . mkBlacklistEnv

{-| 'C.AssetId' of the programmable tokens
-}
programmableTokenAssetId :: DirectoryEnv -> TransferLogicEnv -> C.AssetName -> C.AssetId
programmableTokenAssetId dirEnv inta =
  C.AssetId
    (C.scriptPolicyId $ C.PlutusScript C.plutusScriptVersion $ programmableTokenMintingScript dirEnv inta)

{-| Add a 'DirectoryEnv' for the 'C.TxIn' in to the environment and run the
action with the modified environment
-}
withEnv :: forall era m a. ReaderT (CombinedEnv Proxy Proxy Proxy Proxy Proxy era) m a -> m a
withEnv = flip runReaderT empty

{-| Add a 'RuntimeEnv' to the environment
-}
addRuntimeEnv :: RuntimeEnv -> CombinedEnv o d t r b era -> CombinedEnv o d t Identity b era
addRuntimeEnv env e =
  e{ceRuntime = Identity env }

withRuntime :: MonadReader (CombinedEnv o d t r b era) m => RuntimeEnv -> ReaderT (CombinedEnv o d t Identity b era) m a -> m a
withRuntime runtime_ action =
  asks (addRuntimeEnv runtime_)
    >>= runReaderT action

{-| Add an 'OperatorEnv' to the environment
-}
addOperatorEnv :: OperatorEnv era -> CombinedEnv o d t r b era2 -> CombinedEnv Identity d t r b era
addOperatorEnv op e =
  e{ceOperator = Identity op }

withOperator :: MonadReader (CombinedEnv o d t r b era1) m => OperatorEnv era -> ReaderT (CombinedEnv Identity d t r b era) m a -> m a
withOperator op action = asks (addOperatorEnv op) >>= runReaderT action
