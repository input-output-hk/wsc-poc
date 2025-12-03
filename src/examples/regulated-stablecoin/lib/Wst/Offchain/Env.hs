{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

  -- * Combined environment
  withEnv,
  addDirectoryEnvFor,
  addDirectoryEnv,
  withDirectory,
  withDirectoryFor,
  addRuntimeEnv,
  withRuntime,
  addOperatorEnv,
  replaceOperatorEnv,
  withOperator,
  blacklistNodePolicyId,
  withBlacklist,
  withBlacklistFor,
  addBlacklistEnv
) where

import Cardano.Api (PlutusScript, PlutusScriptV3)
import Cardano.Api qualified as C
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
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
import ProgrammableTokens.OffChain.Env.Runtime (RuntimeEnv)
import ProgrammableTokens.OffChain.Env.Utils qualified as Utils
import ProgrammableTokens.OffChain.Scripts (alwaysSucceedsScript,
                                            scriptPolicyIdV3)
import SmartTokens.Core.Scripts (ScriptTarget)
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

instance (Utils.Elem BlacklistEnv els) => HasBlacklistEnv (Utils.HSet els) where
  blacklistEnv = Utils.hget @_ @BlacklistEnv

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
    , tlrIssuerStakeCredential :: Maybe C.StakeCredential
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

{-| Add a 'DirectoryEnv' for the 'C.TxIn' in to the environment
-}
addDirectoryEnvFor :: (Utils.NotElem DirectoryEnv els) => DirectoryScriptRoot -> Utils.HSet els -> Utils.HSet (DirectoryEnv ': els)
addDirectoryEnvFor = addDirectoryEnv . mkDirectoryEnv

{-| Add a 'DirectoryEnv' for the 'C.TxIn' in to the environment
-}
addDirectoryEnv :: (Utils.NotElem DirectoryEnv els) => DirectoryEnv -> Utils.HSet els -> Utils.HSet (DirectoryEnv ': els)
addDirectoryEnv = Utils.addEnv

withDirectory :: (Utils.NotElem DirectoryEnv els, MonadReader (Utils.HSet els) m) => DirectoryEnv -> ReaderT (Utils.HSet (DirectoryEnv ': els)) m a -> m a
withDirectory dir action = do
  asks (addDirectoryEnv dir)
    >>= runReaderT action

withDirectoryFor :: (Utils.NotElem DirectoryEnv els, MonadReader (Utils.HSet els) m) => DirectoryScriptRoot -> ReaderT (Utils.HSet (DirectoryEnv ': els)) m a -> m a
withDirectoryFor = withDirectory . mkDirectoryEnv

{-| Add a 'TransferLogicEnv' for the 'C.Hash C.PaymentKey' corresponding to the
   admin hash
 -}
addTransferEnv :: (Utils.NotElem TransferLogicEnv els) => TransferLogicEnv -> Utils.HSet els -> Utils.HSet (TransferLogicEnv ': els)
addTransferEnv = Utils.addEnv

withTransfer :: (Utils.NotElem TransferLogicEnv els, MonadReader (Utils.HSet els) m) => TransferLogicEnv -> ReaderT (Utils.HSet (TransferLogicEnv ': els)) m a -> m a
withTransfer dir action = do
  asks (addTransferEnv dir)
    >>= runReaderT action

withTransferFor :: (Utils.NotElem TransferLogicEnv els, MonadReader (Utils.HSet els) m) => BlacklistTransferLogicScriptRoot -> ReaderT (Utils.HSet (TransferLogicEnv ': els)) m a -> m a
withTransferFor = withTransfer . mkTransferLogicEnv

{-| Transfer logic scripts for the blacklist managed by the given 'C.PaymentKey' hash
-}
transferLogicForDirectory :: (HasDirectoryEnv env, MonadReader env m) => C.Hash C.PaymentKey -> Maybe C.StakeCredential -> m (TransferLogicEnv, BlacklistEnv)
transferLogicForDirectory pkh stakeCred = do
  env <- ask
  let dirEnv = directoryEnv env
      sr     = BlacklistTransferLogicScriptRoot (srTarget $ dsScriptRoot dirEnv) dirEnv pkh stakeCred
  pure (mkTransferLogicEnv sr, mkBlacklistEnv sr)

withTransferFromOperator :: forall era els m a. (Utils.NotElem TransferLogicEnv (BlacklistEnv : els), Utils.NotElem BlacklistEnv els, Utils.Elem (OperatorEnv era) els, Utils.Elem DirectoryEnv els, MonadReader (Utils.HSet els) m) => ReaderT (Utils.HSet (TransferLogicEnv : BlacklistEnv : els)) m a -> m a
withTransferFromOperator action = do
  env <- ask
  let opPkh = fst . bteOperator . operatorEnv @era $ env
      stakeCred = toStakeCredential $ snd . bteOperator . operatorEnv @era $ env
  (transferEnv,  ble) <- transferLogicForDirectory opPkh stakeCred
  runReaderT action (addTransferEnv transferEnv $ addBlacklistEnv ble env)
  where
    toStakeCredential :: C.StakeAddressReference -> Maybe C.StakeCredential
    toStakeCredential = \case
      C.StakeAddressByValue stakeCred -> Just stakeCred
      _ -> Nothing


addBlacklistEnv :: (Utils.NotElem BlacklistEnv els) => BlacklistEnv -> Utils.HSet els -> Utils.HSet (BlacklistEnv ': els)
addBlacklistEnv = Utils.addEnv

withBlacklist :: (Utils.NotElem BlacklistEnv els, MonadReader (Utils.HSet els) m) => BlacklistEnv -> ReaderT (Utils.HSet (BlacklistEnv ': els)) m a -> m a
withBlacklist env action =
  asks (addBlacklistEnv env)
    >>= runReaderT action

withBlacklistFor :: (Utils.NotElem BlacklistEnv els, MonadReader (Utils.HSet els) m) => BlacklistTransferLogicScriptRoot -> ReaderT (Utils.HSet (BlacklistEnv ': els)) m a -> m a
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
withEnv :: forall m a. ReaderT (Utils.HSet '[]) m a -> m a
withEnv = flip runReaderT Utils.empty

{-| Add a 'RuntimeEnv' to the environment
-}
addRuntimeEnv :: (Utils.NotElem RuntimeEnv els) => RuntimeEnv -> Utils.HSet els -> Utils.HSet (RuntimeEnv ': els)
addRuntimeEnv = Utils.addEnv

withRuntime :: (MonadReader (Utils.HSet els) m, Utils.NotElem RuntimeEnv els) => RuntimeEnv -> ReaderT (Utils.HSet (RuntimeEnv ': els)) m a -> m a
withRuntime runtime_ action =
  asks (addRuntimeEnv runtime_)
    >>= runReaderT action

{-| Add an 'OperatorEnv' to the environment
-}
addOperatorEnv ::  (Utils.NotElem (OperatorEnv era) els) => OperatorEnv era -> Utils.HSet els -> Utils.HSet (OperatorEnv era ': els)
addOperatorEnv = Utils.addEnv

{-| Replace an existing 'OperatorEnv' inside the environment
-}
replaceOperatorEnv :: (Utils.HMonoModifiable els (OperatorEnv era)) => OperatorEnv era -> Utils.HSet els -> Utils.HSet els
replaceOperatorEnv opEnv = Utils.modifyEnv (const opEnv)

withOperator :: (MonadReader (Utils.HSet els) m, Utils.NotElem (OperatorEnv era) els) => OperatorEnv era -> ReaderT (Utils.HSet (OperatorEnv era ': els)) m a -> m a
withOperator op action = asks (addOperatorEnv op) >>= runReaderT action
