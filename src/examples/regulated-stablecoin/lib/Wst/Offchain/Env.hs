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
  withOperator,
  blacklistNodePolicyId,
  withBlacklist,
  withBlacklistFor,
  addBlacklistEnv
) where

import Cardano.Api (PlutusScript, PlutusScriptV3)
import Cardano.Api qualified as C
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Data.HSet.Get (HGettable)
import Data.HSet.Get qualified as HSet
import Data.HSet.Type (HSet)
import Data.HSet.Type qualified as HSet
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
import ProgrammableTokens.OffChain.Scripts (alwaysSucceedsScript,
                                            scriptPolicyIdV3)
import SmartTokens.Core.Scripts (ScriptTarget)
import TypeFun.Data.List qualified as HList
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

instance (HGettable els BlacklistEnv) => HasBlacklistEnv (HSet els) where
  blacklistEnv = HSet.hget @_ @BlacklistEnv

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

{-| Add a 'DirectoryEnv' for the 'C.TxIn' in to the environment
-}
addDirectoryEnvFor :: (HList.NotElem DirectoryEnv els) => DirectoryScriptRoot -> HSet els -> HSet (DirectoryEnv ': els)
addDirectoryEnvFor = addDirectoryEnv . mkDirectoryEnv

{-| Add a 'DirectoryEnv' for the 'C.TxIn' in to the environment
-}
addDirectoryEnv :: (HList.NotElem DirectoryEnv els) => DirectoryEnv -> HSet els -> HSet (DirectoryEnv ': els)
addDirectoryEnv = HSet.HSCons

withDirectory :: (HList.NotElem DirectoryEnv els, MonadReader (HSet els) m) => DirectoryEnv -> ReaderT (HSet (DirectoryEnv ': els)) m a -> m a
withDirectory dir action = do
  asks (addDirectoryEnv dir)
    >>= runReaderT action

withDirectoryFor :: (HList.NotElem DirectoryEnv els, MonadReader (HSet els) m) => DirectoryScriptRoot -> ReaderT (HSet (DirectoryEnv ': els)) m a -> m a
withDirectoryFor = withDirectory . mkDirectoryEnv

{-| Add a 'TransferLogicEnv' for the 'C.Hash C.PaymentKey' corresponding to the
   admin hash
 -}
addTransferEnv :: (HList.NotElem TransferLogicEnv els) => TransferLogicEnv -> HSet els -> HSet (TransferLogicEnv ': els)
addTransferEnv = HSet.HSCons

withTransfer :: (HList.NotElem TransferLogicEnv els, MonadReader (HSet els) m) => TransferLogicEnv -> ReaderT (HSet (TransferLogicEnv ': els)) m a -> m a
withTransfer dir action = do
  asks (addTransferEnv dir)
    >>= runReaderT action

withTransferFor :: (HList.NotElem TransferLogicEnv els, MonadReader (HSet els) m) => BlacklistTransferLogicScriptRoot -> ReaderT (HSet (TransferLogicEnv ': els)) m a -> m a
withTransferFor = withTransfer . mkTransferLogicEnv

{-| Transfer logic scripts for the blacklist managed by the given 'C.PaymentKey' hash
-}
transferLogicForDirectory :: (HasDirectoryEnv env, MonadReader env m) => C.Hash C.PaymentKey -> m (TransferLogicEnv, BlacklistEnv)
transferLogicForDirectory pkh = do
  env <- ask
  let dirEnv = directoryEnv env
      sr     = BlacklistTransferLogicScriptRoot (srTarget $ dsScriptRoot dirEnv) dirEnv pkh
  pure (mkTransferLogicEnv sr, mkBlacklistEnv sr)

withTransferFromOperator :: forall era els m a. (HList.NotElem TransferLogicEnv (BlacklistEnv : els), HList.NotElem BlacklistEnv els, HGettable els (OperatorEnv era), HGettable els DirectoryEnv, MonadReader (HSet els) m) => ReaderT (HSet (TransferLogicEnv : BlacklistEnv : els)) m a -> m a
withTransferFromOperator action = do
  env <- ask
  let opPkh = fst . bteOperator . operatorEnv @era $ env
  (transferEnv,  ble) <- transferLogicForDirectory opPkh
  runReaderT action (addTransferEnv transferEnv $ addBlacklistEnv ble env)

addBlacklistEnv :: (HList.NotElem BlacklistEnv els) => BlacklistEnv -> HSet els -> HSet (BlacklistEnv ': els)
addBlacklistEnv = HSet.HSCons

withBlacklist :: (HList.NotElem BlacklistEnv els, MonadReader (HSet els) m) => BlacklistEnv -> ReaderT (HSet (BlacklistEnv ': els)) m a -> m a
withBlacklist env action =
  asks (addBlacklistEnv env)
    >>= runReaderT action

withBlacklistFor :: (HList.NotElem BlacklistEnv els, MonadReader (HSet els) m) => BlacklistTransferLogicScriptRoot -> ReaderT (HSet (BlacklistEnv ': els)) m a -> m a
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
withEnv :: forall m a. ReaderT (HSet '[]) m a -> m a
withEnv = flip runReaderT HSet.HSNil

{-| Add a 'RuntimeEnv' to the environment
-}
addRuntimeEnv :: (HList.NotElem RuntimeEnv els) => RuntimeEnv -> HSet els -> HSet (RuntimeEnv ': els)
addRuntimeEnv = HSet.HSCons

withRuntime :: (MonadReader (HSet els) m, HList.NotElem RuntimeEnv els) => RuntimeEnv -> ReaderT (HSet (RuntimeEnv ': els)) m a -> m a
withRuntime runtime_ action =
  asks (addRuntimeEnv runtime_)
    >>= runReaderT action

{-| Add an 'OperatorEnv' to the environment
-}
addOperatorEnv ::  (HList.NotElem (OperatorEnv era) els) => OperatorEnv era -> HSet els -> HSet (OperatorEnv era ': els)
addOperatorEnv = HSet.HSCons

withOperator :: (MonadReader (HSet els) m, HList.NotElem (OperatorEnv era) els) => OperatorEnv era -> ReaderT (HSet (OperatorEnv era ': els)) m a -> m a
withOperator op action = asks (addOperatorEnv op) >>= runReaderT action
