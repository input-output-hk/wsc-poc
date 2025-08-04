{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}

module ProgrammableTokens.OffChain.Env
  ( module Operator,
    module Directory,
    module TransferLogic,

    -- * Combined Environment
    CombinedEnv (..),
    combinedEnv,
    directoryOperatorEnv,
    addTransferLogic,
    withTransferLogic,
    withEnv
  )
where

import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (..))
import ProgrammableTokens.OffChain.Env.Directory as Directory (DirectoryEnv (..),
                                                               DirectoryScriptRoot (..),
                                                               HasDirectoryEnv (..),
                                                               balanceDeployTxEnv_,
                                                               directoryNodePolicyId,
                                                               getGlobalParams,
                                                               globalParams,
                                                               issuanceCborHexPolicyId,
                                                               mkDirectoryEnv,
                                                               programmableLogicBaseCredential,
                                                               programmableLogicStakeCredential,
                                                               programmableTokenReceivingAddress,
                                                               protocolParamsPolicyId)
import ProgrammableTokens.OffChain.Env.Operator as Operator
import ProgrammableTokens.OffChain.Env.TransferLogic as TransferLogic

data CombinedEnv tr era
  = CombinedEnv
  { ceDirectory :: DirectoryEnv,
    ceOperator :: OperatorEnv era,
    ceTransfer :: tr TransferLogicEnv
  }

instance HasDirectoryEnv (CombinedEnv tr era) where
  directoryEnv = ceDirectory

instance HasOperatorEnv era (CombinedEnv tr era) where
  operatorEnv = ceOperator

instance HasTransferLogicEnv (CombinedEnv Identity era) where
  transferLogicEnv = runIdentity . ceTransfer

directoryOperatorEnv :: DirectoryEnv -> OperatorEnv era -> CombinedEnv Proxy era
directoryOperatorEnv ceDirectory ceOperator =
  CombinedEnv
    { ceDirectory,
      ceOperator,
      ceTransfer = Proxy
    }

combinedEnv :: DirectoryEnv -> OperatorEnv era -> TransferLogicEnv -> CombinedEnv Identity era
combinedEnv ceDirectory ceOperator tl =
  CombinedEnv
    { ceDirectory,
      ceOperator,
      ceTransfer = Identity tl
    }

addTransferLogic :: TransferLogicEnv -> CombinedEnv a era -> CombinedEnv Identity era
addTransferLogic tl env =
  env {ceTransfer = Identity tl }

-- | Add the 'TransferLogicEnv' to the combined environment
withTransferLogic :: MonadReader (CombinedEnv b era) m => TransferLogicEnv -> ReaderT (CombinedEnv Identity era) m a -> m a
withTransferLogic op action = asks (addTransferLogic op) >>= runReaderT action

withEnv :: CombinedEnv b era -> ReaderT (CombinedEnv b era) m a -> m a
withEnv = flip runReaderT
