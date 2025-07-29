{-# LANGUAGE FlexibleInstances #-}
module ProgrammableTokens.OffChain.Env(
  module Operator,
  module Directory,
  module TransferLogic,

  -- * Combined Environment
  CombinedEnv(..)
) where

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

data CombinedEnv era =
  CombinedEnv
    { ceDirectory :: DirectoryEnv
    , ceOperator  :: OperatorEnv era
    , ceTransfer  :: TransferLogicEnv
    }

instance HasDirectoryEnv (CombinedEnv era) where
  directoryEnv = ceDirectory

instance HasOperatorEnv era (CombinedEnv era) where
  operatorEnv = ceOperator

instance HasTransferLogicEnv (CombinedEnv era) where
  transferLogicEnv = ceTransfer
