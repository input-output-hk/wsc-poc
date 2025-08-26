{-# LANGUAGE FlexibleInstances #-}

module ProgrammableTokens.OffChain.Env
  ( module Operator,
    module Directory,
    module TransferLogic,

    -- * Combined Environment
    combinedEnv,
    directoryOperatorEnv,
    addTransferLogic,
    withTransferLogic,
    withEnv,
    runAs
  )
where

import Cardano.Api.Shelley qualified as C
import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Convex.Class (MonadUtxoQuery)
import Convex.Wallet.Operator qualified as Op
import Data.HSet.Type (HSet)
import Data.HSet.Type qualified as HSet
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
import TypeFun.Data.List qualified as HList


directoryOperatorEnv :: DirectoryEnv -> OperatorEnv era -> HSet [DirectoryEnv, OperatorEnv era]
directoryOperatorEnv ceDirectory ceOperator =
  HSet.HSCons ceDirectory (HSet.HSCons ceOperator HSet.HSNil)

combinedEnv :: DirectoryEnv -> OperatorEnv era -> TransferLogicEnv -> HSet [DirectoryEnv, OperatorEnv era, TransferLogicEnv]
combinedEnv ceDirectory ceOperator tl =
  HSet.HSCons ceDirectory (HSet.HSCons ceOperator (HSet.HSCons tl HSet.HSNil))

addTransferLogic :: (HList.NotElem TransferLogicEnv els) => TransferLogicEnv -> HSet els -> HSet (TransferLogicEnv ': els)
addTransferLogic = HSet.HSCons

-- | Add the 'TransferLogicEnv' to the combined environment
withTransferLogic :: (HList.NotElem TransferLogicEnv els, MonadReader (HSet els) m) => TransferLogicEnv -> ReaderT (HSet (TransferLogicEnv ': els)) m a -> m a
withTransferLogic op action = asks (addTransferLogic op) >>= runReaderT action

withEnv :: HSet els -> ReaderT (HSet els) m a -> m a
withEnv = flip runReaderT

{-| Load the operator UTxOs and run the action
-}
runAs ::
  forall k era m a.
  ( MonadUtxoQuery m
  , C.IsBabbageBasedEra era
  ) => Op.Operator k -> DirectoryEnv -> TransferLogicEnv -> ReaderT (HSet [DirectoryEnv, OperatorEnv era, TransferLogicEnv]) m a -> m a
runAs op dir transfer action = do
  env <- combinedEnv dir <$> loadConvexOperatorEnv op <*> pure transfer
  withEnv env action
