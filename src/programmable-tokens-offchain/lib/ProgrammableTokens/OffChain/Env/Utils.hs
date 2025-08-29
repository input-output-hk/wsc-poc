{-# LANGUAGE ConstraintKinds #-}
-- | Helper functions for working with HSet-based reader environments
module ProgrammableTokens.OffChain.Env.Utils(
  HSet,
  Elem,
  NotElem,
  HMonoModifiable,

  -- * HSet operations
  empty,
  singleton,
  addEnv,
  hget,

  -- * ReaderT
  ReaderT(..),
  runEnv,
  withEnv,
) where

import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Data.HSet.Get (HGettable, hget)
import Data.HSet.Modify (HMonoModifiable)
import Data.HSet.Type (HSet)
import Data.HSet.Type qualified as HSet
import TypeFun.Data.List (NotElem)

type Elem elm els = HGettable els elm

{-| Run 'ReaderT' with an empty environment
-}
runEnv :: forall m a. ReaderT (HSet '[]) m a -> m a
runEnv = flip runReaderT empty

-- | Empty environment
empty :: HSet '[]
empty = HSet.HSNil

-- | Environment with a single element
singleton :: elm -> HSet '[elm]
singleton elm = HSet.HSCons elm HSet.HSNil

-- | Add an element to the environment
addEnv :: (NotElem elm els) => elm -> HSet els -> HSet (elm ': els)
addEnv = HSet.HSCons

-- | Add an element to the environment and run a 'ReaderT' action with the extended environment
withEnv :: forall elm els m a. (NotElem elm els, MonadReader (HSet els) m) => elm -> ReaderT (HSet (elm ': els)) m a -> m a
withEnv elm action = do
  asks (HSet.HSCons elm) >>= runReaderT action
