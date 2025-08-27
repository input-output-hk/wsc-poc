{-# LANGUAGE NamedFieldPuns #-}
{-| Application monad used by CLI and server
-}
module Wst.App (
  WstApp(..),
  runWstApp,
  runWstAppServant
) where

import Blammo.Logging.Simple (MonadLogger, MonadLoggerIO, WithLogger (..))
import Cardano.Api qualified as C
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Convex.Blockfrost (BlockfrostT (..), evalBlockfrostT)
import Convex.Class (MonadBlockchain, MonadUtxoQuery)
import Data.String (IsString (..))
import ProgrammableTokens.OffChain.Env.Runtime (RuntimeEnv (..))
import ProgrammableTokens.OffChain.Env.Runtime qualified as Env
import Servant.Server (Handler (..))
import Servant.Server qualified as S
import Wst.AppError (AppError (..))

newtype WstApp env era a = WstApp { unWstApp :: ReaderT env (ExceptT (AppError era) (BlockfrostT IO)) a }
  deriving newtype (Monad, Applicative, Functor, MonadIO, MonadReader env, MonadError (AppError era), MonadUtxoQuery, MonadBlockchain C.ConwayEra)
  deriving
    (MonadLogger, MonadLoggerIO)
    via (WithLogger env (ExceptT (AppError era) (BlockfrostT IO)))

runWstApp :: forall env era a. (Env.HasRuntimeEnv env) => env -> WstApp env era a -> IO (Either (AppError era) a)
runWstApp env WstApp{unWstApp} = do
  let RuntimeEnv{ceBlockfrost} = Env.runtimeEnv env
  evalBlockfrostT ceBlockfrost (runExceptT (runReaderT unWstApp env)) >>= \case
    Left e -> pure (Left $ BlockfrostErr e)
    Right a -> pure  a

{-| Interpret the 'WstApp' in a servant handler
-}
runWstAppServant :: forall env era a. (C.IsAlonzoBasedEra era, Env.HasRuntimeEnv env) => env -> WstApp env era a -> Handler a
runWstAppServant env action = liftIO (runWstApp env action) >>= \case
  Left err -> do
    let err_ = S.err500 { S.errBody = fromString (show err) }
    throwError err_
  Right a -> pure a
