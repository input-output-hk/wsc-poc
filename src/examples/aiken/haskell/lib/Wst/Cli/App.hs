{-# LANGUAGE NamedFieldPuns #-}
module Wst.Cli.App(
  WstApp(..),
  runWstApp
) where
import Blammo.Logging.Simple (MonadLogger, MonadLoggerIO, WithLogger (..))
import Cardano.Api qualified as C
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Convex.Blockfrost (BlockfrostT (..), evalBlockfrostT)
import Convex.Class (MonadBlockchain, MonadUtxoQuery)
import Wst.Aiken.Error (AikenError (..))
import Wst.Cli.Env (RuntimeEnv (..))
import Wst.Cli.Env qualified as Env

newtype WstApp env era a = WstApp { unWstApp :: ReaderT env (ExceptT (AikenError era) (BlockfrostT IO)) a }
  deriving newtype (Monad, Applicative, Functor, MonadIO, MonadReader env, MonadError (AikenError era), MonadUtxoQuery, MonadBlockchain C.ConwayEra)
  deriving
    (MonadLogger, MonadLoggerIO)
    via (WithLogger env (ExceptT (AikenError era) (BlockfrostT IO)))

runWstApp :: forall env era a. (Env.HasRuntimeEnv env) => env -> WstApp env era a -> IO (Either (AikenError era) a)
runWstApp env WstApp{unWstApp} = do
  let RuntimeEnv{ceBlockfrost} = Env.runtimeEnv env
  evalBlockfrostT ceBlockfrost (runExceptT (runReaderT unWstApp env)) >>= \case
    Left e -> pure (Left $ ABlockfrostError e)
    Right a -> pure  a
