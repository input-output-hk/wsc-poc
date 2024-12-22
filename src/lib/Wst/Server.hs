{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

{-| servant server for stablecoin POC
-}
module Wst.Server(runServer) where

import Cardano.Api qualified as C
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, asks)
import Convex.Class (MonadBlockchain, MonadUtxoQuery)
import Data.Data (Proxy (..))
import Network.Wai.Handler.Warp qualified as Warp
import Servant (Server, ServerT)
import Servant.API (NoContent (..), (:<|>) (..))
import Servant.Server (hoistServer, serve)
import Wst.App (WstApp, runWstAppServant)
import Wst.AppError (AppError)
import Wst.Offchain.BuildTx.ProgrammableLogic (alwaysSucceedsArgs)
import Wst.Offchain.Endpoints.Deployment qualified as Endpoints
import Wst.Offchain.Env qualified as C
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query qualified as Query
import Wst.Server.Types (APIInEra, BuildTxAPI, IssueProgrammableTokenArgs (..),
                         QueryAPI, TextEnvelopeJSON (..))

runServer :: (Env.HasRuntimeEnv env, Env.HasDirectoryEnv env) => env -> IO ()
runServer env = do
  let app = serve (Proxy @APIInEra) (server env)
      port = 8081
  Warp.run port app

server :: forall env. (Env.HasRuntimeEnv env, C.HasDirectoryEnv env) => env -> Server APIInEra
server env = hoistServer (Proxy @APIInEra) (runWstAppServant env) $
  healthcheck
  :<|> queryApi @env @C.ConwayEra
  :<|> txApi @env

healthcheck :: Applicative m => m NoContent
healthcheck = pure NoContent

queryApi :: forall env era. C.IsBabbageBasedEra era => ServerT (QueryAPI era) (WstApp env era)
queryApi = Query.globalParamsNode

txApi :: forall env. (C.HasDirectoryEnv env) => ServerT (BuildTxAPI C.ConwayEra) (WstApp env C.ConwayEra)
txApi =
  issueProgrammableTokenEndpoint @C.ConwayEra @env

issueProgrammableTokenEndpoint :: forall era env m.
  ( MonadReader env m
  , Env.HasDirectoryEnv env
  , MonadBlockchain era m
  , MonadError (AppError era) m
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadUtxoQuery m
  )
  => IssueProgrammableTokenArgs -> m (TextEnvelopeJSON (C.Tx era))
issueProgrammableTokenEndpoint IssueProgrammableTokenArgs{itaAssetName, itaQuantity, itaOperatorAddress} = do
  operatorEnv <- Env.loadOperatorEnvFromAddress itaOperatorAddress
  dirEnv <- asks Env.directoryEnv

      -- FIXME: Replace alwaysSucceedsArgs with blacklist monetary policy as soon as it is finished
  let tokenArgs = alwaysSucceedsArgs
  Env.withEnv $ Env.withOperator operatorEnv $ Env.withDirectory dirEnv $ do
    TextEnvelopeJSON <$> Endpoints.issueProgrammableTokenTx tokenArgs itaAssetName itaQuantity
