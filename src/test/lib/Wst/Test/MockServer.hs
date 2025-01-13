{-| Mock implementation of server API for testing / UI development
-}
module Wst.Test.MockServer(
  mockServer,
  runMockServer
) where

import Cardano.Api qualified as C
import Control.Monad.IO.Class (MonadIO (..))
import Data.Proxy (Proxy (..))
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors
import Servant (Server, throwError)
import Servant.API (NoContent (..), (:<|>) (..))
import Servant.Server (serve)
import Servant.Server qualified as Server
import Test.Gen.Cardano.Api.Typed qualified as Gen
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Gen qualified as Gen
import Test.QuickCheck.Hedgehog (hedgehog)
import Wst.Server.Types (APIInEra, BuildTxAPI, QueryAPI, TextEnvelopeJSON (..))
import Wst.Test.Gen qualified as Gen

mockServer :: Server APIInEra
mockServer =
  pure NoContent
  :<|> mockQueryApi
  :<|> mockTxApi

mockQueryApi :: Server (QueryAPI C.ConwayEra)
mockQueryApi =
  liftIO (QC.generate $ Gen.genUTxODat Gen.genGlobalParams)
  :<|> (\_ -> liftIO $ QC.generate $ Gen.listOf (hedgehog $ Gen.genVerificationKeyHash (C.proxyToAsType Proxy)))
  :<|> (\_ -> liftIO $ fmap (C.fromLedgerValue C.ShelleyBasedEraConway) $ QC.generate $ hedgehog $ Gen.genValue C.MaryEraOnwardsConway Gen.genAssetId Gen.genPositiveQuantity)
  :<|> liftIO (fmap (C.fromLedgerValue C.ShelleyBasedEraConway) $ QC.generate $ hedgehog $ Gen.genValue C.MaryEraOnwardsConway Gen.genAssetId Gen.genPositiveQuantity)
  :<|> (\_ -> liftIO $ QC.generate Gen.genAddress)

genTx :: MonadIO m => m (TextEnvelopeJSON (C.Tx C.ConwayEra))
genTx = liftIO $ fmap TextEnvelopeJSON $ QC.generate $ hedgehog $ Gen.genTx C.shelleyBasedEra

mockTxApi :: Server (BuildTxAPI C.ConwayEra)
mockTxApi =
  (const genTx
  :<|> const genTx
  :<|> const genTx
  :<|> const genTx)
  :<|> const (throwError Server.err501)
  :<|> const (throwError Server.err501)

-- | Start the mock server
runMockServer :: IO ()
runMockServer = do
  let app = simpleCors $ serve (Proxy @APIInEra) mockServer
      port = 8080
  putStrLn $ "Starting mock server on port " <> show port
  Warp.run port app
