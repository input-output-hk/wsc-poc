{-| Mock implementation of server API for testing / UI development
-}
module Wst.Test.MockServer(
  mockServer
) where

import Cardano.Api qualified as C
import Servant (Server)
import Servant.API (NoContent (..), (:<|>) (..))
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams)
import Wst.Offchain.Query (UTxODat (UTxODat))
import Wst.Server.Types (APIInEra, BuildTxAPI, QueryAPI)

mockServer :: Server APIInEra
mockServer =
  pure NoContent
  :<|> mockQueryApi
  :<|> mockTxApi

mockQueryApi :: Server (QueryAPI C.ConwayEra)
mockQueryApi =
  (pure globalParalsUtxo)
  :<|> undefined

mockTxApi :: Server (BuildTxAPI C.ConwayEra)
mockTxApi = undefined

genUtxo :: Gen a -> Gen (UTxODat era a)
genUtxo g =
  UTxODat
    <$> Gen.genTxIn
    <*> Gen.genTxOut
    <*> g

globalParalsUtxo :: UTxODat era ProgrammableLogicGlobalParams
globalParalsUtxo =
  UTxODat
    { uIn = undefined
    , uOut = undefined
    , uDatum = mockParams
    }

mockParams :: ProgrammableLogicGlobalParams
mockParams = undefined
