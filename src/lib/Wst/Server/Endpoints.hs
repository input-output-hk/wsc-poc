
{- | This module contains the endpoints of the server.
-}
module Wst.Server.Endpoints (
  healthcheck,
  -- * Query endpoints
  queryGlobalParams,

  -- * Build tx endpoints
  issueProgrammableTokens
) where

import Cardano.Api qualified as C
import Control.Monad.Except (MonadError)
import Convex.Class (MonadUtxoQuery)
import Servant (Handler)
import Servant.API (NoContent (..))
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams)
import Wst.AppError (AppError)
import Wst.Offchain.Query (UTxODat)
import Wst.Offchain.Query qualified as Query
import Wst.Server.Types (IssueProgrammableTokenArgs, TextEnvelopeJSON)

healthcheck :: Handler NoContent
healthcheck = pure NoContent

queryGlobalParams :: forall era m. (MonadUtxoQuery m, C.IsBabbageBasedEra era, MonadError (AppError era) m) => m (UTxODat era ProgrammableLogicGlobalParams)
queryGlobalParams = Query.globalParamsNode

issueProgrammableTokens :: forall era m. IssueProgrammableTokenArgs -> m (TextEnvelopeJSON (C.Tx era))
issueProgrammableTokens = undefined
