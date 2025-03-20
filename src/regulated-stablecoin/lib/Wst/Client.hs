{-# LANGUAGE TypeApplications #-}

{- | This module contains the client endpoints of the server.
-}
module Wst.Client (
  getHealthcheck,

  -- * Query routes
  getGlobalParams,

  -- * Build tx
  postIssueProgrammableTokenTx,
  postTransferProgrammableTokenTx,
  postAddToBlacklistTx,
  postRemoveFromBlacklistTx,
  postSeizeFundsTx
) where

import Cardano.Api qualified as C
import Data.Data (Proxy (..))
import Servant.API (NoContent, (:<|>) ((:<|>)))
import Servant.Client (ClientEnv, client, runClientM)
import Servant.Client.Core (ClientError)
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams)
import Wst.Offchain.Query (UTxODat)
import Wst.Server.Types (API, APIInEra, BlacklistNodeArgs,
                         IssueProgrammableTokenArgs (..), SeizeAssetsArgs,
                         TextEnvelopeJSON, TransferProgrammableTokenArgs (..))

getHealthcheck :: ClientEnv -> IO (Either ClientError NoContent)
getHealthcheck env = do
  let healthcheck :<|> _ = client (Proxy @APIInEra)
  runClientM healthcheck env

getGlobalParams :: forall era. C.IsShelleyBasedEra era => ClientEnv -> IO (Either ClientError (UTxODat era ProgrammableLogicGlobalParams))
getGlobalParams env = do
  let _ :<|> (globalParams :<|> _) :<|> _ = client (Proxy @(API era))
  runClientM globalParams env

postIssueProgrammableTokenTx :: forall era. C.IsShelleyBasedEra era => ClientEnv -> IssueProgrammableTokenArgs -> IO (Either ClientError (TextEnvelopeJSON (C.Tx era)))
postIssueProgrammableTokenTx env args = do
  let _ :<|> _ :<|> ((issueProgrammableTokenTx :<|> _) :<|> _) = client (Proxy @(API era))
  runClientM (issueProgrammableTokenTx args) env

postTransferProgrammableTokenTx :: forall era. C.IsShelleyBasedEra era => ClientEnv -> TransferProgrammableTokenArgs -> IO (Either ClientError (TextEnvelopeJSON (C.Tx era)))
postTransferProgrammableTokenTx env args = do
  let _ :<|> _ :<|> ((_ :<|> transferProgrammableTokenTx :<|> _) :<|> _) = client (Proxy @(API era))
  runClientM (transferProgrammableTokenTx args) env

postAddToBlacklistTx :: forall era. C.IsShelleyBasedEra era => ClientEnv -> BlacklistNodeArgs -> IO (Either ClientError (TextEnvelopeJSON (C.Tx era)))
postAddToBlacklistTx env args = do
  let _ :<|> _ :<|> ((_ :<|> _ :<|> addToBlacklistTx :<|> _) :<|> _) = client (Proxy @(API era))
  runClientM (addToBlacklistTx args) env

postRemoveFromBlacklistTx :: forall era. C.IsShelleyBasedEra era => ClientEnv -> BlacklistNodeArgs -> IO (Either ClientError (TextEnvelopeJSON (C.Tx era)))
postRemoveFromBlacklistTx env args = do
  let _ :<|> _ :<|> ((_ :<|> _ :<|> _ :<|>  removeFromBlacklistTx :<|> _) :<|> _) = client (Proxy @(API era))
  runClientM (removeFromBlacklistTx args) env

postSeizeFundsTx :: forall era. C.IsShelleyBasedEra era => ClientEnv -> SeizeAssetsArgs -> IO (Either ClientError (TextEnvelopeJSON (C.Tx era)))
postSeizeFundsTx env args = do
  let _ :<|> _ :<|> ((_ :<|> _ :<|> _ :<|> _ :<|> seizeFunds) :<|> _) = client (Proxy @(API era))
  runClientM (seizeFunds args) env
