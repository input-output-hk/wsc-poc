{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Lightweight SQLite-backed store for mapping freeze/seize policy identifiers
     to their corresponding issuer addresses. This allows off-chain endpoints to
     recover the issuer needed to drive future transactions even after process
     restarts.
-}
module Wst.Server.PolicyIssuerStore (
  PolicyIssuerStore (..),
  HasPolicyIssuerStore (..),
  addPolicyIssuerStore,
  PolicyIssuer,
  openPolicyIssuerStore,
  closePolicyIssuerStore,
  withPolicyIssuerStore,
  insertPolicyIssuer,
  getPolicyIssuer,
  listPolicyIssuers,
) where

import Cardano.Api (AsType (..))
import Cardano.Api qualified as C
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromRow (FromRow (..), field)
import ProgrammableTokens.OffChain.Env.Utils qualified as EnvUtils

-- | Simple wrapper so we can manage the connection lifecycle explicitly.
newtype PolicyIssuerStore = PolicyIssuerStore
  { pisConnection :: SQL.Connection
  }

type PolicyIssuer = (C.PolicyId, C.Address C.ShelleyAddr)

openPolicyIssuerStore :: FilePath -> IO PolicyIssuerStore
openPolicyIssuerStore path = do
  conn <- SQL.open path
  initialiseSchema conn
  pure $ PolicyIssuerStore conn

closePolicyIssuerStore :: PolicyIssuerStore -> IO ()
closePolicyIssuerStore = SQL.close . pisConnection

withPolicyIssuerStore :: FilePath -> (PolicyIssuerStore -> IO a) -> IO a
withPolicyIssuerStore path =
  bracket (openPolicyIssuerStore path) closePolicyIssuerStore

insertPolicyIssuer :: PolicyIssuerStore -> C.PolicyId -> C.Address C.ShelleyAddr -> IO ()
insertPolicyIssuer PolicyIssuerStore{pisConnection} policyId issuer =
  SQL.execute
    pisConnection
    "INSERT INTO policy_issuers (policy_id, issuer_address) VALUES (?, ?) \
    \ON CONFLICT(policy_id) DO UPDATE SET issuer_address = excluded.issuer_address"
    (policyIdToBytes policyId, addressToText issuer)

getPolicyIssuer :: PolicyIssuerStore -> C.PolicyId -> IO (Maybe (C.Address C.ShelleyAddr))
getPolicyIssuer PolicyIssuerStore{pisConnection} policyId = do
  rows <-
    SQL.query
      pisConnection
      "SELECT issuer_address FROM policy_issuers WHERE policy_id = ? LIMIT 1"
      (SQL.Only (policyIdToBytes policyId))
  pure $ listToMaybe rows >>= textToAddress . SQL.fromOnly

listPolicyIssuers :: PolicyIssuerStore -> IO [PolicyIssuer]
listPolicyIssuers PolicyIssuerStore{pisConnection} = do
  rows <- SQL.query_ pisConnection "SELECT policy_id, issuer_address FROM policy_issuers"
  pure $ mapMaybe decode rows
  where
    decode :: PolicyIssuerRow -> Maybe PolicyIssuer
    decode PolicyIssuerRow{pirPolicyId, pirIssuerAddress} = do
      pid <- policyIdFromBytes pirPolicyId
      addr <- textToAddress pirIssuerAddress
      pure (pid, addr)

-- Internal -------------------------------------------------------------------

data PolicyIssuerRow = PolicyIssuerRow
  { pirPolicyId :: ByteString
  , pirIssuerAddress :: Text
  }

instance FromRow PolicyIssuerRow where
  fromRow = PolicyIssuerRow <$> field <*> field

initialiseSchema :: SQL.Connection -> IO ()
initialiseSchema conn = do
  SQL.execute_
    conn
    "CREATE TABLE IF NOT EXISTS policy_issuers ( \
    \  policy_id      BLOB PRIMARY KEY, \
    \  issuer_address TEXT NOT NULL, \
    \  inserted_at    TEXT NOT NULL DEFAULT (datetime('now')) \
    \)"
  SQL.execute_
    conn
    "CREATE INDEX IF NOT EXISTS idx_policy_issuers_issuer \
    \ON policy_issuers (issuer_address)"

policyIdToBytes :: C.PolicyId -> ByteString
policyIdToBytes = C.serialiseToRawBytes

policyIdFromBytes :: ByteString -> Maybe C.PolicyId
policyIdFromBytes bs =
  case C.deserialiseFromRawBytes AsPolicyId bs of
    Left _ -> Nothing
    Right pid -> Just pid

addressToText :: C.Address C.ShelleyAddr -> Text
addressToText = C.serialiseAddress

textToAddress :: Text -> Maybe (C.Address C.ShelleyAddr)
textToAddress = C.deserialiseAddress (AsAddress AsShelleyAddr)

class HasPolicyIssuerStore env where
  policyIssuerStore :: env -> PolicyIssuerStore

instance HasPolicyIssuerStore PolicyIssuerStore where
  policyIssuerStore = id

instance (EnvUtils.Elem PolicyIssuerStore els) => HasPolicyIssuerStore (EnvUtils.HSet els) where
  policyIssuerStore = EnvUtils.hget @_ @PolicyIssuerStore

addPolicyIssuerStore :: (EnvUtils.NotElem PolicyIssuerStore els) => PolicyIssuerStore -> EnvUtils.HSet els -> EnvUtils.HSet (PolicyIssuerStore ': els)
addPolicyIssuerStore = EnvUtils.addEnv
