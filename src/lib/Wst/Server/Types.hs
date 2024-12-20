{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}


{- | This module contains the relevant types for the server.
 -}
module Wst.Server.Types (
  API,
) where

import Servant.API (Capture, Description, Get, JSON, NoContent, Post, ReqBody,
                    type (:>), (:<|>) (..))

type API =
  "healthcheck" :> Description "Is the server alive?" :> Get '[JSON] NoContent
  :<|> "init-merkle-tree" :> Description "Initialize a new Merkle tree." :> ReqBody '[JSON] String :> Post '[JSON] String
  -- creates empty directory
  -- initialize the programmable token params, dir node minting policy
  -- init head of linked list

  :<|> "update-merkle-tree" :> Description "Update the Merkle tree." :> ReqBody '[JSON] String :> Post '[JSON] String -- This might need to be broken down further
  -- dir 1
  -- the programmable script to execute
  -- add program (registers staking scripts as well)
  -- modify program
  -- remove program (maybe not needed)

  -- dir 2 (specific to the program)
  -- add blacklist
  -- remove blacklist

  -- should be user-transfer (invoking spending program)
  :<|> "transfer-to-user" :> Description "Transfer tokens to a user." :> ReqBody '[JSON] String :> Post '[JSON] String

  -- should be issuer-transfe (invoking issuer program)
  :<|> "transfer-to-issuer" :> Description "Transfer tokens to an issuer." :> ReqBody '[JSON] String :> Post '[JSON] String
  :<|> "address" :> Description "Query the balance of an address." :> Capture "address" String :> Get '[JSON] String
  :<|> "all-sanctioned-addresses" :> Description "Query all sanctioned addresses." :> Get '[JSON] String

