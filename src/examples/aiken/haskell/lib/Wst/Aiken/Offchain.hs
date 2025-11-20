{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Off-chain code for the aiken example
module Wst.Aiken.Offchain
  ( Cip143Blueprint (..),
    blueprintKeys,
    lookupScripts,
    lookupScripts_,
    extractV3Scripts_,
    transferLogic,
    -- * Error types
    AsLookupScriptFailure(..),
    LookupScriptFailure(..),
  )
where

import Cardano.Api qualified as C
import Convex.Aiken.Offchain (Cip143Blueprint (..), blueprintKeys,
                              extractV3Scripts_, lookupScripts, lookupScripts_)
import ProgrammableTokens.OffChain.Env.TransferLogic (TransferLogicEnv (..))
import Wst.Aiken.Error (AsLookupScriptFailure (..), LookupScriptFailure (..))

transferLogic :: Cip143Blueprint (C.PlutusScript C.PlutusScriptV3) -> TransferLogicEnv
transferLogic Cip143Blueprint{cbTransfer, cbIssuance, cbGlobalStateCS} =
  TransferLogicEnv
    { tleMintingScript   = cbIssuance
    , tleTransferScript  = cbTransfer
    , tleIssuerScript    = cbTransfer
    , tleGlobalParamsNft = cbGlobalStateCS
    }


-- other endpoints
-- mint
-- burn
-- transfer
-- force-transfer
