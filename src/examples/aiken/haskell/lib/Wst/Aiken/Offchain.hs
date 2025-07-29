{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Off-chain code for the aiken example
module Wst.Aiken.Offchain
  ( registerTx,
    Cip143Blueprint (..),
    blueprintKeys,
    lookupScripts,
    lookupScripts_,
    -- * Error types
    AsLookupScriptFailure(..),
    LookupScriptFailure(..)
  )
where

import Cardano.Api (ScriptInAnyLang)
import Cardano.Api qualified as C
import Control.Lens (makeClassyPrisms, review)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain, MonadUtxoQuery)
import Convex.CoinSelection (AsBalancingError, AsCoinSelectionError)
import Convex.CoinSelection qualified
import Data.Map qualified as Map
import PlutusLedgerApi.V3 (CurrencySymbol)
import ProgrammableTokens.OffChain.BuildTx.Directory qualified as BuildTx
import ProgrammableTokens.OffChain.Env (CombinedEnv (CombinedEnv),
                                        HasDirectoryEnv (..))
import ProgrammableTokens.OffChain.Env.Operator (HasOperatorEnv (..))
import ProgrammableTokens.OffChain.Env.Operator qualified as Env
import ProgrammableTokens.OffChain.Env.TransferLogic (TransferLogicEnv (..))
import ProgrammableTokens.OffChain.Error (AsProgrammableTokensError)
import ProgrammableTokens.OffChain.Query qualified as Query
import Wst.Aiken.Blueprint (Blueprint (..), BlueprintKey)

data Cip143Blueprint v
  = Cip143Blueprint
  { cbTransfer :: v,
    cbIssuance :: v,
    cbGlobalStateCS :: Maybe CurrencySymbol
  }

blueprintKeys :: Cip143Blueprint BlueprintKey
blueprintKeys =
  Cip143Blueprint
    { cbTransfer = "transfer"
    , cbIssuance = "issuance"
    , cbGlobalStateCS = Nothing
    }

data LookupScriptFailure =
  FailedToFindTransferScript BlueprintKey
  | FailedToFindIssuanceScript BlueprintKey
  deriving stock (Eq, Show)

makeClassyPrisms ''LookupScriptFailure

-- | Lookup the scripts that are referenced in the CIP 143 blueprint
lookupScripts :: Blueprint -> Cip143Blueprint BlueprintKey -> Either LookupScriptFailure (Cip143Blueprint ScriptInAnyLang)
lookupScripts Blueprint {validators} b@Cip143Blueprint {cbTransfer, cbIssuance} = do
  tr <- maybe (Left $ FailedToFindTransferScript cbTransfer) Right (Map.lookup cbTransfer validators)
  i <- maybe (Left $ FailedToFindIssuanceScript cbIssuance) Right (Map.lookup cbIssuance validators)
  pure $ b {cbIssuance = i, cbTransfer = tr}

-- | Lookup the scripts that are referenced in the CIP 143 blueprint
lookupScripts_ :: (MonadError err m, AsLookupScriptFailure err) => Blueprint -> Cip143Blueprint BlueprintKey -> m (Cip143Blueprint ScriptInAnyLang)
lookupScripts_ bp =
  either (throwError . review _LookupScriptFailure) pure
  .  lookupScripts bp

scriptHash :: ScriptInAnyLang -> C.ScriptHash
scriptHash (C.ScriptInAnyLang _ s) = C.hashScript s

transferLogic :: Cip143Blueprint (C.PlutusScript C.PlutusScriptV3) -> TransferLogicEnv
transferLogic Cip143Blueprint{cbTransfer, cbIssuance, cbGlobalStateCS} =
  TransferLogicEnv
    { tleMintingScript   = cbIssuance
    , tleTransferScript  = cbTransfer
    , tleIssuerScript    = cbTransfer
    , tleGlobalParamsNft = cbGlobalStateCS
    }

-- | Create a transaction (fully balanced, not signed) that registers the policies
registerTx :: forall era env err m.
  ( C.IsBabbageBasedEra era
  , MonadReader env m
  , HasDirectoryEnv env
  , HasOperatorEnv era env
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadBlockchain era m
  , MonadError err m
  , MonadUtxoQuery m
  , AsBalancingError err era
  , AsCoinSelectionError err
  , AsProgrammableTokensError err
  )
  => Cip143Blueprint (C.PlutusScript C.PlutusScriptV3)
  -> m (C.Tx era)
registerTx blueprint = do
  let logic = transferLogic blueprint
  env <- CombinedEnv <$> asks directoryEnv <*> asks operatorEnv <*> pure logic
  flip runReaderT env $ do
    paramsNode <- Query.globalParamsNode @era
    cborHex <- Query.issuanceCborHexUTxO @era
    udat <- Query.registryNodeForInsertion @era

    (tx, _) <- Env.balanceTxEnv_ (BuildTx.runBuildTxT @era (BuildTx.insertDirectoryNode @era paramsNode cborHex udat))
    pure (Convex.CoinSelection.signBalancedTxBody [] tx)

-- other endpoints
-- mint
-- burn
-- transfer
-- force-transfer
