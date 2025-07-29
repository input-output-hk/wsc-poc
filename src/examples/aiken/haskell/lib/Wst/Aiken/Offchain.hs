{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Off-chain code for the aiken example
module Wst.Aiken.Offchain
  ( registerBlueprintTx,
    Cip143Blueprint (..),
    blueprintKeys,
    lookupScripts,
    lookupScripts_,
    extractV3Scripts_,
    -- * Error types
    AsLookupScriptFailure(..),
    LookupScriptFailure(..),
  )
where

import Cardano.Api (ScriptInAnyLang)
import Cardano.Api qualified as C
import Control.Lens (review)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Convex.Class (MonadBlockchain, MonadUtxoQuery)
import Convex.CoinSelection (AsBalancingError, AsCoinSelectionError)
import Data.Map qualified as Map
import PlutusLedgerApi.V3 (CurrencySymbol)
import ProgrammableTokens.OffChain.Endpoints qualified as Endpoints
import ProgrammableTokens.OffChain.Env (CombinedEnv (CombinedEnv),
                                        HasDirectoryEnv (..))
import ProgrammableTokens.OffChain.Env.Operator (HasOperatorEnv (..))
import ProgrammableTokens.OffChain.Env.TransferLogic (TransferLogicEnv (..))
import ProgrammableTokens.OffChain.Error (AsProgrammableTokensError)
import Wst.Aiken.Blueprint (Blueprint (..))
import Wst.Aiken.Blueprint qualified as Blueprint
import Wst.Aiken.BlueprintKey (BlueprintKey)
import Wst.Aiken.Error (AsBlueprintError (..), AsLookupScriptFailure (..),
                        LookupScriptFailure (..))

data Cip143Blueprint v
  = Cip143Blueprint
  { cbTransfer :: v,
    cbIssuance :: v,
    cbGlobalStateCS :: Maybe CurrencySymbol
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

blueprintKeys :: Cip143Blueprint BlueprintKey
blueprintKeys =
  Cip143Blueprint
    { cbTransfer = "transfer"
    , cbIssuance = "issuance"
    , cbGlobalStateCS = Nothing
    }

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

getPlutus :: C.Script C.PlutusScriptV3 -> C.PlutusScript C.PlutusScriptV3
getPlutus = \case
  C.PlutusScript C.PlutusScriptV3 script -> script

extractV3Scripts_ :: (MonadError err m, AsBlueprintError err) => Cip143Blueprint ScriptInAnyLang -> m (Cip143Blueprint (C.PlutusScript C.PlutusScriptV3))
extractV3Scripts_ = traverse (fmap getPlutus . Blueprint.getPlutusV3)

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

-- | Create a transaction (fully balanced, not signed) that registers the policies from the blueprint
registerBlueprintTx :: forall era env err m.
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
registerBlueprintTx blueprint = do
  let logic = transferLogic blueprint
  CombinedEnv <$> asks directoryEnv <*> asks operatorEnv <*> pure logic
    >>= runReaderT Endpoints.registerCip143PolicyTx


-- other endpoints
-- mint
-- burn
-- transfer
-- force-transfer
