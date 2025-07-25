{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Off-chain code for the aiken example
module Wst.Aiken.Offchain
  ( registerTx,
    Cip143Blueprint (..),
    lookupScripts,
  )
where

import Cardano.Api (ScriptInAnyLang)
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain, MonadUtxoQuery)
import Convex.CoinSelection (AsBalancingError, AsCoinSelectionError)
import Convex.CoinSelection qualified
import Convex.PlutusLedger.V1 (transScriptHash)
import Data.Functor.Constant (Constant (..))
import Data.Functor.Identity (Identity (..))
import Data.Map qualified as Map
import PlutusLedgerApi.V3 (CurrencySymbol)
import ProgrammableTokens.OffChain.Env.Operator qualified as Env
import ProgrammableTokens.OffChain.Error (AsProgrammableTokensError)
import Wst.Aiken.Blueprint (Blueprint (..), BlueprintKey)
import Wst.Offchain.BuildTx.DirectorySet (InsertNodeArgs (..))
import Wst.Offchain.BuildTx.DirectorySet qualified as Directory
import Wst.Offchain.Env (HasDirectoryEnv)
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query qualified as Query

data Cip143Blueprint v
  = Cip143Blueprint
  { cbSymbol :: CurrencySymbol,
    cbTransfer :: v ScriptInAnyLang,
    cbIssuance :: v ScriptInAnyLang,
    cbGlobalStateCS :: Maybe CurrencySymbol
  }

-- | Lookup the scripts that are referenced in the CIP 143 blueprint
lookupScripts :: Blueprint -> Cip143Blueprint (Constant BlueprintKey) -> Either String (Cip143Blueprint Identity)
lookupScripts Blueprint {validators} b@Cip143Blueprint {cbTransfer, cbIssuance} = do
  tr <- maybe (Left $ "Failed to find key " <> show (getConstant cbTransfer)) Right (Map.lookup (getConstant cbTransfer) validators)
  i <- maybe (Left $ "Failed to find key " <> show (getConstant cbIssuance)) Right (Map.lookup (getConstant cbIssuance) validators)
  pure $ b {cbIssuance = Identity i, cbTransfer = Identity tr}

scriptHash :: ScriptInAnyLang -> C.ScriptHash
scriptHash (C.ScriptInAnyLang _ s) = C.hashScript s

mkArgs :: Cip143Blueprint Identity -> InsertNodeArgs
mkArgs Cip143Blueprint {cbSymbol, cbTransfer, cbIssuance, cbGlobalStateCS} =
  let mintingLogicHash = scriptHash $ runIdentity cbIssuance
   in InsertNodeArgs
        { inaNewKey = cbSymbol,
          inaHashedParam = transScriptHash mintingLogicHash,
          inaTransferLogic = C.StakeCredentialByScript $ scriptHash $ runIdentity cbTransfer,
          inaIssuerLogic = C.StakeCredentialByScript $ scriptHash $ runIdentity cbIssuance,
          inaGlobalStateCS = cbGlobalStateCS
        }

-- | Create a transaction (fully balanced, not signed) that registers the policies
registerTx :: forall era env err m. (C.IsBabbageBasedEra era, MonadReader env m, HasDirectoryEnv env, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBlockchain era m, MonadError err m, MonadUtxoQuery m, Env.HasTransferLogicEnv env, AsBalancingError err era, AsCoinSelectionError err, Env.HasOperatorEnv era env, AsProgrammableTokensError err) => Cip143Blueprint Identity -> m (C.Tx era)
registerTx blueprint = do
  let args = mkArgs blueprint
  paramsNode <- Query.globalParamsNode @era
  cborHex <- Query.issuanceCborHexUTxO @era
  udat <- Query.registryNode @era

  (tx, _) <- Env.balanceTxEnv_ (BuildTx.runBuildTxT @era (Directory.insertDirectoryNode @era paramsNode cborHex udat args))
  pure (Convex.CoinSelection.signBalancedTxBody [] tx)

-- other endpoints
-- mint
-- burn
-- transfer
-- force-transfer
