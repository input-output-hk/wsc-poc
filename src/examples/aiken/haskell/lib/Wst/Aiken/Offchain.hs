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
import Cardano.Api.Shelley qualified as C
import Control.Lens (makeClassyPrisms, review)
import Control.Monad.Except (MonadError (..))
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
import ProgrammableTokens.OffChain.Query qualified as Query
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

blueprintKeys :: Cip143Blueprint (Constant BlueprintKey)
blueprintKeys =
  Cip143Blueprint
    { cbSymbol = undefined
    , cbTransfer = Constant "transfer"
    , cbIssuance = Constant "issuance"
    , cbGlobalStateCS = Nothing
    }

data LookupScriptFailure =
  FailedToFindTransferScript BlueprintKey
  | FailedToFindIssuanceScript BlueprintKey
  deriving stock (Eq, Show)

makeClassyPrisms ''LookupScriptFailure

-- | Lookup the scripts that are referenced in the CIP 143 blueprint
lookupScripts :: Blueprint -> Cip143Blueprint (Constant BlueprintKey) -> Either LookupScriptFailure (Cip143Blueprint Identity)
lookupScripts Blueprint {validators} b@Cip143Blueprint {cbTransfer, cbIssuance} = do
  tr <- maybe (Left $ FailedToFindTransferScript (getConstant cbTransfer)) Right (Map.lookup (getConstant cbTransfer) validators)
  i <- maybe (Left $ FailedToFindIssuanceScript (getConstant cbIssuance)) Right (Map.lookup (getConstant cbIssuance) validators)
  pure $ b {cbIssuance = Identity i, cbTransfer = Identity tr}

-- | Lookup the scripts that are referenced in the CIP 143 blueprint
lookupScripts_ :: (MonadError err m, AsLookupScriptFailure err) => Blueprint -> Cip143Blueprint (Constant BlueprintKey) -> m (Cip143Blueprint Identity)
lookupScripts_ bp =
  either (throwError . review _LookupScriptFailure) pure
  .  lookupScripts bp

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
