{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Off-chain code for the aiken example
module Wst.Aiken.Offchain
  ( register,
    Cip143Blueprint (..),
    lookupScripts,
  )
where

import Cardano.Api (ScriptInAnyLang)
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.Error.Lens (throwing_)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain, MonadUtxoQuery)
import Convex.PlutusLedger.V1 (transPolicyId, transScriptHash)
import Data.Functor.Constant (Constant (..))
import Data.Functor.Identity (Identity (..))
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Ord (Down (..))
import PlutusLedgerApi.V3 (CurrencySymbol)
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (key))
import Wst.Aiken.Blueprint (Blueprint (..), BlueprintKey)
import Wst.AppError (AsProgrammableTokensError (_DirectorySetNodeNotFound))
import Wst.Offchain.BuildTx.DirectorySet (InsertNodeArgs (..))
import Wst.Offchain.BuildTx.DirectorySet qualified as Directory
import Wst.Offchain.Env (HasDirectoryEnv)
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query (UTxODat (..))
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

-- | Find the directory set node for the policy what we're working with.
findDirectorySetNode :: forall era env err m. (MonadUtxoQuery m, C.IsBabbageBasedEra era, HasDirectoryEnv env, MonadReader env m, Env.HasTransferLogicEnv env, MonadError err m, AsProgrammableTokensError err) => m (UTxODat era DirectorySetNode)
findDirectorySetNode = do
  directoryList <- Query.registryNodes @era
  dir <- asks Env.directoryEnv
  inta <- asks Env.transferLogicEnv

  let mintingScript = Env.programmableTokenMintingScript dir inta
      issuedPolicyId = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV3 mintingScript
      issuedSymbol = transPolicyId issuedPolicyId
  let udats =
        sortOn (Down . key . uDatum)
        $
          filter ((<= issuedSymbol) . key . uDatum) directoryList -- TODO: Should this be equality instead of LEQ?
  case udats of
    [] -> throwing_ _DirectorySetNodeNotFound
    x : _ -> pure x

-- | Register the policies
register :: forall era env err m. (C.IsBabbageBasedEra era, MonadReader env m, HasDirectoryEnv env, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBlockchain era m, MonadError err m, AsProgrammableTokensError err, MonadUtxoQuery m, Env.HasTransferLogicEnv env) => Cip143Blueprint Identity -> m ()
register blueprint = do
  let args = mkArgs blueprint
  paramsNode <- Query.globalParamsNode @era
  cborHex <- Query.issuanceCborHexUTxO @era
  udat <- findDirectorySetNode @era

  tx <- BuildTx.runBuildTxT @era (Directory.insertDirectoryNode @era paramsNode cborHex udat args)
  pure ()

-- other endpoints
-- mint
-- burn
-- transfer
-- force-transfer
