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
import Control.Monad.Reader (MonadReader, runReaderT)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain)
import Data.Functor.Constant (Constant (..))
import Data.Functor.Identity (Identity (..))
import Data.Map qualified as Map
import PlutusLedgerApi.V3 (CurrencySymbol)
import Wst.Aiken.Blueprint (Blueprint (..), BlueprintKey)
import Wst.Offchain.BuildTx.DirectorySet (InsertNodeArgs (..))
import Wst.Offchain.BuildTx.DirectorySet qualified as Directory
import Wst.Offchain.Env (HasDirectoryEnv)
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
  InsertNodeArgs
    { inaNewKey = cbSymbol,
      inaTransferLogic = C.StakeCredentialByScript $ scriptHash $ runIdentity cbTransfer,
      inaIssuerLogic = C.StakeCredentialByScript $ scriptHash $ runIdentity cbIssuance,
      inaGlobalStateCS = cbGlobalStateCS
    }

-- | Register the policies
register :: forall era env m. (Monad m, C.IsBabbageBasedEra era, MonadReader env m, HasDirectoryEnv env, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBlockchain era m) => Cip143Blueprint Identity -> m ()
register blueprint = do
  let args = mkArgs blueprint
  paramsNode <- Query.globalParamsNode @era
  tx <- BuildTx.runBuildTxT @era (Directory.insertDirectoryNode @era _ _ args)
  pure ()

-- other endpoints
-- mint
-- burn
-- transfer
-- force-transfer
