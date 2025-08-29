{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Information related to the CIP-143 registry of minting policies
module ProgrammableTokens.OffChain.Env.TransferLogic(
  HasTransferLogicEnv(..),
  TransferLogicEnv(..),
  alwaysSucceedsTransferLogic,
  programmableTokenPolicyId,
  programmableTokenMintingScript
) where

import Cardano.Api (PlutusScript, PlutusScriptV3)
import Cardano.Api.Shelley qualified as C
import Control.Monad.Reader (MonadReader (..), asks)
import Convex.PlutusLedger.V1 (unTransCredential)
import Data.Either (fromRight)
import PlutusLedgerApi.V3 (CurrencySymbol)
import ProgrammableTokens.OffChain.Env.Directory (DirectoryEnv (..),
                                                  DirectoryScriptRoot (..),
                                                  HasDirectoryEnv (..),
                                                  globalParams)
import ProgrammableTokens.OffChain.Env.Utils qualified as Env
import ProgrammableTokens.OffChain.Scripts as Scripts
import SmartTokens.Core.Scripts (ScriptTarget)
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (..))

{-| Scripts related to managing the specific transfer logic
-}
data TransferLogicEnv =
  TransferLogicEnv
    { tleMintingScript           :: PlutusScript PlutusScriptV3 -- ^ stake validator for minting
    , tleTransferScript          :: PlutusScript PlutusScriptV3 -- ^ stake validator for transfers
    , tleIssuerScript            :: PlutusScript PlutusScriptV3 -- ^ stake validator for force spending
    , tleGlobalParamsNft         :: Maybe CurrencySymbol
    }

{-| 'IssueNewTokenArgs' for the policy that always succeeds (no checks)
-}
alwaysSucceedsTransferLogic :: ScriptTarget -> TransferLogicEnv
alwaysSucceedsTransferLogic target =
  TransferLogicEnv
    { tleMintingScript = alwaysSucceedsScript target
    , tleTransferScript = alwaysSucceedsScript target
    , tleIssuerScript = alwaysSucceedsScript target
    , tleGlobalParamsNft = Nothing
    }

class HasTransferLogicEnv e where
  transferLogicEnv :: e -> TransferLogicEnv

instance HasTransferLogicEnv TransferLogicEnv where
  transferLogicEnv = id

instance (Env.Elem TransferLogicEnv els) => HasTransferLogicEnv (Env.HSet els) where
  transferLogicEnv = Env.hget @_ @TransferLogicEnv

{-| The minting script for a programmable token that uses the global parameters
-}
programmableTokenMintingScript :: DirectoryEnv -> TransferLogicEnv -> C.PlutusScript C.PlutusScriptV3
programmableTokenMintingScript dirEnv@DirectoryEnv{dsScriptRoot} TransferLogicEnv{tleMintingScript} =
  let ProgrammableLogicGlobalParams {progLogicCred} = globalParams dirEnv
      DirectoryScriptRoot{srTarget} = dsScriptRoot
      progLogicScriptCredential = fromRight (error "could not parse protocol params") $ unTransCredential progLogicCred
  in programmableLogicMintingScript srTarget progLogicScriptCredential (C.StakeCredentialByScript $ C.hashScript $ C.PlutusScript C.plutusScriptVersion tleMintingScript)

-- | The minting policy ID of the programmable token
programmableTokenPolicyId :: (MonadReader env m, HasTransferLogicEnv env, HasDirectoryEnv env) => m C.PolicyId
programmableTokenPolicyId =
  fmap Scripts.scriptPolicyIdV3 (programmableTokenMintingScript <$> asks directoryEnv <*> asks transferLogicEnv)
