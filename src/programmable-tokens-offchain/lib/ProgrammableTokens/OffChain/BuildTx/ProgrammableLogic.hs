module ProgrammableTokens.OffChain.BuildTx.ProgrammableLogic(
  registerProgrammableGlobalScript
) where

import Cardano.Api.Shelley qualified as C
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx (MonadBuildTx)
import Convex.Utils qualified as Utils
import ProgrammableTokens.OffChain.BuildTx.Utils qualified as Utils
import ProgrammableTokens.OffChain.Env qualified as Env

registerProgrammableGlobalScript :: forall env era m. (MonadReader env m, C.IsBabbageBasedEra era, MonadBuildTx era m, Env.HasDirectoryEnv env) => m ()
registerProgrammableGlobalScript = case C.babbageBasedEra @era of
  C.BabbageEraOnwardsBabbage -> error "babbage era registration not implemented"
  C.BabbageEraOnwardsConway  -> Utils.inConway @era $ do
    programmableGlobalScript <- asks (Env.dsProgrammableLogicGlobalScript . Env.directoryEnv)
    let hshGlobal = C.hashScript $ C.PlutusScript C.plutusScriptVersion programmableGlobalScript
        credGlobal = C.StakeCredentialByScript hshGlobal
    Utils.addConwayStakeCredentialCertificate credGlobal
