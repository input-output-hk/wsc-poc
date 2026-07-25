
module ProgrammableTokens.OffChain.BuildTx.Utils
  ( addConwayStakeCredentialCertificate
  ) where


import Cardano.Api qualified as C
import Convex.BuildTx (MonadBuildTx, addConwayRegCertNoWitness)

{-| Add a 'C.StakeCredential' as a registration certificate to the transaction.

cardano-api 11 restructured 'C.TxCertificates' (ledger certs paired with
witnesses in an 'OMap'), so this delegates to sc-tools'
'addConwayRegCertNoWitness', which is monomorphic to 'C.ConwayEra'; the
'C.ConwayEraOnwards' GADT match recovers @era ~ C.ConwayEra@ from our callers'
'C.IsConwayBasedEra' context without changing any call site.
-}
addConwayStakeCredentialCertificate :: forall era m. C.IsConwayBasedEra era => MonadBuildTx era m => C.StakeCredential -> m ()
addConwayStakeCredentialCertificate stk =
  case C.conwayBasedEra @era of
    C.ConwayEraOnwardsConway -> addConwayRegCertNoWitness stk
