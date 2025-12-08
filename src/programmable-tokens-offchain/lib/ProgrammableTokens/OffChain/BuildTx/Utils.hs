
module ProgrammableTokens.OffChain.BuildTx.Utils
  ( addConwayStakeCredentialCertificate
  ) where


import Cardano.Api qualified as C
import Cardano.Ledger.Shelley.TxCert qualified as TxCert
import Convex.BuildTx (MonadBuildTx, addCertificate)

{-| Add a 'C.StakeCredential' as a certificate to the transaction
-}
addConwayStakeCredentialCertificate :: forall era m. C.IsConwayBasedEra era => MonadBuildTx era m => C.StakeCredential -> m ()
addConwayStakeCredentialCertificate stk =
  C.conwayEraOnwardsConstraints @era C.conwayBasedEra $
  addCertificate $ C.ConwayCertificate C.conwayBasedEra $ TxCert.RegTxCert $ C.toShelleyStakeCredential stk
