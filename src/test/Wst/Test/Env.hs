{-| Running tests that use the 'BuildTxEv'
-}
module Wst.Test.Env(
  asAdmin
) where

import Cardano.Api qualified as C
import Control.Monad.Reader (ReaderT, runReaderT)
import Convex.Class (MonadUtxoQuery)
import Convex.Wallet qualified as Wallet
import Convex.Wallet.MockWallet (w1)
import Convex.Wallet.Operator (Operator (..), PaymentExtendedKey (..), Signing)
import Convex.Wallet.Operator qualified as Operator
import Wst.Offchain.Endpoints.Env (BuildTxEnv)
import Wst.Offchain.Endpoints.Env qualified as Env

{-| Key used for actions of the stableoin issuer / operator.
-}
admin :: Operator Signing
admin =
  Operator
    { oPaymentKey = PESigning (Wallet.getWallet w1)
    , oStakeKey   = Nothing
    }

{-| Run an action using the "admin" key. Deploying the system, minting stablecoins, etc.
-}
asAdmin :: forall era m a. (MonadUtxoQuery m, C.IsBabbageBasedEra era) => ReaderT (BuildTxEnv era) m a -> m a
asAdmin action = do
  env <- Env.loadEnv (Operator.verificationKey $ Operator.oPaymentKey admin) (Operator.oStakeKey admin)
  runReaderT action env
