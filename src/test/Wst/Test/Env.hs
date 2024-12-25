{-| Running tests that use the 'BuildTxEv'
-}
module Wst.Test.Env(
  admin,
  asAdmin,
  asWallet,
  user,
) where

import Cardano.Api.Shelley qualified as C
import Control.Monad.Reader (MonadReader, ReaderT)
import Convex.Class (MonadUtxoQuery)
import Convex.Wallet qualified as Wallet
import Convex.Wallet.MockWallet (w1)
import Convex.Wallet.Operator (Operator (..), PaymentExtendedKey (..), Signing)
import Convex.Wallet.Operator qualified as Operator
import Data.Functor.Identity (Identity)
import Wst.Offchain.Env qualified as Env

{-| Key used for actions of the stableoin issuer / operator.
-}
admin :: Operator Signing
admin =
  Operator
    { oPaymentKey = PESigning (Wallet.getWallet w1)
    , oStakeKey   = Nothing
    }

user :: Wallet.Wallet -> Operator Signing
user w = 
  Operator
    { oPaymentKey = PESigning (Wallet.getWallet w)
    , oStakeKey = Nothing
    }

{-| Run an action using the "admin" key. Deploying the system, minting stablecoins, etc.
-}
asAdmin :: forall era o d t r m a.
  ( MonadUtxoQuery m
  , C.IsBabbageBasedEra era
  , MonadReader (Env.CombinedEnv o d t r era) m
  )
  => ReaderT (Env.CombinedEnv Identity d t r era) m a -> m a
asAdmin action = do
  env <- Env.loadOperatorEnv
          (C.verificationKeyHash . Operator.verificationKey . oPaymentKey $ admin)
          (maybe C.NoStakeAddress (C.StakeAddressByValue . C.StakeCredentialByKey . C.verificationKeyHash) $ Operator.oStakeKey admin)
  Env.withOperator env action

asWallet :: forall era o d t r m a.
  ( MonadUtxoQuery m
  , C.IsBabbageBasedEra era
  , MonadReader (Env.CombinedEnv o d t r era) m
  )
  => Wallet.Wallet -> ReaderT (Env.CombinedEnv Identity d t r era) m a -> m a
asWallet w action = do
  env <- Env.loadOperatorEnv
          (C.verificationKeyHash . Operator.verificationKey . oPaymentKey $ user w)
          (maybe C.NoStakeAddress (C.StakeAddressByValue . C.StakeCredentialByKey . C.verificationKeyHash) $ Operator.oStakeKey $ user w)
  Env.withOperator env action
