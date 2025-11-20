
module Wst.Offchain.ExampleTransfer (
  issueStablecoin,
  transferStablecoin,
  seizeStablecoin
  )
  where

import Cardano.Api qualified as C
import Cardano.Api.Plutus qualified as C
import Control.Lens (over)
import Convex.BuildTx (MonadBuildTx, addBtx, addReference,
                       addStakeScriptWitness, addWithdrawalWithTxBody,
                       buildScriptWitness, findIndexReference, mintPlutus,
                       spendPlutusInlineDatum,
                       spendPlutusRefWithoutInRefInlineDatum,
                       spendPublicKeyOutput)
import Convex.CardanoApi.Lenses qualified as L
import Convex.PlutusLedger (transPolicyId, unTransAssetName)
import Convex.Scripts (fromHashableScriptData, toHashableScriptData)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.List (find)
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import PlutusLedgerApi.V3 (Credential (..), CurrencySymbol (..))
import PlutusLedgerApi.V3 qualified as P
import PlutusTx qualified
import Wst.Offchain.DirectorySet (DirectorySetNode (..))


issueStablecoin :: (MonadBuildTx C.ConwayEra m) => C.NetworkId -> C.Hash C.PaymentKey -> m ()
issueStablecoin = undefined

transferStablecoin :: (MonadBuildTx C.ConwayEra m) => C.NetworkId -> C.Hash C.PaymentKey -> m ()
transferStablecoin = undefined

seizeStablecoin :: (MonadBuildTx C.ConwayEra m) => C.NetworkId -> C.Hash C.PaymentKey -> m ()
seizeStablecoin = undefined
