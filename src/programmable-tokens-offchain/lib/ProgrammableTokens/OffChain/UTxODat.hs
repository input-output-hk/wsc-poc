{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
module ProgrammableTokens.OffChain.UTxODat(
  UTxODat(..),
  fromOutputNoDatum,
  extractUtxoNoDatum,
  fromOutput,
  extractUTxO,
  programmableOutputOwner,
  ownerWitnessOrder
) where

import Cardano.Api qualified as C
import Control.Lens qualified as L
import Control.Monad ((>=>))
import Convex.CardanoApi.Lenses qualified as L
import Convex.Scripts (fromHashableScriptData)
import Convex.Utxos (UtxoSet, toApiUtxo)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON
import Data.Map qualified as Map
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.OpenApi.Schema (ToSchema (..))
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import PlutusTx qualified
import ProgrammableTokens.JSON.Utils qualified as JSON
import ProgrammableTokens.OffChain.Orphans ()

{-| Unspent transaction output with 'TxIn', 'TxOut' and an inline datum
-}
data UTxODat era a =
  UTxODat
    { uIn    :: C.TxIn
    , uOut   :: C.TxOut C.CtxUTxO era
    , uDatum :: a
    }
  deriving stock (Eq, Show, Generic)

-- | Aeson options for the UTxODat type. Used to derive JSON instances and ToSchema
utxoDatOptions :: JSON.Options
utxoDatOptions = JSON.customJsonOptions 1

instance (C.IsCardanoEra era, ToJSON a) => ToJSON (UTxODat era a) where
  toJSON = JSON.genericToJSON utxoDatOptions
  toEncoding = JSON.genericToEncoding utxoDatOptions

instance (C.IsCardanoEra era, FromJSON a, C.IsShelleyBasedEra era) => FromJSON (UTxODat era a) where
  parseJSON = JSON.genericParseJSON utxoDatOptions

instance (Typeable a, ToSchema a, Typeable era) => ToSchema (UTxODat era a) where
  declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions utxoDatOptions)

fromOutputNoDatum :: forall era. (C.IsBabbageBasedEra era) => C.TxIn -> C.TxOut C.CtxUTxO era -> Maybe (UTxODat era ())
fromOutputNoDatum uIn uOut@(L.preview (L._TxOut . L._3 . L._TxOutDatumInline) >=> fromHashableScriptData -> Just ()) = Just UTxODat{uIn, uOut, uDatum = ()}
fromOutputNoDatum uIn uOut = Just $ UTxODat{uIn, uOut, uDatum = ()}

extractUtxoNoDatum :: forall era b. (C.IsBabbageBasedEra era) => UtxoSet C.CtxUTxO b -> [UTxODat era ()]
extractUtxoNoDatum = mapMaybe (uncurry fromOutputNoDatum) . Map.toList . C.unUTxO . toApiUtxo @era

fromOutput :: forall era a. (PlutusTx.FromData a, C.IsBabbageBasedEra era) => C.TxIn -> C.TxOut C.CtxUTxO era -> Maybe (UTxODat era a)
fromOutput uIn uOut@(L.preview (L._TxOut . L._3 . L._TxOutDatumInline) >=> fromHashableScriptData -> Just uDatum) = Just UTxODat{uIn, uOut, uDatum}
fromOutput _ _ = Nothing

extractUTxO :: forall era a b. (PlutusTx.FromData a, C.IsBabbageBasedEra era) => UtxoSet C.CtxUTxO b -> [UTxODat era a]
extractUTxO = mapMaybe (uncurry fromOutput) . Map.toList . C.unUTxO . toApiUtxo @era

{- | The mini-ledger owner of a programmable UTxO, which is its address's
STAKING credential. Every programmable UTxO shares one payment credential -- the
base script -- so the staking credential is the only thing that says who owns
it, and it is what the transfer path demands a witness for.

Errors on an address with no staking credential: such a UTxO has no owner, so
no witness exists for it and the transfer validator rejects it. Failing here
gives a comprehensible message instead of an opaque script failure.
-}
programmableOutputOwner :: C.TxOut ctx era -> C.StakeCredential
programmableOutputOwner (C.TxOut addr _ _ _) = case addr of
    C.AddressInEra _ (C.ShelleyAddress _ _ (C.fromShelleyStakeReference -> C.StakeAddressByValue stakeCred)) -> stakeCred
    _ ->
        error "programmableOutputOwner: programmable UTxO has no staking credential, so it has no owner and cannot be spent"

{- | The mini-ledger owners that a 'TransferAct' redeemer must carry witness
indices for, in the order the validator expects them.

Two rules, and both are easy to get wrong, which is why this is a separate
function with its own tests rather than a comprehension inside the builder:

* Only SCRIPT owners appear. A pubkey owner is witnessed by its signature and
  consumes no entry, so including one would push every later index onto the
  wrong input.
* The order is by 'TxIn', because that is the order the ledger presents inputs
  in and the validator consumes the list as it walks them -- not the order the
  caller happened to pass them in.
-}
ownerWitnessOrder :: [(C.TxIn, C.StakeCredential)] -> [C.StakeCredential]
ownerWitnessOrder = mapMaybe scriptOwner . sortOn fst
  where
    scriptOwner (_, cred@C.StakeCredentialByScript{}) = Just cred
    scriptOwner _ = Nothing
