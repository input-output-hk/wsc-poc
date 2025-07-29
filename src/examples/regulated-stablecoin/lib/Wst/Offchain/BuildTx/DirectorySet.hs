{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Wst.Offchain.BuildTx.DirectorySet (
  initDirectorySet,
  -- * Values
  initialNode
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx (MonadBuildTx, addReference, mintPlutus, prependTxOut,
                       spendPlutusInlineDatum)
import Convex.Class (MonadBlockchain, queryNetworkId)
import Convex.PlutusLedger.V1 (transStakeCredential, unTransAssetName)
import Convex.Scripts (toHashableScriptData)
import Convex.Utils qualified as Utils
import Data.ByteString.Base16 (decode)
import Data.Maybe (fromMaybe)
import GHC.Exts (IsList (..))
import Plutarch.Evaluate (unsafeEvalTerm)
import Plutarch.Internal.Term (Config (NoTracing))
import Plutarch.Prelude (pconstant)
import PlutusLedgerApi.V1 qualified as PlutusTx
import PlutusLedgerApi.V3 (Credential (..), CurrencySymbol (..),
                           ScriptHash (..))
import PlutusTx.Prelude (toBuiltin)
import ProgrammableTokens.OffChain.Scripts (scriptPolicyIdV3)
import SmartTokens.CodeLens (_printTerm)
import SmartTokens.Contracts.IssuanceCborHex (IssuanceCborHex (..))
import SmartTokens.LinkedList.MintDirectory (DirectoryNodeAction (..))
import SmartTokens.Types.Constants (directoryNodeToken)
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams)
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (..),
                                          PDirectorySetNode)
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query (UTxODat (..))

_unused :: String
_unused = _printTerm $ unsafeEvalTerm NoTracing (pconstant @PDirectorySetNode initialNode)

{-|

>>> _printTerm $ unsafeEvalTerm NoTracing (pconstant initialNode)
"program\n  1.0.0\n  (List\n     [ B #\n     , B #ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff\n     , Constr 0 [B #]\n     , Constr 0 [B #] ])"
-}
initialNode :: DirectorySetNode
initialNode = DirectorySetNode
  { key = CurrencySymbol ""
  , next = CurrencySymbol $ toBuiltin $ either (error "bytestring") id $ decode "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
  , transferLogicScript = PubKeyCredential ""
  , issuerLogicScript = PubKeyCredential ""
  , globalStateCS = CurrencySymbol ""
  }

initDirectorySet :: forall era env m. (MonadReader env m, Env.HasDirectoryEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => m ()
initDirectorySet = Utils.inBabbage @era $ do
  netId <- queryNetworkId
  directoryMintingScript <- asks (Env.dsDirectoryMintingScript . Env.directoryEnv)
  directorySpendingScript <- asks (Env.dsDirectorySpendingScript . Env.directoryEnv)

  mintPlutus directoryMintingScript InitDirectory (unTransAssetName directoryNodeToken) 1

  let
      val = C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toLedgerValue @era C.maryBasedEra
            $ fromList [(C.AssetId (scriptPolicyIdV3 directoryMintingScript) (unTransAssetName directoryNodeToken), 1)]

      addr =
        C.makeShelleyAddressInEra
          C.shelleyBasedEra
          netId
          (C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 directorySpendingScript)
          C.NoStakeAddress

      dat = C.TxOutDatumInline C.babbageBasedEra $ toHashableScriptData initialNode

      output :: C.TxOut C.CtxTx era
      output = C.TxOut addr val dat C.ReferenceScriptNone

  prependTxOut output
