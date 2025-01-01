{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Wst.Offchain.BuildTx.DirectorySet (
  initDirectorySet,
  InsertNodeArgs(..),
  insertDirectoryNode,
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
import GHC.Exts (IsList (..))
import Plutarch (Config (NoTracing))
import Plutarch.Evaluate (unsafeEvalTerm)
import Plutarch.Prelude (pconstantData)
import PlutusLedgerApi.V1 qualified as PlutusTx
import PlutusLedgerApi.V3 (Credential (..), CurrencySymbol (..))
import PlutusTx.Prelude (toBuiltin)
import SmartTokens.CodeLens (_printTerm)
import SmartTokens.LinkedList.MintDirectory (DirectoryNodeAction (..))
import SmartTokens.Types.Constants (directoryNodeToken)
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams)
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (..))
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query (UTxODat (..))
import Wst.Offchain.Scripts (directoryNodeMintingScript,
                             directoryNodeSpendingScript, scriptPolicyIdV3)

_unused :: String
_unused = _printTerm $ unsafeEvalTerm NoTracing (pconstantData initialNode)

{-|

>>> _printTerm $ unsafeEvalTerm NoTracing (pconstantData initialNode)
"program\n  1.0.0\n  (List\n     [ B #\n     , B #ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff\n     , Constr 0 [B #]\n     , Constr 0 [B #] ])"
-}
initialNode :: DirectorySetNode
initialNode = DirectorySetNode
  { key = CurrencySymbol ""
  , next = CurrencySymbol $ toBuiltin $ either (error "bytestring") id $ decode "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
  , transferLogicScript = PubKeyCredential ""
  , issuerLogicScript = PubKeyCredential ""
  }

initDirectorySet :: forall era env m. (MonadReader env m, Env.HasDirectoryEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => m ()
initDirectorySet = Utils.inBabbage @era $ do
  txIn <- asks (Env.dsTxIn . Env.directoryEnv)
  paramsPolicyId <- asks (Env.protocolParamsPolicyId . Env.directoryEnv)
  netId <- queryNetworkId
  let mintingScript = directoryNodeMintingScript txIn

  mintPlutus mintingScript InitDirectory (unTransAssetName directoryNodeToken) 1

  let
      val = C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toLedgerValue @era C.maryBasedEra
            $ fromList [(C.AssetId (scriptPolicyIdV3 mintingScript) (unTransAssetName directoryNodeToken), 1)]

      addr =
        C.makeShelleyAddressInEra
          C.shelleyBasedEra
          netId
          (C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 $ directoryNodeSpendingScript paramsPolicyId)
          C.NoStakeAddress

      dat = C.TxOutDatumInline C.babbageBasedEra $ toHashableScriptData initialNode

      output :: C.TxOut C.CtxTx era
      output = C.TxOut addr val dat C.ReferenceScriptNone

  prependTxOut output


{-| Data for a  new node to be inserted into the directory
-}
data InsertNodeArgs =
  InsertNodeArgs
    { inaNewKey :: CurrencySymbol
    , inaTransferLogic :: C.StakeCredential
    , inaIssuerLogic :: C.StakeCredential
    }

insertDirectoryNode :: forall era env m. (MonadReader env m, Env.HasDirectoryEnv env, C.IsBabbageBasedEra era, MonadBuildTx era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBlockchain era m) => UTxODat era ProgrammableLogicGlobalParams -> UTxODat era DirectorySetNode -> InsertNodeArgs -> m ()
insertDirectoryNode UTxODat{uIn=paramsRef} UTxODat{uIn, uOut=firstTxOut, uDatum=firstTxData} InsertNodeArgs{inaNewKey, inaTransferLogic, inaIssuerLogic} = Utils.inBabbage @era $ do
  netId <- queryNetworkId
  paramsPolicyId <- asks (Env.protocolParamsPolicyId . Env.directoryEnv)
  directorySpendingScript <- asks (Env.dsDirectorySpendingScript . Env.directoryEnv)
  directoryMintingScript <- asks (Env.dsDirectoryMintingScript . Env.directoryEnv)
  let

      firstTxVal :: C.TxOutValue era
      firstTxVal = case firstTxOut of
        (C.TxOut _ v _ _) -> v

      newTokenName =
        let CurrencySymbol s = inaNewKey
        in C.AssetName $ PlutusTx.fromBuiltin s

      newVal = C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toLedgerValue @era C.maryBasedEra
          $ fromList [(C.AssetId (scriptPolicyIdV3 directoryMintingScript) newTokenName, 1)]

      addr =
        C.makeShelleyAddressInEra
          C.shelleyBasedEra
          netId
          (C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 $ directoryNodeSpendingScript paramsPolicyId )
          C.NoStakeAddress

      dsn = DirectorySetNode
            { key = inaNewKey
            , next = next firstTxData
            , transferLogicScript = transStakeCredential inaTransferLogic
            , issuerLogicScript = transStakeCredential inaIssuerLogic
            }
      newDat = C.TxOutDatumInline C.babbageBasedEra $ toHashableScriptData dsn
      insertedNode = C.TxOut addr newVal newDat C.ReferenceScriptNone

      firstDat = firstTxData { next = inaNewKey }
      firstOutput = C.TxOut addr firstTxVal (C.TxOutDatumInline C.babbageBasedEra $ toHashableScriptData firstDat) C.ReferenceScriptNone
  addReference paramsRef
  spendPlutusInlineDatum uIn directorySpendingScript ()
  mintPlutus directoryMintingScript (InsertDirectoryNode inaNewKey) newTokenName 1
  prependTxOut insertedNode
  prependTxOut firstOutput
