{-# LANGUAGE NamedFieldPuns #-}
module ProgrammableTokens.OffChain.BuildTx.ProtocolParams(
  mintProtocolParams,
  getProtocolParamsGlobalInline
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx (MonadBuildTx, mintPlutus, prependTxOut,
                       spendPublicKeyOutput)
import Convex.Class (MonadBlockchain (..))
import Convex.PlutusLedger.V1 (unTransAssetName)
import Convex.Scripts (fromHashableScriptData, toHashableScriptData)
import Convex.Utils qualified as Utils
import GHC.Exts (IsList (..))
import ProgrammableTokens.OffChain.Env (DirectoryEnv (..))
import ProgrammableTokens.OffChain.Env qualified as Env
import ProgrammableTokens.OffChain.Scripts (scriptPolicyIdV3)
import SmartTokens.Types.Constants (protocolParamsToken)
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams)

protocolParamsTokenC :: C.AssetName
protocolParamsTokenC = unTransAssetName protocolParamsToken

{-| Mint the protocol parameters NFT and place it in the output locked by 'protocolParamsSpendingScript'
-}
mintProtocolParams :: forall era env m. (MonadReader env m, Env.HasDirectoryEnv env, C.IsBabbageBasedEra era, MonadBuildTx era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBlockchain era m) => m ()
mintProtocolParams = Utils.inBabbage @era $ do
  txIn <- asks (Env.srTxIn . Env.dsScriptRoot . Env.directoryEnv)
  params <- asks (Env.globalParams . Env.directoryEnv)
  netId <- queryNetworkId
  DirectoryEnv{dsProtocolParamsMintingScript, dsProtocolParamsSpendingScript} <- asks Env.directoryEnv
  let policyId = scriptPolicyIdV3 dsProtocolParamsMintingScript

      val = C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toLedgerValue @era C.maryBasedEra
            $ fromList [(C.AssetId policyId protocolParamsTokenC, 1)]

      addr =
        C.makeShelleyAddressInEra
          C.shelleyBasedEra
          netId
          (C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 dsProtocolParamsSpendingScript)
          C.NoStakeAddress

      -- Should contain directoryNodeCS and progLogicCred fields
      dat = C.TxOutDatumInline C.babbageBasedEra $ toHashableScriptData params

      output :: C.TxOut C.CtxTx era
      output = C.TxOut addr val dat C.ReferenceScriptNone

  spendPublicKeyOutput txIn
  mintPlutus dsProtocolParamsMintingScript () protocolParamsTokenC 1
  prependTxOut output

getProtocolParamsGlobalInline :: C.InAnyCardanoEra (C.TxOut C.CtxTx) -> Maybe ProgrammableLogicGlobalParams
getProtocolParamsGlobalInline (C.InAnyCardanoEra _ (C.TxOut _ _ dat _)) =
  case dat of
    C.TxOutDatumInline _era (fromHashableScriptData -> Just d) -> Just d
    _ -> Nothing
