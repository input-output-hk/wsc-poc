{-# LANGUAGE NamedFieldPuns #-}

{-| Deploy the directory and global params
-}
module Wst.Offchain.Endpoints.Deployment(
  DeploymentScripts(..),
  deploymentScripts,
  programmableLogicStakeCredential,
  programmableLogicBaseCredential,
  directoryNodePolicyId,
  protocolParamsPolicyId,
  globalParams,
  deployTx
) where

import Cardano.Api (PlutusScript, PlutusScriptV3)
import Cardano.Api.Shelley qualified as C
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Convex.Class (MonadBlockchain)
import Convex.CoinSelection qualified
import Convex.PlutusLedger.V1 (transCredential, transPolicyId)
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (..))
import Wst.Offchain.BuildTx.DirectorySet (initDirectorySet)
import Wst.Offchain.BuildTx.ProtocolParams (mintProtocolParams)
import Wst.Offchain.Endpoints.Env (BuildTxEnv, BuildTxError)
import Wst.Offchain.Endpoints.Env qualified as Env
import Wst.Offchain.Scripts (directoryNodeMintingScript,
                             programmableLogicBaseScript,
                             programmableLogicGlobalScript,
                             protocolParamsMintingScript, scriptPolicyIdV3)

data DeploymentScripts =
  DeploymentScripts
    { dsTxIn :: C.TxIn -- ^ The 'txIn' that we spend when deploying the protocol params and directory set
    , dsDirectoryMintingScript        :: PlutusScript PlutusScriptV3
    , dsProtocolParamsMintingScript   :: PlutusScript PlutusScriptV3
    , dsProgrammableLogicBaseScript   :: PlutusScript PlutusScriptV3
    , dsProgrammableLogicGlobalScript :: PlutusScript PlutusScriptV3
    }

deploymentScripts :: C.TxIn -> DeploymentScripts
deploymentScripts dsTxIn =
  let dsDirectoryMintingScript        = directoryNodeMintingScript dsTxIn
      dsProtocolParamsMintingScript   = protocolParamsMintingScript dsTxIn
      dsProgrammableLogicBaseScript   = programmableLogicBaseScript (programmableLogicStakeCredential result) -- Parameterized by the stake cred of the global script
      dsProgrammableLogicGlobalScript = programmableLogicGlobalScript (directoryNodePolicyId result) -- Parameterized by the CS holding protocol params datum
      result = DeploymentScripts
                { dsTxIn
                , dsDirectoryMintingScript
                , dsProtocolParamsMintingScript
                , dsProgrammableLogicBaseScript
                , dsProgrammableLogicGlobalScript
                }
  in result

programmableLogicStakeCredential :: DeploymentScripts -> C.StakeCredential
programmableLogicStakeCredential =
  C.StakeCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 . dsProgrammableLogicGlobalScript

programmableLogicBaseCredential :: DeploymentScripts -> C.PaymentCredential
programmableLogicBaseCredential =
  C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 . dsProgrammableLogicBaseScript

directoryNodePolicyId :: DeploymentScripts -> C.PolicyId
directoryNodePolicyId = scriptPolicyIdV3 . dsDirectoryMintingScript

protocolParamsPolicyId :: DeploymentScripts -> C.PolicyId
protocolParamsPolicyId = scriptPolicyIdV3 . dsProtocolParamsMintingScript

globalParams :: DeploymentScripts -> ProgrammableLogicGlobalParams
globalParams scripts =
  ProgrammableLogicGlobalParams
    { directoryNodeCS = transPolicyId (directoryNodePolicyId scripts)
    , progLogicCred   = transCredential (programmableLogicBaseCredential scripts) -- its the script hash of the programmable base spending script
    }

{-| Build a transaction that deploys the directory and global params. Returns the
transaction and the 'TxIn' that was selected for the one-shot NFTs.
-}
deployTx :: (MonadReader (BuildTxEnv era) m, MonadBlockchain era m, MonadError (BuildTxError era) m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era) => m (C.Tx era, C.TxIn)
deployTx = do
  (txi, _) <- Env.selectOperatorOutput
  let scripts = deploymentScripts txi
  (tx, _) <- Env.balanceTxEnv $ do
          mintProtocolParams (globalParams scripts) txi
          initDirectorySet (protocolParamsPolicyId scripts) txi
  pure (Convex.CoinSelection.signBalancedTxBody [] tx, txi)
