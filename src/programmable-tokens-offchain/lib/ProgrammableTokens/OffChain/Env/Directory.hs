{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}

-- | Information related to the CIP-143 registry of minting policies
module ProgrammableTokens.OffChain.Env.Directory(
  DirectoryScriptRoot(..),
  HasDirectoryEnv(..),
  DirectoryEnv(..),
  mkDirectoryEnv,
  programmableLogicStakeCredential,
  programmableLogicBaseCredential,
  directoryNodePolicyId,
  protocolParamsPolicyId,
  issuanceCborHexPolicyId,
  globalParams,
  getGlobalParams,
  programmableTokenReceivingAddress,

  -- * Tx balancing
  balanceDeployTxEnv_
) where

import Cardano.Api (PlutusScript, PlutusScriptV3)
import Cardano.Api.Shelley qualified as C
import Control.Lens qualified as L
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx (BuildTxT)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain (queryNetworkId), queryProtocolParameters)
import Convex.CoinSelection (AsBalancingError (..), AsCoinSelectionError (..))
import Convex.CoinSelection qualified as CoinSelection
import Convex.PlutusLedger.V1 (transCredential, transPolicyId,
                               unTransStakeCredential)
import Convex.Utxos (BalanceChanges)
import Convex.Utxos qualified as Utxos
import Convex.Wallet.Operator (returnOutputFor)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Generic)
import ProgrammableTokens.OffChain.Env.Operator (HasOperatorEnv (..),
                                                 OperatorEnv (..))
import ProgrammableTokens.OffChain.Scripts (directoryNodeMintingScript,
                                            directoryNodeSpendingScript,
                                            issuanceCborHexMintingScript,
                                            issuanceCborHexSpendingScript,
                                            programmableLogicBaseScript,
                                            programmableLogicGlobalScript,
                                            protocolParamsMintingScript,
                                            protocolParamsSpendingScript,
                                            scriptPolicyIdV3)
import SmartTokens.Core.Scripts (ScriptTarget)
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (..))

{-| Data that completely determines the on-chain scripts of the programmable
token directory, and their hashes. Any information that results in different
script hashes should go in here. We should be able to write a function
'DirectoryScriptRoot -> script' for all of the directory scripts.
-}
data DirectoryScriptRoot =
  DirectoryScriptRoot
    { srTxIn :: C.TxIn
    , issuanceCborHexTxIn :: C.TxIn
    , srTarget :: ScriptTarget
    }
    deriving (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

class HasDirectoryEnv e where
  directoryEnv :: e -> DirectoryEnv

instance HasDirectoryEnv DirectoryEnv where
  directoryEnv = id

{-| Scripts related to managing the token policy directory.
All of the scripts and their hashes are determined by the 'TxIn'.
-}
data DirectoryEnv =
  DirectoryEnv
    { dsScriptRoot                     :: DirectoryScriptRoot
    , dsDirectoryMintingScript         :: PlutusScript PlutusScriptV3
    , dsDirectorySpendingScript        :: PlutusScript PlutusScriptV3
    , dsProtocolParamsMintingScript    :: PlutusScript PlutusScriptV3
    , dsProtocolParamsSpendingScript   :: PlutusScript PlutusScriptV3
    , dsIssuanceCborHexMintingScript   :: PlutusScript PlutusScriptV3
    , dsIssuanceCborHexSpendingScript  :: PlutusScript PlutusScriptV3
    , dsProgrammableLogicBaseScript    :: PlutusScript PlutusScriptV3
    , dsProgrammableLogicGlobalScript  :: PlutusScript PlutusScriptV3
    }

mkDirectoryEnv :: DirectoryScriptRoot -> DirectoryEnv
mkDirectoryEnv dsScriptRoot@DirectoryScriptRoot{srTxIn, issuanceCborHexTxIn, srTarget} =
  let dsDirectoryMintingScript        = directoryNodeMintingScript srTarget srTxIn issuanceCborHexTxIn
      dsProtocolParamsMintingScript   = protocolParamsMintingScript srTarget srTxIn
      dsProtocolParamsSpendingScript  = protocolParamsSpendingScript srTarget
      dsIssuanceCborHexMintingScript  = issuanceCborHexMintingScript srTarget issuanceCborHexTxIn
      dsIssuanceCborHexSpendingScript = issuanceCborHexSpendingScript srTarget
      dsDirectorySpendingScript       = directoryNodeSpendingScript srTarget (protocolParamsPolicyId result)
      dsProgrammableLogicBaseScript   = programmableLogicBaseScript srTarget (programmableLogicStakeCredential result) -- Parameterized by the stake cred of the global script
      dsProgrammableLogicGlobalScript = programmableLogicGlobalScript srTarget (protocolParamsPolicyId result) -- Parameterized by the CS holding protocol params datum
      result = DirectoryEnv
                { dsScriptRoot
                , dsDirectoryMintingScript
                , dsProtocolParamsMintingScript
                , dsProtocolParamsSpendingScript
                , dsIssuanceCborHexMintingScript
                , dsIssuanceCborHexSpendingScript
                , dsProgrammableLogicBaseScript
                , dsProgrammableLogicGlobalScript
                , dsDirectorySpendingScript
                }
  in result

programmableLogicStakeCredential :: DirectoryEnv -> C.StakeCredential
programmableLogicStakeCredential =
  C.StakeCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 . dsProgrammableLogicGlobalScript

programmableLogicBaseCredential :: DirectoryEnv -> C.PaymentCredential
programmableLogicBaseCredential =
  C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 . dsProgrammableLogicBaseScript

directoryNodePolicyId :: DirectoryEnv -> C.PolicyId
directoryNodePolicyId = scriptPolicyIdV3 . dsDirectoryMintingScript

protocolParamsPolicyId :: DirectoryEnv -> C.PolicyId
protocolParamsPolicyId = scriptPolicyIdV3 . dsProtocolParamsMintingScript

issuanceCborHexPolicyId :: DirectoryEnv -> C.PolicyId
issuanceCborHexPolicyId = scriptPolicyIdV3 . dsIssuanceCborHexMintingScript

globalParams :: DirectoryEnv -> ProgrammableLogicGlobalParams
globalParams scripts =
  ProgrammableLogicGlobalParams
    { directoryNodeCS = transPolicyId (directoryNodePolicyId scripts)
    , progLogicCred   = transCredential (programmableLogicBaseCredential scripts) -- its the script hash of the programmable base spending script
    }

getGlobalParams :: (MonadReader e m, HasDirectoryEnv e) => m ProgrammableLogicGlobalParams
getGlobalParams = asks (globalParams . directoryEnv)

{-| Compute the receiving address for a payment credential and network ID
-}
programmableTokenReceivingAddress :: forall era env m. (MonadReader env m, HasDirectoryEnv env, C.IsShelleyBasedEra era, MonadBlockchain era m) => C.PaymentCredential -> m (C.AddressInEra era)
programmableTokenReceivingAddress destinationCred = do
  nid <- queryNetworkId
  -- TODO: check if there is a better way to achieve: C.PaymentCredential -> C.StakeCredential
  stakeCred <- either (error . ("Could not unTrans credential: " <>) . show) pure $ unTransStakeCredential $ transCredential destinationCred
  progLogicBaseCred <- asks (programmableLogicBaseCredential . directoryEnv)
  return $ C.makeShelleyAddressInEra C.shelleyBasedEra nid progLogicBaseCred (C.StakeAddressByValue stakeCred)

{-| Balance a transaction using the operator's funds and return output ensuring that the issuanceCborHex TxIn is not spent.
-}
balanceDeployTxEnv_ :: forall era env err a m. (MonadBlockchain era m, MonadReader env m, HasDirectoryEnv env, HasOperatorEnv era env, MonadError err m, C.IsBabbageBasedEra era, AsBalancingError err era, AsCoinSelectionError err) => BuildTxT era m a -> m (C.BalancedTxBody era, BalanceChanges)
balanceDeployTxEnv_ btx = do
  issuanceCborHexTxIn <- asks (issuanceCborHexTxIn . dsScriptRoot . directoryEnv)
  OperatorEnv{bteOperatorUtxos, bteOperator} <- asks operatorEnv
  params <- queryProtocolParameters
  txBuilder <- BuildTx.execBuildTxT $ btx >> BuildTx.setMinAdaDepositAll params

  let operatorEligibleUTxOs = L.over Utxos._UtxoSet (`Map.withoutKeys` Set.fromList [issuanceCborHexTxIn]) (Utxos.fromApiUtxo bteOperatorUtxos)
  -- TODO: change returnOutputFor to consider the stake address reference
  -- (needs to be done in sc-tools)
  output <- returnOutputFor (C.PaymentCredentialByKey $ fst bteOperator)
  CoinSelection.balanceTx mempty output operatorEligibleUTxOs txBuilder CoinSelection.TrailingChange
