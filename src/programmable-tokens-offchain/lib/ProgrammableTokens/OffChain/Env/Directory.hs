{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO: Registry or directory?

-- | Information related to the CIP-143 registry of minting policies
module ProgrammableTokens.OffChain.Env.Directory (
    DirectoryScriptRoot (..),
    HasDirectoryEnv (..),
    DirectoryEnv (..),
    mkDirectoryEnv,
    loadFromFile,
    programmableLogicStakeCredential,
    programmableLogicBaseCredential,
    directoryNodePolicyId,
    protocolParamsPolicyId,
    issuanceCborHexPolicyId,
    globalParams,
    getGlobalParams,
    programmableTokenReceivingAddress,

    -- * Tx balancing
    balanceDeployTxEnv_,
) where

import Cardano.Api (PlutusScript, PlutusScriptV3)
import Cardano.Api qualified as C
import Control.Lens qualified as L
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx (BuildTxT)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain (queryNetworkId), queryProtocolParameters)
import Convex.CoinSelection (AsBalancingError (..), AsCoinSelectionError (..))
import Convex.CoinSelection qualified as CoinSelection
import Convex.PlutusLedger.V1 (
    transCredential,
    transPolicyId,
    unTransStakeCredential,
 )
import Convex.Utxos (BalanceChanges)
import Convex.Utxos qualified as Utxos
import Convex.Wallet.Operator (returnOutputFor)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy qualified as BSL
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Generic)
import ProgrammableTokens.JSON.Utils qualified as JSON
import ProgrammableTokens.OffChain.Env.Operator (
    HasOperatorEnv (..),
    OperatorEnv (..),
 )
import ProgrammableTokens.OffChain.Env.Utils qualified as Env
import ProgrammableTokens.OffChain.Scripts (
    directoryNodeMintingScript,
    directoryNodeSpendingScript,
    issuanceCborHexMintingScript,
    issuanceCborHexSpendingScript,
    programmableLogicBaseScript,
    programmableLogicGlobalScript,
    programmableSeizeScript,
    protocolParamsMintingScript,
    protocolParamsSpendingScript,
    scriptPolicyIdV3,
 )
import SmartTokens.Core.Scripts (ScriptTarget)
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (..))
import System.Environment qualified

{- | Data that determines the on-chain scripts of the programmable token
directory and, optionally, carries emulator-only reference-script anchors for
the programmable logic scripts.
-}
data DirectoryScriptRoot
    = DirectoryScriptRoot
    { srTxIn :: C.TxIn
    , srIssuanceCborHexTxIn :: C.TxIn
    , srTarget :: ScriptTarget
    , -- Emulator / test-only anchors for using programmable logic via reference scripts.
      srProgrammableLogicBaseRefTxIn :: Maybe C.TxIn
    , srProgrammableLogicGlobalRefTxIn :: Maybe C.TxIn
    }
    deriving (Show, Generic)

instance ToJSON DirectoryScriptRoot where
    toJSON = JSON.genericToJSON (JSON.customJsonOptions 2)
    toEncoding = JSON.genericToEncoding (JSON.customJsonOptions 2)

instance FromJSON DirectoryScriptRoot where
    parseJSON = JSON.genericParseJSON (JSON.customJsonOptions 2)

class HasDirectoryEnv e where
    directoryEnv :: e -> DirectoryEnv

instance HasDirectoryEnv DirectoryEnv where
    directoryEnv = id

instance (Env.Elem DirectoryEnv els) => HasDirectoryEnv (Env.HSet els) where
    directoryEnv = Env.hget @_ @DirectoryEnv

{- | Load the 'DirectoryScriptRoot' from a JSON file. The JSON file is specified by the
'CIP_143_DIRECTORY_SCRIPT_ROOT' environment variable.
-}
loadFromFile :: IO (Either String DirectoryEnv)
loadFromFile = do
    System.Environment.getEnv "CIP_143_DIRECTORY_SCRIPT_ROOT"
        >>= BSL.readFile
        >>= return . fmap mkDirectoryEnv . JSON.eitherDecode

{- | Scripts related to managing the token policy directory.
All of the scripts and their hashes are determined by the 'TxIn'.
-}
data DirectoryEnv
    = DirectoryEnv
    { dsScriptRoot :: DirectoryScriptRoot
    , dsDirectoryMintingScript :: PlutusScript PlutusScriptV3
    , dsDirectorySpendingScript :: PlutusScript PlutusScriptV3
    , dsProtocolParamsMintingScript :: PlutusScript PlutusScriptV3
    , dsProtocolParamsSpendingScript :: PlutusScript PlutusScriptV3
    , dsIssuanceCborHexMintingScript :: PlutusScript PlutusScriptV3
    , dsIssuanceCborHexSpendingScript :: PlutusScript PlutusScriptV3
    , dsProgrammableLogicBaseScript :: PlutusScript PlutusScriptV3
    , dsProgrammableLogicGlobalScript :: PlutusScript PlutusScriptV3
    , dsProgrammableSeizeScript :: PlutusScript PlutusScriptV3
    }

mkDirectoryEnv :: DirectoryScriptRoot -> DirectoryEnv
mkDirectoryEnv dsScriptRoot@DirectoryScriptRoot{srTxIn, srIssuanceCborHexTxIn, srTarget} =
    let dsDirectoryMintingScript = directoryNodeMintingScript srTarget srTxIn srIssuanceCborHexTxIn
        dsProtocolParamsMintingScript = protocolParamsMintingScript srTarget srTxIn
        dsProtocolParamsSpendingScript = protocolParamsSpendingScript srTarget
        dsIssuanceCborHexMintingScript = issuanceCborHexMintingScript srTarget srIssuanceCborHexTxIn
        dsIssuanceCborHexSpendingScript = issuanceCborHexSpendingScript srTarget
        dsDirectorySpendingScript = directoryNodeSpendingScript srTarget (protocolParamsPolicyId result)
        dsProgrammableLogicBaseScript = programmableLogicBaseScript srTarget (programmableLogicStakeCredential result) (protocolParamsPolicyId result) -- global stake cred + params policy id (to derive the seize cred)
        dsProgrammableLogicGlobalScript = programmableLogicGlobalScript srTarget (protocolParamsPolicyId result) -- Parameterized by the CS holding protocol params datum
        dsProgrammableSeizeScript = programmableSeizeScript srTarget (protocolParamsPolicyId result) -- standalone seize validator; its credential fills params field 3
        result =
            DirectoryEnv
                { dsScriptRoot
                , dsDirectoryMintingScript
                , dsProtocolParamsMintingScript
                , dsProtocolParamsSpendingScript
                , dsIssuanceCborHexMintingScript
                , dsIssuanceCborHexSpendingScript
                , dsProgrammableLogicBaseScript
                , dsProgrammableLogicGlobalScript
                , dsProgrammableSeizeScript
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

-- | Rewarding credential of the global transfer validator (params field 2).
programmableLogicGlobalCredential :: DirectoryEnv -> C.PaymentCredential
programmableLogicGlobalCredential =
    C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 . dsProgrammableLogicGlobalScript

-- | Rewarding credential of the standalone seize validator (params field 3).
programmableSeizeCredential :: DirectoryEnv -> C.PaymentCredential
programmableSeizeCredential =
    C.PaymentCredentialByScript . C.hashScript . C.PlutusScript C.PlutusScriptV3 . dsProgrammableSeizeScript

globalParams :: DirectoryEnv -> ProgrammableLogicGlobalParams
globalParams scripts =
    ProgrammableLogicGlobalParams
        { directoryNodeCS = transPolicyId (directoryNodePolicyId scripts)
        , progLogicCred = transCredential (programmableLogicBaseCredential scripts) -- script hash of the programmable base spending script
        , globalLogicCred = transCredential (programmableLogicGlobalCredential scripts) -- global transfer rewarding credential
        , seizeLogicCred = transCredential (programmableSeizeCredential scripts) -- standalone seize rewarding credential
        }

getGlobalParams :: (MonadReader e m, HasDirectoryEnv e) => m ProgrammableLogicGlobalParams
getGlobalParams = asks (globalParams . directoryEnv)

-- | Compute the receiving address for a payment credential and network ID
programmableTokenReceivingAddress :: forall era env m. (MonadReader env m, HasDirectoryEnv env, C.IsShelleyBasedEra era, MonadBlockchain era m) => C.PaymentCredential -> m (C.AddressInEra era)
programmableTokenReceivingAddress destinationCred = do
    nid <- queryNetworkId
    -- TODO: check if there is a better way to achieve: C.PaymentCredential -> C.StakeCredential
    stakeCred <- either (error . ("Could not unTrans credential: " <>) . show) pure $ unTransStakeCredential $ transCredential destinationCred
    progLogicBaseCred <- asks (programmableLogicBaseCredential . directoryEnv)
    return $ C.makeShelleyAddressInEra C.shelleyBasedEra nid progLogicBaseCred (C.StakeAddressByValue stakeCred)

-- | Balance a transaction using the operator's funds and return output ensuring that the issuanceCborHex TxIn is not spent.
balanceDeployTxEnv_ :: forall era env err a m. (MonadBlockchain era m, MonadReader env m, HasDirectoryEnv env, HasOperatorEnv era env, MonadError err m, C.IsBabbageBasedEra era, AsBalancingError err era, AsCoinSelectionError err) => BuildTxT era m a -> m (C.BalancedTxBody era, BalanceChanges)
balanceDeployTxEnv_ btx = do
    issuanceCborHexTxIn <- asks (srIssuanceCborHexTxIn . dsScriptRoot . directoryEnv)
    OperatorEnv{bteOperatorUtxos, bteOperator} <- asks (operatorEnv @era)
    params <- queryProtocolParameters
    txBuilder <- BuildTx.execBuildTxT $ btx >> BuildTx.setMinAdaDepositAll params

    let operatorEligibleUTxOs = L.over Utxos._UtxoSet (`Map.withoutKeys` Set.fromList [issuanceCborHexTxIn]) (Utxos.fromApiUtxo bteOperatorUtxos)
    -- TODO: change returnOutputFor to consider the stake address reference
    -- (needs to be done in sc-tools)
    output <- returnOutputFor (C.PaymentCredentialByKey $ fst bteOperator)
    CoinSelection.balanceTx mempty output operatorEligibleUTxOs txBuilder CoinSelection.TrailingChange
