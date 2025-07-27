{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

{-| Transaction building environment
-}
module Wst.Offchain.Env(

  -- ** Using the operator environment
  balanceDeployTxEnv_,

  -- * On-chain scripts

  -- ** Directory environment
  DirectoryScriptRoot(..),
  mkDirectoryEnv,
  HasDirectoryEnv(..),
  DirectoryEnv(..),
  programmableLogicStakeCredential,
  programmableLogicBaseCredential,
  directoryNodePolicyId,
  protocolParamsPolicyId,
  issuanceCborHexPolicyId,
  globalParams,
  getGlobalParams,

  -- ** Blacklist environment
  BlacklistEnv(..),
  HasBlacklistEnv(..),
  alwaysSucceedsBlacklistEnv,
  mkBlacklistEnv,

  -- ** Transfer logic environment
  BlacklistTransferLogicScriptRoot(..),
  mkTransferLogicEnv,
  TransferLogicEnv(..),
  transferLogicForDirectory,
  alwaysSucceedsTransferLogic,
  HasTransferLogicEnv(..),
  addTransferEnv,
  withTransfer,
  withTransferFor,
  withTransferFromOperator,

  -- ** Minting tokens
  programmableTokenMintingScript,
  programmableTokenAssetId,
  programmableTokenReceivingAddress,

  -- * Runtime data
  RuntimeEnv(..),
  HasRuntimeEnv(..),
  loadRuntimeEnv,

  -- * Combined environment
  CombinedEnv(..),
  empty,
  withEnv,
  addDirectoryEnvFor,
  addDirectoryEnv,
  withDirectory,
  withDirectoryFor,
  addRuntimeEnv,
  withRuntime,
  addOperatorEnv,
  withOperator,
  blacklistNodePolicyId,
  withBlacklist,
  withBlacklistFor,
  addBlacklistEnv
) where

import Blammo.Logging (Logger)
import Blammo.Logging.Logger (HasLogger (..), newLogger)
import Blammo.Logging.LogSettings.Env qualified as LogSettingsEnv
import Blockfrost.Auth (mkProject)
import Blockfrost.Client.Auth qualified as Blockfrost
import Cardano.Api (PlutusScript, PlutusScriptV3)
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Lens (makeLensesFor)
import Control.Lens qualified as L
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Convex.BuildTx (BuildTxT)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain (queryNetworkId), queryProtocolParameters)
import Convex.CoinSelection (AsBalancingError (..), AsCoinSelectionError (..))
import Convex.CoinSelection qualified as CoinSelection
import Convex.PlutusLedger.V1 (transCredential, transPolicyId,
                               unTransCredential, unTransStakeCredential)
import Convex.Utxos (BalanceChanges)
import Convex.Utxos qualified as Utxos
import Convex.Wallet.Operator (returnOutputFor)
import Data.Aeson (FromJSON, ToJSON)
import Data.Either (fromRight)
import Data.Functor.Identity (Identity (..))
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Generics (Generic)
import PlutusLedgerApi.V3 (CurrencySymbol)
import ProgrammableTokens.OffChain.Env (HasOperatorEnv (..), OperatorEnv (..))
import SmartTokens.Core.Scripts (ScriptTarget)
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (..))
import System.Environment qualified
import Wst.Offchain.Scripts (alwaysSucceedsScript, blacklistMintingScript,
                             blacklistSpendingScript,
                             directoryNodeMintingScript,
                             directoryNodeSpendingScript, freezeTransferScript,
                             issuanceCborHexMintingScript,
                             issuanceCborHexSpendingScript,
                             permissionedMintingScript,
                             permissionedSpendingScript,
                             programmableLogicBaseScript,
                             programmableLogicGlobalScript,
                             programmableLogicMintingScript,
                             protocolParamsMintingScript,
                             protocolParamsSpendingScript, scriptPolicyIdV3)

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

data BlacklistEnv =
  BlacklistEnv
    { bleMintingScript  :: PlutusScript PlutusScriptV3
    , bleSpendingScript :: PlutusScript PlutusScriptV3
    }

alwaysSucceedsBlacklistEnv :: ScriptTarget -> BlacklistEnv
alwaysSucceedsBlacklistEnv target =
  BlacklistEnv
    { bleMintingScript = alwaysSucceedsScript target
    , bleSpendingScript = alwaysSucceedsScript target
    }

class HasBlacklistEnv e where
  blacklistEnv :: e -> BlacklistEnv

instance HasBlacklistEnv BlacklistEnv where
  blacklistEnv = id

mkBlacklistEnv :: BlacklistTransferLogicScriptRoot -> BlacklistEnv
mkBlacklistEnv BlacklistTransferLogicScriptRoot{tlrTarget, tlrIssuer} =
  let blacklistMinting = blacklistMintingScript tlrTarget tlrIssuer
  in BlacklistEnv
      { bleMintingScript = blacklistMinting
      , bleSpendingScript = blacklistSpendingScript tlrTarget tlrIssuer
      }

{-| Scripts related to managing the specific transfer logic
-}

data TransferLogicEnv =
  TransferLogicEnv
    { tleMintingScript           :: PlutusScript PlutusScriptV3
    , tleTransferScript          :: PlutusScript PlutusScriptV3
    , tleIssuerScript            :: PlutusScript PlutusScriptV3
    , tleGlobalParamsNft         :: Maybe CurrencySymbol
    }

{-| 'IssueNewTokenArgs' for the policy that always succeeds (no checks)
-}
alwaysSucceedsTransferLogic :: ScriptTarget -> TransferLogicEnv
alwaysSucceedsTransferLogic target =
  TransferLogicEnv
    { tleMintingScript = alwaysSucceedsScript target
    , tleTransferScript = alwaysSucceedsScript target
    , tleIssuerScript = alwaysSucceedsScript target
    , tleGlobalParamsNft = Nothing
    }

class HasTransferLogicEnv e where
  transferLogicEnv :: e -> TransferLogicEnv

instance HasTransferLogicEnv TransferLogicEnv where
  transferLogicEnv = id

{-| Data that completely determines the on-chain scripts of the blacklist
transfer logic, and their hashes. Any information that results in different
script hashes should go in here. We should be able to write a function
'BlacklistTransferLogicScriptRoot -> script' for all of the blacklist transfer
logic scripts.
-}
data BlacklistTransferLogicScriptRoot =
  BlacklistTransferLogicScriptRoot
    { tlrTarget :: ScriptTarget
    , tlrDirEnv :: DirectoryEnv
    , tlrIssuer :: C.Hash C.PaymentKey
    }

mkTransferLogicEnv :: BlacklistTransferLogicScriptRoot -> TransferLogicEnv
mkTransferLogicEnv BlacklistTransferLogicScriptRoot{tlrTarget, tlrDirEnv, tlrIssuer} =
  let blacklistMinting = blacklistMintingScript tlrTarget tlrIssuer
      blacklistPolicy = scriptPolicyIdV3 blacklistMinting
      progLogicBaseCred = programmableLogicBaseCredential tlrDirEnv
  in
  TransferLogicEnv
    { tleMintingScript =  permissionedMintingScript tlrTarget tlrIssuer
    , tleTransferScript = freezeTransferScript tlrTarget progLogicBaseCred blacklistPolicy
    , tleIssuerScript = permissionedSpendingScript tlrTarget tlrIssuer
    , tleGlobalParamsNft = Nothing
    }

blacklistNodePolicyId :: BlacklistEnv -> C.PolicyId
blacklistNodePolicyId = scriptPolicyIdV3 . bleMintingScript

data RuntimeEnv
  = RuntimeEnv
      { envLogger      :: Logger
      , envBlockfrost  :: Blockfrost.Project
      }

makeLensesFor
  [ ("envLogger", "logger")
  , ("envBlockfrostProject", "blockfrostProject")
  ]
  'RuntimeEnv

instance HasLogger RuntimeEnv where
  loggerL = logger

-- | Load the 'RuntimeEnv' from environment variables
loadRuntimeEnv :: IO RuntimeEnv
loadRuntimeEnv =
  RuntimeEnv
    <$> (LogSettingsEnv.parse >>= newLogger)
    <*> fmap (mkProject . Text.pack) (System.Environment.getEnv "WST_BLOCKFROST_TOKEN")

class HasRuntimeEnv e where
  runtimeEnv :: e -> RuntimeEnv

instance HasRuntimeEnv RuntimeEnv where
  runtimeEnv = id

data CombinedEnv operatorF directoryF transferF runtimeF blacklistF era =
  CombinedEnv
    { ceOperator  :: operatorF (OperatorEnv era)
    , ceDirectory :: directoryF DirectoryEnv
    , ceTransfer  :: transferF TransferLogicEnv
    , ceRuntime   :: runtimeF RuntimeEnv
    , ceBlacklist :: blacklistF BlacklistEnv
    }

makeLensesFor
  [("ceRuntime", "runtime")]
  ''CombinedEnv

{-| 'CombinedEnv' with no values
-}
empty :: forall era. CombinedEnv Proxy Proxy Proxy Proxy Proxy era
empty =
  CombinedEnv
    { ceOperator = Proxy
    , ceDirectory = Proxy
    , ceTransfer = Proxy
    , ceRuntime = Proxy
    , ceBlacklist = Proxy
    }

instance HasOperatorEnv era (CombinedEnv Identity d t r b era) where
  operatorEnv = runIdentity . ceOperator

instance HasDirectoryEnv (CombinedEnv o Identity t r b era) where
  directoryEnv = runIdentity . ceDirectory

instance HasTransferLogicEnv (CombinedEnv o d Identity r b era) where
  transferLogicEnv = runIdentity . ceTransfer

instance HasRuntimeEnv (CombinedEnv o d t Identity b era) where
  runtimeEnv = runIdentity . ceRuntime

instance HasBlacklistEnv (CombinedEnv o d t r Identity era) where
  blacklistEnv = runIdentity . ceBlacklist

_Identity :: L.Iso' (Identity a) a
_Identity = L.iso runIdentity Identity

instance HasLogger (CombinedEnv o d t Identity b era) where
  loggerL = runtime . _Identity . loggerL

{-| Add a 'DirectoryEnv' for the 'C.TxIn' in to the environment
-}
addDirectoryEnvFor :: DirectoryScriptRoot -> CombinedEnv o d t r b era -> CombinedEnv o Identity t r b era
addDirectoryEnvFor = addDirectoryEnv . mkDirectoryEnv

{-| Add a 'DirectoryEnv' for the 'C.TxIn' in to the environment
-}
addDirectoryEnv :: DirectoryEnv -> CombinedEnv o d t r b era -> CombinedEnv o Identity t r b era
addDirectoryEnv de env =
  env{ceDirectory = Identity de }

withDirectory :: MonadReader (CombinedEnv o d t r b era) m => DirectoryEnv -> ReaderT (CombinedEnv o Identity t r b era) m a -> m a
withDirectory dir action = do
  asks (addDirectoryEnv dir)
    >>= runReaderT action

withDirectoryFor :: MonadReader (CombinedEnv o d t r b era) m => DirectoryScriptRoot -> ReaderT (CombinedEnv o Identity t r b era) m a -> m a
withDirectoryFor = withDirectory . mkDirectoryEnv

{-| Add a 'TransferLogicEnv' for the 'C.Hash C.PaymentKey' corresponding to the
   admin hash
 -}
addTransferEnv :: TransferLogicEnv -> CombinedEnv o d t r b era -> CombinedEnv o d Identity r b era
addTransferEnv de env =
  env{ceTransfer = Identity de }

withTransfer :: MonadReader (CombinedEnv o d t r b era) m => TransferLogicEnv -> ReaderT (CombinedEnv o d Identity r b era) m a -> m a
withTransfer dir action = do
  asks (addTransferEnv dir)
    >>= runReaderT action

withTransferFor :: MonadReader (CombinedEnv o Identity t r b era) m => BlacklistTransferLogicScriptRoot -> ReaderT (CombinedEnv o Identity Identity r b era) m a -> m a
withTransferFor = withTransfer . mkTransferLogicEnv

{-| Transfer logic scripts for the blacklist managed by the given 'C.PaymentKey' hash
-}
transferLogicForDirectory :: (HasDirectoryEnv env, MonadReader env m) => C.Hash C.PaymentKey -> m (TransferLogicEnv, BlacklistEnv)
transferLogicForDirectory pkh = do
  env <- ask
  let dirEnv = directoryEnv env
      sr     = BlacklistTransferLogicScriptRoot (srTarget $ dsScriptRoot dirEnv) dirEnv pkh
  pure (mkTransferLogicEnv sr, mkBlacklistEnv sr)

withTransferFromOperator :: (MonadReader (CombinedEnv Identity Identity t r b era) m) => ReaderT (CombinedEnv Identity Identity Identity r Identity era) m a -> m a
withTransferFromOperator action = do
  env <- ask
  let opPkh = fst . bteOperator . operatorEnv $ env
  (transferEnv,  ble) <- transferLogicForDirectory opPkh
  runReaderT action (addTransferEnv transferEnv $ addBlacklistEnv ble env)

addBlacklistEnv :: BlacklistEnv -> CombinedEnv o d t r b era -> CombinedEnv o d t r Identity era
addBlacklistEnv be env =
  env{ceBlacklist = Identity be}

withBlacklist :: MonadReader (CombinedEnv o d t r b era) m => BlacklistEnv -> ReaderT (CombinedEnv o d t r Identity era) m a -> m a
withBlacklist env action =
  asks (addBlacklistEnv env)
    >>= runReaderT action

withBlacklistFor :: MonadReader (CombinedEnv o d t r b era) m => BlacklistTransferLogicScriptRoot -> ReaderT (CombinedEnv o d t r Identity era) m a -> m a
withBlacklistFor = withBlacklist . mkBlacklistEnv

{-| The minting script for a programmable token that uses the global parameters
-}
programmableTokenMintingScript :: DirectoryEnv -> TransferLogicEnv -> C.PlutusScript C.PlutusScriptV3
programmableTokenMintingScript dirEnv@DirectoryEnv{dsScriptRoot} TransferLogicEnv{tleMintingScript} =
  let ProgrammableLogicGlobalParams {progLogicCred} = globalParams dirEnv
      DirectoryScriptRoot{srTarget} = dsScriptRoot
      progLogicScriptCredential = fromRight (error "could not parse protocol params") $ unTransCredential progLogicCred
  in programmableLogicMintingScript srTarget progLogicScriptCredential (C.StakeCredentialByScript $ C.hashScript $ C.PlutusScript C.plutusScriptVersion tleMintingScript)

{-| 'C.AssetId' of the programmable tokens
-}
programmableTokenAssetId :: DirectoryEnv -> TransferLogicEnv -> C.AssetName -> C.AssetId
programmableTokenAssetId dirEnv inta =
  C.AssetId
    (C.scriptPolicyId $ C.PlutusScript C.plutusScriptVersion $ programmableTokenMintingScript dirEnv inta)

{-| Add a 'DirectoryEnv' for the 'C.TxIn' in to the environment and run the
action with the modified environment
-}
withEnv :: forall era m a. ReaderT (CombinedEnv Proxy Proxy Proxy Proxy Proxy era) m a -> m a
withEnv = flip runReaderT empty

{-| Add a 'RuntimeEnv' to the environment
-}
addRuntimeEnv :: RuntimeEnv -> CombinedEnv o d t r b era -> CombinedEnv o d t Identity b era
addRuntimeEnv env e =
  e{ceRuntime = Identity env }

withRuntime :: MonadReader (CombinedEnv o d t r b era) m => RuntimeEnv -> ReaderT (CombinedEnv o d t Identity b era) m a -> m a
withRuntime runtime_ action =
  asks (addRuntimeEnv runtime_)
    >>= runReaderT action

{-| Add an 'OperatorEnv' to the environment
-}
addOperatorEnv :: OperatorEnv era -> CombinedEnv o d t r b era2 -> CombinedEnv Identity d t r b era
addOperatorEnv op e =
  e{ceOperator = Identity op }

withOperator :: MonadReader (CombinedEnv o d t r b era1) m => OperatorEnv era -> ReaderT (CombinedEnv Identity d t r b era) m a -> m a
withOperator op action = asks (addOperatorEnv op) >>= runReaderT action
