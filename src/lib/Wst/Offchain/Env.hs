{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-| Transaction building environment
-}
module Wst.Offchain.Env(
  -- * Operator environment
  HasOperatorEnv(..),
  OperatorEnv(..),
  loadEnv,
  BuildTxError(..),

  -- ** Using the operator environment
  selectOperatorOutput,
  balanceTxEnv,

  -- * Directory environment
  HasDirectoryEnv(..),
  DirectoryEnv(..),
  mkDirectoryEnv,
  programmableLogicStakeCredential,
  programmableLogicBaseCredential,
  directoryNodePolicyId,
  protocolParamsPolicyId,
  globalParams,

  -- * Combined environment
  CombinedEnv(..),
  withDirectoryFor
) where

import Cardano.Api (PlutusScript, PlutusScriptV3, UTxO)
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Convex.BuildTx (BuildTxT)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain, MonadUtxoQuery (..),
                     queryProtocolParameters, utxosByPaymentCredential)
import Convex.CoinSelection qualified as CoinSelection
import Convex.PlutusLedger.V1 (transCredential, transPolicyId)
import Convex.Utils (mapError)
import Convex.Utxos (BalanceChanges)
import Convex.Utxos qualified as Utxos
import Convex.Wallet.Operator (Operator (..), PaymentExtendedKey (..),
                               Verification, operatorPaymentCredential,
                               operatorReturnOutput)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (..))
import Wst.Offchain.Scripts (directoryNodeMintingScript,
                             directoryNodeSpendingScript,
                             programmableLogicBaseScript,
                             programmableLogicGlobalScript,
                             protocolParamsMintingScript, scriptPolicyIdV3)

{-| Environments that have an 'OperatorEnv'
-}
class HasOperatorEnv era e | e -> era where
  operatorEnv :: e -> OperatorEnv era

instance HasOperatorEnv era (OperatorEnv era) where
  operatorEnv = id

{-| Information needed to build transactions
-}
data OperatorEnv era =
  OperatorEnv
    { bteOperator :: Operator Verification
    , bteOperatorUtxos :: UTxO era -- ^ UTxOs owned by the operator, available for spending
    }

{-| Populate the 'OperatorEnv' with UTxOs locked by the verification key
-}
loadEnv :: (MonadUtxoQuery m, C.IsBabbageBasedEra era) => C.VerificationKey C.PaymentKey -> Maybe (C.VerificationKey C.StakeKey) -> m (OperatorEnv era)
loadEnv verificationKey oStakeKey = do
  let bteOperator
        = Operator
            { oPaymentKey = PEVerification verificationKey
            , oStakeKey
            }
  bteOperatorUtxos <- Utxos.toApiUtxo <$> utxosByPaymentCredential (operatorPaymentCredential bteOperator)
  pure OperatorEnv{bteOperator, bteOperatorUtxos}

data BuildTxError era =
  OperatorNoUTxOs -- ^ The operator does not have any UTxOs
  | BalancingError (CoinSelection.BalanceTxError era)
  deriving stock (Show)

{-| Select an output owned by the operator
-}
selectOperatorOutput :: (MonadReader env m, HasOperatorEnv era env, MonadError (BuildTxError era) m) => m (C.TxIn, C.TxOut C.CtxUTxO era)
selectOperatorOutput = asks (listToMaybe . Map.toList . C.unUTxO . bteOperatorUtxos . operatorEnv) >>= \case
  Nothing -> throwError OperatorNoUTxOs
  Just k -> pure k

{-| Balance a transaction using the operator's funds and return output
-}
balanceTxEnv :: forall era env a m. (MonadBlockchain era m, MonadReader env m, HasOperatorEnv era env, MonadError (BuildTxError era) m, C.IsBabbageBasedEra era) => BuildTxT era m a -> m (C.BalancedTxBody era, BalanceChanges)
balanceTxEnv btx = do
  OperatorEnv{bteOperatorUtxos, bteOperator} <- asks operatorEnv
  params <- queryProtocolParameters
  txBuilder <- BuildTx.execBuildTxT $ btx >> BuildTx.setMinAdaDepositAll params
  output <- operatorReturnOutput bteOperator
  mapError BalancingError (CoinSelection.balanceTx mempty output (Utxos.fromApiUtxo bteOperatorUtxos) txBuilder CoinSelection.TrailingChange)

class HasDirectoryEnv e where
  directoryEnv :: e -> DirectoryEnv

instance HasDirectoryEnv DirectoryEnv where
  directoryEnv = id

{-| Scripts relatd to managing the token policy directory.
All of the scripts and their hashes are determined by the 'TxIn'.
-}
data DirectoryEnv =
  DirectoryEnv
    { dsTxIn :: C.TxIn -- ^ The 'txIn' that we spend when deploying the protocol params and directory set
    , dsDirectoryMintingScript        :: PlutusScript PlutusScriptV3
    , dsDirectorySpendingScript       :: PlutusScript PlutusScriptV3
    , dsProtocolParamsMintingScript   :: PlutusScript PlutusScriptV3
    , dsProgrammableLogicBaseScript   :: PlutusScript PlutusScriptV3
    , dsProgrammableLogicGlobalScript :: PlutusScript PlutusScriptV3
    }

mkDirectoryEnv :: C.TxIn -> DirectoryEnv
mkDirectoryEnv dsTxIn =
  let dsDirectoryMintingScript        = directoryNodeMintingScript dsTxIn
      dsProtocolParamsMintingScript   = protocolParamsMintingScript dsTxIn
      dsDirectorySpendingScript       = directoryNodeSpendingScript (protocolParamsPolicyId result)
      dsProgrammableLogicBaseScript   = programmableLogicBaseScript (programmableLogicStakeCredential result) -- Parameterized by the stake cred of the global script
      dsProgrammableLogicGlobalScript = programmableLogicGlobalScript (directoryNodePolicyId result) -- Parameterized by the CS holding protocol params datum
      result = DirectoryEnv
                { dsTxIn
                , dsDirectoryMintingScript
                , dsProtocolParamsMintingScript
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

globalParams :: DirectoryEnv -> ProgrammableLogicGlobalParams
globalParams scripts =
  ProgrammableLogicGlobalParams
    { directoryNodeCS = transPolicyId (directoryNodePolicyId scripts)
    , progLogicCred   = transCredential (programmableLogicBaseCredential scripts) -- its the script hash of the programmable base spending script
    }

data CombinedEnv era =
  CombinedEnv
    { ceOperator :: OperatorEnv era
    , ceDirectory :: DirectoryEnv
    }

instance HasOperatorEnv era (CombinedEnv era) where
  operatorEnv = ceOperator

instance HasDirectoryEnv (CombinedEnv era) where
  directoryEnv = ceDirectory

{-| Add a 'DirectoryEnv' to the environment
-}
withDirectoryFor :: (MonadReader env m, HasOperatorEnv era env) => C.TxIn -> ReaderT (CombinedEnv era) m a -> m a
withDirectoryFor txi action = do
  asks (CombinedEnv . operatorEnv) <*> pure (mkDirectoryEnv txi)
    >>= runReaderT action

