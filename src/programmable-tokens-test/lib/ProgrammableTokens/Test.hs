{-# LANGUAGE NamedFieldPuns #-}
-- | Utilities for writing emulator tests for smart tokens
module ProgrammableTokens.Test(
  ScriptTarget(..),
  nodeParamsFor,
  mockchainSucceedsWithTarget,

  -- * Assertions
  expectSingleton,
  expectN,
  expectLeft,
  assertFailingTx,

  -- * Users
  admin,
  user,

  -- * Mockchain actions
  deployDirectorySet
) where

import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Control.Lens ((%~), (&))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), ask)
import Convex.Class (MonadBlockchain, MonadMockchain, MonadUtxoQuery,
                     ValidationError, getTxById, sendTx)
import Convex.CoinSelection (AsBalancingError, AsCoinSelectionError)
import Convex.MockChain (MockchainT)
import Convex.MockChain.Defaults qualified as Defaults
import Convex.MockChain.Utils (mockchainSucceedsWith)
import Convex.NodeParams (NodeParams, ledgerProtocolParameters,
                          protocolParameters)
import Convex.Wallet qualified as Wallet
import Convex.Wallet.MockWallet (w1)
import Convex.Wallet.Operator (Operator (..), PaymentExtendedKey (..), Signing,
                               signTxOperator)
import Data.Functor (void)
import ProgrammableTokens.OffChain.Endpoints qualified as Endpoints
import ProgrammableTokens.OffChain.Env qualified as Env
import ProgrammableTokens.OffChain.Error (AsProgrammableTokensError)
import ProgrammableTokens.OffChain.Query qualified as Query
import SmartTokens.Core.Scripts (ScriptTarget (Debug, Production))
import Test.Tasty.HUnit (Assertion, assertEqual)

expectSingleton :: MonadFail m => String -> [a] -> m a
expectSingleton msg = \case
  [a] -> pure a
  ls  -> fail $ "Expected a single " ++ msg ++ " but found " ++ show (length ls)

expectN :: MonadFail m => Int -> String -> [a] -> m ()
expectN n msg lst
  | length lst == n = pure ()
  | otherwise       = fail $ "Expected " ++ show n ++ " " ++ msg ++ " but found " ++ show (length lst)

expectLeft :: (MonadFail m, Show b) => String -> Either a b -> m ()
expectLeft msg = \case
  Left _ -> pure ()
  (Right r) -> fail $ "Expected " ++ msg ++ " but found Right " ++ show r

{-| Assert that the transaction exists on the mockchain and that its script validity flag
is set to 'C.ScriptInvalid'
-}
assertFailingTx :: (MonadMockchain era m, C.IsAlonzoBasedEra era, MonadFail m, MonadIO m) => Either (ValidationError era) C.TxId -> m ()
assertFailingTx = \case
  Left err  -> fail $ "Expected TxId, got: " <> show err
  Right txId -> do
    C.TxBody C.TxBodyContent{C.txScriptValidity} <- getTxById txId >>= maybe (fail $ "Tx not found: " <> show txId) (pure . C.getTxBody)
    liftIO (assertEqual "Tx validity" (C.TxScriptValidity C.alonzoBasedEra C.ScriptInvalid) txScriptValidity)

nodeParamsFor :: ScriptTarget -> NodeParams C.ConwayEra
nodeParamsFor = \case
  -- Run the 'Mockchain' action with modified node parameters to allow larger-than-usual
  -- transactions. This is useful for showing debug output from the scripts and fail if there is an error
  Debug ->
    let tenX ExUnits{exUnitsSteps=steps, exUnitsMem=mem} =
          ExUnits{exUnitsSteps = 10 * steps, exUnitsMem = 10 * mem}
    in Defaults.nodeParams
        & ledgerProtocolParameters . protocolParameters . Ledger.ppMaxTxSizeL %~ (*10)
        & ledgerProtocolParameters . protocolParameters . Ledger.ppMaxTxExUnitsL %~ tenX
  Production -> Defaults.nodeParams

mockchainSucceedsWithTarget :: ScriptTarget -> ReaderT ScriptTarget (MockchainT C.ConwayEra IO) a -> Assertion
mockchainSucceedsWithTarget target =
  mockchainSucceedsWith (nodeParamsFor target) . flip runReaderT target

{-| Key used for actions of the token issuer / operator.
-}
admin :: Operator Signing
admin =
  Operator
    { oPaymentKey = PESigning (Wallet.getWallet w1)
    , oStakeKey   = Nothing
    }

{-| Token user
-}
user :: Wallet.Wallet -> Operator Signing
user w =
  Operator
    { oPaymentKey = PESigning (Wallet.getWallet w)
    , oStakeKey = Nothing
    }

{-| Deploy the CIP 143 directory. This issues a total of 3 transactions.
-}
deployDirectorySet :: forall era err m.
  ( MonadReader ScriptTarget m
  , MonadUtxoQuery m
  , MonadBlockchain era m
  , MonadError err m
  , AsCoinSelectionError err
  , AsBalancingError err era
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , AsProgrammableTokensError err
  )
  => Operator Signing
  -> m Env.DirectoryScriptRoot
deployDirectorySet op = do
  target <- ask
  operatorEnv <- Env.loadConvexOperatorEnv op
  flip runReaderT operatorEnv $ do
    Endpoints.frackUtxosTx
      >>= void . sendTx . signTxOperator op

  operatorEnv <- Env.loadConvexOperatorEnv op
  dirScriptRoot <- flip runReaderT operatorEnv $ do
    (tx, scriptRoot) <- Endpoints.deployCip143RegistryTx target
    void $ sendTx $ signTxOperator op tx
    pure scriptRoot

  operatorEnv <- Env.loadConvexOperatorEnv op
  flip runReaderT operatorEnv $ do
    Env.withEnv (Env.directoryOperatorEnv (Env.mkDirectoryEnv dirScriptRoot) operatorEnv) $ do
      void $ Query.globalParamsNode @C.ConwayEra

      Endpoints.deployIssuanceCborHex
        >>= void . sendTx . signTxOperator admin
      void $ Query.issuanceCborHexUTxO @C.ConwayEra
  pure dirScriptRoot
