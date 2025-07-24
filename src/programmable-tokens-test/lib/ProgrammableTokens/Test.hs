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
  assertBlacklistedAddressException
) where

import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Control.Lens ((%~), (&))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (runReaderT))
import Convex.Class (MonadMockchain, ValidationError, getTxById)
import Convex.MockChain (MockchainT)
import Convex.MockChain.Defaults qualified as Defaults
import Convex.MockChain.Utils (mockchainSucceedsWith)
import Convex.NodeParams (NodeParams, ledgerProtocolParameters,
                          protocolParameters)
import Data.List (isPrefixOf)
import GHC.Exception (SomeException, throw)
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

-- TODO: Need to make this nicer
{-| Make sure that the exception is a failure due to blacklisted address
-}
assertBlacklistedAddressException :: SomeException -> Assertion
assertBlacklistedAddressException ex
  | "user error (ProgTokensError (TransferBlacklistedCredential (PubKeyCredential" `isPrefixOf` show ex = pure ()
  | otherwise = throw ex

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
