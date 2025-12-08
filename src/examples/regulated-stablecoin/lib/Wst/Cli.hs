{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Move brackets to avoid $" #-}
module Wst.Cli (runMain) where

import Blammo.Logging.Simple (
    HasLogger,
    Message ((:#)),
    MonadLogger,
    logDebug,
    logError,
    logInfo,
    runLoggerLoggingT,
    (.=),
 )
import Cardano.Api qualified as C
import Control.Monad (when)
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ask, asks)
import Convex.Class (MonadBlockchain (sendTx), MonadUtxoQuery)
import Convex.Wallet.Operator (
    Operator (Operator, oPaymentKey),
    OperatorConfigSigning,
    PaymentExtendedKey (PESigningEx),
    signTxOperator,
    verificationKey,
 )
import Convex.Wallet.Operator qualified as Operator
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Options.Applicative (
    customExecParser,
    disambiguate,
    helper,
    idm,
    info,
    prefs,
    showHelpOnEmpty,
    showHelpOnError,
 )
import ProgrammableTokens.OffChain.Endpoints qualified as Endpoints
import ProgrammableTokens.OffChain.Env.Operator qualified as Env
import ProgrammableTokens.OffChain.Env.Runtime qualified as Env
import ProgrammableTokens.OffChain.Env.Utils qualified as Utils
import SmartTokens.Core.Scripts (ScriptTarget (Production))
import Wst.App (runWstApp)
import Wst.AppError (AppError)
import Wst.Cli.Command (
    Command (..),
    ManageCommand (StartServer, Status),
    parseCommand,
 )
import Wst.Offchain.Endpoints.Deployment (deployBlacklistTx)
import Wst.Offchain.Env qualified as Env
import Wst.Server (ServerArgs (..))
import Wst.Server qualified as Server
import Wst.Server.PolicyIssuerStore (
    HasPolicyIssuerStore,
    addPolicyIssuerStore,
    withPolicyIssuerStore,
 )

runMain :: IO ()
runMain = do
    customExecParser
        (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
        (info (helper <*> parseCommand) idm)
        >>= runCommand

runCommand :: Command -> IO ()
runCommand com = do
    env <- Env.addRuntimeEnv <$> Env.loadRuntimeEnv <*> pure Utils.empty
    result <- case com of
        Deploy config -> runWstApp env (deploy config)
        DeployIssuanceCborHex config txIn issuanceCborHexTxIn -> runWstApp env (deployIssuanceCborHex config txIn issuanceCborHexTxIn)
        RegisterPolicyStakeScripts config txIn issuanceCborHexTxIn -> runWstApp env (registerPolicyStakeScripts config txIn issuanceCborHexTxIn)
        BlacklistInit config txIn issuanceCborHexTxIn -> runWstApp env (deployBlacklist config txIn issuanceCborHexTxIn)
        Manage txIn issuanceCborHexTxIn com_ -> do
            let env' = Env.addDirectoryEnvFor (Env.DirectoryScriptRoot txIn issuanceCborHexTxIn Production) env
            case com_ of
                Status ->
                    runWstApp env' $ do
                        -- TODO: status check (call the query endpoints and print out a summary of the results)
                        logInfo "Manage"
                StartServer options -> do
                    serverArgs <-
                        Server.staticFilesFromEnv options
                            >>= Server.demoFileFromEnv
                            >>= Server.policyIssuerStoreFromEnv
                    withPolicyIssuerStore (saPolicyIssuerStore serverArgs) $ \store -> do
                        let envWithStore = addPolicyIssuerStore store env'
                        runWstApp envWithStore (startServer serverArgs)
    case result of
        Left err -> runLoggerLoggingT env $ logError (fromString $ show err)
        Right a -> pure a

deploy :: forall env m. (MonadLogger m, MonadIO m, MonadUtxoQuery m, MonadReader env m, Env.HasRuntimeEnv env, MonadError (AppError C.ConwayEra) m, MonadBlockchain C.ConwayEra m) => OperatorConfigSigning -> m ()
deploy config = do
    logInfo $ "Loading operator files" :# ["key_file" .= Operator.ocSigningKeyFile config]

    -- FIXME: why does this throw?
    -- _operator <- liftIO (Operator.loadOperatorFiles config)

    signingKey <-
        liftIO $
            C.readFileTextEnvelope (C.File $ Operator.ocSigningKeyFile config)
                >>= either (error . show) pure

    let operator = Operator (PESigningEx signingKey) Nothing
        operatorPaymentHash = C.verificationKeyHash . verificationKey . oPaymentKey $ operator

    opEnv <- Env.loadOperatorEnv @_ @C.ConwayEra operatorPaymentHash C.NoStakeAddress
    runEnv <- asks Env.runtimeEnv

    let env = Env.addOperatorEnv opEnv $ Utils.singleton runEnv

    utxos <- liftIO (runWstApp env (Env.selectOperatorUTxOs @(Utils.HSet '[Env.OperatorEnv C.ConwayEra, Env.RuntimeEnv]) @_ @C.ConwayEra)) >>= liftEither
    when (length utxos < 2) $ do
        frackTx <- liftIO (runWstApp env Endpoints.frackUtxosTx) >>= liftEither
        logInfo "Created frack Tx"
        let signedFrackTx = signTxOperator operator frackTx
        logInfo $ "Signed frack Tx" :# ["fracktx" .= show frackTx]
        sendTx signedFrackTx >>= \case
            Left err -> logError $ "Error sending frack Tx" :# ["err" .= show err]
            Right txid -> logInfo $ "Frack tx submitted successfully" :# ["txid" .= show txid]

    -- reload the operator env
    refreshedOpEnv <- Env.reloadOperatorEnv @_ @C.ConwayEra opEnv
    let env' = Env.replaceOperatorEnv refreshedOpEnv env

    -- Use blockfrost backend to run Wst.Offchain.Endpoints.Deployment with the operator's funds
    (tx, root) <-
        liftIO
            ( runWstApp env' $ do
                Endpoints.deployCip143RegistryTx Production
            )
            >>= liftEither

    logInfo $ "Created deployment Tx" :# ["root" .= root]

    -- Then use operator key to sign
    let signedTx = signTxOperator operator tx
    logDebug $ "Signed Deployment Tx" :# ["tx" .= show tx]

    -- Then submit transaction to blockfrost
    sendTx signedTx >>= \case
        Left err -> logError $ "Error sending Tx" :# ["err" .= show err]
        Right txid -> do
            logInfo $ "Tx submitted successfully" :# ["txid" .= show txid]
            (liftIO $ C.writeFileJSON "deployment-root.json" root) >>= either (error . show) pure

deployIssuanceCborHex :: forall env m. (MonadLogger m, MonadIO m, MonadUtxoQuery m, MonadReader env m, Env.HasRuntimeEnv env, MonadError (AppError C.ConwayEra) m, MonadBlockchain C.ConwayEra m) => OperatorConfigSigning -> C.TxIn -> C.TxIn -> m ()
deployIssuanceCborHex config txin issuanceCborHexTxIn = do
    signingKey <-
        liftIO $
            C.readFileTextEnvelope (C.File $ Operator.ocSigningKeyFile config)
                >>= either (error . show) pure
    let operator = Operator (PESigningEx signingKey) Nothing
        operatorPaymentHash = C.verificationKeyHash . verificationKey . oPaymentKey $ operator
    opEnv <- Env.loadOperatorEnv @_ @C.ConwayEra operatorPaymentHash C.NoStakeAddress
    runEnv <- asks Env.runtimeEnv

    let env = Env.addDirectoryEnvFor (Env.DirectoryScriptRoot txin issuanceCborHexTxIn Production) $ Env.addOperatorEnv opEnv $ Utils.singleton runEnv
    issuanceCborHexTx <- liftIO (runWstApp env Endpoints.deployIssuanceCborHex) >>= liftEither
    let signedIssuanceCborHexTx = signTxOperator operator issuanceCborHexTx
    sendTx signedIssuanceCborHexTx >>= \case
        Left err -> logError $ "Error sending issuance cbor hex Tx" :# ["err" .= show err]
        Right issuanceCborHexTxid ->
            logInfo $ "Issuance cbor hex tx submitted successfully" :# ["txid" .= show issuanceCborHexTxid]

deployBlacklist :: forall env m. (MonadLogger m, MonadIO m, MonadUtxoQuery m, MonadReader env m, Env.HasRuntimeEnv env, MonadError (AppError C.ConwayEra) m, MonadBlockchain C.ConwayEra m) => OperatorConfigSigning -> C.TxIn -> C.TxIn -> m ()
deployBlacklist config txin issuanceCborHexTxIn = do
    signingKey <-
        liftIO $
            C.readFileTextEnvelope (C.File $ Operator.ocSigningKeyFile config)
                >>= either (error . show) pure
    let operator = Operator (PESigningEx signingKey) Nothing
        operatorPaymentHash = C.verificationKeyHash . verificationKey . oPaymentKey $ operator
    opEnv <- Env.loadOperatorEnv @_ @C.ConwayEra operatorPaymentHash C.NoStakeAddress
    runEnv <- asks Env.runtimeEnv
    let env = Env.addDirectoryEnvFor (Env.DirectoryScriptRoot txin issuanceCborHexTxIn Production) $ Env.addOperatorEnv opEnv $ Utils.singleton runEnv
    (transferEnv, _) <- liftIO (runWstApp env $ Env.transferLogicForDirectory operatorPaymentHash Nothing) >>= liftEither
    let env' = Env.addTransferEnv transferEnv env
    policyTx <- liftIO (runWstApp env' deployBlacklistTx) >>= liftEither
    let signedPolicyTx = signTxOperator operator policyTx
    sendTx signedPolicyTx >>= \case
        Left err -> logError $ "Error sending blacklist Tx" :# ["err" .= show err]
        Right txid -> logInfo $ "Blacklist tx submitted successfully" :# ["txid" .= show txid]

registerPolicyStakeScripts :: forall env m. (MonadLogger m, MonadIO m, MonadUtxoQuery m, MonadReader env m, Env.HasRuntimeEnv env, MonadError (AppError C.ConwayEra) m, MonadBlockchain C.ConwayEra m) => OperatorConfigSigning -> C.TxIn -> C.TxIn -> m ()
registerPolicyStakeScripts config txin issuanceCborHexTxIn = do
    signingKey <-
        liftIO $
            C.readFileTextEnvelope (C.File $ Operator.ocSigningKeyFile config)
                >>= either (error . show) pure
    let operator = Operator (PESigningEx signingKey) Nothing
        operatorPaymentHash = C.verificationKeyHash . verificationKey . oPaymentKey $ operator
    opEnv <- Env.loadOperatorEnv @_ @C.ConwayEra operatorPaymentHash C.NoStakeAddress
    runEnv <- asks Env.runtimeEnv
    let env = Env.addDirectoryEnvFor (Env.DirectoryScriptRoot txin issuanceCborHexTxIn Production) $ Env.addOperatorEnv opEnv $ Utils.singleton runEnv
    (transferEnv, _) <- liftIO (runWstApp env $ Env.transferLogicForDirectory operatorPaymentHash Nothing) >>= liftEither
    let env' = Env.addTransferEnv transferEnv env
    policyTx <- liftIO (runWstApp env' Endpoints.registerCip143PolicyTransferScripts) >>= liftEither
    let signedPolicyTx = signTxOperator operator policyTx
    sendTx signedPolicyTx >>= \case
        Left err -> logError $ "Error sending policy stake scripts Tx" :# ["err" .= show err]
        Right txid -> logInfo $ "Policy stake scripts tx submitted successfully" :# ["txid" .= show txid]

startServer ::
    ( MonadReader env m
    , Env.HasRuntimeEnv env
    , Env.HasDirectoryEnv env
    , HasLogger env
    , HasPolicyIssuerStore env
    , MonadIO m
    , MonadLogger m
    ) =>
    Server.ServerArgs -> m ()
startServer serverArgs@ServerArgs{saPort, saStaticFiles, saPolicyIssuerStore} = do
    logInfo $ "starting server" :# ["port" .= saPort, "static_files" .= fromMaybe "(no static files)" saStaticFiles, "policy_issuer_store" .= saPolicyIssuerStore]
    env <- ask
    liftIO (Server.runServer env serverArgs)
