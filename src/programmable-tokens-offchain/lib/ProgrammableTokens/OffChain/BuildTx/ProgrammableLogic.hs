{-# LANGUAGE NamedFieldPuns #-}
module ProgrammableTokens.OffChain.BuildTx.ProgrammableLogic(
  registerProgrammableGlobalScript,
  issueProgrammableToken,
  paySmartTokensToDestination,
  invokeMintingStakeScript,
  registerTransferScripts,
  invokeTransferStakeScript,
  transferProgrammableToken
) where

import Cardano.Api qualified as C
import Control.Monad (unless)
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx (MonadBuildTx, mintPlutus, payToAddress)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadBlockchain, queryNetworkId)
import Convex.PlutusLedger.V1 (transPolicyId, transPubKeyHash, transScriptHash)
import Convex.Utils qualified as Utils
import Data.Foldable (traverse_)
import GHC.Exts (IsList (..))
import PlutusLedgerApi.V3 (CurrencySymbol (..))
import PlutusLedgerApi.V3 qualified as PV3
import ProgrammableTokens.OffChain.BuildTx.Directory (insertDirectoryNode)
import ProgrammableTokens.OffChain.BuildTx.Utils qualified as Utils
import ProgrammableTokens.OffChain.Env (TransferLogicEnv (..))
import ProgrammableTokens.OffChain.Env qualified as Env
import ProgrammableTokens.OffChain.UTxODat (UTxODat (..))
import SmartTokens.Contracts.Issuance (SmartTokenMintingAction (..))
import SmartTokens.Contracts.IssuanceCborHex (IssuanceCborHex)
import SmartTokens.Contracts.ProgrammableLogicBase (ProgrammableLogicGlobalRedeemer (..),
                                                    TokenProof (..))
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams)
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (..))

registerProgrammableGlobalScript :: forall env era m. (MonadReader env m, C.IsBabbageBasedEra era, MonadBuildTx era m, Env.HasDirectoryEnv env) => m ()
registerProgrammableGlobalScript = case C.babbageBasedEra @era of
  C.BabbageEraOnwardsBabbage -> error "babbage era registration not implemented"
  C.BabbageEraOnwardsConway  -> Utils.inConway @era $ do
    programmableGlobalScript <- asks (Env.dsProgrammableLogicGlobalScript . Env.directoryEnv)
    let hshGlobal = C.hashScript $ C.PlutusScript C.plutusScriptVersion programmableGlobalScript
        credGlobal = C.StakeCredentialByScript hshGlobal
    Utils.addConwayStakeCredentialCertificate credGlobal

{- Issue a programmable token and register it in the directory set if necessary. The caller should ensure that the specific
   minting logic stake script witness is included in the final transaction.
  - If the programmable token is not in the directory, then it is registered
  - If the programmable token is in the directory, then it is minted
-}
issueProgrammableToken :: forall era env m. (MonadBlockchain era m, MonadReader env m, Env.HasDirectoryEnv env, Env.HasTransferLogicEnv env, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => UTxODat era ProgrammableLogicGlobalParams -> UTxODat era IssuanceCborHex -> (C.AssetName, C.Quantity) -> UTxODat era DirectorySetNode -> m C.PolicyId
issueProgrammableToken paramsTxOut issuanceCborHexTxOut (an, q) udat@UTxODat{uDatum = dirNodeData} = Utils.inBabbage @era $ do
  inta@TransferLogicEnv{tleMintingScript} <- asks Env.transferLogicEnv
  glParams <- asks (Env.globalParams . Env.directoryEnv)
  dir <- asks Env.directoryEnv

  let mintingLogicHash = C.hashScript $ C.PlutusScript C.plutusScriptVersion tleMintingScript
      mintingLogicCred = SmartTokenMintingAction $ transCredential $ C.PaymentCredentialByScript mintingLogicHash

  -- Debug.Trace.traceM $ "mintingLogicHash: " <> show mintingLogicHash

  -- The global params in the UTxO need to match those in our 'DirectoryEnv'.
  -- If they don't, we get a script error when trying to balance the transaction.
  -- To avoid this we check for equality here and fail early.
  unless (glParams == uDatum paramsTxOut) $
    -- FIXME: Error handling
    error "Global params do not match"

  let mintingScript = Env.programmableTokenMintingScript dir inta
      issuedPolicyId = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV3 mintingScript
      issuedSymbol = transPolicyId issuedPolicyId


  -- Debug.Trace.traceM $ "mintingLogicScript: " <> BSC.unpack (Base16.encode $ C.serialiseToRawBytes mintingScript)
  -- Debug.Trace.traceM $ "issuedCurrencySymbol: " <> show issuedSymbol

  if key dirNodeData == issuedSymbol
    then do
      -- Debug.Trace.traceM "NO insert directory node"
      mintPlutus mintingScript mintingLogicCred an q
    else do
      -- Debug.Trace.traceM "insert directory node"
      mintPlutus mintingScript mintingLogicCred an q
      insertDirectoryNode paramsTxOut issuanceCborHexTxOut udat

  pure issuedPolicyId
    where
      transCredential :: C.PaymentCredential -> PV3.Credential
      transCredential = \case
        C.PaymentCredentialByKey k -> PV3.PubKeyCredential (transPubKeyHash k)
        C.PaymentCredentialByScript k -> PV3.ScriptCredential (transScriptHash k)

{-| Add a smart token output that locks the given value,
addressed to the payment credential
-}
paySmartTokensToDestination :: forall era env m. (MonadBuildTx era m, MonadReader env m, Env.HasDirectoryEnv env, MonadBlockchain era m, C.IsBabbageBasedEra era) => (C.AssetName, C.Quantity) -> C.PolicyId -> C.PaymentCredential -> m ()
paySmartTokensToDestination (an, q) issuedPolicyId destinationCred = Utils.inBabbage @era $ do
  let value = fromList [(C.AssetId issuedPolicyId an, q)]
  addr <- Env.programmableTokenReceivingAddress destinationCred
  payToAddress addr value

{-| Call the stake validator that validates the minting logic
-}
invokeMintingStakeScript :: forall era redeemer env m.
  ( MonadReader env m
  , Env.HasTransferLogicEnv env
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadBuildTx era m
  , MonadBlockchain era m
  , PV3.ToData redeemer
  )
  => redeemer -> m ()
invokeMintingStakeScript redeemer = do
  Env.TransferLogicEnv{Env.tleMintingScript} <- asks Env.transferLogicEnv
  let hsh = C.hashScript (C.PlutusScript C.plutusScriptVersion tleMintingScript)
  BuildTx.addScriptWithdrawal hsh 0 $ BuildTx.buildScriptWitness tleMintingScript C.NoScriptDatumForStake redeemer

{-| Call the stake validator that validates the transfer logic
-}
invokeTransferStakeScript :: forall era redeemer env m.
  ( MonadReader env m
  , Env.HasTransferLogicEnv env
  , C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadBuildTx era m
  , MonadBlockchain era m
  , PV3.ToData redeemer
  )
  => redeemer -> m ()
invokeTransferStakeScript redeemer = do
  Env.TransferLogicEnv{Env.tleTransferScript} <- asks Env.transferLogicEnv
  let hsh = C.hashScript (C.PlutusScript C.plutusScriptVersion tleTransferScript)
  BuildTx.addScriptWithdrawal hsh 0 $ BuildTx.buildScriptWitness tleTransferScript C.NoScriptDatumForStake redeemer


{-| Register the stake scripts for the CIP-143 transfer logic
-}
registerTransferScripts :: forall era env m.
  ( MonadReader env m
  , MonadBuildTx era m
  , Env.HasTransferLogicEnv env
  , C.IsConwayBasedEra era
  )
  => m ()
registerTransferScripts = do
  transferMintingScript <- asks (Env.tleMintingScript . Env.transferLogicEnv)
  transferSpendingScript <- asks (Env.tleTransferScript . Env.transferLogicEnv)
  transferSeizeSpendingScript <- asks (Env.tleIssuerScript . Env.transferLogicEnv)

  let
      hshMinting = C.hashScript $ C.PlutusScript C.plutusScriptVersion transferMintingScript
      credMinting = C.StakeCredentialByScript hshMinting

      hshSpending = C.hashScript $ C.PlutusScript C.plutusScriptVersion transferSpendingScript
      credSpending = C.StakeCredentialByScript hshSpending

      hshSeizeSpending = C.hashScript $ C.PlutusScript C.plutusScriptVersion transferSeizeSpendingScript
      credSeizeSpending = C.StakeCredentialByScript hshSeizeSpending


  Utils.addConwayStakeCredentialCertificate credSpending
  Utils.addConwayStakeCredentialCertificate credMinting
  Utils.addConwayStakeCredentialCertificate credSeizeSpending

{- User facing transfer of programmable tokens from one address to another.
   The caller should ensure that the specific transfer logic stake script
   witness is included in the final transaction.

   NOTE: If the token is not in the directory, then the function will
   use a PDoesNotExist redeemer to prove that the token is not programmable

   IMPORTANT: The caller should ensure that the destination address of the
   programmable token(s) in this transaction all correspond to the same
   programmable logic payment credential otherwise the transaction will fail onchain validation.
-}
transferProgrammableToken :: forall env era m. (MonadReader env m, Env.HasDirectoryEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => UTxODat era ProgrammableLogicGlobalParams ->  [C.TxIn] -> CurrencySymbol -> UTxODat era DirectorySetNode -> m ()
transferProgrammableToken paramsTxIn tokenTxIns programmableTokenSymbol UTxODat{uIn = dirNodeRef, uDatum = dirNodeDat} = Utils.inBabbage @era $ do
  nid <- queryNetworkId

  -- TODO: Check that the directory node is an exact match for our policy

  baseSpendingScript <- asks (Env.dsProgrammableLogicBaseScript . Env.directoryEnv)
  globalStakeScript <- asks (Env.dsProgrammableLogicGlobalScript . Env.directoryEnv)


  let globalStakeCred = C.StakeCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 globalStakeScript

      -- Finds the index of the directory node reference in the transaction ref
      -- inputs
      directoryNodeReferenceIndex txBody =
        fromIntegral @Int @Integer $ BuildTx.findIndexReference dirNodeRef txBody

      -- The redeemer for the global script based on whether a dirctory node
      -- exists with the programmable token symbol
      programmableLogicGlobalRedeemer txBody =
        if key dirNodeDat == programmableTokenSymbol
          -- TODO: extend to allow multiple proofs, onchain allows it
          then TransferAct [TokenExists $ directoryNodeReferenceIndex txBody]
          else TransferAct [TokenDoesNotExist $ directoryNodeReferenceIndex txBody]

      programmableGlobalWitness txBody = BuildTx.buildScriptWitness globalStakeScript C.NoScriptDatumForStake (programmableLogicGlobalRedeemer txBody)

  BuildTx.addReference (uIn paramsTxIn) -- Protocol Params TxIn
  BuildTx.addReference dirNodeRef -- Directory Node TxIn
  traverse_ (\tin -> BuildTx.spendPlutusInlineDatum tin baseSpendingScript ()) tokenTxIns
  BuildTx.addWithdrawalWithTxBody -- Add the global script witness to the transaction
    (C.makeStakeAddress nid globalStakeCred)
    (C.Quantity 0)
    $ C.ScriptWitness C.ScriptWitnessForStakeAddr . programmableGlobalWitness
