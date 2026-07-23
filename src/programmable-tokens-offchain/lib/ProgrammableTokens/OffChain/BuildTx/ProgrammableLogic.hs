{-# LANGUAGE NamedFieldPuns #-}

module ProgrammableTokens.OffChain.BuildTx.ProgrammableLogic (
    registerProgrammableGlobalScript,
    registerProgrammableSeizeScript,
    issueProgrammableToken,
    paySmartTokensToDestination,
    invokeMintingStakeScript,
    registerTransferScripts,
    invokeTransferStakeScript,
    transferProgrammableToken,
) where

import Cardano.Api qualified as C
import Control.Lens (over, view, (^.), _1, _2)
import Control.Monad (unless)
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx (MonadBuildTx, TxBuilder (..), addMintWithTxBody, buildScriptWitness, mintPlutus, payToAddress, spendPlutusRefWithInlineDatum)
import Convex.BuildTx qualified as BuildTx
import Convex.CardanoApi.Lenses qualified as L
import Convex.Class (MonadBlockchain, queryNetworkId)
import Convex.PlutusLedger.V1 (transPolicyId, transPubKeyHash, transScriptHash, unTransAssetName)
import Convex.Utils qualified as Utils
import Data.Foldable (traverse_)
import Data.List (findIndex, nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import GHC.Exts (IsList (..))
import PlutusLedgerApi.V3 (CurrencySymbol (..))
import PlutusLedgerApi.V3 qualified as PV3
import ProgrammableTokens.OffChain.BuildTx.Directory (insertDirectoryNode)
import ProgrammableTokens.OffChain.BuildTx.Utils qualified as Utils
import ProgrammableTokens.OffChain.Env (TransferLogicEnv (..))
import ProgrammableTokens.OffChain.Env qualified as Env
import ProgrammableTokens.OffChain.UTxODat (UTxODat (..))
import SmartTokens.Contracts.Issuance (MintRedeemer (..), RegistrationWitness (..))
import SmartTokens.Contracts.IssuanceCborHex (IssuanceCborHex)
import SmartTokens.Contracts.ProgrammableLogicBase (
    MintProof (..),
    ProgrammableLogicGlobalRedeemer (..),
 )
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (..))
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams)

registerProgrammableGlobalScript :: forall env era m. (MonadReader env m, C.IsBabbageBasedEra era, MonadBuildTx era m, Env.HasDirectoryEnv env) => m ()
registerProgrammableGlobalScript = case C.babbageBasedEra @era of
    C.BabbageEraOnwardsBabbage -> error "babbage era registration not implemented"
    C.BabbageEraOnwardsConway -> Utils.inConway @era $ do
        programmableGlobalScript <- asks (Env.dsProgrammableLogicGlobalScript . Env.directoryEnv)
        let hshGlobal = C.hashScript $ C.PlutusScript C.plutusScriptVersion programmableGlobalScript
            credGlobal = C.StakeCredentialByScript hshGlobal
        Utils.addConwayStakeCredentialCertificate credGlobal

-- | Register the standalone seize validator's stake credential.
--
-- 'mkProgrammableSeize' runs as a withdraw-zero (rewarding) script, exactly like
-- the global validator, so its stake credential must be a registered reward
-- account before any 'SeizeAct' withdrawal can be included in a transaction
-- (otherwise the ledger rejects it with 'WithdrawalsNotInRewardsCERTS'). This is
-- the deployment-side counterpart of splitting the seize logic out of the global
-- validator into its own script: the split introduced a second rewarding
-- credential that must be registered alongside the global one.
registerProgrammableSeizeScript :: forall env era m. (MonadReader env m, C.IsBabbageBasedEra era, MonadBuildTx era m, Env.HasDirectoryEnv env) => m ()
registerProgrammableSeizeScript = case C.babbageBasedEra @era of
    C.BabbageEraOnwardsBabbage -> error "babbage era registration not implemented"
    C.BabbageEraOnwardsConway -> Utils.inConway @era $ do
        programmableSeizeScript' <- asks (Env.dsProgrammableSeizeScript . Env.directoryEnv)
        let hshSeize = C.hashScript $ C.PlutusScript C.plutusScriptVersion programmableSeizeScript'
            credSeize = C.StakeCredentialByScript hshSeize
        Utils.addConwayStakeCredentialCertificate credSeize

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
    netId <- queryNetworkId

    let mintingLogicHash = C.hashScript $ C.PlutusScript C.plutusScriptVersion tleMintingScript
        -- The token's minting-logic authorization runs as a withdraw-zero script;
        -- its stake address is what the issuance policy looks for at
        -- 'mrMintingLogicWdrlIdx'.
        mintingLogicStakeAddr = C.makeStakeAddress netId (C.StakeCredentialByScript mintingLogicHash)

    -- The global params in the UTxO need to match those in our 'DirectoryEnv'.
    -- If they don't, we get a script error when trying to balance the transaction.
    -- To avoid this we check for equality here and fail early.
    unless (glParams == uDatum paramsTxOut) $
        -- FIXME: Error handling
        error "Global params do not match"

    let mintingScript = Env.programmableTokenMintingScript dir inta
        issuedPolicyId = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV3 mintingScript
        issuedSymbol = transPolicyId issuedPolicyId
        -- The directory NFT for this policy is named after the policy id, under
        -- the directory minting policy.
        nodeAssetId =
            C.AssetId
                (Env.directoryNodePolicyId dir)
                (unTransAssetName (PV3.TokenName (unCurrencySymbol issuedSymbol)))

        -- Indices shared by both Local sub-cases, computed against the balanced
        -- transaction: the minting-logic withdrawal and the protocol-params
        -- reference input.
        mkLocal registration txBody =
            Local
                (fromIntegral (BuildTx.findIndexWithdrawal mintingLogicStakeAddr txBody))
                (fromIntegral (BuildTx.findIndexReference (uIn paramsTxOut) txBody))
                registration

    -- Mint the token with a redeemer computed against the FINAL transaction body
    -- (addMintWithTxBody, not the WithRedeemerFn variant, so that later-added
    -- withdrawals/outputs are visible when the indices are resolved).
    let mintWith registrationOf =
            addMintWithTxBody
                issuedPolicyId
                an
                q
                (\txBody -> buildScriptWitness mintingScript C.NoScriptDatumForMint (mkLocal (registrationOf txBody) txBody))

    if key dirNodeData == issuedSymbol
        then do
            -- Already registered: reference the existing directory node and prove
            -- registration by that reference input (Local + RegisteredByReferenceInput).
            -- The Local arm reads its base/directory credentials from the
            -- protocol-params reference input, so reference it here (the
            -- register-and-mint branch gets it from 'insertDirectoryNode').
            BuildTx.addReference (uIn paramsTxOut)
            mintWith (\txBody -> RegisteredByReferenceInput (fromIntegral (BuildTx.findIndexReference (uIn udat) txBody)))
            BuildTx.addReference (uIn udat)
        else do
            -- Register-and-mint in one tx: insertDirectoryNode produces the new
            -- node as an OUTPUT (and references the params UTxO); prove registration
            -- by that output (Local + RegisteredByOutput). The output is located by
            -- content — the directory NFT — because its position depends on the tx.
            mintWith (\txBody -> RegisteredByOutput (findNodeOutputIndex nodeAssetId txBody))
            insertDirectoryNode paramsTxOut issuanceCborHexTxOut udat

    pure issuedPolicyId

-- | Index of the transaction output that holds one unit of @assetId@ (the
-- directory node NFT), located by content because its position depends on the
-- full transaction layout.
findNodeOutputIndex :: (C.IsMaryBasedEra era) => C.AssetId -> C.TxBodyContent C.BuildTx era -> Integer
findNodeOutputIndex assetId txBody =
    fromIntegral $
        fromMaybe (error "issueProgrammableToken: directory node output not found") $
            findIndex
                ( \txOut ->
                    C.selectAsset (view (L._TxOut . _2 . L._TxOutValue) txOut) assetId == 1
                )
                (txBody ^. L.txOuts)

{- | Add a smart token output that locks the given value,
addressed to the payment credential
-}
paySmartTokensToDestination :: forall era env m. (MonadBuildTx era m, MonadReader env m, Env.HasDirectoryEnv env, MonadBlockchain era m, C.IsBabbageBasedEra era) => (C.AssetName, C.Quantity) -> C.PolicyId -> C.PaymentCredential -> m ()
paySmartTokensToDestination (an, q) issuedPolicyId destinationCred = Utils.inBabbage @era $ do
    let value = fromList [(C.AssetId issuedPolicyId an, q)]
    addr <- Env.programmableTokenReceivingAddress destinationCred
    payToAddress addr value

-- | Call the stake validator that validates the minting logic
invokeMintingStakeScript ::
    forall era redeemer env m.
    ( MonadReader env m
    , Env.HasTransferLogicEnv env
    , C.IsBabbageBasedEra era
    , C.HasScriptLanguageInEra C.PlutusScriptV3 era
    , MonadBuildTx era m
    , MonadBlockchain era m
    , PV3.ToData redeemer
    ) =>
    redeemer -> m ()
invokeMintingStakeScript redeemer = do
    Env.TransferLogicEnv{Env.tleMintingScript} <- asks Env.transferLogicEnv
    let hsh = C.hashScript (C.PlutusScript C.plutusScriptVersion tleMintingScript)
    BuildTx.addScriptWithdrawal hsh 0 $ BuildTx.buildScriptWitness tleMintingScript C.NoScriptDatumForStake redeemer

-- | Call the stake validator that validates the transfer logic
invokeTransferStakeScript ::
    forall era redeemer env m.
    ( MonadReader env m
    , Env.HasTransferLogicEnv env
    , C.IsBabbageBasedEra era
    , C.HasScriptLanguageInEra C.PlutusScriptV3 era
    , MonadBuildTx era m
    , MonadBlockchain era m
    , PV3.ToData redeemer
    ) =>
    redeemer -> m ()
invokeTransferStakeScript redeemer = do
    Env.TransferLogicEnv{Env.tleTransferScript} <- asks Env.transferLogicEnv
    let hsh = C.hashScript (C.PlutusScript C.plutusScriptVersion tleTransferScript)
    BuildTx.addScriptWithdrawal hsh 0 $ BuildTx.buildScriptWitness tleTransferScript C.NoScriptDatumForStake redeemer

-- | Register the stake scripts for the CIP-143 transfer logic
registerTransferScripts ::
    forall era env m.
    ( MonadReader env m
    , MonadBuildTx era m
    , Env.HasTransferLogicEnv env
    , C.IsConwayBasedEra era
    ) =>
    m ()
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
   reference the covering directory node whose key/next range proves that the token is not programmable

   IMPORTANT: The caller should ensure that the destination address of the
   programmable token(s) in this transaction all correspond to the same
   programmable logic payment credential otherwise the transaction will fail onchain validation.
-}
transferProgrammableToken :: forall env era m. (MonadReader env m, Env.HasDirectoryEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => UTxODat era ProgrammableLogicGlobalParams -> [C.TxIn] -> CurrencySymbol -> [UTxODat era DirectorySetNode] -> m ()
transferProgrammableToken paramsTxIn tokenTxIns programmableTokenSymbol directoryNodes = Utils.inBabbage @era $ do
    nid <- queryNetworkId

    baseSpendingScript <- asks (Env.dsProgrammableLogicBaseScript . Env.directoryEnv)
    globalStakeScript <- asks (Env.dsProgrammableLogicGlobalScript . Env.directoryEnv)
    baseRefTxIn <- asks (Env.srProgrammableLogicBaseRefTxIn . Env.dsScriptRoot . Env.directoryEnv)
    globalRefTxIn <- asks (Env.srProgrammableLogicGlobalRefTxIn . Env.dsScriptRoot . Env.directoryEnv)

    let sortedDirectoryNodes = sortOn (Down . key . uDatum) directoryNodes

        globalStakeCred = C.StakeCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 globalStakeScript

        transferProofs txBody =
            [proofNodeIndex txBody (proofNodeForSymbol sortedDirectoryNodes programmableTokenSymbol)]

        -- Classify each minted currency symbol against the directory (spec §11.3):
        -- the covering node found by 'proofNodeForSymbol' is a Member when its key
        -- equals the symbol (registered — no index needed), otherwise a NonMember
        -- whose covering-node reference index the mint walk authenticates.
        mintProofFor txBody cs =
            let node = proofNodeForSymbol sortedDirectoryNodes cs
             in if key (uDatum node) == cs
                    then Member
                    else NonMember (proofNodeIndex txBody node)

        mintProofs txBody = map (mintProofFor txBody) (mintedCurrencySymbols txBody)

        -- Only NonMember proofs need their covering node referenced; Member proofs
        -- touch no reference input.
        mintProofReferences txBody =
            [ uIn node
            | cs <- mintedCurrencySymbols txBody
            , let node = proofNodeForSymbol sortedDirectoryNodes cs
            , key (uDatum node) /= cs
            ]

        transferProofReferences _ = map (uIn . proofNodeForSymbol sortedDirectoryNodes) [programmableTokenSymbol]

        programmableLogicGlobalRedeemer txBody =
            TransferAct
                { plgrTransferProofs = transferProofs txBody
                , plgrMintProofs = mintProofs txBody
                , plgrParamsRefIdx = fromIntegral (BuildTx.findIndexReference (uIn paramsTxIn) txBody)
                }

        programmableGlobalWitness txBody =
            case globalRefTxIn of
                Just globalRef ->
                    BuildTx.buildRefScriptWitness globalRef C.PlutusScriptV3 C.NoScriptDatumForStake (programmableLogicGlobalRedeemer txBody)
                Nothing ->
                    BuildTx.buildScriptWitness globalStakeScript C.NoScriptDatumForStake (programmableLogicGlobalRedeemer txBody)

    BuildTx.addReference (uIn paramsTxIn) -- Protocol Params TxIn
    addReferencesWithTxBody transferProofReferences
    addReferencesWithTxBody mintProofReferences
    case baseRefTxIn of
        Just baseRef -> do
            traverse_ (\tin -> spendPlutusRefWithInlineDatum tin baseRef C.PlutusScriptV3 ()) tokenTxIns
            BuildTx.addTxBuilder (TxBuilder $ \_ -> over (L.txInsReference . L._TxInsReferenceIso . _1) nub)
        Nothing ->
            traverse_ (\tin -> BuildTx.spendPlutusInlineDatum tin baseSpendingScript ()) tokenTxIns
    traverse_ BuildTx.addReference globalRefTxIn
    BuildTx.addWithdrawalWithTxBody -- Add the global script witness to the transaction
        (C.makeStakeAddress nid globalStakeCred)
        (C.Quantity 0)
        $ C.ScriptWitness C.ScriptWitnessForStakeAddr . programmableGlobalWitness

proofNodeForSymbol :: [UTxODat era DirectorySetNode] -> CurrencySymbol -> UTxODat era DirectorySetNode
proofNodeForSymbol directoryNodes targetSymbol =
    case filter ((<= targetSymbol) . key . uDatum) directoryNodes of
        [] ->
            error $ "Missing directory coverage for mint policy: " <> show targetSymbol
        node : _ ->
            node

proofNodeIndex :: (C.IsBabbageBasedEra era) => C.TxBodyContent C.BuildTx era -> UTxODat era DirectorySetNode -> Integer
proofNodeIndex txBody UTxODat{uIn} =
    toRefIndex uIn
  where
    toRefIndex txInRef = fromIntegral @Int @Integer $ BuildTx.findIndexReference txInRef txBody

mintedCurrencySymbols :: C.TxBodyContent C.BuildTx era -> [CurrencySymbol]
mintedCurrencySymbols txBody =
    case txBody ^. L.txMintValue of
        C.TxMintNone -> []
        C.TxMintValue _ mintValueMap ->
            map transPolicyId (Map.keys mintValueMap)

addReferencesWithTxBody :: (MonadBuildTx era m, C.IsBabbageBasedEra era) => (C.TxBodyContent C.BuildTx era -> [C.TxIn]) -> m ()
addReferencesWithTxBody f =
    BuildTx.addTxBuilder (TxBuilder $ \body -> over (L.txInsReference . L._TxInsReferenceIso . _1) (nub . (f body <>)))
