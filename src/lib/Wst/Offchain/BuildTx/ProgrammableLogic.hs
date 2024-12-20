{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ViewPatterns     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use second" #-}
module Wst.Offchain.BuildTx.ProgrammableLogic
  (
    IssueNewTokenArgs (..),
    issueProgrammableToken,
    transferProgrammableToken,
    seizeProgrammableToken,
  )
where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Lens (over, (^.))
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Convex.BuildTx (MonadBuildTx, addBtx, addReference,
                       addWithdrawalWithTxBody, buildScriptWitness,
                       findIndexReference, findIndexSpending, mintPlutus,
                       spendPlutusInlineDatum)
import Convex.CardanoApi.Lenses as L
import Convex.Class (MonadBlockchain (queryNetworkId))
import Convex.PlutusLedger.V1 (transPolicyId, unTransCredential,
                               unTransPolicyId)
import Convex.Scripts (fromHashableScriptData)
import Convex.Utils qualified as Utils
import Data.Foldable (find, maximumBy, traverse_)
import Data.Function (on)
import Data.Maybe (fromJust)
import PlutusLedgerApi.V3 (CurrencySymbol (..))
import SmartTokens.Contracts.Issuance (SmartTokenMintingAction (MintPToken, RegisterPToken))
import SmartTokens.Contracts.ProgrammableLogicBase (ProgrammableLogicGlobalRedeemer (..),
                                                    TokenProof (..))
import SmartTokens.Types.ProtocolParams
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (..))
import Wst.Offchain.BuildTx.DirectorySet (InsertNodeArgs (..),
                                          insertDirectoryNode)
import Wst.Offchain.BuildTx.ProtocolParams (getProtocolParamsGlobalInline)
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query (UTxODat (..))
import Wst.Offchain.Query qualified as Query
import Wst.Offchain.Scripts (programmableLogicBaseScript,
                             programmableLogicGlobalScript,
                             programmableLogicMintingScript)


data IssueNewTokenArgs = IssueNewTokenArgs
  { intaMintingLogic :: C.StakeCredential,
    intaTransferLogic :: C.StakeCredential,
    intaIssuerLogic :: C.StakeCredential
  }

{- Issue a programmable token and register it in the directory set if necessary. The caller should ensure that the specific
   minting logic stake script witness is included in the final transaction.
  - If the programmable token is not in the directory, then it is registered
  - If the programmable token is in the directory, then it is minted
-}
issueProgrammableToken :: forall env era m. (MonadReader env m, Env.HasDirectoryEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => UTxODat era ProgrammableLogicGlobalParams -> (C.AssetName, C.Quantity) -> IssueNewTokenArgs -> [UTxODat era DirectorySetNode] -> m C.PolicyId
issueProgrammableToken paramsTxOut (an, q) IssueNewTokenArgs{intaMintingLogic, intaTransferLogic, intaIssuerLogic} directoryList = Utils.inBabbage @era $ do
  directoryInitialTxIn <- asks (Env.dsTxIn . Env.directoryEnv)

  let ProgrammableLogicGlobalParams {directoryNodeCS, progLogicCred} = uDatum paramsTxOut

  progLogicScriptCredential <- either (const $ error "could not parse protocol params") pure $ unTransCredential progLogicCred
  directoryNodeSymbol <- either (const $ error "could not parse protocol params") pure $ unTransPolicyId directoryNodeCS

  -- TODO: maybe move programmableLogicMintingScript to DirectoryEnv
  let mintingScript = programmableLogicMintingScript progLogicScriptCredential intaMintingLogic directoryNodeSymbol
      issuedPolicyId = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV3 mintingScript
      issuedSymbol = transPolicyId issuedPolicyId

      udat@UTxODat{uDatum = dirNodeData} =
        maximumBy (compare `on` (key . uDatum)) $
          filter ((<= issuedSymbol) . key . uDatum) directoryList

  if key dirNodeData == issuedSymbol
    then
      mintPlutus mintingScript MintPToken an q
    else do
      let nodeArgs  = InsertNodeArgs{inaNewKey = issuedSymbol, inaTransferLogic = intaTransferLogic, inaIssuerLogic = intaIssuerLogic}
      mintPlutus mintingScript RegisterPToken an q
        -- TODO: propagate the HasEnv constraint upwards
        >> runReaderT (insertDirectoryNode paramsTxOut udat nodeArgs) (Env.mkDirectoryEnv directoryInitialTxIn)

  pure issuedPolicyId

{- User facing transfer of programmable tokens from one address to another.
   The caller should ensure that the specific transfer logic stake script
   witness is included in the final transaction.

   NOTE: If the token is not in the directory, then the function will
   use a PDoesNotExist redeemer to prove that the token is not programmable

   IMPORTANT: The caller should ensure that the destination address of the
   programmable token(s) in this transaction all correspond to the same
   programmable logic payment credential (even in the case of non-programmable
   tokens) otherwise the transaction will fail onchain validation.
-}
transferProgrammableToken :: forall env era m. (MonadReader env m, Env.HasDirectoryEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => [C.TxIn] -> CurrencySymbol -> [UTxODat era DirectorySetNode] -> m ()
transferProgrammableToken _ _ [] = error "directory list not initialised"
transferProgrammableToken tokenTxIns programmableTokenSymbol directoryList = Utils.inBabbage @era $ do
  nid <- queryNetworkId
  paramsPolId <- asks (Env.protocolParamsPolicyId . Env.directoryEnv)
  paramsTxIn <- asks (Env.dsTxIn . Env.directoryEnv)

  let globalStakeScript = programmableLogicGlobalScript paramsPolId
      globalStakeCred = C.StakeCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 globalStakeScript
      baseSpendingScript = programmableLogicBaseScript globalStakeCred

      -- Finds the directory node with the highest key that is less than or equal
      -- to the programmable token symbol
      UTxODat{uIn = dirNodeRef, uDatum = dirNodeDat} =
        maximumBy (compare `on` (key . uDatum)) $
          filter ((<= programmableTokenSymbol) . key . uDatum) directoryList

      -- Finds the index of the directory node reference in the transaction ref
      -- inputs
      directoryNodeReferenceIndex txBody =
        fromIntegral @Int @Integer $ findIndexReference dirNodeRef txBody

      -- The redeemer for the global script based on whether a dirctory node
      -- exists with the programmable token symbol
      programmableLogicGlobalRedeemer txBody =
        if key dirNodeDat == programmableTokenSymbol
          -- TODO: extend to allow multiple proofs, onchain allows it
          then TransferAct [TokenExists $ directoryNodeReferenceIndex txBody]
          else TransferAct [TokenDoesNotExist $ directoryNodeReferenceIndex txBody]

      programmableGlobalWitness txBody = buildScriptWitness globalStakeScript C.NoScriptDatumForStake (programmableLogicGlobalRedeemer txBody)

  addReference paramsTxIn -- Protocol Params TxIn
  addReference dirNodeRef -- Directory Node TxIn
  traverse_ (\tin -> spendPlutusInlineDatum tin baseSpendingScript ()) tokenTxIns
  addWithdrawalWithTxBody -- Add the global script witness to the transaction
    (C.makeStakeAddress nid globalStakeCred)
    (C.Quantity 0)
    $ C.ScriptWitness C.ScriptWitnessForStakeAddr . programmableGlobalWitness

{- Seize a programmable token from a user address to an issuer address. The
   outputs address will be that of the issuer retrieved from @issuerTxOut@.
   Throws if the payment credentials of the issuer output does not match the
   programmable logic payment credential.

   IMPORTANT: It is the caller's responsibility to
   ensure that the specific issuer logic stake script witness is included in the
   final transaction.

   NOTE: Seems the issuer is only able to seize 1 UTxO at a time.
   In the future we should allow multiple UTxOs in 1 Tx.
-}
seizeProgrammableToken :: forall a env era m. (MonadReader env m, Env.HasDirectoryEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => UTxODat era a -> UTxODat era a -> C.PolicyId -> [UTxODat era DirectorySetNode] -> m ()
seizeProgrammableToken UTxODat{uIn = seizingTxIn, uOut = seizingTxOut} UTxODat{uIn = issuerTxIn, uOut = issuerTxOut} seizingTokenPolicyId directoryList = Utils.inBabbage @era $ do
  nid <- queryNetworkId
  paramsPolId <- asks (Env.protocolParamsPolicyId . Env.directoryEnv)
  paramsTxIn <- asks (Env.dsTxIn . Env.directoryEnv)

  let globalStakeScript = programmableLogicGlobalScript paramsPolId
      globalStakeCred = C.StakeCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 globalStakeScript
      baseSpendingScript = programmableLogicBaseScript globalStakeCred

  -- Finds the directory node entry that references the programmable token symbol
  dirNodeRef <-
    maybe (error "Cannot seize non-programmable token. Entry does not exist in directoryList") (pure . uIn) $
      find (isNodeWithProgrammableSymbol (transPolicyId seizingTokenPolicyId)) directoryList

  checkIssuerAddressIsProgLogicCred (C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 baseSpendingScript) issuerTxOut

  let seizedValue = case seizingTxOut of
        (C.TxOut _ v _ _) ->
          C.filterValue
            ( \case
                C.AdaAssetId -> True
                C.AssetId a _ -> a == seizingTokenPolicyId
            )
            $ C.txOutValueToValue v

      (issuerOutAddr, issuerOutVal) = case issuerTxOut of
        (C.TxOut a (C.txOutValueToValue -> v) _ _) ->
          (a, C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toLedgerValue @era C.maryBasedEra (v <> seizedValue))

      seizedIssuerOutput = C.TxOut issuerOutAddr issuerOutVal C.TxOutDatumNone C.ReferenceScriptNone

      -- Finds the index of the directory node reference in the transaction ref
      -- inputs
      directoryNodeReferenceIndex txBody =
        fromIntegral @Int @Integer $ findIndexReference dirNodeRef txBody

      -- Finds the index of the issuer input in the transaction body
      issuerInputIndex txBody =
        fromIntegral @Int @Integer $ findIndexSpending issuerTxIn txBody

      -- Finds the index of the issuer seized output in the transaction body
      issueOutputIndex txBody =
        fromIntegral @Int @Integer $ fst $ fromJust (find ((== seizedIssuerOutput) . snd) $ zip [0 ..] $ txBody ^. L.txOuts)

      -- The seizing redeemer for the global script
      programmableLogicGlobalRedeemer txBody =
        SeizeAct
          { plgrSeizeInputIdx = issuerInputIndex txBody,
            plgrSeizeOutputIdx = issueOutputIndex txBody,
            plgrDirectoryNodeIdx = directoryNodeReferenceIndex txBody
          }

      programmableGlobalWitness txBody = buildScriptWitness globalStakeScript C.NoScriptDatumForStake (programmableLogicGlobalRedeemer txBody)

  addReference paramsTxIn -- Protocol Params TxIn
  addReference dirNodeRef -- Directory Node TxIn
  spendPlutusInlineDatum seizingTxIn baseSpendingScript () -- Redeemer is ignored in programmableLogicBase
  addBtx (over L.txOuts (seizedIssuerOutput :)) -- Add the seized output to the transaction
  addWithdrawalWithTxBody -- Add the global script witness to the transaction
    (C.makeStakeAddress nid globalStakeCred)
    (C.Quantity 0)
    $ C.ScriptWitness C.ScriptWitnessForStakeAddr . programmableGlobalWitness

  -- TODO: check that the issuerTxOut is at a programmable logic payment credential
checkIssuerAddressIsProgLogicCred :: forall era ctx m. ( MonadBuildTx era m) => C.PaymentCredential -> C.TxOut ctx era -> m ()
checkIssuerAddressIsProgLogicCred _progLogicCred (C.TxOut (C.AddressInEra _ (C.ShelleyAddress _ _pcred _stakeRef)) _ _ C.ReferenceScriptNone) =
  pure ()
checkIssuerAddressIsProgLogicCred _ _ = error "Issuer address is not a programmable logic credential"

isNodeWithProgrammableSymbol :: forall era. CurrencySymbol -> UTxODat era DirectorySetNode -> Bool
isNodeWithProgrammableSymbol programmableTokenSymbol (uDatum -> dat) = key dat == programmableTokenSymbol

getDirectoryNodeInline :: C.InAnyCardanoEra (C.TxOut C.CtxTx) -> Maybe DirectorySetNode
getDirectoryNodeInline (C.InAnyCardanoEra C.ConwayEra (C.TxOut _ _ dat _)) =
  case dat of
    C.TxOutDatumInline C.BabbageEraOnwardsConway (fromHashableScriptData -> Just d) -> Just d
    _ -> Nothing
getDirectoryNodeInline _ = Nothing
