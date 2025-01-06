{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ViewPatterns     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use second" #-}
module Wst.Offchain.BuildTx.ProgrammableLogic
  ( issueProgrammableToken,
    transferProgrammableToken,
    seizeProgrammableToken,
    registerProgrammableGlobalScript,
  )
where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Lens ((^.))
import Control.Monad (unless)
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx (MonadBuildTx, addReference, addWithdrawalWithTxBody,
                       buildScriptWitness, findIndexReference,
                       findIndexSpending, mintPlutus, prependTxOut,
                       spendPlutusInlineDatum)
import Convex.CardanoApi.Lenses as L
import Convex.Class (MonadBlockchain (queryNetworkId))
import Convex.PlutusLedger.V1 (transPolicyId)
import Convex.Utils qualified as Utils
import Data.Foldable (find, maximumBy, traverse_)
import Data.Function (on)
import Data.List (partition)
import Data.Maybe (fromJust)
import GHC.Exts (IsList (..))
import PlutusLedgerApi.V3 (CurrencySymbol (..))
import SmartTokens.Contracts.Issuance (SmartTokenMintingAction (MintPToken, RegisterPToken))
import SmartTokens.Contracts.ProgrammableLogicBase (ProgrammableLogicGlobalRedeemer (..),
                                                    TokenProof (..))
import SmartTokens.Types.ProtocolParams
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (..))
import Wst.Offchain.BuildTx.DirectorySet (InsertNodeArgs (..),
                                          insertDirectoryNode)
import Wst.Offchain.BuildTx.Utils (addConwayStakeCredentialCertificate)
import Wst.Offchain.Env (TransferLogicEnv (..))
import Wst.Offchain.Env qualified as Env
import Wst.Offchain.Query (UTxODat (..))

{- Issue a programmable token and register it in the directory set if necessary. The caller should ensure that the specific
   minting logic stake script witness is included in the final transaction.
  - If the programmable token is not in the directory, then it is registered
  - If the programmable token is in the directory, then it is minted
-}
issueProgrammableToken :: forall era env m. (MonadReader env m, Env.HasDirectoryEnv env, Env.HasTransferLogicEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => UTxODat era ProgrammableLogicGlobalParams -> (C.AssetName, C.Quantity) -> [UTxODat era DirectorySetNode] -> m C.PolicyId
issueProgrammableToken paramsTxOut (an, q) directoryList = Utils.inBabbage @era $ do
  inta@TransferLogicEnv{tleTransferScript, tleIssuerScript} <- asks Env.transferLogicEnv
  glParams <- asks (Env.globalParams . Env.directoryEnv)
  dir <- asks Env.directoryEnv

  -- The global params in the UTxO need to match those in our 'DirectoryEnv'.
  -- If they don't, we get a script error when trying to balance the transaction.
  -- To avoid this we check for equality here and fail early.
  unless (glParams == uDatum paramsTxOut) $
    -- FIXME: Error handling
    error "Global params do not match"

  let mintingScript = Env.programmableTokenMintingScript dir inta
      issuedPolicyId = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV3 mintingScript
      issuedSymbol = transPolicyId issuedPolicyId

      udat@UTxODat{uDatum = dirNodeData} =
        maximumBy (compare `on` (key . uDatum)) $
          filter ((<= issuedSymbol) . key . uDatum) directoryList

  if key dirNodeData == issuedSymbol
    then
      mintPlutus mintingScript MintPToken an q
    else do
      let nodeArgs =
            InsertNodeArgs
              { inaNewKey = issuedSymbol
              , inaTransferLogic = C.StakeCredentialByScript $ C.hashScript $ C.PlutusScript C.plutusScriptVersion tleTransferScript
              , inaIssuerLogic = C.StakeCredentialByScript $ C.hashScript $ C.PlutusScript C.plutusScriptVersion tleIssuerScript
              }

      mintPlutus mintingScript RegisterPToken an q
      insertDirectoryNode paramsTxOut udat nodeArgs

  pure issuedPolicyId

{- User facing transfer of programmable tokens from one address to another.
   The caller should ensure that the specific transfer logic stake script
   witness is included in the final transaction.

   NOTE: If the token is not in the directory, then the function will
   use a PDoesNotExist redeemer to prove that the token is not programmable

   IMPORTANT: The caller should ensure that the destination address of the
   programmable token(s) in this transaction all correspond to the same
   programmable logic payment credential otherwise the transaction will fail onchain validation.
-}
transferProgrammableToken :: forall env era m. (MonadReader env m, Env.HasDirectoryEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => UTxODat era ProgrammableLogicGlobalParams ->  [C.TxIn] -> CurrencySymbol -> [UTxODat era DirectorySetNode] -> m ()
transferProgrammableToken _ _ _ [] = error "directory list not initialised"
transferProgrammableToken paramsTxIn tokenTxIns programmableTokenSymbol directoryList = Utils.inBabbage @era $ do
  nid <- queryNetworkId

  baseSpendingScript <- asks (Env.dsProgrammableLogicBaseScript . Env.directoryEnv)
  globalStakeScript <- asks (Env.dsProgrammableLogicGlobalScript . Env.directoryEnv)


  let globalStakeCred = C.StakeCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 globalStakeScript

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

  addReference (uIn paramsTxIn) -- Protocol Params TxIn
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
seizeProgrammableToken :: forall a env era m. (MonadReader env m, Env.HasDirectoryEnv env, C.IsBabbageBasedEra era, MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m) => UTxODat era ProgrammableLogicGlobalParams -> UTxODat era a -> C.PolicyId -> [UTxODat era DirectorySetNode] -> m ()
seizeProgrammableToken UTxODat{uIn = paramsTxIn} UTxODat{uIn = seizingTxIn, uOut = seizingTxOut} seizingTokenPolicyId directoryList = Utils.inBabbage @era $ do
  nid <- queryNetworkId
  globalStakeScript <- asks (Env.dsProgrammableLogicGlobalScript . Env.directoryEnv)
  baseSpendingScript <- asks (Env.dsProgrammableLogicBaseScript . Env.directoryEnv)

  let globalStakeCred = C.StakeCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 globalStakeScript

  -- Finds the directory node entry that references the programmable token symbol
  dirNodeRef <-
    maybe (error "Cannot seize non-programmable token. Entry does not exist in directoryList") (pure . uIn) $
      find (isNodeWithProgrammableSymbol (transPolicyId seizingTokenPolicyId)) directoryList

  -- destStakeCred <- either (error . ("Could not unTrans credential: " <>) . show) pure $ unTransStakeCredential $ transCredential seizeDestinationCred
  let
      -- issuerDestinationAddress = C.makeShelleyAddressInEra C.shelleyBasedEra nid progLogicBaseCred (C.StakeAddressByValue destStakeCred)

      (seizedAddr, remainingValue) = case seizingTxOut of
        (C.TxOut a v _ _) ->
          let (_seized, other) =
                  partition
                    ( \case
                        (C.AdaAssetId, _q) -> False
                        (C.AssetId a_ _, _q) -> a_ == seizingTokenPolicyId
                    )
                    $ toList $ C.txOutValueToValue v
          in (a, fromList other)

      remainingTxOutValue = C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toLedgerValue @era C.maryBasedEra remainingValue

      seizedOutput = C.TxOut seizedAddr remainingTxOutValue C.TxOutDatumNone C.ReferenceScriptNone

      -- Finds the index of the directory node reference in the transaction ref
      -- inputs
      directoryNodeReferenceIndex txBody =
        fromIntegral @Int @Integer $ findIndexReference dirNodeRef txBody

      -- Finds the index of the issuer input in the transaction body
      seizingInputIndex txBody =
        fromIntegral @Int @Integer $ findIndexSpending seizingTxIn txBody

      -- Finds the index of the issuer seized output in the transaction body
      seizingOutputIndex txBody =
        fromIntegral @Int @Integer $ fst $ fromJust (find ((== seizedOutput) . snd ) $ zip [0 ..] $ txBody ^. L.txOuts)

      -- The seizing redeemer for the global script
      programmableLogicGlobalRedeemer txBody =
        SeizeAct
          { plgrSeizeInputIdx = seizingInputIndex txBody,
            plgrSeizeOutputIdx = seizingOutputIndex txBody,
            plgrDirectoryNodeIdx = directoryNodeReferenceIndex txBody
          }

      programmableGlobalWitness txBody = buildScriptWitness globalStakeScript C.NoScriptDatumForStake (programmableLogicGlobalRedeemer txBody)

  prependTxOut seizedOutput
  addReference paramsTxIn -- Protocol Params TxIn
  addReference dirNodeRef -- Directory Node TxIn
  spendPlutusInlineDatum seizingTxIn baseSpendingScript () -- Redeemer is ignored in programmableLogicBase
  addWithdrawalWithTxBody -- Add the global script witness to the transaction
    (C.makeStakeAddress nid globalStakeCred)
    (C.Quantity 0)
    $ C.ScriptWitness C.ScriptWitnessForStakeAddr . programmableGlobalWitness

registerProgrammableGlobalScript :: forall env era m. (MonadReader env m, C.IsBabbageBasedEra era, MonadBuildTx era m, Env.HasDirectoryEnv env) => m ()
registerProgrammableGlobalScript = case C.babbageBasedEra @era of
  C.BabbageEraOnwardsBabbage -> error "babbage era registration not implemented"
  C.BabbageEraOnwardsConway  -> Utils.inConway @era $ do
    programmableGlobalScript <- asks (Env.dsProgrammableLogicGlobalScript . Env.directoryEnv)
    let hshGlobal = C.hashScript $ C.PlutusScript C.plutusScriptVersion programmableGlobalScript
        credGlobal = C.StakeCredentialByScript hshGlobal
    addConwayStakeCredentialCertificate credGlobal

  -- TODO: check that the issuerTxOut is at a programmable logic payment credential
_checkIssuerAddressIsProgLogicCred :: forall era ctx m. ( MonadBuildTx era m) => C.PaymentCredential -> C.TxOut ctx era -> m ()
_checkIssuerAddressIsProgLogicCred _progLogicCred (C.TxOut (C.AddressInEra _ (C.ShelleyAddress _ _pcred _stakeRef)) _ _ C.ReferenceScriptNone) =
  pure ()
_checkIssuerAddressIsProgLogicCred _ _ = error "Issuer address is not a programmable logic credential"

isNodeWithProgrammableSymbol :: forall era. CurrencySymbol -> UTxODat era DirectorySetNode -> Bool
isNodeWithProgrammableSymbol programmableTokenSymbol (uDatum -> dat) = key dat == programmableTokenSymbol
