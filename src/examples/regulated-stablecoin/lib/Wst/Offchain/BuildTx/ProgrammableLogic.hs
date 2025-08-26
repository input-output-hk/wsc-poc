{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use second" #-}
module Wst.Offchain.BuildTx.ProgrammableLogic
  ( seizeProgrammableToken,
  )
where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Lens ((^.))
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx (MonadBuildTx, addReference, addWithdrawalWithTxBody,
                       buildScriptWitness, findIndexReference,
                       findIndexSpending, prependTxOut, spendPlutusInlineDatum)
import Convex.CardanoApi.Lenses as L
import Convex.Class (MonadBlockchain (queryNetworkId))
import Convex.PlutusLedger.V1 (transPolicyId)
import Convex.Utils qualified as Utils
import Data.Foldable (find)
import Data.List (partition)
import Data.Maybe (fromJust)
import GHC.Exts (IsList (..))
import PlutusLedgerApi.V3 (CurrencySymbol (..))
import ProgrammableTokens.OffChain.Env qualified as Env
import SmartTokens.Contracts.ProgrammableLogicBase (ProgrammableLogicGlobalRedeemer (..))
import SmartTokens.Types.ProtocolParams
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (..))
import Wst.Offchain.Query (UTxODat (..))

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

  -- TODO: check that the issuerTxOut is at a programmable logic payment credential
_checkIssuerAddressIsProgLogicCred :: forall era ctx m. ( MonadBuildTx era m) => C.PaymentCredential -> C.TxOut ctx era -> m ()
_checkIssuerAddressIsProgLogicCred _progLogicCred (C.TxOut (C.AddressInEra _ (C.ShelleyAddress _ _pcred _stakeRef)) _ _ C.ReferenceScriptNone) =
  pure ()
_checkIssuerAddressIsProgLogicCred _ _ = error "Issuer address is not a programmable logic credential"

isNodeWithProgrammableSymbol :: forall era. CurrencySymbol -> UTxODat era DirectorySetNode -> Bool
isNodeWithProgrammableSymbol programmableTokenSymbol (uDatum -> dat) = key dat == programmableTokenSymbol
