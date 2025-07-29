{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module ProgrammableTokens.OffChain.BuildTx.Directory(
  insertDirectoryNode
) where

import Cardano.Api.Shelley qualified as C
import Control.Monad.Reader (MonadReader, asks)
import Convex.BuildTx (MonadBuildTx, addReference, mintPlutus, prependTxOut,
                       spendPlutusInlineDatum)
import Convex.Class (MonadBlockchain, queryNetworkId)
import Convex.PlutusLedger.V1 (transPolicyId, transScriptHash,
                               transStakeCredential)
import Convex.Scripts (toHashableScriptData)
import Convex.Utils qualified as Utils
import Data.Maybe (fromMaybe)
import GHC.Exts (IsList (..))
import PlutusLedgerApi.V1 qualified as PlutusTx
import PlutusLedgerApi.V3 (CurrencySymbol (..), ScriptHash (..))
import ProgrammableTokens.OffChain.Env (DirectoryEnv (..),
                                        TransferLogicEnv (..))
import ProgrammableTokens.OffChain.Env qualified as Env
import ProgrammableTokens.OffChain.Scripts (scriptPolicyIdV3)
import ProgrammableTokens.OffChain.UTxODat (UTxODat (..))
import SmartTokens.Contracts.IssuanceCborHex (IssuanceCborHex)
import SmartTokens.LinkedList.MintDirectory (DirectoryNodeAction (InsertDirectoryNode))
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams)
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (..))

{-| The 'InsertNodeArgs' for adding a new policy to the registry
-}
insertNodeArgs :: DirectoryEnv -> TransferLogicEnv -> InsertNodeArgs
insertNodeArgs dir inta@TransferLogicEnv{tleTransferScript, tleIssuerScript, tleMintingScript, tleGlobalParamsNft} =

  let mintingScript  = Env.programmableTokenMintingScript dir inta
      issuedPolicyId = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV3 mintingScript
      issuedSymbol   = transPolicyId issuedPolicyId
      mintingLogicHash = C.hashScript $ C.PlutusScript C.plutusScriptVersion tleMintingScript

  in
    InsertNodeArgs
      { inaNewKey = issuedSymbol
      , inaHashedParam = transScriptHash mintingLogicHash
      , inaTransferLogic = C.StakeCredentialByScript $ C.hashScript $ C.PlutusScript C.plutusScriptVersion tleTransferScript
      , inaIssuerLogic   = C.StakeCredentialByScript $ C.hashScript $ C.PlutusScript C.plutusScriptVersion tleIssuerScript
      , inaGlobalStateCS = tleGlobalParamsNft
      }

{-| Data for a new node to be inserted into the directory
-}
data InsertNodeArgs =
  InsertNodeArgs
    { inaNewKey        :: CurrencySymbol -- ^ currency symbol of the CIP-0143 token
    , inaHashedParam   :: ScriptHash
    , inaTransferLogic :: C.StakeCredential -- ^ Stake validator for transfers
    , inaIssuerLogic   :: C.StakeCredential -- ^ Stake validator for minting and burning
    , inaGlobalStateCS :: Maybe CurrencySymbol -- ^ Currency symbol of an NFT that identifies a UTxO with global parameters specific to the new token
    }

{-| Use the 'MonadBuildTx' effect to construct a transaction that inserts a new node into the CIP-143 registry.
-}
insertDirectoryNode :: forall era env m.
  ( MonadReader env m
  , Env.HasDirectoryEnv env
  , C.IsBabbageBasedEra era
  , MonadBuildTx era m
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadBlockchain era m
  , Env.HasTransferLogicEnv env
  )
  => UTxODat era ProgrammableLogicGlobalParams
  -> UTxODat era IssuanceCborHex
  -> UTxODat era DirectorySetNode
  -> m ()
insertDirectoryNode UTxODat{uIn=paramsRef} UTxODat{uIn=issuanceCborHexRef} UTxODat{uIn, uOut=firstTxOut, uDatum=firstTxData} = Utils.inBabbage @era $ do
  netId <- queryNetworkId
  directorySpendingScript <- asks (Env.dsDirectorySpendingScript . Env.directoryEnv)
  directoryMintingScript <- asks (Env.dsDirectoryMintingScript . Env.directoryEnv)
  InsertNodeArgs{inaNewKey, inaHashedParam, inaTransferLogic, inaIssuerLogic, inaGlobalStateCS} <- insertNodeArgs <$> asks Env.directoryEnv <*> asks Env.transferLogicEnv
  let

      firstTxVal :: C.TxOutValue era
      firstTxVal = case firstTxOut of
        (C.TxOut _ v _ _) -> v

      newTokenName =
        let CurrencySymbol s = inaNewKey
        in C.AssetName $ PlutusTx.fromBuiltin s

      newVal = C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toLedgerValue @era C.maryBasedEra
          $ fromList [(C.AssetId (scriptPolicyIdV3 directoryMintingScript) newTokenName, 1)]

      addr =
        C.makeShelleyAddressInEra
          C.shelleyBasedEra
          netId
          (C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 directorySpendingScript)
          C.NoStakeAddress

      dsn = DirectorySetNode
            { key = inaNewKey
            , next = next firstTxData
            , transferLogicScript = transStakeCredential inaTransferLogic
            , issuerLogicScript = transStakeCredential inaIssuerLogic
            , globalStateCS = fromMaybe (CurrencySymbol "") inaGlobalStateCS
            }
      newDat = C.TxOutDatumInline C.babbageBasedEra $ toHashableScriptData dsn
      insertedNode = C.TxOut addr newVal newDat C.ReferenceScriptNone

      firstDat = firstTxData { next = inaNewKey }
      firstOutput = C.TxOut addr firstTxVal (C.TxOutDatumInline C.babbageBasedEra $ toHashableScriptData firstDat) C.ReferenceScriptNone
  addReference paramsRef
  addReference issuanceCborHexRef
  spendPlutusInlineDatum uIn directorySpendingScript ()
  mintPlutus directoryMintingScript (InsertDirectoryNode inaNewKey inaHashedParam) newTokenName 1
  prependTxOut insertedNode
  prependTxOut firstOutput
