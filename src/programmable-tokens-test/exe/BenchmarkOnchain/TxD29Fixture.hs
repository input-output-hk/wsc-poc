{-# LANGUAGE OverloadedStrings #-}

module BenchmarkOnchain.TxD29Fixture (
    TxD29Payloads (..),
    mkTxD29ProgrammableLogicBaseSpendingCtx,
    mkTxD29ProgrammableLogicBaseStakeCtx,
    txD29BaseScriptCred,
    txD29DirectoryNodeIssuerCred,
    txD29DirectoryNodeNext,
    txD29GlobalStakeCred,
    txD29Hash,
    txD29IssuancePostfix,
    txD29IssuancePrefix,
    txD29ProgrammableAssetCS,
    txD29ProtocolParamsCS,
    txD29RegistryNodeCS,
    txD29ScriptInputRef,
    txD29TransferLogicStakeCred,
) where

import BenchmarkOnchain.ScriptHelpers (assetUnitHex, currencySymbolHex, hexToBuiltin, mkValue, pubKeyHashHex, scriptHashHex, txIdHex)
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map
import ProgrammableTokens.Test.ScriptContext.Builder (mkAdaValue)

data TxD29Payloads = TxD29Payloads
    { txD29PayloadDirectoryNodeDatumData :: BuiltinData
    , txD29PayloadGlobalStakeRedeemer :: BuiltinData
    , txD29PayloadProtocolParamsDatumData :: BuiltinData
    , txD29PayloadRefDatum1Data :: BuiltinData
    }

txD29Hash :: String
txD29Hash = "d29ce2a9f79a70a91d83a40e0e1cf346ab94979b6f0ba001de8a89895aa518df"

txD29ProtocolParamsCS :: CurrencySymbol
txD29ProtocolParamsCS = currencySymbolHex "c348817600e8cd22ddf01b4fef9437a4d768700c313415fc2ad5ee09"

txD29RegistryNodeCS :: CurrencySymbol
txD29RegistryNodeCS = currencySymbolHex "f1f838a525637791ca06d1fd415358a27ba829024ee0b90af5e32f14"

txD29GlobalStakeCred :: Credential
txD29GlobalStakeCred = ScriptCredential (scriptHashHex "36775ef231d797f8234268d4a7ecb51d5c44c096685dfb2cb881d7d3")

txD29TransferLogicStakeCred :: Credential
txD29TransferLogicStakeCred = ScriptCredential (scriptHashHex "5b7d265e7d862937c6e1a5266bba7dc685b786b1cc33551aa66867c9")

txD29BaseScriptCred :: Credential
txD29BaseScriptCred = ScriptCredential (scriptHashHex "fca77bcce1e5e73c97a0bfa8c90f7cd2faff6fd6ed5b6fec1c04eefa")

txD29DirectoryNodeIssuerCred :: Credential
txD29DirectoryNodeIssuerCred = ScriptCredential (scriptHashHex "b427cc9d5b829bbf66a784066fa3c03684fee2bfac7b571654ae8f55")

txD29InputStakePkh :: PubKeyHash
txD29InputStakePkh = pubKeyHashHex "37cd439e255465bce092323ed5b2e8b6d66581a1e170355c2a69ce62"

txD29RecipientStakePkh :: PubKeyHash
txD29RecipientStakePkh = pubKeyHashHex "586bc2eab5e93fef22664bf8c6db809b5ae90e2d848afb90a38bea8f"

txD29WstUnit :: String
txD29WstUnit = "b34a184f1f2871aa4d33544caecefef5242025f45c3fa5213d7662a9575354"

txD29ProgrammableAssetCS :: CurrencySymbol
txD29ProgrammableAssetCS = currencySymbolHex "b34a184f1f2871aa4d33544caecefef5242025f45c3fa5213d7662a9"

txD29IssuancePrefix :: BuiltinByteString
txD29IssuancePrefix = hexToBuiltin "338ef18819c855fc8970be65827abb86089b58fad24b31cff332ce97"

txD29IssuancePostfix :: BuiltinByteString
txD29IssuancePostfix = hexToBuiltin "87f2531e16923b81a0f3f507363db6b7cebf30735cb76b94543ca7ac"

txD29DirectoryNodeNext :: BuiltinByteString
txD29DirectoryNodeNext = hexToBuiltin "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

txD29ScriptInputRef :: TxOutRef
txD29ScriptInputRef = TxOutRef "0f0bef2ec112ff86d062cd5cf8e0172dd1449b740a6677fa348de09ee9a1fef3" 1

txD29PubInputRef :: TxOutRef
txD29PubInputRef = TxOutRef "57e6c1c0c8a19799a15682b5dc89882757c41aba70622a1a7f948996c8f16be7" 2

txD29RefInput0Ref :: TxOutRef
txD29RefInput0Ref = TxOutRef "09dab423b34f670ebb49f62fb3b9c25a4c03c53689cd74ab415df2d7fa90734c" 2

txD29RefInput1Ref :: TxOutRef
txD29RefInput1Ref = TxOutRef "bb16e777bfc4977f922a163ab90bf8415c21e4cd3800ef68a956f494a55193e8" 0

txD29RefInput2Ref :: TxOutRef
txD29RefInput2Ref = TxOutRef "d0975d3bbb024c3f0df82614ac800435f56f13a14a2480c63938a6ac385ea556" 2

txD29SpendingRedeemer :: BuiltinData
txD29SpendingRedeemer = PlutusTx.dataToBuiltinData (PlutusTx.Constr 0 [])

txD29TransferLogicStakeRedeemer :: BuiltinData
txD29TransferLogicStakeRedeemer =
    PlutusTx.dataToBuiltinData $
        PlutusTx.List
            [ PlutusTx.Constr 0 [PlutusTx.I 1]
            , PlutusTx.Constr 0 [PlutusTx.I 1]
            , PlutusTx.Constr 0 [PlutusTx.I 1]
            ]

txD29ScriptAddressWithStake :: PubKeyHash -> Address
txD29ScriptAddressWithStake stakePkh =
    Address txD29BaseScriptCred (Just (StakingHash (PubKeyCredential stakePkh)))

txD29PubAddress :: Address
txD29PubAddress = Address (PubKeyCredential txD29InputStakePkh) Nothing

txD29ReferenceScriptAddress0 :: Address
txD29ReferenceScriptAddress0 = Address (ScriptCredential (scriptHashHex "b8459bc5cc1dae2962abad27450f7dcc81376ae6ebda8d25e942508e")) Nothing

txD29ReferenceScriptAddress1 :: Address
txD29ReferenceScriptAddress1 = Address (ScriptCredential (scriptHashHex "9f6381bdf046fd671ebac45c1ad59263e282fa00a7083620fb449413")) Nothing

txD29ReferenceScriptAddress2 :: Address
txD29ReferenceScriptAddress2 = Address (ScriptCredential (scriptHashHex "9c0b7840bea475b3b20714bfa2d105df42a13283ca53f62692ec3f8d")) Nothing

txD29ProtocolParamsAssetUnit :: String
txD29ProtocolParamsAssetUnit = "c348817600e8cd22ddf01b4fef9437a4d768700c313415fc2ad5ee0950726f746f636f6c506172616d73"

txD29IssuanceAssetUnit :: String
txD29IssuanceAssetUnit = "8ea903ae40af4e0cff4164285b050c6a24ef10576b9fbf091a870b3f338ef18819c855fc8970be65827abb86089b58fad24b31cff332ce97"

txD29DirectoryNodeAssetUnit :: String
txD29DirectoryNodeAssetUnit = "f1f838a525637791ca06d1fd415358a27ba829024ee0b90af5e32f14b34a184f1f2871aa4d33544caecefef5242025f45c3fa5213d7662a9"

mkTxD29TxOut :: Address -> Value -> OutputDatum -> TxOut
mkTxD29TxOut address value datum =
    TxOut
        { txOutAddress = address
        , txOutValue = value
        , txOutDatum = datum
        , txOutReferenceScript = Nothing
        }

mkTxD29NoDatumInput :: TxOutRef -> Address -> Value -> TxInInfo
mkTxD29NoDatumInput outRef address value =
    TxInInfo
        { txInInfoOutRef = outRef
        , txInInfoResolved = mkTxD29TxOut address value NoOutputDatum
        }

mkTxD29DatumInput :: TxOutRef -> Address -> Value -> BuiltinData -> TxInInfo
mkTxD29DatumInput outRef address value datumData =
    TxInInfo
        { txInInfoOutRef = outRef
        , txInInfoResolved = mkTxD29TxOut address value (OutputDatum (Datum datumData))
        }

mkTxD29TxInfo :: TxD29Payloads -> TxInfo
mkTxD29TxInfo payloads =
    TxInfo
        { txInfoInputs =
            [ mkTxD29NoDatumInput
                txD29ScriptInputRef
                (txD29ScriptAddressWithStake txD29InputStakePkh)
                (mkAdaValue 1_137_840 <> mkValue [assetUnitHex txD29WstUnit 42])
            , mkTxD29NoDatumInput txD29PubInputRef txD29PubAddress (mkAdaValue 9_998_519_974)
            ]
        , txInfoReferenceInputs =
            [ mkTxD29DatumInput
                txD29RefInput0Ref
                txD29ReferenceScriptAddress0
                (mkAdaValue 1_383_510 <> mkValue [assetUnitHex txD29ProtocolParamsAssetUnit 1])
                (txD29PayloadProtocolParamsDatumData payloads)
            , mkTxD29DatumInput
                txD29RefInput1Ref
                txD29ReferenceScriptAddress1
                (mkAdaValue 1_430_920 <> mkValue [assetUnitHex txD29IssuanceAssetUnit 1])
                (txD29PayloadRefDatum1Data payloads)
            , mkTxD29DatumInput
                txD29RefInput2Ref
                txD29ReferenceScriptAddress2
                (mkAdaValue 1_732_620 <> mkValue [assetUnitHex txD29DirectoryNodeAssetUnit 1])
                (txD29PayloadDirectoryNodeDatumData payloads)
            ]
        , txInfoOutputs =
            [ mkTxD29TxOut
                (txD29ScriptAddressWithStake txD29InputStakePkh)
                (mkAdaValue 1_137_840 <> mkValue [assetUnitHex txD29WstUnit 28])
                NoOutputDatum
            , mkTxD29TxOut
                (txD29ScriptAddressWithStake txD29RecipientStakePkh)
                (mkAdaValue 1_133_530 <> mkValue [assetUnitHex txD29WstUnit 14])
                NoOutputDatum
            , mkTxD29TxOut txD29PubAddress (mkAdaValue 9_997_039_904) NoOutputDatum
            ]
        , txInfoFee = 346_540
        , txInfoMint = UnsafeMintValue Map.empty
        , txInfoTxCerts = []
        , txInfoWdrl = Map.unsafeFromList [(txD29GlobalStakeCred, 0), (txD29TransferLogicStakeCred, 0)]
        , txInfoValidRange = always
        , txInfoSignatories = [txD29InputStakePkh]
        , txInfoRedeemers =
            Map.unsafeFromList
                [ (Spending txD29ScriptInputRef, Redeemer txD29SpendingRedeemer)
                , (Rewarding txD29GlobalStakeCred, Redeemer (txD29PayloadGlobalStakeRedeemer payloads))
                , (Rewarding txD29TransferLogicStakeCred, Redeemer txD29TransferLogicStakeRedeemer)
                ]
        , txInfoData = Map.empty
        , txInfoId = txIdHex txD29Hash
        , txInfoVotes = Map.empty
        , txInfoProposalProcedures = []
        , txInfoCurrentTreasuryAmount = Nothing
        , txInfoTreasuryDonation = Nothing
        }

mkTxD29ProgrammableLogicBaseSpendingCtx :: TxD29Payloads -> ScriptContext
mkTxD29ProgrammableLogicBaseSpendingCtx payloads =
    ScriptContext
        (mkTxD29TxInfo payloads)
        (Redeemer txD29SpendingRedeemer)
        (SpendingScript txD29ScriptInputRef Nothing)

mkTxD29ProgrammableLogicBaseStakeCtx :: TxD29Payloads -> ScriptContext
mkTxD29ProgrammableLogicBaseStakeCtx payloads =
    ScriptContext
        (mkTxD29TxInfo payloads)
        (Redeemer (txD29PayloadGlobalStakeRedeemer payloads))
        (RewardingScript txD29GlobalStakeCred)
