module BenchmarkOnchain.MainnetDexFixture (
    mainnetDexFeeAda,
    mainnetDexNightToken,
    mainnetDexOutputStakePkhAt,
    mainnetDexPoolInputAda,
    mainnetDexPoolInputNight,
    mainnetDexPoolOutputAda,
    mainnetDexPoolOutputNight,
    mainnetDexPoolStateToken,
    mainnetDexSwapInputAda,
    mainnetDexSwapInputNightQtys,
    mainnetDexSwapOutputAdaQtys,
    mainnetDexSwapOutputNightQtys,
    mainnetDexTxHash,
) where

import BenchmarkOnchain.ScriptHelpers (bs28, tokenNameHex)
import PlutusLedgerApi.V3 (PubKeyHash (PubKeyHash), TokenName)

mainnetDexTxHash :: String
mainnetDexTxHash = "c3111df78f97a8f7ed4934510e04d9c777339a00d5eac26e123471ff4ab954ff"

mainnetDexSwapInputAda :: Integer
mainnetDexSwapInputAda = 3_280_000

mainnetDexFeeAda :: Integer
mainnetDexFeeAda = 635_743_434

mainnetDexNightToken :: TokenName
mainnetDexNightToken = tokenNameHex "4e49474854"

mainnetDexPoolStateToken :: TokenName
mainnetDexPoolStateToken = tokenNameHex "504f4f4c"

mainnetDexSwapInputNightQtys :: [Integer]
mainnetDexSwapInputNightQtys =
    [ 1_464_529_092
    , 1_486_106_441
    , 1_417_330_747
    , 1_459_421_934
    , 1_626_123_671
    , 1_609_609_121
    , 1_503_344_748
    , 1_625_264_687
    , 1_459_417_156
    , 1_506_472_613
    , 1_580_441_203
    , 1_529_578_748
    , 1_583_849_411
    , 1_472_409_806
    , 1_696_633_357
    , 1_524_474_956
    ]

mainnetDexSwapOutputAdaQtys :: [Integer]
mainnetDexSwapOutputAdaQtys =
    [ 164_168
    , 167_412
    , 157_648
    , 163_407
    , 188_003
    , 185_472
    , 170_031
    , 187_870
    , 163_406
    , 170_466
    , 181_020
    , 173_796
    , 181_500
    , 165_312
    , 199_133
    , 173_071
    ]

mainnetDexSwapOutputNightQtys :: [Integer]
mainnetDexSwapOutputNightQtys =
    zipWith (-) mainnetDexSwapInputNightQtys (repeat 5_100_000)

mainnetDexPoolInputAda :: Integer
mainnetDexPoolInputAda = 139_587_470_341

mainnetDexPoolInputNight :: Integer
mainnetDexPoolInputNight = 1_147_114_273_642

mainnetDexPoolOutputAda :: Integer
mainnetDexPoolOutputAda = 127_849_411_782

mainnetDexPoolOutputNight :: Integer
mainnetDexPoolOutputNight = 1_256_417_018_801

mainnetDexOutputStakePkhAt :: Integer -> PubKeyHash
mainnetDexOutputStakePkhAt idx =
    PubKeyHash (bs28 (fromIntegral (0x30 + idx)))
