module BenchmarkOnchain.CardanoScriptHelpers (scriptHashFromCardanoScript) where

import Cardano.Api qualified as C
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V3 (ScriptHash (ScriptHash))

scriptHashFromCardanoScript :: C.PlutusScript C.PlutusScriptV3 -> ScriptHash
scriptHashFromCardanoScript =
    ScriptHash
        . PV1.toBuiltin
        . C.serialiseToRawBytes
        . C.hashScript
        . C.PlutusScript C.PlutusScriptV3
