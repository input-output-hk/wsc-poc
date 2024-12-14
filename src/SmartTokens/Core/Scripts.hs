module SmartTokens.Core.Scripts (
tryCompile) where

import Plutarch
import Plutarch.ByteString (PByteString, plengthBS, psliceBS, pindexBS)
import Plutarch.Crypto (pkeccak_256, pblake2b_224)
import Plutarch.Integer (PInteger, pmod)
import Plutarch.Lift (pconstant)
import Plutarch.Bool (pif, (#==))
import PlutusCore.Crypto.Hash qualified as Hash
import PlutusLedgerApi.Common (serialiseUPLC)
import Data.ByteString.Short (fromShort)
import Plutarch.Script (Script(unScript))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import PlutusLedgerApi.V2 (
  Data,
  ExBudget,
 )
import Data.Text
import Plutarch.Evaluate
import Data.Bifunctor ( Bifunctor(first) )

tryCompile :: Config -> ClosedTerm a -> Script
tryCompile cfg x = case compile cfg x of
  Left e -> error $ "Compilation failed: " <> show e
  Right s -> s

tryCompileTracingAndBinds :: ClosedTerm a -> Script
tryCompileTracingAndBinds = tryCompile (Tracing LogInfo DoTracingAndBinds)

tryCompileNoTracing :: ClosedTerm a -> Script
tryCompileNoTracing = tryCompile NoTracing





