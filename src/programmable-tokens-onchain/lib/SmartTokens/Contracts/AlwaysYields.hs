{-| Plutus V3 script that always yields, ignoring its argument
-}
module SmartTokens.Contracts.AlwaysYields(
  palwaysSucceed
) where

import Plutarch.LedgerApi.V3 (PScriptContext)
import Plutarch.Prelude (PUnit, Term, pconstant, plam, (:-->))

{-| Validator that always succeeds
-}
palwaysSucceed :: Term s (PScriptContext :--> PUnit)
palwaysSucceed = plam (const $ pconstant ())
