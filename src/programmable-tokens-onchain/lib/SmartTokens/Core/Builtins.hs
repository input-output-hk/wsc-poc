{- | Builtin primitives from the Van Rossem hard fork (PV11) that upstream
Plutarch does not wrap.

Upstream Plutarch knows about @PLC.DropList@ -- its optimiser carries an arity
entry for the builtin -- but exposes no term for it, and its
'Plutarch.Internal.ListLike.pdrop' is a compile-time-unrolled chain of
@tailList@ applications that cannot take a runtime index. Every one of our uses
drops to an index carried in a redeemer, so that is not a substitute.

Defining the wrapper here rather than depending on a Plutarch fork for it keeps
the binding available whichever Plutarch we build against: it needs only
'punsafeBuiltin' and the @DropList@ constructor, both of which are present in
the fork we pin today and in upstream master.
-}
module SmartTokens.Core.Builtins (
    pdropList,
) where

import Data.Kind (Type)
import Plutarch.Builtin.Data (PBuiltinList)
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Internal.Term (
    S,
    Term,
    pforce,
    phoistAcyclic,
    punsafeBuiltin,
    (:-->),
 )
import PlutusCore qualified as PLC

{- | Drop the given number of elements from the front of a builtin list, using
the @dropList@ builtin (CIP-132, available to PlutusV3 scripts from protocol
version 11 / the Van Rossem hard fork).

Costing (variant E): CPU @116,711 + 1,957*n@ where @n@ is the drop count;
memory is a flat 4. That is strictly cheaper than any tail-recursive or
unrolled drop for every @n >= 1@, and unlike them it does not grow in memory.

One force: @dropList@'s denotation has a single type variable, so the builtin
carries one type abstraction.

= Note

A negative count is treated as zero -- the list comes back unchanged, matching
Haskell's 'drop'. Callers that must REJECT a negative index (a redeemer-supplied
one, say) need an explicit guard in front; see the caller in
"SmartTokens.Contracts.Issuance".
-}
pdropList ::
    forall (a :: S -> Type) (s :: S).
    Term s (PInteger :--> PBuiltinList a :--> PBuiltinList a)
pdropList = phoistAcyclic $ pforce $ punsafeBuiltin PLC.DropList
