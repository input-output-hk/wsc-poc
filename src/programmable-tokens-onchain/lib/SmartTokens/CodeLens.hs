module SmartTokens.CodeLens(
  _printTerm
) where

import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import Plutarch.Internal.Other (printScript)
import Plutarch.Internal.Term
import Plutarch.Internal.Term qualified as PI

-- TODO: Move to catalyst-libs project

-- _printTerm (communicated by Philip) just print some term as string. The term we want to print is
-- @
-- _term :: forall {s :: S}. Term s PBlacklistNode
-- _term = unsafeEvalTerm NoTracing (pconstant $ BlackListNode { key = "a", next = "b" })
-- @
-- Below, we inline the term and have it in a code lens. You can even run the code lens via Haskell
-- language server. The lens will then replace the string starting with "program ..." with exactly
-- the same string.
--
-- >>> _printTerm (pconstant $ BlacklistNode { blnKey = "a hi", blnNext = "a" })
-- "program 1.0.0 (List [B #61206869, B #61])"
_printTerm :: HasCallStack => ClosedTerm a -> String
_printTerm term = printScript $ either (error . T.unpack) id $ PI.compile PI.NoTracing term
