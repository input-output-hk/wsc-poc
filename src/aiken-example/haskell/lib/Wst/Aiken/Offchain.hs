-- | Off-chain code for the aiken example
module Wst.Aiken.Offchain
  ( register,
  )
where

import Wst.Offchain.BuildTx.DirectorySet (InsertNodeArgs (..))
import Wst.Offchain.BuildTx.DirectorySet qualified as Directory

-- | Register the policy
register :: (Applicative m) => m ()
register = do
  let args =
        InsertNodeArgs
          { inaNewKey = undefined
          }
  pure ()
