{-# LANGUAGE TemplateHaskell #-}
module ProgrammableTokens.OffChain.Error(
  ProgrammableTokensError(..),
  AsProgrammableTokensError(..)
) where

import Control.Lens (makeClassyPrisms)

data ProgrammableTokensError =
  OperatorNoUTxOs -- ^ The operator does not have any UTxOs
  | GlobalParamsNodeNotFound -- ^ The node with the global parameters was not found
  | IssuanceCborHexUTxONotFound -- ^ The UTxO with the issuance minting cbor hex was not found
  | DirectorySetNodeNotFound String -- ^ The UTxO with the directory entry for the policy was not found. (Policy not registered properly?)
  | UnexpectedScriptAddress -- ^ Expected a public key address, got a script address
  deriving stock (Show)

makeClassyPrisms ''ProgrammableTokensError
