#! /bin/bash

export CIP_0143_BLOCKFROST_TOKEN=preview1F0TvMF5VAk70b5YPiR20njoEG9ByGjf

cabal run cip-143-cli -- register \
  --signing-key-file=keys/operator.sk \
  aiken src/examples/aiken/aiken/plutus.json
