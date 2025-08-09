#! /bin/bash

export CIP_0143_BLOCKFROST_TOKEN=preview1F0TvMF5VAk70b5YPiR20njoEG9ByGjf
export DIRECTORY_SCRIPT_ROOT=preview-network/preview_script_root.json

cabal run cip-143-cli -- register \
  --signing-key-file=preview-network/operator.sk \
  aiken src/examples/aiken/aiken/plutus.json
