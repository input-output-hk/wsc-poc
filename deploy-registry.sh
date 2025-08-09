#! /bin/bash

export CIP_143_BLOCKFROST_TOKEN=preview1F0TvMF5VAk70b5YPiR20njoEG9ByGjf

cabal run cip-143-cli -- deploy --signing-key-file=preview-network/operator.sk \
  preview-network/preview_script_root.json
