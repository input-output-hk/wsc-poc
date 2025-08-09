#! /bin/bash

export CIP_143_BLOCKFROST_TOKEN=preview1F0TvMF5VAk70b5YPiR20njoEG9ByGjf
export CIP_143_DIRECTORY_SCRIPT_ROOT=preview-network/preview_script_root.json

cabal run cip-143-cli -- policy src/examples/aiken/aiken/plutus.json \
  transfer --signing-key-file=preview-network/operator.sk \
  addr_test1qpd42egme33z960rr8vlnt69lpmythdpm7ydk2e6k5nj5hlsntgg007y0wgjlfwju6eksrghay9rg60vw49kejfah76sr024g0 aabbccdd 100 "I 200" \


