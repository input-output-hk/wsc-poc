#! /bin/bash

export BLOCKFROST_TOKEN=preview1F0TvMF5VAk70b5YPiR20njoEG9ByGjf

set -e

cabal exec -- convex-tx-mod download $1 -o preview-network/tx.json
cabal exec -- convex-tx-mod graph -f ./preview-network/tx.json -o preview-network/tx.dot
