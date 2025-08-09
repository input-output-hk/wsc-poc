#! /bin/bash

export CIP_143_BLOCKFROST_TOKEN=preview1F0TvMF5VAk70b5YPiR20njoEG9ByGjf
export CIP_143_DIRECTORY_SCRIPT_ROOT=preview-network/preview_script_root.json

cabal run cip-143-cli -- query
