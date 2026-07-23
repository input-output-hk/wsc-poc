#!/usr/bin/env bash
set -euo pipefail

cabal run benchmark-onchain-scripts -- "$@"
