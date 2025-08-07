#! /bin/bash

nix run github:aiken-lang/aiken#aiken -- build aiken
cp aiken/plutus.json haskell/data/aiken-scripts.json