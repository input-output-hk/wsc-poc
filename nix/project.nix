{ repoRoot, inputs, pkgs, lib, system }:

let
  sha256map = {
    "https://github.com/colll78/plutarch-plutus"."b2379767c7f1c70acf28206bf922f128adc02f28" = "sha256-mhuW2CHxnc6FDWuMcjW/51PKuPOdYc4yxz+W5RmlQew=";
    "https://github.com/input-output-hk/catalyst-onchain-libs"."25996ca26f1b33ffef611bc81423aae5841e297f" = "sha256-NUh+l97+eO27Ppd8Bx0yMl0E5EV+p7+7GuFun1B8gRc=";
  };

  modules = [{ }];

  cabalProject = pkgs.haskell-nix.cabalProject' {
    inherit modules sha256map;
    src = ../.;
    name = "smart-tokens-plutarch";
    compiler-nix-name = "ghc966";
    index-state = "2024-10-16T00:00:00Z";
    inputMap = {
      "https://chap.intersectmbo.org/" = inputs.CHaP;
    };
    shell.withHoogle = false;
  };

  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    shellArgs = repoRoot.nix.shell;
  };

in
project
