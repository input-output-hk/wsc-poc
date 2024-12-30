{ repoRoot, inputs, pkgs, lib, system }:

let
  sha256map = {
    "https://github.com/j-mueller/sc-tools"."e2759559324e172f12b11ab815323c48ed8922b0" = "sha256-NHX+Euys+jBwKdTRJhK4XZLOOxQ+lf45T0BOroMF1m4=";
    "https://github.com/colll78/plutarch-plutus"."b2379767c7f1c70acf28206bf922f128adc02f28" = "sha256-mhuW2CHxnc6FDWuMcjW/51PKuPOdYc4yxz+W5RmlQew=";
    "https://github.com/input-output-hk/catalyst-onchain-libs"."650a3435f8efbd4bf36e58768fac266ba5beede4" = "sha256-NUh+l97+eO27Ppd8Bx0yMl0E5EV+p7+7GuFun1B8gRc=";
  };

  modules = [{ }];

  cabalProject = pkgs.haskell-nix.cabalProject' {
    inherit modules sha256map;
    src = ../.;
    name = "smart-tokens-plutarch";
    compiler-nix-name = "ghc966";
    # index-state = "2024-10-16T00:00:00Z";
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
