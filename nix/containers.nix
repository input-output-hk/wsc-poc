{ repoRoot, inputs, pkgs, lib, system }:
{

  # Builds a docker container for the cabal executable given as input. First we
  # build the container json itself. Note the explicit architecture.
  #
  # $ nix build .#containers.x86_64-linux.wst
  #
  wst = lib.iogx.mkContainerFromCabalExe {
    exe = inputs.self.packages.wst-poc-cli;
    name = "wst-poc";
    description = "WST Proof of Concept";
    packages = [ ];
    sourceUrl = "https://github.com/input-output-hk/wsc-poc";
  };

}

