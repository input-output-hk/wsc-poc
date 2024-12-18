{
  description = "Smart Tokens";

  inputs = {
    iogx = {
      url = "github:input-output-hk/iogx";
      inputs.hackage.follows = "hackage";
      inputs.CHaP.follows = "CHaP";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    iohk-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

    plutarch = {
      url = "github:colll78/plutarch-plutus/b2379767c7f1c70acf28206bf922f128adc02f28";
    };
  };

  outputs = inputs: inputs.iogx.lib.mkFlake {
    inherit inputs;
    repoRoot = ./.;
    outputs = import ./nix/outputs.nix;
    systems = [ "x86_64-linux" ]; # "x86_64-darwin" ];
  };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };

}
