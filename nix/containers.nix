{ repoRoot, inputs, pkgs, lib, system }: let

staticFilesDerivation = pkgs.stdenv.mkDerivation {
  name = "staticFiles";
  src = ../generated;
  unpackPhase = "true";
  installPhase = ''
    ls -alh "$src"
    mkdir -p "$out"
    cp -r $src/html $out
    ls -alh $out
  '';
};

staticFiles = pkgs.buildEnv {
  name = "staticFiles";
  paths = [
    # the actual payload we want
    staticFilesDerivation
    # allow interactivity with the image
    # NOTE: Uncomment the lines below if you need a shell inside the image
    # (for example when debugging the image contents)
    # pkgs.bashInteractive
    # pkgs.coreutils
  ];
  pathsToLink = [ "/html" "/bin" ];
  extraOutputsToInstall = [ "/html" ];
};

in rec {

  # Builds a docker container for the cabal executable given as input. First we
  # build the container json itself. Note the explicit architecture.
  #
  # NOTE: The following commands produce a nice test environment for the container
  # Build via nix first
  #
  #$ nix build --accept-flake-config .#containers.x86_64-linux.wst.copyTo
  #
  # Instead of generating a container, generate into a directory
  #
  #$ ./result/bin/copy-to dir:./tmp
  #
  # Now we can run the container (the tx is just some random I copied from the explorer)
  #$ podman run --publish 8080:8080 --env WST_BLOCKFROST_TOKEN=REPLACE_ME_APIKEY dir:./tmp manage 76e2cfb0b087873ef50a3f709fa6ab3df21bdd5b67c1254837cc353613524251.0 start --static-files /html
  #
  # NOTE: To build the oci container image run:
  #
  #$ ./result/bin/copy-to oci-archive:oci.tar
  #
  wst = inputs.n2c.packages.nix2container.buildImage {
    name = "wst";
    config = {
      Entrypoint = lib.singleton (lib.getExe inputs.self.packages.wst-poc-cli);
      Labels = {
        "org.opencontainers.image.source"      = "https://github.com/input-output-hk/wsc-poc";
        "org.opencontainers.image.description" = "Programmable token and regulated stablecoin proof-of-concept";
      };
    };
    layers = [
      (inputs.n2c.packages.nix2container.buildLayer {
        copyToRoot = [staticFiles];
      })
    ];
  };

  # NOTE: I don't think iogx.mkContainerFromCabalExe enables linking in the base image correctly. Hence the more manual construction above.
  # TODO: Consider patching iogx if that is the case?

  # Builds a docker container for the cabal executable given as input. First we
  # build the container json itself. Note the explicit architecture.
  #
  # $ nix build .#containers.x86_64-linux.wstBinary
  #
  # wstBinary = lib.iogx.mkContainerFromCabalExe {
  #   exe = inputs.self.packages.wst-poc-cli;
  #   name = "wst-poc";
  #   description = "WST Proof of Concept";
  #   # packages = [ staticFiles staticFilesDerivation ];
  #   sourceUrl = "https://github.com/input-output-hk/wsc-poc";
  # };

  wst-poc-mock-server = lib.iogx.mkContainerFromCabalExe {
    exe = inputs.self.packages.wst-poc-mock-server;
    name = "wst-poc-mock-server";
    description = "WST mockserver";
    packages = [ ];
    sourceUrl = "https://github.com/input-output-hk/wsc-poc";
  };


}

