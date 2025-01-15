{ repoRoot, inputs, pkgs, lib, system }: let

frontendNpm = pkgs.buildNpmPackage rec {
  name = "frontend";
  src = ../frontend;
  npmDepsHash = "sha256-wtpNvgzcY0jpp5EgmWp4hXYfVs3xieq3Lb05tlhHlv4=";
  npmPackFlags = [ "--ignore-scripts" ];
  npmBuildScript = "export";
  installPhase = ''
    mkdir -p $out/frontend
    cp -r out/* $out/frontend
  '';
};

frontend = pkgs.buildEnv {
  name = "frontend";
  paths = [ frontendNpm ];
};

in rec {

  # Builds a docker container for the cabal executable given as input. First we
  # build the container json itself. Note the explicit architecture.
  #
  # NOTE: I don't think iogx.mkContainerFromCabalExe enables linking in the base image correctly. Hence the more manual construction below.
  # TODO: Consider patching iogx if that is the case?
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
  #$ podman run --publish 8080:8080 --env WST_BLOCKFROST_TOKEN=REPLACE_ME_APIKEY dir:./tmp manage 76e2cfb0b087873ef50a3f709fa6ab3df21bdd5b67c1254837cc353613524251.0 start --static-files /frontend
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
      # npm-created data for frontend
      (inputs.n2c.packages.nix2container.buildLayer {
        copyToRoot = [frontend];
      })
      # CA certificates for SSL (required for calling blockfrost API)
      (inputs.n2c.packages.nix2container.buildLayer {
        copyToRoot = [pkgs.dockerTools.caCertificates];
      })
    ];
  };


  wst-poc-mock-server = lib.iogx.mkContainerFromCabalExe {
    exe = inputs.self.packages.wst-poc-mock-server;
    name = "wst-poc-mock-server";
    description = "WST mockserver";
    packages = [ ];
    sourceUrl = "https://github.com/input-output-hk/wsc-poc";
  };


}

