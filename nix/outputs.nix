{ repoRoot, inputs, pkgs, lib, system }:
let
  project = repoRoot.nix.project;
  containers = repoRoot.nix.containers;
in
[
  ( project.flake )
  { inherit containers; }
]
