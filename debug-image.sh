#! /bin/bash

set -e

# Build the 'wst' image and load it as 'localhost/wsc-debug'

nix build --accept-flake-config .#containers.x86_64-linux.wst.copyTo
./result/bin/copy-to oci-archive:oci-debug.tar

IMAGE_NAME=localhost/wsc-debug
podman rmi -f $IMAGE_NAME

IMAGE_HASH=$(podman load --quiet -i oci-debug.tar | sed 's/.*sha256://')
IMAGE_TAG=latest
FULL_IMAGE=$IMAGE_NAME:$IMAGE_TAG

podman tag $IMAGE_HASH $FULL_IMAGE
