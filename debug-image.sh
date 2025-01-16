#! /bin/bash

set -e

# Build the 'wst' image and load it as 'localhost/wsc-debug'
CONTAINER_TAR=oci-debug.tar

nix build --accept-flake-config .#containers.x86_64-linux.wst.copyTo
./result/bin/copy-to oci-archive:$CONTAINER_TAR

IMAGE_NAME=localhost/wsc-debug
podman rmi -f $IMAGE_NAME

IMAGE_HASH=$(podman load --quiet -i $CONTAINER_TAR | sed 's/.*sha256://')
rm $CONTAINER_TAR
IMAGE_TAG=latest
FULL_IMAGE=$IMAGE_NAME:$IMAGE_TAG

podman tag $IMAGE_HASH $FULL_IMAGE

podman run --rm -p 8080:8080 --env WST_BLOCKFROST_TOKEN=previewosqzsHuDVycDMX2UGlFLsuUTyJm1aWwp $FULL_IMAGE manage 08a8d0bb8717839931b0a594f7c28b0a3b7c78f6e9172e977e250eab7637d879.0 start