#!/usr/bin/env bash

set -x

VERSION=$(git describe --tags --always --dirty)

echo "Building version: ${VERSION}"

DOCKER_IMAGE_NAME=easy1staking/programmable-tokens-ui
DOCKER_IMAGE="${DOCKER_IMAGE_NAME}:${VERSION}"
DOCKER_IMAGE_LATEST="${DOCKER_IMAGE_NAME}:latest"

docker build -t "${DOCKER_IMAGE}" \
  -t "${DOCKER_IMAGE_LATEST}" \
  --build-arg NEXT_PUBLIC_API_URL=https://preview-api.programmabletokens.xyz \
  --build-arg NEXT_PUBLIC_BLOCKFROST_API_KEY=preview6rf9Lym3f9XQrTDnxSBbAGwvz5mNafdz \
  --build-arg NETWORK=Preview \
  --push \
  .
