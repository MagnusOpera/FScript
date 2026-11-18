#!/usr/bin/env bash
set -euo pipefail
image="$1"
dotnet_sdk="$2"
node="$3"
platform="$4"
# Avoid attested indexes whose child manifests were missing on fresh runners.
docker build --file Dockerfile --tag "$image" --provenance=false \
    --build-arg "DOTNET_SDK_VERSION=$dotnet_sdk" \
    --build-arg "NODE_VERSION=$node" --platform "$platform" .
if [[ "${FSCRIPT_PUBLISH_TOOLCHAIN:-false}" == true ]]; then
    docker push "$image"
fi
