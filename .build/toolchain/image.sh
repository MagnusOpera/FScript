#!/usr/bin/env bash
set -euo pipefail

image=${1:?Expected image reference}
sdk=${2:?Expected .NET SDK version}
node=${3:?Expected Node version}
platform=${4:?Expected target platform}
mode=${FSCRIPT_TOOLCHAIN_MODE:-local}
case "$mode" in
    local|publish|pull) ;;
    *) echo "Unknown toolchain mode: $mode" >&2; exit 2 ;;
esac

if docker image inspect "$image" > /dev/null 2>&1; then
    if [[ "$mode" == publish ]]; then docker push "$image"; fi
    exit 0
fi

if [[ "$mode" != local ]]; then
    if docker pull "$image"; then exit 0; fi
    if [[ "$mode" == pull ]]; then
        echo "The build job must publish toolchain image $image before test shards start." >&2
        exit 1
    fi
fi

docker build --platform "$platform" --build-arg "DOTNET_SDK_VERSION=$sdk" --build-arg "NODE_VERSION=$node" --tag "$image" .
if [[ "$mode" == publish ]]; then docker push "$image"; fi
