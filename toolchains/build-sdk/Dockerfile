ARG node_image=node:latest
ARG dotnet_image=mcr.microsoft.com/dotnet/sdk:latest
FROM ${node_image} AS node
FROM ${dotnet_image}
COPY --from=node /usr/local/ /usr/local/
ARG pnpm=latest
RUN apt-get update && apt-get install -y --no-install-recommends clang zlib1g-dev make git python3 \
    && rm -rf /var/lib/apt/lists/* \
    && npm install --global "pnpm@${pnpm}"
ENV DOTNET_NOLOGO=1 DOTNET_CLI_TELEMETRY_OPTOUT=1
