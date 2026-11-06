FROM node:22-bookworm-slim AS node
FROM mcr.microsoft.com/dotnet/sdk:10.0.401

# AOT integration tests invoke the native compiler. Node is also needed by the
# debugger tests, Fable package, VS Code extension, and documentation build.
RUN apt-get update \
    && apt-get install -y --no-install-recommends clang zlib1g-dev git ca-certificates \
    && rm -rf /var/lib/apt/lists/*
COPY --from=node /usr/local/bin/node /usr/local/bin/node
COPY --from=node /usr/local/lib/node_modules /usr/local/lib/node_modules
RUN ln -s ../lib/node_modules/npm/bin/npm-cli.js /usr/local/bin/npm \
    && ln -s ../lib/node_modules/npm/bin/npx-cli.js /usr/local/bin/npx
ENV DOTNET_CLI_TELEMETRY_OPTOUT=1 DOTNET_NOLOGO=1 NPM_CONFIG_AUDIT=false NPM_CONFIG_FUND=false

# Tests capture subprocess output. Persistent SDK/compiler servers inherit those
# pipes and can keep ReadToEnd waiting after the child command has exited.
ENV MSBUILDDISABLENODEREUSE=1 DOTNET_CLI_USE_MSBUILD_SERVER=0 UseSharedCompilation=false
