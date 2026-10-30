workspace {
    engine = ~docker
    version = "0.205.0"
    id = "6ae115ba87040d459b6ccf40"
}

# Shared inventory for container toolchains and optional host release tools.
# The workspace.version above also pins the native Terrabuild installer.
locals {
    versions = {
        dotnet_sdk: "10.0.401"
        node: "22"
        pnpm: "10.33.0"
        vsce: "4.0.0"
        ovsx: "0.9"
    }
}

phase toolchains { }
phase application {
    depends_on = [ phase.toolchains ]
}

target toolchain { }

# All application commands execute in the same pinned toolchain container.
extension @shell {
    platform = "linux/${terrabuild.arch}"
    image = "fscript-build:${project.build_sdk.version}"
}

# Bootstrapping the image uses the host Docker daemon, with no application tools.
extension @bootstrap {
    script = "toolchains/build-sdk/bootstrap.fss"
}

target build {
    lock = "fscript-solution-build"
    phase = phase.application
    artifacts = ~managed
}

target test {
    phase = phase.application
    depends_on = [ target.build target.^build target.materialize target.^materialize ]
    artifacts = ~managed
    batch = ~never
}

target check {
    phase = phase.application
    build = ~always
}

target sdk { phase = phase.application }

# Utility tasks execute each time; their filesystem effects stay in the workspace.
target smoke {
    phase = phase.application
    depends_on = [ target.build target.materialize ]
    build = ~always
    artifacts = ~none
}

target javascript_build { phase = phase.application build = ~always }
target install { phase = phase.application build = ~always }
target typecheck { phase = phase.application build = ~always }

target materialize { phase = phase.application }
