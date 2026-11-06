workspace {
    id = "6ae115ba87040d459b6ccf40"
    engine = ~docker
    ignores = [ "**/bin" "**/obj" "**/node_modules" "**/TestResults"
                "**/.fscript" "**/.docusaurus" ".out" "out" ".terrabuild" ]
}

phase toolchains { }
phase application { depends_on = [ phase.toolchains ] }

variable config { default = "Release" }

locals {
    versions = {
        dotnet_sdk: "10.0.403"
        node: "22"
        terrabuild: "latest"
        vsce: "4.0.0"
        ovsx: "0.9"
    }
}

target install {
    phase = phase.application
    build = ~lazy
    artifacts = ~workspace
}

target build {
    batch = ~never
    phase = phase.application
    artifacts = ~managed
    depends_on = [ target.install target.^build ]
}

target test {
    batch = ~never
    phase = phase.application
    artifacts = ~managed
    depends_on = [ target.build ]
}

target smoke {
    phase = phase.application
    artifacts = ~managed
    depends_on = [ target.build target.^build ]
}

target sdk {
    phase = phase.application
    artifacts = ~managed
    depends_on = [ target.build target.^build ]
}

target reports {
    phase = phase.application
    build = ~always
    artifacts = ~none
    depends_on = [ target.^test ]
}

# Static CI entry points consume only their selected test outputs.
target ci {
    phase = phase.application
    build = ~always
    artifacts = ~none
    depends_on = []
}

target test_ffi_managed {
    phase = phase.application
    artifacts = ~managed
    depends_on = [ target.build ]
}

target test_ffi_default {
    phase = phase.application
    artifacts = ~managed
    depends_on = [ target.build ]
}

target test_ffi_size {
    phase = phase.application
    artifacts = ~managed
    depends_on = [ target.build ]
}

extension @dotnet {
    image = "fscript-build:${project.toolchain.version}"
    platform = "linux/${terrabuild.arch}"
    defaults { configuration = var.config }
}

extension @npm {
    image = "fscript-build:${project.toolchain.version}"
    platform = "linux/${terrabuild.arch}"
    env { CI = "true" }
}

extension workflow {
    script = ".build/workflow.fss"
    image = "fscript-build:${project.toolchain.version}"
    platform = "linux/${terrabuild.arch}"
    env { FSCRIPT_BUILD_CONFIG = var.config }
}
