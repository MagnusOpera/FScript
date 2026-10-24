# Compiler CLI

The compiler emits a versioned FScript LIR artifact, a Go or C# source project, or a standalone .NET assembly.

```text
fscript compile <program.fss> --target ir --output <directory>
fscript compile <program.fss> --target go --output <directory>
fscript compile <program.fss> --target csharp --output <directory>
fscript compile <program.fss> --target dotnet --output <directory>
```

The IR form writes `<directory>/<program-name>.fsir`. The Go form writes a project rooted at the requested directory, including `go.mod`, `main.go`, the shared specialized program at `internal/fscriptprogram/program.go`, and deterministic `module_<normalized-path>.go` files for contributing FScript sources.

All targets create missing parent directories and deterministically replace files owned by the selected output on a repeated invocation. Go generation also removes stale compiler-owned `internal/fscriptprogram/module_*.go` files when the source graph changes. An unknown target is rejected before the output directory is created. Filesystem failures are reported as `FSA5101` diagnostics.

Generated projects declare `go 1.25`, use no Go generics, and are already formatted for an empty `gofmt -d` result. Generation itself does not invoke Go; building or running the resulting project remains the caller's responsibility.

The C# target writes an independently buildable .NET 10 project. The .NET target writes `<program-name>.dll`, runtime configuration, and managed dependencies. Both support existing FFI providers selected through `fscript.toml` or `--extern-assembly`; `--reference` adds managed dependencies. Generated programs execute compiled methods without an interpreter.


For a self-contained executable, add `--single-file --runtime <rid>` to the .NET target. For a Native AOT executable, use `--aot --runtime <rid>`. Runtime identifiers use .NET names such as `osx-arm64`, `linux-x64`, and `win-x64`. Both options require a .NET 10 SDK for publishing and are rejected for other backends.

See [Compile to .NET or C#](./compile-to-dotnet.md) and the complete workflows in [Compile FScript to Go](./compile-to-go.md) and [Compile and Interpret FScript IR](./interpret-ir.md). The normative representations are documented in the [typed HIR](../specs/typed-hir.md) and [executable LIR](../specs/executable-lir.md) specifications.
