# Compiler CLI

The compiler emits either a versioned FScript LIR artifact or a complete Go source project.

```text
fscript compile <program.fss> --target ir --output <directory>
fscript compile <program.fss> --target go --output <directory>
```

The IR form writes `<directory>/<program-name>.fsir`. The Go form writes a project rooted at the requested directory, including `go.mod`, `main.go`, the shared specialized program at `internal/fscriptprogram/program.go`, and deterministic `module_<normalized-path>.go` files for contributing FScript sources.

Both forms create missing parent directories and deterministically replace files owned by the selected output on a repeated invocation. Go generation also removes stale compiler-owned `internal/fscriptprogram/module_*.go` files when the source graph changes. An unknown target is rejected before the output directory is created. Filesystem failures are reported as `FSA5101` diagnostics.

Generated projects declare `go 1.25`, use no Go generics, and are already formatted for an empty `gofmt -d` result. Generation itself does not invoke Go; building or running the resulting project remains the caller's responsibility.

See the complete workflows in [Compile FScript to Go](./compile-to-go.md) and [Compile and Interpret FScript IR](./interpret-ir.md). The normative representations are documented in the [typed HIR](../specs/typed-hir.md) and [executable LIR](../specs/executable-lir.md) specifications.
