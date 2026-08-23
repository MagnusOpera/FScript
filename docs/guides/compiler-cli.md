# Compiler CLI

The compiler emits either a versioned FScript LIR artifact or a complete Go source project.

```text
fscript compile <program.fss> --target ir --output <directory>
fscript compile <program.fss> --target go --output <directory>
```

The IR form writes `<directory>/<program-name>.fsir`. The Go form writes a project rooted at the requested directory, including `go.mod`, `main.go`, `internal/fscriptprogram/program.go`, and the embedded runtime under `internal/fsruntime/`.

Both forms create missing parent directories and deterministically replace files owned by the selected output on a repeated invocation. An unknown target is rejected before the output directory is created. Filesystem failures are reported as `FSA5101` diagnostics.

Generated projects declare `go 1.25`, use no Go generics, and are already formatted for an empty `gofmt -d` result. Generation itself does not invoke Go; building or running the resulting project remains the caller's responsibility.
