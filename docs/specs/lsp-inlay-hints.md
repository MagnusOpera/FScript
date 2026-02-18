# LSP Inlay Hints Specification

## Purpose
This document specifies inlay hints produced by the FScript Language Server.

## Scope
Covered in this spec:
- parameter name hints at call sites,
- inferred type hints for bindings and parameters,
- inferred type hints for pattern-bound variables,
- configuration gate for inlay hints,
- injected function hover/signature formatting and stdlib definition navigation.

Not covered:
- semantic token classification,
- diagnostics.

## Hint categories

### Parameter name hints
Shown for function call arguments to clarify parameter intent.

Example labels:
- `x:`
- `y:`

### Type hints for bindings
Shown for inferred binding/parameter types when not explicitly annotated.

Example labels:
- `: int`
- `: string option`
- `: unknown`

### Type hints for pattern variables
Shown for variables introduced by match patterns.

Example:
```fsharp
match firstEven with
| Some x -> print $"{x}"
| None -> print "none"
```
`x` includes inlay label `: int`.

## Type label formatting

Formatting follows LSP-friendly type rendering rules:
- unknown inferred variable: `unknown`
- map value type when key domain is unresolved: `unknown map`

Examples:
- `: unknown map`

## Configuration

Inlay hints are controlled by language-server initialization option:
- `inlayHintsEnabled` (boolean)

Behavior:
- `true` (default): hints are produced.
- `false`: inlay hint endpoint returns no hints.

## Known limitations

- Hints are inference-driven; if inference is incomplete or blocked by errors, hints may be missing.
- Labels are concise and intentionally avoid full generic-style displays beyond FScript surface semantics.

## Injected Function Signature and Definition Behavior

- Runtime externs and stdlib prelude functions are injected into LSP typing/signature data.
- When injected parameter names are known, hover/signature displays named arrows:
  - `Option.map: (mapper: 't -> 'u) -> (value: 't option) -> 'u option`
- Go-to-definition on injected stdlib symbols resolves to virtual readonly documents:
  - `fscript-stdlib:///Option.fss`
  - `fscript-stdlib:///List.fss`
  - `fscript-stdlib:///Map.fss`

## Local Binding Definition Navigation

- Go-to-definition on a local variable usage resolves to the nearest lexical local declaration in scope.
- This includes function parameters, lambda parameters, local `let` bindings, and pattern-bound variables.
- Local definition navigation is resolved by lexical AST binding identity (usage span -> declaration span), with boundary-tolerant cursor matching.
- If a client immediately follows a successful definition request with a references request at the same cursor position, the server returns the resolved declaration location as a single-location references response.
- The immediate follow-up suppression does not apply when the cursor is on the declaration identifier itself; in that case, normal references enumeration is returned.

## LSP Debug Logging

- Verbose definition/references debug logging is controlled by initialization option `logLevel`.
- Debug logs are emitted only when `logLevel = "debug"` (mirrored in VS Code as `fscript.server.logLevel`).

## Feature Toggles

- LSP inlay hints are controlled by initialization option `inlayHintsEnabled` (default `true`).
- LSP hover hints are controlled by initialization option `hoverHintsEnabled` (default `true`).
## Related specifications

- Syntax and indentation: [`syntax-and-indentation.md`](./syntax-and-indentation.md)
- Supported types: [`supported-types.md`](./supported-types.md)
- Map matching: [`map-matching-reference.md`](./map-matching-reference.md)
