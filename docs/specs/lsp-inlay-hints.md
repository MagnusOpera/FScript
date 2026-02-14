# LSP Inlay Hints Specification

## Purpose
This document specifies inlay hints produced by the FScript Language Server.

## Scope
Covered in this spec:
- parameter name hints at call sites,
- inferred type hints for bindings and parameters,
- inferred type hints for pattern-bound variables,
- configuration gate for inlay hints.

Not covered:
- hover rendering,
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
- map key-domain variable constrained by language map rules: `int|string`

Examples:
- `: int|string`
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

## Related specifications

- Syntax and indentation: [`syntax-and-indentation.md`](./syntax-and-indentation.md)
- Supported types: [`supported-types.md`](./supported-types.md)
- Map matching: [`map-matching-reference.md`](./map-matching-reference.md)
