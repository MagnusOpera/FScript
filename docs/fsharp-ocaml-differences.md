# FScript vs F# / OCaml: Major Differences

## Purpose
This document is a quick comparison guide. It highlights intentional differences between FScript and full F#/OCaml so users can port code with fewer surprises.

## Scope of the language
- FScript is a small interpreted subset, not a full compiler language.
- FScript supports a focused core: `let`, `let rec` (functions), `if/elif/else`, `for`, `match`, lists, tuples, options, records, pipelines, and extern calls.
- Many F#/OCaml features are intentionally absent today (modules, classes, custom ADTs/DU declarations, interfaces, computation expressions, etc.).

## Syntax differences

### `let ... in`
- F#/OCaml use `in` heavily for local bindings.
- FScript does not support `in`; it uses layout/indentation blocks instead.

Example:
```fsharp
// F#/OCaml style
let x =
  let y = 1 in
  y + 1
```

```fsharp
// FScript style
let x =
    let y = 1
    y + 1
```

### Indentation and layout strictness
- FScript is layout-aware and strict in specific places:
  - `match` case lines must align.
  - multiline record type fields in `type` declarations must align.
- F#/OCaml tooling/parsers are generally more permissive/flexible depending on construct and separators.

### Generic angle-bracket syntax
- F#/OCaml often use forms like `list<string>` (or equivalent library forms).
- FScript does not support angle-bracket generic syntax.
- Use postfix type syntax:
  - `string list`
  - `int option`
  - `string map`

## Type-system differences

### Named record types vs structural records
- FScript uses structural records and also supports top-level named record declarations.
- In current behavior, declared record names can unify with matching record shapes (structural equivalence), including recursive records.

### Type annotations (optional, parameter-focused)
- FScript supports optional parameter annotations using F#-style syntax:
  - `let f (x: int) = ...`
  - `fun (x: int) -> ...`
- Return type annotations are not supported.
- Pattern-level type annotations are not supported.

### Recursive types
- Recursive record declarations must be explicit: `type rec Name = { ... }`.
- Mutual recursive type groups are not supported (`and` declarations are not available).

### User-declared ADTs / discriminated unions
- F#/OCaml allow rich custom sum types.
- FScript currently does not support user-declared union/DU types.
- Built-in option and list pattern forms are supported.

## Pattern matching differences
- FScript supports wildcard, literal, tuple, list-cons, option patterns, and record patterns in `match`.
- Record patterns are supported in `match` cases, not in `let` patterns.
- Pattern features are narrower than full F#/OCaml pattern systems.

## Runtime and ecosystem differences

### Compilation model
- F#/OCaml are compiled languages.
- FScript is interpreted (parse -> infer -> evaluate).

### Standard library model
- F#/OCaml ship large standard libraries.
- FScript has a smaller built-in surface and relies on host-provided externs (for example `List.*`, `Map.*`, `Option.*`, `Fs.*`, `Json.*`, `Xml.*`, `Regex.*`).

### Reflection-style type tokens
- FScript provides `typeof Name` tokens for host extern workflows (for example JSON/XML decode helpers).
- This is a focused runtime integration feature, not full .NET/OCaml runtime reflection.

## Practical porting checklist
- Replace `let ... in` with indented block bindings.
- Replace generic angle syntax with postfix syntax (`'a list`, `'a option`, `'a map`).
- Keep annotations on parameters only.
- Convert unsupported ADTs/modules/features into records + functions + `match` where possible.
- Check indentation alignment for `match` cases and multiline type fields.
