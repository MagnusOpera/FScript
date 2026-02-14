# FScript Map Matching Reference

## Purpose
This document is a focused reference for `match` patterns on maps.

## Map pattern basics
Map patterns use brace + keyed clauses:

```fsharp
match values with
| { ["name"] = v } -> v
| _ -> ""
```

Keys must be `string`.

## Supported forms

### Single key
```fsharp
| { ["a"] = x } -> ...
```

### Multiple keys
```fsharp
| { ["a"] = x; ["b"] = y } -> ...
```

### Tail capture
```fsharp
| { ["a"] = x; ..rest } -> ...
```

### Dynamic key extraction
```fsharp
| { [k] = v; ..rest } -> ...
```

### Guarded key checks
Use `when` for key equality checks (instead of pinning syntax):

```fsharp
| { [current] = v; ..rest } when current = key -> ...
```

## Partial matching semantics
Map matching is partial:
- a pattern only requires the listed keys to exist,
- additional keys in the matched value are allowed.

Example:

```fsharp
match values with
| { ["name"] = name } -> $"name={name}"
| _ -> "no match"
```

This still matches if `values` has extra keys.

## Practical examples

### Remove one key
```fsharp
let remove key values =
  match values with
  | { [current] = _; ..rest } when current = key -> rest
  | _ -> values
```

### Match two required keys
```fsharp
let describe values =
  match values with
  | { ["name"] = name; ["owner"] = owner } -> $"{name} by {owner}"
  | _ -> "unknown"
```

## Notes
- Map literals:
  - `{}` empty map
  - `{ ["k"] = v }`
  - `{ ["k"] = v; ..tail }`
- For map construction/update rules, see [`supported-types.md`](./supported-types.md).
- For full syntax/layout rules, see [`syntax-and-indentation.md`](./syntax-and-indentation.md).
