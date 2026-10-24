---
id: syntax-reference
title: Syntax and Operators
slug: /reference/syntax
---

This page indexes the script syntax. Start with the [Language Tour](../learn/language-tour) for a walkthrough. The [native types reference](./native-types-reference.md) covers data representations and the [signature index](./stdlib-reference.md) covers functions.

## Program forms

| Form | Purpose |
| --- | --- |
| `import "file.fss" as Alias` | Import before all executable/type code; use `Alias.member` |
| `type Name = { Field: int }` | Declare an ordinary record |
| `type Choice = \| First \| Second of string` | Declare union cases |
| `type rec Name = ...` | Permit self-references in a record or union declaration |
| `let name = expr` | Bind an immutable value |
| `let f x y = expr` | Declare a curried function |
| `let rec f x = ... and g y = ...` | Declare mutually recursive functions |
| `export let name = expr` | Expose a top-level binding to embedding hosts |
| `expr` | Evaluate a top-level expression |

`export` is contextual: `let export = 42`, parameters named `export`, and `export let export value = value` are valid. Only a top-level `export let` declaration sets export visibility; a recursive group exports all its members. Script attributes are unsupported.

Imports use explicit aliases, not `open` or a `module` declaration. Type declarations are top-level and support records/unions; generic type declarations, mutual recursive type groups, classes, and arbitrary type aliases are not script syntax.

## Expressions

| Form | Purpose |
| --- | --- |
| `42`, `3.14`, `true`, `"text"`, `()` | Integer, float, boolean, string, unit |
| `f x y` | Apply arguments from left to right: `(f x) y` |
| `fun x -> expr`, `fun () -> expr` | Anonymous function |
| `if condition then a elif other then b else c` | Select a branch; branch types must agree |
| `for item in values do body` | Iterate a list for effects; produces unit |
| `match value with \| pattern when guard -> body` | Choose the first matching case with a true guard |
| `raise "message"` | Stop evaluation with an error |
| `typeof Name` | Produce a runtime token for a known declared type |
| `nameof binding` | Produce the name of an existing binding as a string |
| `$"hello {expression}"` | Interpolate evaluated expressions; `{{` and `}}` produce literal braces |

A block contains local bindings and expressions and takes its value from the final expression. Local `let` bindings use indentation rather than `let ... in`. A unit parameter is an actual argument: call `ping ()`, not `ping`.

Matches are not checked for exhaustiveness. An unmatched value is a runtime error. There is no script exception handler; use options or domain unions for recoverable outcomes.

`typeof` is not CLR reflection. In `Json.deserialize (typeof Settings) text`, the token selects the runtime decoder; use a typed receiving function when the result contract matters. `nameof` checks that a binding exists and does not call it.

## Operators

Precedence is listed from highest to lowest. Parentheses override it. Field access/indexing and function application bind more tightly than these binary operators. Binary operators at the same precedence associate left to right.

| Precedence | Operators | Operands and result |
| --- | --- | --- |
| 7 | `*`, `/`, `%` | Numeric multiplication/division; `%` requires integers |
| 6 | `+`, `-` | Numeric addition/subtraction |
| 5 | `@` | Concatenate two lists of the same element type |
| 4 | `::` | Prepend one element to a list |
| 3 | `=`, `<`, `>`, `<=`, `>=` | Equality; ordered comparisons require matching numeric types |
| 2 | `&&` | Boolean conjunction |
| 1 | `\|\|` | Boolean disjunction |
| 0 | `\|>` | `value \|> f` applies `f value` |

Use `=` for equality; `==`, `!=`, and `<>` are unsupported. Express inequality with `if left = right then false else true`. Boolean operators evaluate **both operands**: use `if` when the second expression must be skipped. There is no built-in `not` function.

Arithmetic requires two integers or two floats, without implicit conversion. Integer division truncates toward zero. `+` does not join strings; use interpolation or `String.concat`. There is no unary negation form: write `0 - 1` or `0 - value` instead of `-1` or `-value`.

Equality recursively compares data values (including lists, tuples, options, records, maps, and union payloads). Host-managed values compare visible fields within the same host type, excluding private payloads. Function and task values do not have useful structural equality. `List.distinct` follows data equality; `List.contains` currently supports scalar membership only.

For multiple list prepends, write `first :: (second :: rest)` to make the grouping explicit.

## Literals and access

| Form | Meaning |
| --- | --- |
| `[a; b]`, `[]` | List literal |
| `[first..last]` | Inclusive integer range, ascending or descending by one |
| `(a, b)` | Tuple |
| `Some value`, `None` | Option cases |
| `{ Field = value }` | Ordinary record literal |
| `{\| Field = value \|}` | Structural record literal |
| `{ record with Field = value }` | Replace existing fields with compatible values |
| `{\| record with Field = value; Added = other \|}` | Update/add structural fields; existing field types must agree |
| `{ [key] = value; ..baseMap }`, `{}` | Map literal/spread; empty braces mean an empty map |
| `value.Field` | Read a visible record field |
| `values[indexOrKey]` | Optional list/map lookup |
| `Case`, `Case payload` | Declared union constructors |

An ordinary `{ Field = value }` literal must match exactly one declared record field set. Use `{| Field = value |}` for an ad-hoc record without a declaration. Host-managed opaque types must come from the host and reject both update forms. See [Host-Provided Types](../embedding/host-managed-types).

## Patterns and annotations

| Pattern | Meaning |
| --- | --- |
| `_`, `name` | Ignore or bind a value |
| Literal | Match the same literal |
| `()`, `(a, b)` | Match unit or destructure a tuple |
| `[]`, `[a; b]`, `head :: tail` | Match list shape |
| `Some item`, `None` | Match an option |
| `Case`, `Case payload` | Match a union case |
| `{ Field = pattern }` | Match selected visible record fields; extra fields are allowed |
| `{ ["key"] = value; ..rest }` | Match map entries and optionally bind the remaining map |
| `{ [key] = value; ..rest }` | Extract an entry with bound key/value; guards can select it |
| `{\| Field: int \|}` | Structural record-shape pattern |
| `{ Field: int }` | Pattern resolving a declared record type by shape |

Tuple patterns are also supported in nonrecursive `let` bindings: `let (left, right) = pair`. Record destructuring uses `match`; it is not a `let { ... }` form. See [Pattern Matching](../language/pattern-matching) for guards and [map matching](https://github.com/MagnusOpera/FScript/blob/main/docs/specs/map-matching-reference.md) for extraction rules.

Annotate parameters as `(value: Type)` and function returns as `let f x : Type = ...`. Return annotations require at least one parameter; `let value : int = 1` is unsupported. Type forms include scalars, `numeric`, named types, postfix `list`/`option`/`task`/`map[key]`, tuples, arrows, and both record annotation forms. See [Structural vs Named Annotations](../language/structural-vs-named-annotations).

## Text and layout

Use `//` for line comments. Strings support escaped quotes/backslashes and `\n`, `\r`, `\t`. Interpolation placeholders accept expressions, including quoted strings.

Indent block bodies consistently. Align multiline parameters and match cases. For nonempty list, record, and map literals, keep the opening delimiter beside the first item and the closing delimiter beside the last item; standalone delimiter lines are invalid. See [Indentation and Layout](../language/indentation-and-layout) for valid and invalid examples.
