# FScript Syntax and Indentation Specification

## Purpose
This document describes the concrete syntax accepted by the interpreter and the layout (indentation/newline) rules used by the lexer/parser.

## File and program structure
- A program is a sequence of top-level statements.
- Top-level statements are:
  - `#include "relative/path/file.fss"` directives
  - `type` / `type rec` declarations
  - `let` / `let rec` bindings
  - `[<export>] let` / `[<export>] let rec` bindings
  - expressions
- Parsing is layout-aware: the lexer emits `Indent`/`Dedent` tokens from leading whitespace.
- Comments use `//` line-comment syntax.

## Core expression forms
- Literals: `int`, `float`, `bool`, `string`, `()`
- Variables and function application by whitespace:
  - `f x y`
- Lambda:
  - `fun x -> expr`
  - `fun (x: int) -> expr`
- Let expression (layout style):
  - `let x = expr`
  - `let rec f x = ... and g y = ...`
  - nested via blocks
  - `[<export>]` is only valid on top-level `let` bindings
  - attribute names are case-sensitive (`[<export>]` is valid, `[<Export>]` is not)
- Conditionals:
  - `if cond then a else b`
  - `elif` desugars to nested `if`
- For loop:
  - `for x in listExpr do bodyExprOrBlock`
- Match:
  - `match expr with | ...`
  - case guards: `| pattern when condition -> expr`
  - record patterns in cases: `{ Field = pattern; ... }`
  - union case patterns in cases: `Case` and `Case pattern`
- Records:
  - literal `{ Name = "a"; Age = 1 }`
  - if fields start on the next line, `{` must be on its own line
  - multiline example:
  ```fsharp
  {
      Name = "a"
      Age = 1
  }
  ```
  - field access `p.Name`
  - copy-update `{ p with Age = 2 }`
- Maps:
  - empty `{}`
  - literal `{ ["a"] = 1; ["b"] = 2 }`
  - update/merge with spread `{ ["a"] = 1; ..tail }`
  - map entries always use bracketed keys (`[expr] = value`)
  - record entries use field assignments (`Field = value`)
  - when braces are empty (`{}`), the literal is a map
  - keys are bracketed expressions (`[expr]`) and must infer to `string`
  - if entries start on the next line, `{` must be on its own line
  - multiline example:
  ```fsharp
  {
      ["a"] = 1
      ["b"] = 2
  }
  ```
- Lists:
  - `[a; b; c]`
  - multiline examples:
  ```fsharp
  [1
   2
   3]
  ```
  ```fsharp
  [
    1
    2
    3
  ]
  ```
  - range `[a..b]`
  - `::`, `@` (list-only)
  - if elements start on the next line, `[` must be on its own line
- Tuples:
  - `(a, b, c)`
- Options:
  - `Some x`, `None`
- Discriminated unions:
  - constructors in expressions: `Case`, `Case payload`
- Other:
  - `raise "message"`
  - `typeof Name`
  - `nameof identifier`
  - interpolated strings `$"hello {name}"` (placeholders accept regular expressions, including string literals)

## Operator precedence
From highest to lowest:
1. `* / %`
2. `+ -`
3. `@`
4. `::`
5. `= < > <= >=`
6. `&&`
7. `||`
8. `|>`

## Indentation and layout rules

### General block behavior
- After constructs that accept block bodies (`let`, `fun`, `if/else`, `for`, etc.), a newline followed by increased indent opens a block.
- A block ends on matching dedent.

### Let parameter alignment
- Single-line function declarations remain valid:
  - `let f x y = expr`
- Multiline parameter declarations are valid when all continuation parameter lines are aligned to the same column:
```fsharp
let format_address (address: { City: string; Zip: int })
                   (name: string) = $"{address.City} ({name})"
```
```fsharp
let format_address (address: { City: string; Zip: int })
                   (name: string) =
    $"{address.City} ({name})"
```
- If a continuation parameter starts at a different column, parsing fails.

### Multiline lambda arguments in call chains
- Lambda bodies can be written on the next indented line inside parenthesized function arguments.
- After the closing `)`, pipeline continuation (for example `|>`) remains valid.
```fsharp
let main =
    [0..9]
    |> List.map (fun i ->
        i)
    |> List.iter print
```
```fsharp
let main =
    [0..9]
    |> List.map (fun i ->
        i |> fib |> fun x -> $"{x}")
    |> List.iter print
```

### Match-case alignment
- Multiline `match` case lines must all start at the same column.
- First case column must be at or deeper than the `match` column.

### Type declaration field alignment
- `type`/`type rec` record declarations support:
  - single-line `{ A: int; B: string }`
  - multiline with newline-separated fields
- In multiline type declarations, field-start columns align exactly.

### Map layout examples
All of the following map literal layouts are valid:

```fsharp
{ ["format_address"] = "address formatting"; ["make_office_address"] = "office address constructor" }
```

```fsharp
{ ["format_address"] = "address formatting"
  ["make_office_address"] = "office address constructor" }
```

```fsharp
{
  ["format_address"] = "address formatting"
  ["make_office_address"] = "office address constructor"
}
```


### Multiline union declaration layout
- `type`/`type rec` union declarations support:
  - single-line: `type Shape = | Point | Circle of int`
  - multiline:
    - `type Shape =`
    - indented case lines starting with `|`
- The multiline union block ends on dedent.

## Type declaration forms
- Non-recursive:
  - `type Person = { Name: string; Age: int }`
- Explicit recursive:
  - `type rec Node = { Value: int; Next: Node option }`
- Union:
  - `type Shape = | Point | Circle of int`
- Recursive union:
  - `type rec Tree = | Empty | Node of (int * Tree list)`

## Inline type annotations in parameters
- Parameter annotations accept inline structural record types:
  - `let format_address (address: { City: string; Zip: int }) = ...`
  - `fun (x: { Name: string; Tags: string list }) -> ...`
- Inline record annotation fields are `;`-separated in single-line form.

## Include directive
- Include uses preprocessor-style syntax:
  - `#include "shared/helpers.fss"`
- Includes are top-level only.
- Included files are merged into the same global namespace as the current script.
- Include loading is recursive.
- Cycles are fatal and reported as parse errors.
- File paths in includes must be `.fss`.
