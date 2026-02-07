# FScript Syntax and Indentation Specification

## Purpose
This document describes the concrete syntax accepted by the interpreter and the layout (indentation/newline) rules used by the lexer/parser.

## File and program structure
- A program is a sequence of top-level statements.
- Top-level statements are:
  - `type` / `type rec` declarations
  - `let` / `let rec` bindings
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
- Conditionals:
  - `if cond then a else b`
  - `elif` desugars to nested `if`
- For loop:
  - `for x in listExpr do bodyExprOrBlock`
- Match:
  - `match expr with | ...`
  - record patterns in cases: `{ Field = pattern; ... }`
- Records:
  - literal `{ Name = "a"; Age = 1 }`
  - field access `p.Name`
  - copy-update `{ p with Age = 2 }`
- Lists:
  - `[a; b; c]`
  - range `[a..b]`
  - `::`, `@`
- Tuples:
  - `(a, b, c)`
- Options:
  - `Some x`, `None`
- Other:
  - `raise "message"`
  - `typeof Name`
  - interpolated strings `$"hello {name}"`

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

### Match-case alignment
- Multiline `match` case lines must all start at the same column.
- First case column must be at or deeper than the `match` column.

### Type declaration field alignment
- `type`/`type rec` record declarations support:
  - single-line `{ A: int; B: string }`
  - multiline with newline-separated fields
- In multiline type declarations, field-start columns align exactly.

## Type declaration forms
- Non-recursive:
  - `type Person = { Name: string; Age: int }`
- Explicit recursive:
  - `type rec Node = { Value: int; Next: Node option }`
