# FScript

FScript is a minimal F#/ML-subset interpreter implemented in F#/.NET. It parses a single file, performs Hindley–Milner type inference, annotates the AST with types, and then evaluates the program.

## Supported Syntax and Features (v1)

### Bindings and Functions
- `let` bindings (top-level and nested)
- `let rec` for recursive function bindings (top-level and inside expressions)
- Function definitions via `let f x = ...`
- Lambdas via `fun x -> ...`
- Function application by whitespace: `f x`

### Expressions
- Literals: `int`, `float`, `bool`, `string`
- `if ... then ... else ...`
- `match ... with` using list/option patterns
- Lists: `[a; b; c]`, `::` (cons), `@` (append)
- Integer ranges: `[a..b]` (inclusive, auto-descending when `a > b`)
- Tuples: `(a, b, c)`
- Records: literals `{ First = "a"; Last = "b" }`, field access `p.First`, copy-update `{ p with Age = 2 }`
- Option values and matching: `Some x`, `None`, and pattern matching with `Some p` / `None`
- Pipeline operator: `x |> f` (equivalent to `f x`)
- Built-in discard function: `ignore` (`'a -> unit`)
- Operators with precedence:
  - `* / %`
  - `+ -`
  - `@`
  - `::`
  - `= < > <= >=`
  - `&& ||`
  - `|>`

### Patterns
- `_` wildcard
- Identifier bindings
- Literals
- `[]`
- `p1 :: p2`
- `Some p` / `None`

### Types
- `int`, `float`, `bool`, `string`
- `'a list`
- tuples `(t1 * t2 * ...)`
- `'a option`
- structural record types inferred from record literals
- `unit` (for empty blocks or programs with only `let` bindings)

### Immutability
- Lists, tuples, and records are immutable values in FScript.

### Notes and Limitations
- `let rec` is only supported for function bindings
- No mutually recursive `and` bindings for `let rec`
- No user-defined type declarations (`type ...`), including record/DU type definitions
- Discarding a non-`unit` expression is a type error unless explicitly piped to `ignore`
- No `List.*` or `printfn`
- Range endpoints must be `int` in v1
- Comments supported: `//` line comments only

## Usage

### Build
```bash
make build
```

### Test
```bash
make test
```

### Run
```bash
# Example
cat > /tmp/sample.fss <<'EOF'
let add x = fun y -> x + y
add 2 3
EOF

dotnet run --project /Users/pct/src/MagnusOpera/FScript/src/FScript.Cli -- /tmp/sample.fss
```

## Output
- The program result is the last top-level expression.
- If a program contains only `let` bindings, the result is `unit` and prints as `()`.
