# FScript

FScript is a minimal F#/ML-subset interpreter implemented in F#/.NET. It parses a single file, performs Hindley–Milner type inference, annotates the AST with types, and then evaluates the program.

## Supported Syntax and Features

### Bindings and Functions
- `let` bindings (top-level and nested)
- `let rec` for recursive function bindings (top-level and inside expressions)
- Function definitions via `let f x = ...`
- Lambdas via `fun x -> ...`
- Optional parameter type annotations: `let f (x: int) = ...`, `fun (x: int) -> ...`
- Function application by whitespace: `f x`

### Expressions
- Literals: `int`, `float`, `bool`, `string`
- Unit literal: `()`
- `if ... then ... else ...`
- `if ... then ... elif ... then ... else ...` (`elif` is sugar for nested `else if`)
- `for x in <list> do <expr-or-block>`
- `match ... with` using list/option patterns
- Lists: `[a; b; c]`, `::` (cons), `@` (append)
- Integer ranges: `[a..b]` (inclusive, auto-descending when `a > b`)
- Tuples: `(a, b, c)`
- Records: literals `{ First = "a"; Last = "b" }`, field access `p.First`, copy-update `{ p with Age = 2 }`
- Option values and matching: `Some x`, `None`, and pattern matching with `Some p` / `None`
- String interpolation: `$"hello {name}"`
- `raise <stringExpr>` to abort execution with an eval error
- `typeof <TypeName>` to produce a type token value
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
- record patterns in `match`: `{ Field = pat; ... }` (subset field matching)
- `Some p` / `None`

### Types
- `int`, `float`, `bool`, `string`
- `'a list`
- tuples `(t1 * t2 * ...)`
- functions `(t1 -> t2)` in type annotations
- `'a option`
- `'a map` (string-keyed map)
- structural record types inferred from record literals
- top-level record type declarations: `type Name = { ... }` and `type rec Name = { ... }` (single-line or multiline with aligned fields)
- `unit` (for empty blocks or programs with only `let` bindings)

### Type Reflection and Decoding
- `typeof Name` produces a type token.
- `Json.deserialize (typeof Name) jsonText` can decode JSON into an `option` record value.

### Host Externals
- `print : string -> unit`
- `Map.empty`, `Map.add`, `Map.try`, `Map.tryFind`, `Map.containsKey`, `Map.remove`
- `List.map`, `List.choose`, `List.collect`, `List.contains`, `List.distinct`, `List.exists`, `List.fold`, `List.filter`, `List.iter`, `List.rev`, `List.length`, `List.tryFind`, `List.tryFindIndex`, `List.tryHead`, `List.tail`, `List.append`
- `Option.get`, `Option.defaultValue`, `Option.defaultWith`, `Option.isNone`, `Option.isSome`, `Option.map`
- `Hash.md5`, `Guid.new`, `Regex.matchGroups`, `Xml.values`, `Fs.glob`, `Fs.readText`, `Json.deserialize`

### Immutability
- Lists, tuples, and records are immutable values in FScript.

### Notes and Limitations
- `let rec` is only supported for function bindings
- No mutually recursive `and` bindings for `let rec`
- User-defined types are currently limited to top-level record declarations
- Recursive record types require `type rec` and mutual recursive type declarations are not yet supported
- `typeof` only accepts declared record type names
- Discarding a non-`unit` expression is a type error unless explicitly piped to `ignore`
- No `printfn`
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

dotnet run --project /Users/pct/src/MagnusOpera/FScript/src/FScript -- /tmp/sample.fss
```

## Output
- The program result is the last top-level expression.
- If a program contains only `let` bindings, the result is `unit` and prints as `()`.
