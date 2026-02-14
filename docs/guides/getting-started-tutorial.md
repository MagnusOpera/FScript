# FScript Getting Started Tutorial

## Goal
This tutorial helps you go from zero to writing useful FScript scripts.

You will learn:
- how to run a script,
- how to model data with records/lists/maps/options/unions,
- how to use pattern matching,
- how to split scripts with `import`,
- how to expose host-callable functions with `[<export>]`.

## Get FScript

### Install with Homebrew
```bash
brew install magnusopera/tap/fscript
```

Then run scripts with:

```bash
fscript your-script.fss
```

### Build from source
```bash
git clone https://github.com/MagnusOpera/FScript.git
cd FScript
make build
```

Then run scripts with:

```bash
./src/FScript/bin/Debug/net10.0/fscript your-script.fss
```

## 1. Run your first script
Create `hello.fss`:

```fsharp
let name = "FScript"
print $"Hello {name}"
```

Run it:

```bash
fscript hello.fss
```

## 2. Bindings and expressions
FScript is expression-first and immutable.
FScript is also indentation-based: layout defines blocks.

```fsharp
let a = 40
let b = 2
let result = a + b
print $"result = {result}"
```

Short block example:

```fsharp
let describe x =
  if x > 0 then
    "positive"
  else
    "zero-or-negative"
```

Pipelines are supported:

```fsharp
let text =
  "fscript"
  |> fun x -> $"{x}-lang"

print text
```

## 3. Basic types

FScript supports the following scalar/basic types:
- `string`
- `int`
- `float`
- `unit` (`()`)
- `bool`

```fsharp
let projectName = "fscript"   // string
let retries = 3               // int
let ratio = 0.75              // float
let enabled = true            // bool
let nothing = ()              // unit
```

## 4. Flow control

FScript provides expression-based control flow:
- `if ... then ... elif ... else ...`
- `for ... in ... do ...`
- `match ... with ...`

### if / elif / else
```fsharp
let classify n =
  if n < 0 then
    "negative"
  elif n = 0 then
    "zero"
  else
    "positive"
```

### for ... in ... do
```fsharp
for value in [1; 2; 3] do
  print $"{value}"
```

### match
```fsharp
let describe value =
  match value with
  | Some x -> $"some:{x}"
  | None -> "none"
```

## 5. Core data types

### List
```fsharp
let numbers = [1; 2; 3]
let doubled = numbers |> List.map (fun n -> n * 2)
print $"{doubled}"
```

### Option
```fsharp
let maybeName = Some "Ada"
let label = maybeName |> Option.defaultValue "unknown"
print label
```

### Tuple
```fsharp
let pair = ("pkg", 3)
```

### Record
```fsharp
type Project = { Name: string; Version: int }
let p = { Name = "core"; Version = 1 }
print p.Name
```

### Map
```fsharp
let scores = { ["math"] = 18; ["science"] = 20 }
let maybeMath = scores |> Map.tryGet "math"
print $"{maybeMath}"
```

Map keys are `string`:

```fsharp
let status = { ["200"] = "ok"; ["404"] = "not-found" }
```

### Discriminated union
```fsharp
type Result =
  | Ok of string
  | Error of string
```

Usage:

```fsharp
let success = Ok "done"
let failure = Error "failed"
```

## 6. Pattern matching
Pattern matching is central in FScript.

### Option
```fsharp
let readName value =
  match value with
  | Some name -> name
  | None -> "missing"
```

### List
```fsharp
let describe items =
  match items with
  | head :: tail -> $"head={head}, tail-size={List.length tail}"
  | _ -> "empty"
```

### Tuple
```fsharp
let describePair pair =
  match pair with
  | ("ok", code) -> $"success:{code}"
  | (kind, code) -> $"{kind}:{code}"
```

### Record
```fsharp
let cityLabel address =
  match address with
  | { City = c } -> c
```

### Map
```fsharp
let removeKey key values =
  match values with
  | { [current] = _; ..tail } when current = key -> tail
  | _ -> values
```

### Discriminated union
```fsharp
let statusMessage result =
  match result with
  | Ok value -> $"ok: {value}"
  | Error message -> $"error: {message}"
```

### Guards with `when`
Use `when` to add a boolean condition to a case.

```fsharp
let classify n =
  match n with
  | x when x < 0 -> "negative"
  | x when x = 0 -> "zero"
  | _ -> "positive"
```

## 7. Partial matching (record and map)
Record and map patterns are partial in FScript.
You can match only the fields/keys you care about; extra entries are allowed.

### Record

```fsharp
type User = { Name: string; Role: string; Team: string }

let label user =
  match user with
  | { Name = n; Role = "admin" } -> $"admin:{n}"
  | { Name = n } -> $"user:{n}"
```

### Map

```fsharp
let metadata =
  { ["name"] = "core"
    ["owner"] = "platform"
    ["tier"] = "gold" }

let summary values =
  match values with
  | { ["name"] = name; ["owner"] = owner } -> $"{name} by {owner}"
  | _ -> "unknown"
```

You can also capture the remaining map entries:

```fsharp
let removeName values =
  match values with
  | { ["name"] = _; ..rest } -> rest
  | _ -> values
```

## 8. Functions and recursion
Curried functions are the default.

```fsharp
let add x y = x + y
let inc = add 1
print $"{inc 41}"
```

Recursive functions use `let rec`:

```fsharp
let rec fib n =
  if n < 2 then n
  else fib (n - 1) + fib (n - 2)
```

## 9. Stdlib
FScript ships with a preloaded stdlib focused on functional collection workflows.

Common families:
- `List.*`
- `Option.*`
- `Map.*`

Examples:

```fsharp
let values = [1; 2; 3]
let doubled = values |> List.map (fun n -> n * 2)

let chosen = Some 42 |> Option.defaultValue 0

let m = { ["a"] = 1; ["b"] = 2 }
let hasA = m |> Map.containsKey "a"
```

Full reference:
- [`docs/specs/stdlib-functions.md`](../specs/stdlib-functions.md)

## 10. Imports and file modules
You can split scripts using `import`.

`main.fss`:

```fsharp
import "shared/math.fss" as Math
print $"{Math.sum 20 22}"
```

`shared/math.fss`:

```fsharp
let sum a b = a + b
```

Notes:
- imported files are `.fss`,
- import cycles are fatal,
- imported symbols must be accessed through the explicit alias (`Math.sum`, `Common.join`, ...).

## 11. Hosting, exports, and sandboxing (advanced)
FScript is designed to be embedded.
At host boundary, scripts can expose explicit entry points while the host controls capabilities and security.

Mark top-level bindings with `[<export>]` when a host must discover them:

```fsharp
[<export>] let dispatch (context: { Command: string }) =
  [{ Command = "echo"; Arguments = context.Command; ErrorLevel = 0 }]

// Export descriptor map
{
  [nameof dispatch] = [Dispatch; Never]
}
```

Security/hosting teaser:
- core FScript evaluation is pure in-memory computation,
- side effects exist only through host-exposed externs,
- sandboxing is enforced by host decisions (filesystem root, allowed functions, execution controls).

When embedding, keep this mindset:
- expose only needed externs,
- keep filesystem/network boundaries explicit,
- use host-level timeout/cancellation/resource limits.
- see [`docs/specs/embedding-fscript-language.md`](../specs/embedding-fscript-language.md) for the embedding API,
- see [`docs/specs/external-functions.md`](../specs/external-functions.md) for extern design/registration,
- see [`docs/specs/sandbox-and-security.md`](../specs/sandbox-and-security.md) for the full security model.

## 12. Next steps
- Sample scripts:
  - [`samples/types-showcase.fss`](../../samples/types-showcase.fss)
  - [`samples/patterns-and-collections.fss`](../../samples/patterns-and-collections.fss)
  - [`samples/map-matching.fss`](../../samples/map-matching.fss)
  - [`samples/imports-and-exports.fss`](../../samples/imports-and-exports.fss)
- Specifications index: [`docs/specs/README.md`](../specs/README.md)
- Architecture index: [`docs/architecture/README.md`](../architecture/README.md)
- [`samples/`](../../samples/)
