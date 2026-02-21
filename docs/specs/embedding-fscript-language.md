# Embedding `FScript.Language`

This document describes the host-facing interface of `FScript.Language`:
- running the interpreter pipeline,
- discovering script functions,
- invoking functions,
- supported host-visible types and values.

Note on CLI host behavior:
- The `fscript` CLI injects `let Env` into script execution.
- The `Environment` type is provided by stdlib.
- That injection is a CLI host convenience, not part of the `FScript.Language` core API contract.
- Hosts can provide filesystem denied glob patterns through `HostContext.DeniedPathGlobs`.

## Namespace and entry points

Reference `MagnusOpera.FScript.Language` and use namespace `FScript.Language`.

Primary entry points are in module `FScript`:

- `FScript.parse : string -> Program`
- `FScript.infer : Program -> TypeInfer.TypedProgram`
- `FScript.inferWithExterns : ExternalFunction list -> Program -> TypeInfer.TypedProgram`
- `FScript.eval : TypeInfer.TypedProgram -> Value`
- `FScript.evalWithExterns : ExternalFunction list -> TypeInfer.TypedProgram -> Value`
- `FScript.run : string -> Value` (parse + infer + eval without externs)

## Running scripts

```fsharp
open FScript.Language

let source = "let inc x = x + 1\ninc 41"
let result = FScript.run source

match result with
| VInt n -> printfn "Result: %d" n
| other -> printfn "Unexpected value: %s" (Pretty.valueToString other)
```

## Running with external functions

Use `inferWithExterns` and `evalWithExterns` when the script needs host functions.

```fsharp
open FScript.Language

let toUpperExtern =
    { Name = "toUpper"
      Scheme = Forall([], TFun(TString, TString))
      Arity = 1
      Impl =
        function
        | [ VString s ] -> VString (s.ToUpperInvariant())
        | _ -> raise (EvalException { Message = "toUpper expects string"; Span = Span.mk (Span.pos 0 0) (Span.pos 0 0) }) }

let source = "toUpper \"fscript\""
let program = FScript.parse source
let typed = FScript.inferWithExterns [ toUpperExtern ] program
let result = FScript.evalWithExterns [ toUpperExtern ] typed
```

## Loading once and invoking by name

Use `FScript.Runtime.ScriptHost` when a host needs reusable loading and direct function invocation.
Only top-level exported bindings are exposed through this API.

```fsharp
open FScript.Language
open FScript.Runtime

let externs = Registry.all { RootDirectory = "."; DeniedPathGlobs = [] }
let loaded = ScriptHost.loadSource externs "[<export>] let add x y = x + y"
let result = ScriptHost.invoke loaded "add" [ VInt 1L; VInt 2L ]
```

## Getting function metadata

After inference, function descriptors can be produced with `Descriptor.describeFunctions`.

```fsharp
open FScript.Language

let src = "let add x y = x + y\nlet rec fact n = if n = 0 then 1 else n * fact (n - 1)"
let typed = src |> FScript.parse |> FScript.infer

let functions =
    Descriptor.describeFunctions typed Map.empty

for f in functions do
    let args = f.Parameters |> List.map Types.typeToString |> String.concat " -> "
    printfn "%s : %s -> %s" f.Name args (Types.typeToString f.ReturnType)
```

`FunctionDescriptor` exposes:
- `Name`
- `Parameters` (`Type list`)
- `ReturnType` (`Type`)
- `Scheme` (`Scheme option`)
- `IsRecursive`
- `Span`

## Invoking functions

Function invocation is expression-based: evaluate a script where the target function is called.

```fsharp
open FScript.Language

let src = "let add x y = x + y\nadd 1 2"
let value = FScript.run src
```

You can also evaluate to a function value:

```fsharp
open FScript.Language

let value = FScript.run "let add1 x = x + 1\nadd1"

match value with
| VClosure _ -> printfn "Function value returned"
| _ -> printfn "Not a function"
```

## Using `nameof` for exported capability keys

`nameof` returns the bound identifier name as a string and type-checks that the identifier exists.

```fsharp
open FScript.Language

let src = "let run_step x = x\nnameof run_step"
let value = FScript.run src
// value = VString "run_step"
```

## Supported types

Host-visible static types are represented by `Type`:

- `TUnit`
- `TInt`
- `TFloat`
- `TBool`
- `TString`
- `TList of Type`
- `TTuple of Type list`
- `TRecord of Map<string, Type>`
- `TMap of Type * Type`
- `TOption of Type`
- `TFun of Type * Type`
- `TNamed of string`
- `TUnion of string * Map<string, Type option>`
- `TTypeToken`
- `TVar of int`

Use `Types.typeToString` for display/diagnostics.
The postfix script type syntax `'a map` corresponds to `TMap(TString, 'a)`.

## Runtime values

Evaluation returns `Value`:

- `VUnit`
- `VInt of int64`
- `VFloat of float`
- `VBool of bool`
- `VString of string`
- `VList of Value list`
- `VTuple of Value list`
- `VRecord of Map<string, Value>`
- `VMap of Map<MapKey, Value>`
- `VOption of Value option`
- `VUnionCase of string * string * Value option`
- `VUnionCtor of string * string`
- `VTypeToken of Type`
- `VClosure of string * Expr * Env ref`
- `VExternal of ExternalFunction * Value list`

Use `Pretty.valueToString` for a readable string form.

## Errors

`FScript.Language` raises typed exceptions with source spans:

- `ParseException` (`ParseError`)
- `TypeException` (`TypeError`)
- `EvalException` (`EvalError`)

Each error contains:
- `Message`
- `Span` (`Line`/`Column` information)

## Embedding cookbook

### 1. Parse/eval a file with `import`
Use import-aware parsing when executing scripts from disk.

```fsharp
open FScript.Language

let root = "/path/to/workspace"
let entry = "/path/to/workspace/main.fss"

let program = FScript.parseFileWithIncludes root entry
let typed = FScript.infer program
let value = FScript.eval typed
```

### 2. Load once, invoke many times
Use `ScriptHost` when you want to invoke exported functions repeatedly.

```fsharp
open FScript.Language
open FScript.Runtime

let hostContext = { RootDirectory = "."; DeniedPathGlobs = [] }
let externs = Registry.all hostContext

let loaded =
    ScriptHost.loadFile externs "./script.fss"

let r1 = ScriptHost.invoke loaded "run" [ VString "build" ]
let r2 = ScriptHost.invoke loaded "run" [ VString "test" ]
```

### 3. Load an in-memory script with `import`
Use resolver-backed loading when the entry script source is in memory and imports must be resolved by the host.

```fsharp
open System.IO
open FScript.Language
open FScript.Runtime

let root = "/virtual/workspace"
let entryFile = Path.Combine(root, "main.fss")
let entrySource = "import \"shared.fss\" as Shared\n[<export>] let run x = Shared.add1 x"

let embeddedSources =
    Map [
        Path.Combine(root, "shared.fss"), "let add1 x = x + 1"
    ]

let resolver path = embeddedSources |> Map.tryFind path

let externs = Registry.all { RootDirectory = root; DeniedPathGlobs = [] }
let loaded =
    ScriptHost.loadSourceWithIncludes externs root entryFile entrySource resolver

let result = ScriptHost.invoke loaded "run" [ VInt 41L ]
```

### 4. Add one custom extern
Define a typed external function and pass it to inference/evaluation.

```fsharp
open FScript.Language

let envExtern =
    { Name = "Env.get"
      Scheme = Forall([], TFun(TString, TOption TString))
      Arity = 1
      Impl =
        function
        | [ VString key ] ->
            System.Environment.GetEnvironmentVariable(key)
            |> Option.ofObj
            |> VOption
        | _ ->
            raise (EvalException { Message = "Env.get expects one string"; Span = Span.mk (Span.pos 0 0) (Span.pos 0 0) }) }

let src = "Env.get \"HOME\""
let program = FScript.parse src
let typed = FScript.inferWithExterns [ envExtern ] program
let value = FScript.evalWithExterns [ envExtern ] typed
```

### 5. Export descriptor discovery
After inference, get function descriptors to expose callable surface in hosts.

```fsharp
open FScript.Language

let typed = "[<export>] let run x = x" |> FScript.parse |> FScript.infer
let descriptors = Descriptor.describeFunctions typed Map.empty
```
