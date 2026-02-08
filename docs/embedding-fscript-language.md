# Embedding `FScript.Language`

This document describes the host-facing interface of `FScript.Language`:
- running the interpreter pipeline,
- discovering script functions,
- invoking functions,
- supported host-visible types and values.

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
- `TStringMap of Type`
- `TOption of Type`
- `TFun of Type * Type`
- `TNamed of string`
- `TUnion of string * Map<string, Type option>`
- `TTypeToken`
- `TVar of int`

Use `Types.typeToString` for display/diagnostics.

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
- `VStringMap of Map<string, Value>`
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
