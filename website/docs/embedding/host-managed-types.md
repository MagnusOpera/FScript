---
id: host-managed-types
title: Host-Provided Types
slug: /embedding/host-managed-types
---

A host module can provide both ordinary data records that scripts construct and opaque values that only host code creates. Both travel through the existing typed extern bridge. The public attribute option `ScriptConstructible` controls construction; merely providing a type from a module imposes no ownership restriction.

Opaque projection is the default and can retain hidden resource state. Set `ScriptConstructible = true` explicitly for a fully visible F# data record.

## Register a type and a factory

A descriptor names the script type and its visible field types. The typed function helper handles argument decoding and result snapshots, including retention of opaque CLR payloads. An `ExtensionRegistration` groups descriptors with ordinary `ExternalFunction` records. This complete F# example registers a `Node` factory and passes its result to a typed host handler:

```fsharp
open System
open FScript.Extensibility

[<FScriptHostType("Node")>]
type Node =
    { [<FScriptVisible>]
      Value: int64
      Token: Guid }

let nodeDescriptor = HostTypes.describe<Node>()

let typeRegistration =
    { HostInterop.empty with Types = [ nodeDescriptor ] }

let makeNode =
    HostFunctions.create typeRegistration "Host.makeNode"
        (fun (value: int64) ->
            { Value = value; Token = Guid.NewGuid() })

let registration =
    { typeRegistration with Functions = [ makeNode ] }

// CLR conversion stays inside the SDK handler.
let inspect =
    HostFunctions.create typeRegistration "Host.inspect"
        (fun (node: Node) -> printfn "Value: %d; private token: %O" node.Value node.Token)
let environment = { registration with Functions = [makeNode; inspect] }
let loaded =
    FScript.Runtime.ScriptHost.loadSourceWithRegistration
        (FScript.Runtime.ExtensionBridge.toLanguageRegistration environment)
        "let node = Host.makeNode 42\nHost.inspect node"
```

`HostTypes.describe<Node>()` inspects the real CLR type without constructing a value or invoking getters. `[<FScriptVisible>]` explicitly opts a property into the script surface; unmarked members remain hidden, regardless of their CLR type. The descriptor is still an ordinary `HostTypeDescriptor` in the existing registration.

`HostTypes.wrap` verifies the registration, snapshots each visible property once, and retains the original `Node` as its opaque payload. `HostTypes.unwrap<Node>` in opaque mode checks the descriptor, snapshot field types, and CLR payload type before returning that payload. It does not invoke getters or reconstruct a CLR object from visible fields. The host state may have changed since a snapshot was taken; wrap again to publish a fresh snapshot.

### Supported CLR field mappings

| CLR field type | Script field type |
| --- | --- |
| F# `int` (`Int32`), `int64` | `int` (`VInt`, with `int` widened to 64 bits) |
| F# `float` (`Double`), `bool`, `string`, `unit` | `float`, `bool`, `string`, `unit` |
| F# list, set, or one-dimensional array | List of converted elements |
| F# option | Option of the converted value |
| F# tuple or struct tuple | Tuple of converted elements |
| F# `Map<string, 'T>` | String-keyed map of converted values |
| Another attributed host type | Its registered named type, opaque or script-constructible as declared |

Register a descriptor for every referenced host type, including types inside empty lists or `None`. Arrays and other containers are copied into interpreter values, so later CLR mutation does not change the visible snapshot. Scalars and nested containers use the same mapping for descriptor generation and value conversion.

Only public readable instance properties are supported; annotated static members, private getters, and indexers are errors. Unsupported visible types (such as `Guid`, enums, arbitrary objects, and unannotated records) are rejected with the CLR member path. They can still be used as hidden state. Null objects/fields are rejected; use F# options to represent absence. Recursive visible type graphs are rejected, so keep cyclic links hidden. Two distinct CLR types in one projected graph cannot share a script type name.

### The type descriptor behind the helper

`HostTypes.describe<Node>()` builds this ordinary `FScript.Extensibility.HostTypeDescriptor` record:

```fsharp
let nodeDescriptor : HostTypeDescriptor =
    { Name = "Node"
      Fields = Map.ofList [ ("Value", TInt) ]
      HostManaged = true }
```

| Field | Type | Purpose |
| --- | --- | --- |
| `Name` | `string` | Script-visible type name, referenced by `TNamed "Node"` in function schemes. |
| `Fields` | `Map<string, ScriptType>` | Visible field names and their script types. |
| `HostManaged` | `bool` | `true` requires host-created opaque values; `false` allows ordinary record construction and updates in scripts. |

The descriptor describes the script surface. It contains no CLR type, constructor, field getter, or hidden payload: `Token` is absent. The attributed CLR type supplies the conversion rules used by `HostTypes` and `HostFunctions`; the descriptor alone does not define how to reconstruct a CLR object.

For the `Point` example below, `HostTypes.describe<Point>()` produces the same visible `Value: int` field with `Name = "Point"` and `HostManaged = false`, matching `ScriptConstructible = true`.

A function descriptor connects these types to a callable script API. For `makeNode` above, its `Scheme` is `Forall([], TFun(TInt, TNamed "Node"))` and its `Arity` is `1`; its `Impl` adapts interpreter arguments, calls the typed handler, and wraps the result. See [the function descriptor behind the helper](./register-externs.md#the-function-descriptor-behind-the-helper).

### Explicit descriptors remain available

You can write descriptors directly and place them in `ExtensionRegistration.Types`. This is useful when the visible snapshot or payload does not directly correspond to one CLR type.

Use `HostInterop.create registration typeName fields payload` when the visible snapshot or payload does not directly correspond to one CLR type. `HostInterop.unwrap typeName value` exposes its descriptor, fields, and arbitrary payload to trusted host code. These lower-level helpers do not use reflection. `HostTypes.unwrap<Node>` requires a compatible `Node` payload, so it cannot retrieve an unrelated custom payload.

Descriptors and registrations belong to the host/session; duplicate type names and collisions with built-in or script type declarations are rejected. `HostFunctions.create` derives a function scheme, arity, and argument/result conversion from a concrete F# handler using these same mappings. It validates all referenced type registrations when the function is declared. No handler or getter runs at declaration time. See [Declare typed host functions](./register-externs.md#declare-a-typed-handler).

## Allow scripts to construct a provided record

For data-only F# records, opt into script construction:

```fsharp
open FScript.Extensibility

[<FScriptHostType("Point", ScriptConstructible = true)>]
type Point =
    { [<FScriptVisible>]
      Value: int64 }

let types =
    { HostInterop.empty with Types = [HostTypes.describe<Point>()] }
let inspect = HostFunctions.create types "Host.inspect" (fun (point: Point) -> printfn "%d" point.Value)
let registration = { types with Functions = [inspect] }
let source = """
let point = { Value = 1 }
Host.inspect { point with Value = 42 }
"""
let loaded =
    FScript.Runtime.ScriptHost.loadSourceWithRegistration
        (FScript.Runtime.ExtensionBridge.toLanguageRegistration registration) source
```

Scripts use normal record construction and updates, and extern functions can receive those records through their declared `Point` parameter. `HostTypes.unwrap<Point>` constructs the CLR record from the script fields; `HostTypes.wrap` converts it back to an ordinary `VRecord`. This mode carries no opaque payload or retained host state.

**Every constructor field must be visible.** An unmarked field is a declaration error in `HostTypes.describe`, during provider initialization before any script runs. Constructible records also cannot expose computed properties outside their constructor fields. No hidden values are guessed, defaulted, or obtained from an earlier host object. The first constructible adapter supports F# records and struct records; CLR classes remain available in opaque mode.

Nested records, lists, arrays, options, tuples/struct tuples, and string-keyed F# maps convert in both directions. A nested opaque value still requires a host-created instance and retains its payload. Conversion back to CLR `int` checks the Int32 range rather than truncating a script's 64-bit integer.

The low-level `HostTypeDescriptor.HostManaged` field remains for compatibility: `false` represents an ordinary constructible record; `true` represents an opaque host-managed value. The attribute's `ScriptConstructible` option expresses the public intent directly and maps to this existing distinction.

## What scripts can do with opaque values

Scripts can read visible fields, destructure them, and pass the complete value through functions, containers, closures, tasks, and host calls:

```fsharp
let node = Host.makeNode 42
let read x = x.Value
let keep x =
    let visible = x.Value
    x
let saved = keep node
match saved with
| { Value = n } -> read saved = n
```

A helper that reads a field and returns its argument retains the host-managed type and payload. Selecting a scalar field returns only that field value.

Scripts cannot manufacture a `Node` from `{ Value = 42 }`, access `Token`, or update a host-managed value using either `{ node with Value = 0 }` or `{| node with Value = 0 |}`. An ordinary record explicitly built from projected fields remains an ordinary record. External calls and exported-function invocation validate declared argument and result types, including nested containers and polymorphic round trips.

Visible fields are snapshots supplied by the host. Equality recursively compares visible fields of the same host type and ignores private state; `List.distinct` uses the same rule. `List.contains` currently compares only scalars; for composite values, use `List.exists (fun item -> item = expected)`. Pretty printing, interpolation, and JSON serialization expose visible fields only. JSON deserialization cannot reconstruct host-managed values, including values nested inside containers.

## Host responsibilities and other entry points

The host owns resource lifetime, synchronization, and cloning. A clone is an ordinary registered function whose implementation decides whether to copy or share its payload and supplies a new visible snapshot. Preserving state and metadata is guaranteed; wrapper or CLR object identity is not.

Use `ScriptHost.loadFileWithRegistration` for files, `loadSourceWithIncludesAndRegistration` for host-controlled imports, or `Interactive.createWithRegistration` for persistent submissions. Direct language hosts can use `FScript.inferWithRegistration` and `FScript.evalWithRegistration`. Existing extern-list APIs remain supported when no host types are needed.

[Assembly providers](./register-externs#assembly-providers-for-cli-and-editor) can return the same registration for CLI execution and VS Code type checking. The [environment configuration](../reference/environment-configuration) selects those assemblies.

The [JavaScript facade](./fable-javascript#register-host-types-and-functions) supports explicit descriptors, opaque payload handles, constructible records, and registered unions. Attributed descriptor generation and snapshot conversion are optional .NET runtime helpers; there is no automatic CLR method registration or native ABI mapping.

## Host-provided unions

Use an attributed F# union when the contract includes named choices or payload cases:

```fsharp
[<FScriptHostType("Protocol.Cacheability", ScriptConstructible = true)>]
type Cacheability = Never | Local | Remote

[<FScriptHostType("Protocol.ExportFlag", ScriptConstructible = true)>]
type ExportFlag = Dispatch | Default | Cache of Cacheability

let registration =
    { HostInterop.empty with
        Unions = [ HostTypes.describeUnion<Cacheability>()
                   HostTypes.describeUnion<ExportFlag>() ] }
```

Scripts can construct `Cache Remote`, annotate a value as `Protocol.ExportFlag`, use `Protocol.ExportFlag.Cache Remote`, and match its cases. `HostFunctions.create` handles conversion to and from these CLR unions. Use short constructors when unambiguous; qualified constructors disambiguate types supplied by different modules.

The helper produces a plain `HostUnionDescriptor`:

| Field | Type | Purpose |
| --- | --- | --- |
| `Name` | `string` | Script-visible nominal union type name. |
| `Cases` | `Map<string, ScriptType option>` | Case names and payload types; `None` means a nullary case. Multiple CLR payload fields use `TTuple`. |

Put every referenced union descriptor in `ExtensionRegistration.Unions`, and every referenced record descriptor in `Types`. Union projections require `ScriptConstructible = true` and nonrecursive supported payloads. `HostTypes.describe` describes records; `describeUnion` describes unions.

F# sets use script lists at the boundary. Host snapshots follow set iteration order; converting a script list back to a set sorts and removes duplicates using CLR comparison semantics.

## Compiler purity guarantees

Providers can optionally declare `Pure` information for native compilation. Functions promise deterministic, effect-free, successful termination for valid inputs; external types promise deeply immutable visible data and payloads with safe, terminating registered field reads and structural observers. Constructors and ordinary methods do not inherit a receiver type's guarantee.

The compiler reads optional `FScriptPureFunctions()` and `FScriptPureTypes()` provider methods returning registered script names. These declarations are trusted; ordinary interpreter execution and SDK validation remain unchanged. See [Pure FFI and external values](../compiler/compile-to-dotnet.md#pure-ffi-and-external-values) for the contract, examples, callback rules, and compiler API.
