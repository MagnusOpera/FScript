---
id: host-managed-types
title: Host-Provided Types
slug: /embedding/host-managed-types
---

A host module can provide both ordinary data records that scripts construct and opaque values that only host code creates. Both travel through the existing typed extern bridge. The public attribute option `ScriptConstructible` controls construction; merely providing a type from a module imposes no ownership restriction.

Opaque projection is the default and can retain hidden resource state. Set `ScriptConstructible = true` explicitly for a fully visible F# data record.

## Register a type and a factory

A descriptor names the script type and its visible field types. An `ExtensionRegistration` groups descriptors with ordinary `ExternalFunction` records. This complete F# example registers a `Node` factory and calls script exports:

```fsharp
open System
open FScript.Language
open FScript.Runtime

[<FScriptHostType("Node")>]
type Node =
    { [<FScriptVisible>]
      Value: int64
      Token: Guid }

let nodeDescriptor = HostTypes.describe<Node>()

let typeRegistration =
    { HostInterop.empty with Types = [ nodeDescriptor ] }

let makeNode =
    { Name = "Host.makeNode"
      Scheme = Forall([], TFun(TInt, TNamed "Node"))
      Arity = 1
      Impl = fun _ args ->
          match args with
          | [ VInt value ] ->
              HostTypes.wrap typeRegistration
                  { Value = value; Token = Guid.NewGuid() }
          | _ -> failwith "Host.makeNode expects an int" }

let registration =
    { typeRegistration with Functions = [ makeNode ] }

let source = """
export let make (value: int) = Host.makeNode value

export let retain (node: Node) =
    let visible = node.Value
    node
"""

let loaded = ScriptHost.loadSourceWithRegistration registration source
let original = ScriptHost.invoke loaded "make" [ VInt 42L ]
let returned = ScriptHost.invoke loaded "retain" [ original ]
let state = HostTypes.unwrap<Node> returned
printfn "Value: %d; private token: %O" state.Value state.Token
```

`HostTypes.describe<Node>()` inspects the real CLR type without constructing a value or invoking getters. `[<FScriptVisible>]` explicitly opts a property into the script surface; unmarked members remain hidden, regardless of their CLR type. The descriptor is still an ordinary `HostTypeDescriptor` in the existing registration.

`HostTypes.wrap` verifies the registration, snapshots each visible property once, and retains the original `Node` as its opaque payload. `HostTypes.unwrap<Node>` in opaque mode checks the descriptor, snapshot field types, and CLR payload type before returning that payload. It does not invoke getters or reconstruct a CLR object from visible fields. The host state may have changed since a snapshot was taken; wrap again to publish a fresh snapshot.

### Supported CLR field mappings

| CLR field type | Script field type |
| --- | --- |
| F# `int` (`Int32`), `int64` | `int` (`VInt`, with `int` widened to 64 bits) |
| F# `float` (`Double`), `bool`, `string`, `unit` | `float`, `bool`, `string`, `unit` |
| F# list or one-dimensional array | List of converted elements |
| F# option | Option of the converted value |
| F# tuple or struct tuple | Tuple of converted elements |
| F# `Map<string, 'T>` | String-keyed map of converted values |
| Another attributed host type | Its registered named type, opaque or script-constructible as declared |

Register a descriptor for every referenced host type, including types inside empty lists or `None`. Arrays and other containers are copied into interpreter values, so later CLR mutation does not change the visible snapshot. Scalars and nested containers use the same mapping for descriptor generation and value conversion.

Only public readable instance properties are supported; annotated static members, private getters, and indexers are errors. Unsupported visible types (such as `Guid`, enums, arbitrary objects, and unannotated records) are rejected with the CLR member path. They can still be used as hidden state. Null objects/fields are rejected; use F# options to represent absence. Recursive visible type graphs are rejected, so keep cyclic links hidden. Two distinct CLR types in one projected graph cannot share a script type name.

### Explicit descriptors remain available

Use `HostInterop.create registration typeName fields payload` when the visible snapshot or payload does not directly correspond to one CLR type. `HostInterop.unwrap typeName value` exposes its descriptor, fields, and arbitrary payload to trusted host code. These lower-level helpers do not use reflection. `HostTypes.unwrap<Node>` requires a compatible `Node` payload, so it cannot retrieve an unrelated custom payload.

Descriptors and registrations belong to the host/session; duplicate type names and collisions with built-in or script type declarations are rejected. Reflection helpers do not automatically register host methods or derive their function schemes.

## Allow scripts to construct a provided record

For data-only F# records, opt into script construction:

```fsharp
open FScript.Language
open FScript.Runtime

[<FScriptHostType("Point", ScriptConstructible = true)>]
type Point =
    { [<FScriptVisible>]
      Value: int64 }

let registration =
    { HostInterop.empty with Types = [HostTypes.describe<Point>()] }

let source = """
let point = { Value = 1 }
{ point with Value = 42 }
"""
let loaded = ScriptHost.loadSourceWithRegistration registration source
let native = HostTypes.unwrap<Point> loaded.LastValue
printfn "%d" native.Value
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

The JavaScript facade does not support authoring or exporting host-managed payloads. Attributed descriptor generation and snapshot conversion are optional .NET runtime helpers; there is no automatic CLR method registration or native ABI mapping.
