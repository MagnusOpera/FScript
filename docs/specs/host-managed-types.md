# Host-managed types

## Contract

Host-managed types expose immutable snapshots of selected fields while carrying an opaque host payload. Only the host creates these values. Scripts may read fields, match and destructure visible fields, compare values structurally, and transport the complete value through functions, collections, closures, and tasks.

Transport preserves the payload and `HostManaged` metadata. CLR object or interpreter wrapper identity is not a language guarantee. The host owns resource lifetime, cloning, synchronization, and the meaning of the payload.

## Explicit registration

`FScript.Language.HostTypeDescriptor` contains `Name`, `Fields : Map<string, Type>`, and `HostManaged : bool`. Hidden state is not a field descriptor. `ExtensionRegistration` contains `Types` and existing `Functions : ExternalFunction list`; function schemes remain authoritative.

Type names are local to a registration/session. Duplicate names and collisions with built-in or script type declarations fail. Descriptors with `HostManaged = false` use ordinary record semantics.

`HostInterop.create registration typeName visibleFields payload` creates a value after checking its visible field names and types. `HostInterop.unwrap typeName value` returns its descriptor, fields, and payload to trusted host code. These helpers do not reflect over CLR objects or invoke getters.

```fsharp
let descriptor =
    { Name = "Node"
      Fields = Map.ofList ["Value", TInt]
      HostManaged = true }
let registration = { HostInterop.empty with Types = [descriptor] }
let value = HostInterop.create registration "Node" (Map.ofList ["Value", VInt 42L]) nativeState
```

Host functions can return this value through their existing `Impl`. Cloning is an ordinary host function that creates another value with an appropriate payload and field snapshot.

## Script behavior

Given an injected `Host.make : int -> Node`:

```fsharp
let read x = x.Value
let node = Host.make 42
let captured () = node
match captured () with
| { Value = n } -> read node = n
```

Field readers and structural parameter annotations retain the concrete type of whole values returned from helpers. Reading a field projects only that field's value; it does not modify the source value or copy its hidden state into the projected scalar.

A visible-field literal is an ordinary record and cannot satisfy a `Node` parameter. Neither `{ node with Value = 0 }` nor `{| node with Value = 0 |}` is permitted. This remains true through generic functions, annotations, and interactive submissions. Scripts may explicitly construct an ordinary record from projected fields, but that record does not become a `Node`.

Record patterns can inspect visible fields only. Hidden state has no script name and cannot be accessed by field projection or matching. Existing map-key restrictions remain in effect; host-managed values may be map values, not keys.

## Equality, display, and decoding

Equality recursively compares visible fields and ignores opaque payloads. Collection equality helpers use the same visible-field rule. Two instances of the same host type may compare equal despite different host resources.

Pretty printing, string interpolation, and JSON serialization expose visible fields only. JSON decoding cannot create a host-managed value, including one nested in a record or container; decoding follows the existing `None` failure convention. Deserialization does not restore a host-managed value from its serialized visible snapshot.

## Integration and validation

Use `FScript.inferWithRegistration` and `FScript.evalWithRegistration`, `ScriptHost.loadSourceWithRegistration` / `loadFileWithRegistration` / `loadSourceWithIncludesAndRegistration`, or `Interactive.createWithRegistration`. Existing extern-list APIs remain available with an empty host-type registration.

Existing `[FScriptExternProvider]` methods may return `ExtensionRegistration` with either no parameters or one `HostContext` parameter. Legacy `ExternalFunction list` providers remain supported. CLI loading continues using `--extern-assembly`, including file, stdin, and REPL execution. Provider names and function-name conflict checks are unchanged.

External calls and embedded exported-function invocation validate declared argument and result types. Per-call type-variable bindings also prevent polymorphic round trips from replacing a host-managed argument, including one nested in containers, with an ordinary record. Plain records and values of another host type cannot satisfy a required host-managed type. Runtime record updates reject host-managed operands even when inference is bypassed.

The JavaScript tagged-value facade does not expose host-managed payloads; attempting to export such a value fails explicitly. The F# type provider can use registered host types inside scripts, but host-managed and constrained-record types cannot be projected into exported CLR signatures. No reflection attributes, native ABI, pointers, disposal protocol, package resolution, or editor extension-loading configuration are introduced.
