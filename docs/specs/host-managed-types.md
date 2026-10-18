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

Existing `[FScriptExternProvider]` methods may return `ExtensionRegistration` with either no parameters or one `HostContext` parameter. Legacy `ExternalFunction list` providers remain supported. CLI loading supports `--extern-assembly` and automatically discovered or explicit `--config` environments, including file, stdin, and REPL execution. VS Code uses the same provider metadata through a selected, trusted [environment configuration](./environment-configuration.md). Provider names and function-name conflict checks are unchanged.

External calls and embedded exported-function invocation validate declared argument and result types. Per-call type-variable bindings also prevent polymorphic round trips from replacing a host-managed argument, including one nested in containers, with an ordinary record. Plain records and values of another host type cannot satisfy a required host-managed type. Runtime record updates reject host-managed operands even when inference is bypassed.

The JavaScript tagged-value facade does not expose host-managed payloads; attempting to export such a value fails explicitly. There is no native ABI, pointer representation, disposal protocol, package resolution, or automatic CLR method registration.

## Optional CLR projection helpers

`FScript.Runtime` provides `FScriptHostTypeAttribute(name)` on CLR classes/structs and `FScriptVisibleAttribute` on properties. Its `ScriptConstructible` option defaults to `false` (opaque values); `true` selects ordinary constructible record projection. Being provided by a module does not itself prohibit script construction. `HostTypes.describe<'T>()` derives a `HostTypeDescriptor` without invoking getters. The type attribute is required on each projected CLR type, and only explicitly marked public readable instance properties become visible fields. Hidden members are ignored even when their types are unsupported.

`HostTypes.wrap registration value` checks that the registration contains exactly matching descriptors for the root and every referenced attributed type, then snapshots marked getters once per value occurrence. Opaque mode retains the original CLR value as payload; constructible mode produces an ordinary `VRecord` without a payload. The snapshot is recursively converted into ordinary interpreter scalars/containers or nested host-managed values. No partial value is returned on a getter/conversion failure. Registrations remain explicit and local; helpers add no global type registry.

`HostTypes.unwrap<'T> value` validates visible field types against the CLR projection. In opaque mode it also validates descriptor metadata and payload CLR type, then returns that payload. In constructible mode it recursively converts fields and constructs a CLR record. It does not call getters or reconstruct an object. It does not require the current payload state to equal its earlier visible snapshot. Existing low-level `HostInterop.create`/`unwrap` continue to accept arbitrary host-owned payloads; typed retrieval rejects incompatible payloads.

Mappings are `Int32`/`Int64` to script `int`, `Double` to `float`, `Boolean`, `String`, F# `unit`, F# lists and one-dimensional arrays to lists, F# options, F# tuples/struct tuples, string-keyed F# maps, and nested attributed types to named host types. Container conversion copies visible values. Null roots/fields fail except F# option `None` and boxed unit. Unsupported types fail at description time with the CLR property path; there is no string/object fallback or numeric narrowing. Annotated static properties, private/nonexistent getters, indexers, invalid names, duplicate visible names, colliding CLR projections, and recursive visible type graphs fail clearly. Missing nested registrations fail even when a container has no actual nested values.

These helpers live entirely in Runtime; Language stays reflection-free and independent of Runtime. The JavaScript facade does not support authoring or exporting host-managed payloads. `HostFunctions.create` reuses the same conversion path to derive closed function schemes and synchronous handler invocation. Arbitrary CLR method discovery, native layouts, and resource lifetime management remain outside these helpers. See [typed host functions](./host-functions.md).

## Script-constructible provided types

`[<FScriptHostType("Point", ScriptConstructible = true)>]` opts an F# record or struct record into ordinary script construction and both record-update syntaxes. Every constructor field must have `[<FScriptVisible>]`; hidden fields and extra computed visible properties fail in `HostTypes.describe` during provider initialization, before script analysis/evaluation. CLR classes require opaque mode in this adapter version. Attributes alone do not introduce compiler-time CLR validation; registration performs the check.

Constructible projection uses the existing descriptor encoding `HostManaged = false`, ordinary `TRecord` type definitions, and `VRecord` values. The historical descriptor field is retained for compatibility; new attribute-facing configuration uses `ScriptConstructible`. There is no CLR payload to preserve, and typed retrieval reconstructs a fresh CLR record from fields in constructor order. It never fills missing fields or ignores extra fields. Int64 script integers are range-checked when converting to CLR Int32. Lists/arrays/options/tuples/string-keyed maps and nested provided records convert recursively. A nested opaque host type remains nonconstructible and is retrieved from its validated payload.

Constructible provided types follow existing ordinary record literal resolution, structural matching, update and JSON behavior. Opaque descriptors remain excluded from ordinary record construction. No language syntax, automatic function registration, or method constructor invocation bridge is added.
