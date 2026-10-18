# Typed Host Function Adapters

## Public API

`FScript.Extensibility.HostFunctions.create registration name handler : ExternalFunction` adapts a concrete synchronous F# function. It derives a monomorphic `Forall([], ...)` scheme, arity, and interpreter-value conversion. The resulting record works with all existing extern-list and registration-aware host entry points and assembly providers.

```fsharp
let add =
    HostFunctions.create HostInterop.empty "Host.add"
        (fun (left: int64) (right: int64) -> left + right)
```

No script syntax, general CLR method discovery, new function attributes, or implicit registration is added.

## Descriptor representation

The helper returns the SDK `FScript.Extensibility.ExternalFunction` record, whose fields are `Name: string`, `Scheme: FunctionScheme`, `Arity: int`, and `Impl: ExternalCallContext -> ScriptValue list -> ScriptValue`. For the example above, `Scheme` is `Forall([], TFun(TInt, TFun(TInt, TInt)))` and `Arity` is `2`; `Impl` performs validated argument decoding, handler invocation, and result encoding.

Referenced host types use `HostTypeDescriptor` records with `Name: string`, `Fields: Map<string, ScriptType>`, and `HostManaged: bool`. `HostTypes.describe` produces these descriptors from attributed CLR types. Descriptors contain only script-visible type metadata, never CLR conversion code or opaque payloads. A named type appears in function schemes as `TNamed descriptor.Name`; `HostManaged = false` permits script record construction and updates, while `true` requires host-created opaque values.

`ExtensionRegistration` groups `Types: HostTypeDescriptor list`, `Unions: HostUnionDescriptor list`, and `Functions: ExternalFunction list`. Both helpers produce these SDK data structures; direct descriptor authoring remains supported.

## Signatures and registration

- Names must be nonempty qualified identifiers, using the same identifier-character convention as host type projections.
- The handler must have a concrete F# function type and be non-null.
- Every outer function arrow adds one curried argument. Unit arguments remain explicit; `unit -> int` has arity one.
- All parameter and final-result types use the same supported mappings as `HostTypes`: primitives, unit, lists/sets/one-dimensional arrays, options, tuples/struct tuples, string-keyed F# maps, and attributed host records/unions.
- Every referenced attributed type must have a matching descriptor in the supplied registration, even when only present inside an empty container or absent option.
- Distinct CLR types in one signature cannot project to the same script type name.
- Unsupported signatures and invalid registrations fail at adapter construction, without executing handlers or getters.
- No polymorphic schemes, higher-order callback conversion, delegate conversion, asynchronous values, or `ExternalCallContext` injection is provided. A function-valued return extends the curried argument chain; it is not marshaled as a separate function value.
- A host can capture `HostContext` or resources in a handler closure. Lifetime and thread safety remain host responsibilities.

## Invocation

- The helper returns the SDK `ExternalFunction` representation; interpreter application and partial application remain unchanged.
- The complete argument count is checked, including when `Impl` is called directly.
- Every argument is validated and converted before the first CLR invocation step. Invalid later arguments cannot trigger earlier curried side effects.
- CLR Int32 input conversion checks the full 64-bit script integer range before narrowing.
- Opaque arguments require matching descriptors, valid visible fields, and a compatible CLR payload. Ordinary records cannot impersonate them.
- Constructible record arguments are rebuilt from their script fields. Containers convert recursively.
- Results use the same snapshot conversion as `HostTypes`, then undergo script-type validation. Opaque results preserve host payloads and visible snapshots; constructible results are ordinary records.
- Null values are rejected except for the CLR representations of unit and absent F# options.
- Reflection invocation wrappers are removed from handler-error diagnostics; errors identify the registered function.

Manual `ExternalFunction` records remain available for authoritative custom schemes, callbacks through `ExternalCallContext.Apply`, explicit runtime-context access, and arity-zero external values. Language remains independent of SDK reflection helpers.

## Executable graphics example

The [`samples/ffi`](../../samples/ffi/README.md) sample applies these adapters to geometry, lighting, camera, and rendering functions. Script-constructible records carry scene data; opaque `Window` and `Mesh` values retain backend ownership and resource handles. The provider is discoverable without creating native resources. The script entry point calls `Graphics.run` or `Graphics.runHeadless`. These callback-based externs use explicit descriptors and `ExternalCallContext.Apply`; typed adapters remain responsible for data-only calls. The provider owns the main-thread window/render loop and releases GPU resources on exit. Headless integration tests run the actual geometry-generation and scene scripts through the same registration and adapters.
