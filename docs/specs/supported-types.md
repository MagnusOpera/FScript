# FScript Supported Types Specification

## Purpose
This document specifies the value and type system used by the interpreter.

## Primitive types
- `unit`
- `int` (`int64` at runtime)
- `float`
- `bool`
- `string`

## Composite/container types
- List: `'a list`
- Task: `'a task`
- Tuple: `(t1 * t2 * ...)`
- Option: `'a option`
- Map: `'a map` for string keys, or `value map[key]` for other supported keys
- Record: structural record types
- Discriminated union: named union types with cases

Map key annotations use postfix syntax: `string map[int]`, `int map[bool]`, or `string map[Priority]`.
Explicit `map[string]` is accepted and displayed as the legacy `map` suffix.
Unresolved `numeric` keys specialize to `int`; concrete `float` keys are rejected. Map values retain normal numeric inference.
Angle-bracket map type syntax is not supported.

## Map literals
- Native map literal syntax:
  - empty: `{}`
  - populated: `{ ["a"] = 1; ["b"] = 2 }`
  - spread update: `{ ["a"] = 1; ..tail }`
  - multiline entries are supported in an indented block.
- Keys are bracketed expressions of type `string`, `int`, `bool`, or a union whose cases all have no payload (for example `{ [keyExpr] = value }`).
- Record literals and map literals share `{ ... }` braces:
  - map entries use `[expr] = value`
  - record entries use `Field = value`
  - `{}` denotes an empty map.
- Values are inferred and unified to a single value type.
- Keys are inferred and unified to a single supported key type; empty maps acquire their key and value types from use.
- Map operations contribute key constraints to Hindley–Milner inference. For example, `let getZero m = m |> Map.tryGet 0` infers `'v map[int] -> 'v option` without an argument annotation.
- Indexing an otherwise unknown collection with an integer still infers a list. Use `Map.tryGet 0` to establish an integer-keyed map without an annotation.
- See `./map-matching-reference.md` for focused map-pattern examples.

## Index access
- Lists support optional zero-based index access with integer keys (for example `values[0]`).
- Maps support optional key lookup with their declared key type (for example `scores["math"]` or `byId[1]`).
- Negative or out-of-range list indices return `None`.

## Native access summary

### Lists
- Type form: `'a list`
- Indexer signature: `'a list -> int -> 'a option`
- Notes:
  - `values[index]` is zero-based
  - negative or out-of-range indices return `None`

### Maps
- Type forms: `'a map` (string keys) and `'value map['key]`
- Indexer signature: `'value map['key] -> 'key -> 'value option`
- Notes:
  - `values[key]` performs optional lookup
  - key type must be `string`, `int`, `bool`, or a union with no payload-bearing cases

### Records
- Field access signature: `{ Field: 'a; ... } -> 'a`
- Notes:
  - records use named field access (`record.Field`)

### Tuples
- Access model:
  - tuples are consumed by destructuring and pattern matching
  - tuples do not expose a native indexer

### Options
- Constructors:
  - `Some : 'a -> 'a option`
  - `None : 'a option`

### Tasks
- Type form: `'a task`
- Construction and observation:
  - `Task.spawn : (unit -> 'a) -> 'a task`
  - `Task.await : 'a task -> 'a`
- Notes:
  - tasks are opaque runtime handles
  - spawned thunks execute concurrently
  - side effects may interleave across tasks

## Function types
- Functions use curried arrow types:
  - `t1 -> t2`
  - `t1 -> t2 -> t3` is right-associative
- Function type syntax is available in parameter annotations.
- Parameter annotations support two inline record type forms:
  - structural: `let f (x: {| A: int; B: string |}) = ...`
  - declared-type-by-shape: `let f (x: { A: int; B: string }) = ...`
- `{ ... }` resolves only if exactly one declared record type matches that shape.
- Inline record annotation fields are semicolon-separated in single-line form.

## Named record declarations
- Top-level record declarations:
  - `type Name = { ... }`
  - `type rec Name = { ... }`
- Recursive record references use `type rec`.
- Named record types and matching record shapes unify structurally.

## Discriminated union declarations
- Top-level union declarations are supported:
  - `type Shape = | Point | Circle of int`
  - `type rec Node = | Empty | Branch of (int * Node list)`
- Union case names start with an uppercase identifier.
- A case payload is optional:
  - no payload: `| Point`
  - payload: `| Circle of int`
- Payloads are single-type payloads; tuple payloads are used for multi-value payloads.

## Generic record and union declarations

A type declaration can introduce one or more unconstrained type parameters.

```fsharp
type Box<'a> = { Value: 'a }
type Outcome<'value, 'error> = | Success of 'value | Failure of 'error

let unbox (box: Box<'a>) : 'a = box.Value
let wrap (value: 'a) : Outcome<Box<'a>, string> = Success { Value = value }
```

Use a user-defined type with the number of arguments declared by the type.
Field access and record updates use the fields of the applied type. Union case
constructors and patterns infer fresh arguments at each use. A recursive generic
record or union uses `type rec` and refers to itself with its declared parameters
in order.

Generic record and union wrappers preserve generalization according to their
contents. Pure values and unrelated callbacks may remain polymorphic, while an
existing opaque instance inside a wrapper retains its type arguments through
record projection, record updates, union matching, and captured closures.
Constructing a union around a factory forwards the factory without calling it;
separate calls to that factory still instantiate independently. An ordinary
function that shadows a constructor follows its own allocation behavior.

Built-in containers use postfix syntax in source and type displays, including
`int list`, `int option`, `int task`, `int map`, and `string map[int]`.

## Union constructors and matching
- Union cases are constructors in expression position:
  - `let x = Circle 3`
  - `let p = Point`
- `match` supports union-case patterns:
  - `| Circle r -> ...`
  - `| Point -> ...`
- `option` values (`Some`/`None`) are also represented as union-style constructors and patterns.

## Type inference model
- Hindley–Milner style inference.
- Independent inference calls may run concurrently; fresh type-variable identities remain isolated from races between callers.
- Let-polymorphism. Variables associated with an existing opaque generic host instance remain fixed through aliases and captured closures. Independent callback variables remain polymorphic even in an aggregate containing that instance, and separate factory calls get fresh instance types.
- `numeric` is a constrained inference meta-type that can specialize to `int` or `float`; it is not a distinct runtime value type and introduces no implicit conversion.
- Arithmetic operators (`+`, `-`, `*`, `/`) unify both operand types and preserve that type as the result. Unresolved operands carry a shared numeric constraint: `let add x y = x + y` displays `numeric -> numeric -> numeric`. Each application specializes independently, returning `int` for integer operands or `float` for float operands.
- Arithmetic rejects mixed `int`/`float` operands and nonnumeric operands during inference, including calls through polymorphic functions, aliases, and higher-order functions. A concrete annotation specializes the whole arithmetic relationship: `let add x (y: int) = x + y` infers `int -> int -> int`.
- Numeric comparisons (`<`, `>`, `<=`, `>=`) apply the same operand constraint and return `bool`. Remainder (`%`) accepts integers only and infers `int -> int -> int`, matching runtime support.
- Numeric variables retain separate identities internally. Identical `numeric` labels do not imply that unrelated numeric expressions or parameters are tied; operators and ordinary unification establish the relationship.
- Structural typing for record values.
- Field access is typed when the target expression is inferred as a record.

### Generalization validation

The language suite checks let-polymorphism and captured-variable restrictions
with deterministic generated programs. Expectations cover independent pure
instantiations, monomorphic captured parameters and opaque instances, fresh
factory calls, and independent callbacks paired with captured opaque instances.
Generic campaigns cover two-parameter records and unions, nesting in both orders,
concrete type-argument mismatches, pure aggregate polymorphism, and opaque cells
paired with unrelated callbacks. Generic containers required by a scenario remain
present during shrinking.
Every generated rejection has a compatible acceptance control. See
[inference property tests](../guides/inference-property-tests.md) for the bounded
generation model, shrinking rules, and replay commands.

## Runtime value representation (high level)
- `VUnit`, `VInt`, `VFloat`, `VBool`, `VString`
- `VList`, `VTuple`
- `VRecord`
- `VMap`
- `VOption`
- `VTask`
- `VUnionCase`, `VUnionCtor`
- `VClosure`
- `VExternal`
- `VTypeToken`

## Reflection
- `typeof Name` yields a type token.
- `nameof identifier` yields the bound identifier name as `string`.
- Type tokens are consumed by host externs (for example JSON/XML decoding).

## Host-managed records

Explicit host descriptors may mark a type `HostManaged`. Such values expose immutable visible fields and preserve opaque host state and metadata through transport. Scripts may read, match, and compare visible fields but cannot construct or update these values. Structural readers preserve the concrete host type when returning the whole argument. See [host-managed types](./host-managed-types.md).

.NET hosts can derive host-managed descriptors from explicitly attributed CLR types using `FScript.Extensibility.HostTypes`. This is an opt-in host adapter, not a new script type category. Supported CLR field mappings and rejected shapes are specified in [host-managed types](./host-managed-types.md#optional-clr-projection-helpers).

Host registrations may also supply nominal, script-constructible unions through `HostUnionDescriptor`. CLR union payloads and F# sets use the mappings described in [host types](./host-managed-types.md#constructible-foreign-unions).


List literal inference accumulates substitutions from every preceding element before unifying the next element. This also enforces homogeneous task-result types: `[Task.fromResult 1; Task.fromResult "text"]` is a type error. A task callback may return another task; `Task.continueWith` and related operations preserve the resulting nested `TTask(TTask item)` type.
