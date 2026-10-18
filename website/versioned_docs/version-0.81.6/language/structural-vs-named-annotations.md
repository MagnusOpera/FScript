---
id: structural-vs-named-annotations
title: Structural vs Named Annotations
slug: /language/structural-vs-named-annotations
---

FScript supports different ways to annotate record-shaped values.

## 1) Structural annotation

Use `{| ... |}` when you care about field shape directly.

```fsharp
let formatAddress (address: {| City: string; Zip: int |}) =
  $"{address.City} ({address.Zip})"
```

This means: any value with compatible fields can be used.

## 2) Named annotation

Use a declared record type name when you want an explicit domain type.

```fsharp
type Project = { Name: string; Version: int }

let describeProject (p: Project) =
  $"{p.Name} v{p.Version}"
```

This is the clearest style when a type is part of your domain model.

## 3) Declared-type-by-shape annotation

Use `{ ... }` in an annotation to refer to an existing declared record type by its shape.

```fsharp
type Project = { Name: string; Version: int }

let describeProject (p: { Name: string; Version: int }) =
  $"{p.Name} v{p.Version}"
```

This form resolves only when exactly one declared record type matches the shape.

## Which one should you use?

- Prefer **named annotation** (`Project`) for public APIs and shared domain types.
- Use **structural annotation** (`{| ... |}`) for local helpers and ad-hoc transforms.
- Use **declared-type-by-shape** (`{ ... }`) only when the shape is unambiguous and intentional.

## Practical rule

If the value represents a business/domain concept, define a named record type and annotate with that type name.

## Construction and updates

```fsharp
let original = {| City = "Paris"; Zip = 75001 |}
let moved = { original with City = "Lyon"; Zip = 69001 }
let tagged = {| moved with Label = "office" |}
```

Both forms create new values. `{ record with ... }` replaces existing fields; `{| record with ... |}` can add fields. Existing field types must remain compatible.

A structural reader may accept an opaque host-managed value with the required visible fields, and returning the argument preserves its complete type and payload. An updating helper requires a constructible record and cannot update an opaque value. See [Host-Provided Types](../embedding/host-managed-types).

Next: [Imports and Exports](./modules-imports-exports.md).
