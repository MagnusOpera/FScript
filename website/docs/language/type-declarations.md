---
id: type-declarations
title: Type Declarations
slug: /language/type-declarations
---

Name a stable data shape with a record, or model a choice between states with a discriminated union. Type declarations appear at the top level.

## Record type

```fsharp
type Project = { Name: string; Version: int }
```

Declared ordinary records still participate in structural compatibility; their names document intent rather than impose CLR class identity. Opaque host-provided types follow stricter construction rules described in [Host-Provided Types](../embedding/host-managed-types).

## Union type

```fsharp
type BuildStatus =
  | Queued
  | Running
  | Failed of string
  | Succeeded
```

A case has either no payload or one payload type. Use a tuple to carry several values, such as `Failed of (int * string)`. Construct cases with `Running` or `Failed "timeout"`; consume them with `match`.

## Generic records and unions

Declare type parameters after the type name and use them in fields or case payloads.

```fsharp
type Box<'item> = { Items: 'item list }

type Outcome<'value, 'error> =
  | Found of 'value
  | Failed of 'error

let first (box: Box<'item>) : Outcome<'item, string> =
  match List.tryHead box.Items with
  | Some item -> Found item
  | None -> Failed "empty box"

let result = first { Items = [42; 7] }
```

Here, `result` has type `Outcome<int, string>`. Each use of a union constructor can infer its own arguments. Built-in types use postfix syntax, including `int list`, `int option`, `int task`, and `string map[int]`.

## Recursive type

```fsharp
type rec Tree =
  | Empty
  | Node of (int * Tree list)
```

A recursive generic type refers to itself with its declared parameters in order, as in `type rec Chain<'item> = { Value: 'item; Next: Chain<'item> option }`.

## Using declared types

```fsharp
let statusMessage status =
  match status with
  | Queued -> "queued"
  | Running -> "running"
  | Failed msg -> $"failed:{msg}"
  | Succeeded -> "ok"
```

Next: [Pattern Matching](./pattern-matching).
