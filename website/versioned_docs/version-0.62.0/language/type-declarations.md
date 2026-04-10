---
id: type-declarations
title: Type Declarations
slug: /language/type-declarations
---

Use `type` for records and discriminated unions.

## Record type

```fsharp
type Project = { Name: string; Version: int }
```

## Union type

```fsharp
type BuildStatus =
  | Queued
  | Running
  | Failed of string
  | Succeeded
```

## Recursive type

```fsharp
type rec Tree =
  | Empty
  | Node of (int * Tree list)
```

## Using declared types

```fsharp
let statusMessage status =
  match status with
  | Queued -> "queued"
  | Running -> "running"
  | Failed msg -> $"failed:{msg}"
  | Succeeded -> "ok"
```
