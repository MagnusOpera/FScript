---
id: common-patterns
title: Common Script Patterns
slug: /examples/common-patterns
---

## Parse + validate + transform

```fsharp
let normalize maybeText =
  match maybeText with
  | Some text -> text |> String.toLower
  | None -> ""
```

## Map lookup with fallback

```fsharp
let getOrDefault key values fallback =
  values
  |> Map.tryGet key
  |> Option.defaultValue fallback
```

## Branch on rich outcomes

```fsharp
type Outcome =
  | Accepted of string
  | Rejected of string
```

Model meaningful states with unions and match over them.
