---
id: stdlib-json
title: Json Module
slug: /stdlib/json
---

The `Json` module is supplied by the default .NET runtime extern registry.

- `Json.deserialize : Type<'a> -> string -> 'a option`
- `Json.serialize : 'a -> string option`

Use `typeof` to provide the target type when decoding:

```fsharp
type Person = { Name: string; Age: int }

let person = Json.deserialize (typeof Person) "{\"Name\":\"Ada\",\"Age\":42}"
let encoded = Json.serialize {| Name = "Ada"; Age = 42 |}
```

Records, unions, tuples, options, lists, maps, and scalar values are encoded when their complete shape is supported. Invalid input or unsupported values return `None`.

`Json.*` is currently available in the .NET runtime and CLI, but not in generated Go or the LIR interpreter.
