---
id: stdlib-json
title: Json Module
slug: /stdlib/json
---

`Json` is provided by the default .NET runtime extern registry. It converts FScript values without reflecting arbitrary CLR objects.

## `Json.deserialize : type -> string -> 'a option`

Argument order is `typeToken -> json`. Use `typeof Name` for a declared type. The token selects the decoder at runtime; the generic function scheme does not statically tie the token to the result type. A function return annotation makes the intended result explicit, as below.

```fsharp
type Settings = { Port: int; Label: string option }
let decodeSettings json : Settings option =
  Json.deserialize (typeof Settings) json
let settings = decodeSettings "{\"Port\":8080}"
match settings with
| Some value -> Console.writeLine $"port={value.Port}, label={value.Label}"
| None -> Console.writeLine "invalid settings"
```

Malformed JSON, incompatible values, or unsupported target types return `None`.

| Target field/value | JSON decoding |
| --- | --- |
| `string`, `int`, `float`, `bool` | Corresponding JSON scalar; integers must fit signed 64 bits |
| `unit` | Produces `()` |
| `'a list` | JSON array; every element must decode |
| Tuple | JSON array with exactly the tuple's arity |
| `'a option` | `null` becomes `None`; other values decode as `Some` |
| Ordinary record | JSON object; field names are case-sensitive; extra properties are ignored |
| Map | JSON object; property names decode as string, int, or bool keys |

Missing record fields are allowed only for option fields, which become `None`. A failed nested conversion fails the entire result. Union types, union map keys, functions, tasks, and host-managed types are unsupported decoding targets. Decoding cannot manufacture opaque host state, including inside a record or container.

## `Json.serialize : 'a -> string option`

Returns JSON text when the entire value is serializable.

```fsharp
let text = Json.serialize { Port = 8080; Label = Some "api" }
// Some "{\"Label\":\"api\",\"Port\":8080}"
```

| Value | JSON encoding |
| --- | --- |
| Strings, finite numbers, booleans | JSON scalars |
| `()` and `None` | `null` |
| `Some value` | Encoding of `value` |
| Lists and tuples | JSON arrays |
| Records | JSON objects |
| Maps with string, int, or bool keys | JSON objects; integer and boolean keys become property names |
| Host-managed values | Visible fields only, as a JSON object |

A union value, union-keyed map, function, task, or type token anywhere in the value makes serialization return `None`. Opaque payloads and host-type metadata are never serialized. Non-finite floating-point values are not suitable for portable JSON round trips.

Options and tuples lose their FScript type information in JSON; use the intended target type when decoding. This API does not promise arbitrary value round trips.
