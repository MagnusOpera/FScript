---
id: stdlib-xml
title: Xml Module
slug: /stdlib/xml
---

`Xml` is provided by the default .NET runtime extern registry. It supports a small path-query language for extracting strings, rather than a full XPath engine or XML object serializer.

## `Xml.queryValues : string -> string -> string list option`

Argument order is `query -> xml`. Returns `Some values` for a valid query/document, including `Some []` when nothing is selected. Invalid queries or malformed XML return `None`.

```fsharp
let xml = "<projects><project name=\"api\">ready</project><project name=\"web\">pending</project></projects>"
let names = Xml.queryValues "projects/project/@name" xml
let states = Xml.queryValues "//project/text()" xml
// names = Some ["api"; "web"]
// states = Some ["ready"; "pending"]
```

| Query part | Meaning |
| --- | --- |
| `root/child` | Walk named elements; the first step can name the root or a direct child of it |
| Leading `//name` | Find matching elements anywhere, including the root, then walk remaining child steps |
| Final `/@attribute` | Extract an attribute; elements missing it contribute no value |
| Final `/text()` | Extract trimmed element text, including descendant text; empty text is omitted |

A query must contain at least one element step and a final selector. Namespace prefixes, wildcards, predicates, `.` and `..` steps are unsupported. Results follow document traversal order. There is no public `Xml.serialize` or `Xml.deserialize` function.
