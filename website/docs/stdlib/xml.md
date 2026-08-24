---
id: stdlib-xml
title: Xml Module
slug: /stdlib/xml
---

The `Xml` module is supplied by the default .NET runtime extern registry.

- `Xml.queryValues : string -> string -> string list option`

The query is a deliberately small path language. It supports element paths, a leading `//`, attribute selection with `/@name`, and text selection with `/text()`.

```fsharp
Xml.queryValues "//person/@name" "<root><person name=\"Ada\"/></root>"
// Some ["Ada"]
```

Malformed XML, invalid queries, namespaces, wildcards, predicates, and general XPath expressions are not supported and return `None`.

`Xml.*` is currently available in the .NET runtime and CLI, but not in generated Go or the LIR interpreter.
