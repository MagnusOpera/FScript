---
id: stdlib-guid
title: Guid Module
slug: /stdlib/guid
---

`Guid` is provided by the default .NET runtime extern registry.

## `Guid.new : 'a -> string option`

Creates a GUID and returns it as a hyphenated string, for example `Some "b91b7ea0-8754-4cc3-ae37-068bed387017"`. The argument is ignored; use `()` to express that no input is needed.

```fsharp
let id = Guid.new ()
```

The polymorphic argument is part of the current signature; this function is not a script-side constructor for a CLR `Guid`. The returned value is an ordinary FScript string inside an option.
