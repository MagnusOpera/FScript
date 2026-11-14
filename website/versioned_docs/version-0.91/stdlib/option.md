---
id: stdlib-option
title: Option Module
slug: /stdlib/option
---

**Hosted interpreter permissions:** Every function and value on this page requires no host permissions. Higher-order functions still enforce the requirements of their callbacks. These checks do not apply to CLI execution or compilation. See [Host permissions](../embedding/host-permissions).

`option` is a native type with two cases: `Some value` and `None`.

## `Option.defaultValue : 'a -> 'a option -> 'a`

<a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a>

Returns option value or a fallback.

```fsharp
let port = Some 8080 |> Option.defaultValue 80
```

## `Option.defaultWith : (unit -> 'a) -> 'a option -> 'a`

<a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a>

Computes fallback lazily.

```fsharp
let value = None |> Option.defaultWith (fun () -> 42)
```

## `Option.isNone : 'a option -> bool`

<a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a>

Returns `true` when option is `None`.

```fsharp
let missing = None |> Option.isNone
```

## `Option.isSome : 'a option -> bool`

<a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a>

Returns `true` when option is `Some`.

```fsharp
let present = Some "x" |> Option.isSome
```

## `Option.map : ('a -> 'b) -> 'a option -> 'b option`

<a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a>

Transforms value inside `Some`.

```fsharp
let upper = Some "fscript" |> Option.map String.toUpper
```

## Pure support additions

### `Option.bind`

<a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a>

```fsharp
Option.bind : ('a -> 'b option) -> 'a option -> 'b option
```

Calls the binder only for `Some`; its option result is returned directly.

```fsharp
Option.bind Int.tryParse (Some "42")
// Some 42
```

### `Option.flatten`

<a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a>

```fsharp
Option.flatten : 'a option option -> 'a option
```

Removes one nested option layer. Both `None` and `Some None` produce `None`.

```fsharp
Option.flatten (Some (Some 2))
// Some 2
```

### `Option.filter`

<a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a>

```fsharp
Option.filter : ('a -> bool) -> 'a option -> 'a option
```

Retains a present value only when the predicate is true. A missing value makes no callback call.

```fsharp
Option.filter (fun x -> x>1) (Some 1)
// None
```

### `Option.orElse`

<a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a>

```fsharp
Option.orElse : 'a option -> 'a option -> 'a option
```

Uses the fallback when the final argument is `None`. Arguments are evaluated eagerly; use `orElseWith` for lazy fallback computation.

```fsharp
Option.orElse (Some 2) None
// Some 2
```

### `Option.orElseWith`

<a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a>

```fsharp
Option.orElseWith : (unit -> 'a option) -> 'a option -> 'a option
```

Calls the fallback with unit only when the final argument is `None`.

```fsharp
Option.orElseWith (fun () -> Some 2) None
// Some 2
```

### `Option.map2`

<a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a>

```fsharp
Option.map2 : ('a -> 'b -> 'd) -> 'a option -> 'b option -> 'd option
```

Calls the mapper only when both options are present; otherwise returns `None`.

```fsharp
Option.map2 (fun a b -> a+b) (Some 1) (Some 2)
// Some 3
```

### `Option.toList`

<a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a>

```fsharp
Option.toList : 'a option -> 'a list
```

Returns a singleton list for `Some` or an empty list for `None`.

```fsharp
Option.toList (Some 1)
// [1]
```
