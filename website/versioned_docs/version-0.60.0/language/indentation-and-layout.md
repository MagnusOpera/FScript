---
id: indentation-and-layout
title: Indentation and Layout
slug: /language/indentation-and-layout
---

FScript is indentation-aware: layout determines block structure.

This page shows the supported indentation styles you can use confidently.

## 1) Block indentation

After `let`, `if`, `for`, `match`, and `fun`, increasing indentation opens a block.
Dedenting closes it.

```fsharp
let describe n =
  if n > 0 then
    "positive"
  else
    "zero-or-negative"
```

## 2) Function declaration styles

### Single-line parameters

```fsharp
let add x y = x + y
```

### Multi-line parameters (aligned)

Continuation parameter lines must start at the same column.

```fsharp
let formatAddress (address: {| City: string; Zip: int |})
                  (name: string) =
  $"{address.City} ({name})"
```

## 3) Pipeline with multiline lambda

A lambda body can be placed on the next indented line inside call arguments.

```fsharp
let result =
  [0..9]
  |> List.map (fun i ->
      i + 1)
  |> List.filter (fun i -> i % 2 == 0)
```

## 4) `match` case alignment

Case lines in a multiline match must align with each other.

```fsharp
let label value =
  match value with
  | Some x -> $"some:{x}"
  | None -> "none"
```

## 5) Record and map literal layout styles

### Single-line record and map

```fsharp
let person = { Name = "Ada"; Age = 37 }
let ports = { ["http"] = 80; ["https"] = 443 }
```

### Compact multiline record and map

Opening `{` stays on the line of the first field/entry.
Closing `}` stays on the line of the last field/entry.

```fsharp
let person = { Name = "Ada"
               Age = 37 }

let ports = { ["http"] = 80
              ["https"] = 443 }
```

## 6) List layout styles

### Single-line list

```fsharp
let xs = [1; 2; 3]
```

### Compact multiline list

```fsharp
let xs = [1
          2
          3]
```

For non-empty lists, do not put `[` or `]` on standalone lines.

## 7) Union declaration styles

### Single-line union

```fsharp
type Shape = | Point | Circle of int
```

### Multiline union

```fsharp
type Shape =
  | Point
  | Circle of int
```

## 8) Common invalid layout patterns

### Misaligned multiline parameters

```fsharp
// Invalid style: second parameter line is not aligned
let f (x: int)
    (y: int) = x + y
```

### Block delimiter style for non-empty list/map

```fsharp
// Invalid style
let xs =
  [
    1
    2
  ]
```

Use compact list/map/record multiline styles instead.
