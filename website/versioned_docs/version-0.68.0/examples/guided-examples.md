---
id: guided-examples
title: Guided Examples
slug: /examples/guided-examples
---

This chapter is example-first: each section includes complete code you can copy into a `.fss` file and run directly.

## 1) Hello with script arguments

This first example introduces:

- reading CLI arguments through `Env.Arguments`,
- pattern matching on lists,
- string interpolation.

```fsharp
let name =
  match Env.Arguments with
  | head :: _ -> head
  | _ -> "Unknown"

print $"Hello {name} !"
```

Run it:

```bash
fscript hello.fss -- Ada
```

## 2) FizzBuzz with `for` + `match`

This example introduces:

- helper functions,
- modulo checks,
- tuple matching in `match`.

```fsharp
let fizz n = n % 3 = 0
let buzz n = n % 5 = 0

for n in [0..20] do
  match (fizz n, buzz n) with
  | (true, true) -> print "Fizz Buzz"
  | (true, _) -> print "Fizz"
  | (_, true) -> print "Buzz"
  | _ -> print $"{n}"
```

## 3) Collections and pattern matching

This example introduces:

- `List.filter`, `List.map`, `List.fold`,
- `List.tryHead` and option matching,
- head/tail list patterns,
- map-pattern removal with `{ [key] = _; ..rest }`.

```fsharp
let values = [1;2;3;4;5;6]

let evens = values |> List.filter (fun n -> n % 2 = 0)
let doubled = evens |> List.map (fun n -> n * 2)
let total = doubled |> List.fold (fun acc -> fun n -> acc + n) 0

print $"evens={evens}"
print $"doubled={doubled}"
print $"total={total}"

let firstEven = evens |> List.tryHead
match firstEven with
| Some x -> print $"first even: {x}"
| None -> print "no even value"

match doubled with
| head :: tail -> print $"head={head} tail={tail}"
| [] -> print "empty"

let scores = { ["a"] = 1; ["b"] = 2 }
let remove k m =
  match m with
  | { [key] = _; ..rest } when key = k -> rest
  | _ -> m

let removedB = remove "b" scores
print $"scores without b = {removedB}"
```

## 4) Recursive records and traversal

This example introduces:

- recursive record types (`type rec`),
- optional child nodes,
- recursive tree traversal.

```fsharp
type rec Node =
  { Value: int
    Left: Node option
    Right: Node option }

let leaf1 = { Value = 4; Left = None; Right = None }
let leaf2 = { Value = 5; Left = None; Right = None }
let leaf3 = { Value = 6; Left = None; Right = None }
let leaf4 = { Value = 7; Left = None; Right = None }

let n2 = { Value = 2; Left = Some leaf1; Right = Some leaf2 }
let n3 = { Value = 3; Left = Some leaf3; Right = Some leaf4 }
let root = { Value = 1; Left = Some n2; Right = Some n3 }

let rec explore visit (node: Node) =
  visit node

  let exploreChild child =
    match child with
    | Some n -> explore visit n
    | _ -> ()

  exploreChild node.Left
  exploreChild node.Right

let displayNode (node: Node) =
  print $"{node.Value}"

explore displayNode root
```

## 5) Imports + exports (multi-file)

This example introduces:

- `import "..." as Alias`,
- top-level `[<export>]` bindings,
- using imported record types in exported functions.

### File: `includes/common.fss`

```fsharp
type ProjectInfo = { Name: string; Language: string }

let describe_project project =
  $"{project.Name} ({project.Language})"

let describe_command cmd =
  $"step={cmd}"

let join_with_comma items =
  String.concat ", " items
```

### File: `imports-and-exports.fss`

```fsharp
import "includes/common.fss" as Common

[<export>] let extension_name = "demo"

[<export>] let summary (project: Common.ProjectInfo) =
  let steps = [ "build"; "test"; "publish" ] |> List.map Common.describe_command
  $"{Common.describe_project project}: {Common.join_with_comma steps}"

let main =
  let project = { Name = "Terrabuild"; Language = "F#" }
  print (summary project)
  print $"Extension: {extension_name}"

main
```

## Suggested order

1. Run example 1 and 2 to get comfortable with flow and syntax.
2. Run example 3 to practice pattern matching on real data.
3. Run example 4 to practice recursion with typed data.
4. Run example 5 when you are ready for multi-file scripts and host-facing exports.
