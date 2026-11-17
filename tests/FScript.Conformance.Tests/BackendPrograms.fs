namespace FScript.Conformance.Tests

open FsCheck.FSharp

/// A closed, typed grammar. No parser/checker calls, rejection, or filtering are
/// used to generate programs. Binders always bind an int named x; nested binders
/// may shadow x, and shrinking keeps the binder around its body.
module internal BackendPrograms =
    type Ground = Integer | Boolean | Text
    type Expression =
        | Number of int | Flag of bool | String of string | Variable
        | Add of Expression * Expression | Subtract of Expression * Expression
        | Equal of Expression * Expression
        | Choose of Expression * Expression * Expression
        | Apply of Expression * Expression

    let rec ground = function
        | Number _ | Variable | Add _ | Subtract _ | Apply _ -> Integer
        | Flag _ | Equal _ -> Boolean
        | String _ -> Text
        | Choose(_, yes, _) -> ground yes

    let literal n = if n < 0 then $"(0 - {-n})" else string n
    let quote (s: string) =
        "\"" + s.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n").Replace("\t", "\\t").Replace("\r", "\\r") + "\""
    let rec render = function
        | Number n -> literal n | Flag b -> if b then "true" else "false"
        | String s -> quote s | Variable -> "x"
        | Add(a, b) -> $"({render a} + {render b})"
        | Subtract(a, b) -> $"({render a} - {render b})"
        | Equal(a, b) -> $"({render a} = {render b})"
        | Choose(c, a, b) -> $"(if {render c} then {render a} else {render b})"
        | Apply(argument, body) -> $"((fun (x: int) -> {render body}) {render argument})"

    let rec generate depth bound kind =
        let atom =
            match kind with
            | Integer ->
                let number = Gen.choose(-12, 12) |> Gen.map Number
                if bound then Gen.oneof [Gen.constant Variable; number] else number
            | Boolean -> Gen.elements [Flag false; Flag true]
            | Text -> Gen.elements [""; "a"; "quote\""; "a\\b"; "line\nnext"; "é中🙂"] |> Gen.map String
        if depth = 0 then atom
        else
            let child = generate (depth - 1) bound
            let pair constructor ty = Gen.map2 (fun a b -> constructor(a, b)) (child ty) (child ty)
            let conditional = gen {
                let! c = child Boolean
                let! a = child kind
                let! b = child kind
                return Choose(c, a, b)
            }
            let composites =
                match kind with
                | Integer -> [pair Add Integer; pair Subtract Integer;
                              Gen.map2 (fun a b -> Apply(a, b)) (child Integer) (generate (depth - 1) true Integer)]
                | Boolean -> [pair Equal Integer; pair Equal Text; pair Equal Boolean]
                | Text -> []
            Gen.oneof (atom :: conditional :: composites)

    let shrinkNumber n = seq {
        if n <> 0 then yield 0
        if n / 2 <> 0 then yield n / 2
    }
    let rec shrink = function
        | Number n -> shrinkNumber n |> Seq.map Number
        | Flag true -> Seq.singleton (Flag false)
        | String s when s <> "" -> Seq.singleton (String "")
        | Add(a,b) -> shrinkPair Add a b [a;b]
        | Subtract(a,b) -> shrinkPair Subtract a b [a;b]
        | Equal(a,b) -> shrinkPair Equal a b [Flag false]
        | Choose(c,a,b) -> seq {
            yield a; yield b
            for c in shrink c do yield Choose(c,a,b)
            for a in shrink a do yield Choose(c,a,b)
            for b in shrink b do yield Choose(c,a,b)
          }
        | Apply(a,b) -> seq {
            // Extracting b would leave free x references. Keep its binding.
            yield a
            for a in shrink a do yield Apply(a,b)
            for b in shrink b do yield Apply(a,b)
          }
        | _ -> Seq.empty
    and shrinkPair constructor a b direct = seq {
        yield! direct
        for a in shrink a do yield constructor(a,b)
        for b in shrink b do yield constructor(a,b)
    }

    type Shape = Scalars | Closures | GenericData | Collections | Recursion | Effects | Raised | DivideByZero | Patterns
    let shapes = [Scalars; Closures; GenericData; Collections; Recursion; Effects; Raised; DivideByZero; Patterns]
    type Program = { Shape: Shape; Int: Expression; Bool: Expression; Text: Expression; Items: Expression list; Bound: int }

    let source p =
        let n, b, s = render p.Int, render p.Bool, render p.Text
        let items = "(integers [" + (p.Items |> List.map render |> String.concat "; ") + "])"
        let body =
            match p.Shape with
            | Scalars -> $"({n}, {b}, {s}, (if {b} then Some {n} else None), {items})"
            | Closures ->
                $"let identity value = value\nlet capture offset = fun value -> offset + value\nlet add = capture {n}\n(identity (add {p.Bound}), identity {s}, identity {b}, List.map add {items})"
            | GenericData ->
                $"type Box<'a> = {{ Content: 'a }}\ntype Choice<'a> = | Missing | Found of Box<'a>\nlet box<'a> (v: 'a) : Box<'a> = {{ Content = v }}\nlet get fallback item = match item with | Missing -> fallback | Found b -> b.Content\n(get ({n} + {p.Bound} + 1) (if {b} then Found (box {n}) else Missing), get \"fallback\" (Found (box {s})), Found (box {b}), box (Some {items}))"
            | Collections ->
                $"let offset = {n}\nlet values = List.map (fun x -> x + offset) {items}\nlet lookup = {{ [\"value\"] = values }}\n(List.filter (fun x -> x > 0) values, List.fold (fun acc x -> acc + x) 0 values, lookup[\"value\"], {b})"
            | Recursion ->
                $"let rec sum n acc = if n = 0 then acc else sum (n - 1) (acc + n)\n(sum {p.Bound} {n}, {s})"
            | Effects ->
                $"let emit value =\n    Console.writeLine value\n    value\nlet first = emit {s}\nlet second = emit \"after\"\nlet values = List.map (fun x ->\n    Console.writeLine \"item\"\n    x + {n}) {items}\n(first, second, values)"
            | Raised -> $"let message = {s}\nlet fail (value: int) : int = raise message\nif {b} then fail {n} else fail {p.Bound}"
            | DivideByZero -> $"let denominator = {n}\n{p.Bound} / (denominator - denominator)"
            | Patterns -> $"match (Some {n}, {b}) with | (Some value, true) when value > 0 -> value | (Some value, _) -> value + {p.Bound} | (None, _) -> {p.Bound}"
        "let integers (values: int list) = values\n" + body

    let shrinkProgram p =
        seq {
            for n in shrink p.Int do yield {p with Int=n}
            for b in shrink p.Bool do yield {p with Bool=b}
            for s in shrink p.Text do yield {p with Text=s}
            for n in shrinkNumber p.Bound do yield {p with Bound=n}
            if not p.Items.IsEmpty then yield {p with Items=[]}
            for index in 0 .. p.Items.Length - 1 do
                yield {p with Items=List.removeAt index p.Items}
                for item in shrink p.Items[index] do yield {p with Items=List.updateAt index item p.Items}
        } |> Seq.distinct

    let arbitrary shape =
        let generator = Gen.sized (fun size -> gen {
            let depth = min 3 (size / 8)
            let! number = generate depth false Integer
            let! boolean = generate depth false Boolean
            let! text = generate depth false Text
            let! count = Gen.choose(0, min 4 size)
            let! items = Gen.listOfLength count (generate depth false Integer)
            let! bound = Gen.choose(0, 6)
            return { Shape=shape; Int=number; Bool=boolean; Text=text; Items=items; Bound=bound }
        })
        Arb.fromGenShrink(generator, shrinkProgram)
