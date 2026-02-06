namespace FScript.Tests

open NUnit.Framework
open FsUnit
open FScript.Core

[<TestFixture>]
type LexerTests () =
    [<Test>]
    member _.``Tokenizes keywords and operators`` () =
        let tokens = Lexer.tokenize "let x = 1 + 2 * 3\n"
        tokens |> List.exists (fun t -> t.Kind = Let) |> should equal true
        tokens |> List.exists (fun t -> t.Kind = Ident "x") |> should equal true
        tokens |> List.exists (fun t -> t.Kind = IntLit 1L) |> should equal true
        tokens |> List.exists (fun t -> t.Kind = Plus) |> should equal true
        tokens |> List.exists (fun t -> t.Kind = Star) |> should equal true

    [<Test>]
    member _.``Skips line comments`` () =
        let tokens = Lexer.tokenize "// comment\nlet x = 1\n"
        tokens |> List.exists (fun t -> t.Kind = Let) |> should equal true
        tokens |> List.exists (fun t -> t.Kind = IntLit 1L) |> should equal true

    [<Test>]
    member _.``Rejects block comments`` () =
        let act () = Lexer.tokenize "(* nope *)" |> ignore
        act |> should throw typeof<ParseException>

    [<Test>]
    member _.``Tokenizes indentation with indent and dedent`` () =
        let tokens = Lexer.tokenize "let f =\n    1\n"
        tokens |> List.exists (fun t -> t.Kind = Indent) |> should equal true
        tokens |> List.exists (fun t -> t.Kind = Dedent) |> should equal true

    [<Test>]
    member _.``Tokenizes escaped strings`` () =
        let tokens = Lexer.tokenize "\"a\\n\\\"b\\\"\""
        tokens
        |> List.exists (fun t ->
            match t.Kind with
            | StringLit s -> s = "a\n\"b\""
            | _ -> false)
        |> should equal true

    [<Test>]
    member _.``Tokenizes pipeline operator`` () =
        let tokens = Lexer.tokenize "1 |> f"
        tokens |> List.exists (fun t -> t.Kind = PipeForward) |> should equal true

    [<Test>]
    member _.``Tokenizes range dots and preserves float and field dot`` () =
        let rangeTokens = Lexer.tokenize "[1..5]"
        rangeTokens |> List.exists (fun t -> t.Kind = RangeDots) |> should equal true

        let floatTokens = Lexer.tokenize "1.0"
        floatTokens |> List.exists (fun t -> t.Kind = FloatLit 1.0) |> should equal true

        let fieldTokens = Lexer.tokenize "p.Name"
        fieldTokens |> List.exists (fun t -> t.Kind = Dot) |> should equal true
