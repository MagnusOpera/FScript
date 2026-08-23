namespace FScript.Compiler

open System
open System.Text
open FScript.Language

type GoOptions = { ModuleName: string }
type GeneratedFile = { RelativePath: string; Content: string }

module GoBackend =
    let rec private goType = function
        | TUnit -> "struct{}"
        | TInt -> "int64"
        | TFloat -> "float64"
        | TBool -> "bool"
        | TString -> "string"
        | TVar _ -> "any"
        | TFun(argument, result) -> $"func({goType argument}) {goType result}"
        | TTuple _ -> "[]any"
        | TList _ -> "[]any"
        | TOption _ -> "fsruntime.Option"
        | TRecord _ -> "map[string]any"
        | value -> invalidArg "value" $"Unsupported Go representation for {Types.typeToString value}."

    let private identifier prefix id = $"{prefix}{Lir.symbolNumber id}"

    let private constant = function
        | Lir.Unit -> "struct{}{}"
        | Lir.Int value -> $"int64({value})"
        | Lir.Float value -> value.ToString("R", Globalization.CultureInfo.InvariantCulture)
        | Lir.Bool true -> "true"
        | Lir.Bool false -> "false"
        | Lir.String value -> System.Text.Json.JsonSerializer.Serialize(value)

    let private operation = function
        | Lir.IntAdd | Lir.FloatAdd -> Some "+"
        | Lir.IntSubtract | Lir.FloatSubtract -> Some "-"
        | Lir.IntMultiply | Lir.FloatMultiply -> Some "*"
        | Lir.IntDivide | Lir.FloatDivide -> Some "/"
        | Lir.IntRemainder -> Some "%"
        | Lir.StructuralEqual -> None
        | Lir.NumericLessThan -> Some "<"
        | Lir.NumericGreaterThan -> Some ">"
        | Lir.NumericLessThanOrEqual -> Some "<="
        | Lir.NumericGreaterThanOrEqual -> Some ">="
        | Lir.BooleanAnd -> Some "&&"
        | Lir.BooleanOr -> Some "||"

    let private cast valueType code =
        match valueType with
        | TInt -> $"fsruntime.Int({code})"
        | TFloat -> $"fsruntime.Float({code})"
        | TBool -> $"fsruntime.Bool({code})"
        | TString -> $"fsruntime.String({code})"
        | TList _ | TTuple _ -> $"fsruntime.List({code})"
        | TRecord _ -> $"fsruntime.Record({code})"
        | TOption _ -> $"fsruntime.AsOption({code})"
        | _ -> code

    let rec private patternBindings (pattern: Lir.Pattern) =
        match pattern.Node with
        | Lir.Binding(id, _) -> [ id ]
        | Lir.ConsPattern(head, tail) -> patternBindings head @ patternBindings tail
        | Lir.TuplePattern items -> List.collect patternBindings items
        | Lir.RecordPattern fields -> fields |> List.collect (snd >> patternBindings)
        | Lir.SomePattern item -> patternBindings item
        | _ -> []

    let rec private emitPattern valueCode (pattern: Lir.Pattern) =
        let combine parts =
            let tests, assignments = List.unzip parts
            let test = tests |> List.filter ((<>) "true") |> function [] -> "true" | values -> String.concat " && " values
            test, List.concat assignments
        match pattern.Node with
        | Lir.Wildcard -> "true", []
        | Lir.Binding(id, _) ->
            let name = identifier "match" id
            "true", [ $"{name} := {cast pattern.Type valueCode}; _ = {name}" ]
        | Lir.LiteralPattern value -> $"fsruntime.Equal({valueCode}, {constant value})", []
        | Lir.EmptyList -> $"len(fsruntime.List({valueCode})) == 0", []
        | Lir.NonePattern -> $"!fsruntime.IsSome({valueCode})", []
        | Lir.SomePattern item ->
            let test, assignments = emitPattern $"fsruntime.OptionValue({valueCode})" item
            $"fsruntime.IsSome({valueCode}) && ({test})", assignments
        | Lir.ConsPattern(head, tail) ->
            let headPart = emitPattern $"fsruntime.List({valueCode})[0]" head
            let tailPart = emitPattern $"fsruntime.Tail({valueCode})" tail
            let test, assignments = combine [ headPart; tailPart ]
            $"len(fsruntime.List({valueCode})) > 0 && ({test})", assignments
        | Lir.TuplePattern items ->
            items |> List.mapi (fun index item -> emitPattern $"fsruntime.List({valueCode})[{index}]" item)
            |> combine |> fun (test, assignments) -> $"len(fsruntime.List({valueCode})) == {items.Length} && ({test})", assignments
        | Lir.RecordPattern fields ->
            fields |> List.map (fun (name, item) ->
                let quoted = System.Text.Json.JsonSerializer.Serialize(name)
                let test, assignments = emitPattern $"fsruntime.Record({valueCode})[{quoted}]" item
                $"fsruntime.HasField({valueCode}, {quoted}) && ({test})", assignments)
            |> combine

    let rec private emitExpression environment (expression: Lir.Expression) =
        match expression.Node with
        | Lir.Constant value -> Ok(constant value)
        | Lir.Load id ->
            environment |> Map.tryFind id
            |> Option.map Ok
            |> Option.defaultValue (Error(sprintf "Unknown symbol %%%d" (Lir.symbolNumber id)))
        | Lir.MakeClosure(parameter, _, parameterType, body) ->
            let name = identifier "arg" parameter
            emitExpression (environment |> Map.add parameter name) body
            |> Result.map (fun bodyCode -> $"func({name} {goType parameterType}) {goType body.Type} {{ return {bodyCode} }}")
        | Lir.CallClosure(fn, argument) ->
            match emitExpression environment fn, emitExpression environment argument with
            | Ok fnCode, Ok argumentCode -> Ok $"({fnCode})({argumentCode})"
            | Error error, _ | _, Error error -> Error error
        | Lir.Branch(condition, whenTrue, whenFalse) ->
            match emitExpression environment condition, emitExpression environment whenTrue, emitExpression environment whenFalse with
            | Ok c, Ok t, Ok f -> Ok $"func() {goType expression.Type} {{ if {c} {{ return {t} }}; return {f} }}()"
            | Error error, _, _ | _, Error error, _ | _, _, Error error -> Error error
        | Lir.Operation(op, [ left; right ]) ->
            match emitExpression environment left, emitExpression environment right with
            | Ok l, Ok r when op = Lir.StructuralEqual -> Ok $"fsruntime.Equal({l}, {r})"
            | Ok l, Ok r ->
                match operation op with
                | Some token ->
                    let l, r =
                        match op with
                        | Lir.IntAdd | Lir.IntSubtract | Lir.IntMultiply | Lir.IntDivide | Lir.IntRemainder ->
                            $"fsruntime.Int({l})", $"fsruntime.Int({r})"
                        | _ -> l, r
                    Ok $"({l} {token} {r})"
                | None -> Error $"Unsupported Go operation {op}"
            | Error error, _ | _, Error error -> Error error
        | Lir.Tuple items | Lir.List items ->
            items |> List.map (emitExpression environment) |> collect
            |> Result.map (fun values ->
                let joined = String.concat ", " values
                $"[]any{{{joined}}}")
        | Lir.Record fields ->
            fields |> List.map (fun (name, value) -> emitExpression environment value |> Result.map (fun code -> $"{System.Text.Json.JsonSerializer.Serialize(name)}: {code}"))
            |> collect |> Result.map (fun values ->
                let joined = String.concat ", " values
                $"map[string]any{{{joined}}}")
        | Lir.FieldGet(target, name) ->
            emitExpression environment target |> Result.map (fun code -> $"({code})[{System.Text.Json.JsonSerializer.Serialize(name)}]")
        | Lir.SomeValue item -> emitExpression environment item |> Result.map (fun code -> $"fsruntime.Some({code})")
        | Lir.NoneValue -> Ok "fsruntime.None()"
        | Lir.Cons(head, tail) ->
            match emitExpression environment head, emitExpression environment tail with
            | Ok h, Ok t -> Ok $"fsruntime.Cons({h}, {t})"
            | Error error, _ | _, Error error -> Error error
        | Lir.Append(left, right) ->
            match emitExpression environment left, emitExpression environment right with
            | Ok l, Ok r -> Ok $"fsruntime.Append({l}, {r})"
            | Error error, _ | _, Error error -> Error error
        | Lir.Let(id, _, recursive, binding, body) ->
            let name = identifier "local" id
            let scoped = environment |> Map.add id name
            match emitExpression scoped binding, emitExpression scoped body with
            | Ok bindingCode, Ok bodyCode ->
                let declaration =
                    if recursive then $"var {name} {goType binding.Type}; {name} = {bindingCode}"
                    else $"{name} := {bindingCode}"
                Ok $"func() {goType expression.Type} {{ {declaration}; return {bodyCode} }}()"
            | Error error, _ | _, Error error -> Error error
        | Lir.Fail value ->
            emitExpression environment value
            |> Result.map (fun code -> $"func() {goType expression.Type} {{ fsruntime.Fail({code}); var zero {goType expression.Type}; return zero }}()")
        | Lir.DecisionTree(target, cases) ->
            emitExpression environment target |> Result.bind (fun targetCode ->
                let mutable failure = None
                let clauses = StringBuilder()
                for case in cases do
                    let bindings = patternBindings case.Pattern
                    let caseEnvironment = bindings |> List.fold (fun env id -> Map.add id (identifier "match" id) env) environment
                    match case.Guard |> Option.map (emitExpression caseEnvironment) |> Option.defaultValue (Ok "true"), emitExpression caseEnvironment case.Body with
                    | Ok guardCode, Ok bodyCode ->
                        let test, assignments = emitPattern "matchValue" case.Pattern
                        clauses.Append($"if {test} {{ ") |> ignore
                        assignments |> List.iter (fun assignment -> clauses.Append(assignment).Append("; ") |> ignore)
                        clauses.Append($"if {guardCode} {{ return {bodyCode} }} }}; ") |> ignore
                    | Error error, _ | _, Error error -> failure <- Some error
                match failure with
                | Some error -> Error error
                | None -> Ok $"func() {goType expression.Type} {{ matchValue := {targetCode}; {clauses}fsruntime.Fail(\"Match failure.\"); var zero {goType expression.Type}; return zero }}()")
        | Lir.Operation _ -> Error "Invalid operation arity"

    and private collect<'a> (results: Result<'a, string> list) : Result<'a list, string> =
        results |> List.fold (fun state result ->
            match state, result with Ok values, Ok value -> Ok(values @ [ value ]) | Error error, _ | _, Error error -> Error error) (Ok [])

    let generate options (program: Lir.Program) : Result<GeneratedFile list, CompilerDiagnostic list> =
        try
            let names =
                program.Declarations
                |> List.fold (fun env declaration -> env |> Map.add declaration.Id (identifier "value" declaration.Id)) Map.empty
            let declarations = StringBuilder()
            let mutable error = None
            for declaration in program.Declarations do
                match emitExpression names declaration.Value with
                | Ok code -> declarations.AppendLine($"var {names.[declaration.Id]} = {code}") |> ignore
                | Error message -> error <- Some(message, declaration.Span)
            let entry =
                match program.Entry with
                | None -> Ok "struct{}{}"
                | Some value -> emitExpression names value
            match error, entry with
            | Some(message, span), _ ->
                Error [ { Code = "FSB4001"; Stage = GoGeneration; Message = message; Span = Some span } ]
            | None, Error message ->
                Error [ { Code = "FSB4001"; Stage = GoGeneration; Message = message; Span = None } ]
            | None, Ok entryCode ->
                let moduleName = if String.IsNullOrWhiteSpace options.ModuleName then "fscript.generated/program" else options.ModuleName
                let programSource = $"package fscriptprogram\n\nimport \"{moduleName}/internal/fsruntime\"\n\n{declarations}\nfunc Run() any {{ return {entryCode} }}\n"
                Ok
                    [ { RelativePath = "go.mod"; Content = $"module {moduleName}\n\ngo 1.25\n" }
                      { RelativePath = "main.go"; Content = $"package main\n\nimport (\n\t\"fmt\"\n\t\"{moduleName}/internal/fscriptprogram\"\n)\n\nfunc main() {{ fmt.Println(fscriptprogram.Run()) }}\n" }
                      { RelativePath = "internal/fscriptprogram/program.go"; Content = programSource }
                      { RelativePath = "internal/fsruntime/runtime.go"; Content = "package fsruntime\n\nimport (\n\t\"fmt\"\n\t\"reflect\"\n)\n\ntype Option struct { HasValue bool; Value any }\n\nfunc Int(value any) int64 { return value.(int64) }\nfunc Float(value any) float64 { return value.(float64) }\nfunc Bool(value any) bool { return value.(bool) }\nfunc String(value any) string { return value.(string) }\nfunc List(value any) []any { return value.([]any) }\nfunc Record(value any) map[string]any { return value.(map[string]any) }\nfunc AsOption(value any) Option { return value.(Option) }\nfunc Some(value any) Option { return Option{HasValue: true, Value: value} }\nfunc None() Option { return Option{} }\nfunc IsSome(value any) bool { return AsOption(value).HasValue }\nfunc OptionValue(value any) any { return AsOption(value).Value }\nfunc HasField(value any, name string) bool { _, ok := Record(value)[name]; return ok }\nfunc Equal(left any, right any) bool { return reflect.DeepEqual(left, right) }\nfunc Cons(head any, tail []any) []any { return append([]any{head}, tail...) }\nfunc Append(left []any, right []any) []any { return append(append([]any{}, left...), right...) }\nfunc Tail(value any) []any { return append([]any{}, List(value)[1:]...) }\nfunc Fail(value any) { panic(fmt.Sprint(value)) }\n" } ]
        with ex ->
            Error [ { Code = "FSB4002"; Stage = GoGeneration; Message = ex.Message; Span = None } ]
