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
        | Lir.StructuralEqual -> Some "=="
        | Lir.NumericLessThan -> Some "<"
        | Lir.NumericGreaterThan -> Some ">"
        | Lir.NumericLessThanOrEqual -> Some "<="
        | Lir.NumericGreaterThanOrEqual -> Some ">="
        | Lir.BooleanAnd -> Some "&&"
        | Lir.BooleanOr -> Some "||"

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
            match operation op, emitExpression environment left, emitExpression environment right with
            | Some token, Ok l, Ok r ->
                let l, r =
                    match op with
                    | Lir.IntAdd | Lir.IntSubtract | Lir.IntMultiply | Lir.IntDivide | Lir.IntRemainder ->
                        $"fsruntime.Int({l})", $"fsruntime.Int({r})"
                    | _ -> l, r
                Ok $"({l} {token} {r})"
            | None, _, _ -> Error $"Unsupported Go operation {op}"
            | _, Error error, _ | _, _, Error error -> Error error
        | Lir.Operation _ -> Error "Invalid operation arity"

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
                      { RelativePath = "internal/fsruntime/runtime.go"; Content = "package fsruntime\n\nfunc Int(value any) int64 { return value.(int64) }\n" } ]
        with ex ->
            Error [ { Code = "FSB4002"; Stage = GoGeneration; Message = ex.Message; Span = None } ]
