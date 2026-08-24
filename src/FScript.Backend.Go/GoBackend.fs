namespace FScript.Compiler

open System
open FScript.Language

type GoOptions = { ModuleName: string }
type GeneratedFile = { RelativePath: string; Content: string }

module GoBackend =
    let private diagnostic code message span =
        { Code = code; Stage = GoGeneration; Message = message; Span = span }

    let rec private unsupportedExpression (expression: Lir.Expression) =
        let first values = values |> List.tryPick unsupportedExpression
        match expression.Node with
            | Lir.HostExternal(name, _) when not (TypedGoEmitter.isTypedHostExternal name) ->
                Some(diagnostic "FSB4006" $"Host external '{name}' is not supported by the Go backend." (Some expression.Span))
            | Lir.Intrinsic name when not (TypedGoEmitter.isTypedIntrinsic name) ->
                Some(diagnostic "FSB4006" $"Runtime intrinsic '{name}' is not supported by the Go backend." (Some expression.Span))
            | Lir.CallClosure(left, right) | Lir.PipeClosure(left, right)
            | Lir.TailCallClosure(left, right) | Lir.TailPipeClosure(left, right)
            | Lir.Cons(left, right) | Lir.Append(left, right)
            | Lir.Range(left, right) | Lir.IndexGet(left, right) -> first [ left; right ]
            | Lir.Branch(condition, whenTrue, whenFalse) -> first [ condition; whenTrue; whenFalse ]
            | Lir.Operation(_, values) | Lir.Tuple values | Lir.List values -> first values
            | Lir.Record fields -> fields |> List.map snd |> first
            | Lir.FieldGet(value, _) | Lir.SomeValue value | Lir.Fail value | Lir.RecordConvert(value, _) -> unsupportedExpression value
            | Lir.Let(_, _, _, binding, body) -> first [ binding; body ]
            | Lir.LetRecursiveGroup(bindings, body) -> first ((bindings |> List.map (fun (_, _, _, value) -> value)) @ [ body ])
            | Lir.DecisionTree(target, cases) ->
                first (target :: (cases |> List.collect (fun case -> (case.Guard |> Option.toList) @ [ case.Body ])))
            | Lir.RecordUpdate(target, fields) -> first (target :: List.map snd fields)
            | Lir.Interpolate parts -> parts |> List.choose (function Lir.ValuePart value -> Some value | _ -> None) |> first
            | Lir.UnionCase(_, _, payload) -> payload |> Option.bind unsupportedExpression
            | Lir.Map entries -> entries |> List.collect (function Lir.KeyValue(key, value) -> [ key; value ] | Lir.Spread value -> [ value ]) |> first
            | Lir.ForEach(_, _, _, source, body) -> first [ source; body ]
            | _ -> None

    let private unsupportedDiagnostic (program: Lir.Program) =
        program.ImportedRecords
        |> List.tryFind (fun record ->
            record.IsUnion
            || record.Fields.IsEmpty
            || (record.Fields |> List.exists (fun (name, fieldType) ->
                String.IsNullOrEmpty name
                || not (Char.IsUpper name.[0])
                || match fieldType with TUnit | TInt | TFloat | TBool | TString -> false | _ -> true)))
        |> Option.map (fun record -> diagnostic "FSB4005" $"Imported Go record '{record.Name}' must have exported, exactly named primitive fields and cannot be a union." (Some record.Span))
        |> Option.orElseWith (fun () ->
            program.Declarations
            |> List.tryPick (fun declaration -> unsupportedExpression declaration.Value)
            |> Option.orElseWith (fun () -> program.Entry |> Option.bind unsupportedExpression))

    let generate options (program: Lir.Program) : Result<GeneratedFile list, CompilerDiagnostic list> =
        match LirValidation.validate program with
        | Error diagnostics -> Error diagnostics
        | Ok validated ->
            match GoMonomorphization.specialize validated with
            | Error diagnostics -> Error diagnostics
            | Ok specialized when TypedGoEmitter.canEmit specialized ->
                match specialized |> GoOptimization.optimize |> LirValidation.validate with
                | Error diagnostics -> Error diagnostics
                | Ok optimized ->
                    try
                        let moduleName = if String.IsNullOrWhiteSpace options.ModuleName then "fscript.generated/program" else options.ModuleName
                        TypedGoEmitter.generate moduleName optimized
                        |> List.map (fun (path, content) -> { RelativePath = path; Content = content })
                        |> Ok
                    with ex -> Error [ diagnostic "FSB4001" ex.Message None ]
            | Ok specialized ->
                match unsupportedDiagnostic specialized with
                | Some value -> Error [ value ]
                | None -> Error [ diagnostic "FSB4001" "The reachable program contains a Go representation that is not supported." None ]
