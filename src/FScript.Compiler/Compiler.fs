namespace FScript.Compiler

open FScript.Language

type CompileInput =
    { Program: Program
      Externs: CompilerExtern list }

type CompiledProgram =
    { Hir: Hir.Program
      Lir: Lir.Program
      ExportedFunctions: Map<string, ExportSignatures.ExportedFunctionSignature> }

module Compiler =
    let compile input =
        try
            let compilerBindings = CompilerExternCatalog.defaults
            let reserved = compilerBindings |> List.map _.Name |> Set.ofList
            let externs = compilerBindings @ (input.Externs |> List.filter (fun external -> not (Set.contains external.Name reserved)))
            let inferenceExterns =
                externs
                |> List.map (fun external ->
                    ({ Name = external.Name; Scheme = external.Scheme; Arity = external.Arity; Impl = fun _ _ -> VUnit }: ExternalFunction))
            let resolved = TypeInfer.inferResolvedProgramWithExterns inferenceExterns input.Program
            let exportedFunctions = ExportSignatures.fromTypedProgram resolved.TypedProgram
            resolved
            |> HirLowering.lower externs
            |> Result.bind HirValidation.validate
            |> Result.bind (fun hir ->
                hir |> LirLowering.lower |> LirOptimization.optimize |> LirValidation.validate
                |> Result.map (fun lir ->
                    { Hir = hir
                      Lir = lir
                      ExportedFunctions = exportedFunctions }))
        with
        | TypeException error ->
            Error
                [ { Code = "FSH1000"
                    Stage = Hir
                    Message = error.Message
                    Span = Some error.Span } ]
