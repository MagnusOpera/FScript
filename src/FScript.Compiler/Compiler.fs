namespace FScript.Compiler

open FScript.Language

type CompileInput =
    { Program: Program
      Externs: ExternalFunction list }

type CompiledProgram =
    { Hir: Hir.Program
      Lir: Lir.Program }

module Compiler =
    let compile input =
        try
            input.Program
            |> TypeInfer.inferResolvedProgramWithExterns input.Externs
            |> HirLowering.lower input.Externs
            |> Result.bind HirValidation.validate
            |> Result.bind (fun hir ->
                hir |> LirLowering.lower |> LirValidation.validate
                |> Result.map (fun lir -> { Hir = hir; Lir = lir }))
        with
        | TypeException error ->
            Error
                [ { Code = "FSH1000"
                    Stage = Hir
                    Message = error.Message
                    Span = Some error.Span } ]
