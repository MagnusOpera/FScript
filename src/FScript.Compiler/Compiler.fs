namespace FScript.Compiler

open FScript.Language

type CompileInput =
    { Program: Program
      Externs: ExternalFunction list }

type CompiledProgram =
    { Hir: Hir.Program }

module Compiler =
    let compile input =
        try
            input.Program
            |> TypeInfer.inferResolvedProgramWithExterns input.Externs
            |> HirLowering.lower
            |> Result.bind HirValidation.validate
            |> Result.map (fun hir -> { Hir = hir })
        with
        | TypeException error ->
            Error
                [ { Code = "FSH1000"
                    Stage = Hir
                    Message = error.Message
                    Span = Some error.Span } ]
