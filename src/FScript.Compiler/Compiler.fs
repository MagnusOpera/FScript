namespace FScript.Compiler

open FScript.Language

type CompileInput =
    { Program: Program
      Externs: ExternalFunction list }

type CompiledProgram =
    { Hir: Hir.Program
      Lir: Lir.Program }

module Compiler =
    let private runtimeContext : ExternalFunction =
        { Name = "Env"
          Scheme = Forall([], TNamed "Environment")
          Arity = 0
          Impl = fun _ _ -> VUnit }

    let compile input =
        try
            let externs = runtimeContext :: (input.Externs |> List.filter (fun external -> external.Name <> runtimeContext.Name))
            input.Program
            |> TypeInfer.inferResolvedProgramWithExterns externs
            |> HirLowering.lower externs
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
