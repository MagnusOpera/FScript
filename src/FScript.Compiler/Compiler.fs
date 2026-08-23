namespace FScript.Compiler

open FScript.Language

type CompileInput =
    { Program: Program
      Externs: ExternalFunction list }

type CompiledProgram =
    { Hir: Hir.Program
      Lir: Lir.Program }

module Compiler =
    let private descriptor name scheme arity : ExternalFunction =
        { Name = name; Scheme = scheme; Arity = arity; Impl = fun _ _ -> VUnit }

    let private runtimeContext : ExternalFunction =
        descriptor "Env" (Forall([], TNamed "Environment")) 0

    let private taskBindings =
        [ descriptor "Task.spawn" (Forall([ 0 ], TFun(TFun(TUnit, TVar 0), TTask(TVar 0)))) 1
          descriptor "Task.await" (Forall([ 0 ], TFun(TTask(TVar 0), TVar 0))) 1 ]

    let compile input =
        try
            let compilerBindings = runtimeContext :: taskBindings
            let reserved = compilerBindings |> List.map _.Name |> Set.ofList
            let externs = compilerBindings @ (input.Externs |> List.filter (fun external -> not (Set.contains external.Name reserved)))
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
