namespace FScript.Compiler

open FScript.Language

type CompileInput =
    { Program: Program
      Externs: CompilerExtern list }

type CompiledProgram =
    { Hir: Hir.Program
      Lir: Lir.Program }

module Compiler =
    let private descriptor name scheme arity kind : CompilerExtern =
        { Name = name; Scheme = scheme; Arity = arity; Kind = kind }

    let private runtimeContext : CompilerExtern =
        descriptor "Env" (Forall([], TNamed "Environment")) 0 RuntimeContextIntrinsic

    let private taskBindings =
        [ descriptor "Task.spawn" (Forall([ 0 ], TFun(TFun(TUnit, TVar 0), TTask(TVar 0)))) 1 UnsupportedHostExtern
          descriptor "Task.await" (Forall([ 0 ], TFun(TTask(TVar 0), TVar 0))) 1 UnsupportedHostExtern ]

    let private runtimeIntrinsics =
        RuntimeIntrinsics.schemes
        |> Map.toList
        |> List.map (fun (name, scheme) -> descriptor name scheme 1 PortableRuntimeIntrinsic)

    let compile input =
        try
            let compilerBindings = runtimeContext :: (runtimeIntrinsics @ taskBindings)
            let reserved = compilerBindings |> List.map _.Name |> Set.ofList
            let externs = compilerBindings @ (input.Externs |> List.filter (fun external -> not (Set.contains external.Name reserved)))
            let inferenceExterns =
                externs
                |> List.map (fun external ->
                    ({ Name = external.Name; Scheme = external.Scheme; Arity = external.Arity; Impl = fun _ _ -> VUnit }: ExternalFunction))
            input.Program
            |> TypeInfer.inferResolvedProgramWithExterns inferenceExterns
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
