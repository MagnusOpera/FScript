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
    let compileWithRegistrationAndPurity (registration: ExtensionRegistration) (purity: PurityContracts) input =
        try
            let compilerBindings = CompilerExternCatalog.defaults
            let reserved = compilerBindings |> List.map _.Name |> Set.ofList
            let supplied = input.Externs |> List.map _.Name |> Set.ofList
            let hostBindings = registration.Functions |> List.filter (fun fn -> not (Set.contains fn.Name supplied || Set.contains fn.Name reserved)) |> List.map (fun fn ->
                { Name = fn.Name; Scheme = fn.Scheme; Arity = fn.Arity; Kind = UnsupportedHostExtern })
            let externs = compilerBindings @ (input.Externs |> List.filter (fun external -> not (Set.contains external.Name reserved))) @ hostBindings
            let allowedFunctions = Set.union (hostBindings |> List.map _.Name |> Set.ofList) (Set.difference supplied reserved)
            let allowedTypes = Set.union (registration.Types |> List.map _.Name |> Set.ofList) (registration.Unions |> List.map _.Name |> Set.ofList)
            let unknownFunctions = Set.difference purity.PureFunctions allowedFunctions
            let unknownTypes = Set.difference purity.PureTypes allowedTypes
            if not unknownFunctions.IsEmpty || not unknownTypes.IsEmpty then
                raise (TypeException { Message = "Purity contracts must name registered external functions/types, not protected runtime bindings: " + String.concat ", " (Set.toList (Set.union unknownFunctions unknownTypes)); Span = Span.mk (Span.pos 0 0) (Span.pos 0 0) })
            let inferenceExterns =
                externs
                |> List.map (fun external ->
                    ({ Name = external.Name; Scheme = external.Scheme; Arity = external.Arity; Impl = fun _ _ -> VUnit }: ExternalFunction))
            let resolved = TypeInfer.inferResolvedProgramWithRegistrationAndPurity { registration with Functions = inferenceExterns } purity input.Program
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

    let compileWithRegistration registration input = compileWithRegistrationAndPurity registration PurityContracts.empty input
    let compileWithPurity purity input = compileWithRegistrationAndPurity HostInterop.empty purity input
    let compile input = compileWithRegistration HostInterop.empty input
