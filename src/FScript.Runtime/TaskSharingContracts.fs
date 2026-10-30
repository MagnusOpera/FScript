namespace FScript.Runtime

open System
open System.Reflection
open FScript.Language

/// Optional contracts for standalone and hosted script analysis.
module TaskSharingContracts =
    /// Add known runtime guarantees only for the actual built-in implementations.
    /// A provider rebinding the same name does not inherit these contracts.
    let withRuntime (registration: ExtensionRegistration) (options: TaskSharingAnalysis.Options) =
        let knownFunctions = [ConsoleExterns.write_line; ConsoleExterns.read_line] @ PureExterns.functions @ TaskFunctions.all
        let known =
            registration.Functions
            |> List.choose (fun fn ->
                if knownFunctions |> List.exists (fun builtin -> Object.ReferenceEquals(fn.Impl, builtin.Impl)) then Some fn.Name
                else None)
            |> Set.ofList
        { options with ThreadSafeFunctions = Set.union options.ThreadSafeFunctions known }

    /// Optional concurrency contracts, read only when static task-sharing analysis is requested.
    let fromProviders (providers: MethodInfo list) : TaskSharingAnalysis.Options =
        let names methodName =
            providers
            |> List.map (fun method -> method.DeclaringType |> nonNull)
            |> List.distinct
            |> List.collect (fun typ ->
                match typ.GetMethod(methodName, BindingFlags.Public ||| BindingFlags.Static, null, System.Type.EmptyTypes, null) |> Option.ofObj with
                | None -> []
                | Some factory ->
                    match factory.Invoke(null, [||]) with
                    | :? Collections.Generic.IEnumerable<string> as values ->
                        values |> Seq.map (fun value -> if String.IsNullOrWhiteSpace value then invalidOp $"{methodName} contains an empty binding name." else value) |> Seq.toList
                    | _ -> invalidOp $"{methodName} must return IEnumerable<string>.")
            |> Set.ofList
        { ThreadSafeTypes = names "FScriptThreadSafeTypes"
          ThreadSafeFunctions = names "FScriptThreadSafeFunctions" }
