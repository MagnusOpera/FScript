open System
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open Argu
open CliArgs
open FScript.Language
open FScript.Runtime

let formatSpan (span: Span) =
    match span.Start.File with
    | Some file -> sprintf "(%s:%d:%d)" file span.Start.Line span.Start.Column
    | None -> sprintf "(line %d, col %d)" span.Start.Line span.Start.Column

let runTypedProgram (mode: ScriptHost.ExecutionMode) (externs: ExternalFunction list) (program: Program) =
    let typed = TypeInfer.inferProgramWithExterns externs program
    let result =
        match mode with
        | ScriptHost.Interpreted -> Eval.evalProgramWithExterns externs typed
        | ScriptHost.Compiled ->
            let executable = FScript.compileWithExterns externs typed
            FScript.execute executable
    Console.WriteLine(Pretty.valueToString result)
    0

let escapeFScriptString (value: string) =
    value
        .Replace("\\", "\\\\")
        .Replace("\"", "\\\"")
        .Replace("\r", "\\r")
        .Replace("\n", "\\n")
        .Replace("\t", "\\t")

let splitScriptArguments (argv: string array) =
    match argv |> Array.tryFindIndex (fun value -> value = "--") with
    | None -> argv, []
    | Some index ->
        let cliArgs =
            if index = 0 then [||]
            else argv.[0 .. index - 1]
        let scriptArgs =
            if index + 1 >= argv.Length then []
            else argv.[index + 1 ..] |> Array.toList
        cliArgs, scriptArgs

let extractCompileFlag (argv: string array) =
    let mutable compileFirst = false
    let filtered =
        argv
        |> Array.filter (fun arg ->
            if String.Equals(arg, "--compile", StringComparison.OrdinalIgnoreCase) then
                compileFirst <- true
                false
            else
                true)
    compileFirst, filtered

let resolveExternalFunctions
    (args: ParseResults<CliArgs>)
    (context: HostContext)
    (baseDirectory: string)
    : Result<ExternalFunction list, string> =
    let defaultSources =
        if args.Contains <@ No_Default_Externs @> then
            []
        else
            [ "runtime default externs", Registry.all context ]

    let assemblyInputs = args.GetResults <@ Extern_Assembly @>

    if not assemblyInputs.IsEmpty && not RuntimeFeature.IsDynamicCodeSupported then
        Error "External assembly injection is not supported by this runtime (likely AOT/native)."
    else
        let normalizeAssemblyPath (path: string) =
            if Path.IsPathRooted(path) then
                Path.GetFullPath(path)
            else
                Path.GetFullPath(Path.Combine(baseDirectory, path))

        let uniqueAssemblyPaths =
            let seen = Collections.Generic.HashSet<string>(StringComparer.OrdinalIgnoreCase)
            assemblyInputs
            |> List.map normalizeAssemblyPath
            |> List.filter seen.Add

        let externListType = typeof<ExternalFunction list>

        let tryGetTypes (assembly: Assembly) : Result<System.Type list, string> =
            try
                Ok (assembly.GetTypes() |> Array.toList)
            with
            | :? ReflectionTypeLoadException as ex ->
                ex.Types
                |> Array.choose (fun t ->
                    match t with
                    | null -> None
                    | value -> Some value)
                |> Array.toList
                |> Ok
            | ex ->
                Error ex.Message

        let tryLoadProviderSources (assemblyPath: string) : Result<(string * ExternalFunction list) list, string> =
            if not (File.Exists assemblyPath) then
                Error $"Extern assembly not found: {assemblyPath}"
            else
                try
                    let assembly = Assembly.LoadFrom(assemblyPath)
                    match tryGetTypes assembly with
                    | Error message ->
                        Error $"Failed to inspect assembly '{assemblyPath}': {message}"
                    | Ok types ->
                        let providerMethods : (System.Type * MethodInfo) list =
                            types
                            |> List.collect (fun t ->
                                t.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
                                |> Array.toList
                                |> List.filter (fun m -> m.GetCustomAttributes(typeof<FScriptExternProviderAttribute>, false).Length > 0)
                                |> List.map (fun m -> t, m))

                        if providerMethods.IsEmpty then
                            Error $"No [<FScriptExternProvider>] methods found in assembly '{assemblyPath}'."
                        else
                            let rec collectProviders
                                (remaining: (System.Type * MethodInfo) list)
                                (acc: (string * ExternalFunction list) list)
                                : Result<(string * ExternalFunction list) list, string> =
                                match remaining with
                                | [] -> Ok (List.rev acc)
                                | (declaringType, providerMethod) :: tail ->
                                    if providerMethod.ReturnType <> externListType then
                                        Error $"Invalid extern provider '{declaringType.FullName}.{providerMethod.Name}' in '{assemblyPath}': return type must be ExternalFunction list."
                                    else
                                        let parameters = providerMethod.GetParameters()
                                        let invokeArgsResult : Result<objnull array, string> =
                                            if parameters.Length = 0 then
                                                Ok [||]
                                            elif parameters.Length = 1 && parameters.[0].ParameterType = typeof<HostContext> then
                                                Ok [| box context |]
                                            else
                                                Error $"Invalid extern provider '{declaringType.FullName}.{providerMethod.Name}' in '{assemblyPath}': expected signature unit -> ExternalFunction list or HostContext -> ExternalFunction list."

                                        match invokeArgsResult with
                                        | Error message -> Error message
                                        | Ok invokeArgs ->
                                            try
                                                let rawProvided = providerMethod.Invoke(null, invokeArgs)
                                                if isNull rawProvided then
                                                    Error $"Extern provider '{declaringType.FullName}.{providerMethod.Name}' in '{assemblyPath}' returned null."
                                                else
                                                    match rawProvided with
                                                    | :? (ExternalFunction list) as provided ->
                                                        let sourceName = $"{Path.GetFileName(assemblyPath)}:{declaringType.FullName}.{providerMethod.Name}"
                                                        collectProviders tail ((sourceName, provided) :: acc)
                                                    | _ ->
                                                        Error $"Extern provider '{declaringType.FullName}.{providerMethod.Name}' in '{assemblyPath}' returned an invalid value."
                                            with
                                            | ex ->
                                                Error $"Extern provider '{declaringType.FullName}.{providerMethod.Name}' in '{assemblyPath}' failed: {ex.GetBaseException().Message}"

                            collectProviders providerMethods []
                with
                | ex ->
                    Error $"Failed to load extern assembly '{assemblyPath}': {ex.GetBaseException().Message}"

        let loadedUserSources : Result<(string * ExternalFunction list) list, string> =
            uniqueAssemblyPaths
            |> List.fold (fun state assemblyPath ->
                match state with
                | Error _ as err -> err
                | Ok acc ->
                    match tryLoadProviderSources assemblyPath with
                    | Ok providers -> Ok (acc @ providers)
                    | Error _ as err -> err) (Ok ([] : (string * ExternalFunction list) list))

        loadedUserSources
        |> Result.bind (fun userSources ->
            let allSources = defaultSources @ userSources
            let conflicts =
                allSources
                |> List.collect (fun (sourceName, externs) ->
                    externs |> List.map (fun ext -> ext.Name, sourceName))
                |> List.groupBy fst
                |> List.choose (fun (name, entries) ->
                    if entries.Length > 1 then
                        let details =
                            entries
                            |> List.map snd
                            |> List.distinct
                            |> String.concat ", "
                        Some $"{name} ({details})"
                    else
                        None)

            if conflicts.IsEmpty then
                allSources |> List.collect snd |> Ok
            else
                let conflictSummary = String.concat "; " conflicts
                Error $"External function name conflicts detected: {conflictSummary}")

let environmentPrelude (scriptName: string option) (arguments: string list) =
    let scriptNameExpr =
        match scriptName with
        | Some value -> $"Some \"{escapeFScriptString value}\""
        | None -> "None"

    let argsExpr =
        arguments
        |> List.map (fun arg -> $"\"{escapeFScriptString arg}\"")
        |> String.concat "; "
        |> fun body -> if String.IsNullOrWhiteSpace(body) then "[]" else $"[{body}]"

    $"""
let asEnvironment (value: Environment) = value

let Env = asEnvironment {{ ScriptName = {scriptNameExpr}; Arguments = {argsExpr} }}
"""

let parseEnvironmentPrelude (scriptName: string option) (arguments: string list) =
    environmentPrelude scriptName arguments
    |> FScript.parseWithSourceName (Some "<cli-environment>")

let runFile (mode: ScriptHost.ExecutionMode) (externs: ExternalFunction list) (rootDirectory: string) (scriptPath: string) (arguments: string list) =
    if not (File.Exists scriptPath) then
        Console.Error.WriteLine($"File not found: {scriptPath}")
        1
    else
        let scriptName =
            match Path.GetFileName(scriptPath) with
            | null
            | "" -> scriptPath
            | value -> value
        let envProgram = parseEnvironmentPrelude (Some scriptName) arguments
        let scriptProgram = FScript.parseFileWithIncludes rootDirectory scriptPath
        let program = envProgram @ scriptProgram
        runTypedProgram mode externs program

let runSource (mode: ScriptHost.ExecutionMode) (externs: ExternalFunction list) (_rootDirectory: string) (source: string) (arguments: string list) =
    let combinedSource = $"{environmentPrelude None arguments}\n{source}"
    let loadOptions: ScriptHost.LoadOptions = { ExecutionMode = mode }
    let loaded = ScriptHost.loadSourceWithOptions loadOptions externs combinedSource
    Console.WriteLine(Pretty.valueToString loaded.LastValue)
    0

let tryGetLastExpressionType (typed: TypeInfer.TypedProgram) =
    typed
    |> List.rev
    |> List.tryPick (function
        | TypeInfer.TSExpr expr -> Some expr.Type
        | _ -> None)

let decomposeFunctionType (t: Type) =
    let rec loop (args: Type list) (current: Type) =
        match current with
        | TFun (arg, ret) -> loop (args @ [ arg ]) ret
        | _ -> args, current
    loop [] t

let closureParameterNames (firstArg: string) (body: Expr) =
    let rec loop (names: string list) (expr: Expr) =
        match expr with
        | ELambda (param, next, _) -> loop (names @ [ param.Name ]) next
        | _ -> names
    loop [ firstArg ] body

let tryFormatFunctionValueWithType (value: Value) (valueType: Type) =
    let argTypes, retType = decomposeFunctionType valueType
    if argTypes.IsEmpty then
        None
    else
        let argNames =
            match value with
            | VClosure (argName, body, _) -> closureParameterNames argName body
            | _ -> [ for i in 1 .. argTypes.Length -> $"arg{i}" ]

        let nameCount = argNames.Length
        let normalizedNames =
            if nameCount >= argTypes.Length then argNames |> List.take argTypes.Length
            else argNames @ [ for i in (nameCount + 1) .. argTypes.Length -> $"arg{i}" ]

        let parameters =
            List.zip normalizedNames argTypes
            |> List.map (fun (name, t) -> $"({name}: {Types.typeToString t})")
            |> String.concat " -> "
        Some $"{parameters} -> {Types.typeToString retType}"

let printVersion () =
    let assembly = Assembly.GetExecutingAssembly()
    let informationalVersion =
        assembly.GetCustomAttributes(typeof<AssemblyInformationalVersionAttribute>, false)
        |> Seq.tryHead
        |> Option.map (fun attr -> (attr :?> AssemblyInformationalVersionAttribute).InformationalVersion)

    match informationalVersion with
    | Some value when not (String.IsNullOrWhiteSpace(value)) -> value
    | _ ->
        match assembly.GetName().Version with
        | null -> "0.0.0"
        | v -> v.ToString()

let runRepl (mode: ScriptHost.ExecutionMode) (externs: ExternalFunction list) (_rootDirectory: string) =
    let mutable baseProgram: Program = parseEnvironmentPrelude None []
    let mutable pendingLines: string list = []
    let mutable pendingBlankLines = 0
    let mutable running = true

    Console.CancelKeyPress.Add(fun args ->
        args.Cancel <- true
        running <- false
        Console.WriteLine()
        Environment.Exit(0))

    let tryRunPending () =
        let source = String.concat "\n" pendingLines
        let parsed = FScript.parse source
        let candidate = baseProgram @ parsed
        let typed = TypeInfer.inferProgramWithExterns externs candidate
        let lastValue =
            match mode with
            | ScriptHost.Interpreted -> (Eval.evalProgramWithExternsState externs typed).LastValue
            | ScriptHost.Compiled ->
                let executable = FScript.compileWithExterns externs typed
                (FScript.executeWithState executable).LastValue
        let hasExpression =
            parsed
            |> List.exists (function
                | SExpr _ -> true
                | _ -> false)
        if hasExpression then
            match tryGetLastExpressionType typed with
            | Some t ->
                match tryFormatFunctionValueWithType lastValue t with
                | Some signature -> Console.WriteLine(signature)
                | None -> Console.WriteLine(Pretty.valueToString lastValue)
            | None ->
                Console.WriteLine(Pretty.valueToString lastValue)
        let retained =
            parsed
            |> List.filter (function
                | SExpr _ -> false
                | _ -> true)
        baseProgram <- baseProgram @ retained
        pendingLines <- []
        pendingBlankLines <- 0

    while running do
        if pendingLines.IsEmpty then
            Console.Write("> ")
        else
            Console.Write(". ")

        match Console.ReadLine() with
        | null ->
            if not pendingLines.IsEmpty then
                try
                    tryRunPending ()
                with
                | ParseException err ->
                    Console.Error.WriteLine($"Parse error {formatSpan err.Span}: {err.Message}")
                | TypeException err ->
                    Console.Error.WriteLine($"Type error {formatSpan err.Span}: {err.Message}")
                | EvalException err ->
                    Console.Error.WriteLine($"Eval error {formatSpan err.Span}: {err.Message}")
            running <- false
        | line when String.IsNullOrWhiteSpace(line) ->
            if not pendingLines.IsEmpty then
                pendingBlankLines <- pendingBlankLines + 1
                if pendingBlankLines >= 2 then
                    try
                        tryRunPending ()
                    with
                    | ParseException err ->
                        Console.Error.WriteLine($"Parse error {formatSpan err.Span}: {err.Message}")
                        pendingLines <- []
                        pendingBlankLines <- 0
                    | TypeException err ->
                        Console.Error.WriteLine($"Type error {formatSpan err.Span}: {err.Message}")
                        pendingLines <- []
                        pendingBlankLines <- 0
                    | EvalException err ->
                        Console.Error.WriteLine($"Eval error {formatSpan err.Span}: {err.Message}")
                        pendingLines <- []
                        pendingBlankLines <- 0
        | line ->
            pendingBlankLines <- 0
            pendingLines <- pendingLines @ [ line ]
            try
                tryRunPending ()
            with
            | ParseException _ ->
                ()
            | TypeException err ->
                Console.Error.WriteLine($"Type error {formatSpan err.Span}: {err.Message}")
                pendingLines <- []
                pendingBlankLines <- 0
            | EvalException err ->
                Console.Error.WriteLine($"Eval error {formatSpan err.Span}: {err.Message}")
                pendingLines <- []
                pendingBlankLines <- 0
    0

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CliArgs>(programName = "fscript")
    let cliArgvRaw, scriptArguments = splitScriptArguments argv
    let compileFirst, cliArgv = extractCompileFlag cliArgvRaw
    try
        if scriptArguments.IsEmpty && cliArgv.Length = 1 && String.Equals(cliArgv.[0], "version", StringComparison.OrdinalIgnoreCase) then
            Console.WriteLine(printVersion ())
            0
        else
            let args = parser.ParseCommandLine(inputs = cliArgv, raiseOnUsage = true)

            let currentDirectory = Directory.GetCurrentDirectory()
            let scriptPath =
                args.TryGetResult <@ Script @>
                |> Option.map Path.GetFullPath

            let defaultRoot =
                match scriptPath with
                | Some path ->
                    match Path.GetDirectoryName(path) with
                    | null
                    | "" -> currentDirectory
                    | dir -> dir
                | None -> currentDirectory

            let rootDirectory =
                args.TryGetResult <@ Root @>
                |> Option.map Path.GetFullPath
                |> Option.defaultValue defaultRoot
            let context : HostContext = { RootDirectory = rootDirectory; DeniedPathGlobs = [] }
            let externsResult = resolveExternalFunctions args context currentDirectory
            let executionMode =
                if compileFirst || args.Contains <@ Compile @> then ScriptHost.Compiled else ScriptHost.Interpreted

            try
                match externsResult with
                | Error message ->
                    Console.Error.WriteLine(message)
                    1
                | Ok externs ->
                    match scriptPath with
                    | Some path ->
                        runFile executionMode externs rootDirectory path scriptArguments
                    | None when Console.IsInputRedirected ->
                        let source = Console.In.ReadToEnd()
                        runSource executionMode externs rootDirectory source scriptArguments
                    | None ->
                        if not scriptArguments.IsEmpty then
                            Console.Error.WriteLine("Script arguments require file or stdin mode. Use 'fscript <script.fss> -- <args...>' or pipe stdin with '--'.")
                            1
                        else
                            runRepl executionMode externs rootDirectory
            with
            | ParseException err ->
                Console.Error.WriteLine($"Parse error {formatSpan err.Span}: {err.Message}")
                1
            | TypeException err ->
                Console.Error.WriteLine($"Type error {formatSpan err.Span}: {err.Message}")
                2
            | EvalException err ->
                Console.Error.WriteLine($"Eval error {formatSpan err.Span}: {err.Message}")
                3
    with
    | :? ArguParseException as ex ->
        Console.Error.WriteLine(ex.Message)
        1
