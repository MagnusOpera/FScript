open System
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open Argu
open CliArgs
open FScript.Language
open FScript.Runtime

let writeGeneratedFile (root: string) (file: FScript.Compiler.GeneratedFile) =
    let path = Path.Combine(root, file.RelativePath)
    match Path.GetDirectoryName(path) with
    | null -> ()
    | directory -> Directory.CreateDirectory(directory) |> ignore
    File.WriteAllText(path, file.Content)

let reportTaskSharingWarning (warning: TaskSharingAnalysis.Diagnostic) =
    let location (span: Span) =
        let file = span.Start.File |> Option.defaultValue "<script>"
        $"{file}({span.Start.Line},{span.Start.Column})"
    Console.Error.WriteLine($"{location warning.Span}: warning {warning.Code}: {warning.Message}")
    Console.Error.WriteLine($"  Related task access or capture: {location warning.RelatedSpan}")

let runCompileCommand (argv: string array) =
    let usage () =
        Console.Error.WriteLine("Usage: fscript compile <program.fss> [--target <ir|csharp|dotnet>] [--output <directory>] [--reference <assembly.dll>] [--single-file|--aot] [--runtime <rid>] [--size-optimized|--no-size-optimized] [--no-aot] [--no-single-file] [--no-result]")
        1
    if argv.Length < 2 then usage ()
    else
        let scriptPath = Path.GetFullPath(argv.[1])
        let tryOption name =
            argv |> Array.tryFindIndex ((=) name)
            |> Option.bind (fun index -> if index + 1 < argv.Length then Some argv.[index + 1] else None)
        let sourceRoot = Path.GetDirectoryName(scriptPath) |> Option.ofObj |> Option.defaultValue (Directory.GetCurrentDirectory())
        let allOptions name = argv |> Array.indexed |> Array.choose (fun (index,value) -> if value=name && index+1<argv.Length then Some(Path.GetFullPath argv.[index+1]) else None) |> Array.toList
        let resolved =
            EnvironmentConfiguration.resolveForCli (tryOption "--config") (Array.contains "--no-config" argv) (Directory.GetCurrentDirectory()) sourceRoot None (Array.contains "--no-default-externs" argv) (allOptions "--extern-assembly")
            |> Result.bind (fun settings -> CompileOptions.resolve argv scriptPath settings.Compilation |> Result.map (fun compilation -> settings, compilation))
        match resolved with
        | Ok (settings, compilation) when File.Exists scriptPath ->
            let normalizedTarget = compilation.Backend
            let singleFile = compilation.SingleFile
            let aot = compilation.AheadOfTime
            let minimizeSize = compilation.MinimizeSize
            let executable = singleFile || aot
            let hasRuntime = Array.contains "--runtime" argv
            let requestedRuntime = compilation.RuntimeIdentifier
            let runtime = requestedRuntime
            if not (List.contains normalizedTarget ["ir"; "csharp"; "dotnet"]) then usage ()
            elif Array.contains "--analyze-task-sharing" argv then
                Console.Error.WriteLine("Task-sharing analysis is a separate command: fscript analyze <program.fss>")
                usage ()
            elif normalizedTarget="ir" && Array.contains "--no-result" argv then
                Console.Error.WriteLine("--no-result applies to script execution and C#/ .NET compilation, not IR artifacts.")
                usage ()
            elif minimizeSize && (normalizedTarget<>"dotnet" || not aot) then
                Console.Error.WriteLine("--size-optimized requires --target dotnet --aot. It uses invariant globalization, resource-key framework errors, and disables stack traces and EventSource.")
                usage ()
            elif normalizedTarget<>"dotnet" && (executable || Array.contains "--runtime" argv) then
                Console.Error.WriteLine("--single-file, --aot, and --runtime are only supported with --target dotnet.")
                usage ()
            elif hasRuntime && requestedRuntime.IsNone then
                Console.Error.WriteLine("--runtime requires a value, for example osx-arm64, linux-x64, or win-x64. Omit --runtime to target the current platform.")
                usage ()
            elif not executable && Array.contains "--runtime" argv then
                Console.Error.WriteLine("--runtime requires --single-file or --aot.")
                usage ()
            else
                try
                    let timer = Diagnostics.Stopwatch.StartNew()
                    let report (message: string) = Console.WriteLine(message)
                    let targetDescription =
                        match normalizedTarget with
                        | "dotnet" when aot -> $".NET (Native AOT, {runtime.Value})"
                        | "dotnet" when singleFile -> $".NET (single-file, {runtime.Value})"
                        | "dotnet" -> ".NET"
                        | "csharp" -> "C#"
                        | _ -> "IR"
                    report $"Compiling {argv.[1]} to {targetDescription}..."
                    let outputDirectory = compilation.OutputDirectory
                    let complete path =
                        report $"Compiled successfully in {timer.Elapsed.TotalSeconds:F1}s. Output: {path}"
                        0
                    let program = FScript.parseFileWithIncludes sourceRoot scriptPath
                    settings.ConfigurationPath |> Option.iter (fun path -> report $"Loading configuration {path}...")
                    let loaded = EnvironmentLoader.loadWithInfo settings |> function Ok environment -> environment | Error message -> failwith message
                    let sdkReferences = if loaded.ProviderMethods.IsEmpty then [] else [typeof<FScript.Extensibility.ScriptValue>.Assembly.Location; typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location]
                    let references = allOptions "--reference" @ loaded.AssemblyPaths @ sdkReferences |> List.distinct
                    let purity = FScript.Compiler.ClrBindings.providerPurity (loaded.ProviderMethods |> List.map snd)
                    match FScript.Compiler.Compiler.compileWithRegistrationAndPurity loaded.Registration purity { Program = program; Externs = [] } with
                    | Error diagnostics ->
                        diagnostics |> List.iter (fun diagnostic -> Console.Error.WriteLine($"{diagnostic.Code}: {diagnostic.Message}"))
                        4
                    | Ok compiled ->
                        let lir = loaded.ProviderMethods |> List.fold (fun program (names,method) -> FScript.Compiler.ClrBindings.bindProvider names method program) compiled.Lir
                        let lir =
                            if normalizedTarget="dotnet" || normalizedTarget="csharp" then
                                FScript.Compiler.ClrBindings.bindCompiledProviders (loaded.ProviderMethods |> List.map snd) lir
                            else lir
                        Directory.CreateDirectory(outputDirectory) |> ignore
                        match normalizedTarget with
                        | "ir" ->
                            let baseName =
                                match Path.GetFileNameWithoutExtension(scriptPath) with
                                | null | "" -> "program"
                                | value -> value
                            let name = baseName + ".fsir"
                            report "Writing IR artifact..."
                            File.WriteAllBytes(Path.Combine(outputDirectory, name), FScript.Compiler.IrArtifact.serialize lir)
                            complete (Path.Combine(outputDirectory, name))
                        | "csharp" ->
                            report "Generating C# project..."
                            match FScript.Compiler.CSharpBackend.generateWithResultOutput (not (Array.contains "--no-result" argv)) {Namespace="FScript.Generated"; ClassName="Program"; References=references} lir with
                            | Error diagnostics ->
                                diagnostics |> List.iter (fun d -> Console.Error.WriteLine($"{d.Code}: {d.Message}"))
                                4
                            | Ok files ->
                                files |> List.iter (writeGeneratedFile outputDirectory)
                                complete (Path.Combine(outputDirectory, "Program.csproj"))
                        | "dotnet" ->
                            let options: FScript.Compiler.DotNetOptions =
                                { AssemblyName=Path.GetFileNameWithoutExtension scriptPath |> nonNull; Namespace="FScript.Generated"; ClassName="Program"
                                  OutputKind=FScript.Compiler.ConsoleApplication; References=references }
                            report "Generating .NET assembly..."
                            let generated=
                                if executable then FScript.Compiler.DotNetBackend.generateExecutableWithResultOutputAndProgress (not (Array.contains "--no-result" argv)) report {RuntimeIdentifier=runtime.Value;AheadOfTime=aot;MinimizeSize=minimizeSize} options lir
                                else FScript.Compiler.DotNetBackend.generateWithResultOutput (not (Array.contains "--no-result" argv)) options lir
                            match generated with
                            | Error diagnostics ->
                                diagnostics |> List.iter (fun d -> Console.Error.WriteLine($"{d.Code}: {d.Message}"))
                                4
                            | Ok files ->
                                for file in files do
                                    let path=Path.Combine(outputDirectory,file.RelativePath)
                                    File.WriteAllBytes(path,file.Content)
                                    if executable && not (OperatingSystem.IsWindows()) && not (runtime.Value.StartsWith("win-",StringComparison.Ordinal)) then
                                        File.SetUnixFileMode(path,UnixFileMode.UserRead ||| UnixFileMode.UserWrite ||| UnixFileMode.UserExecute ||| UnixFileMode.GroupRead ||| UnixFileMode.GroupExecute ||| UnixFileMode.OtherRead ||| UnixFileMode.OtherExecute)
                                let name =
                                    if not executable then options.AssemblyName + ".dll"
                                    elif runtime.Value.StartsWith("win-", StringComparison.Ordinal) then options.AssemblyName + ".exe"
                                    else options.AssemblyName
                                complete (Path.Combine(outputDirectory, name))
                        | _ -> usage ()
                with
                | ParseException error -> Console.Error.WriteLine($"Parse error: {error.Message}"); 1
                | TypeException error -> Console.Error.WriteLine($"Type error: {error.Message}"); 2
                | :? UnauthorizedAccessException as ex -> Console.Error.WriteLine($"FSA5101: Cannot write compiler output: {ex.Message}"); 4
                | :? IOException as ex -> Console.Error.WriteLine($"FSA5101: Cannot write compiler output: {ex.Message}"); 4
                | ex -> Console.Error.WriteLine(ex.Message); 4
        | Ok _ -> Console.Error.WriteLine($"File not found: {scriptPath}"); 1
        | Error message -> Console.Error.WriteLine(message); usage ()

let formatSpan (span: Span) =
    match span.Start.File with
    | Some file -> sprintf "(%s:%d:%d)" file span.Start.Line span.Start.Column
    | None -> sprintf "(line %d, col %d)" span.Start.Line span.Start.Column

let runTypedProgram printResult (registration: ExtensionRegistration) (program: Program) =
    let typed = TypeInfer.inferProgramWithRegistration registration program
    let state = Eval.evalProgramWithRegistrationState registration typed
    if printResult then Console.WriteLine(Pretty.valueToString state.LastValue)
    state.ExitCode

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

let runAnalyzeCommand (argv: string array) =
    let reportError (span: Span) message =
        let file = span.Start.File |> Option.defaultValue "<script>"
        Console.Error.WriteLine($"{file}({span.Start.Line},{span.Start.Column}): {message}")
        1
    try
        let parser = ArgumentParser.Create<AnalyzeArgs>(programName = "fscript analyze")
        let args = parser.ParseCommandLine(inputs = argv.[1..], raiseOnUsage = true)
        match args.TryGetResult <@ AnalyzeArgs.Script @> with
        | None -> Console.Error.WriteLine("Usage: fscript analyze <program.fss> [--config <fscript.toml>] [--no-config] [--extern-assembly <provider.dll>]"); 1
        | Some script ->
            let path = Path.GetFullPath script
            let root = Path.GetDirectoryName(path) |> Option.ofObj |> Option.defaultValue (Directory.GetCurrentDirectory())
            let settings =
                EnvironmentConfiguration.resolveForCli (args.TryGetResult <@ AnalyzeArgs.Config @>) (args.Contains <@ AnalyzeArgs.No_Config @>) (Directory.GetCurrentDirectory()) root (args.TryGetResult <@ AnalyzeArgs.Root @>) (args.Contains <@ AnalyzeArgs.No_Default_Externs @>) (args.GetResults <@ AnalyzeArgs.Extern_Assembly @>)
                |> function Ok settings -> settings | Error message -> failwith message
            let loaded = EnvironmentLoader.loadWithInfo settings |> function Ok loaded -> loaded | Error message -> failwith message
            let providers = loaded.ProviderMethods |> List.map snd
            let purity = FScript.Compiler.ClrBindings.providerPurity providers
            let options = TaskSharingContracts.fromProviders providers |> TaskSharingContracts.withRuntime loaded.Registration
            let program = parseEnvironmentPrelude (Some(Path.GetFileName path |> nonNull)) [] @ FScript.parseFileWithIncludes settings.RootDirectory path
            let resolved = TypeInfer.inferResolvedProgramWithRegistrationAndPurity loaded.Registration purity program
            TaskSharingAnalysis.analyze loaded.Registration options resolved |> List.iter reportTaskSharingWarning
            0
    with
    | ParseException error -> reportError error.Span error.Message
    | TypeException error -> reportError error.Span error.Message
    | error -> Console.Error.WriteLine(error.Message); 1

let runFile printResult (registration: ExtensionRegistration) (rootDirectory: string) (scriptPath: string) (arguments: string list) =
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
        runTypedProgram printResult registration program

let runSource printResult (registration: ExtensionRegistration) (_rootDirectory: string) (source: string) (arguments: string list) =
    let combinedSource = $"{environmentPrelude None arguments}\n{source}"
    let loaded = ScriptHost.loadSourceWithRegistration registration combinedSource
    if printResult then Console.WriteLine(Pretty.valueToString loaded.LastValue)
    loaded.ExitCode

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

let runRepl printResult (registration: ExtensionRegistration) (_rootDirectory: string) =
    let initialProgram = parseEnvironmentPrelude None []
    let initialSubmission = Interactive.createWithRegistration registration |> fun session -> Interactive.submit session initialProgram
    let mutable session = initialSubmission.Session
    let mutable exitCode = initialSubmission.State.ExitCode
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
        let submission = Interactive.submit session parsed
        let typed = submission.TypedProgram
        let state = submission.State
        exitCode <- state.ExitCode
        let hasExpression =
            parsed
            |> List.exists (function
                | SExpr _ -> true
                | _ -> false)
        if hasExpression && printResult then
            match tryGetLastExpressionType typed with
            | Some t ->
                match tryFormatFunctionValueWithType state.LastValue t with
                | Some signature -> Console.WriteLine(signature)
                | None -> Console.WriteLine(Pretty.valueToString state.LastValue)
            | None ->
                Console.WriteLine(Pretty.valueToString state.LastValue)
        session <- submission.Session
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
    exitCode

// Keep the adapter's assembly and protocol dependencies out of ordinary CLI
// startup. The JIT resolves these calls only when a debugger mode is selected.
[<MethodImpl(MethodImplOptions.NoInlining)>]
let runDebugAdapter () = FScript.DebugAdapter.DebuggerEntry.RunAdapter()

[<MethodImpl(MethodImplOptions.NoInlining)>]
let runDebugHost port token = FScript.DebugAdapter.DebuggerEntry.RunHost(port, token)

let runOrdinaryCommand argv =
    let parser = ArgumentParser.Create<CliArgs>(programName = "fscript")
    let cliArgv, scriptArguments = splitScriptArguments argv
    try
        if argv = [| "--debug-adapter" |] then runDebugAdapter ()
        elif argv.Length = 3 && argv[0] = "--debug-host" then runDebugHost (Int32.Parse argv[1]) argv[2]
        elif argv = [| "--debug-capabilities" |] then
            Console.WriteLine("{\"protocolVersion\":1,\"transport\":\"stdio\",\"terminal\":true}")
            0
        elif cliArgv |> Array.exists (fun value -> value = "--debug-adapter" || value = "--debug-capabilities" || value = "--debug-host") then
            Console.Error.WriteLine("Debugger modes must be invoked separately from ordinary CLI arguments.")
            1
        elif scriptArguments.IsEmpty && cliArgv.Length = 1 && String.Equals(cliArgv.[0], "version", StringComparison.OrdinalIgnoreCase) then
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

            let settingsResult =
                EnvironmentConfiguration.resolveForCli
                    (args.TryGetResult <@ Config @>) (args.Contains <@ No_Config @>) currentDirectory defaultRoot
                    (args.TryGetResult <@ Root @>)
                    (args.Contains <@ No_Default_Externs @>)
                    (args.GetResults <@ Extern_Assembly @>)
            let rootDirectory = settingsResult |> Result.map (fun settings -> settings.RootDirectory) |> Result.defaultValue defaultRoot
            let externsResult = settingsResult |> Result.bind EnvironmentLoader.load

            try
                match externsResult with
                | Error message ->
                    Console.Error.WriteLine(message)
                    1
                | Ok registration ->
                    match scriptPath with
                    | Some path ->
                        runFile (not (args.Contains <@ No_Result @>)) registration rootDirectory path scriptArguments
                    | None when Console.IsInputRedirected ->
                        let source = Console.In.ReadToEnd()
                        runSource (not (args.Contains <@ No_Result @>)) registration rootDirectory source scriptArguments
                    | None ->
                        if not scriptArguments.IsEmpty then
                            Console.Error.WriteLine("Script arguments require file or stdin mode. Use 'fscript <script.fss> -- <args...>' or pipe stdin with '--'.")
                            1
                        else
                            runRepl (not (args.Contains <@ No_Result @>)) registration rootDirectory
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

[<EntryPoint>]
let main argv =
    if argv.Length > 0 && String.Equals(argv.[0], "compile", StringComparison.OrdinalIgnoreCase) then
        runCompileCommand argv
    elif argv.Length > 0 && String.Equals(argv.[0], "analyze", StringComparison.OrdinalIgnoreCase) then
        runAnalyzeCommand argv
    else
        runOrdinaryCommand argv
