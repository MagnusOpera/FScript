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

let runTypedProgram (registration: ExtensionRegistration) (program: Program) =
    let typed = TypeInfer.inferProgramWithRegistration registration program
    let result = Eval.evalProgramWithRegistration registration typed
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

let runFile (registration: ExtensionRegistration) (rootDirectory: string) (scriptPath: string) (arguments: string list) =
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
        runTypedProgram registration program

let runSource (registration: ExtensionRegistration) (_rootDirectory: string) (source: string) (arguments: string list) =
    let combinedSource = $"{environmentPrelude None arguments}\n{source}"
    let loaded = ScriptHost.loadSourceWithRegistration registration combinedSource
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

let runRepl (registration: ExtensionRegistration) (_rootDirectory: string) =
    let initialProgram = parseEnvironmentPrelude None []
    let initialSubmission = Interactive.createWithRegistration registration |> fun session -> Interactive.submit session initialProgram
    let mutable session = initialSubmission.Session
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
        let hasExpression =
            parsed
            |> List.exists (function
                | SExpr _ -> true
                | _ -> false)
        if hasExpression then
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
    0

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CliArgs>(programName = "fscript")
    let cliArgv, scriptArguments = splitScriptArguments argv
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
                        runFile registration rootDirectory path scriptArguments
                    | None when Console.IsInputRedirected ->
                        let source = Console.In.ReadToEnd()
                        runSource registration rootDirectory source scriptArguments
                    | None ->
                        if not scriptArguments.IsEmpty then
                            Console.Error.WriteLine("Script arguments require file or stdin mode. Use 'fscript <script.fss> -- <args...>' or pipe stdin with '--'.")
                            1
                        else
                            runRepl registration rootDirectory
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
