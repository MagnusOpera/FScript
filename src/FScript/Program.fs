open System
open System.IO
open System.Reflection
open Argu
open CliArgs
open FScript.Language
open FScript.Runtime

let formatSpan (span: Span) =
    match span.Start.File with
    | Some file -> sprintf "(%s:%d:%d)" file span.Start.Line span.Start.Column
    | None -> sprintf "(line %d, col %d)" span.Start.Line span.Start.Column

let runTypedProgram (externs: ExternalFunction list) (program: Program) =
    let typed = TypeInfer.inferProgramWithExterns externs program
    let result = Eval.evalProgramWithExterns externs typed
    Console.WriteLine(Pretty.valueToString result)
    0

let runFile (rootDirectory: string) (scriptPath: string) =
    if not (File.Exists scriptPath) then
        Console.Error.WriteLine($"File not found: {scriptPath}")
        1
    else
        let context : HostContext = { RootDirectory = rootDirectory }
        let externs : ExternalFunction list = Registry.all context
        let program = FScript.parseFileWithIncludes rootDirectory scriptPath
        runTypedProgram externs program

let runSource (rootDirectory: string) (source: string) =
    let context : HostContext = { RootDirectory = rootDirectory }
    let externs : ExternalFunction list = Registry.all context
    let loaded = ScriptHost.loadSource externs source
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

let runRepl (rootDirectory: string) =
    let context : HostContext = { RootDirectory = rootDirectory }
    let externs : ExternalFunction list = Registry.all context
    let mutable baseProgram: Program = []
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
        let state = Eval.evalProgramWithExternsState externs typed
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
    try
        if argv.Length = 1 && String.Equals(argv.[0], "version", StringComparison.OrdinalIgnoreCase) then
            Console.WriteLine(printVersion ())
            0
        else
            let args = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

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

            try
                match scriptPath with
                | Some path ->
                    runFile rootDirectory path
                | None when Console.IsInputRedirected ->
                    let source = Console.In.ReadToEnd()
                    runSource rootDirectory source
                | None ->
                    runRepl rootDirectory
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
