open System
open System.IO
open Argu
open CliArgs
open FScript.Language
open FScript.Runtime

let formatSpan (span: Span) =
    match span.Start.File with
    | Some file -> sprintf "(%s:%d:%d)" file span.Start.Line span.Start.Column
    | None -> sprintf "(line %d, col %d)" span.Start.Line span.Start.Column

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CliArgs>(programName = "fscript")
    try
        let args = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        let scriptPath = args.GetResult <@ Script @> |> Path.GetFullPath

        if not (File.Exists scriptPath) then
            Console.Error.WriteLine($"File not found: {scriptPath}")
            1
        else
            let scriptDirectory =
                match Path.GetDirectoryName(scriptPath) with
                | null
                | "" -> Directory.GetCurrentDirectory()
                | dir -> dir

            let rootDirectory =
                args.TryGetResult <@ Root @>
                |> Option.map Path.GetFullPath
                |> Option.defaultValue scriptDirectory

            try
                let context : HostContext = { RootDirectory = rootDirectory }
                let externs : ExternalFunction list = Registry.all context
                let program = FScript.parseFileWithIncludes rootDirectory scriptPath
                let typed = TypeInfer.inferProgramWithExterns externs program
                let result = Eval.evalProgramWithExterns externs typed
                Console.WriteLine(Pretty.valueToString result)
                0
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
