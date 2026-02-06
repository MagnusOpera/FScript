open System
open System.IO
open FScript.Core
open FScript.Host

let formatSpan (span: Span) =
    sprintf "(line %d, col %d)" span.Start.Line span.Start.Column

[<EntryPoint>]
let main argv =
    if argv.Length <> 1 then
        Console.Error.WriteLine("Usage: fscript <file.fss>")
        1
    else
        let path = argv.[0]
        if not (File.Exists path) then
            Console.Error.WriteLine($"File not found: {path}")
            1
        else
            try
                let context : HostContext = { RootDirectory = Directory.GetCurrentDirectory() }
                let externs : ExternalFunction list = Registry.all context
                let source = File.ReadAllText(path)
                let program = Parser.parseProgram source
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
