open System
open System.IO
open FScript.Core

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
                let externs : ExternalFunction list =
                    [ { Name = "readFile"
                        Scheme = Forall([], TFun(TString, TString))
                        Arity = 1
                        Impl = function
                            | [ VString p ] -> VString (File.ReadAllText p)
                            | _ -> raise (EvalException { Message = "readFile expects a single string"; Span = { Start = { Line = 0; Column = 0 }; End = { Line = 0; Column = 0 } } }) }
                      { Name = "regexIsMatch"
                        Scheme = Forall([], TFun(TString, TFun(TString, TBool)))
                        Arity = 2
                        Impl = function
                            | [ VString pattern; VString input ] -> VBool (Text.RegularExpressions.Regex.IsMatch(input, pattern))
                            | _ -> raise (EvalException { Message = "regexIsMatch expects pattern and input strings"; Span = { Start = { Line = 0; Column = 0 }; End = { Line = 0; Column = 0 } } }) } ]
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
