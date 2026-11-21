module TestCommand

open System
open System.IO
open System.Diagnostics
open System.Globalization
open System.Text.Json
open Argu
open CliArgs
open FScript.Language
open FScript.Runtime

let private discover directory =
    let options = EnumerationOptions(
        RecurseSubdirectories = true,
        IgnoreInaccessible = false,
        AttributesToSkip = FileAttributes.ReparsePoint)
    Directory.EnumerateFiles(directory, "*_test.fss", options)
    |> Seq.filter (fun path -> path.EndsWith("_test.fss", StringComparison.Ordinal))
    |> Seq.sortWith (fun left right -> StringComparer.Ordinal.Compare(left, right))
    |> Seq.toArray

let private selectFile directory path =
    let fullPath = Path.GetFullPath(path, directory)
    let relative = Path.GetRelativePath(directory, fullPath)
    if Path.IsPathRooted relative || relative = ".." || relative.StartsWith(".." + string Path.DirectorySeparatorChar, StringComparison.Ordinal) then
        invalidArg "file" "The test file must be under the working directory."
    if not (fullPath.EndsWith("_test.fss", StringComparison.Ordinal) && File.Exists fullPath) then
        invalidArg "file" "Expected an existing *_test.fss file."
    relative.Split(Path.DirectorySeparatorChar)
    |> Array.fold (fun parent segment ->
        let current = Path.Combine(parent, segment)
        if File.GetAttributes(current).HasFlag(FileAttributes.ReparsePoint) then
            invalidArg "file" "Test discovery skips symbolic links."
        current) directory
    |> ignore
    fullPath

[<CLIMutable>]
type TestEvent =
    { Status: string
      File: string
      Name: string
      Line: int
      Column: int
      Source: string
      Message: string
      Input: string }

let private event status file name (span: Span) message input =
    { Status = status; File = file; Name = name; Line = span.Start.Line; Column = span.Start.Column
      Source = defaultArg span.Start.File file; Message = message; Input = input }

let private unknownSpan = Span.mk (Span.pos 0 0) (Span.pos 0 0)

let private errorSpan = function
    | ParseException error -> error.Span
    | TypeException error -> error.Span
    | EvalException error -> error.Span
    | _ -> unknownSpan

let private errorMessage = function
    | ParseException error -> error.Message
    | TypeException error -> error.Message
    | EvalException error -> error.Message
    | error -> error.Message

let private staticCases path =
    let mutable lists = Map.empty
    let rec unwrap = function EParen (inner, _) -> unwrap inner | expression -> expression
    let rows expression =
        match unwrap expression with
        | EList (values, _) -> values
        | EVar (name, _) -> lists |> Map.tryFind name |> Option.defaultValue []
        | _ -> []
    let declarations expression =
        match unwrap expression with
        | EApply (EApply (EFieldGet (EVar ("Test", _), "case", _), ELiteral (LString name, _), _), _, span) ->
            [event "discovered" path name span "" ""]
        | EApply (EApply (EApply (EFieldGet (EVar ("Test", _), "each", _), ELiteral (LString group, _), _), input, _), _, _) ->
            rows input |> List.choose (fun row ->
                match unwrap row with
                | ETuple ([ELiteral (LString name, _); _], span) -> Some (event "discovered" path $"{group} / {name}" span "" "")
                | _ -> None)
        | _ -> []
    File.ReadAllText path
    |> FScript.parseWithSourceName (Some path)
    |> List.collect (function
        | SExpr expression -> declarations expression
        | SLet (name, [], _, expression, _, _, _) ->
            lists <- lists.Add(name, rows expression)
            declarations expression
        | _ -> [])

let private describeError = function
    | ParseException error -> $"{error.Span.Start.Line}:{error.Span.Start.Column}: {error.Message}"
    | TypeException error -> $"{error.Span.Start.Line}:{error.Span.Start.Column}: {error.Message}"
    | EvalException error ->
        let file = error.Span.Start.File |> Option.defaultValue "<test>"
        $"{file}:{error.Span.Start.Line}:{error.Span.Start.Column}: {error.Message}"
    | error -> error.Message

type private TestProfile =
    { Setups: ResizeArray<string * TimeSpan>
      Cases: ResizeArray<string * TimeSpan> }

let private measure (samples: ResizeArray<string * TimeSpan> option) name action =
    match samples with
    | None -> action ()
    | Some samples ->
        let start = Stopwatch.GetTimestamp()
        try action ()
        finally samples.Add(name, Stopwatch.GetElapsedTime start)

let private printProfile profile =
    let milliseconds (elapsed: TimeSpan) =
        elapsed.TotalMilliseconds.ToString("F3", CultureInfo.InvariantCulture)

    let printTimings label limit (samples: ResizeArray<string * TimeSpan>) =
        let total = samples |> Seq.sumBy (fun (_, elapsed) -> elapsed.Ticks) |> TimeSpan.FromTicks
        Console.WriteLine($"  {label} ({samples.Count}, {milliseconds total} ms total)")
        samples
        |> Seq.sortBy (fun (name, elapsed) -> -elapsed.Ticks, name)
        |> Seq.truncate limit
        |> Seq.iter (fun (name, elapsed) -> Console.WriteLine($"    {milliseconds elapsed} ms  {name}"))

    Console.WriteLine("Test profile (wall-clock time)")
    printTimings "File setup" profile.Setups.Count profile.Setups
    printTimings "Slowest cases" 10 profile.Cases

let private runFile prelude (environment: ExtensionRegistration) directory root (selected: string -> bool) listOnly emit profile path =
    let displayPath = Path.GetRelativePath(directory, path)
    let functions, cases, seal = TestSupport.create ()
    let registration = TestSupport.extend environment functions
    let setupSamples = profile |> Option.map (fun timings -> timings.Setups)
    let caseSamples = profile |> Option.map (fun timings -> timings.Cases)
    try
        let state =
            measure setupSamples displayPath (fun () ->
                let state =
                    try
                        let program = prelude path @ FScript.parseFileWithIncludes root path
                        let typed = TypeInfer.inferProgramWithRegistration registration program
                        Eval.evalProgramWithRegistrationState registration typed
                    finally
                        seal ()
                if state.ExitCode <> 0 then failwith $"Test file initialization set exit status {state.ExitCode}."
                if cases.Count = 0 then failwith "No cases registered with Test.case or Test.each."
                state)
        cases
        |> Seq.filter (fun test -> selected test.Name)
        |> Seq.fold (fun (passed, failed) test ->
            let name = $"{displayPath} / {test.Name}"
            if listOnly then
                emit (event "discovered" path test.Name test.Span "" "")
                Console.WriteLine name
                passed + 1, failed
            else
              try
                measure caseSamples name (fun () ->
                    let argument = test.Row |> Option.defaultValue VUnit
                    let result = Eval.invokeValueWithPermissionsAndExitCode registration.Permissions state.TypeDefs test.Body [argument]
                    if result.ExitCode <> 0 then failwith $"Test case set exit status {result.ExitCode}.")
                emit (event "passed" path test.Name test.Span "" "")
                Console.WriteLine($"PASS {name}")
                passed + 1, failed
              with error ->
                let input = test.Row |> Option.map Pretty.valueToString |> Option.defaultValue ""
                emit (event "failed" path test.Name (errorSpan error) (errorMessage error) input)
                Console.Error.WriteLine($"FAIL {name}")
                test.Row |> Option.iter (fun row -> Console.Error.WriteLine($"  Input: {Pretty.valueToString row}"))
                Console.Error.WriteLine($"  {describeError error}")
                passed, failed + 1) (0, 0)
    with error ->
        emit (event "error" path "" (errorSpan error) (errorMessage error) "")
        Console.Error.WriteLine($"FAIL {displayPath}")
        Console.Error.WriteLine($"  {describeError error}")
        0, 1

let run prelude argv =
    let parser = ArgumentParser.Create<TestArgs>(programName = "fscript test")
    try
        if argv = [| "--help" |] || argv = [| "-h" |] then
            Console.WriteLine(parser.PrintUsage())
            0
        else
            let args = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
            let directory = Directory.GetCurrentDirectory()
            let file = args.TryGetResult <@ TestArgs.File @>
            let exact = args.TryGetResult <@ TestArgs.Case @>
            let filter = args.TryGetResult <@ TestArgs.Filter @>
            if exact.IsSome && (file.IsNone || filter.IsSome) then
                invalidArg "case" "--case requires --file and cannot be combined with --filter."
            let selected (name: string) =
                exact |> Option.forall ((=) name)
                && (filter |> Option.forall (fun substring -> name.Contains(substring, StringComparison.Ordinal)))
            let files = match file with Some path -> [|selectFile directory path|] | None -> discover directory
            let report = args.TryGetResult <@ TestArgs.Report @>
            let events = ResizeArray<TestEvent>()
            let emit value = if report.IsSome then events.Add value
            if args.Contains <@ TestArgs.Discover @> then
                let results =
                    files |> Array.collect (fun path ->
                        try staticCases path |> List.filter (fun test -> selected test.Name) |> List.toArray
                        with error -> [|event "error" path "" (errorSpan error) (errorMessage error) ""|])
                Console.WriteLine(JsonSerializer.Serialize results)
                0
            else
              let listOnly = args.Contains <@ TestArgs.List @>
              let settings =
                  EnvironmentConfiguration.resolveForCli
                      (args.TryGetResult <@ TestArgs.Config @>) (args.Contains <@ TestArgs.No_Config @>)
                      directory directory None (args.Contains <@ TestArgs.No_Default_Externs @>)
                      (args.GetResults <@ TestArgs.Extern_Assembly @>)
                  |> Result.bind (PermissionPolicy.forCli (args.TryGetResult <@ TestArgs.Permissions @>))
                  |> function Ok settings -> settings | Error message -> failwith message
              let registration =
                  EnvironmentLoader.loadWithPermissions HostPermissions.All settings
                  |> function Ok registration -> registration | Error message -> failwith message
              let profile =
                  if args.Contains <@ TestArgs.Profile @> then Some { Setups = ResizeArray(); Cases = ResizeArray() }
                  else None
              let passed, failed =
                  files |> Array.fold (fun (passed, failed) path ->
                      let filePassed, fileFailed = runFile prelude registration directory settings.RootDirectory selected listOnly emit profile path
                      passed + filePassed, failed + fileFailed) (0, 0)
              report |> Option.iter (fun path -> File.WriteAllText(path, JsonSerializer.Serialize events))
              if not listOnly then Console.WriteLine($"{passed} passed, {failed} failed")
              profile |> Option.iter printProfile
              if failed > 0 then 1
              elif passed = 0 then
                  Console.Error.WriteLine("No test cases matched.")
                  1
              else 0
    with error ->
        Console.Error.WriteLine(error.Message)
        1
