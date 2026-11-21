module TestCommand

open System
open System.IO
open System.Diagnostics
open System.Globalization
open Argu
open CliArgs
open FScript.Language
open FScript.Runtime

let private externalFunction name typeVariables signature arity implementation : ExternalFunction =
    { Name = name
      Scheme = Forall(typeVariables, signature)
      Arity = arity
      RequiredPermissions = HostPermissions.None
      Impl = implementation }

exception private AssertionFailure of string

let private assertions =
    let failAssertion message = raise (AssertionFailure message)
    let assertion name typeVariables signature arity implementation =
        externalFunction name typeVariables signature arity (fun context arguments ->
            try implementation arguments
            with AssertionFailure message ->
                raise (EvalException { Message = message; Span = HostInterop.currentCallSpan context }))

    let fail expected actual details =
        let show = Pretty.valueToString
        failAssertion $"Expected {show expected}, got {show actual}{details}."

    let floatComparison = TFun(TFloat, TFun(TFloat, TFun(TFloat, TUnit)))
    let showFloat value = Pretty.valueToString (VFloat value)
    let validateTolerance assertion tolerance =
        if not (Double.IsFinite tolerance) || tolerance < 0.0 then
            failAssertion $"{assertion} requires a finite, nonnegative tolerance. Got {showFloat tolerance}."

    [ assertion "Assert.equal" [0] (TFun(TVar 0, TFun(TVar 0, TUnit))) 2 (function
          | [expected; actual] ->
              if not (Eval.valuesEqual expected actual) then fail expected actual ""
              VUnit
          | _ -> invalidOp "Invalid arguments to Assert.equal.")
      assertion "Assert.some" [0] (TFun(TOption(TVar 0), TVar 0)) 1 (function
          | [VOption (Some value)] -> value
          | [VOption None] -> failAssertion "Expected Some, got None."
          | _ -> invalidOp "Invalid arguments to Assert.some.")
      assertion "Assert.none" [0] (TFun(TOption(TVar 0), TUnit)) 1 (function
          | [VOption None] -> VUnit
          | [(VOption (Some _) as actual)] -> fail (VOption None) actual ""
          | _ -> invalidOp "Invalid arguments to Assert.none.")
      assertion "Assert.near" [] floatComparison 3 (function
          | [VFloat tolerance; (VFloat expected as expectedValue); (VFloat actual as actualValue)] ->
              validateTolerance "Assert.near" tolerance
              let difference = abs (expected - actual)
              if not (Double.IsFinite difference) || difference > tolerance then
                  fail expectedValue actualValue $" (absolute error {showFloat difference}, tolerance {showFloat tolerance})"
              VUnit
          | _ -> invalidOp "Invalid arguments to Assert.near.")
      assertion "Assert.relative" [] floatComparison 3 (function
          | [VFloat tolerance; (VFloat expected as expectedValue); (VFloat actual as actualValue)] ->
              validateTolerance "Assert.relative" tolerance
              if not (Double.IsFinite expected && Double.IsFinite actual) then
                  fail expectedValue actualValue " (relative comparison requires finite values)"
              elif expected = 0.0 then
                  if actual <> 0.0 then fail expectedValue actualValue " (zero reference requires an exact match)"
              else
                  let difference = actual - expected
                  let error =
                      if Double.IsFinite difference then abs (difference / expected)
                      else abs (actual / expected - 1.0)
                  if error > tolerance then
                      fail expectedValue actualValue $" (relative error {showFloat error}, tolerance {showFloat tolerance})"
              VUnit
          | _ -> invalidOp "Invalid arguments to Assert.relative.") ]

let private discover directory =
    let options = EnumerationOptions(
        RecurseSubdirectories = true,
        IgnoreInaccessible = false,
        AttributesToSkip = FileAttributes.ReparsePoint)
    Directory.EnumerateFiles(directory, "*_test.fss", options)
    |> Seq.filter (fun path -> path.EndsWith("_test.fss", StringComparison.Ordinal))
    |> Seq.sortWith (fun left right -> StringComparer.Ordinal.Compare(left, right))
    |> Seq.toArray

let private describeError = function
    | ParseException error -> $"{error.Span.Start.Line}:{error.Span.Start.Column}: {error.Message}"
    | TypeException error -> $"{error.Span.Start.Line}:{error.Span.Start.Column}: {error.Message}"
    | EvalException error ->
        let file = error.Span.Start.File |> Option.defaultValue "<test>"
        $"{file}:{error.Span.Start.Line}:{error.Span.Start.Column}: {error.Message}"
    | error -> error.Message

let private caseName = "Test.case"
let private eachName = "Test.each"

type private TestCase =
    { Name: string
      Body: Value
      Row: Value option }

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

let private runFile prelude (environment: ExtensionRegistration) directory root (filter: string option) profile path =
    let displayPath = Path.GetRelativePath(directory, path)
    let cases = ResizeArray<TestCase>()
    let names = Collections.Generic.HashSet<string>(StringComparer.Ordinal)
    let mutable registering = true
    let register api action =
        lock cases (fun () ->
            if not registering then failwith $"{api} can only register during test file initialization."
            action ()
            VUnit)
    let addCase name body row =
        if not (names.Add name) then failwith $"Duplicate test name '{name}'."
        cases.Add({ Name = name; Body = body; Row = row })
    let single =
        externalFunction caseName [] (TFun(TString, TFun(TFun(TUnit, TUnit), TUnit))) 2 (fun _ -> function
            | [VString name; body] ->
                register caseName (fun () ->
                    if String.IsNullOrWhiteSpace name then failwith "Test.case requires a nonempty name."
                    addCase name body None)
            | _ -> invalidOp "Invalid arguments to Test.case.")
    let rowType = TVar 0
    let rowsType = TList(TTuple [TString; rowType])
    let eachSignature = TFun(TString, TFun(rowsType, TFun(TFun(rowType, TUnit), TUnit)))
    let each =
        externalFunction eachName [0] eachSignature 3 (fun _ -> function
            | [VString group; VList rows; body] ->
                register eachName (fun () ->
                    if String.IsNullOrWhiteSpace group then failwith "Test.each requires a nonempty group name."
                    for row in rows do
                        match row with
                        | VTuple [VString label; data] ->
                            if String.IsNullOrWhiteSpace label then failwith "Test.each requires a nonempty row name."
                            addCase $"{group} / {label}" body (Some data)
                        | _ -> invalidOp "Invalid row in Test.each.")
            | _ -> invalidOp "Invalid arguments to Test.each.")
    let registration = { environment with Functions = [single; each] @ assertions @ environment.Functions }
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
                        lock cases (fun () -> registering <- false)
                if state.ExitCode <> 0 then failwith $"Test file initialization set exit status {state.ExitCode}."
                if cases.Count = 0 then failwith "No cases registered with Test.case or Test.each."
                state)
        cases
        |> Seq.filter (fun test -> filter |> Option.forall (fun substring -> test.Name.Contains(substring, StringComparison.Ordinal)))
        |> Seq.fold (fun (passed, failed) test ->
            let name = $"{displayPath} / {test.Name}"
            try
                measure caseSamples name (fun () ->
                    let argument = test.Row |> Option.defaultValue VUnit
                    let result = Eval.invokeValueWithPermissionsAndExitCode registration.Permissions state.TypeDefs test.Body [argument]
                    if result.ExitCode <> 0 then failwith $"Test case set exit status {result.ExitCode}.")
                Console.WriteLine($"PASS {name}")
                passed + 1, failed
            with error ->
                Console.Error.WriteLine($"FAIL {name}")
                test.Row |> Option.iter (fun row -> Console.Error.WriteLine($"  Input: {Pretty.valueToString row}"))
                Console.Error.WriteLine($"  {describeError error}")
                passed, failed + 1) (0, 0)
    with error ->
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
            let files = discover directory
            let filter = args.TryGetResult <@ TestArgs.Filter @>
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
            let reserved = Set.ofList (caseName :: eachName :: List.map (fun (assertion: ExternalFunction) -> assertion.Name) assertions)
            for externalFunction in registration.Functions do
                if reserved.Contains externalFunction.Name then
                    failwith $"External function '{externalFunction.Name}' conflicts with the test API."
            let profile =
                if args.Contains <@ TestArgs.Profile @> then Some { Setups = ResizeArray(); Cases = ResizeArray() }
                else None
            let passed, failed =
                files |> Array.fold (fun (passed, failed) path ->
                    let filePassed, fileFailed = runFile prelude registration directory settings.RootDirectory filter profile path
                    passed + filePassed, failed + fileFailed) (0, 0)
            Console.WriteLine($"{passed} passed, {failed} failed")
            profile |> Option.iter printProfile
            if failed > 0 then 1
            elif passed = 0 then
                Console.Error.WriteLine("No test cases matched.")
                1
            else 0
    with error ->
        Console.Error.WriteLine(error.Message)
        1
