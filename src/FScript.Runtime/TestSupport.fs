namespace FScript.Runtime

open System
open FScript.Language

module TestSupport =
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

    let private caseName = "Test.case"
    let private eachName = "Test.each"

    type TestCase =
        { Name: string
          Body: Value
          Row: Value option
          Span: Span }

    let create () =
        let cases = ResizeArray<TestCase>()
        let names = Collections.Generic.HashSet<string>(StringComparer.Ordinal)
        let mutable registering = true
        let register api action =
            lock cases (fun () ->
                if not registering then failwith $"{api} can only register during test file initialization."
                action ()
                VUnit)
        let addCase span name body row =
            if not (names.Add name) then failwith $"Duplicate test name '{name}'."
            cases.Add({ Name = name; Body = body; Row = row; Span = span })
        let single =
            externalFunction caseName [] (TFun(TString, TFun(TFun(TUnit, TUnit), TUnit))) 2 (fun context -> function
                | [VString name; body] ->
                    register caseName (fun () ->
                        if String.IsNullOrWhiteSpace name then failwith "Test.case requires a nonempty name."
                        addCase (HostInterop.currentCallSpan context) name body None)
                | _ -> invalidOp "Invalid arguments to Test.case.")
        let rowType = TVar 0
        let rowsType = TList(TTuple [TString; rowType])
        let eachSignature = TFun(TString, TFun(rowsType, TFun(TFun(rowType, TUnit), TUnit)))
        let each =
            externalFunction eachName [0] eachSignature 3 (fun context -> function
                | [VString group; VList rows; body] ->
                    register eachName (fun () ->
                        if String.IsNullOrWhiteSpace group then failwith "Test.each requires a nonempty group name."
                        for row in rows do
                            match row with
                            | VTuple [VString label; data] ->
                                if String.IsNullOrWhiteSpace label then failwith "Test.each requires a nonempty row name."
                                addCase (HostInterop.currentCallSpan context) $"{group} / {label}" body (Some data)
                            | _ -> invalidOp "Invalid row in Test.each.")
                | _ -> invalidOp "Invalid arguments to Test.each.")
        let functions = [single; each] @ assertions
        let seal () = lock cases (fun () -> registering <- false)
        functions, cases, seal

    let extend (environment: ExtensionRegistration) (functions: ExternalFunction list) =
        let reserved = functions |> List.map (fun (fn: ExternalFunction) -> fn.Name) |> Set.ofList
        for fn in environment.Functions do
            if reserved.Contains fn.Name then
                invalidOp $"External function '{fn.Name}' conflicts with the test API."
        { environment with Functions = functions @ environment.Functions }

    let symbols =
        let functions, _, _ = create ()
        functions |> List.map (fun fn -> { fn with Impl = fun _ _ -> invalidOp "Test symbols require a test session." })

    let prepareDebugCase environment program name =
        let functions, cases, seal = create ()
        let row = TVar 0
        let selection =
            externalFunction "FScriptTestRunner.selected" [0] (TFun(TUnit, TTuple [TFun(row, TUnit); row])) 1
                (fun _ _ ->
                    seal ()
                    match cases |> Seq.tryFind (fun item -> item.Name = name) with
                    | Some item -> VTuple [item.Body; defaultArg item.Row VUnit]
                    | None -> invalidOp $"No test case named '{name}' was registered.")
        let footer = FScript.parseWithSourceName (Some "<test-runner>") "let (fscriptTestBody, fscriptTestInput) = FScriptTestRunner.selected ()
fscriptTestBody fscriptTestInput"
        let setup =
            match List.rev program with
            | SExpr expression :: rest ->
                let binding = SLet ("fscriptTestSetupResult", [], None, expression, false, false, Ast.spanOfExpr expression)
                List.rev (binding :: rest)
            | _ -> program
        extend environment (selection :: functions), setup @ footer
