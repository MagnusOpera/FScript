namespace FScript.Language.Tests

open FScript.Language
open NUnit.Framework

[<TestFixture>]
type TaskSharingAnalysisTests() =
    let external name argument result : ExternalFunction =
        { Name = name; Scheme = Forall([], TFun(argument, result)); Arity = 1
          Impl = fun _ _ -> failwith "Static analysis must not invoke FFI" }
    let handle = TNamed "Handle"
    let registration =
        { HostInterop.empty with
            Types = [{ Name = "Handle"; Fields = Map.ofList ["Value", TInt]; HostManaged = true }]
            Functions = TaskFunctions.all @ [
                         external "Host.make" TInt handle
                         external "Host.read" handle TInt
                         external "Host.alias" handle handle
                         external "Host.tick" TUnit TInt] }
    let analyzeWith options purity source =
        Helpers.parse source
        |> TypeInfer.inferResolvedProgramWithRegistrationAndPurity registration purity
        |> TaskSharingAnalysis.analyze registration options
    let analyze source = analyzeWith TaskSharingAnalysis.defaults PurityContracts.empty source
    let shared = "let value = Host.make 0\nlet first = Task.spawn (fun () -> Host.read value)\nlet second = Task.spawn (fun () -> Host.read value)\nTask.await first + Task.await second"
    let valueWarnings source = analyze source |> List.filter (fun warning -> warning.Code = "FST1001")

    [<Test>]
    member _.``Sharing reports two source locations deterministically without executing`` () =
        let first = analyze shared
        Assert.That((first = analyze shared), Is.True)
        let warning = first |> List.find (fun warning -> warning.Code = "FST1001")
        Assert.That(warning.Span.Start.Line, Is.EqualTo(3))
        Assert.That(warning.RelatedSpan.Start.Line, Is.EqualTo(2))
        Assert.That(warning.Message, Does.Contain("Handle"))

    [<Test>]
    member _.``Await establishes sequential access including task aliases`` () =
        let source = "let value = Host.make 0\nlet first = Task.spawn (fun () -> Host.read value)\nlet alias = first\nTask.await alias |> ignore\nlet second = Task.spawn (fun () -> Host.read value)\nTask.await second"
        Assert.That(analyze source, Is.Empty)

    [<Test>]
    member _.``Parent access before spawn is ordered but after spawn is not`` () =
        Assert.That(valueWarnings "let value = Host.make 0\nHost.read value |> ignore\nlet first = Task.spawn (fun () -> Host.read value)\nTask.await first", Is.Empty)
        Assert.That(valueWarnings "let value = Host.make 0\nlet first = Task.spawn (fun () -> Host.read value)\nHost.read value", Is.Not.Empty)

    [<Test>]
    member _.``Aliases aggregates and helper captures retain host identity`` () =
        let source = "let value = Host.make 0\nlet alias = {| Item = Some value |}\nlet read item = Host.read item\nlet first = Task.spawn (fun () -> match alias.Item with | Some item -> read item | None -> 0)\nlet second = Task.spawn (fun () -> read value)\nTask.await first + Task.await second"
        Assert.That(valueWarnings source, Is.Not.Empty)

    [<Test>]
    member _.``Higher order helpers carry closures to task boundaries`` () =
        let source = "let launch work = Task.spawn work\nlet use f item = f item\nlet value = Host.make 0\nlet first = launch (fun () -> use Host.read value)\nlet second = launch (fun () -> use Host.read value)\nTask.await first + Task.await second"
        Assert.That(valueWarnings source, Is.Not.Empty)

    [<Test>]
    member _.``Repeated spawn inside a loop shares captured instances`` () =
        Assert.That(valueWarnings "let value = Host.make 0\nfor i in [1..3] do\n    Task.spawn (fun () -> Host.read value) |> ignore", Is.Not.Empty)

    [<Test>]
    member _.``Host result may alias an argument`` () =
        let source = shared.Replace("let first =", "let alias = Host.alias value\nlet first =").Replace("Host.read value)\nlet second", "Host.read alias)\nlet second")
        Assert.That(valueWarnings source, Is.Not.Empty)

    [<Test>]
    member _.``Thread safe type contract suppresses value warnings only`` () =
        let options = { TaskSharingAnalysis.defaults with ThreadSafeTypes = set ["Handle"] }
        let warnings = analyzeWith options PurityContracts.empty shared
        Assert.That(warnings |> List.exists (fun warning -> warning.Code = "FST1001"), Is.False)
        Assert.That(warnings |> List.exists (fun warning -> warning.Code = "FST1002"), Is.True)

    [<Test>]
    member _.``Pure types and thread safe function contracts suppress independent concerns`` () =
        let options = { TaskSharingAnalysis.defaults with ThreadSafeFunctions = set ["Host.read"] }
        let purity = { PurityContracts.empty with PureTypes = set ["Handle"] }
        Assert.That(analyzeWith options purity shared, Is.Empty)

    [<Test>]
    member _.``Unknown static FFI state warns without host instance arguments`` () =
        let warnings = analyze "let first = Task.spawn (fun () -> Host.tick ())\nlet second = Task.spawn (fun () -> Host.tick ())\nTask.await first + Task.await second"
        Assert.That(warnings |> List.exists (fun warning -> warning.Code = "FST1002" && warning.Message.Contains("Host.tick")), Is.True)

    [<Test>]
    member _.``Ordinary immutable script values do not warn`` () =
        Assert.That(analyze "let value = [1;2;3]\nlet first = Task.spawn (fun () -> List.length value)\nlet second = Task.spawn (fun () -> List.length value)\nTask.await first + Task.await second", Is.Empty)

    [<Test>]
    member _.``Uncalled exported functions are analyzed`` () =
        Assert.That(valueWarnings ("export let work () =\n" + (shared.Split('\n') |> Array.map (fun line -> "    " + line) |> String.concat "\n")), Is.Not.Empty)

    [<Test>]
    member _.``Conditional await does not prove ordering on every path`` () =
        let source = "let work wait =\n    let value = Host.make 0\n    let first = Task.spawn (fun () -> Host.read value)\n    if wait then Task.await first |> ignore else ()\n    Host.read value"
        Assert.That(valueWarnings source, Is.Not.Empty)

    [<Test>]
    member _.``Mutually exclusive task branches do not overlap`` () =
        let source = "let work choose =\n    let value = Host.make 0\n    if choose then\n        Task.spawn (fun () -> Host.read value) |> ignore\n    else\n        Task.spawn (fun () -> Host.read value) |> ignore"
        Assert.That(analyze source, Is.Empty)

    [<Test>]
    member _.``Await inside repeated spawn loop establishes ordering`` () =
        let source = "let value = Host.make 0\nfor i in [1..3] do\n    Task.spawn (fun () -> Host.read value) |> Task.await |> ignore"
        Assert.That(analyze source, Is.Empty)

    [<Test>]
    member _.``Pure scalar results do not retain their input host handle`` () =
        let source = "let value = Host.make 0\nlet number = Host.read value\nlet first = Task.spawn (fun () -> number + 1)\nlet second = Task.spawn (fun () -> number + 2)\nTask.await first + Task.await second"
        Assert.That(analyze source, Is.Empty)

    [<Test>]
    member _.``Collection callbacks expose repeated task creation`` () =
        let source = "let value = Host.make 0\nlet tasks = [1;2] |> List.map (fun i -> Task.spawn (fun () -> Host.read value))\n()"
        Assert.That(valueWarnings source, Is.Not.Empty)

    [<Test>]
    member _.``Ignoring a closure does not run it and empty collections do not call callbacks`` () =
        let source = "let value = Host.make 0\nignore (fun () -> Task.spawn (fun () -> Host.read value))\nlet tasks = [] |> List.map (fun i -> Task.spawn (fun () -> Host.read value))\nHost.read value"
        Assert.That(analyze source, Is.Empty)

    [<Test>]
    member _.``Task handles survive record projection and destructuring`` () =
        let source = "let value = Host.make 0\nlet first = Task.spawn (fun () -> Host.read value)\nlet wrapper = {| Pending = first |}\nlet (pending, n) = (wrapper.Pending, 0)\nTask.await pending |> ignore\nHost.read value"
        Assert.That(analyze source, Is.Empty)

    [<Test>]
    member _.``Recursion analysis terminates and follows direct captures`` () =
        let source = "let value = Host.make 0\nlet rec launch n =\n    if n = 0 then () else\n        Task.spawn (fun () -> Host.read value) |> ignore\n        launch (n - 1)\nlaunch 2\nHost.read value"
        Assert.That(valueWarnings source, Is.Not.Empty)

    [<Test>]
    member _.``Scalar field snapshots can be shared without retaining host identity`` () =
        let source = "let value = Host.make 0\nlet number = value.Value\nlet first = Task.spawn (fun () -> number + 1)\nlet second = Task.spawn (fun () -> number + 2)\nTask.await first + Task.await second"
        Assert.That(analyze source, Is.Empty)

    [<Test>]
    member _.``List patterns preserve awaited task identity`` () =
        let source = "let value = Host.make 0\nlet first = Task.spawn (fun () -> Host.read value)\nmatch [first] with\n| pending :: _ -> Task.await pending |> ignore\n| [] -> ()\nHost.read value"
        // A match join is deliberately conservative; a direct nonempty-list binding is exact.
        let direct = "let value = Host.make 0\nlet first = Task.spawn (fun () -> Host.read value)\nlet (pending :: rest, n) = ([first], 0)\nTask.await pending |> ignore\nHost.read value"
        Assert.That(analyze direct, Is.Empty)
        Assert.That(valueWarnings source, Is.Not.Empty)

    [<Test>]
    member _.``Curried recursive functions are bounded by lexical lambda identity`` () =
        let source = "let value = Host.make 0\nlet rec launch n acc =\n    if n = 0 then acc else\n        Task.spawn (fun () -> Host.read value) |> ignore\n        launch (n - 1) (acc + n)\nlaunch 2 0 |> ignore\nHost.read value"
        Assert.That(valueWarnings source, Is.Not.Empty)

    [<Test>]
    member _.``Mutually recursive curried entry points finish analysis`` () =
        let source = "let rec even n acc = if n = 0 then acc else odd (n - 1) acc\nand odd n acc = if n = 0 then acc else even (n - 1) acc\neven 4 0"
        Assert.That(analyze source, Is.Empty)

    [<Test>]
    member _.``Possible callees do not establish unconditional await ordering`` () =
        let source = "let work condition =\n    let value = Host.make 0\n    let first = Task.spawn (fun () -> Host.read value)\n    let wait = if condition then (fun () -> Task.await first |> ignore) else (fun () -> ())\n    wait ()\n    Host.read value"
        Assert.That(valueWarnings source, Is.Not.Empty)

    [<Test>]
    member _.``Closures capture referenced handles without sharing unrelated visible handles`` () =
        let source =
            "let unused = Host.make 0\nlet used = Host.make 1\nlet read () = Host.read used\nlet first = Task.spawn read\nlet second = Task.spawn read\nTask.await first + Task.await second"
        let warnings = valueWarnings source
        Assert.That(warnings, Is.Not.Empty)
        Assert.That(warnings |> List.forall (fun warning -> warning.Message.Contains("Handle")), Is.True)
        let isolated =
            "let unused = Host.make 0\nlet first = Task.spawn (fun () -> Host.read (Host.make 1))\nlet second = Task.spawn (fun () -> 42)\nTask.await first + Task.await second"
        Assert.That(valueWarnings isolated, Is.Empty)

    [<Test>]
    member _.``Task continuation waits for its prerequisite before accessing captures`` () =
        Assert.That(valueWarnings "let value=Host.make 0\nlet first=Task.spawn (fun () -> Host.read value)\nTask.continueWith (fun _ -> Host.read value) first |> Task.await",Is.Empty)

    [<Test>]
    member _.``Independent continuations may race on the same capture`` () =
        Assert.That(valueWarnings "let value=Host.make 0\nlet ready=Task.fromResult 1\nlet first=Task.continueWith (fun _ -> Host.read value) ready\nlet second=Task.continueWith (fun _ -> Host.read value) ready\nTask.awaitAll [first;second]",Is.Not.Empty)

    [<Test>]
    member _.``WhenAll and awaitAll establish ordering for every input`` () =
        Assert.That(valueWarnings "let value=Host.make 0\nlet first=Task.spawn (fun () -> Host.read value)\nlet second=Task.fromResult 1\nTask.whenAll (fun _ -> Host.read value) [first;second] |> Task.await |> ignore\nHost.read value",Is.Empty)
        Assert.That(valueWarnings "let value=Host.make 0\nlet first=Task.spawn (fun () -> Host.read value)\nTask.awaitAll [first] |> ignore\nHost.read value",Is.Empty)

    [<Test>]
    member _.``WhenAny cannot establish ordering with every candidate`` () =
        Assert.That(valueWarnings "let value=Host.make 0\nlet first=Task.spawn (fun () -> Host.read value)\nlet second=Task.fromResult 1\nTask.whenAny (fun _ -> Host.read value) [first;second] |> Task.await |> ignore\nTask.awaitAll [first;second]",Is.Not.Empty)

    [<Test>]
    member _.``Completed tasks preserve resources carried by their result`` () =
        Assert.That(valueWarnings "let value=Host.make 0\nlet ready=Task.fromResult value\nlet first=Task.continueWith Host.read ready\nlet second=Task.continueWith Host.read ready\nTask.awaitAll [first;second]",Is.Not.Empty)
