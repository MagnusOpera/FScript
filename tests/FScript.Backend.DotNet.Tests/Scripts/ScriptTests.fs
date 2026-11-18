namespace FScript.Backend.DotNet.Tests

open System
open System.IO
open FScript.Language
open FScript.Runtime
open FScript.Runtime.Tests
open FScript.Compiler
open NUnit.Framework

[<TestFixture>]
type ScriptBackendTests() =
    let withRoot run =
        let root = Path.Combine(Directory.GetCurrentDirectory(),".fscript","script-tests",Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory root |> ignore
        try run root
        finally Directory.Delete(root,true)
    let registration () = Registry.registration {HostContext.create (Directory.GetCurrentDirectory()) with Permissions=HostPermissions.All}
    let options root = "{ FScript.defaults with rootDirectory = Some "+ScriptTestFiles.quote root+" }"
    let call root path args opts = ScriptTestFiles.call (Path.Combine(root,path)) args opts
    let execute assemblyTarget aliases source =
        let registration = registration ()
        let registration =
            if aliases then
                let functions =
                    registration.Functions |> List.map (fun fn ->
                        let name = match fn.Name with "FScript.exec" -> "Host.child" | "FScript.defaults" -> "Host.options" | name -> name
                        {fn with Name=name})
                {registration with Functions=functions}
            else registration
        let program =
            match Compiler.compileWithRegistration registration {Program=Parser.parseProgram source;Externs=[]} with
            | Ok compiled -> compiled.Lir
            | Error errors -> failwith (String.concat "; " (errors |> List.map _.Message))
        let provider = typeof<CompiledFScript>
        let program =
            if aliases then
                program |> ClrBindings.bindMethod "Host.child" (provider.GetMethod("Exec") |> nonNull)
                        |> ClrBindings.bindMethod "Host.options" (provider.GetMethod("Defaults") |> nonNull)
            else ClrBindings.bindCompiledProviders [provider.GetMethod("Exec") |> nonNull] program
        let references = [provider.Assembly.Location;typeof<FScript.Extensibility.ScriptValue>.Assembly.Location]
        if assemblyTarget then Harness.executeWith {Harness.options with References=references} program
        else
            let assembly = Harness.csharpAssembly references program
            let target = assembly.GetType("Generated.Program") |> nonNull
            let result = (target.GetMethod("Run") |> nonNull).Invoke(null,[||])
            ((target.GetMethod("FormatResult") |> nonNull).Invoke(null,[|result|]) |> nonNull) :?> string
    let compare assemblyTarget source =
        let expected = ScriptTestFiles.run (registration ()) source |> Pretty.valueToString
        Assert.That(execute assemblyTarget false source, Is.EqualTo(expected))

    [<TestCase(true)>]
    [<TestCase(false)>]
    member _.``Script results arguments imports and exit codes match interpretation`` (assemblyTarget: bool) =
        withRoot (fun root ->
            ScriptTestFiles.write root "lib.fss" "export let prefix = \"child:\""
            ScriptTestFiles.write root "child.fss" "import \"lib.fss\" as Lib\nEnv.setExitCode 7\nString.concat \"\" [Lib.prefix; Env.Arguments |> List.tryHead |> Option.defaultValue \"\"]"
            let source = "let first = "+call root "child.fss" "[\"first\"]" (options root)+"\nlet second = "+call root "child.fss" "[\"second\"]" (options root)+"\n[first;second] |> Task.awaitAll"
            compare assemblyTarget source)

    [<TestCase(true)>]
    [<TestCase(false)>]
    member _.``Operational failures and restricted children match interpretation`` (assemblyTarget: bool) =
        withRoot (fun root ->
            for path,source in ["parse.fss","let =";"type.fss","missing";"value.fss","42";"delay.fss","Task.delay 30000 |> Task.await\n\"ok\"";"ok.fss","\"ok\""] do
                ScriptTestFiles.write root path source
            let opts = options root
            let task path opts = call root path "[]" opts
            let calls = [task "parse.fss" opts;task "type.fss" opts;task "value.fss" opts;task "missing.fss" opts;
                         task "delay.fss" ("{ "+opts+" with timeoutMilliseconds = Some 50 }");
                         task "ok.fss" ("{ "+opts+" with permissions = Some [] }")]
            let source = "let classify value =\n  match value with\n  | FScriptResult.Completed info -> info.value\n  | FScriptResult.Failed info -> info.message |> String.split \":\" |> List.tryHead |> Option.defaultValue \"\"\n["+String.concat "; " calls+"] |> Task.awaitAll |> List.map classify"
            compare assemblyTarget source)

    [<TestCase(true)>]
    [<TestCase(false)>]
    member _.``Nested permissions and narrowed roots cannot be elevated`` (assemblyTarget: bool) =
        withRoot (fun root ->
            ScriptTestFiles.write root "sub/child.fss" "match FScript.exec \"grandchild.fss\" [] { FScript.defaults with permissions = Some [\"IO\"] } |> Task.await with\n| FScriptResult.Failed info -> info.message\n| _ -> \"unexpected\""
            ScriptTestFiles.write root "sub/grandchild.fss" "\"unsafe\""
            let opts = "{ FScript.defaults with rootDirectory = Some "+ScriptTestFiles.quote(Path.Combine(root,"sub"))+"; permissions = Some [\"Threading\"] }"
            compare assemblyTarget (call root "sub/child.fss" "[]" opts+" |> Task.await"))

    [<TestCase(true)>]
    [<TestCase(false)>]
    member _.``Script execution also compiles under arbitrary FFI aliases`` (assemblyTarget: bool) =
        withRoot (fun root ->
            ScriptTestFiles.write root "child.fss" "\"ok\""
            let source = (call root "child.fss" "[]" (options root)+" |> Task.await").Replace("FScript.exec","Host.child").Replace("FScript.defaults","Host.options")
            let expected = ScriptTestFiles.run (registration ()) (source.Replace("Host.child","FScript.exec").Replace("Host.options","FScript.defaults")) |> Pretty.valueToString
            Assert.That(execute assemblyTarget true source, Is.EqualTo(expected)))

    [<TestCase(true,false)>]
    [<TestCase(false,false)>]
    [<TestCase(true,true)>]
    [<TestCase(false,true)>]
    member _.``Compiled callers join unused child tasks even on foreground failure`` (assemblyTarget: bool, fail: bool) =
        withRoot (fun root ->
            ScriptTestFiles.write root "child.fss" "Task.delay 50 |> Task.await\nFs.writeText \"done.txt\" \"done\" |> ignore\n\"ok\""
            let source = "let pending = "+call root "child.fss" "[]" (options root)+"\n"+(if fail then "raise \"parent failed\"" else "42")
            if fail then Assert.Catch(fun () -> execute assemblyTarget false source |> ignore) |> ignore
            else Assert.That(execute assemblyTarget false source, Is.EqualTo("42"))
            Assert.That(File.ReadAllText(Path.Combine(root,"done.txt")), Is.EqualTo("done")))
