namespace FScript.Backend.DotNet.Tests

open System
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open FScript.Language
open FScript.Compiler
open NUnit.Framework

module private Harness =
    let compile source =
        match Compiler.compile {Program=Parser.parseProgram source; Externs=[]} with
        | Ok program -> program.Lir
        | Error errors -> failwith (String.concat "; " (errors |> List.map _.Message))
    let options: DotNetOptions =
        { AssemblyName="CompiledScript"; Namespace="Generated"; ClassName="Program"; OutputKind=Library; References=[] }
    let generate options program =
        match DotNetBackend.generate options program with
        | Ok files -> files
        | Error errors -> failwith (String.concat "\n" (errors |> List.map (fun d -> d.Code+": "+d.Message)))
    let execute program =
        let files=generate options program
        let assembly=Assembly.Load((files |> List.find (fun f -> f.RelativePath.EndsWith(".dll"))).Content)
        for typ in assembly.GetTypes() do
            for method in typ.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly) do
                if not method.IsAbstract && not method.ContainsGenericParameters && not (nonNull method.DeclaringType).ContainsGenericParameters then
                    RuntimeHelpers.PrepareMethod(method.MethodHandle)
        Assert.That(assembly.GetReferencedAssemblies() |> Array.exists (fun a -> (a.Name |> Option.ofObj |> Option.defaultValue "").StartsWith("FScript.")), Is.False)
        let programType=assembly.GetType("Generated.Program") |> nonNull
        let result=(programType.GetMethod("Run") |> nonNull).Invoke(null,[||])
        ((programType.GetMethod("FormatResult") |> nonNull).Invoke(null,[|result|]) |> nonNull) :?> string

[<TestFixture>]
type BackendTests() =
    [<TestCase("let add x y = x+y\nadd 1 2", "3")>]
    [<TestCase("let add x y = x+y\n(add 1 2, add 1.5 2.5)", "(3, 4)")>]
    [<TestCase("let make x = fun y -> x+y\nlet add = make 10\nadd 32", "42")>]
    [<TestCase("let rec fib n = if n <= 1 then n else fib (n-1) + fib (n-2)\nfib 10", "55")>]
    [<TestCase("let rec even n = if n=0 then true else odd (n-1)\nand odd n = if n=0 then false else even (n-1)\neven 20", "true")>]
    [<TestCase("[3..1] |> List.map (fun n -> n*2)", "[6; 4; 2]")>]
    [<TestCase("(Some [1;2]) = (Some [1;2])", "true")>]
    [<TestCase("false && raise \"unreachable\"", "false")>]
    [<TestCase("true || raise \"unreachable\"", "true")>]
    [<TestCase("match Some (1,2) with | Some (a,b) when a=1 -> a+b | _ -> 0", "3")>]
    [<TestCase("type Choice = Number of int | Text of string\nmatch Text \"ok\" with | Number n -> Int.toString n | Text s -> s", "\"ok\"")>]
    [<TestCase("type Color = Red | Blue\nmatch Red with | Red -> 1 | Blue -> 2", "1")>]
    [<TestCase("let m = { [true] = 1; [false] = 2 }\nm[false]", "Some 2")>]
    [<TestCase("match { [\"a\"] = 1; [\"b\"] = 2 } with | { [k] = 2; ..rest } -> (k, Map.count rest) | _ -> (\"\",0)", "(\"b\", 1)")>]
    [<TestCase("let x = {| Name = \"Ada\"; Age = 42 |}\n{| x with Age = 43 |}.Age", "43")>]
    [<TestCase("Task.spawn (fun () -> 42) |> Task.await", "42")>]
    member _.``Generated assemblies execute core semantics and every closed method JITs`` (source: string, expected: string) =
        Assert.That(Harness.execute (Harness.compile source), Is.EqualTo(expected))

    [<Test>]
    member _.``Exact CLR calls survive serialization and Go rejects them`` () =
        let external: CompilerExtern = {Name="Native.abs"; Scheme=Forall([],TFun(TInt,TInt)); Arity=1; Kind=UnsupportedHostExtern}
        let compiled =
            match Compiler.compile {Program=Parser.parseProgram "Native.abs (0-42)"; Externs=[external]} with
            | Ok compiled -> compiled.Lir
            | Error errors -> failwith (String.concat "; " (errors |> List.map _.Message))
        let method=typeof<Math>.GetMethod("Abs",[|typeof<int64>|]) |> nonNull
        let linked=ClrBindings.bindMethod "Native.abs" method compiled
        let linked=match IrArtifact.deserialize (IrArtifact.serialize linked) with Ok p -> p | Error errors -> failwith (String.concat "; " (errors |> List.map _.Message))
        Assert.That(Harness.execute linked, Is.EqualTo("42"))
        match GoBackend.generate {ModuleName="generated"} linked with
        | Error errors -> Assert.That(errors |> List.map _.Code, Does.Contain("FSB4010"))
        | Ok _ -> Assert.Fail("Go must reject CLR FFI.")

    [<Test>]
    member _.``Host managed CLR values support constructors properties and instance methods`` () =
        let registration: ExtensionRegistration =
            { Types=[{Name="Buffer"; Fields=Map.ofList ["Length",TInt]; HostManaged=true}]; Unions=[]; Functions=[] }
        let externs: CompilerExtern list =
            [ {Name="Native.create"; Scheme=Forall([],TFun(TString,TNamed "Buffer")); Arity=1; Kind=UnsupportedHostExtern}
              {Name="Native.text"; Scheme=Forall([],TFun(TNamed "Buffer",TString)); Arity=1; Kind=UnsupportedHostExtern} ]
        let program=match Compiler.compileWithRegistration registration {Program=Parser.parseProgram "let value = Native.create \"hello\"\n(value.Length, Native.text value)"; Externs=externs} with Ok p -> p.Lir | Error errors -> failwith (String.concat "; " (errors |> List.map _.Message))
        let native=typeof<System.Text.StringBuilder>
        let program=program |> ClrBindings.bindType "Buffer" native |> ClrBindings.bindMethod "Native.create" (native.GetConstructor([|typeof<string>|]) |> nonNull) |> ClrBindings.bindMethod "Native.text" (native.GetMethod("ToString",[||]) |> nonNull)
        Assert.That(Harness.execute program, Is.EqualTo("(5, \"hello\")"))

    [<Test>]
    member _.``Roslyn produces deterministic portable PE files`` () =
        let program=Harness.compile "40+2"
        let first=Harness.generate Harness.options program
        let second=Harness.generate Harness.options program
        Assert.That((first=second), Is.True)

    [<Test>]
    member _.``CSharp projects compile independently`` () =
        let source=Harness.compile "[1..3] |> List.map (fun n -> n+1)"
        let files=match CSharpBackend.generate {Namespace="Generated"; ClassName="Program"; References=[]} source with Ok f -> f | Error e -> failwith (String.concat "; " (e |> List.map _.Message))
        let directory=Path.Combine(Path.GetTempPath(), "fscript-csharp-"+Guid.NewGuid().ToString("N"))
        try
            Directory.CreateDirectory directory |> ignore
            for file in files do File.WriteAllText(Path.Combine(directory,file.RelativePath),file.Content)
            let start=Diagnostics.ProcessStartInfo("dotnet",WorkingDirectory=directory,RedirectStandardOutput=true,RedirectStandardError=true)
            start.ArgumentList.Add("run")
            start.ArgumentList.Add("--project")
            start.ArgumentList.Add("Program.csproj")
            use proc=Diagnostics.Process.Start(start) |> nonNull
            let output=proc.StandardOutput.ReadToEnd()
            let error=proc.StandardError.ReadToEnd()
            Assert.That(proc.WaitForExit(60000),Is.True)
            Assert.That(proc.ExitCode,Is.EqualTo(0),output+error)
            Assert.That(output.Trim(),Is.EqualTo("[2; 3; 4]"))
        finally
            if Directory.Exists directory then Directory.Delete(directory,true)
