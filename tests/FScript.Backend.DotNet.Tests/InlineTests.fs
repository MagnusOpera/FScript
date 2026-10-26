namespace FScript.Backend.DotNet.Tests

open System
open System.Reflection
open System.IO
open System.Runtime.CompilerServices
open NUnit.Framework
open FScript.Language
open FScript.Compiler
open FScript.DotNet.Comparison
open Mono.Cecil
open Mono.Cecil.Cil

module InlineFixtures =
    type Effects =
        static let mutable values : int64 list = []
        static member Reset() = values<-[]
        static member Values = List.rev values
        static member Record(value:int64) = values<-value::values;value
    type private Storage(value:int64) =
        member _.Value = value
    type Kernels =
        static member Add(value:int64) = value+1L
        static member Pair(left:int64,right:int64) = left+right
        static member Branch(value:int64) = if value<0L then -value else value*2L+1L
        static member Loop(value:int64) =
            let mutable count=value
            let mutable total=0L
            while count>0L do total<-total+count;count<-count-1L
            total
        static member Drain32(value:int32) =
            let mutable left=value
            while left>0 do left<-left-1
            int64 left
        static member Large(value:int64) =
            let x0=value
            let x1=x0*3L+1L
            let x2=x1*3L+2L
            let x3=x2*3L+3L
            let x4=x3*3L+4L
            let x5=x4*3L+5L
            let x6=x5*3L+6L
            let x7=x6*3L+7L
            let x8=x7*3L+8L
            let x9=x8*3L+9L
            let x10=x9*3L+10L
            let x11=x10*3L+11L
            let x12=x11*3L+12L
            let x13=x12*3L+13L
            let x14=x13*3L+14L
            let x15=x14*3L+15L
            let x16=x15*3L+16L
            let x17=x16*3L+17L
            let x18=x17*3L+18L
            let x19=x18*3L+19L
            let x20=x19*3L+20L
            let x21=x20*3L+21L
            let x22=x21*3L+22L
            let x23=x22*3L+23L
            let x24=x23*3L+24L
            let x25=x24*3L+25L
            let x26=x25*3L+26L
            let x27=x26*3L+27L
            let x28=x27*3L+28L
            let x29=x28*3L+29L
            let x30=x29*3L+30L
            x30
        static member Divide(value:int64) = 100L/value
        static member Record(value:int64) = Effects.Record value
        static member Unit(value:int64) = Effects.Record value |> ignore
        static member Handler(value:int64) = try 100L/value with :? DivideByZeroException -> 0L
        static member PrivateCall(value:int64) = Storage(value).Value
    type Provider =
        static member FScriptInlineFunctions() : Collections.Generic.IEnumerable<string> = ["Host.inline"]
        static member FScriptCompiledFunctions() : Collections.Generic.IEnumerable<Collections.Generic.KeyValuePair<string,MethodInfo>> =
            [Collections.Generic.KeyValuePair("Host.inline",typeof<Kernels>.GetMethod("Add") |> nonNull)]

module private InlineHarness =
    let methodInfo name = typeof<InlineFixtures.Kernels>.GetMethod(name) |> nonNull
    let compile source bindings =
        let externs:CompilerExtern list=bindings |> List.map (fun (name,methodName,_) ->
            let method=methodInfo methodName
            let inputs=List.replicate (method.GetParameters().Length) TInt
            let result=if method.ReturnType=typeof<Void> then TUnit else TInt
            {Name=name;Scheme=Forall([],List.foldBack (fun input result -> TFun(input,result)) inputs result);Arity=inputs.Length;Kind=UnsupportedHostExtern})
        let program=match Compiler.compile {Program=Parser.parseProgram source;Externs=externs} with Ok value -> value.Lir | Error errors -> failwith (string errors)
        bindings |> List.fold (fun (program:Lir.Program) (name,methodName,inlineCall) ->
            if program.Declarations |> List.exists (fun declaration -> declaration.Name=name) |> not then program
            elif inlineCall then ClrBindings.bindInlineMethod name (methodInfo methodName) program
            else ClrBindings.bindMethod name (methodInfo methodName) program) program
    let options:DotNetOptions={AssemblyName="InlineScript";Namespace="Generated";ClassName="Program";OutputKind=Library;References=[typeof<InlineFixtures.Kernels>.Assembly.Location]}
    let generate program =
        match DotNetBackend.generate options program with Ok files -> files | Error errors -> failwith (string errors)
    let bytes files=files |> List.find (fun (file:GeneratedBinary) -> file.RelativePath="InlineScript.dll") |> _.Content
    let load files =
        let assembly=Assembly.Load(bytes files)
        for method in IlInspection.methods assembly do
            if not method.ContainsGenericParameters && not (nonNull method.DeclaringType).ContainsGenericParameters then RuntimeHelpers.PrepareMethod method.MethodHandle
        assembly
    let run (assembly:Assembly) =
        let typ=assembly.GetType("Generated.Program") |> nonNull
        let result=(typ.GetMethod("Run") |> nonNull).Invoke(null,[||])
        ((typ.GetMethod("FormatResult") |> nonNull).Invoke(null,[|result|]) |> nonNull) :?> string
    let calls (assembly:Assembly) methodName =
        IlInspection.methods assembly |> Array.collect IlInspection.decode |> Array.filter (fun instruction ->
            instruction.Target |> Option.exists (fun method -> method.DeclaringType=typeof<InlineFixtures.Kernels> && method.Name=methodName))

[<TestFixture>]
type InlineTests() =
    [<TestCase("Add", "Host.inline 41", "42")>]
    [<TestCase("Branch", "Host.inline (0 - 7)", "7")>]
    [<TestCase("Branch", "Host.inline 7", "15")>]
    [<TestCase("Drain32", "Host.inline 100", "0")>]
    member _.``Inline expands actual IL and preserves results`` (methodName:string,source:string,expected:string) =
        let assembly=InlineHarness.compile source ["Host.inline",methodName,true] |> InlineHarness.generate |> InlineHarness.load
        Assert.That(InlineHarness.run assembly,Is.EqualTo(expected))
        Assert.That(InlineHarness.calls assembly methodName,Is.Empty)
        Assert.That(assembly.GetReferencedAssemblies() |> Array.exists (fun name -> name.Name=typeof<InlineFixtures.Kernels>.Assembly.GetName().Name),Is.False)
        Assert.That(IlInspection.methods assembly |> Array.exists (fun method -> method.Name.StartsWith("FScriptInline_",StringComparison.Ordinal)),Is.False)

    [<TestCase("Add", "export let exposed = Host.inline", 41L)>]
    [<TestCase("Pair", "export let exposed = Host.inline 7", 35L)>]
    member _.``Escaping and partially applied FFI functions expand inside generated closures`` (name:string,source:string,argument:int64) =
        let assembly=InlineHarness.compile source ["Host.inline",name,true] |> InlineHarness.generate |> InlineHarness.load
        let typ=assembly.GetType("Generated.Program") |> nonNull
        let fn=(typ.GetProperty("v_exposed") |> nonNull).GetValue(null) |> nonNull :?> Func<int64,int64>
        Assert.That(fn.Invoke(argument),Is.EqualTo(42L))
        Assert.That(InlineHarness.calls assembly name,Is.Empty)

    [<Test>]
    member _.``Large opted-in callees remain typed calls`` () =
        let method=InlineHarness.methodInfo "Large"
        Assert.That((method.GetMethodBody() |> nonNull).GetILAsByteArray() |> nonNull |> Array.length,Is.GreaterThan(100))
        let assembly=InlineHarness.compile "Host.inline 7" ["Host.inline","Large",true] |> InlineHarness.generate |> InlineHarness.load
        Assert.That(InlineHarness.run assembly,Is.EqualTo(InlineFixtures.Kernels.Large(7L).ToString()))
        Assert.That(InlineHarness.calls assembly "Large" |> Array.length,Is.EqualTo(1))
        Assert.That(IlInspection.methods assembly |> Array.exists (fun method -> method.Name.StartsWith("FScriptInline_",StringComparison.Ordinal)),Is.False)

    [<Test>]
    member _.``Medium callee without enough simplification does not grow the caller`` () =
        let assembly=InlineHarness.compile "Host.inline 100" ["Host.inline","Loop",true] |> InlineHarness.generate |> InlineHarness.load
        Assert.That(InlineHarness.run assembly,Is.EqualTo("5050"))
        Assert.That(InlineHarness.calls assembly "Loop" |> Array.length,Is.EqualTo(1))

    [<Test>]
    member _.``Many small sites share a bounded caller growth budget`` () =
        let program=InlineHarness.compile "Host.inline 7" ["Host.inline","Branch",true]
        let path=program.Declarations |> List.pick (fun declaration -> match declaration.Value.Node with Lir.TargetImport(path,_) -> Some path | _ -> None)
        // A controlled single caller avoids source lowering splitting sites into helpers.
        use moduleDef=ModuleDefinition.CreateModule("BudgetScript",ModuleKind.Dll)
        let intType=moduleDef.ImportReference(typeof<int64>)
        let typ=TypeDefinition("Generated","Budget",Mono.Cecil.TypeAttributes.Public ||| Mono.Cecil.TypeAttributes.Abstract ||| Mono.Cecil.TypeAttributes.Sealed,moduleDef.ImportReference(typeof<obj>))
        moduleDef.Types.Add typ
        let marker=MethodDefinition(ClrBindings.inlineMarkerName path,Mono.Cecil.MethodAttributes.Private ||| Mono.Cecil.MethodAttributes.Static,intType)
        marker.Parameters.Add(ParameterDefinition(intType))
        marker.Body.Instructions.Add(Instruction.Create(OpCodes.Ldarg_0))
        marker.Body.Instructions.Add(Instruction.Create(OpCodes.Call,moduleDef.ImportReference(InlineHarness.methodInfo "Branch")))
        marker.Body.Instructions.Add(Instruction.Create(OpCodes.Ret))
        typ.Methods.Add marker
        let caller=MethodDefinition("Caller",Mono.Cecil.MethodAttributes.Public ||| Mono.Cecil.MethodAttributes.Static,intType)
        caller.Parameters.Add(ParameterDefinition(intType))
        caller.Body.Instructions.Add(Instruction.Create(OpCodes.Ldc_I8,0L))
        for _ in 1..24 do
            caller.Body.Instructions.Add(Instruction.Create(OpCodes.Ldarg_0))
            caller.Body.Instructions.Add(Instruction.Create(OpCodes.Call,marker))
            caller.Body.Instructions.Add(Instruction.Create(OpCodes.Add))
        caller.Body.Instructions.Add(Instruction.Create(OpCodes.Ret))
        typ.Methods.Add caller
        use stream=new MemoryStream()
        moduleDef.Write(stream)
        let passType=typeof<DotNetOptions>.Assembly.GetType("FScript.Compiler.DotNetIlInlining") |> nonNull
        let rewrite=passType.GetMethod("rewrite",BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic) |> nonNull
        let rewritten=rewrite.Invoke(null,[|box program;box InlineHarness.options.References;box (stream.ToArray())|]) |> nonNull :?> Result<byte array,CompilerDiagnostic list>
        let binary=match rewritten with Ok value -> value | Error errors -> failwith (string errors)
        let assembly=Assembly.Load binary
        let remaining=InlineHarness.calls assembly "Branch" |> Array.length
        Assert.That(remaining,Is.GreaterThan(0).And.LessThan(24))
        let method=(assembly.GetType("Generated.Budget") |> nonNull).GetMethod("Caller") |> nonNull
        RuntimeHelpers.PrepareMethod method.MethodHandle
        Assert.That(method.Invoke(null,[|box 7L|]),Is.EqualTo(box (24L*15L)))

    [<Test>]
    member _.``Inline is per binding even when two names map to the same CLR method`` () =
        let assembly=InlineHarness.compile "(Host.inline 1,Host.normal 2)" ["Host.inline","Add",true;"Host.normal","Add",false] |> InlineHarness.generate |> InlineHarness.load
        Assert.That(InlineHarness.run assembly,Is.EqualTo("(2, 3)"))
        Assert.That(InlineHarness.calls assembly "Add" |> Array.length,Is.EqualTo(1))

    [<Test>]
    member _.``Inline does not imply Pure and preserves ordered argument effects`` () =
        InlineFixtures.Effects.Reset()
        let assembly=InlineHarness.compile "(Host.inline (Host.record 1),Host.inline (Host.record 2))" ["Host.inline","Record",true;"Host.record","Record",false] |> InlineHarness.generate |> InlineHarness.load
        Assert.That(InlineHarness.run assembly,Is.EqualTo("(1, 2)"))
        Assert.That(InlineFixtures.Effects.Values,Is.EqualTo(box [1L;1L;2L;2L]))

    [<Test>]
    member _.``Void bodies retain effects`` () =
        InlineFixtures.Effects.Reset()
        let assembly=InlineHarness.compile "Host.inline 7" ["Host.inline","Unit",true] |> InlineHarness.generate |> InlineHarness.load
        Assert.That(InlineHarness.run assembly,Is.EqualTo("()"))
        Assert.That(InlineFixtures.Effects.Values,Is.EqualTo(box [7L]))

    [<Test>]
    member _.``Inlining a failing operation does not remove its failure`` () =
        let assembly=InlineHarness.compile "let unused = Host.inline 0\n42" ["Host.inline","Divide",true] |> InlineHarness.generate |> InlineHarness.load
        let error=Assert.Throws<TargetInvocationException>(fun () -> InlineHarness.run assembly |> ignore)
        let rec cause (error:Exception) = if isNull error.InnerException then error else cause (nonNull error.InnerException)
        Assert.That(cause (nonNull error),Is.TypeOf<DivideByZeroException>())

    [<TestCase("Handler","exception-handler")>]
    [<TestCase("PrivateCall","not public")>]
    member _.``Unsupported explicit bodies produce actionable diagnostics`` (name,reason) =
        let program=InlineHarness.compile "Host.inline 0" ["Host.inline",name,true]
        match DotNetBackend.generate InlineHarness.options program with
        | Ok _ -> Assert.Fail("Unsupported Inline body was accepted.")
        | Error errors -> Assert.That(errors.Head.Code,Is.EqualTo("FSN7201"));Assert.That(errors.Head.Message,Does.Contain(reason));Assert.That(errors.Head.Span.IsSome,Is.True)

    [<Test>]
    member _.``Unused unsupported Inline binding does not block compilation`` () =
        let assembly=InlineHarness.compile "42" ["Host.inline","Handler",true] |> InlineHarness.generate |> InlineHarness.load
        Assert.That(InlineHarness.run assembly,Is.EqualTo("42"))

    [<Test>]
    member _.``Provider discovers Inline independently and artifacts retain it`` () =
        let external:CompilerExtern={Name="Host.inline";Scheme=Forall([],TFun(TInt,TInt));Arity=1;Kind=UnsupportedHostExtern}
        let compiled=match Compiler.compile {Program=Parser.parseProgram "Host.inline 41";Externs=[external]} with Ok value -> value.Lir | Error errors -> failwith (string errors)
        let provider=typeof<InlineFixtures.Provider>.GetMethod("FScriptCompiledFunctions") |> nonNull
        let program=ClrBindings.bindCompiledProviders [provider] compiled
        Assert.That(ClrBindings.providerPurity [provider],Is.EqualTo(PurityContracts.empty))
        let loaded=match program |> IrArtifact.serialize |> IrArtifact.deserialize with Ok value -> value | Error errors -> failwith (string errors)
        let files=InlineHarness.generate loaded
        Assert.That(InlineHarness.run (InlineHarness.load files),Is.EqualTo("42"))
        Assert.That(InlineHarness.bytes (InlineHarness.generate loaded),Is.EqualTo(box (InlineHarness.bytes files)),"IL rewriting must be byte deterministic")

    [<Test>]
    member _.``CSharp output retains ordinary calls for Inline bindings`` () =
        let program=InlineHarness.compile "Host.inline 41" ["Host.inline","Add",true]
        let options:CSharpOptions={Namespace="Generated";ClassName="Program";References=InlineHarness.options.References}
        let files=match CSharpBackend.generate options program with Ok files -> files | Error errors -> failwith (string errors)
        let source=files |> List.find (fun file -> file.RelativePath="Program.cs") |> _.Content
        Assert.That(source,Does.Contain(".@Add("))
        Assert.That(source,Does.Not.Contain("FScriptInline_"))

    [<Test>]
    member _.``ILVerify validates expanded branches loops effects and return stacks`` () =
        let verifier=Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__,"../../.out/ilverify/ilverify"))
        if not (File.Exists verifier) then Assert.Ignore("Install dotnet-ilverify into .out/ilverify to run the independent verifier.")
        let source="let effect = Host.effect 3\n(Host.add (Host.branch 7),Host.loop 100,Host.divide 4,Host.drain 100)"
        let program=InlineHarness.compile source ["Host.effect","Unit",true;"Host.add","Add",true;"Host.branch","Branch",true;"Host.loop","Loop",true;"Host.divide","Divide",true;"Host.drain","Drain32",true]
        let files=InlineHarness.generate program
        let directory=Path.Combine(Path.GetTempPath(),"fscript-inline-verifier-"+Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory directory |> ignore
        try
            for file in files do File.WriteAllBytes(Path.Combine(directory,file.RelativePath),file.Content)
            let start=Diagnostics.ProcessStartInfo(verifier,RedirectStandardOutput=true,RedirectStandardError=true)
            start.ArgumentList.Add(Path.Combine(directory,"InlineScript.dll"))
            start.ArgumentList.Add("--system-module");start.ArgumentList.Add("System.Private.CoreLib")
            let platform=(AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") |> nonNull) :?> string
            for reference in Array.append (platform.Split(Path.PathSeparator)) (Directory.GetFiles(AppContext.BaseDirectory,"*.dll")) |> Array.distinctBy Path.GetFileName do
                start.ArgumentList.Add("-r");start.ArgumentList.Add(reference)
            use verifierProcess=Diagnostics.Process.Start(start) |> nonNull
            let output=verifierProcess.StandardOutput.ReadToEndAsync()
            let error=verifierProcess.StandardError.ReadToEndAsync()
            if not (verifierProcess.WaitForExit(30000)) then verifierProcess.Kill(true);Assert.Fail("ILVerify timed out.")
            Assert.That(verifierProcess.ExitCode,Is.Zero,output.Result+error.Result)
        finally Directory.Delete(directory,true)
