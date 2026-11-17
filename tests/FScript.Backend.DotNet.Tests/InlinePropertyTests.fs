namespace FScript.Backend.DotNet.Tests

open System
open System.IO
open System.Reflection
open System.Runtime.Loader
open FsCheck
open FsCheck.FSharp
open FScript.Compiler
open Mono.Cecil
open Mono.Cecil.Cil
open NUnit.Framework

module internal InlineProperties =
    type Form = Direct | Stacked | Repeated | Mutated | Addressed | Loop | Switch | Protected | Finalized | Constants64 | Constants32 | Checked64 | MergeSecond | MergeOperation
    let forms = [Direct; Stacked; Repeated; Mutated; Addressed; Loop; Switch; Protected; Finalized; Constants64; Constants32; Checked64; MergeSecond; MergeOperation]
    let kernels = ["Add";"Pair";"Difference";"Reuse";"Branch";"Divide";"Drain32";"Record";"Unit";"TouchAndDivide";"Switch";"PublicGeneric"]
    type Input = { Kernel:string; Form:Form; Left:int64; Right:int64; Count:int }

    let shrinkNumber n = seq {
        if n <> 0L then yield 0L
        if n/2L <> 0L then yield n/2L
    }
    let arbitraryWith kernels form =
        let number = Gen.frequency [3, Gen.elements [Int64.MinValue;Int64.MaxValue;0L;1L;-1L]; 7, Gen.choose(-20,20) |> Gen.map int64]
        let generator = gen {
            let! kernel = Gen.elements kernels
            let! left = number
            let! right = number
            let! count = Gen.choose(0,5)
            return {Kernel=kernel;Form=form;Left=left;Right=right;Count=count}
        }
        let shrink p = seq {
            for n in shrinkNumber p.Left do yield {p with Left=n}
            for n in shrinkNumber p.Right do yield {p with Right=n}
            for n in shrinkNumber (int64 p.Count) do yield {p with Count=int n}
        }
        Arb.fromGenShrink(generator,shrink)

    let arbitrary form = arbitraryWith kernels form

    let defaultReplay seed = {Rnd=Rnd seed;Size=None}
    let parseReplay (text:string) =
        match text.Split(',') with
        | [|seed;gamma|] -> {Rnd=Rnd(UInt64.Parse seed,UInt64.Parse gamma);Size=None}
        | [|seed;gamma;size|] ->
            let size=Int32.Parse size
            if size<0 then invalidArg "text" "Replay size must be nonnegative"
            {Rnd=Rnd(UInt64.Parse seed,UInt64.Parse gamma);Size=Some size}
        | _ -> invalidArg "text" "FSCRIPT_INLINE_REPLAY must be seed,gamma or seed,gamma,size"
    let run replay count arbitrary property =
        let mutable result=None
        let runner =
            {new IRunner with
                member _.OnStartFixture _ = ()
                member _.OnArguments(_,_,_) = ()
                member _.OnShrink(_,_) = ()
                member _.OnFinished(_,value) = result<-Some value}
        Check.One(Config.Quick.WithMaxTest(count).WithReplay(Some replay).WithRunner(runner),Prop.forAll arbitrary property)
        result |> Option.defaultWith (fun () -> failwith "FsCheck did not finish")
    let report name result =
        let details=Runner.onFinishedToStringWithFormatter (fun value -> "FFI Inline generator version: 1\n" + sprintf "%A" value) name result
        match result with
        | TestResult.Failed(_,_,_,_,_,seed,size) ->
            details + $"\nReplay:\nFSCRIPT_INLINE_REPLAY='{seed.Seed},{seed.Gamma},{size}' dotnet test tests/FScript.Backend.DotNet.Tests/FScript.Backend.DotNet.Tests.fsproj -c Release --filter 'Name~{name}'"
        | _ -> details
    let verify name count arbitrary seed property =
        let replay =
            match Environment.GetEnvironmentVariable "FSCRIPT_INLINE_REPLAY" with
            | null | "" -> defaultReplay seed
            | value -> parseReplay value
        match run replay count arbitrary property with
        | TestResult.Passed _ -> ()
        | result -> Assert.Fail(report name result)

    let private linked = Collections.Generic.Dictionary<string,Lir.Program * string>()
    let program kernel =
        match linked.TryGetValue kernel with
        | true,pair -> pair
        | _ ->
            let arity=(InlineHarness.methodInfo kernel).GetParameters().Length
            let source="Host.inline " + String.concat " " (List.replicate arity "1")
            let lir=InlineHarness.compile source ["Host.inline",kernel,true]
            let lir=match lir |> IrArtifact.serialize |> IrArtifact.deserialize with Ok p -> p | Error errors -> failwithf "%A" errors
            let path=lir.Declarations |> List.pick (fun d -> match d.Value.Node with Lir.TargetImport(path,_) -> Some path | _ -> None)
            linked.Add(kernel,(lir,path))
            lir,path

    /// Emit a small valid caller independently of FScript lowering and Roslyn.
    /// The ordinary and rewritten executions receive exactly the same binary.
    let binary p =
        let _,path=program p.Kernel
        use m=ModuleDefinition.CreateModule("InlineProperty" + string p.Form + p.Kernel,ModuleKind.Dll)
        let target=InlineHarness.methodInfo p.Kernel
        let typ=TypeDefinition("Generated","Caller",TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed,m.ImportReference(typeof<obj>))
        m.Types.Add typ
        let marker=MethodDefinition(ClrBindings.inlineMarkerName path,MethodAttributes.Private ||| MethodAttributes.Static,m.ImportReference target.ReturnType)
        for parameter in target.GetParameters() do marker.Parameters.Add(ParameterDefinition(m.ImportReference parameter.ParameterType))
        let markerIl=marker.Body.GetILProcessor()
        for parameter in marker.Parameters do markerIl.Emit(OpCodes.Ldarg,parameter)
        markerIl.Emit(OpCodes.Call,m.ImportReference target)
        markerIl.Emit OpCodes.Ret
        typ.Methods.Add marker
        let caller=MethodDefinition("Run",MethodAttributes.Public ||| MethodAttributes.Static,m.TypeSystem.Int64)
        caller.Parameters.Add(ParameterDefinition(m.TypeSystem.Int64))
        caller.Parameters.Add(ParameterDefinition(m.TypeSystem.Int64))
        caller.Parameters.Add(ParameterDefinition(m.TypeSystem.Int32))
        caller.Body.InitLocals<-true
        let il=caller.Body.GetILProcessor()
        let emitCall () =
            for index in 0..marker.Parameters.Count-1 do
                il.Emit(OpCodes.Ldarg,caller.Parameters[index])
                if (target.GetParameters()).[index].ParameterType=typeof<int32> then il.Emit OpCodes.Conv_I4
            il.Emit(OpCodes.Call,marker)
            if target.ReturnType=typeof<Void> then il.Emit(OpCodes.Ldarg,caller.Parameters[0])
        let parameter index = il.Emit(OpCodes.Ldarg,caller.Parameters[index])
        match p.Form with
        | Direct -> emitCall (); il.Emit OpCodes.Ret
        | Stacked -> parameter 1; emitCall (); il.Emit OpCodes.Sub; il.Emit OpCodes.Ret
        | Repeated -> emitCall (); emitCall (); il.Emit OpCodes.Sub; il.Emit OpCodes.Ret
        | Mutated ->
            emitCall ()
            parameter 0; il.Emit(OpCodes.Ldc_I8,1L); il.Emit OpCodes.Add; il.Emit(OpCodes.Starg,caller.Parameters[0])
            emitCall (); il.Emit OpCodes.Sub; il.Emit OpCodes.Ret
        | Addressed ->
            emitCall ()
            il.Emit(OpCodes.Ldarga,caller.Parameters[0]); il.Emit OpCodes.Dup; il.Emit OpCodes.Ldind_I8
            il.Emit(OpCodes.Ldc_I8,1L); il.Emit OpCodes.Add; il.Emit OpCodes.Stind_I8
            emitCall (); il.Emit OpCodes.Sub; il.Emit OpCodes.Ret
        | Loop ->
            let counter=VariableDefinition(m.TypeSystem.Int32)
            let accumulator=VariableDefinition(m.TypeSystem.Int64)
            caller.Body.Variables.Add counter
            caller.Body.Variables.Add accumulator
            il.Emit(OpCodes.Ldc_I4,0); il.Emit(OpCodes.Stloc,counter)
            il.Emit(OpCodes.Ldc_I8,0L); il.Emit(OpCodes.Stloc,accumulator)
            let check=Instruction.Create OpCodes.Nop
            let start=Instruction.Create OpCodes.Nop
            il.Emit(OpCodes.Br,check); il.Append start
            il.Emit(OpCodes.Ldloc,accumulator); emitCall (); il.Emit OpCodes.Add; il.Emit(OpCodes.Stloc,accumulator)
            il.Emit(OpCodes.Ldloc,counter); il.Emit(OpCodes.Ldc_I4,1); il.Emit OpCodes.Add; il.Emit(OpCodes.Stloc,counter)
            il.Append check
            il.Emit(OpCodes.Ldloc,counter); parameter 2; il.Emit(OpCodes.Blt,start); il.Emit(OpCodes.Ldloc,accumulator); il.Emit OpCodes.Ret
        | Switch ->
            let cases=Array.init 3 (fun _ -> Instruction.Create OpCodes.Nop)
            parameter 2; il.Emit(OpCodes.Switch,cases)
            il.Emit(OpCodes.Ldc_I8,17L); il.Emit OpCodes.Ret
            for index in 0..2 do
                il.Append cases[index]
                emitCall (); il.Emit(OpCodes.Ldc_I8,int64 index); il.Emit OpCodes.Add; il.Emit OpCodes.Ret
        | Constants64 | Constants32 | Checked64 ->
            emitCall ()
            if p.Form=Constants32 then
                il.Emit(OpCodes.Ldc_I4,int32 p.Left); il.Emit(OpCodes.Ldc_I4,int32 p.Right)
            else il.Emit(OpCodes.Ldc_I8,p.Left); il.Emit(OpCodes.Ldc_I8,p.Right)
            let operation=if p.Form=Checked64 then OpCodes.Add_Ovf else [|OpCodes.Add;OpCodes.Sub;OpCodes.Mul;OpCodes.And;OpCodes.Or;OpCodes.Xor|].[p.Count]
            il.Emit operation
            if p.Form=Constants32 then il.Emit OpCodes.Conv_I8
            il.Emit OpCodes.Sub; il.Emit OpCodes.Ret
        | MergeSecond | MergeOperation ->
            emitCall ()
            let first=Instruction.Create(OpCodes.Ldc_I8,p.Left)
            let second=Instruction.Create(OpCodes.Ldc_I8,p.Right)
            let operation=Instruction.Create OpCodes.Add
            parameter 2; il.Emit(OpCodes.Brfalse,first)
            il.Emit(OpCodes.Ldc_I8,7L)
            if p.Form=MergeOperation then il.Emit(OpCodes.Ldc_I8,p.Right)
            il.Emit(OpCodes.Br,if p.Form=MergeSecond then second else operation)
            il.Append first; il.Append second; il.Append operation
            il.Emit OpCodes.Add; il.Emit OpCodes.Ret
        | Finalized ->
            let result=VariableDefinition(m.TypeSystem.Int64)
            caller.Body.Variables.Add result
            let start=Instruction.Create OpCodes.Nop
            let handler=Instruction.Create OpCodes.Nop
            let finish=Instruction.Create OpCodes.Nop
            il.Append start
            emitCall (); il.Emit(OpCodes.Stloc,result); il.Emit(OpCodes.Leave,finish)
            il.Append handler
            parameter 1
            il.Emit(OpCodes.Call,m.ImportReference(typeof<InlineFixtures.Effects>.GetMethod("Record") |> nonNull))
            il.Emit OpCodes.Pop; il.Emit OpCodes.Endfinally
            il.Append finish
            il.Emit(OpCodes.Ldloc,result); il.Emit OpCodes.Ret
            caller.Body.ExceptionHandlers.Add(ExceptionHandler(ExceptionHandlerType.Finally,
                TryStart=start,TryEnd=handler,HandlerStart=handler,HandlerEnd=finish))
        | Protected ->
            let result=VariableDefinition(m.TypeSystem.Int64)
            caller.Body.Variables.Add result
            let start=Instruction.Create OpCodes.Nop
            let handler=Instruction.Create OpCodes.Pop
            let finish=Instruction.Create OpCodes.Nop
            il.Append start
            emitCall (); il.Emit(OpCodes.Stloc,result); il.Emit(OpCodes.Leave,finish)
            il.Append handler
            il.Emit(OpCodes.Ldc_I8,-999L); il.Emit(OpCodes.Stloc,result); il.Emit(OpCodes.Leave,finish)
            il.Append finish
            il.Emit(OpCodes.Ldloc,result); il.Emit OpCodes.Ret
            caller.Body.ExceptionHandlers.Add(ExceptionHandler(ExceptionHandlerType.Catch,
                TryStart=start,TryEnd=handler,HandlerStart=handler,HandlerEnd=finish,CatchType=m.ImportReference(typeof<DivideByZeroException>)))
        typ.Methods.Add caller
        use output=new MemoryStream()
        m.Write output
        output.ToArray()

    type Outcome = { Result:Result<int64,string * string>; Effects:int64 list }
    let rec cause (error:exn) =
        match error with
        | :? TargetInvocationException as e when not (isNull e.InnerException) -> cause (nonNull e.InnerException)
        | :? TypeInitializationException as e when not (isNull e.InnerException) -> cause (nonNull e.InnerException)
        | _ -> error
    let execute bytes p =
        let context=new AssemblyLoadContext("FFI Inline property",isCollectible=true)
        InlineFixtures.Effects.Reset()
        try
            use stream=new MemoryStream(bytes,false)
            let assembly=context.LoadFromStream stream
            let method=(assembly.GetType("Generated.Caller") |> nonNull).GetMethod("Run") |> nonNull
            let result =
                try Ok(unbox<int64>(method.Invoke(null,[|box p.Left;box p.Right;box p.Count|]))) with error ->
                    let error=cause error
                    match error with
                    | :? DivideByZeroException | :? OverflowException -> Error(error.GetType().FullName |> nonNull,error.Message)
                    | _ -> raise error
            {Result=result;Effects=InlineFixtures.Effects.Values}
        finally context.Unload()

    let check p =
        let original=binary p
        let expected=execute original p
        let lir,_=program p.Kernel
        let rewritten=match DotNetIlInlining.rewrite lir InlineHarness.options.References original with Ok bytes -> bytes | Error errors -> failwithf "Rewrite failed: %A" errors
        let actual=execute rewritten p
        if actual<>expected then failwithf "Caller: %A\nOrdinary: %A\nRewritten: %A" p expected actual
        // Accepted markers must disappear even when profitability retains a call.
        use stream=new MemoryStream(rewritten,false)
        use m=ModuleDefinition.ReadModule stream
        for typ in m.Types do
            for method in typ.Methods do
                if method.Name.StartsWith("FScriptInline_",StringComparison.Ordinal) then failwith "Marker survived rewriting"
                if p.Form=Direct && List.contains p.Kernel ["Add";"Pair";"Difference";"Reuse"] && method.HasBody then
                    for instruction in method.Body.Instructions do
                        match instruction.Operand with
                        | :? MethodReference as target when target.DeclaringType.FullName=typeof<InlineFixtures.Kernels>.FullName && target.Name=p.Kernel ->
                            failwith "Mandatory small candidate was not expanded"
                        | _ -> ()
        true

    let literal n =
        if n=Int64.MinValue then "(0 - 9223372036854775807 - 1)"
        elif n<0L then $"(0 - {-n})" else string n
    let source shape p =
        let left,right=literal p.Left,literal p.Right
        match shape with
        | "Capture" -> $"let partial = Host.inline (Host.record {left})\n(partial (Host.record {right}), partial (Host.record {p.Count}))"
        | "Arguments" -> $"Host.inline (Host.record {left}) (Host.record {right})"
        | "Map" -> $"let offset = Host.record {left}\nList.map (fun x -> Host.inline offset (Host.record x)) [{right};{p.Count}]"
        | "Unused" -> $"let unused = Host.inline (Host.record {left}) (Host.record {right})\n42"
        | _ -> invalidArg "shape" shape

    let private executeScript bytes =
        let context=new AssemblyLoadContext("FFI pipeline property",isCollectible=true)
        InlineFixtures.Effects.Reset()
        try
            use stream=new MemoryStream(bytes,false)
            let assembly=context.LoadFromStream stream
            let typ=assembly.GetType("Generated.Program") |> nonNull
            let result =
                try
                    let value=(typ.GetMethod("Run") |> nonNull).Invoke(null,[||])
                    Ok((typ.GetMethod("FormatResult") |> nonNull).Invoke(null,[|value|]) |> nonNull :?> string)
                with error ->
                    let error=cause error
                    match error with
                    | :? DivideByZeroException | :? OverflowException -> Error(error.GetType().FullName |> nonNull,error.Message)
                    | _ -> raise error
            result,InlineFixtures.Effects.Values
        finally context.Unload()

    let private sourceOracle shape p =
        InlineFixtures.Effects.Reset()
        let record=InlineFixtures.Effects.Record
        let call left right =
            (InlineHarness.methodInfo p.Kernel).Invoke(null,[|box left;box right|]) |> unbox<int64>
        let result =
            try
                let left=record p.Left
                match shape with
                | "Capture" ->
                    let first=call left (record p.Right)
                    let second=call left (record (int64 p.Count))
                    Ok $"({first}, {second})"
                | "Map" ->
                    let values=[p.Right;int64 p.Count] |> List.map (fun value -> call left (record value))
                    Ok ("[" + (values |> List.map string |> String.concat ";") + "]")
                | "Arguments" -> Ok (call left (record p.Right) |> string)
                | "Unused" -> call left (record p.Right) |> ignore; Ok "42"
                | _ -> invalidArg "shape" shape
            with error ->
                let error=cause error
                match error with
                | :? DivideByZeroException | :? OverflowException -> Error(error.GetType().FullName |> nonNull,error.Message)
                | _ -> raise error
        result,InlineFixtures.Effects.Values

    let checkSource shape p =
        let source=source shape p
        try
            let execute inlineCall =
                InlineHarness.compile source ["Host.inline",p.Kernel,inlineCall;"Host.record","Record",false]
                |> InlineHarness.generate |> InlineHarness.bytes |> executeScript
            let expected=sourceOracle shape p
            let ordinary=execute false
            let actual=execute true
            if ordinary<>expected || actual<>expected then failwithf "Reference: %A\nOrdinary: %A\nInline: %A" expected ordinary actual
            true
        with error -> raise (Exception($"FFI source:\n{source}\n{error.Message}",error))

[<TestFixture;NonParallelizable>]
type InlinePropertyTests() =
    static member Cases =
        [for form in InlineProperties.forms do
            for seed in [104729UL;130363UL] do
                yield TestCaseData(string form,seed).SetName($"InlineProperty_{form}_{seed}")]
    [<TestCaseSource(nameof InlinePropertyTests.Cases)>]
    member _.RewritingPreservesBehavior(formName:string,seed:uint64) =
        let form=InlineProperties.forms |> List.find (fun form -> string form=formName)
        InlineProperties.verify $"InlineProperty_{form}_{seed}" 20 (InlineProperties.arbitrary form) seed InlineProperties.check

    static member SourceCases =
        [for shape in ["Capture";"Arguments";"Map";"Unused"] do
            for seed in [104729UL;130363UL] do
                yield TestCaseData(shape,seed).SetName($"InlineSource_{shape}_{seed}")]
    [<TestCaseSource(nameof InlinePropertyTests.SourceCases)>]
    member _.SourceCallsPreserveBehavior(shape:string,seed:uint64) =
        let arbitrary=InlineProperties.arbitraryWith ["Difference";"Reuse";"TouchAndDivide"] InlineProperties.Direct
        InlineProperties.verify $"InlineSource_{shape}_{seed}" 4 arbitrary seed (InlineProperties.checkSource shape)

    [<TestCase("Direct",0)>]
    [<TestCase("Stacked",1)>]
    [<TestCase("Repeated",1)>]
    [<TestCase("Loop",1)>]
    member _.LoopExpansionRetainsCallsWithLiveStackValues(formName:string,remaining:int) =
        let form=InlineProperties.forms |> List.find (fun form -> string form=formName)
        let p:InlineProperties.Input={Kernel="Drain32";Form=form;Left=7L;Right=2L;Count=3}
        let original=InlineProperties.binary p
        let lir,_=InlineProperties.program p.Kernel
        let bytes=match DotNetIlInlining.rewrite lir InlineHarness.options.References original with Ok bytes -> bytes | Error errors -> failwithf "%A" errors
        use stream=new MemoryStream(bytes,false)
        use m=ModuleDefinition.ReadModule stream
        let calls=
            m.Types |> Seq.collect _.Methods |> Seq.filter _.HasBody |> Seq.collect (fun method -> method.Body.Instructions)
            |> Seq.filter (fun instruction -> match instruction.Operand with :? MethodReference as target -> target.Name="Drain32" | _ -> false) |> Seq.length
        Assert.That(calls,Is.EqualTo(remaining))
        Assert.That(InlineProperties.execute bytes p,Is.EqualTo(InlineProperties.execute original p))

    [<Test>]
    member _.IndependentVerifierAcceptsGeneratedCallers() =
        let verifier=Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__,"../../.out/ilverify/ilverify"))
        if not (File.Exists verifier) then Assert.Ignore("Install dotnet-ilverify into .out/ilverify to run the independent verifier.")
        let directory=Path.Combine(Path.GetTempPath(),"fscript-inline-properties-"+Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory directory |> ignore
        try
            let originals=Path.Combine(directory,"original")
            let expanded=Path.Combine(directory,"expanded")
            Directory.CreateDirectory originals |> ignore
            Directory.CreateDirectory expanded |> ignore
            let originalPaths=ResizeArray<string>()
            let expandedPaths=ResizeArray<string>()
            for form in InlineProperties.forms do
                for kernel in InlineProperties.kernels do
                    let p:InlineProperties.Input={Kernel=kernel;Form=form;Left=7L;Right=2L;Count=3}
                    let lir,_=InlineProperties.program kernel
                    let bytes=InlineProperties.binary p
                    let rewritten=match DotNetIlInlining.rewrite lir InlineHarness.options.References bytes with Ok bytes -> bytes | Error errors -> failwithf "%A" errors
                    let name = $"{form}{kernel}.dll"
                    let originalPath=Path.Combine(originals,name)
                    let expandedPath=Path.Combine(expanded,name)
                    File.WriteAllBytes(originalPath,bytes);originalPaths.Add originalPath
                    File.WriteAllBytes(expandedPath,rewritten);expandedPaths.Add expandedPath
            let verify paths =
                let start=Diagnostics.ProcessStartInfo(verifier,RedirectStandardOutput=true,RedirectStandardError=true)
                for path in paths do start.ArgumentList.Add path
                start.ArgumentList.Add("--system-module");start.ArgumentList.Add("System.Private.CoreLib")
                let platform=(AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") |> nonNull) :?> string
                for reference in Array.append (platform.Split(Path.PathSeparator)) (Directory.GetFiles(AppContext.BaseDirectory,"*.dll")) |> Array.distinctBy Path.GetFileName do
                    start.ArgumentList.Add("-r");start.ArgumentList.Add reference
                use verifierProcess=Diagnostics.Process.Start(start) |> nonNull
                let output=verifierProcess.StandardOutput.ReadToEndAsync()
                let error=verifierProcess.StandardError.ReadToEndAsync()
                if not (verifierProcess.WaitForExit(60000)) then verifierProcess.Kill(true);Assert.Fail("ILVerify timed out")
                Assert.That(verifierProcess.ExitCode,Is.Zero,output.Result+error.Result)
            verify originalPaths
            verify expandedPaths
        finally Directory.Delete(directory,true)

    [<TestCase("1,2")>]
    [<TestCase("1,3,-1")>]
    [<TestCase("1,3,0,4")>]
    member _.InvalidReplayIsRejected(input:string) =
        Assert.Throws<ArgumentException>(TestDelegate(fun () -> InlineProperties.parseReplay input |> ignore)) |> ignore

    [<Test>]
    member _.CounterexamplesShrinkAndReplay() =
        let arbitrary=InlineProperties.arbitrary InlineProperties.Repeated
        let property (p:InlineProperties.Input) = p.Left=0L
        let run replay=InlineProperties.run replay 100 arbitrary property
        match run (InlineProperties.defaultReplay 104729UL) with
        | TestResult.Failed(data,original,reduced,_,_,seed,size) as failure ->
            Assert.That(data.NumberOfShrinks,Is.GreaterThan(0))
            Assert.That(abs (unbox<InlineProperties.Input>(List.exactlyOne reduced)).Left,Is.EqualTo(1L))
            match run (InlineProperties.parseReplay $"{seed.Seed},{seed.Gamma},{size}") with
            | TestResult.Failed(_,again,smaller,_,_,_,_) ->
                Assert.That(again,Is.EqualTo(original));Assert.That(smaller,Is.EqualTo(reduced))
            | _ -> Assert.Fail("Replay did not reproduce the injected mismatch")
            Assert.That(InlineProperties.report "injected" failure,Does.Contain("FSCRIPT_INLINE_REPLAY="))
        | _ -> Assert.Fail("Injected mismatch was not detected")
