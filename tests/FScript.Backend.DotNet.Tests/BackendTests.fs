namespace FScript.Backend.DotNet.Tests

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Runtime.CompilerServices
open FScript.Language
open FScript.Compiler
open NUnit.Framework

module ProviderFixture =
    open FScript.Extensibility
    type QuietValue() =
        member _.Value = 7L
    type ConstructedValue(value: int64) =
        static let mutable created = 0
        do created <- created+1
        static member Reset() = created <- 0
        static member Created = created
        member _.Value = value
    type ObservedValue() =
        static let mutable reads = 0
        static member Reset() = reads <- 0
        static member Reads = reads
        member _.Value = reads <- reads+1; 7L
    type Provider =
        static member Provide() : ExtensionRegistration =
            let descriptor={Name="Secret"; Fields=Map.ofList ["Name",TString]; HostManaged=true}
            { Types=[descriptor]; Unions=[{Name="ForeignState"; Cases=Map.ofList ["Ready",None; "Count",Some TInt]}]
              Functions=
                [ {Name="Sdk.create"; Scheme=Forall([],TFun(TString,TNamed "Secret")); Arity=1
                   Impl=fun _ arguments -> match arguments with [VString name] -> VHostManaged {Descriptor=descriptor; Fields=Map.ofList ["Name",VString name]; Payload=box name |> nonNull} | _ -> failwith "arguments"}
                  {Name="Sdk.apply"; Scheme=Forall([],TFun(TFun(TInt,TInt),TFun(TInt,TInt))); Arity=2
                   Impl=fun context arguments -> context.Apply arguments.[0] arguments.[1]}
                  {Name="Sdk.identity"; Scheme=Forall([0],TFun(TVar 0,TVar 0)); Arity=1; Impl=fun _ arguments -> arguments.[0]}
                  {Name="Sdk.await"; Scheme=Forall([],TFun(TTask TInt,TInt)); Arity=1; Impl=fun context arguments -> context.AwaitTask arguments.[0]} ] }
    type ContextProvider =
        static member Provide(context: HostContext) : ExtensionRegistration =
            let fn=context.DefaultFunctions |> List.find (fun fn -> fn.Name="Guid.new")
            {Types=[]; Unions=[]; Functions=[{fn with Name="Sdk.guid"}]}

module private Harness =
    let legacy source =
        let registration=HostInterop.ofExterns (FScript.Runtime.Registry.all {RootDirectory=Directory.GetCurrentDirectory(); DeniedPathGlobs=[]})
        Parser.parseProgram source |> TypeInfer.inferProgramWithRegistration registration |> Eval.evalProgramWithRegistration registration |> Pretty.valueToString
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
    let executeWith options program =
        let files=generate options program
        let assembly=Assembly.Load((files |> List.find (fun f -> f.RelativePath.EndsWith(".dll"))).Content)
        for typ in assembly.GetTypes() do
            for method in typ.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly) do
                if not method.IsAbstract && not method.ContainsGenericParameters && not (nonNull method.DeclaringType).ContainsGenericParameters then
                    RuntimeHelpers.PrepareMethod(method.MethodHandle)
        Assert.That(assembly.GetReferencedAssemblies() |> Array.exists (fun a -> List.contains (a.Name |> Option.ofObj |> Option.defaultValue "") ["FScript.Language"; "FScript.Runtime"; "FScript.Interpreter.Lir"]), Is.False)
        let programType=assembly.GetType("Generated.Program") |> nonNull
        let result=(programType.GetMethod("Run") |> nonNull).Invoke(null,[||])
        ((programType.GetMethod("FormatResult") |> nonNull).Invoke(null,[|result|]) |> nonNull) :?> string
    let execute program = executeWith options program

module private IL =
    let instructions (assembly: Assembly) =
        let opcodes=typeof<OpCodes>.GetFields(BindingFlags.Public ||| BindingFlags.Static) |> Array.map (fun field -> let opcode=field.GetValue(null) :?> OpCode in uint16 opcode.Value,opcode) |> Map.ofArray
        [ for typ in assembly.GetTypes() do
            for method in typ.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly) do
                let body=method.GetMethodBody()
                if not (isNull body) then
                    let bytes=(nonNull body).GetILAsByteArray() |> nonNull
                    let mutable offset=0
                    while offset<bytes.Length do
                        let first=bytes.[offset]
                        offset <- offset+1
                        let code=if first=0xfeuy then let second=bytes.[offset] in offset <- offset+1; 0xfe00us ||| uint16 second else uint16 first
                        let opcode=opcodes.[code]
                        let memberInfo=
                            if opcode.OperandType=OperandType.InlineMethod then
                                let declaringArgs=if typ.IsGenericType then typ.GetGenericArguments() else [||]
                                method.Module.ResolveMember(BitConverter.ToInt32(bytes,offset),declaringArgs,method.GetGenericArguments()) |> Option.ofObj
                            else None
                        yield method,opcode,memberInfo
                        let length=
                            match opcode.OperandType with
                            | OperandType.InlineNone -> 0
                            | OperandType.ShortInlineBrTarget | OperandType.ShortInlineI | OperandType.ShortInlineVar -> 1
                            | OperandType.InlineVar -> 2
                            | OperandType.InlineI8 | OperandType.InlineR -> 8
                            | OperandType.InlineSwitch -> 4+4*BitConverter.ToInt32(bytes,offset)
                            | _ -> 4
                        offset <- offset+length ]
    let load source =
        let files=Harness.generate Harness.options (Harness.compile source)
        Assembly.Load((files |> List.find (fun file -> file.RelativePath="CompiledScript.dll")).Content)

[<TestFixture>]
type BackendTests() =
    [<Test>]
    member _.``A pure external type does not imply a pure constructor`` () =
        let native=typeof<ProviderFixture.ConstructedValue>
        let registration: ExtensionRegistration={HostInterop.empty with Types=[{Name="NativeValue";Fields=Map.ofList ["Value",TInt];HostManaged=false}]}
        let contracts={PurityContracts.empty with PureTypes=Set.singleton "NativeValue"}
        let input={Program=Parser.parseProgram "let discarded = { Value=7 }\n42";Externs=[]}
        let program=match Compiler.compileWithRegistrationAndPurity registration contracts input with Ok value -> value.Lir | Error errors -> failwith (string errors)
        let linked=ClrBindings.bindType "NativeValue" native program
        ProviderFixture.ConstructedValue.Reset()
        Assert.That(Harness.executeWith {Harness.options with References=[native.Assembly.Location]} linked,Is.EqualTo("42"))
        Assert.That(ProviderFixture.ConstructedValue.Created,Is.EqualTo(1))

    [<TestCase(false,"match value with | { Value=7 } -> 1 | _ -> 0")>]
    [<TestCase(true,"match value with | { Value=7 } -> 1 | _ -> 0")>]
    [<TestCase(false,"value.Value")>]
    [<TestCase(true,"value.Value")>]
    [<TestCase(false,"value = value")>]
    [<TestCase(true,"value = value")>]
    [<TestCase(false,"$\"{value}\"")>]
    [<TestCase(true,"$\"{value}\"")>]
    member _.``Native getter pruning requires a pure external type`` (isPure: bool, expression: string) =
        let registration: ExtensionRegistration={HostInterop.empty with Types=[{Name="NativeValue";Fields=Map.ofList ["Value",TInt];HostManaged=true}]}
        let external: CompilerExtern={Name="Native.create";Scheme=Forall([],TFun(TUnit,TNamed "NativeValue"));Arity=1;Kind=UnsupportedHostExtern}
        let contracts=if isPure then {PurityContracts.empty with PureTypes=Set.singleton "NativeValue"} else PurityContracts.empty
        let native=typeof<ProviderFixture.QuietValue>
        let compiled=match Compiler.compileWithRegistrationAndPurity registration contracts {Program=Parser.parseProgram ("let value = Native.create ()\nlet discarded = "+expression+"\n42");Externs=[external]} with Ok value -> value.Lir | Error errors -> failwith (string errors)
        let linked=compiled |> ClrBindings.bindType "NativeValue" native |> ClrBindings.bindMethod "Native.create" (native.GetConstructor(System.Type.EmptyTypes) |> nonNull)
        let options={Harness.options with References=[native.Assembly.Location]}
        let binary=Harness.generate options linked |> List.find (fun file -> file.RelativePath="CompiledScript.dll")
        let assembly=Assembly.Load(binary.Content)
        let callsGetter=IL.instructions assembly |> List.exists (fun (_,_,memberInfo) -> memberInfo |> Option.exists (fun info -> info.DeclaringType=native && info.Name="get_Value"))
        Assert.That(callsGetter,Is.EqualTo(not isPure))
        Assert.That(Harness.executeWith options linked,Is.EqualTo("42"))

    [<Test>]
    member _.``Unmarked external getters execute once and cannot be duplicated`` () =
        let native=typeof<ProviderFixture.ObservedValue>
        let registration: ExtensionRegistration={HostInterop.empty with Types=[{Name="NativeValue";Fields=Map.ofList ["Value",TInt];HostManaged=true}]}
        let external: CompilerExtern={Name="Native.create";Scheme=Forall([],TFun(TUnit,TNamed "NativeValue"));Arity=1;Kind=UnsupportedHostExtern}
        let source="let value = Native.create ()\nlet useValue () =\n    let read = value.Value\n    (read,read)\nuseValue ()"
        let program=match Compiler.compileWithRegistration registration {Program=Parser.parseProgram source;Externs=[external]} with Ok value -> value.Lir | Error errors -> failwith (string errors)
        let linked=program |> ClrBindings.bindType "NativeValue" native |> ClrBindings.bindMethod "Native.create" (native.GetConstructor(System.Type.EmptyTypes) |> nonNull)
        ProviderFixture.ObservedValue.Reset()
        Assert.That(Harness.executeWith {Harness.options with References=[native.Assembly.Location]} linked,Is.EqualTo("(7, 7)"))
        Assert.That(ProviderFixture.ObservedValue.Reads,Is.EqualTo(1))

    [<TestCase("let value = Sdk.create \"hello\"\nvalue.Name", "\"hello\"")>]
    [<TestCase("Sdk.apply (fun value -> value+2) 40", "42")>]
    [<TestCase("Sdk.identity (Some [1;2])", "Some [1;2]")>]
    [<TestCase("Sdk.await (Task.spawn (fun () -> 42))", "42")>]
    [<TestCase("type Choice = Number of int | Text of string\nSdk.identity (Text \"hello\")", "Text \"hello\"")>]
    [<TestCase("Sdk.identity (ForeignState.Count 42)", "Count 42")>]
    [<TestCase("let secret=Sdk.create \"hello\"\nmatch secret with | { Name = name } -> name", "\"hello\"")>]
    member _.``Existing SDK providers marshal compiled values callbacks and tasks`` (source: string, expected: string) =
        let registration=ProviderFixture.Provider.Provide() |> FScript.Runtime.ExtensionBridge.toLanguageRegistration
        let compiled=match Compiler.compileWithRegistration registration {Program=Parser.parseProgram source; Externs=[]} with Ok p -> p | Error errors -> failwith (String.concat "; " (errors |> List.map _.Message))
        let provider=typeof<ProviderFixture.Provider>.GetMethod("Provide") |> nonNull
        let program=ClrBindings.bindProvider (registration.Functions |> List.map _.Name) provider compiled.Lir
        let program=match IrArtifact.deserialize (IrArtifact.serialize program) with Ok p -> p | Error errors -> failwith (String.concat "; " (errors |> List.map _.Message))
        let references=[typeof<ProviderFixture.Provider>.Assembly.Location; typeof<FScript.Extensibility.ScriptValue>.Assembly.Location; typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location]
        Assert.That(Harness.executeWith {Harness.options with References=references} program,Is.EqualTo(expected))

    [<Test>]
    member _.``SDK providers receive compiled default functions through HostContext`` () =
        let defaultFunctions=FScript.Runtime.Registry.all {RootDirectory=Directory.GetCurrentDirectory(); DeniedPathGlobs=[]} |> FScript.Runtime.ExtensionBridge.toExtensionFunctions
        let context=FScript.Extensibility.HostContext(Directory.GetCurrentDirectory(),[],defaultFunctions)
        let registration=ProviderFixture.ContextProvider.Provide(context) |> FScript.Runtime.ExtensionBridge.toLanguageRegistration
        let compiled=match Compiler.compileWithRegistration registration {Program=Parser.parseProgram "Sdk.guid () |> Option.isSome"; Externs=[]} with Ok p -> p | Error errors -> failwith (String.concat "; " (errors |> List.map _.Message))
        let provider=typeof<ProviderFixture.ContextProvider>.GetMethod("Provide") |> nonNull
        let program=ClrBindings.bindProvider ["Sdk.guid"] provider compiled.Lir
        let references=[typeof<ProviderFixture.Provider>.Assembly.Location; typeof<FScript.Extensibility.ScriptValue>.Assembly.Location; typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location]
        Assert.That(Harness.executeWith {Harness.options with References=references} program,Is.EqualTo("true"))
    [<TestCase("let add x y = x+y\nadd 1 2", "3")>]
    [<TestCase("let add x y = x+y\n(add 1 2, add 1.5 2.5)", "(3, 4)")>]
    [<TestCase("let make x = fun y -> x+y\nlet add = make 10\nadd 32", "42")>]
    [<TestCase("let make n =\n    let x=n+1\n    fun y -> x+y\nmake 1 40", "42")>]
    [<TestCase("let add x y = x+y\nlet increment=add 1\n(increment 1,increment 41)", "(2, 42)")>]
    [<TestCase("let twice f x = f (f x)\ntwice (fun x -> x+1) 40", "42")>]
    [<TestCase("let outer seed =\n    let add value = seed+value\n    fun delta -> add delta\nList.map (outer 10) [1;2]", "[11;12]")>]
    [<TestCase("let rec swap n a b = if n=0 then (a,b) else swap (n-1) b a\nswap 3 1 2", "(2, 1)")>]
    [<TestCase("let rec collect n acc = if n=0 then List.map (fun f -> f ()) acc else collect (n-1) ((fun () -> n)::acc)\ncollect 3 []", "[1;2;3]")>]
    [<TestCase("(List.map (fun n -> n+1) [1;2], List.filter (fun n -> n>1) [1;2], List.fold (fun s n -> s+n) 0 [1;2], List.choose (fun n -> if n>1 then Some n else None) [1;2], List.collect (fun n -> [n;n]) [1;2], List.exists (fun n -> n=2) [1;2], List.tryFind (fun n -> n=2) [1;2], List.tryGet (fun n -> n=2) [1;2])", "([2;3], [2], 3, [2], [1;1;2;2], true, Some 2, Some 1)")>]
    [<TestCase("let rec fib n = if n <= 1 then n else fib (n-1) + fib (n-2)\nfib 10", "55")>]
    [<TestCase("let rec even n = if n=0 then true else odd (n-1)\nand odd n = if n=0 then false else even (n-1)\neven 20", "true")>]
    [<TestCase("[3..1] |> List.map (fun n -> n*2)", "[6;4;2]")>]
    [<TestCase("(Some [1;2]) = (Some [1;2])", "true")>]
    [<TestCase("false && raise \"unreachable\"", "false")>]
    [<TestCase("true || raise \"unreachable\"", "true")>]
    [<TestCase("match Some (1,2) with | Some (a,b) when a=1 -> a+b | _ -> 0", "3")>]
    [<TestCase("type Choice = Number of int | Text of string\nmatch Text \"ok\" with | Number n -> Int.toString n | Text s -> s", "\"ok\"")>]
    [<TestCase("type Color = Red | Blue\nmatch Red with | Red -> 1 | Blue -> 2", "1")>]
    [<TestCase("let m = { [true] = 1; [false] = 2 }\nm[false]", "Some 2")>]
    [<TestCase("match { [\"a\"] = 1; [\"b\"] = 2 } with | { [k] = 2; ..rest } -> (k, Map.count rest) | _ -> (\"\",0)", "(\"\", 0)")>]
    [<TestCase("let x = {| Name = \"Ada\"; Age = 42 |}\n{| x with Age = 43 |}.Age", "43")>]
    [<TestCase("Task.spawn (fun () -> 42) |> Task.await", "42")>]
    [<TestCase("Regex.matchGroups \"(a)(b)\" \"ab\"", "Some [\"a\";\"b\"]")>]
    [<TestCase("match { [1] = 2; [2] = 2 } with | { [x] = x; ..rest } -> (x,Map.containsKey 1 rest) | _ -> (0,false)", "(0, false)")>]
    [<TestCase("type Address = { City: string }\nmatch { City=\"Paris\" } with | {| City: string |} -> 1 | _ -> 2", "1")>]
    [<TestCase("type Address = { City: string; Zip: int }\nlet check value = match value with | {| City: string |} -> 1 | _ -> 2\ncheck { City=\"Paris\"; Zip=75000 }", "1")>]
    member _.``Generated assemblies execute core semantics and every closed method JITs`` (source: string, expected: string) =
        let actual=Harness.execute (Harness.compile source)
        Assert.That(actual, Is.EqualTo(expected))
        Assert.That(actual, Is.EqualTo(Harness.legacy source))

    [<TestCase("Json.serialize (Some {| Count=2; Name=\"Ada\" |})")>]
    [<TestCase("Json.serialize { [\"quote\\\"\"] = 1 }")>]
    [<TestCase("type Person = { Name: string; Age: int option }\nlet person (value: Person option) = value\nJson.deserialize typeof Person \"{\\\"Name\\\":\\\"Ada\\\"}\" |> person")>]
    [<TestCase("Xml.queryValues \"//item/@name\" \"<root><item name=\\\"a\\\"/><item name=\\\"b\\\"/></root>\"")>]
    [<TestCase("Xml.queryValues \"root/item/text()\" \"<root><item> x </item><item> </item></root>\"")>]
    [<TestCase("let values=Some [\"x\";\"y\"]\n$\"{values}\"")>]
    [<TestCase("{| Z=1; A=2 |}")>]
    member _.``Generated formatting JSON XML and interpolation conform to the legacy evaluator`` (source: string) =
        Assert.That(Harness.execute (Harness.compile source), Is.EqualTo(Harness.legacy source))

    [<TestCase("let rec walk n acc = if n=0 then acc else walk (n-1) (acc+1)\nwalk 1000000 0")>]
    [<TestCase("let count start =\n    let rec walk n acc = if n=0 then acc else walk (n-1) (acc+1)\n    walk start 0\ncount 1000000")>]
    [<TestCase("let rec walk n acc = match n with | 0 -> acc | _ -> walk (n-1) (acc+1)\nwalk 1000000 0")>]
    member _.``Self tail recursion runs one million steps with bounded stack`` (source: string) =
        Assert.That(Harness.execute (Harness.compile source),Is.EqualTo("1000000"))

    [<TestCase("let add x y=x+y\nlet increment=add 1\nincrement 41")>]
    [<TestCase("let twice f x=f (f x)\ntwice (fun x -> x+1) 40")>]
    [<TestCase("List.map (fun x -> x+1) [1;2] |> List.fold (fun s x -> s+x) 0")>]
    member _.``Known calls and partial applications contain no delegate invocation in IL`` (source: string) =
        let instructions=IL.load source |> IL.instructions
        let invokes=instructions |> List.choose (fun (_,_,memberInfo) -> memberInfo |> Option.filter (fun memberInfo -> memberInfo.Name="Invoke" && not (isNull memberInfo.DeclaringType) && typeof<MulticastDelegate>.IsAssignableFrom(memberInfo.DeclaringType)))
        Assert.That(invokes,Is.Empty)

    [<Test>]
    member _.``Unused runtime types helpers and record machinery are absent from the assembly`` () =
        let assembly=IL.load "40+2"
        Assert.That(String.concat "," (assembly.GetTypes() |> Array.map _.Name),Is.EqualTo("Program"))
        let names=(assembly.GetType("Generated.Program") |> nonNull).GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static) |> Array.map _.Name
        Assert.That(names |> Array.exists (fun name -> name.StartsWith("Json",StringComparison.Ordinal) || name.StartsWith("Equal",StringComparison.Ordinal) || name.StartsWith("Encode",StringComparison.Ordinal)),Is.False)

    [<TestCase(false)>]
    [<TestCase(true)>]
    member _.``Hello with environment and console retains only used stdlib members`` (executable: bool) =
        let source="let name = match Env.Arguments with | head :: _ -> head | _ -> \"Unknown\"\nConsole.writeLine $\"Hello {name} !\""
        let options={Harness.options with OutputKind=if executable then ConsoleApplication else Library}
        let files=Harness.generate options (Harness.compile source)
        let assembly=Assembly.Load((files |> List.find (fun file -> file.RelativePath="CompiledScript.dll")).Content)
        let runtime=assembly.GetType("Generated.FsRuntime") |> nonNull
        let methods=runtime.GetMethods(BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.DeclaredOnly) |> Array.map _.Name |> Array.sort
        Assert.That(String.concat "," methods,Is.EqualTo("WriteLine"))
        Assert.That(assembly.GetType("Generated.OrderedMap`2"),Is.Null)
        Assert.That(assembly.GetType("Generated.TypeToken"),Is.Null)
        let program=assembly.GetType("Generated.Program") |> nonNull
        let helpers=program.GetMethods(BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.DeclaredOnly) |> Array.map _.Name
        Assert.That(helpers |> Array.exists (fun name -> name.StartsWith("Json",StringComparison.Ordinal) || name.StartsWith("Equal",StringComparison.Ordinal) || name.StartsWith("Encode",StringComparison.Ordinal) || name.StartsWith("Decode",StringComparison.Ordinal)),Is.False)

    [<Test>]
    member _.``A warmed fully static numeric program allocates no managed values`` () =
        let assembly=IL.load "export let compute (value: int) =\n    let add x y=x+y\n    let increment=add 1\n    increment value"
        let run=((assembly.GetType("Generated.Program") |> nonNull).GetProperty("v_compute") |> nonNull).GetValue(null) |> nonNull :?> Func<int64,int64>
        for input in 1..1000 do run.Invoke(int64 input) |> ignore
        let before=GC.GetAllocatedBytesForCurrentThread()
        let mutable result=0L
        for input in 1..10000 do result <- run.Invoke(int64 input)
        let allocated=GC.GetAllocatedBytesForCurrentThread()-before
        Assert.That(result,Is.EqualTo(10001L))
        Assert.That(allocated,Is.Zero)

    [<Test>]
    member _.``Redundant true checks disappear from IL while CSharp output retains its policy`` () =
        let source="export let check value = value && true"
        let program=Harness.compile source
        let files=match CSharpBackend.generate {Namespace="Generated";ClassName="Program";References=[]} program with Ok files -> files | Error errors -> failwith (String.concat "; " (errors |> List.map _.Message))
        Assert.That((files |> List.find (fun file -> file.RelativePath="Runtime.cs")).Content,Does.Contain("public readonly record struct Unit"))
        let instructions=IL.load source |> IL.instructions |> List.filter (fun (method,_,_) -> method.Name.StartsWith("Function",StringComparison.Ordinal) && method.ReturnType=typeof<bool>)
        Assert.That(instructions,Is.Not.Empty)
        Assert.That(instructions |> List.exists (fun (_,opcode,_) -> opcode=OpCodes.Ldc_I4_1),Is.False)

    [<Test>]
    member _.``Closed library exports remain roots without a program entry`` () =
        let assembly=IL.load "export let answer=42\nexport let add (value: int)=value+1"
        let typ=assembly.GetType("Generated.Program") |> nonNull
        Assert.That((typ.GetProperty("v_answer") |> nonNull).GetValue(null),Is.EqualTo(box 42L))
        let add=((typ.GetProperty("v_add") |> nonNull).GetValue(null) |> nonNull) :?> Func<int64,int64>
        Assert.That(add.Invoke(41L),Is.EqualTo(42L))

    [<Test>]
    member _.``Open polymorphic exports cannot silently disappear from native libraries`` () =
        match DotNetBackend.generate Harness.options (Harness.compile "export let identity value=value") with
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code,Does.Contain("FSC6009"))
        | Ok _ -> Assert.Fail("An exported open function needs a concrete native signature.")

    [<TestCase("let field name =\n    Console.writeLine name\n    1\n{| Z=field \"first\"; A=field \"second\" |}")>]
    [<TestCase("let values () =\n    Console.writeLine \"first\"\n    [1]\nlet index () =\n    Console.writeLine \"second\"\n    0\n(values ())[index ()]")>]
    [<TestCase("let step name n =\n    Console.writeLine name\n    n\nlet rec loop a b = if a=0 then b else loop (step \"first\" (a-1)) (step \"second\" (b+1))\nloop 1 0")>]
    [<TestCase("let capture () =\n    Console.writeLine \"first\"\n    1\nlet add x y=x+y\nlet increment=add (capture ())\nConsole.writeLine \"second\"\n(increment 1,increment 2)")>]
    member _.``Aggregate construction and indexing preserve effect order`` (source: string) =
        let capture action =
            let previous=Console.Out
            use output=new StringWriter()
            try
                Console.SetOut(output)
                let result=action ()
                result,output.ToString()
            finally Console.SetOut(previous)
        let expected=capture (fun () -> Harness.legacy source)
        let actual=capture (fun () -> Harness.execute (Harness.compile source))
        Assert.That(actual,Is.EqualTo(expected))
        Assert.That(snd actual,Is.EqualTo("first"+Environment.NewLine+"second"+Environment.NewLine))

    [<Test>]
    member _.``Filesystem globbing preserves relative paths and recursive selection`` () =
        let previous=Directory.GetCurrentDirectory()
        let directory=Path.Combine(Path.GetTempPath(),"fscript-native-fs-"+Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(Path.Combine(directory,"nested")) |> ignore
        File.WriteAllText(Path.Combine(directory,"root.txt"),"root")
        File.WriteAllText(Path.Combine(directory,"nested","child.txt"),"child")
        File.WriteAllText(Path.Combine(directory,"nested","skip.log"),"skip")
        try
            Directory.SetCurrentDirectory(directory)
            let source="(Fs.glob \"**/*.txt\", Fs.enumerateFiles \"nested\" \"*.txt\", Fs.enumerateFiles \"missing\" \"*.txt\")"
            Assert.That(Harness.execute (Harness.compile source),Is.EqualTo(Harness.legacy source))
        finally
            Directory.SetCurrentDirectory(previous)
            Directory.Delete(directory,true)

    [<Test>]
    member _.``Exact CLR calls survive serialization`` () =
        let external: CompilerExtern = {Name="Native.abs"; Scheme=Forall([],TFun(TInt,TInt)); Arity=1; Kind=UnsupportedHostExtern}
        let compiled =
            match Compiler.compile {Program=Parser.parseProgram "Native.abs (0-42)"; Externs=[external]} with
            | Ok compiled -> compiled.Lir
            | Error errors -> failwith (String.concat "; " (errors |> List.map _.Message))
        let method=typeof<Math>.GetMethod("Abs",[|typeof<int64>|]) |> nonNull
        let linked=ClrBindings.bindMethod "Native.abs" method compiled
        let linked=match IrArtifact.deserialize (IrArtifact.serialize linked) with Ok p -> p | Error errors -> failwith (String.concat "; " (errors |> List.map _.Message))
        Assert.That(Harness.execute linked, Is.EqualTo("42"))

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
    member _.``Missing managed references return diagnostics without emitting files`` () =
        let program=Harness.compile "42"
        let missing=Path.Combine(Path.GetTempPath(),Guid.NewGuid().ToString("N")+".dll")
        Assert.That(DotNetBackend.generate {Harness.options with References=[missing]} program |> Result.isError,Is.True)
        Assert.That(CSharpBackend.generate {Namespace="Generated";ClassName="Program";References=[missing]} program |> Result.isError,Is.True)

    [<Test>]
    member _.``Executable publishing validates target and output kind before running the SDK`` () =
        let program=Harness.compile "42"
        let check executable options expected =
            match DotNetBackend.generateExecutable executable options program with
            | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code,Does.Contain(expected))
            | Ok _ -> Assert.Fail("Invalid publishing options must fail before publishing.")
        let current=System.Runtime.InteropServices.RuntimeInformation.RuntimeIdentifier
        check {RuntimeIdentifier=current;AheadOfTime=false} Harness.options "FSN7101"
        let console={Harness.options with OutputKind=ConsoleApplication}
        check {RuntimeIdentifier="darwin";AheadOfTime=false} console "FSN7101"
        let foreign=if OperatingSystem.IsWindows() then "linux-x64" else "win-x64"
        check {RuntimeIdentifier=foreign;AheadOfTime=true} console "FSN7104"

    [<Test>]
    member _.``Unused provider references and unresolved external types are omitted`` () =
        let registration: ExtensionRegistration={Types=[{Name="Unused";Fields=Map.ofList ["Value",TInt];HostManaged=true}];Unions=[];Functions=[]}
        let program=match Compiler.compileWithRegistration registration {Program=Parser.parseProgram "42";Externs=[]} with Ok p -> p.Lir | Error errors -> failwith (String.concat "; " (errors |> List.map _.Message))
        let imported: Lir.ImportedRecord={Name="Unused";ModulePath="clr:Missing.Type, Missing.Assembly";MemberName="Missing.Type";Fields=["Value",TInt];IsUnion=false;Span=program.Types.Head.Span}
        let program={program with ImportedRecords=[imported];Types=program.Types |> List.map (fun t -> if t.Name="Unused" then {t with Import=Some imported} else t)}
        let files=Harness.generate {Harness.options with References=[typeof<ProviderFixture.Provider>.Assembly.Location]} program
        Assert.That(files.Length,Is.EqualTo(1))
        let assembly=Assembly.Load(files.Head.Content)
        Assert.That(assembly.GetReferencedAssemblies() |> Array.exists (fun reference -> List.contains reference.Name ["FScript.Extensibility";"FSharp.Core";"FScript.Backend.DotNet.Tests"]),Is.False)

    [<Test>]
    member _.``AOT rejects reachable legacy SDK dispatch with a static binding diagnostic`` () =
        let registration=ProviderFixture.Provider.Provide() |> FScript.Runtime.ExtensionBridge.toLanguageRegistration
        let program=match Compiler.compileWithRegistration registration {Program=Parser.parseProgram "Sdk.apply (fun n -> n+1) 41";Externs=[]} with Ok p -> p.Lir | Error errors -> failwith (String.concat "; " (errors |> List.map _.Message))
        let program=ClrBindings.bindProvider ["Sdk.apply"] (typeof<ProviderFixture.Provider>.GetMethod("Provide") |> nonNull) program
        let options={Harness.options with OutputKind=ConsoleApplication;References=[typeof<ProviderFixture.Provider>.Assembly.Location;typeof<FScript.Extensibility.ScriptValue>.Assembly.Location;typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location]}
        match DotNetBackend.generateExecutable {RuntimeIdentifier=System.Runtime.InteropServices.RuntimeInformation.RuntimeIdentifier;AheadOfTime=true} options program with
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code,Does.Contain("FSN7105"))
        | Ok _ -> Assert.Fail("Opaque factory dispatch requires a static binding for Native AOT.")

    [<Test>]
    member _.``Dependency pruning uses assembly identity rather than a renamed filename`` () =
        let registration=ProviderFixture.Provider.Provide() |> FScript.Runtime.ExtensionBridge.toLanguageRegistration
        let program=match Compiler.compileWithRegistration registration {Program=Parser.parseProgram "Sdk.apply (fun n -> n+1) 41";Externs=[]} with Ok p -> p.Lir | Error errors -> failwith (String.concat "; " (errors |> List.map _.Message))
        let program=ClrBindings.bindProvider ["Sdk.apply"] (typeof<ProviderFixture.Provider>.GetMethod("Provide") |> nonNull) program
        let references=[typeof<ProviderFixture.Provider>.Assembly.Location;typeof<FScript.Extensibility.ScriptValue>.Assembly.Location;typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location]
        let root=Harness.generate {Harness.options with References=references} program |> List.find (fun file -> file.RelativePath="CompiledScript.dll")
        let renamed=Path.Combine(Path.GetTempPath(),"renamed-provider-"+Guid.NewGuid().ToString("N")+".dll")
        try
            File.Copy(typeof<ProviderFixture.Provider>.Assembly.Location,renamed)
            Assert.That(String.concat "," (ClrBindings.referencedDependencies root.Content [renamed]),Is.EqualTo(renamed))
            let options={Harness.options with References=renamed :: (List.tail references)}
            let files=Harness.generate options program
            Assert.That(files |> List.map _.RelativePath,Does.Contain("FScript.Backend.DotNet.Tests.dll"))
        finally File.Delete renamed

    [<Test>]
    member _.``CSharp projects compile independently`` () =
        let source=Harness.compile "[1..3] |> List.map (fun n -> n+1)"
        let files=match CSharpBackend.generate {Namespace="Generated"; ClassName="Program"; References=[]} source with Ok f -> f | Error e -> failwith (String.concat "; " (e |> List.map _.Message))
        let directory=Path.Combine(Path.GetTempPath(), "fscript-csharp-"+Guid.NewGuid().ToString("N"))
        try
            Directory.CreateDirectory directory |> ignore
            for file in files do File.WriteAllText(Path.Combine(directory,file.RelativePath),file.Content)
            File.WriteAllText(Path.Combine(directory,"Unused.cs"),"This stale file is not part of the generated project.")
            let start=Diagnostics.ProcessStartInfo("dotnet",WorkingDirectory=directory,RedirectStandardOutput=true,RedirectStandardError=true)
            start.ArgumentList.Add("run")
            start.ArgumentList.Add("--project")
            start.ArgumentList.Add("Program.csproj")
            use proc=Diagnostics.Process.Start(start) |> nonNull
            let output=proc.StandardOutput.ReadToEnd()
            let error=proc.StandardError.ReadToEnd()
            Assert.That(proc.WaitForExit(60000),Is.True)
            Assert.That(proc.ExitCode,Is.EqualTo(0),output+error)
            Assert.That(output.Trim(),Is.EqualTo("[2;3;4]"))
        finally
            if Directory.Exists directory then Directory.Delete(directory,true)
