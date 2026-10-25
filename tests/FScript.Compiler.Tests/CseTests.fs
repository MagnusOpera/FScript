namespace FScript.Compiler.Tests

open System
open FScript.Compiler
open FScript.Language
open NUnit.Framework

[<TestFixture>]
type CseTests() =
    let span=Span.mk (Span.pos 0 0) (Span.pos 0 0)
    let expr t node : Lir.Expression={Type=t;Span=span;Node=node}
    let number n=expr TInt (Lir.Constant(Lir.Int n))
    let load id=expr TInt (Lir.Load(Lir.SymbolId id))
    let add a b=expr TInt (Lir.Operation(Lir.IntAdd,[a;b]))
    let tuple values=expr (TTuple(List.map (fun (v: Lir.Expression) -> v.Type) values)) (Lir.Tuple values)
    let empty entry : Lir.Program={Types=[];ImportedRecords=[];Functions=[];Declarations=[];Entry=Some entry}
    let count label program=(Lir.format program).Split('\n') |> Array.filter (fun line -> line.TrimStart().StartsWith(label,StringComparison.Ordinal)) |> Array.length
    let optimize p=LirCommonSubexpressions.optimize p
    let compile externs purity source =
        let p=match Compiler.compileWithPurity purity {Program=Parser.parseProgram source;Externs=externs} with Ok p -> p.Lir | Error errors -> failwith (string errors)
        let p=match LirMonomorphization.specialize p with Ok p -> p | Error errors -> failwith (string errors)
        LirSimplification.optimize p
    let host={Name="Host.calculate";Scheme=Forall([],TFun(TInt,TInt));Arity=1;Kind=UnsupportedHostExtern}
    let pureHost={PurityContracts.empty with PureFunctions=Set.singleton host.Name}
    let validated p=match LirValidation.validate p with Ok _ -> () | Error errors -> Assert.Fail(string errors)

    [<Test>]
    member _.``Repeated nested expressions share one evaluation despite different spans`` () =
        let first=add (number 1L) (number 2L)
        let second={first with Span=Span.mk (Span.pos 2 1) (Span.pos 2 4)}
        let p=empty (tuple [first;second]) |> optimize
        Assert.That(count "operation IntAdd" p,Is.EqualTo(1))
        Assert.That(count "let " p,Is.EqualTo(1))
        validated p

    [<Test>]
    member _.``Unshared computations retain their original shape`` () =
        let p=empty (add (number 1L) (number 2L))
        Assert.That(Lir.format (optimize p),Is.EqualTo(Lir.format p))

    [<Test>]
    member _.``Branches neither hoist nor leak available expressions`` () =
        let work=add (number 1L) (number 2L)
        let branch=expr TInt (Lir.Branch(expr TBool (Lir.Constant(Lir.Bool true)),work,work))
        let p=empty (tuple [branch;work]) |> optimize
        Assert.That(count "operation IntAdd" p,Is.EqualTo(3))
        Assert.That(count "let " p,Is.EqualTo(0))
        validated p

    [<Test>]
    member _.``Dominating computation is available in both branches`` () =
        let work=add (number 1L) (number 2L)
        let branch=expr TInt (Lir.Branch(expr TBool (Lir.Constant(Lir.Bool true)),work,work))
        let p=empty (tuple [work;branch]) |> optimize
        Assert.That(count "operation IntAdd" p,Is.EqualTo(1))
        validated p

    [<TestCase(false)>]
    [<TestCase(true)>]
    member _.``FFI calls share only with a pure contract`` isPure =
        let original=compile [host] (if isPure then pureHost else PurityContracts.empty) "(Host.calculate 7, Host.calculate 7)"
        let optimized=optimize original
        Assert.That(count "call-closure" optimized,Is.EqualTo(if isPure then 1 else 2))
        validated optimized

    [<Test>]
    member _.``Observable work is a reuse barrier`` () =
        let original=compile [host] pureHost "(Host.calculate 7,Console.writeLine \"between\",Host.calculate 7)"
        Assert.That(count "call-closure" (optimize original),Is.EqualTo(3))

    [<Test>]
    member _.``Aliases and top-level declarations preserve value numbers`` () =
        let original=compile [host] pureHost "let first=Host.calculate 7\nlet alias=first\n(alias,Host.calculate 7)"
        let optimized=optimize original
        Assert.That(count "call-closure" optimized,Is.EqualTo(1))
        validated optimized

    [<Test>]
    member _.``Pattern guards invalidate availability for following alternatives`` () =
        let original=compile [host] pureHost "let first=Host.calculate 7\nlet guard () =\n    Console.writeLine \"guard\"\n    false\n(first, match 1 with | n when guard () -> 0 | _ -> Host.calculate 7)"
        let optimized=optimize original
        Assert.That(count "call-closure" optimized,Is.GreaterThanOrEqualTo(3))
        validated optimized

    [<Test>]
    member _.``Function bodies cannot reuse values from another invocation`` () =
        let original=compile [host] pureHost "let work value = (Host.calculate value,Host.calculate value)\n(work 1,work 2)"
        let optimized=optimize original
        Assert.That(count "call-closure" optimized,Is.EqualTo(3))
        validated optimized

    [<Test>]
    member _.``Signed floating zero keys remain distinct`` () =
        let float n=expr TFloat (Lir.Constant(Lir.Float n))
        let divide n=expr TFloat (Lir.Operation(Lir.FloatDivide,[float 1.;float n]))
        let optimized=empty (tuple [divide 0.;divide -0.]) |> optimize
        Assert.That(count "operation FloatDivide" optimized,Is.EqualTo(2))

    [<Test>]
    member _.``NaN payloads have reflexive bit-exact keys`` () =
        let n=BitConverter.Int64BitsToDouble(0x7ff8000000000001L)
        let work=expr TFloat (Lir.Operation(Lir.FloatAdd,[expr TFloat (Lir.Constant(Lir.Float n));expr TFloat (Lir.Constant(Lir.Float 1.))]))
        let optimized=empty (tuple [work;work]) |> optimize
        Assert.That(count "operation FloatAdd" optimized,Is.EqualTo(1))

    [<Test>]
    member _.``Potential failures are neither shared nor speculated`` () =
        let work=expr TInt (Lir.Operation(Lir.IntDivide,[number 1L;number 0L]))
        let optimized=empty (tuple [work;work]) |> optimize
        Assert.That(count "operation IntDivide" optimized,Is.EqualTo(2))

    [<Test>]
    member _.``Optimization is deterministic and idempotent`` () =
        let original=compile [host] pureHost "let work n = (Host.calculate (n+1),Host.calculate (n+1))\nwork 7"
        let optimized=optimize original
        Assert.That(Lir.format (optimize original),Is.EqualTo(Lir.format optimized))
        Assert.That(Lir.format (optimize optimized),Is.EqualTo(Lir.format optimized))
        validated optimized

    [<Test>]
    member _.``Short circuit right operands stay conditional`` () =
        let work=expr TBool (Lir.Operation(Lir.StructuralEqual,[add (number 1L) (number 2L);number 3L]))
        let conditional=expr TBool (Lir.Operation(Lir.BooleanAnd,[expr TBool (Lir.Constant(Lir.Bool false));work]))
        let p=empty (tuple [conditional;work]) |> optimize
        Assert.That(count "operation IntAdd" p,Is.EqualTo(2))
        Assert.That(count "let " p,Is.EqualTo(0))
        validated p

    [<Test>]
    member _.``Loop iterations and following expressions have separate availability`` () =
        let work=add (load 100) (number 1L)
        let body=tuple [work;work]
        let source=expr (TList TInt) (Lir.List [number 1L;number 2L])
        let loop=expr TUnit (Lir.ForEach(Lir.SymbolId 100,"item",TInt,source,body))
        let p=empty loop |> optimize
        Assert.That(count "operation IntAdd" p,Is.EqualTo(1))
        validated p

    [<Test>]
    member _.``Pure calls with effectful arguments remain distinct`` () =
        let arg={Name="Host.input";Scheme=Forall([],TFun(TUnit,TInt));Arity=1;Kind=UnsupportedHostExtern}
        let original=compile [host;arg] pureHost "(Host.calculate (Host.input ()),Host.calculate (Host.input ()))"
        let p=optimize original
        Assert.That(count "call-closure" p,Is.EqualTo(4))
        validated p

    [<TestCase(false)>]
    [<TestCase(true)>]
    member _.``External observations require immutable type contracts for reuse`` isPure =
        let registration={HostInterop.empty with Types=[{Name="Handle";Fields=Map.ofList ["Value",TInt];HostManaged=true}]}
        let host={Name="Host.obtain";Scheme=Forall([],TFun(TUnit,TNamed "Handle"));Arity=1;Kind=UnsupportedHostExtern}
        let contracts=if isPure then {PurityContracts.empty with PureTypes=Set.singleton "Handle"} else PurityContracts.empty
        let p=match Compiler.compileWithRegistrationAndPurity registration contracts {Program=Parser.parseProgram "let handle=Host.obtain ()\n(handle.Value,handle.Value)";Externs=[host]} with Ok p -> p.Lir | Error errors -> failwith (string errors)
        let p=match LirMonomorphization.specialize p with Ok p -> p | Error errors -> failwith (string errors)
        let p=LirSimplification.optimize p |> optimize
        Assert.That(count "field-get Value" p,Is.EqualTo(if isPure then 1 else 2))
        validated p

    [<Test>]
    member _.``CSE snapshot has stable lexical bindings`` () =
        let work=add (number 1L) (number 2L)
        let p=empty (tuple [work;work]) |> optimize
        let expected=IO.File.ReadAllText(IO.Path.Combine(__SOURCE_DIRECTORY__,"Snapshots","cse.lir")).Replace("\r\n","\n")
        Assert.That(Lir.format p,Is.EqualTo(expected))

    [<Test>]
    member _.``Function-containing aggregates are not value-numbered`` () =
        let fn: Lir.Function={Id=Lir.FunctionId 0;Parameter=Lir.SymbolId 42;Name="identity";ParameterType=TInt;ReturnType=TInt;Captures=[];Span=span;Body=load 42}
        let closure=expr (TFun(TInt,TInt)) (Lir.MakeClosure(fn.Id,[]))
        let inner=tuple [closure;number 1L]
        let p={empty (tuple [inner;inner]) with Functions=[fn]} |> optimize
        Assert.That(count "tuple" p,Is.EqualTo(3))
        Assert.That(count "let " p,Is.Zero)
        validated p

    [<Test>]
    member _.``Separately constructed callback handles do not acquire equal value numbers`` () =
        let fn: Lir.Function={Id=Lir.FunctionId 0;Parameter=Lir.SymbolId 42;Name="identity";ParameterType=TInt;ReturnType=TInt;Captures=[];Span=span;Body=load 42}
        let callback=expr (TFun(TInt,TInt)) (Lir.MakeClosure(fn.Id,[]))
        let external=expr (TFun(callback.Type,TInt)) (Lir.HostExternal("Host.inspect",1))
        let binding: Lir.Declaration={Id=Lir.SymbolId 100;Name="Host.inspect";Type=external.Type;IsRecursive=false;IsExported=false;IsPure=true;ExternalArity=Some 1;Export=None;Span=span;Value=external}
        let call=expr TInt (Lir.CallClosure(external,callback))
        let p={empty (tuple [call;call]) with Functions=[fn];Declarations=[binding]} |> optimize
        Assert.That(count "call-closure" p,Is.EqualTo(2))
        validated p
