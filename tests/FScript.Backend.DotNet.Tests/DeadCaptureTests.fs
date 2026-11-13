namespace FScript.Backend.DotNet.Tests

open System
open System.IO
open System.Reflection
open FScript.Compiler
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open NUnit.Framework

[<TestFixture>]
type DeadCaptureTests() =
    [<Test>]
    member _.``Forwarded captures survive only when read or their arguments have effects`` () =
        let source = """
using System;
public static class Program
{
    private static int observed;
    private static int Observe() => ++observed;
    private static int NativeFirst(int remaining, int dead, int live)
    {
        int copy = dead;
        return remaining == 0 ? live : NativeSecond(remaining - 1, copy, live);
    }
    private static int NativeSecond(int remaining, int dead, int live) =>
        NativeFirst(remaining, dead, live);
    private static int NativeEffect(int unused) => 0;
    private static int NativeDelegate(int unused) => 0;
    private static Func<int> NativeFactory(Func<int> capture)
    {
        return () => capture();
    }
    private static void NativeCycle()
    {
        Func<int> unused = default(Func<int>);
        unused = NativeFactory(unused);
    }
    public static int Run()
    {
        NativeCycle();
        Func<int, int> callback = NativeDelegate;
        return NativeFirst(3, 99, 41) + NativeEffect(Observe()) + callback(1) + observed;
    }
}
"""
        let references =
            ((AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") |> nonNull) :?> string).Split(Path.PathSeparator)
            |> Array.map (fun path -> MetadataReference.CreateFromFile(path) :> MetadataReference)
        let tree = CSharpSyntaxTree.ParseText(source)
        let options = CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary, optimizationLevel=OptimizationLevel.Release)
        let compilation = CSharpCompilation.Create("DeadCaptureTest", [tree], references, options)
        let optimized = DotNetDeadCaptures.optimize "Program" compilation
        use output = new MemoryStream()
        let result = optimized.Emit(output)
        Assert.That(result.Success, Is.True, String.concat "\n" (result.Diagnostics |> Seq.map string))
        let program = Assembly.Load(output.ToArray()).GetType("Program") |> nonNull
        let parameters name =
            (program.GetMethod(name, BindingFlags.NonPublic ||| BindingFlags.Static) |> nonNull).GetParameters()
            |> Array.map _.Name
        Assert.That(parameters "NativeFirst", Is.EqualTo([|"remaining"; "live"|]))
        Assert.That(parameters "NativeSecond", Is.EqualTo([|"remaining"; "live"|]))
        Assert.That(parameters "NativeEffect", Is.EqualTo([|"unused"|]))
        Assert.That(parameters "NativeDelegate", Is.EqualTo([|"unused"|]))
        let cycle = program.GetMethod("NativeCycle", BindingFlags.NonPublic ||| BindingFlags.Static) |> nonNull
        let body = cycle.GetMethodBody() |> nonNull
        Assert.That(body.GetILAsByteArray(), Is.EqualTo([|byte System.Reflection.Emit.OpCodes.Ret.Value|]))
        Assert.That((program.GetMethod("Run") |> nonNull).Invoke(null, [||]), Is.EqualTo(42))
