using FScript.CSharpInterop;
using NUnit.Framework;

namespace FScript.LanguageServer.Tests;

[TestFixture]
public sealed class InteropServicesTests
{
    [Test]
    public void Interop_parses_and_infers_a_simple_script()
    {
        const string script = "let add x y = x + y";
        const string sourcePath = "/tmp/interop-test.fss";
        var externs = InteropServices.runtimeExternsForSourcePath(sourcePath);
        var program = InteropServices.parseProgramFromSourceWithIncludes(sourcePath, script);
        var inferred = InteropServices.inferProgramWithExternsAndLocalVariableTypes(externs, program);
        var typed = inferred.Item1;
        Assert.That(typed, Is.Not.Null);
    }
}
