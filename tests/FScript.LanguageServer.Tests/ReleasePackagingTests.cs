using System.Reflection;
using NUnit.Framework;

namespace FScript.LanguageServer.Tests;

[TestFixture]
public sealed class ReleasePackagingTests
{
    [Test]
    public void Staged_server_assemblies_match_the_published_package_version()
    {
        var directory = Environment.GetEnvironmentVariable("FSCRIPT_RELEASE_SERVER_DIR");
        if (string.IsNullOrWhiteSpace(directory))
            Assert.Ignore("Release staging directory is supplied by the tag workflow.");
        var releaseVersion = Environment.GetEnvironmentVariable("FSCRIPT_RELEASE_VERSION");
        Assert.That(releaseVersion, Is.Not.Null.And.Not.Empty);
        var expected = Version.Parse(releaseVersion!);
        expected = new Version(expected.Major, expected.Minor, expected.Build, 0);
        var sdk = AssemblyName.GetAssemblyName(Path.Combine(directory!, "FScript.Extensibility.dll"));
        Assert.That(sdk.Version, Is.EqualTo(new Version(1, 0, 0, 0)),
            "The SDK assembly identity is independent of the product release.");
        Assert.That(sdk.GetPublicKeyToken(), Is.Empty, "The SDK is unsigned.");
        foreach (var name in new[] { "FScript.Language", "FScript.Runtime", "FScript.CSharpInterop", "FScript.LanguageServer" })
        {
            var assembly = AssemblyName.GetAssemblyName(Path.Combine(directory!, name + ".dll"));
            Assert.That(assembly.Version, Is.EqualTo(expected),
                $"{name} must match the NuGet package assembly identity so configured providers can load.");
        }
    }
}
