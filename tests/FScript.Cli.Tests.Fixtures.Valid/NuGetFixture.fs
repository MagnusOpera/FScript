namespace FScript.Cli.Tests.Fixtures.Valid

open System
open System.IO
open System.IO.Compression

/// Offline package feed shared by runtime, CLI, and packaged-server integration tests.
type NuGetFixture() =
    let directory = Path.Combine(Path.GetTempPath(), $"fscript-nuget-test-{Guid.NewGuid():N}")
    let feed = Path.Combine(directory, "feed")
    let name = "FScript.TestExtension." + Guid.NewGuid().ToString("N")
    do
        Directory.CreateDirectory feed |> ignore
        let config = $"<configuration><packageSources><clear/><add key=\"fixtures\" value=\"{feed}\"/></packageSources></configuration>"
        File.WriteAllText(Path.Combine(directory, "NuGet.Config"), config)
    member _.Directory = directory
    member _.Name = name
    member _.ConfigPath = Path.Combine(directory, "fscript.toml")
    member _.AddPackage(packageName: string, version: string, assemblyPaths: string array, dependencies: string) =
        let path = Path.Combine(feed, $"{packageName}.{version}.nupkg")
        use archive = ZipFile.Open(path, ZipArchiveMode.Create)
        let entry = archive.CreateEntry($"{packageName}.nuspec")
        do
            use writer = new StreamWriter(entry.Open())
            writer.Write($"<package><metadata><id>{packageName}</id><version>{version}</version><authors>FScript tests</authors><description>Offline extension fixture</description>{dependencies}</metadata></package>")
        for assembly in assemblyPaths do
            archive.CreateEntryFromFile(assembly, "lib/net10.0/" + Path.GetFileName assembly) |> ignore
    member this.AddProvider(version: string) =
        this.AddPackage(name, version, [| typeof<ValidExternProvider>.Assembly.Location |], "")
    member _.WriteManifest(version: string) =
        let property = if version = "" then "" else $"version = '{version}'\n"
        File.WriteAllText(Path.Combine(directory, "fscript.toml"), $"[[runtime.packages]]\nname = '{name}'\n{property}")
    interface IDisposable with
        member _.Dispose() = Directory.Delete(directory, true)
