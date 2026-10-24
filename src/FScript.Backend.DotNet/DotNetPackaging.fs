namespace FScript.Compiler

open System
open System.Diagnostics
open System.IO
open System.Security

/// Uses the SDK's supported bundler/native compiler with the optimized FScript PE as input.
module internal DotNetPackaging =
    let private diagnostic code message = {Code=code;Stage=DotNetGeneration;Message=message;Span=None}

    let validate executable outputKind =
        let supported=
            set ["osx-x64";"osx-arm64";"linux-x64";"linux-arm64";"linux-arm";"linux-musl-x64";"linux-musl-arm64";"linux-musl-arm";"win-x64";"win-arm64";"win-x86"]
        if outputKind<>ConsoleApplication then
            Error [diagnostic "FSN7101" "Single-file and Native AOT output require a console application, not a library."]
        elif not (Set.contains executable.RuntimeIdentifier supported) then
            Error [diagnostic "FSN7101" "Specify a portable .NET runtime identifier, for example osx-arm64, linux-x64, or win-x64."]
        elif executable.AheadOfTime &&
             not ((OperatingSystem.IsMacOS() && executable.RuntimeIdentifier.StartsWith("osx-",StringComparison.Ordinal)) ||
                  (OperatingSystem.IsLinux() && executable.RuntimeIdentifier.StartsWith("linux-",StringComparison.Ordinal)) ||
                  (OperatingSystem.IsWindows() && executable.RuntimeIdentifier.StartsWith("win-",StringComparison.Ordinal))) then
            Error [diagnostic "FSN7104" $"Native AOT cannot cross-compile operating systems. Build {executable.RuntimeIdentifier} on its matching OS with the native .NET toolchain; single-file managed output supports cross-OS publishing."]
        else Ok ()

    let publish executable trim (options: DotNetOptions) (files: GeneratedBinary list) =
        let directory=Path.Combine(Path.GetTempPath(),"fscript-publish-"+Guid.NewGuid().ToString("N"))
        let run () =
            try
                let payload=Path.Combine(directory,"payload")
                let output=Path.Combine(directory,"publish")
                Directory.CreateDirectory(payload) |> ignore
                for file in files |> List.filter (fun file -> file.RelativePath.EndsWith(".dll",StringComparison.OrdinalIgnoreCase)) do
                    File.WriteAllBytes(Path.Combine(payload,file.RelativePath),file.Content)
                let xml (value: string) = SecurityElement.Escape(value) |> nonNull
                let references=files |> List.filter (fun file -> file.RelativePath.EndsWith(".dll",StringComparison.OrdinalIgnoreCase) && file.RelativePath<>options.AssemblyName+".dll")
                               |> List.map (fun file -> $"<Reference Include=\"{xml (Path.GetFileNameWithoutExtension file.RelativePath |> nonNull)}\"><HintPath>payload/{xml file.RelativePath}</HintPath><Private>true</Private></Reference>") |> String.concat "\n"
                let aot=if executable.AheadOfTime then "true" else "false"
                let trim=if trim then "true" else "false"
                let bundle=if executable.AheadOfTime then "false" else "true"
                let project= $"""<Project Sdk="Microsoft.NET.Sdk">
      <PropertyGroup>
        <TargetFramework>net10.0</TargetFramework><OutputType>Exe</OutputType>
        <AssemblyName>{xml options.AssemblyName}</AssemblyName><RuntimeIdentifier>{xml executable.RuntimeIdentifier}</RuntimeIdentifier>
        <SelfContained>true</SelfContained><UseAppHost>true</UseAppHost>
        <PublishSingleFile>{bundle}</PublishSingleFile><IncludeNativeLibrariesForSelfExtract>true</IncludeNativeLibrariesForSelfExtract>
        <PublishAot>{aot}</PublishAot><PublishTrimmed>{trim}</PublishTrimmed>
        <!-- FSharp.Core requests removal of optional compiler metadata resources that
             may already be absent. IL2040 concerns only these resource removals;
             reflection, trimming and AOT compatibility warnings remain errors. -->
        <NoWarn>$(NoWarn);IL2040</NoWarn>
        <NativeOutputPath>$(MSBuildProjectDirectory)/aot-output/</NativeOutputPath>
        <DebugType>none</DebugType><DebugSymbols>false</DebugSymbols><EnableDefaultCompileItems>false</EnableDefaultCompileItems>
        <GenerateAssemblyInfo>false</GenerateAssemblyInfo><GenerateTargetFrameworkAttribute>false</GenerateTargetFrameworkAttribute>
        <ImplicitUsings>disable</ImplicitUsings><Nullable>enable</Nullable><TreatWarningsAsErrors>true</TreatWarningsAsErrors>
        <Deterministic>true</Deterministic><EnableCompressionInSingleFile>false</EnableCompressionInSingleFile>
      </PropertyGroup>
      <ItemGroup><Compile Include="Entry.cs"/>{references}</ItemGroup>
      <Target Name="UseFScriptAssembly" AfterTargets="CoreCompile">
        <Copy SourceFiles="$(MSBuildProjectDirectory)/payload/{xml options.AssemblyName}.dll" DestinationFiles="@(IntermediateAssembly)"/>
      </Target>
    </Project>
    """
                // The SDK needs an executable project to resolve runtime packs and apphost assets.
                // CoreCompile's placeholder is replaced before publishing; the final entry point,
                // methods and optimization are those of the original FScript assembly.
                File.WriteAllText(Path.Combine(directory,"Program.csproj"),project)
                File.WriteAllText(Path.Combine(directory,"Entry.cs"),"internal static class Entry { public static void Main(string[] args) { } }\n")
                let start=ProcessStartInfo("dotnet",WorkingDirectory=directory,RedirectStandardOutput=true,RedirectStandardError=true,UseShellExecute=false)
                for argument in ["publish";"Program.csproj";"--configuration";"Release";"--output";output;"--nologo";"--verbosity";"minimal";"-p:ImportDirectoryBuildProps=false";"-p:ImportDirectoryBuildTargets=false"] do start.ArgumentList.Add(argument)
                use proc=new Process(StartInfo=start)
                try
                    proc.Start() |> ignore
                    let stdout=proc.StandardOutput.ReadToEndAsync()
                    let stderr=proc.StandardError.ReadToEndAsync()
                    if not (proc.WaitForExit(300000)) then
                        proc.Kill(true)
                        proc.WaitForExit()
                        Error [diagnostic "FSN7103" "Executable publishing exceeded five minutes. Check SDK restore access and the target native toolchain."]
                    else
                        let log=stdout.GetAwaiter().GetResult()+stderr.GetAwaiter().GetResult()
                        if proc.ExitCode<>0 then
                            let log=if log.Length>16000 then log.Substring(log.Length-16000) else log
                            Error [diagnostic "FSN7103" $".NET executable publishing failed for {executable.RuntimeIdentifier}. A .NET 10 SDK is required; Native AOT also requires the target's native compiler and AOT-compatible dependencies.\n{log.Trim()}"]
                        else
                            let name=options.AssemblyName+(if executable.RuntimeIdentifier.StartsWith("win-",StringComparison.Ordinal) then ".exe" else "")
                            let path=Path.Combine(output,name)
                            if not (File.Exists path) then Error [diagnostic "FSN7103" $"The SDK did not produce the expected executable '{name}'."]
                            else
                                // Refuse to silently drop a native/content dependency that the SDK
                                // could not embed. Symbol files are optional development artifacts.
                                let extras=Directory.GetFiles(output,"*",SearchOption.AllDirectories) |> Array.filter (fun file ->
                                    let relative=Path.GetRelativePath(output,file).Replace('\\','/')
                                    file<>path && not (relative.StartsWith(name+".dSYM/",StringComparison.Ordinal) || file.EndsWith(".pdb",StringComparison.OrdinalIgnoreCase) || file.EndsWith(".dbg",StringComparison.OrdinalIgnoreCase)))
                                let extraNames=extras |> Array.map (fun path -> Path.GetRelativePath(output,path)) |> String.concat ", "
                                if extras.Length<>0 then Error [diagnostic "FSN7103" $"Publishing left unbundled dependencies: {extraNames}."]
                                else Ok [{RelativePath=name;Content=File.ReadAllBytes(path)}]
                with :? ComponentModel.Win32Exception as ex ->
                    Error [diagnostic "FSN7102" $"A .NET 10 SDK with dotnet on PATH is required for executable publishing: {ex.Message}"]
            with ex -> Error [diagnostic "FSN7100" $"Cannot publish .NET executable: {ex.Message}"]
        try run ()
        finally
            if Directory.Exists directory then Directory.Delete(directory,true)
