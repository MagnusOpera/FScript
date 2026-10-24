namespace FScript.Compiler

open System
open System.IO
open System.Text
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp

type DotNetOutputKind = Library | ConsoleApplication
type DotNetOptions =
    { AssemblyName: string
      Namespace: string
      ClassName: string
      OutputKind: DotNetOutputKind
      References: string list }

type GeneratedBinary = { RelativePath: string; Content: byte array }

/// Produces PE assemblies from compiled LIR. Generated binaries contain no FScript interpreter.
module DotNetBackend =
    let private diagnostic code message =
        { Code=code; Stage=DotNetGeneration; Message=message; Span=None }

    let generate (options: DotNetOptions) (program: Lir.Program) =
        try
            if String.IsNullOrWhiteSpace options.AssemblyName || options.AssemblyName.IndexOfAny(Path.GetInvalidFileNameChars()) >= 0 || options.AssemblyName.Contains('/') || options.AssemblyName.Contains('\\') then
                Error [diagnostic "FSN7001" "AssemblyName must be a nonempty file name without path separators."]
            else
                let csharpOptions: CSharpOptions = { Namespace=options.Namespace; ClassName=options.ClassName; References=options.References }
                match CSharpBackend.generate csharpOptions program with
                | Error diagnostics -> Error diagnostics
                | Ok files ->
                    let trees = files |> List.filter (fun f -> f.RelativePath.EndsWith(".cs",StringComparison.Ordinal)) |> List.map (fun f ->
                        CSharpSyntaxTree.ParseText(f.Content, CSharpParseOptions(LanguageVersion.Latest), f.RelativePath, Encoding.UTF8))
                    let platformReferences =
                        match AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") with
                        | :? string as paths -> paths.Split(Path.PathSeparator) |> Array.toList
                        | _ -> [typeof<obj>.Assembly.Location; typeof<Console>.Assembly.Location]
                    let references =
                        platformReferences @ [typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location] @ options.References
                        |> List.distinct |> List.sort
                        |> List.map (fun path -> MetadataReference.CreateFromFile(Path.GetFullPath path) :> MetadataReference)
                    let kind = match options.OutputKind with Library -> OutputKind.DynamicallyLinkedLibrary | ConsoleApplication -> OutputKind.ConsoleApplication
                    let compilationOptions = CSharpCompilationOptions(kind, optimizationLevel=OptimizationLevel.Release, allowUnsafe=false, deterministic=true, nullableContextOptions=NullableContextOptions.Enable)
                    let compilation = CSharpCompilation.Create(options.AssemblyName, trees, references, compilationOptions)
                    use output = new MemoryStream()
                    let result = compilation.Emit(output)
                    let diagnostics = result.Diagnostics |> Seq.filter (fun d -> d.Severity=DiagnosticSeverity.Error || d.Severity=DiagnosticSeverity.Warning) |> Seq.map (fun d -> diagnostic "FSN7002" (d.ToString())) |> Seq.toList
                    if not result.Success || not diagnostics.IsEmpty then Error diagnostics
                    else
                        let binary = { RelativePath=options.AssemblyName+".dll"; Content=output.ToArray() }
                        let runtimeConfig =
                            "{\"runtimeOptions\":{\"tfm\":\"net10.0\",\"framework\":{\"name\":\"Microsoft.NETCore.App\",\"version\":\"10.0.0\"},\"rollForward\":\"LatestMinor\"}}\n"
                        let dependencies =
                            options.References |> List.map Path.GetFullPath |> List.distinct
                            |> List.filter (fun path -> Path.GetFileNameWithoutExtension path <> options.AssemblyName)
                            |> List.map (fun path -> { RelativePath=Path.GetFileName path |> nonNull; Content=File.ReadAllBytes path })
                        let files = binary :: dependencies
                        if options.OutputKind=ConsoleApplication then
                            Ok(files @ [{ RelativePath=options.AssemblyName+".runtimeconfig.json"; Content=Encoding.UTF8.GetBytes(runtimeConfig) }])
                        else Ok files
        with ex -> Error [diagnostic "FSN7000" ex.Message]
