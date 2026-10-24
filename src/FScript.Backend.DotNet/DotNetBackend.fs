namespace FScript.Compiler

open System
open System.IO
open System.Text
open System.Reflection.PortableExecutable
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp

/// Produces PE assemblies from compiled LIR. Generated binaries contain no FScript interpreter.
module DotNetBackend =
    let private diagnostic code message =
        { Code=code; Stage=DotNetGeneration; Message=message; Span=None }

    let generate (options: DotNetOptions) (program: Lir.Program) =
        try
            let referencesExist = options.References |> List.forall File.Exists
            if not referencesExist then
                invalidArg "References" "One or more reference assemblies do not exist."
            let options={options with References=ClrBindings.referenceClosure options.References}
            if String.IsNullOrWhiteSpace options.AssemblyName || options.AssemblyName.IndexOfAny(Path.GetInvalidFileNameChars()) >= 0 || options.AssemblyName.Contains('/') || options.AssemblyName.Contains('\\') then
                Error [diagnostic "FSN7001" "AssemblyName must be a nonempty file name without path separators."]
            else
                let csharpOptions: CSharpOptions = { Namespace=options.Namespace; ClassName=options.ClassName; References=options.References }
                match CSharpBackend.generateForAssembly DotNetOptimization.optimize csharpOptions program with
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
                    let compilation =
                        CSharpCompilation.Create(options.AssemblyName, trees, references, compilationOptions)
                        |> DotNetSourceOptimization.optimize options.ClassName (options.OutputKind=Library)
                    use output = new MemoryStream()
                    let result = compilation.Emit(output)
                    let diagnostics = result.Diagnostics |> Seq.filter (fun d -> d.Severity=DiagnosticSeverity.Error || d.Severity=DiagnosticSeverity.Warning) |> Seq.map (fun d -> diagnostic "FSN7002" (d.ToString())) |> Seq.toList
                    if not result.Success || not diagnostics.IsEmpty then Error diagnostics
                    else
                        let binary = { RelativePath=options.AssemblyName+".dll"; Content=output.ToArray() }
                        let runtimeConfig =
                            "{\"runtimeOptions\":{\"tfm\":\"net10.0\",\"framework\":{\"name\":\"Microsoft.NETCore.App\",\"version\":\"10.0.0\"},\"rollForward\":\"LatestMinor\"}}\n"
                        let dependencies =
                            ClrBindings.referencedDependencies binary.Content options.References
                            |> List.filter (fun path -> Path.GetFileNameWithoutExtension path <> options.AssemblyName)
                            |> List.map (fun path ->
                                let name=Reflection.AssemblyName.GetAssemblyName(path).Name |> nonNull
                                { RelativePath=name+".dll"; Content=File.ReadAllBytes path })
                        let files = binary :: dependencies
                        if options.OutputKind=ConsoleApplication then
                            Ok(files @ [{ RelativePath=options.AssemblyName+".runtimeconfig.json"; Content=Encoding.UTF8.GetBytes(runtimeConfig) }])
                        else Ok files
        with ex -> Error [diagnostic "FSN7000" ex.Message]

    /// Produce one self-contained executable for an explicit .NET runtime identifier.
    let generateExecutable executable options program =
        DotNetPackaging.validate executable options.OutputKind
        |> Result.bind (fun () -> generate options program)
        |> Result.bind (fun files ->
            let assembly=files |> List.find (fun file -> file.RelativePath=options.AssemblyName+".dll")
            use stream=new MemoryStream(assembly.Content,false)
            use pe=new PEReader(stream)
            let metadata=System.Reflection.Metadata.PEReaderExtensions.GetMetadataReader(pe)
            let legacyBridge=metadata.TypeDefinitions |> Seq.exists (fun handle ->
                let typ=metadata.GetTypeDefinition(handle)
                metadata.GetString(typ.Name)="SdkInterop" && metadata.GetString(typ.Namespace)=options.Namespace)
            if executable.AheadOfTime && legacyBridge then
                Error [diagnostic "FSN7105" "Native AOT requires static compiled FFI bindings. Add FScriptCompiledFunctions/FScriptCompiledTypes companions or bind exact CLR methods/types with ClrBindings; provider-factory SDK dispatch cannot be safely pruned. Managed single-file output still supports legacy providers."]
            else DotNetPackaging.publish executable (not legacyBridge) options files)
