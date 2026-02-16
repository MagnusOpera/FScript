namespace FScript.CSharpInterop

open System
open System.IO
open FScript.Language
open FScript.Runtime

module InteropServices =
    let private resolveRootDirectory (sourcePath: string) =
        try
            match Path.GetDirectoryName(sourcePath) with
            | null
            | "" -> Directory.GetCurrentDirectory()
            | dir -> dir
        with _ ->
            Directory.GetCurrentDirectory()

    let runtimeExternsForSourcePath (sourcePath: string) : ExternalFunction list =
        let ctx = { HostContext.RootDirectory = resolveRootDirectory sourcePath }
        Registry.all ctx

    let parseProgramFromSourceWithIncludes (sourcePath: string) (sourceText: string) : Program =
        let rootDirectory = resolveRootDirectory sourcePath
        IncludeResolver.parseProgramFromSourceWithIncludes rootDirectory sourcePath sourceText

    let inferProgramWithExterns (externs: ExternalFunction list) (program: Program) : TypeInfer.TypedProgram =
        TypeInfer.inferProgramWithExterns externs program

    let inferProgramWithExternsAndLocalVariableTypes (externs: ExternalFunction list) (program: Program) : TypeInfer.TypedProgram * TypeInfer.LocalVariableTypeInfo list =
        TypeInfer.inferProgramWithExternsAndLocalVariableTypes externs program

    let inferStdlibWithExternsRaw (externs: ExternalFunction list) : TypeInfer.TypedProgram =
        TypeInfer.inferProgramWithExternsRaw externs (Stdlib.loadProgram())

    let stdlibProgram () : Program =
        Stdlib.loadProgram()

    let tryLoadStdlibSourceText (uri: string) : string option =
        try
            let parsed = Uri(uri)
            if not (String.Equals(parsed.Scheme, "fscript-stdlib", StringComparison.OrdinalIgnoreCase)) then
                None
            else
                let fileName = parsed.AbsolutePath.TrimStart('/')
                let resourceName =
                    match fileName with
                    | "Option.fss" -> Some "FScript.Language.Stdlib.Option.fss"
                    | "List.fss" -> Some "FScript.Language.Stdlib.List.fss"
                    | "Map.fss" -> Some "FScript.Language.Stdlib.Map.fss"
                    | "Environment.fss" -> Some "FScript.Language.Stdlib.Environment.fss"
                    | _ -> None

                match resourceName with
                | None -> None
                | Some name ->
                    let assembly = typeof<Span>.Assembly
                    match assembly.GetManifestResourceStream(name) with
                    | null -> None
                    | stream ->
                        use stream = stream
                        use reader = new StreamReader(stream)
                        Some(reader.ReadToEnd())
        with _ ->
            None
