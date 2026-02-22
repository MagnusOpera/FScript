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
        let ctx = { HostContext.RootDirectory = resolveRootDirectory sourcePath; DeniedPathGlobs = [] }
        Registry.all ctx

    let private lspEnvironmentPreludeProgram : Program =
        FScript.parseWithSourceName
            (Some "<lsp-environment>")
            """
let asEnvironment (value: Environment) = value
let Env = asEnvironment { ScriptName = None; Arguments = [] }
"""

    let private withLspEnvironmentPrelude (program: Program) : Program =
        lspEnvironmentPreludeProgram @ program

    let parseProgramFromSourceWithIncludes (sourcePath: string) (sourceText: string) : Program =
        let rootDirectory = resolveRootDirectory sourcePath
        IncludeResolver.parseProgramFromSourceWithIncludes rootDirectory sourcePath sourceText

    let inferProgramWithExterns (externs: ExternalFunction list) (program: Program) : TypeInfer.TypedProgram =
        TypeInfer.inferProgramWithExterns externs (withLspEnvironmentPrelude program)

    let inferProgramWithExternsAndLocalVariableTypes (externs: ExternalFunction list) (program: Program) : TypeInfer.TypedProgram * TypeInfer.LocalVariableTypeInfo list =
        TypeInfer.inferProgramWithExternsAndLocalVariableTypes externs (withLspEnvironmentPrelude program)
