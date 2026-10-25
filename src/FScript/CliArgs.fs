module CliArgs

open Argu

type CliArgs =
    | [<MainCommand; Last>] Script of path: string
    | [<AltCommandLine("-r")>] Root of path: string
    | [<AltCommandLine("-c")>] Config of path: string
    | Debug_Adapter
    | Debug_Capabilities
    | No_Result
    | No_Config
    | No_Default_Externs
    | Extern_Assembly of path: string
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Script _ -> "Path to the FScript source file (.fss)."
            | Root _ -> "Override sandbox root directory for filesystem externs."
            | Config _ -> "Override automatic fscript.toml selection with an explicit configuration."
            | Debug_Adapter -> "Run the Debug Adapter Protocol service (used by editor integrations)."
            | Debug_Capabilities -> "Report the CLI debugger protocol capabilities as JSON."
            | No_Result -> "Do not display the final program value; explicit output and evaluation are preserved."
            | No_Config -> "Disable automatic fscript.toml loading (script directory, or cwd for stdin/REPL)."
            | No_Default_Externs -> "Disable default runtime externs from FScript.Runtime.Registry."
            | Extern_Assembly _ -> "Load external-function providers from an assembly path (repeatable)."

[<RequireQualifiedAccess>]
type AnalyzeArgs =
    | [<MainCommand>] Script of path: string
    | [<AltCommandLine("-r")>] Root of path: string
    | [<AltCommandLine("-c")>] Config of path: string
    | No_Config
    | No_Default_Externs
    | Extern_Assembly of path: string
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | AnalyzeArgs.Script _ -> "Analyze a script for potential FFI sharing across tasks without executing it."
            | AnalyzeArgs.Root _ -> "Override the sandbox root used to resolve imports and externs."
            | AnalyzeArgs.Config _ -> "Select an explicit fscript.toml environment."
            | AnalyzeArgs.No_Config -> "Disable automatic fscript.toml loading."
            | AnalyzeArgs.No_Default_Externs -> "Disable default runtime externs."
            | AnalyzeArgs.Extern_Assembly _ -> "Load an external-function provider assembly (repeatable)."
