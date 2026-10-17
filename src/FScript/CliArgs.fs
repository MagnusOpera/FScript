module CliArgs

open Argu

type CliArgs =
    | [<MainCommand; Last>] Script of path: string
    | [<AltCommandLine("-r")>] Root of path: string
    | [<AltCommandLine("-c")>] Config of path: string
    | No_Default_Externs
    | Extern_Assembly of path: string
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Script _ -> "Path to the FScript source file (.fss)."
            | Root _ -> "Override sandbox root directory for filesystem externs."
            | Config _ -> "Load an FScript environment configuration (fscript.toml)."
            | No_Default_Externs -> "Disable default runtime externs from FScript.Runtime.Registry."
            | Extern_Assembly _ -> "Load external-function providers from an assembly path (repeatable)."
