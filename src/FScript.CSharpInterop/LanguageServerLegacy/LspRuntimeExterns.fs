namespace FScript.LanguageServer

open FScript.Language
open FScript.CSharpInterop

module LspRuntimeExterns =
    let forSourcePath (sourcePath: string) : ExternalFunction list =
        InteropServices.runtimeExternsForSourcePath sourcePath
