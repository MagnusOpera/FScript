namespace FScript.LanguageServer

open System
open System.IO
open FScript.Language
open FScript.Runtime

module LspRuntimeExterns =
    let private resolveRootDirectory (sourcePath: string) =
        try
            match Path.GetDirectoryName(sourcePath) with
            | null
            | "" -> Directory.GetCurrentDirectory()
            | dir -> dir
        with _ ->
            Directory.GetCurrentDirectory()

    let forSourcePath (sourcePath: string) : ExternalFunction list =
        let ctx = { HostContext.RootDirectory = resolveRootDirectory sourcePath }
        Registry.all ctx
