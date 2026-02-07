namespace FScript.Runtime.Tests

open System
open System.IO
open FScript.Language

module HostTestHelpers =
    let invoke (ext: ExternalFunction) (args: Value list) =
        ext.Impl args

    let withTempRoot (name: string) (run: string -> unit) =
        let root = Path.Combine(Path.GetTempPath(), name, Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            run root
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)
