namespace FScript.Runtime.Tests

open System
open System.IO
open FScript.Language

module HostTestHelpers =
    let private noApply _ _ =
        failwith "Function application callback is not available in this test helper"

    let private noSpawnTask _ =
        failwith "Task spawning callback is not available in this test helper"

    let private noAwaitTask _ =
        failwith "Task await callback is not available in this test helper"

    let invoke (ext: ExternalFunction) (args: Value list) =
        ext.Impl
            { Apply = noApply
              SpawnTask = noSpawnTask
              AwaitTask = noAwaitTask
              CheckRuntime = fun () -> () }
            args

    let withTempRoot (name: string) (run: string -> unit) =
        let root = Path.Combine(Path.GetTempPath(), name, Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            run root
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)
