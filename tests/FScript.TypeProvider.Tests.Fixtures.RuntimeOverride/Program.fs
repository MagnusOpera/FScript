open System
open System.IO
open FScript.TypeProvider

type Script = FScriptScriptProvider<ScriptPath="script.fss">

[<EntryPoint>]
let main _ =
    let root = Directory.GetCurrentDirectory()
    let entryPath = Path.Combine(root, "virtual-main.fss")

    Script.SetRuntimeResolver(fun () ->
        Some
            { RootDirectory = root
              EntryFile = entryPath
              EntrySource = "[<export>] let add (x: int) (y: int) = x + y + 10"
              ResolveImportedSource = None })

    let updated = Script.add(1L, 2L)
    if updated <> 13L then
        failwith $"Expected updated add result 13 but got {updated}"

    Script.SetRuntimeResolver(fun () ->
        Some
            { RootDirectory = root
              EntryFile = entryPath
              EntrySource = "[<export>] let add (x: int) = x + 1"
              ResolveImportedSource = None })

    let mismatchRejected =
        try
            Script.add(1L, 2L) |> ignore
            false
        with
        | :? InvalidOperationException ->
            true

    Script.ClearRuntimeResolver()

    if not mismatchRejected then
        failwith "Expected strict signature mismatch rejection."

    0
