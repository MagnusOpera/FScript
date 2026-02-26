open FScript.TypeProvider

type Script = FScriptScriptProvider<ScriptPath="script.fss">

[<EntryPoint>]
let main _ =
    let result = Script.add(1L, 2L)
    if result <> 3L then
        failwith $"Expected 3 but got {result}"
    0
