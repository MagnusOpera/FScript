module InvalidFixture

open FScript.TypeProvider

type BadScript = FScriptScriptProvider<ScriptPath="script-invalid.fss">

let value = 1
