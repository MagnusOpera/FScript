module FScript.LanguageServer.Program

[<EntryPoint>]
let main _ =
    LspServer.run ()
    0
