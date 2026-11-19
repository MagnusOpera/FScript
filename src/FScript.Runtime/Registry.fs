namespace FScript.Runtime

open FScript.Language

module Registry =
    let internal mailbox = StandardLibrary.mailbox

    /// Default providers that also supply typed compiled linkage.
    let compiledProviders () =
        [mailbox.Functions |> List.map _.Name, typeof<CompiledMailbox>.GetMethod("Start") |> nonNull
         StandardLibrary.reference.Functions |> List.map _.Name, typeof<CompiledRef>.GetMethod("Create") |> nonNull
         ["Process.defaults";"Process.exec"], typeof<CompiledProcess>.GetMethod("Exec") |> nonNull
         ["FScript.defaults";"FScript.exec"], typeof<CompiledFScript>.GetMethod("Exec") |> nonNull]

    /// Create an interpreter registration with exactly the host's grants.
    let registration (context: HostContext) =
        FScriptLibrary.registration context (FileSystemPolicy.create context) ScriptRunner.initialScope

    let all context = (registration context).Functions
