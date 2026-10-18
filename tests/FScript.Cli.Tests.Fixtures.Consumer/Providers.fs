namespace FScript.Cli.Tests.Fixtures.Consumer

open FScript.Extensibility
open FScript.Cli.Tests.Fixtures.Valid

/// Models a plotting extension that consumes the numerical extension's opaque CLR type.
type ConsumerExternProvider =
    [<FScriptExternProvider>]
    static member Provide() : ExtensionRegistration =
        let validationTypes = { HostInterop.empty with Types = [HostTypes.describe<ProviderNode>()] }
        let consume = HostFunctions.create validationTypes "Consumer.readNode" (fun (node: ProviderNode) -> node.Value)
        // The owner registers Node; a consumer declares only its own functions.
        { HostInterop.empty with Functions = [consume] }
