namespace FScript.Cli.Tests.Fixtures.Valid

open FScript.Extensibility

type ValidExternProvider =
    static member AnswerNative() = 42L
    static member FScriptCompiledFunctions() : System.Collections.Generic.IEnumerable<System.Collections.Generic.KeyValuePair<string,System.Reflection.MethodInfo>> =
        [|System.Collections.Generic.KeyValuePair("Ext.answer",typeof<ValidExternProvider>.GetMethod("AnswerNative") |> nonNull)|]
    [<FScriptExternProvider>]
    static member Provide() : ExternalFunction list =
        [ { Name = "Ext.answer"
            Scheme = Forall([], TInt)
            Arity = 0
            Impl = (fun _ _ -> VInt 42L) } ]

type ValidHostContextProvider =
    [<FScriptExternProvider>]
    static member Provide(ctx: HostContext) : ExternalFunction list =
        let marker = System.IO.Path.Combine(ctx.RootDirectory, "provider-registration.requested")
        if System.IO.File.Exists(marker) then
            System.IO.File.WriteAllText(System.IO.Path.Combine(ctx.RootDirectory, "provider-registration.executed"), "registered")
            System.Console.WriteLine("fixture provider registration")
        [ { Name = "Ext.rootDirectory"
            Scheme = Forall([], TString)
            Arity = 0
            Impl = (fun _ _ -> VString ctx.RootDirectory) } ]


[<FScriptHostType("Node")>]
type ProviderNode =
    { [<FScriptVisible>] Value: int64
      Context: HostContext }

[<FScriptHostType("Point", ScriptConstructible = true)>]
type ProviderPoint = { [<FScriptVisible>] X: int64 }

[<FScriptHostType("Protocol.Mode", ScriptConstructible = true)>]
type ProviderMode = Disabled | Enabled of int64

type HostManagedProvider =
    [<FScriptExternProvider>]
    static member Provide(ctx: HostContext) : ExtensionRegistration =
        let descriptor = HostTypes.describe<ProviderNode>()
        let registration = { HostInterop.empty with Types = [descriptor; HostTypes.describe<ProviderPoint>()]; Unions = [HostTypes.describeUnion<ProviderMode>()] }
        { registration with
            Functions =
                [ HostFunctions.create registration "Ext.mode" (fun (mode: ProviderMode) -> mode)
                  HostFunctions.create registration "Ext.node" (fun (n: int64) -> { Value = n; Context = ctx })
                  HostFunctions.create registration "Ext.acceptNode" (fun (node: ProviderNode) ->
                      if node.Context = ctx then node.Value
                      else failwith "Host state was lost")
                  HostFunctions.create registration "Ext.acceptPoint" (fun (point: ProviderPoint) -> point.X) ] }

[<FScriptHostType("NativeCounter")>]
type NativeCounter(value: int64) =
    [<FScriptVisible>]
    member _.Value=value

type CompiledProvider =
    static member Create(value: int64) = NativeCounter(value)
    static member Read(value: NativeCounter) = value.Value
    static member FScriptPureFunctions() : System.Collections.Generic.IEnumerable<string> =
        [| "Ext.createCounter"; "Ext.readCounter" |]
    static member FScriptPureTypes() : System.Collections.Generic.IEnumerable<string> =
        [| "NativeCounter" |]
    // This method must never become an AOT root merely because its provider is loaded.
    static member Unused() = System.Reflection.Assembly.Load("Not.A.Runtime.Dependency") |> ignore; 0L
    [<FScriptExternProvider>]
    static member Provide() : ExtensionRegistration =
        let registration={HostInterop.empty with Types=[HostTypes.describe<NativeCounter>()]}
        {registration with Functions=[HostFunctions.create registration "Ext.createCounter" CompiledProvider.Create
                                      HostFunctions.create registration "Ext.readCounter" CompiledProvider.Read
                                      HostFunctions.create registration "Ext.unusedNative" (fun () -> CompiledProvider.Unused())]}
    static member FScriptCompiledTypes() : System.Collections.Generic.IEnumerable<System.Collections.Generic.KeyValuePair<string,System.Type>> =
        [|System.Collections.Generic.KeyValuePair("NativeCounter",typeof<NativeCounter>)|]
    static member FScriptCompiledFunctions() : System.Collections.Generic.IEnumerable<System.Collections.Generic.KeyValuePair<string,System.Reflection.MethodInfo>> =
        [| for name,methodName in ["Ext.createCounter","Create";"Ext.readCounter","Read";"Ext.unusedNative","Unused"] do
               yield System.Collections.Generic.KeyValuePair(name,typeof<CompiledProvider>.GetMethod(methodName) |> nonNull) |]
