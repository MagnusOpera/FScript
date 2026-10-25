namespace FScript.TaskSharing.Sample

open FScript.Extensibility

/// Deliberately unsafe: immutable script bindings do not protect this field.
[<FScriptHostType("Counter")>]
type Counter() =
    let mutable value = 0L
    [<FScriptVisible>]
    member _.Name = "unsynchronized counter"
    member _.Next() =
        value <- value + 1L
        value

/// Effectful and mutable, but synchronized for concurrent use.
[<FScriptHostType("SafeCounter")>]
type SafeCounter() =
    let gate = obj()
    let mutable value = 0L
    [<FScriptVisible>]
    member _.Name = "synchronized counter"
    member _.Next() = lock gate (fun () -> value <- value + 1L; value)

type CounterProvider =
    static let globalCounter = Counter()

    // Thread safety is independent of purity: these operations change state.
    static member FScriptPureFunctions() : System.Collections.Generic.IEnumerable<string> = Seq.empty
    static member FScriptPureTypes() : System.Collections.Generic.IEnumerable<string> = Seq.empty
    static member FScriptThreadSafeTypes() : System.Collections.Generic.IEnumerable<string> = ["SafeCounter"]
    static member FScriptThreadSafeFunctions() : System.Collections.Generic.IEnumerable<string> =
        ["Host.createCounter"; "Host.createSafeCounter"; "Host.nextSafe"]

    [<FScriptExternProvider>]
    static member Provide() : ExtensionRegistration =
        let registration =
            { HostInterop.empty with
                Types = [HostTypes.describe<Counter>(); HostTypes.describe<SafeCounter>()] }
        { registration with
            Functions =
                [ HostFunctions.create registration "Host.createCounter" (fun () -> Counter())
                  HostFunctions.create registration "Host.next" (fun (counter: Counter) -> counter.Next())
                  HostFunctions.create registration "Host.createSafeCounter" (fun () -> SafeCounter())
                  HostFunctions.create registration "Host.nextSafe" (fun (counter: SafeCounter) -> counter.Next())
                  HostFunctions.create registration "Host.nextGlobal" (fun () -> globalCounter.Next()) ] }
