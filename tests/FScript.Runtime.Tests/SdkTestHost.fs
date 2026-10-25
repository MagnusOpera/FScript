namespace FScript.Testing

open FScript.Language
open FScript.Runtime

module SdkTestHost =
    let private context: ExternalCallContext =
        { Session = obj()
          IsActive = fun () -> true
          Apply = fun _ _ -> failwith "No callbacks in data conversion"
          SpawnTask = fun _ -> failwith "No tasks in data conversion"
          AwaitTask = fun _ -> failwith "No tasks in data conversion"
          SetExitCode = ignore
          CheckRuntime = ignore }
    let invokeExternal (fn: FScript.Extensibility.ExternalFunction) args =
        fn.Impl (FScript.Extensibility.ExternalCallContext((fun _ _ -> failwith "unused"), (fun _ -> failwith "unused"), (fun _ -> failwith "unused"), ignore)) args
    let toSdk native =
        let mutable captured = FScript.Extensibility.VUnit
        let fn: FScript.Extensibility.ExternalFunction =
            { Name = "Test.capture"; Arity = 1; Scheme = FScript.Extensibility.Forall([0], FScript.Extensibility.TFun(FScript.Extensibility.TVar 0, FScript.Extensibility.TUnit))
              Impl = fun _ args -> captured <- args.Head; FScript.Extensibility.VUnit }
        let registration = ExtensionBridge.toLanguageRegistration { FScript.Extensibility.HostInterop.empty with Functions = [fn] }
        registration.Functions.Head.Impl context [native] |> ignore
        captured
    let toNative sdk =
        let fn: FScript.Extensibility.ExternalFunction =
            { Name = "Test.value"; Arity = 0; Scheme = FScript.Extensibility.Forall([0], FScript.Extensibility.TVar 0); Impl = fun _ _ -> sdk }
        let registration = ExtensionBridge.toLanguageRegistration { FScript.Extensibility.HostInterop.empty with Functions = [fn] }
        registration.Functions.Head.Impl context []
    type Loaded = { Native: ScriptHost.LoadedScript; LastValue: FScript.Extensibility.ScriptValue }
    let private loaded native = { Native = native; LastValue = toSdk native.LastValue }
    let loadSourceWithRegistration registration source = ScriptHost.loadSourceWithRegistration (ExtensionBridge.toLanguageRegistration registration) source |> loaded
    let loadFileWithRegistration registration path = ScriptHost.loadFileWithRegistration (ExtensionBridge.toLanguageRegistration registration) path |> loaded
    let getValue loaded name = ScriptHost.getValue loaded.Native name |> toSdk
    let invoke loaded name args = ScriptHost.invoke loaded.Native name (List.map toNative args) |> toSdk

module SdkTestPretty =
    let valueToString value = value |> SdkTestHost.toNative |> Pretty.valueToString
