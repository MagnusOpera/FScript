namespace FScript.Runtime

open System
open System.Runtime.CompilerServices
open FScript.Language

/// Translate stable extension contracts at the interpreter boundary.
module ExtensionBridge =
    type private ST = FScript.Extensibility.ScriptType
    type private SV = FScript.Extensibility.ScriptValue
    type private SK = FScript.Extensibility.MapKey
    type private SE = FScript.Extensibility.ExtensionException

    let rec typeToLanguage (value: ST) : Type =
        match value with
        | ST.TUnit -> TUnit
        | ST.TInt -> TInt
        | ST.TFloat -> TFloat
        | ST.TBool -> TBool
        | ST.TString -> TString
        | ST.TTypeToken -> TTypeToken
        | ST.TList item -> TList(typeToLanguage item)
        | ST.TTask item -> TTask(typeToLanguage item)
        | ST.TOption item -> TOption(typeToLanguage item)
        | ST.TTuple items -> TTuple(List.map typeToLanguage items)
        | ST.TRecord fields -> TRecord(Map.map (fun _ -> typeToLanguage) fields)
        | ST.TMap(key, value) -> TMap(typeToLanguage key, typeToLanguage value)
        | ST.TFun(arg, result) -> TFun(typeToLanguage arg, typeToLanguage result)
        | ST.TNamed name -> TNamed name
        | ST.TUnion(name, cases) -> TUnion(name, Map.map (fun _ -> Option.map typeToLanguage) cases)
        | ST.THostManaged(name, fields) -> THostManaged(name, Map.map (fun _ -> typeToLanguage) fields)
        | ST.TVar id -> TVar id
        | ST.TNumeric id -> TNumeric id

    let rec typeFromLanguage (value: Type) : ST =
        match value with
        | TUnit -> ST.TUnit
        | TInt -> ST.TInt
        | TFloat -> ST.TFloat
        | TBool -> ST.TBool
        | TString -> ST.TString
        | TTypeToken -> ST.TTypeToken
        | TList item -> ST.TList(typeFromLanguage item)
        | TTask item -> ST.TTask(typeFromLanguage item)
        | TOption item -> ST.TOption(typeFromLanguage item)
        | TTuple items -> ST.TTuple(List.map typeFromLanguage items)
        | TRecord fields -> ST.TRecord(Map.map (fun _ -> typeFromLanguage) fields)
        | TMap(key, value) -> ST.TMap(typeFromLanguage key, typeFromLanguage value)
        | TFun(arg, result) -> ST.TFun(typeFromLanguage arg, typeFromLanguage result)
        | TNamed name -> ST.TNamed name
        | TUnion(name, cases) -> ST.TUnion(name, Map.map (fun _ -> Option.map typeFromLanguage) cases)
        | THostManaged(name, fields) -> ST.THostManaged(name, Map.map (fun _ -> typeFromLanguage) fields)
        | TVar id -> ST.TVar id
        | TNumeric id -> ST.TNumeric id
        | TRecordShape _ -> raise (SE "Interpreter inference witnesses cannot be exposed through the extension SDK.")

    type private Callable(owner: obj, value: Value, active: unit -> bool) =
        member _.Owner = owner
        member _.Value = value
        member _.Active = active()
        interface FScript.Extensibility.ICallableHandle

    type private TaskValue(owner: obj, value: Value, active: unit -> bool) =
        member _.Owner = owner
        member _.Value = value
        member _.Active = active()
        interface FScript.Extensibility.ITaskHandle

    type private Frame =
        { Native: ExternalCallContext
          Encode: Value -> SV
          Decode: SV -> Value }
    let private frames = ConditionalWeakTable<FScript.Extensibility.ExternalCallContext, Frame>()

    let private keyToLanguage = function
        | SK.MKString v -> MKString v | SK.MKInt v -> MKInt v
        | SK.MKBool v -> MKBool v | SK.MKUnionCase(n, c) -> MKUnionCase(n, c)
    let private keyFromLanguage = function
        | MKString v -> SK.MKString v | MKInt v -> SK.MKInt v
        | MKBool v -> SK.MKBool v | MKUnionCase(n, c) -> SK.MKUnionCase(n, c)

    /// Expose host-supplied standard functions without exposing their implementation types.
    let toExtensionFunctions (functions: ExternalFunction list) : FScript.Extensibility.ExternalFunction list =
        functions |> List.map (fun fn ->
            let (Forall(vars, signature)) = fn.Scheme
            { Name = fn.Name; Arity = fn.Arity
              Scheme = FScript.Extensibility.Forall(vars, typeFromLanguage signature)
              Impl = fun context args ->
                  match frames.TryGetValue context with
                  | true, frame -> fn.Impl frame.Native (List.map frame.Decode args) |> frame.Encode
                  | _ -> raise (SE "Host-supplied functions require an active interpreter invocation.") })

    /// Adapt one SDK environment. Values cross only at external-call boundaries.
    let toLanguageRegistration (registration: FScript.Extensibility.ExtensionRegistration) : ExtensionRegistration =
        FScript.Extensibility.HostInterop.typeDefinitions registration [] |> ignore
        let functions = registration.Functions |> List.map (fun fn ->
            let (FScript.Extensibility.Forall(vars, signature)) = fn.Scheme
            { Name = fn.Name; Arity = fn.Arity; Scheme = Forall(vars, typeToLanguage signature)
              Impl = fun native arguments ->
                  let invocationGate = obj()
                  let mutable active = true
                  let check () =
                      let invocationActive = lock invocationGate (fun () -> active)
                      if not invocationActive || not (native.IsActive()) then raise (SE "The extension invocation has expired.")
                      native.CheckRuntime()
                  let rec encode value =
                      match value with
                      | VUnit -> SV.VUnit
                      | VInt v -> SV.VInt v
                      | VFloat v -> SV.VFloat v
                      | VBool v -> SV.VBool v
                      | VString v -> SV.VString v
                      | VList vs -> SV.VList(List.map encode vs)
                      | VTuple vs -> SV.VTuple(List.map encode vs)
                      | VRecord vs -> SV.VRecord(Map.map (fun _ -> encode) vs)
                      | VMap vs -> SV.VMap(vs |> Map.toList |> List.map (fun (k,v) -> keyFromLanguage k, encode v) |> Map.ofList)
                      | VOption v -> SV.VOption(Option.map encode v)
                      | VUnionCase(n,c,v) -> SV.VUnionCase(n,c,Option.map encode v)
                      | VTypeToken t -> SV.VTypeToken(typeFromLanguage t)
                      | VHostManaged host ->
                          SV.VHostManaged { Descriptor = { Name = host.Descriptor.Name; HostManaged = host.Descriptor.HostManaged; Fields = Map.map (fun _ -> typeFromLanguage) host.Descriptor.Fields }
                                            Fields = Map.map (fun _ -> encode) host.Fields; Payload = host.Payload }
                      | VClosure _ | VExternal _ | VUnionCtor _ -> SV.VCallable(Callable(native.Session, value, native.IsActive))
                      | VTask _ -> SV.VTask(TaskValue(native.Session, value, native.IsActive))
                  and decode value =
                      match value with
                      | SV.VUnit -> VUnit
                      | SV.VInt v -> VInt v
                      | SV.VFloat v -> VFloat v
                      | SV.VBool v -> VBool v
                      | SV.VString v -> VString v
                      | SV.VList vs -> VList(List.map decode vs)
                      | SV.VTuple vs -> VTuple(List.map decode vs)
                      | SV.VRecord vs -> VRecord(Map.map (fun _ -> decode) vs)
                      | SV.VMap vs -> VMap(vs |> Map.toList |> List.map (fun (k,v) -> keyToLanguage k, decode v) |> Map.ofList)
                      | SV.VOption v -> VOption(Option.map decode v)
                      | SV.VUnionCase(n,c,v) -> VUnionCase(n,c,Option.map decode v)
                      | SV.VTypeToken t -> VTypeToken(typeToLanguage t)
                      | SV.VHostManaged host ->
                          VHostManaged { Descriptor = { Name = host.Descriptor.Name; HostManaged = host.Descriptor.HostManaged; Fields = Map.map (fun _ -> typeToLanguage) host.Descriptor.Fields }
                                         Fields = Map.map (fun _ -> decode) host.Fields; Payload = host.Payload }
                      | SV.VCallable (:? Callable as handle) when obj.ReferenceEquals(native.Session, handle.Owner) && handle.Active -> handle.Value
                      | SV.VTask (:? TaskValue as handle) when obj.ReferenceEquals(native.Session, handle.Owner) && handle.Active -> handle.Value
                      | SV.VCallable _ | SV.VTask _ -> raise (SE "Foreign or expired extension handle.")
                  let callback action =
                      check()
                      try action()
                      with EvalException error -> raise (SE(error.Message, EvalException error))
                  let context = FScript.Extensibility.ExternalCallContext(
                      (fun f x -> callback (fun () -> native.Apply (decode f) (decode x) |> encode)),
                      (fun f -> callback (fun () -> native.SpawnTask (decode f) |> encode)),
                      (fun t -> callback (fun () -> native.AwaitTask (decode t) |> encode)), check)
                  frames.Add(context, { Native = native; Encode = encode; Decode = decode })
                  try
                      try fn.Impl context (List.map encode arguments) |> decode
                      with :? SE as error ->
                          match error.InnerException with
                          | :? EvalException as original -> raise original
                          | _ -> raise (HostCommon.evalError error.Message)
                  finally
                      lock invocationGate (fun () -> active <- false)
                      frames.Remove context |> ignore })
        { Types = registration.Types |> List.map (fun d -> { Name = d.Name; HostManaged = d.HostManaged; Fields = Map.map (fun _ -> typeToLanguage) d.Fields })
          Unions = registration.Unions |> List.map (fun d -> { Name = d.Name; Cases = Map.map (fun _ -> Option.map typeToLanguage) d.Cases })
          Functions = functions }
