namespace FScript.Language

/// Canonical task operations, recognized by identity by the evaluator.
module TaskFunctions =
    let private fn arguments result = List.foldBack (fun argument result -> TFun(argument,result)) arguments result
    let private a,b = TVar 0,TVar 1
    let signatures : (string * Scheme * string list) list =
        [ "Task.spawn", Forall([0],fn [fn [TUnit] a] (TTask a)), ["thunk"]
          "Task.await", Forall([0],fn [TTask a] a), ["task"]
          "Task.delay", Forall([],fn [TInt] (TTask TUnit)), ["milliseconds"]
          "Task.fromResult", Forall([0],fn [a] (TTask a)), ["value"]
          "Task.continueWith", Forall([0;1],fn [fn [a] b; TTask a] (TTask b)), ["continuation";"task"]
          "Task.whenAll", Forall([0;1],fn [fn [TList a] b; TList(TTask a)] (TTask b)), ["continuation";"tasks"]
          "Task.whenAny", Forall([0;1],fn [fn [a] b; TList(TTask a)] (TTask b)), ["continuation";"tasks"]
          "Task.awaitAll", Forall([0],fn [TList(TTask a)] (TList a)), ["tasks"] ]
    let private scheme name = signatures |> List.find (fun (key,_,_) -> key=name) |> fun (_,scheme,_) -> scheme
    let spawn : ExternalFunction =
        { Name = "Task.spawn"; Scheme = scheme "Task.spawn"; Arity = 1
          Impl = fun context args -> match args with [thunk] -> context.SpawnTask thunk | _ -> invalidArg "args" "Task.spawn expects a thunk." }
    let awaitTask : ExternalFunction =
        { Name = "Task.await"; Scheme = scheme "Task.await"; Arity = 1
          Impl = fun context args -> match args with [value] -> context.AwaitTask value | _ -> invalidArg "args" "Task.await expects a task." }

    // SDK providers can forward a default descriptor via Impl. Re-enter the owning
    // evaluator through Apply so its scheduler, debugger, and lifetime rules still apply.
    let private operation name =
        let target = ref Unchecked.defaultof<ExternalFunction>
        let _,signature,parameters = signatures |> List.find (fun (key,_,_) -> key=name)
        let descriptor =
            { Name=name; Scheme=signature; Arity=parameters.Length
              Impl=fun context args -> List.fold context.Apply (VExternal(target.Value,[])) args }
        target.Value <- descriptor
        descriptor
    let delay = operation "Task.delay"
    let fromResult = operation "Task.fromResult"
    let continueWith = operation "Task.continueWith"
    let whenAll = operation "Task.whenAll"
    let whenAny = operation "Task.whenAny"
    let awaitAll = operation "Task.awaitAll"
    let all = [spawn;awaitTask;delay;fromResult;continueWith;whenAll;whenAny;awaitAll]
    let internal isCanonical descriptor = all |> List.exists (fun candidate -> obj.ReferenceEquals(candidate,descriptor))
