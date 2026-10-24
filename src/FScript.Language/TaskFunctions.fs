namespace FScript.Language

/// Canonical task operations, recognized by identity by the interpreter.
module TaskFunctions =
    let spawn : ExternalFunction =
        { Name = "Task.spawn"; Scheme = Forall([0], TFun(TFun(TUnit, TVar 0), TTask(TVar 0))); Arity = 1
          Impl = fun context args -> match args with [thunk] -> context.SpawnTask thunk | _ -> failwith "Task.spawn expects (thunk)" }
    let awaitTask : ExternalFunction =
        { Name = "Task.await"; Scheme = Forall([0], TFun(TTask(TVar 0), TVar 0)); Arity = 1
          Impl = fun context args -> match args with [value] -> context.AwaitTask value | _ -> failwith "Task.await expects (task)" }
