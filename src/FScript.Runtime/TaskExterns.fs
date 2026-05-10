namespace FScript.Runtime

open FScript.Language

module TaskExterns =
    let spawn : ExternalFunction =
        { Name = "Task.spawn"
          Scheme = Forall([ 0 ], TFun(TFun(TUnit, TVar 0), TTask(TVar 0)))
          Arity = 1
          Impl =
            fun ctx -> function
                | [ thunk ] -> ctx.SpawnTask thunk
                | _ -> raise (HostCommon.evalError "Task.spawn expects (thunk)") }

    let await_task : ExternalFunction =
        { Name = "Task.await"
          Scheme = Forall([ 0 ], TFun(TTask(TVar 0), TVar 0))
          Arity = 1
          Impl =
            fun ctx -> function
                | [ VTask _ as task ] -> ctx.AwaitTask task
                | _ -> raise (HostCommon.evalError "Task.await expects (task)") }
