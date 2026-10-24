namespace FScript.Language

#if !FABLE_COMPILER
open System.Threading.Tasks
open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("FScript.Runtime")>]
do ()

/// Low-level evaluator observer. Public inspection is provided by Runtime.DebugSession.
type internal IDebugObserver =
    abstract Scheduler: TaskScheduler
    abstract TaskStarted: parent: int option -> int
    abstract TaskExited: taskId: int * error: exn option -> unit
    abstract EnterFrame: taskId: int * name: string * span: Span * parameters: Env * captures: Env -> unit
    abstract LeaveFrame: taskId: int -> unit
    abstract Expression: taskId: int * expression: Expr * environment: Env * phase: string -> Task
    abstract Awaiting: taskId: int * awaitedTaskId: int option * waiting: bool -> unit
    abstract RecordFault: taskId: int * error: exn -> unit
    abstract Fault: taskId: int * error: exn -> Task
    abstract CheckCancellation: unit -> unit
    abstract InvokeHost: taskId: int * invocation: (unit -> Value) -> Task<Value>
#endif
