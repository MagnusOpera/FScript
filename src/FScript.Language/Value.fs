namespace FScript.Language

#if FABLE_COMPILER
#else
open System.Threading.Tasks
#endif

type MapKey =
    | MKString of string
    | MKInt of int64

type Value =
    | VUnit
    | VInt of int64
    | VFloat of float
    | VBool of bool
    | VString of string
    | VList of Value list
    | VTuple of Value list
    | VRecord of Map<string, Value>
    | VMap of Map<MapKey, Value>
    | VOption of Value option
    | VUnionCase of string * string * Value option
    | VUnionCtor of string * string
    | VTypeToken of Type
    | VTask of TaskHandle
    | VClosure of string * Expr * Env ref
    | VExternal of ExternalFunction * Value list

and TaskOutcome =
    | TaskSucceeded of Value
    | TaskFailed of EvalError

and TaskHandle =
#if FABLE_COMPILER
    { mutable Awaited: bool }
#else
    { Worker: Task<TaskOutcome>
      mutable Awaited: bool }
#endif

and ExternalCallContext =
    { Apply: Value -> Value -> Value
      SpawnTask: Value -> Value
      AwaitTask: Value -> Value
      CheckRuntime: unit -> unit }

and ExternalFunction =
    { Name: string
      Scheme: Scheme
      Impl: ExternalCallContext -> Value list -> Value
      Arity: int }

and Env = Map<string, Value>
