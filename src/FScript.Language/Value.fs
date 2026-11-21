namespace FScript.Language

#if FABLE_COMPILER
#else
open System.Threading.Tasks
#endif

/// Host-granted capabilities. An empty policy grants no effects.
[<System.Flags>]
type HostPermissions =
    | None = 0
    | Interactive = 1
    | Threading = 2
    | IO = 4
    | Native = 8
    | All = 15

type MapKey =
    | MKString of string
    | MKInt of int64
    | MKBool of bool
    | MKUnionCase of string * string

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
    | VHostManaged of HostManagedValue

and HostManagedValue =
    { Descriptor: HostTypeDescriptor
      Fields: Map<string, Value>
      Payload: obj }

and TaskOutcome =
    | TaskSucceeded of Value
    | TaskFailed of EvalError

and TaskHandle =
#if FABLE_COMPILER
    { mutable Awaited: bool }
#else
    { Worker: Task<TaskOutcome>
      DebugTaskId: int option
      mutable Awaited: bool }
#endif

and ExternalCallContext =
    { Session: obj
      IsActive: unit -> bool
      Apply: Value -> Value -> Value
      SpawnTask: Value -> Value
      AwaitTask: Value -> Value
      CheckRuntime: unit -> unit
      SetExitCode: int -> unit
      CheckPermissions: HostPermissions -> unit }

and ExternalFunction =
    { RequiredPermissions: HostPermissions
      Name: string
      Scheme: Scheme
      Impl: ExternalCallContext -> Value list -> Value
      Arity: int }

and Env = Map<string, Value>

type ExtensionRegistration =
    { Permissions: HostPermissions
      Types: HostTypeDescriptor list
      Unions: HostUnionDescriptor list
      Functions: ExternalFunction list }

/// The script call has one argument. The evaluator binds the target and its
/// type definitions before calling the host codec.
module JsonDeserialization =
    let name = "Json.deserialize"
    let scheme = Forall([0], TFun(TString, TOption(TVar 0)))

    let tokenScheme target =
        Forall(Set.toList (Types.ftvType target), TFun(TTypeToken, TFun(TString, TOption target)))

    let withToken target (external: ExternalFunction) =
        { external with Scheme = tokenScheme target; Arity = 2 }

    let withTarget definitions target (external: ExternalFunction) =
        let typeDefinitions = VTypeToken(TRecord definitions)
        { external with
            Scheme = Forall([], TFun(TString, TOption target))
            Impl = fun context arguments -> external.Impl context (VTypeToken target :: typeDefinitions :: arguments) }

    let expandTarget definitions target =
        let rec expand seen typ =
            let expanded = Types.expand definitions typ
            if expanded = typ then Types.mapChildren (expand seen) typ
            elif Set.contains typ seen then typ
            else expand (Set.add typ seen) expanded
        expand Set.empty target
