namespace FScript.Core

type ExternalFunction =
    { Name: string
      Scheme: Scheme
      Impl: Value list -> Value
      Arity: int }

and Value =
    | VUnit
    | VInt of int64
    | VFloat of float
    | VBool of bool
    | VString of string
    | VList of Value list
    | VTuple of Value list
    | VRecord of Map<string, Value>
    | VStringMap of Map<string, Value>
    | VOption of Value option
    | VUnionCase of string * string * Value option
    | VUnionCtor of string * string
    | VTypeToken of Type
    | VClosure of string * Expr * Env ref
    | VExternal of ExternalFunction * Value list

and Env = Map<string, Value>
