namespace FScript.Language

open System

[<StructuralEquality; StructuralComparison>]
type Type =
    | TUnit
    | TInt
    | TFloat
    | TBool
    | TString
    | TList of Type
    | TTask of Type
    | TTuple of Type list
    | TRecord of Map<string, Type>
    | TMap of Type * Type
    | TOption of Type
    | TFun of Type * Type
    | TNamed of string
    | TUnion of string * Map<string, Type option>
    | TTypeToken
    | TVar of int
    | TNumeric of int
    | THostManaged of name: string * fields: Map<string, Type>
    // The witness retains the whole argument type through structural readers.
    | TRecordShape of id: int * fields: Map<string, Type> * requiresConstruction: bool

type HostTypeDescriptor =
    { Name: string
      Fields: Map<string, Type>
      HostManaged: bool }

/// Script-constructible union supplied by a host; payloads use ordinary script types.
type HostUnionDescriptor =
    { Name: string
      Cases: Map<string, Type option> }

[<StructuralEquality; StructuralComparison>]
type Scheme = Forall of int list * Type

/// Trusted host contracts. Function purity includes deterministic, effect-free,
/// successful termination; type purity means deeply immutable values and payloads.
type PurityContracts =
    { PureFunctions: Set<string>
      PureTypes: Set<string> }

module PurityContracts =
    let empty = { PureFunctions = Set.empty; PureTypes = Set.empty }

module Types =
    let mutable private nextId = 0

    let freshVar () =
#if FABLE_COMPILER
        let id = nextId
        nextId <- nextId + 1
#else
        let id = System.Threading.Interlocked.Increment(&nextId) - 1
#endif
        TVar id

    let freshNumeric () =
        match freshVar () with
        | TVar id -> TNumeric id
        | _ -> failwith "Expected fresh type variable"

    let rec ftvType t =
        match t with
        | TUnit | TInt | TFloat | TBool | TString -> Set.empty
        | TList t1 -> ftvType t1
        | TTask t1 -> ftvType t1
        | TTuple ts -> ts |> List.map ftvType |> List.fold Set.union Set.empty
        | THostManaged _ -> Set.empty
        | TRecordShape (id, fields, _) -> Set.add id (fields |> Map.values |> Seq.map ftvType |> Seq.fold Set.union Set.empty)
        | TRecord fields -> fields |> Map.values |> Seq.map ftvType |> Seq.fold Set.union Set.empty
        | TMap (tk, tv) -> Set.union (ftvType tk) (ftvType tv)
        | TOption t1 -> ftvType t1
        | TFun (a, b) -> Set.union (ftvType a) (ftvType b)
        | TNamed _ | TUnion _ | TTypeToken -> Set.empty
        | TVar v | TNumeric v -> Set.singleton v

    let ftvScheme (Forall (vars, t)) =
        Set.difference (ftvType t) (Set.ofList vars)

    let ftvEnv (env: Map<string, Scheme>) =
        env |> Map.values |> Seq.map ftvScheme |> Seq.fold Set.union Set.empty

    let generalize env t =
        let typeVariables = ftvType t
        if Set.isEmpty typeVariables then
            Forall ([], t)
        else
            let vars = Set.difference typeVariables (ftvEnv env) |> Set.toList
            Forall (vars, t)

    let rec typeToString t =
        let rec go t =
            match t with
            | TUnit -> "unit"
            | TInt -> "int"
            | TFloat -> "float"
            | TBool -> "bool"
            | TString -> "string"
            | TList t1 -> sprintf "%s list" (postfixArg t1)
            | TTask t1 -> sprintf "%s task" (postfixArg t1)
            | TTuple ts -> ts |> List.map go |> String.concat " * " |> sprintf "(%s)"
            | THostManaged (name, _) -> name
            | TRecordShape (_, fields, _) -> go (TRecord fields)
            | TRecord fields ->
                fields
                |> Map.toList
                |> List.map (fun (name, t) -> sprintf "%s: %s" name (go t))
                |> String.concat "; "
                |> sprintf "{ %s }"
            | TMap (tk, tv) ->
                match tk with
                | TString -> sprintf "%s map" (postfixArg tv)
                | _ -> sprintf "%s map[%s]" (postfixArg tv) (go tk)
            | TOption t1 -> sprintf "%s option" (postfixArg t1)
            | TFun (a, b) -> sprintf "(%s -> %s)" (go a) (go b)
            | TNamed n -> n
            | TUnion (name, _) -> name
            | TTypeToken -> "type"
            | TNumeric _ -> "numeric"
            | TVar v ->
                let letter = char (int 'a' + (v % 26))
                let suffix = v / 26
                if suffix = 0 then sprintf "'%c" letter else sprintf "'%c%d" letter suffix
        and postfixArg t =
            match t with
            | TFun _ | TTuple _ | TRecord _ -> sprintf "(%s)" (go t)
            | _ -> go t
        go t
