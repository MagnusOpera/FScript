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
    | TApplied of name: string * arguments: Type list
    | TGenericDefinition of parameters: int list * body: Type
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
      HostManaged: bool
      TypeParameters: int list }

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

    type TypeConstructor =
        | ListType
        | OptionType
        | TaskType
        | MapType
        | UserType of string

    // Keep the established built-in cases at the public boundary and expose all
    // applications uniformly to unification and specialization.
    let (|TypeApplication|_|) = function
        | TList item -> Some (ListType, [item])
        | TOption item -> Some (OptionType, [item])
        | TTask item -> Some (TaskType, [item])
        | TMap (key, value) -> Some (MapType, [key; value])
        | TApplied (name, arguments) -> Some (UserType name, arguments)
        | _ -> None

    let mapChildren map = function
        | TypeApplication (constructor, arguments) ->
            match constructor, List.map map arguments with
            | ListType, [item] -> TList item
            | OptionType, [item] -> TOption item
            | TaskType, [item] -> TTask item
            | MapType, [key; value] -> TMap (key, value)
            | UserType name, arguments -> TApplied (name, arguments)
            | _ -> invalidOp "Invalid type constructor arity"
        | TTuple items -> TTuple (List.map map items)
        | TFun (input, output) -> TFun (map input, map output)
        | TRecord fields -> TRecord (Map.map (fun _ -> map) fields)
        | TRecordShape (id, fields, constructible) -> TRecordShape (id, Map.map (fun _ -> map) fields, constructible)
        | TUnion (name, cases) -> TUnion (name, Map.map (fun _ -> Option.map map) cases)
        | typ -> typ

    let rec substitute (replacements: Map<int, Type>) typ =
        match typ with
        | TVar id | TNumeric id -> replacements.TryFind id |> Option.defaultValue typ
        | TGenericDefinition (parameters, body) ->
            let scope = List.fold (fun state parameter -> Map.remove parameter state) replacements parameters
            TGenericDefinition (parameters, substitute scope body)
        | _ -> mapChildren (substitute replacements) typ

    let expand (definitions: Map<string, Type>) typ =
        match typ with
        | TNamed name -> definitions.TryFind name |> Option.defaultValue typ
        | TApplied (name, arguments) ->
            match definitions.TryFind name with
            | Some (TGenericDefinition (parameters, body)) when parameters.Length = arguments.Length ->
                substitute (List.zip parameters arguments |> Map.ofList) body
            | _ -> typ
        | _ -> typ

    let definitionBody = function
        | TGenericDefinition (_, body) -> body
        | typ -> typ

    let applied name = function
        | TGenericDefinition (parameters, _) -> TApplied (name, parameters |> List.map (fun _ -> freshVar ()))
        | _ -> TNamed name

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
        | TApplied (_, arguments) -> arguments |> List.map ftvType |> List.fold Set.union Set.empty
        | TGenericDefinition (parameters, body) -> Set.difference (ftvType body) (Set.ofList parameters)
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
            | TApplied (name, arguments) -> sprintf "%s<%s>" name (arguments |> List.map go |> String.concat ", ")
            | TGenericDefinition (_, body) -> go body
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
