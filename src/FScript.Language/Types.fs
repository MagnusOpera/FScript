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
    | TTuple of Type list
    | TRecord of Map<string, Type>
    | TMap of Type * Type
    | TOption of Type
    | TFun of Type * Type
    | TNamed of string
    | TUnion of string * Map<string, Type option>
    | TTypeToken
    | TVar of int

[<StructuralEquality; StructuralComparison>]
type Scheme = Forall of int list * Type

module Types =
    let mutable private nextId = 0

    let freshVar () =
        let id = nextId
        nextId <- nextId + 1
        TVar id

    let rec ftvType t =
        match t with
        | TUnit | TInt | TFloat | TBool | TString -> Set.empty
        | TList t1 -> ftvType t1
        | TTuple ts -> ts |> List.map ftvType |> List.fold Set.union Set.empty
        | TRecord fields -> fields |> Map.values |> Seq.map ftvType |> Seq.fold Set.union Set.empty
        | TMap (tk, tv) -> Set.union (ftvType tk) (ftvType tv)
        | TOption t1 -> ftvType t1
        | TFun (a, b) -> Set.union (ftvType a) (ftvType b)
        | TNamed _ | TUnion _ | TTypeToken -> Set.empty
        | TVar v -> Set.singleton v

    let ftvScheme (Forall (vars, t)) =
        Set.difference (ftvType t) (Set.ofList vars)

    let ftvEnv (env: Map<string, Scheme>) =
        env |> Map.values |> Seq.map ftvScheme |> Seq.fold Set.union Set.empty

    let generalize env t =
        let vars = Set.difference (ftvType t) (ftvEnv env) |> Set.toList
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
            | TTuple ts -> ts |> List.map go |> String.concat " * " |> sprintf "(%s)"
            | TRecord fields ->
                fields
                |> Map.toList
                |> List.map (fun (name, t) -> sprintf "%s: %s" name (go t))
                |> String.concat "; "
                |> sprintf "{ %s }"
            | TMap (tk, tv) ->
                match tk with
                | TString -> sprintf "%s map" (postfixArg tv)
                | _ -> sprintf "map<%s, %s>" (go tk) (go tv)
            | TOption t1 -> sprintf "%s option" (postfixArg t1)
            | TFun (a, b) -> sprintf "(%s -> %s)" (go a) (go b)
            | TNamed n -> n
            | TUnion (name, _) -> name
            | TTypeToken -> "type"
            | TVar v ->
                let letter = char (int 'a' + (v % 26))
                let suffix = v / 26
                if suffix = 0 then sprintf "'%c" letter else sprintf "'%c%d" letter suffix
        and postfixArg t =
            match t with
            | TFun _ | TTuple _ | TRecord _ -> sprintf "(%s)" (go t)
            | _ -> go t
        go t
