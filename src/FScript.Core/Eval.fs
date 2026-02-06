namespace FScript.Core

module Eval =
    let private builtinIgnore : ExternalFunction =
        { Name = "ignore"
          Scheme = Forall([ 0 ], TFun (TVar 0, TUnit))
          Arity = 1
          Impl = (fun _ -> VUnit) }

    let private literalToValue lit =
        match lit with
        | LInt v -> VInt v
        | LFloat v -> VFloat v
        | LBool v -> VBool v
        | LString v -> VString v

    let rec private valueEquals a b =
        match a, b with
        | VUnit, VUnit -> true
        | VInt x, VInt y -> x = y
        | VFloat x, VFloat y -> x = y
        | VBool x, VBool y -> x = y
        | VString x, VString y -> x = y
        | VList xs, VList ys ->
            xs.Length = ys.Length && List.forall2 valueEquals xs ys
        | VTuple xs, VTuple ys ->
            xs.Length = ys.Length && List.forall2 valueEquals xs ys
        | VRecord xf, VRecord yf ->
            xf.Count = yf.Count
            && (Set.ofSeq xf.Keys = Set.ofSeq yf.Keys)
            && (xf |> Map.forall (fun k xv -> valueEquals xv yf.[k]))
        | VStringMap xf, VStringMap yf ->
            xf.Count = yf.Count
            && (Set.ofSeq xf.Keys = Set.ofSeq yf.Keys)
            && (xf |> Map.forall (fun k xv -> valueEquals xv yf.[k]))
        | VOption None, VOption None -> true
        | VOption (Some x), VOption (Some y) -> valueEquals x y
        | VTypeToken tx, VTypeToken ty -> tx = ty
        | _ -> false

    let rec private patternMatch (pat: Pattern) (value: Value) : Env option =
        match pat, value with
        | PWildcard _, _ -> Some Map.empty
        | PVar (name, _), v -> Some (Map.ofList [ name, v ])
        | PLiteral (lit, _), v ->
            if valueEquals (literalToValue lit) v then Some Map.empty else None
        | PNil _, VList [] -> Some Map.empty
        | PCons (p1, p2, _), VList (h :: t) ->
            match patternMatch p1 h, patternMatch p2 (VList t) with
            | Some env1, Some env2 ->
                Some (Map.fold (fun acc k v -> Map.add k v acc) env1 env2)
            | _ -> None
        | PSome (p, _), VOption (Some v) ->
            patternMatch p v
        | PNone _, VOption None -> Some Map.empty
        | _ -> None

    let rec private evalExpr (typeDefs: Map<string, Type>) (env: Env) (expr: Expr) : Value =
        match expr with
        | ELiteral (lit, _) -> literalToValue lit
        | EVar (name, span) ->
            match env |> Map.tryFind name with
            | Some v -> v
            | None -> raise (EvalException { Message = sprintf "Unbound variable '%s'" name; Span = span })
        | ELambda (arg, body, _) -> VClosure (arg, body, env)
        | EApply (fn, arg, span) ->
            let fVal = evalExpr typeDefs env fn
            let aVal = evalExpr typeDefs env arg
            match fVal with
            | VClosure (argName, body, closureEnv) ->
                let env' = closureEnv |> Map.add argName aVal
                evalExpr typeDefs env' body
            | VExternal (ext, args) ->
                let args' = args @ [ aVal ]
                if args'.Length = ext.Arity then
                    ext.Impl args'
                elif args'.Length < ext.Arity then
                    VExternal (ext, args')
                else
                    raise (EvalException { Message = sprintf "External function '%s' received too many arguments" ext.Name; Span = span })
            | _ -> raise (EvalException { Message = "Attempted to apply non-function"; Span = span })
        | EIf (cond, tExpr, fExpr, span) ->
            match evalExpr typeDefs env cond with
            | VBool true -> evalExpr typeDefs env tExpr
            | VBool false -> evalExpr typeDefs env fExpr
            | _ -> raise (EvalException { Message = "Condition must be bool"; Span = span })
        | ERaise (valueExpr, span) ->
            match evalExpr typeDefs env valueExpr with
            | VString message -> raise (EvalException { Message = message; Span = span })
            | _ -> raise (EvalException { Message = "raise expects a string"; Span = span })
        | EFor (name, source, body, span) ->
            match evalExpr typeDefs env source with
            | VList items ->
                for item in items do
                    let env' = env |> Map.add name item
                    evalExpr typeDefs env' body |> ignore
                VUnit
            | _ -> raise (EvalException { Message = "For loop source must be list"; Span = span })
        | ELet (name, value, body, _) ->
            let v = evalExpr typeDefs env value
            let env' = env |> Map.add name v
            evalExpr typeDefs env' body
        | EMatch (scrutinee, cases, span) ->
            let v = evalExpr typeDefs env scrutinee
            let rec tryCases cs =
                match cs with
                | [] -> raise (EvalException { Message = "No match cases matched"; Span = span })
                | (pat, body, _) :: rest ->
                    match patternMatch pat v with
                    | Some bindings ->
                        let env' = Map.fold (fun acc k v -> Map.add k v acc) env bindings
                        evalExpr typeDefs env' body
                    | None -> tryCases rest
            tryCases cases
        | EList (items, _) ->
            items |> List.map (evalExpr typeDefs env) |> VList
        | ERange (startExpr, endExpr, span) ->
            let startValue = evalExpr typeDefs env startExpr
            let endValue = evalExpr typeDefs env endExpr
            match startValue, endValue with
            | VInt s, VInt e ->
                let step = if s <= e then 1L else -1L
                let rec build acc current =
                    if (step > 0L && current > e) || (step < 0L && current < e) then
                        List.rev acc
                    else
                        build (VInt current :: acc) (current + step)
                VList (build [] s)
            | _ -> raise (EvalException { Message = "Range endpoints must be int"; Span = span })
        | ETuple (items, _) ->
            items |> List.map (evalExpr typeDefs env) |> VTuple
        | ERecord (fields, _) ->
            fields
            |> List.map (fun (name, valueExpr) -> name, evalExpr typeDefs env valueExpr)
            |> Map.ofList
            |> VRecord
        | ERecordUpdate (target, updates, span) ->
            match evalExpr typeDefs env target with
            | VRecord fields ->
                let updated =
                    updates
                    |> List.fold (fun acc (name, valueExpr) ->
                        if Map.containsKey name acc then
                            Map.add name (evalExpr typeDefs env valueExpr) acc
                        else
                            raise (EvalException { Message = sprintf "Record field '%s' not found" name; Span = span })) fields
                VRecord updated
            | _ -> raise (EvalException { Message = "Record update requires a record value"; Span = span })
        | EFieldGet (target, fieldName, span) ->
            match evalExpr typeDefs env target with
            | VRecord fields ->
                match fields.TryFind fieldName with
                | Some value -> value
                | None -> raise (EvalException { Message = sprintf "Record field '%s' not found" fieldName; Span = span })
            | _ -> raise (EvalException { Message = "Field access requires a record value"; Span = span })
        | ECons (head, tail, span) ->
            let h = evalExpr typeDefs env head
            let t = evalExpr typeDefs env tail
            match t with
            | VList xs -> VList (h :: xs)
            | _ -> raise (EvalException { Message = "Right side of '::' must be list"; Span = span })
        | EAppend (a, b, span) ->
            let av = evalExpr typeDefs env a
            let bv = evalExpr typeDefs env b
            match av, bv with
            | VList xs, VList ys -> VList (xs @ ys)
            | _ -> raise (EvalException { Message = "Both sides of '@' must be lists"; Span = span })
        | EBinOp (op, a, b, span) ->
            let av = evalExpr typeDefs env a
            let bv = evalExpr typeDefs env b
            let arith fInt fFloat =
                match av, bv with
                | VInt x, VInt y -> VInt (fInt x y)
                | VFloat x, VFloat y -> VFloat (fFloat x y)
                | _ -> raise (EvalException { Message = "Numeric operands required"; Span = span })
            match op with
            | "|>" ->
                match bv with
                | VClosure (argName, body, closureEnv) ->
                    let env' = closureEnv |> Map.add argName av
                    evalExpr typeDefs env' body
                | VExternal (ext, args) ->
                    let args' = args @ [ av ]
                    if args'.Length = ext.Arity then
                        ext.Impl args'
                    elif args'.Length < ext.Arity then
                        VExternal (ext, args')
                    else
                        raise (EvalException { Message = sprintf "External function '%s' received too many arguments" ext.Name; Span = span })
                | _ -> raise (EvalException { Message = "Right side of '|>' must be a function"; Span = span })
            | "+" -> arith ( + ) ( + )
            | "-" -> arith ( - ) ( - )
            | "*" -> arith ( * ) ( * )
            | "/" ->
                match av, bv with
                | VInt x, VInt y -> VInt (x / y)
                | VFloat x, VFloat y -> VFloat (x / y)
                | _ -> raise (EvalException { Message = "Numeric operands required"; Span = span })
            | "%" ->
                match av, bv with
                | VInt x, VInt y -> VInt (x % y)
                | _ -> raise (EvalException { Message = "Integer operands required"; Span = span })
            | "=" -> VBool (valueEquals av bv)
            | "<" | ">" | "<=" | ">=" ->
                match av, bv with
                | VInt x, VInt y ->
                    let res =
                        match op with
                        | "<" -> x < y
                        | ">" -> x > y
                        | "<=" -> x <= y
                        | ">=" -> x >= y
                        | _ -> false
                    VBool res
                | VFloat x, VFloat y ->
                    let res =
                        match op with
                        | "<" -> x < y
                        | ">" -> x > y
                        | "<=" -> x <= y
                        | ">=" -> x >= y
                        | _ -> false
                    VBool res
                | _ -> raise (EvalException { Message = "Numeric operands required"; Span = span })
            | "&&" | "||" ->
                match av, bv with
                | VBool x, VBool y ->
                    let res = if op = "&&" then x && y else x || y
                    VBool res
                | _ -> raise (EvalException { Message = "Boolean operands required"; Span = span })
            | "::" ->
                match bv with
                | VList xs -> VList (av :: xs)
                | _ -> raise (EvalException { Message = "Right side of '::' must be list"; Span = span })
            | "@" ->
                match av, bv with
                | VList xs, VList ys -> VList (xs @ ys)
                | _ -> raise (EvalException { Message = "Both sides of '@' must be lists"; Span = span })
            | _ -> raise (EvalException { Message = sprintf "Unknown operator %s" op; Span = span })
        | ESome (value, _) -> VOption (Some (evalExpr typeDefs env value))
        | ENone _ -> VOption None
        | ETypeOf (name, span) ->
            match typeDefs.TryFind name with
            | Some t -> VTypeToken t
            | None -> raise (EvalException { Message = $"Unknown type '{name}'"; Span = span })
        | EInterpolatedString (parts, span) ->
            let sb = System.Text.StringBuilder()
            for part in parts do
                match part with
                | IPText text -> sb.Append(text) |> ignore
                | IPExpr pexpr ->
                    match evalExpr typeDefs env pexpr with
                    | VString s -> sb.Append(s) |> ignore
                    | _ -> raise (EvalException { Message = "Interpolation placeholder must evaluate to string"; Span = Ast.spanOfExpr pexpr })
            VString (sb.ToString())

    let evalProgramWithExterns (externs: ExternalFunction list) (program: TypeInfer.TypedProgram) : Value =
        let decls =
            program
            |> List.choose (function | TypeInfer.TSType def -> Some(def.Name, def) | _ -> None)
            |> Map.ofList

        let rec fromRef (seen: Set<string>) (tref: TypeRef) =
            match tref with
            | TRName "unit" -> TUnit
            | TRName "int" -> TInt
            | TRName "float" -> TFloat
            | TRName "bool" -> TBool
            | TRName "string" -> TString
            | TRName n ->
                if seen.Contains n then TNamed n
                else
                    match decls.TryFind n with
                    | Some d ->
                        d.Fields
                        |> List.map (fun (fname, ft) -> fname, fromRef (seen.Add n) ft)
                        |> Map.ofList
                        |> TRecord
                    | None -> TNamed n
            | TRTuple ts -> ts |> List.map (fromRef seen) |> TTuple
            | TRPostfix (inner, "list") -> TList (fromRef seen inner)
            | TRPostfix (inner, "option") -> TOption (fromRef seen inner)
            | TRPostfix (inner, "map") -> TStringMap (fromRef seen inner)
            | TRPostfix (_, suffix) -> failwith $"Unsupported type suffix {suffix}"

        let typeDefs =
            decls
            |> Map.map (fun name def ->
                def.Fields
                |> List.map (fun (fname, ft) -> fname, fromRef (Set.singleton name) ft)
                |> Map.ofList
                |> TRecord)

        let mutable env : Env =
            (builtinIgnore :: externs)
            |> List.fold (fun acc ext -> acc.Add(ext.Name, VExternal (ext, []))) Map.empty
        let mutable lastValue = VUnit
        for stmt in program do
            match stmt with
            | TypeInfer.TSType _ ->
                ()
            | TypeInfer.TSLet(name, expr, _, _) ->
                let v = evalExpr typeDefs env expr
                env <- env |> Map.add name v
            | TypeInfer.TSExpr texpr ->
                lastValue <- evalExpr typeDefs env texpr.Expr
        lastValue

    let evalProgram (program: TypeInfer.TypedProgram) : Value =
        evalProgramWithExterns [] program
