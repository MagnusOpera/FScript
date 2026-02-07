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

    let rec private valueToInterpolationString v =
        match v with
        | VUnit -> "()"
        | VInt i -> string i
        | VFloat f -> string f
        | VBool b -> if b then "true" else "false"
        | VString s -> s
        | VList xs ->
            xs |> List.map valueToInterpolationString |> String.concat ";" |> sprintf "[%s]"
        | VTuple xs ->
            xs |> List.map valueToInterpolationString |> String.concat ", " |> sprintf "(%s)"
        | VRecord fields ->
            fields
            |> Map.toList
            |> List.map (fun (name, value) -> sprintf "%s = %s" name (valueToInterpolationString value))
            |> String.concat "; "
            |> sprintf "{ %s }"
        | VStringMap fields ->
            fields
            |> Map.toList
            |> List.map (fun (name, value) -> sprintf "\"%s\" => %s" name (valueToInterpolationString value))
            |> String.concat "; "
            |> sprintf "map { %s }"
        | VOption None -> "None"
        | VOption (Some value) -> sprintf "Some %s" (valueToInterpolationString value)
        | VTypeToken t -> sprintf "<type %s>" (Types.typeToString t)
        | VClosure _ -> "<fun>"
        | VExternal _ -> "<extern>"

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
        | PTuple (patterns, _), VTuple values when patterns.Length = values.Length ->
            (Some Map.empty, List.zip patterns values)
            ||> List.fold (fun accOpt (p, v) ->
                match accOpt, patternMatch p v with
                | Some acc, Some next ->
                    Some (Map.fold (fun state k value -> Map.add k value state) acc next)
                | _ -> None)
        | PSome (p, _), VOption (Some v) ->
            patternMatch p v
        | PNone _, VOption None -> Some Map.empty
        | _ -> None

    let rec private applyFunctionValue
        (eval: Map<string, Type> -> Env -> Expr -> Value)
        (typeDefs: Map<string, Type>)
        (span: Span)
        (fnValue: Value)
        (argValue: Value)
        : Value =
        match fnValue with
        | VClosure (argName, body, closureEnv) ->
            let env' = closureEnv |> Map.add argName argValue
            eval typeDefs env' body
        | VExternal (ext, args) ->
            let args' = args @ [ argValue ]
            if args'.Length = ext.Arity then
                if ext.Name = "List.map" then
                    match args' with
                    | [ mapper; VList items ] ->
                        items
                        |> List.map (fun item -> applyFunctionValue eval typeDefs span mapper item)
                        |> VList
                    | _ -> raise (EvalException { Message = "List.map expects (function, list)"; Span = span })
                elif ext.Name = "List.iter" then
                    match args' with
                    | [ iterator; VList items ] ->
                        for item in items do
                            applyFunctionValue eval typeDefs span iterator item |> ignore
                        VUnit
                    | _ -> raise (EvalException { Message = "List.iter expects (function, list)"; Span = span })
                elif ext.Name = "List.choose" then
                    match args' with
                    | [ chooser; VList items ] ->
                        let chosen =
                            items
                            |> List.fold (fun acc item ->
                                match applyFunctionValue eval typeDefs span chooser item with
                                | VOption (Some value) -> value :: acc
                                | VOption None -> acc
                                | _ -> raise (EvalException { Message = "List.choose chooser must return option"; Span = span })) []
                            |> List.rev
                        VList chosen
                    | _ -> raise (EvalException { Message = "List.choose expects (function, list)"; Span = span })
                elif ext.Name = "List.collect" then
                    match args' with
                    | [ collector; VList items ] ->
                        let collected =
                            items
                            |> List.fold (fun acc item ->
                                match applyFunctionValue eval typeDefs span collector item with
                                | VList values -> acc @ values
                                | _ -> raise (EvalException { Message = "List.collect collector must return list"; Span = span })) []
                        VList collected
                    | _ -> raise (EvalException { Message = "List.collect expects (function, list)"; Span = span })
                elif ext.Name = "List.contains" then
                    match args' with
                    | [ needle; VList items ] -> VBool (items |> List.exists (fun item -> valueEquals item needle))
                    | _ -> raise (EvalException { Message = "List.contains expects (value, list)"; Span = span })
                elif ext.Name = "List.distinct" then
                    match args' with
                    | [ VList items ] ->
                        let distinctItems =
                            items
                            |> List.fold (fun acc item ->
                                if acc |> List.exists (fun existing -> valueEquals existing item) then acc
                                else acc @ [ item ]) []
                        VList distinctItems
                    | _ -> raise (EvalException { Message = "List.distinct expects (list)"; Span = span })
                elif ext.Name = "List.exists" then
                    match args' with
                    | [ predicate; VList items ] ->
                        let rec loop xs =
                            match xs with
                            | [] -> false
                            | x :: rest ->
                                match applyFunctionValue eval typeDefs span predicate x with
                                | VBool true -> true
                                | VBool false -> loop rest
                                | _ -> raise (EvalException { Message = "List.exists predicate must return bool"; Span = span })
                        VBool (loop items)
                    | _ -> raise (EvalException { Message = "List.exists expects (function, list)"; Span = span })
                elif ext.Name = "List.fold" then
                    match args' with
                    | [ folder; state; VList items ] ->
                        let finalState =
                            items
                            |> List.fold (fun acc item ->
                                let step = applyFunctionValue eval typeDefs span folder acc
                                applyFunctionValue eval typeDefs span step item) state
                        finalState
                    | _ -> raise (EvalException { Message = "List.fold expects (function, state, list)"; Span = span })
                elif ext.Name = "List.filter" then
                    match args' with
                    | [ predicate; VList items ] ->
                        let filtered =
                            items
                            |> List.filter (fun item ->
                                match applyFunctionValue eval typeDefs span predicate item with
                                | VBool b -> b
                                | _ -> raise (EvalException { Message = "List.filter predicate must return bool"; Span = span }))
                        VList filtered
                    | _ -> raise (EvalException { Message = "List.filter expects (function, list)"; Span = span })
                elif ext.Name = "List.tryFind" then
                    match args' with
                    | [ predicate; VList items ] ->
                        let rec loop xs =
                            match xs with
                            | [] -> VOption None
                            | x :: rest ->
                                match applyFunctionValue eval typeDefs span predicate x with
                                | VBool true -> VOption (Some x)
                                | VBool false -> loop rest
                                | _ -> raise (EvalException { Message = "List.tryFind predicate must return bool"; Span = span })
                        loop items
                    | _ -> raise (EvalException { Message = "List.tryFind expects (function, list)"; Span = span })
                elif ext.Name = "List.tryFindIndex" then
                    match args' with
                    | [ predicate; VList items ] ->
                        let rec loop index xs =
                            match xs with
                            | [] -> VOption None
                            | x :: rest ->
                                match applyFunctionValue eval typeDefs span predicate x with
                                | VBool true -> VOption (Some (VInt index))
                                | VBool false -> loop (index + 1L) rest
                                | _ -> raise (EvalException { Message = "List.tryFindIndex predicate must return bool"; Span = span })
                        loop 0L items
                    | _ -> raise (EvalException { Message = "List.tryFindIndex expects (function, list)"; Span = span })
                elif ext.Name = "Option.map" then
                    match args' with
                    | [ mapper; VOption (Some value) ] ->
                        let mapped = applyFunctionValue eval typeDefs span mapper value
                        VOption (Some mapped)
                    | [ _; VOption None ] -> VOption None
                    | _ -> raise (EvalException { Message = "Option.map expects (function, option)"; Span = span })
                elif ext.Name = "Option.defaultWith" then
                    match args' with
                    | [ _; VOption (Some value) ] -> value
                    | [ fallback; VOption None ] ->
                        applyFunctionValue eval typeDefs span fallback VUnit
                    | _ -> raise (EvalException { Message = "Option.defaultWith expects (function, option)"; Span = span })
                else
                    ext.Impl args'
            elif args'.Length < ext.Arity then
                VExternal (ext, args')
            else
                raise (EvalException { Message = sprintf "External function '%s' received too many arguments" ext.Name; Span = span })
        | _ -> raise (EvalException { Message = "Attempted to apply non-function"; Span = span })

    let rec private evalExpr (typeDefs: Map<string, Type>) (env: Env) (expr: Expr) : Value =
        match expr with
        | ELiteral (lit, _) -> literalToValue lit
        | EVar (name, span) ->
            match env |> Map.tryFind name with
            | Some v -> v
            | None -> raise (EvalException { Message = sprintf "Unbound variable '%s'" name; Span = span })
        | ELambda (param, body, _) -> VClosure (param.Name, body, env)
        | EApply (fn, arg, span) ->
            let fVal = evalExpr typeDefs env fn
            let aVal = evalExpr typeDefs env arg
            applyFunctionValue evalExpr typeDefs span fVal aVal
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
        | ELet (name, value, body, isRec, span) ->
            if isRec then
                match value with
                | ELambda (param, lambdaBody, _) ->
                    let rec selfValue : Value = VClosure (param.Name, lambdaBody, recEnv)
                    and recEnv : Env = env |> Map.add name selfValue
                    evalExpr typeDefs recEnv body
                | _ ->
                    raise (EvalException { Message = "let rec requires a function binding"; Span = span })
            else
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
            match target with
            | EVar (moduleName, _) when not (env.ContainsKey moduleName) ->
                let qualifiedName = $"{moduleName}.{fieldName}"
                match env.TryFind qualifiedName with
                | Some value -> value
                | None ->
                    match evalExpr typeDefs env target with
                    | VRecord fields ->
                        match fields.TryFind fieldName with
                        | Some fieldValue -> fieldValue
                        | None -> raise (EvalException { Message = sprintf "Record field '%s' not found" fieldName; Span = span })
                    | _ -> raise (EvalException { Message = "Field access requires a record value"; Span = span })
            | _ ->
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
                applyFunctionValue evalExpr typeDefs span bv av
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
                    let rendered = evalExpr typeDefs env pexpr |> valueToInterpolationString
                    sb.Append(rendered) |> ignore
            VString (sb.ToString())

    let evalProgramWithExterns (externs: ExternalFunction list) (program: TypeInfer.TypedProgram) : Value =
        let decls =
            program
            |> List.choose (function | TypeInfer.TSType def -> Some(def.Name, def) | _ -> None)
            |> Map.ofList

        let unknownSpan = Span.mk (Span.pos 0 0) (Span.pos 0 0)

        let rec fromRef (stack: string list) (tref: TypeRef) =
            match tref with
            | TRName "unit" -> TUnit
            | TRName "int" -> TInt
            | TRName "float" -> TFloat
            | TRName "bool" -> TBool
            | TRName "string" -> TString
            | TRName n ->
                match decls.TryFind n with
                | Some d ->
                    match stack |> List.tryFindIndex ((=) n) with
                    | Some 0 ->
                        if d.IsRecursive then
                            TNamed n
                        else
                            raise (EvalException { Message = $"Recursive type '{n}' requires 'type rec'"; Span = unknownSpan })
                    | Some _ ->
                        raise (EvalException { Message = "Mutual recursive types are not supported"; Span = unknownSpan })
                    | None ->
                        d.Fields
                        |> List.map (fun (fname, ft) -> fname, fromRef (n :: stack) ft)
                        |> Map.ofList
                        |> TRecord
                | None -> TNamed n
            | TRTuple ts -> ts |> List.map (fromRef stack) |> TTuple
            | TRFun (a, b) -> TFun(fromRef stack a, fromRef stack b)
            | TRPostfix (inner, "list") -> TList (fromRef stack inner)
            | TRPostfix (inner, "option") -> TOption (fromRef stack inner)
            | TRPostfix (inner, "map") -> TStringMap (fromRef stack inner)
            | TRPostfix (_, suffix) ->
                raise (EvalException { Message = $"Unsupported type suffix {suffix}"; Span = unknownSpan })

        let typeDefs =
            decls
            |> Map.map (fun name def ->
                def.Fields
                |> List.map (fun (fname, ft) -> fname, fromRef [ name ] ft)
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
            | TypeInfer.TSLet(name, expr, _, isRec, span) ->
                if isRec then
                    match expr with
                    | ELambda (param, lambdaBody, _) ->
                        let rec selfValue : Value = VClosure (param.Name, lambdaBody, recEnv)
                        and recEnv : Env = env |> Map.add name selfValue
                        env <- recEnv
                    | _ ->
                        raise (EvalException { Message = "let rec requires a function binding"; Span = span })
                else
                    let v = evalExpr typeDefs env expr
                    env <- env |> Map.add name v
            | TypeInfer.TSExpr texpr ->
                lastValue <- evalExpr typeDefs env texpr.Expr
        lastValue

    let evalProgram (program: TypeInfer.TypedProgram) : Value =
        evalProgramWithExterns [] program
