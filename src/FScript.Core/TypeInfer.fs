namespace FScript.Core

module TypeInfer =
    let private unknownSpan = Span.mk (Span.pos 0 0) (Span.pos 0 0)
    type Subst = Map<int, Type>

    let private emptySubst : Subst = Map.empty

    let rec private applyType (s: Subst) (t: Type) : Type =
        match t with
        | TVar v -> s |> Map.tryFind v |> Option.defaultValue t
        | TList t1 -> TList (applyType s t1)
        | TTuple ts -> TTuple (ts |> List.map (applyType s))
        | TRecord fields -> TRecord (fields |> Map.map (fun _ t1 -> applyType s t1))
        | TStringMap t1 -> TStringMap (applyType s t1)
        | TOption t1 -> TOption (applyType s t1)
        | TFun (a, b) -> TFun (applyType s a, applyType s b)
        | _ -> t

    let private applyScheme (s: Subst) (Forall (vars, t)) =
        let s' = vars |> List.fold (fun acc v -> Map.remove v acc) s
        Forall (vars, applyType s' t)

    let private applyEnv (s: Subst) (env: Map<string, Scheme>) =
        env |> Map.map (fun _ sc -> applyScheme s sc)

    let private compose (s1: Subst) (s2: Subst) : Subst =
        let s2' = s2 |> Map.map (fun _ t -> applyType s1 t)
        Map.fold (fun acc k v -> Map.add k v acc) s1 s2'

    let private occurs (v: int) (t: Type) =
        Types.ftvType t |> Set.contains v

    let private unify (t1: Type) (t2: Type) (span: Span) : Subst =
        let rec uni a b =
            match a, b with
            | TUnit, TUnit
            | TInt, TInt
            | TFloat, TFloat
            | TBool, TBool
            | TString, TString
            | TTypeToken, TTypeToken -> emptySubst
            | TNamed x, TNamed y when x = y -> emptySubst
            | TList x, TList y -> uni x y
            | TTuple xs, TTuple ys ->
                if xs.Length <> ys.Length then
                    raise (TypeException { Message = "Tuple arity mismatch"; Span = span })
                List.zip xs ys
                |> List.fold (fun sAcc (x, y) ->
                    let s1 = uni (applyType sAcc x) (applyType sAcc y)
                    compose s1 sAcc) emptySubst
            | TRecord xf, TRecord yf ->
                if xf.Count <> yf.Count || (Set.ofSeq xf.Keys <> Set.ofSeq yf.Keys) then
                    raise (TypeException { Message = "Record field set mismatch"; Span = span })
                xf
                |> Map.toList
                |> List.fold (fun sAcc (name, xt) ->
                    let yt = yf.[name]
                    let s1 = uni (applyType sAcc xt) (applyType sAcc yt)
                    compose s1 sAcc) emptySubst
            | TStringMap x, TStringMap y -> uni x y
            | TOption x, TOption y -> uni x y
            | TFun (a1, b1), TFun (a2, b2) ->
                let s1 = uni a1 a2
                let s2 = uni (applyType s1 b1) (applyType s1 b2)
                compose s2 s1
            | TVar v, t
            | t, TVar v ->
                if t = TVar v then emptySubst
                elif occurs v t then raise (TypeException { Message = "Occurs check failed"; Span = span })
                else Map.ofList [ v, t ]
            | _ ->
                raise (TypeException { Message = sprintf "Type mismatch: %s vs %s" (Types.typeToString a) (Types.typeToString b); Span = span })
        uni t1 t2

    let private instantiate (Forall (vars, t)) =
        let freshMap = vars |> List.map (fun v -> v, Types.freshVar()) |> Map.ofList
        let rec subst t =
            match t with
            | TVar v -> freshMap |> Map.tryFind v |> Option.defaultValue t
            | TList a -> TList (subst a)
            | TTuple ts -> TTuple (ts |> List.map subst)
            | TRecord fields -> TRecord (fields |> Map.map (fun _ t1 -> subst t1))
            | TStringMap a -> TStringMap (subst a)
            | TOption a -> TOption (subst a)
            | TFun (a, b) -> TFun (subst a, subst b)
            | _ -> t
        subst t

    type TypedExpr = { Expr: Expr; Type: Type; Span: Span }
    type TypedStmt =
        | TSType of TypeDef
        | TSLet of string * Expr * Type * bool * Span
        | TSExpr of TypedExpr
    type TypedProgram = TypedStmt list

    let private asTyped expr t = { Expr = expr; Type = t; Span = Ast.spanOfExpr expr }

    let private builtinType (name: string) : Type option =
        match name with
        | "unit" -> Some TUnit
        | "int" -> Some TInt
        | "float" -> Some TFloat
        | "bool" -> Some TBool
        | "string" -> Some TString
        | _ -> None

    let rec private typeFromRef (decls: Map<string, TypeDef>) (stack: string list) (tref: TypeRef) : Type =
        match tref with
        | TRName name ->
            match builtinType name with
            | Some t -> t
            | None ->
                match decls.TryFind name with
                | Some def ->
                    match stack |> List.tryFindIndex ((=) name) with
                    | Some 0 ->
                        if def.IsRecursive then
                            TNamed name
                        else
                            raise (TypeException { Message = $"Recursive type '{name}' requires 'type rec'"; Span = unknownSpan })
                    | Some _ ->
                        raise (TypeException { Message = "Mutual recursive types are not supported"; Span = unknownSpan })
                    | None ->
                        def.Fields
                        |> List.map (fun (field, t) -> field, typeFromRef decls (name :: stack) t)
                        |> Map.ofList
                        |> TRecord
                | None -> TNamed name
        | TRTuple ts ->
            ts |> List.map (typeFromRef decls stack) |> TTuple
        | TRPostfix (inner, suffix) ->
            let resolved = typeFromRef decls stack inner
            match suffix with
            | "list" -> TList resolved
            | "option" -> TOption resolved
            | "map" -> TStringMap resolved
            | _ -> raise (TypeException { Message = $"Unknown type suffix '{suffix}'"; Span = unknownSpan })

    let rec private inferPattern (pat: Pattern) : Map<string, Type> * Type =
        match pat with
        | PWildcard _ -> Map.empty, Types.freshVar()
        | PVar (name, _) ->
            let tv = Types.freshVar()
            Map.ofList [ name, tv ], tv
        | PLiteral (lit, _) ->
            let t =
                match lit with
                | LInt _ -> TInt
                | LFloat _ -> TFloat
                | LBool _ -> TBool
                | LString _ -> TString
            Map.empty, t
        | PNil _ ->
            let tv = Types.freshVar()
            Map.empty, TList tv
        | PCons (p1, p2, span) ->
            let env1, t1 = inferPattern p1
            let env2, t2 = inferPattern p2
            let s = unify t2 (TList t1) span
            let env = Map.fold (fun acc k v -> Map.add k v acc) env1 env2
            env |> Map.map (fun _ ty -> applyType s ty), applyType s (TList t1)
        | PTuple (ps, _) ->
            let env, types =
                ps
                |> List.fold (fun (envAcc, typesAcc) p ->
                    let envPart, tPart = inferPattern p
                    let merged = Map.fold (fun acc k v -> Map.add k v acc) envAcc envPart
                    merged, tPart :: typesAcc) (Map.empty, [])
            env, TTuple (List.rev types)
        | PSome (p, _) ->
            let env, t = inferPattern p
            env, TOption t
        | PNone _ ->
            let tv = Types.freshVar()
            Map.empty, TOption tv

    let private numericResult (t: Type) (span: Span) =
        match t with
        | TInt | TFloat -> t
        | TVar _ -> TInt
        | _ -> raise (TypeException { Message = "Expected numeric type"; Span = span })

    let rec private inferExpr (typeDefs: Map<string, Type>) (env: Map<string, Scheme>) (expr: Expr) : Subst * Type * TypedExpr =
        match expr with
        | ELiteral (lit, _) ->
            let t =
                match lit with
                | LInt _ -> TInt
                | LFloat _ -> TFloat
                | LBool _ -> TBool
                | LString _ -> TString
            emptySubst, t, asTyped expr t
        | EVar (name, span) ->
            match env |> Map.tryFind name with
            | Some scheme ->
                let t = instantiate scheme
                emptySubst, t, asTyped expr t
            | None -> raise (TypeException { Message = sprintf "Unbound variable '%s'" name; Span = span })
        | ETypeOf (name, span) ->
            if typeDefs.ContainsKey name then
                emptySubst, TTypeToken, asTyped expr TTypeToken
            else
                raise (TypeException { Message = sprintf "Unknown type '%s'" name; Span = span })
        | EInterpolatedString (parts, span) ->
            let mutable sAcc = emptySubst
            for part in parts do
                match part with
                | IPText _ -> ()
                | IPExpr pexpr ->
                    let s1, _, _ = inferExpr typeDefs (applyEnv sAcc env) pexpr
                    sAcc <- compose s1 sAcc
            sAcc, TString, asTyped expr TString
        | ELambda (arg, body, _) ->
            let tv = Types.freshVar()
            let env' = env |> Map.add arg (Forall([], tv))
            let s1, tBody, _ = inferExpr typeDefs env' body
            let tArg = applyType s1 tv
            let tFun = TFun (tArg, tBody)
            s1, tFun, asTyped expr tFun
        | EApply (fn, arg, span) ->
            let s1, t1, _ = inferExpr typeDefs env fn
            let env2 = applyEnv s1 env
            let s2, t2, _ = inferExpr typeDefs env2 arg
            let tv = Types.freshVar()
            let s3 = unify (applyType s2 t1) (TFun (t2, tv)) span
            let s = compose s3 (compose s2 s1)
            let tRes = applyType s tv
            s, tRes, asTyped expr tRes
        | EIf (cond, tExpr, fExpr, span) ->
            let s1, tCond, _ = inferExpr typeDefs env cond
            let sBool = unify tCond TBool span
            let env2 = applyEnv (compose sBool s1) env
            let s2, tThen, _ = inferExpr typeDefs env2 tExpr
            let env3 = applyEnv (compose s2 (compose sBool s1)) env
            let s3, tElse, _ = inferExpr typeDefs env3 fExpr
            let s4 = unify (applyType s3 tThen) tElse span
            let s = compose s4 (compose s3 (compose s2 (compose sBool s1)))
            let tRes = applyType s tElse
            s, tRes, asTyped expr tRes
        | ERaise (value, span) ->
            let s1, t1, _ = inferExpr typeDefs env value
            let s2 = unify (applyType s1 t1) TString span
            let s = compose s2 s1
            let tRes = Types.freshVar()
            s, tRes, asTyped expr tRes
        | EFor (name, source, body, span) ->
            let s1, tSource, _ = inferExpr typeDefs env source
            let tv = Types.freshVar()
            let sList = unify (applyType s1 tSource) (TList tv) span
            let sSource = compose sList s1
            let itemType = applyType sSource tv
            let envBody =
                applyEnv sSource env
                |> Map.add name (Forall([], itemType))
            let s2, tBody, _ = inferExpr typeDefs envBody body
            let sBodyUnit = unify (applyType s2 tBody) TUnit (Ast.spanOfExpr body)
            let s = compose sBodyUnit (compose s2 sSource)
            s, TUnit, asTyped expr TUnit
        | ELet (name, value, body, isRec, span) ->
            if isRec then
                let tv = Types.freshVar()
                let envRec = env |> Map.add name (Forall([], tv))
                let s1, t1, _ = inferExpr typeDefs envRec value
                let s2 = unify (applyType s1 tv) t1 span
                let sValue = compose s2 s1
                let env1 = applyEnv sValue env
                let scheme = Types.generalize env1 (applyType sValue t1)
                let env2 = env1 |> Map.add name scheme
                let s3, tBody, _ = inferExpr typeDefs env2 body
                let s = compose s3 sValue
                s, tBody, asTyped expr tBody
            else
                let s1, t1, _ = inferExpr typeDefs env value
                let sDiscard =
                    if name = "_" then
                        unify (applyType s1 t1) TUnit (Ast.spanOfExpr value)
                    else
                        emptySubst
                let sValue = compose sDiscard s1
                let env1 = applyEnv sValue env
                let scheme = Types.generalize env1 (applyType sValue t1)
                let env2 = env1 |> Map.add name scheme
                let s2, t2, _ = inferExpr typeDefs env2 body
                let s = compose s2 sValue
                s, t2, asTyped expr t2
        | EMatch (scrutinee, cases, span) ->
            let s1, tScrut, _ = inferExpr typeDefs env scrutinee
            let mutable sAcc = s1
            let mutable resultTypeOpt : Type option = None
            for (pat, body, caseSpan) in cases do
                let envPat, tPat = inferPattern pat
                let sPat = unify (applyType sAcc tScrut) tPat caseSpan
                let envCase = applyEnv (compose sPat sAcc) env
                let envCase' = envPat |> Map.fold (fun acc k v -> Map.add k (Forall([], applyType sPat v)) acc) envCase
                let sBody, tBody, _ = inferExpr typeDefs envCase' body
                let tBody' = applyType sBody tBody
                resultTypeOpt <-
                    match resultTypeOpt with
                    | None -> Some tBody'
                    | Some tPrev ->
                        let sEq = unify tPrev tBody' caseSpan
                        sAcc <- compose sEq sAcc
                        Some (applyType sEq tPrev)
                sAcc <- compose sBody (compose sPat sAcc)
            let tRes = resultTypeOpt |> Option.defaultValue (Types.freshVar())
            sAcc, tRes, asTyped expr tRes
        | EList (items, span) ->
            let tv = Types.freshVar()
            let mutable sAcc = emptySubst
            for item in items do
                let s1, t1, _ = inferExpr typeDefs (applyEnv sAcc env) item
                let s2 = unify (applyType s1 tv) t1 span
                sAcc <- compose s2 (compose s1 sAcc)
            let tRes = TList (applyType sAcc tv)
            sAcc, tRes, asTyped expr tRes
        | ERange (startExpr, endExpr, span) ->
            let s1, tStart, _ = inferExpr typeDefs env startExpr
            let env2 = applyEnv s1 env
            let s2, tEnd, _ = inferExpr typeDefs env2 endExpr
            let s3 = unify (applyType s2 tStart) TInt span
            let s4 = unify (applyType s3 (applyType s2 tEnd)) TInt span
            let s = compose s4 (compose s3 (compose s2 s1))
            let tRes = TList TInt
            s, tRes, asTyped expr tRes
        | ETuple (items, _) ->
            let mutable sAcc = emptySubst
            let mutable inferred : Type list = []
            for item in items do
                let s1, t1, _ = inferExpr typeDefs (applyEnv sAcc env) item
                sAcc <- compose s1 sAcc
                inferred <- inferred @ [ applyType sAcc t1 ]
            let tRes = TTuple inferred
            sAcc, tRes, asTyped expr tRes
        | ERecord (fields, _) ->
            let mutable sAcc = emptySubst
            let mutable inferred : Map<string, Type> = Map.empty
            for (name, valueExpr) in fields do
                let s1, t1, _ = inferExpr typeDefs (applyEnv sAcc env) valueExpr
                sAcc <- compose s1 sAcc
                inferred <- inferred.Add(name, applyType sAcc t1)
            let tRes = TRecord inferred
            sAcc, tRes, asTyped expr tRes
        | ERecordUpdate (baseExpr, updates, span) ->
            let sBase, tBase, _ = inferExpr typeDefs env baseExpr
            let baseFields =
                match applyType sBase tBase with
                | TRecord fields -> fields
                | _ -> raise (TypeException { Message = "Record update requires a record value"; Span = span })
            let mutable sAcc = sBase
            for (name, valueExpr) in updates do
                let expectedFieldType =
                    match baseFields.TryFind name with
                    | Some t -> t
                    | None -> raise (TypeException { Message = sprintf "Record field '%s' not found" name; Span = Ast.spanOfExpr valueExpr })
                let envForField = applyEnv sAcc env
                let sField, tField, _ = inferExpr typeDefs envForField valueExpr
                let expected = applyType sField (applyType sAcc expectedFieldType)
                let sEq = unify expected tField (Ast.spanOfExpr valueExpr)
                sAcc <- compose sEq (compose sField sAcc)
            let resultFields = baseFields |> Map.map (fun _ t -> applyType sAcc t)
            let tRes = TRecord resultFields
            sAcc, tRes, asTyped expr tRes
        | EFieldGet (target, fieldName, span) ->
            match target with
            | EVar (moduleName, _) when not (env.ContainsKey moduleName) ->
                let qualifiedName = $"{moduleName}.{fieldName}"
                match env.TryFind qualifiedName with
                | Some scheme ->
                    let t = instantiate scheme
                    emptySubst, t, asTyped expr t
                | None ->
                    let s1, tTarget, _ = inferExpr typeDefs env target
                    match applyType s1 tTarget with
                    | TRecord fields ->
                        match fields.TryFind fieldName with
                        | Some tField -> s1, tField, asTyped expr tField
                        | None -> raise (TypeException { Message = sprintf "Record field '%s' not found" fieldName; Span = span })
                    | _ -> raise (TypeException { Message = "Field access requires a record value"; Span = span })
            | _ ->
                let s1, tTarget, _ = inferExpr typeDefs env target
                match applyType s1 tTarget with
                | TRecord fields ->
                    match fields.TryFind fieldName with
                    | Some tField -> s1, tField, asTyped expr tField
                    | None -> raise (TypeException { Message = sprintf "Record field '%s' not found" fieldName; Span = span })
                | _ -> raise (TypeException { Message = "Field access requires a record value"; Span = span })
        | EBinOp (op, a, b, span) ->
            match op with
            | "|>" ->
                let s1, tLeft, _ = inferExpr typeDefs env a
                let env2 = applyEnv s1 env
                let s2, tRight, _ = inferExpr typeDefs env2 b
                let tv = Types.freshVar()
                let s3 = unify (applyType s2 tRight) (TFun (applyType s2 tLeft, tv)) span
                let s = compose s3 (compose s2 s1)
                let tRes = applyType s tv
                s, tRes, asTyped expr tRes
            | "::" ->
                let s1, tHead, _ = inferExpr typeDefs env a
                let env2 = applyEnv s1 env
                let s2, tTail, _ = inferExpr typeDefs env2 b
                let s3 = unify (applyType s2 tTail) (TList tHead) span
                let s = compose s3 (compose s2 s1)
                let tRes = applyType s (TList tHead)
                s, tRes, asTyped expr tRes
            | "@" ->
                let s1, t1, _ = inferExpr typeDefs env a
                let env2 = applyEnv s1 env
                let s2, t2, _ = inferExpr typeDefs env2 b
                let s3 = unify (applyType s2 t1) t2 span
                let s = compose s3 (compose s2 s1)
                let tRes = applyType s t2
                s, tRes, asTyped expr tRes
            | _ ->
                let s1, t1, _ = inferExpr typeDefs env a
                let env2 = applyEnv s1 env
                let s2, t2, _ = inferExpr typeDefs env2 b
                let s3 = unify (applyType s2 t1) t2 span
                let t = applyType s3 t2
                let resultType =
                    match op with
                    | "+" | "-" | "*" | "/" | "%" -> numericResult t span
                    | "=" -> TBool
                    | "<" | ">" | "<=" | ">=" ->
                        let _ = numericResult t span
                        TBool
                    | "&&" | "||" ->
                        let _ = unify t TBool span
                        TBool
                    | _ -> raise (TypeException { Message = sprintf "Unknown operator %s" op; Span = span })
                let s = compose s3 (compose s2 s1)
                s, resultType, asTyped expr resultType
        | ESome (value, _) ->
            let s1, t1, _ = inferExpr typeDefs env value
            s1, TOption t1, asTyped expr (TOption t1)
        | ENone _ ->
            let tv = Types.freshVar()
            emptySubst, TOption tv, asTyped expr (TOption tv)
        | ECons (head, tail, span) ->
            let s1, t1, _ = inferExpr typeDefs env head
            let env2 = applyEnv s1 env
            let s2, t2, _ = inferExpr typeDefs env2 tail
            let s3 = unify (applyType s2 t2) (TList t1) span
            let s = compose s3 (compose s2 s1)
            let tRes = applyType s (TList t1)
            s, tRes, asTyped expr tRes
        | EAppend (a, b, span) ->
            let s1, t1, _ = inferExpr typeDefs env a
            let env2 = applyEnv s1 env
            let s2, t2, _ = inferExpr typeDefs env2 b
            let s3 = unify (applyType s2 t1) t2 span
            let s = compose s3 (compose s2 s1)
            s, t2, asTyped expr t2

    let private envFromExterns (externs: ExternalFunction list) : Map<string, Scheme> =
        let builtins =
            [ "ignore", Forall([ 0 ], TFun (TVar 0, TUnit)) ]
            |> Map.ofList

        externs
        |> List.fold (fun acc ext -> acc.Add(ext.Name, ext.Scheme)) builtins

    let inferProgramWithExterns (externs: ExternalFunction list) (program: Program) : TypedProgram =
        let decls =
            program
            |> List.choose (function | SType d -> Some (d.Name, d) | _ -> None)
            |> Map.ofList
        let typeDefs =
            decls
            |> Map.map (fun _ d ->
                d.Fields
                |> List.map (fun (name, t) -> name, typeFromRef decls [ d.Name ] t)
                |> Map.ofList
                |> TRecord)

        let mutable env : Map<string, Scheme> = envFromExterns externs
        let typed = ResizeArray<TypedStmt>()
        for i, stmt in program |> List.indexed do
            match stmt with
            | SType def ->
                typed.Add(TSType def)
            | SLet(name, args, expr, isRec, span) ->
                let exprVal = Seq.foldBack (fun arg acc -> ELambda(arg, acc, span)) args expr
                if isRec then
                    let tv = Types.freshVar()
                    let envRec = env |> Map.add name (Forall([], tv))
                    let s1, t1, _ = inferExpr typeDefs envRec exprVal
                    let s2 = unify (applyType s1 tv) t1 span
                    let s = compose s2 s1
                    let env' = applyEnv s env
                    let scheme = Types.generalize env' (applyType s t1)
                    env <- env' |> Map.add name scheme
                    typed.Add(TSLet(name, exprVal, applyType s t1, true, span))
                else
                    let s1, t1, _ = inferExpr typeDefs env exprVal
                    let env' = applyEnv s1 env
                    let scheme = Types.generalize env' t1
                    env <- env' |> Map.add name scheme
                    typed.Add(TSLet(name, exprVal, t1, false, span))
            | SExpr expr ->
                let s1, t1, typedExpr = inferExpr typeDefs env expr
                let sDiscard =
                    if i < (program.Length - 1) then
                        unify (applyType s1 t1) TUnit (Ast.spanOfExpr expr)
                    else
                        emptySubst
                let s = compose sDiscard s1
                env <- applyEnv s env
                typed.Add(TSExpr typedExpr)
        typed |> Seq.toList

    let inferProgram (program: Program) : TypedProgram =
        inferProgramWithExterns [] program
