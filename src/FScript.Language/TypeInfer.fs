namespace FScript.Language

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
        | TMap (tk, tv) -> TMap (applyType s tk, applyType s tv)
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

    let private unify (typeDefs: Map<string, Type>) (t1: Type) (t2: Type) (span: Span) : Subst =
        let rec uni (seen: Set<Type * Type>) a b =
            if seen.Contains(a, b) || seen.Contains(b, a) then
                emptySubst
            else
                let seen' = seen.Add(a, b)
                match a, b with
                | TUnit, TUnit
                | TInt, TInt
                | TFloat, TFloat
                | TBool, TBool
                | TString, TString
                | TTypeToken, TTypeToken -> emptySubst
                | TNamed x, TNamed y when x = y -> emptySubst
                | TList x, TList y -> uni seen' x y
                | TTuple xs, TTuple ys ->
                    if xs.Length <> ys.Length then
                        raise (TypeException { Message = "Tuple arity mismatch"; Span = span })
                    List.zip xs ys
                    |> List.fold (fun sAcc (x, y) ->
                        let s1 = uni seen' (applyType sAcc x) (applyType sAcc y)
                        compose s1 sAcc) emptySubst
                | TRecord xf, TRecord yf ->
                    let xKeys = Set.ofSeq xf.Keys
                    let yKeys = Set.ofSeq yf.Keys
                    let fieldsToCheck, otherFields =
                        if Set.isSubset xKeys yKeys then xf, yf
                        elif Set.isSubset yKeys xKeys then yf, xf
                        else
                            raise (TypeException { Message = "Record field set mismatch"; Span = span })

                    fieldsToCheck
                    |> Map.toList
                    |> List.fold (fun sAcc (name, xt) ->
                        let yt = otherFields.[name]
                        let s1 = uni seen' (applyType sAcc xt) (applyType sAcc yt)
                        compose s1 sAcc) emptySubst
                | TMap (kx, vx), TMap (ky, vy) ->
                    let s1 = uni seen' kx ky
                    let s2 = uni seen' (applyType s1 vx) (applyType s1 vy)
                    compose s2 s1
                | TOption x, TOption y -> uni seen' x y
                | TFun (a1, b1), TFun (a2, b2) ->
                    let s1 = uni seen' a1 a2
                    let s2 = uni seen' (applyType s1 b1) (applyType s1 b2)
                    compose s2 s1
                | TVar v, t
                | t, TVar v ->
                    if t = TVar v then emptySubst
                    elif occurs v t then raise (TypeException { Message = "Occurs check failed"; Span = span })
                    else Map.ofList [ v, t ]
                | TNamed name, t ->
                    match typeDefs.TryFind name with
                    | Some expanded when expanded <> TNamed name -> uni seen' expanded t
                    | _ ->
                        match t with
                        | TNamed other when other = name -> emptySubst
                        | _ -> raise (TypeException { Message = sprintf "Type mismatch: %s vs %s" (Types.typeToString a) (Types.typeToString b); Span = span })
                | t, TNamed name ->
                    match typeDefs.TryFind name with
                    | Some expanded when expanded <> TNamed name -> uni seen' t expanded
                    | _ ->
                        match t with
                        | TNamed other when other = name -> emptySubst
                        | _ -> raise (TypeException { Message = sprintf "Type mismatch: %s vs %s" (Types.typeToString a) (Types.typeToString b); Span = span })
                | _ ->
                    raise (TypeException { Message = sprintf "Type mismatch: %s vs %s" (Types.typeToString a) (Types.typeToString b); Span = span })
        uni Set.empty t1 t2

    let private instantiate (Forall (vars, t)) =
        let freshMap = vars |> List.map (fun v -> v, Types.freshVar()) |> Map.ofList
        let rec subst t =
            match t with
            | TVar v -> freshMap |> Map.tryFind v |> Option.defaultValue t
            | TList a -> TList (subst a)
            | TTuple ts -> TTuple (ts |> List.map subst)
            | TRecord fields -> TRecord (fields |> Map.map (fun _ t1 -> subst t1))
            | TMap (k, v) -> TMap (subst k, subst v)
            | TOption a -> TOption (subst a)
            | TFun (a, b) -> TFun (subst a, subst b)
            | _ -> t
        subst t

    type TypedExpr = { Expr: Expr; Type: Type; Span: Span }
    type LocalVariableTypeInfo =
        { Name: string
          Span: Span
          Type: Type }

    type private LocalVariableTypeCapture =
        { Name: string
          Span: Span
          mutable Type: Type }
    type TypedStmt =
        | TSType of TypeDef
        | TSLet of string * Expr * Type * bool * bool * Span
        | TSLetRecGroup of (string * Expr * Type * Span) list * bool * Span
        | TSExpr of TypedExpr
    type TypedProgram = TypedStmt list

    type private InferenceTelemetry =
        { mutable ScopeStack: Set<string> list
          LocalVariableTypes: ResizeArray<LocalVariableTypeCapture> }

    let mutable private telemetry: InferenceTelemetry option = None

    let private beginTelemetry () =
        telemetry <- Some { ScopeStack = [ Set.empty ]; LocalVariableTypes = ResizeArray() }

    let private endTelemetry () =
        let captured : LocalVariableTypeInfo list =
            match telemetry with
            | Some t ->
                t.LocalVariableTypes
                |> Seq.map (fun item ->
                    ({ Name = item.Name
                       Span = item.Span
                       Type = item.Type }: LocalVariableTypeInfo))
                |> Seq.toList
            | None ->
                []

        telemetry <- None
        captured

    let private withLocalBindings (names: string list) (work: unit -> 'T) : 'T =
        match telemetry with
        | None -> work ()
        | Some t ->
            let current =
                match t.ScopeStack with
                | head :: _ -> head
                | [] -> Set.empty

            let pushed =
                names
                |> List.fold (fun acc name ->
                    if System.String.IsNullOrWhiteSpace(name) || name = "_" then acc
                    else Set.add name acc) current

            t.ScopeStack <- pushed :: t.ScopeStack
            try
                work ()
            finally
                match t.ScopeStack with
                | _ :: tail -> t.ScopeStack <- tail
                | [] -> ()

    let private recordLocalVariableUse (name: string) (span: Span) (varType: Type) =
        match telemetry with
        | None -> ()
        | Some t ->
            let current =
                match t.ScopeStack with
                | head :: _ -> head
                | [] -> Set.empty

            if Set.contains name current then
                t.LocalVariableTypes.Add({ Name = name; Span = span; Type = varType })

    let private recordLocalVariableDeclaration (name: string) (span: Span) (varType: Type) =
        if System.String.IsNullOrWhiteSpace(name) || name = "_" then
            ()
        else
            match telemetry with
            | None -> ()
            | Some t ->
                t.LocalVariableTypes.Add({ Name = name; Span = span; Type = varType })

    let private applySubstToCapturedLocals (startIndex: int) (subst: Subst) =
        match telemetry with
        | None -> ()
        | Some t ->
            for i = startIndex to t.LocalVariableTypes.Count - 1 do
                let entry = t.LocalVariableTypes[i]
                entry.Type <- applyType subst entry.Type

    type private ConstructorSig =
        { UnionName: string
          Payload: Type option }

    let private asTyped expr t = { Expr = expr; Type = t; Span = Ast.spanOfExpr expr }

    let private builtinType (name: string) : Type option =
        match name with
        | "unit" -> Some TUnit
        | "int" -> Some TInt
        | "float" -> Some TFloat
        | "bool" -> Some TBool
        | "string" -> Some TString
        | _ -> None

    let private resolveNamedRecordByShape (typeDefs: Map<string, Type>) (shape: Map<string, Type>) (span: Span) : Type =
        let matches =
            typeDefs
            |> Map.toList
            |> List.choose (fun (name, t) ->
                match t with
                | TRecord fields when fields = shape -> Some name
                | _ -> None)

        match matches with
        | [ name ] -> TNamed name
        | [] ->
            let shapeText = Types.typeToString (TRecord shape)
            raise (TypeException { Message = $"No declared record type matches shape {shapeText}"; Span = span })
        | many ->
            let shapeText = Types.typeToString (TRecord shape)
            let names = String.concat ", " many
            raise (TypeException { Message = $"Ambiguous declared record type for shape {shapeText}: {names}"; Span = span })

    let private resolveNamedRecordByFieldSet (typeDefs: Map<string, Type>) (fields: Set<string>) (span: Span) : string =
        let matches =
            typeDefs
            |> Map.toList
            |> List.choose (fun (name, t) ->
                match t with
                | TRecord recordFields when (recordFields |> Map.keys |> Set.ofSeq) = fields -> Some name
                | _ -> None)

        match matches with
        | [ name ] -> name
        | [] ->
            let shapeText =
                fields
                |> Set.toList
                |> String.concat "; "
                |> fun f -> $"{{ {f} }}"
            raise (TypeException { Message = $"No declared record type matches fields {shapeText}"; Span = span })
        | many ->
            let shapeText =
                fields
                |> Set.toList
                |> String.concat "; "
                |> fun f -> $"{{ {f} }}"
            let names = String.concat ", " many
            raise (TypeException { Message = $"Ambiguous declared record type for fields {shapeText}: {names}"; Span = span })

    let private resolveReferencedTypeName (knownNames: seq<string>) (name: string) (span: Span) : string =
        let nameSet = knownNames |> Set.ofSeq
        if nameSet.Contains(name) then
            name
        elif name.Contains(".") then
            let shortName = name.Split('.') |> Array.last
            let matches =
                knownNames
                |> Seq.filter (fun candidate -> candidate = shortName || candidate.EndsWith("." + shortName, System.StringComparison.Ordinal))
                |> Seq.distinct
                |> Seq.toList
            match matches with
            | [ single ] -> single
            | [] -> name
            | many ->
                let options = String.concat ", " many
                raise (TypeException { Message = $"Ambiguous type reference '{name}'. Candidates: {options}"; Span = span })
        else
            name

    let rec private annotationTypeFromRef (typeDefs: Map<string, Type>) (span: Span) (tref: TypeRef) : Type =
        match tref with
        | TRName "unit" -> TUnit
        | TRName "int" -> TInt
        | TRName "float" -> TFloat
        | TRName "bool" -> TBool
        | TRName "string" -> TString
        | TRName name ->
            let resolvedName = resolveReferencedTypeName typeDefs.Keys name span
            TNamed resolvedName
        | TRTuple ts -> ts |> List.map (annotationTypeFromRef typeDefs span) |> TTuple
        | TRFun (a, b) -> TFun(annotationTypeFromRef typeDefs span a, annotationTypeFromRef typeDefs span b)
        | TRPostfix (inner, "list") -> TList (annotationTypeFromRef typeDefs span inner)
        | TRPostfix (inner, "option") -> TOption (annotationTypeFromRef typeDefs span inner)
        | TRPostfix (inner, "map") -> TMap (TString, annotationTypeFromRef typeDefs span inner)
        | TRPostfix (_, suffix) ->
            raise (TypeException { Message = $"Unknown type suffix '{suffix}'"; Span = span })
        | TRRecord fields ->
            let shape =
                fields
                |> List.map (fun (name, t) -> name, annotationTypeFromRef typeDefs span t)
                |> Map.ofList
            resolveNamedRecordByShape typeDefs shape span
        | TRStructuralRecord fields ->
            fields
            |> List.map (fun (name, t) -> name, annotationTypeFromRef typeDefs span t)
            |> Map.ofList
            |> TRecord

    let rec private typeFromRef (decls: Map<string, TypeDef>) (stack: string list) (tref: TypeRef) : Type =
        match tref with
        | TRName name ->
            match builtinType name with
            | Some t -> t
            | None ->
                let resolvedName = resolveReferencedTypeName decls.Keys name unknownSpan
                match decls.TryFind resolvedName with
                | Some def ->
                    match stack |> List.tryFindIndex ((=) resolvedName) with
                    | Some 0 ->
                        if def.IsRecursive then
                            TNamed resolvedName
                        else
                            raise (TypeException { Message = $"Recursive type '{resolvedName}' requires 'type rec'"; Span = unknownSpan })
                    | Some _ ->
                        raise (TypeException { Message = "Mutual recursive types are not supported"; Span = unknownSpan })
                    | None ->
                        if not def.Cases.IsEmpty then
                            TNamed resolvedName
                        else
                            def.Fields
                            |> List.map (fun (field, t) -> field, typeFromRef decls (resolvedName :: stack) t)
                            |> Map.ofList
                            |> TRecord
                | None -> TNamed resolvedName
        | TRTuple ts ->
            ts |> List.map (typeFromRef decls stack) |> TTuple
        | TRFun (a, b) ->
            TFun(typeFromRef decls stack a, typeFromRef decls stack b)
        | TRPostfix (inner, suffix) ->
            let resolved = typeFromRef decls stack inner
            match suffix with
            | "list" -> TList resolved
            | "option" -> TOption resolved
            | "map" -> TMap (TString, resolved)
            | _ -> raise (TypeException { Message = $"Unknown type suffix '{suffix}'"; Span = unknownSpan })
        | TRRecord fields ->
            fields
            |> List.map (fun (name, t) -> name, typeFromRef decls stack t)
            |> Map.ofList
            |> TRecord
        | TRStructuralRecord fields ->
            fields
            |> List.map (fun (name, t) -> name, typeFromRef decls stack t)
            |> Map.ofList
            |> TRecord

    let rec private inferPattern (typeDefs: Map<string, Type>) (constructors: Map<string, ConstructorSig>) (pat: Pattern) : Map<string, Type> * Type =
        match pat with
        | PWildcard _ -> Map.empty, Types.freshVar()
        | PVar (name, _) ->
            let tv = Types.freshVar()
            recordLocalVariableDeclaration name (Ast.spanOfPattern pat) tv
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
            let env1, t1 = inferPattern typeDefs constructors p1
            let env2, t2 = inferPattern typeDefs constructors p2
            let s = unify Map.empty t2 (TList t1) span
            let env = Map.fold (fun acc k v -> Map.add k v acc) env1 env2
            env |> Map.map (fun _ ty -> applyType s ty), applyType s (TList t1)
        | PTuple (ps, _) ->
            let env, types =
                ps
                |> List.fold (fun (envAcc, typesAcc) p ->
                    let envPart, tPart = inferPattern typeDefs constructors p
                    let merged = Map.fold (fun acc k v -> Map.add k v acc) envAcc envPart
                    merged, tPart :: typesAcc) (Map.empty, [])
            env, TTuple (List.rev types)
        | PRecord (fields, _) ->
            let env, fieldTypes =
                fields
                |> List.fold (fun (envAcc, fieldAcc) (name, p) ->
                    let envPart, tPart = inferPattern typeDefs constructors p
                    let merged = Map.fold (fun acc k v -> Map.add k v acc) envAcc envPart
                    merged, Map.add name tPart fieldAcc) (Map.empty, Map.empty)
            env, TRecord fieldTypes
        | PMap (clauses, tailPattern, span) ->
            let ensureSupportedMapKeyType (t: Type) =
                match t with
                | TString
                | TVar _ -> ()
                | _ ->
                    raise (TypeException { Message = $"Map key type must be string, got {Types.typeToString t}"; Span = span })

            let mapKeyType = Types.freshVar()
            let valueType = Types.freshVar()
            let mutable sAcc = emptySubst
            let mutable envAcc : Map<string, Type> = Map.empty

            for (keyPattern, valuePattern) in clauses do
                let keyEnv, keyTypePart = inferPattern typeDefs constructors keyPattern
                let valueEnv, currentValueType = inferPattern typeDefs constructors valuePattern

                let sKey = unify typeDefs (applyType sAcc keyTypePart) (applyType sAcc mapKeyType) span
                sAcc <- compose sKey sAcc
                ensureSupportedMapKeyType (applyType sAcc mapKeyType)

                let sValue = unify typeDefs (applyType sAcc currentValueType) (applyType sAcc valueType) span
                sAcc <- compose sValue sAcc

                let mergedClauseEnv =
                    keyEnv
                    |> Map.fold (fun acc k v -> Map.add k v acc) valueEnv
                    |> Map.map (fun _ t -> applyType sAcc t)

                envAcc <- mergedClauseEnv |> Map.fold (fun acc k v -> Map.add k v acc) envAcc

            match tailPattern with
            | Some tail ->
                let tailEnv, tailType = inferPattern typeDefs constructors tail
                let expectedTailType = TMap (applyType sAcc mapKeyType, applyType sAcc valueType)
                let sTail = unify typeDefs (applyType sAcc tailType) expectedTailType span
                sAcc <- compose sTail sAcc
                envAcc <- tailEnv |> Map.map (fun _ t -> applyType sAcc t) |> Map.fold (fun acc k v -> Map.add k v acc) envAcc
            | None -> ()

            ensureSupportedMapKeyType (applyType sAcc mapKeyType)
            envAcc |> Map.map (fun _ t -> applyType sAcc t), TMap (applyType sAcc mapKeyType, applyType sAcc valueType)
        | PSome (p, _) ->
            let env, t = inferPattern typeDefs constructors p
            env, TOption t
        | PNone _ ->
            let tv = Types.freshVar()
            Map.empty, TOption tv
        | PUnionCase (qualifier, caseName, payload, span) ->
            let constructorName =
                match qualifier with
                | Some typeName -> $"{typeName}.{caseName}"
                | None -> caseName
            match constructors.TryFind constructorName with
            | None ->
                raise (TypeException { Message = sprintf "Unknown union case '%s'" constructorName; Span = span })
            | Some sigInfo ->
                let unionType = TNamed sigInfo.UnionName
                match sigInfo.Payload, payload with
                | None, None -> Map.empty, unionType
                | Some expectedPayload, Some payloadPattern ->
                    let envP, tP = inferPattern typeDefs constructors payloadPattern
                    let s = unify typeDefs tP expectedPayload span
                    envP |> Map.map (fun _ t -> applyType s t), unionType
                | None, Some _ ->
                    raise (TypeException { Message = sprintf "Union case '%s' does not take a payload" constructorName; Span = span })
                | Some _, None ->
                    raise (TypeException { Message = sprintf "Union case '%s' requires a payload" constructorName; Span = span })
        | PTypeRef (tref, span) ->
            Map.empty, annotationTypeFromRef typeDefs span tref

    let private numericResult (t: Type) (span: Span) =
        match t with
        | TInt | TFloat -> t
        | TVar _ -> TInt
        | _ -> raise (TypeException { Message = "Expected numeric type"; Span = span })

    let private validateMapKeyTypes (span: Span) (t: Type) : unit =
        let rec loop t =
            match t with
            | TMap (keyType, valueType) ->
                loop keyType
                loop valueType
                match keyType with
                | TString
                | TVar _ -> ()
                | _ ->
                    raise (TypeException { Message = $"Map key type must be string, got {Types.typeToString keyType}"; Span = span })
            | TList inner -> loop inner
            | TTuple items -> items |> List.iter loop
            | TRecord fields -> fields |> Map.values |> Seq.iter loop
            | TOption inner -> loop inner
            | TFun (a, b) -> loop a; loop b
            | _ -> ()
        loop t

    let rec private inferExpr (typeDefs: Map<string, Type>) (constructors: Map<string, ConstructorSig>) (env: Map<string, Scheme>) (expr: Expr) : Subst * Type * TypedExpr =
        match expr with
        | EUnit _ ->
            emptySubst, TUnit, asTyped expr TUnit
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
                recordLocalVariableUse name span t
                emptySubst, t, asTyped expr t
            | None -> raise (TypeException { Message = sprintf "Unbound variable '%s'" name; Span = span })
        | EParen (inner, _) ->
            let s, t, _ = inferExpr typeDefs constructors env inner
            s, t, asTyped expr t
        | ETypeOf (name, span) ->
            if typeDefs.ContainsKey name then
                emptySubst, TTypeToken, asTyped expr TTypeToken
            else
                raise (TypeException { Message = sprintf "Unknown type '%s'" name; Span = span })
        | ENameOf (name, span) ->
            if env.ContainsKey name then
                emptySubst, TString, asTyped expr TString
            else
                raise (TypeException { Message = sprintf "Unbound variable '%s'" name; Span = span })
        | EInterpolatedString (parts, span) ->
            let mutable sAcc = emptySubst
            for part in parts do
                match part with
                | IPText _ -> ()
                | IPExpr pexpr ->
                    let s1, _, _ = inferExpr typeDefs constructors (applyEnv sAcc env) pexpr
                    sAcc <- compose s1 sAcc
            sAcc, TString, asTyped expr TString
        | ELambda (param, body, _) ->
            let tv =
                match param.Annotation with
                | Some tref -> annotationTypeFromRef typeDefs param.Span tref
                | None -> Types.freshVar()
            recordLocalVariableDeclaration param.Name param.Span tv
            let env' = env |> Map.add param.Name (Forall([], tv))
            let s1, tBody, _ =
                withLocalBindings [ param.Name ] (fun () ->
                    inferExpr typeDefs constructors env' body)
            let tArg = applyType s1 tv
            let tFun = TFun (tArg, tBody)
            s1, tFun, asTyped expr tFun
        | EApply (fn, arg, span) ->
            let s1, t1, _ = inferExpr typeDefs constructors env fn
            let env2 = applyEnv s1 env
            let s2, t2, _ = inferExpr typeDefs constructors env2 arg
            let tv = Types.freshVar()
            let s3 = unify typeDefs (applyType s2 t1) (TFun (t2, tv)) span
            let s = compose s3 (compose s2 s1)
            let tRes = applyType s tv
            s, tRes, asTyped expr tRes
        | EIf (cond, tExpr, fExpr, span) ->
            let s1, tCond, _ = inferExpr typeDefs constructors env cond
            let sBool = unify typeDefs tCond TBool span
            let env2 = applyEnv (compose sBool s1) env
            let s2, tThen, _ = inferExpr typeDefs constructors env2 tExpr
            let env3 = applyEnv (compose s2 (compose sBool s1)) env
            let s3, tElse, _ = inferExpr typeDefs constructors env3 fExpr
            let s4 = unify typeDefs (applyType s3 tThen) tElse span
            let s = compose s4 (compose s3 (compose s2 (compose sBool s1)))
            let tRes = applyType s tElse
            s, tRes, asTyped expr tRes
        | ERaise (value, span) ->
            let s1, t1, _ = inferExpr typeDefs constructors env value
            let s2 = unify typeDefs (applyType s1 t1) TString span
            let s = compose s2 s1
            let tRes = Types.freshVar()
            s, tRes, asTyped expr tRes
        | EFor (name, source, body, span) ->
            let s1, tSource, _ = inferExpr typeDefs constructors env source
            let tv = Types.freshVar()
            let sList = unify typeDefs (applyType s1 tSource) (TList tv) span
            let sSource = compose sList s1
            let itemType = applyType sSource tv
            let envBody =
                applyEnv sSource env
                |> Map.add name (Forall([], itemType))
            let s2, tBody, _ =
                withLocalBindings [ name ] (fun () ->
                    inferExpr typeDefs constructors envBody body)
            let sBodyUnit = unify typeDefs (applyType s2 tBody) TUnit (Ast.spanOfExpr body)
            let s = compose sBodyUnit (compose s2 sSource)
            s, TUnit, asTyped expr TUnit
        | ELet (name, value, body, isRec, span) ->
            if isRec then
                let tv = Types.freshVar()
                let envRec = env |> Map.add name (Forall([], tv))
                let s1, t1, _ =
                    withLocalBindings [ name ] (fun () ->
                        inferExpr typeDefs constructors envRec value)
                let s2 = unify typeDefs (applyType s1 tv) t1 span
                let sValue = compose s2 s1
                let env1 = applyEnv sValue env
                let scheme = Types.generalize env1 (applyType sValue t1)
                let env2 = env1 |> Map.add name scheme
                let s3, tBody, _ =
                    withLocalBindings [ name ] (fun () ->
                        inferExpr typeDefs constructors env2 body)
                let s = compose s3 sValue
                s, tBody, asTyped expr tBody
            else
                let s1, t1, _ = inferExpr typeDefs constructors env value
                let sDiscard =
                    if name = "_" then
                        unify typeDefs (applyType s1 t1) TUnit (Ast.spanOfExpr value)
                    else
                        emptySubst
                let sValue = compose sDiscard s1
                let env1 = applyEnv sValue env
                let scheme = Types.generalize env1 (applyType sValue t1)
                let env2 = env1 |> Map.add name scheme
                let s2, t2, _ =
                    withLocalBindings [ name ] (fun () ->
                        inferExpr typeDefs constructors env2 body)
                let s = compose s2 sValue
                s, t2, asTyped expr t2
        | ELetRecGroup (bindings, body, span) ->
            let foldedBindings =
                bindings
                |> List.map (fun (name, args, valueExpr, bindingSpan) ->
                    if args.IsEmpty then
                        raise (TypeException { Message = "'let rec ... and ...' requires function arguments for each binding"; Span = bindingSpan })
                    let folded = Seq.foldBack (fun arg acc -> ELambda(arg, acc, bindingSpan)) args valueExpr
                    name, folded, bindingSpan)

            let freshByName =
                foldedBindings
                |> List.map (fun (name, _, _) -> name, Types.freshVar())
                |> Map.ofList

            let envRec =
                freshByName
                |> Map.fold (fun acc name tv -> Map.add name (Forall([], tv)) acc) env

            let mutable sRec = emptySubst
            let recursiveNames = freshByName |> Map.toList |> List.map fst
            withLocalBindings recursiveNames (fun () ->
                for (name, exprVal, bindingSpan) in foldedBindings do
                    let envForBinding = applyEnv sRec envRec
                    let s1, t1, _ = inferExpr typeDefs constructors envForBinding exprVal
                    let expected = applyType s1 (applyType sRec freshByName.[name])
                    let s2 = unify typeDefs expected t1 bindingSpan
                    sRec <- compose s2 (compose s1 sRec))

            let envGeneralize = applyEnv sRec env
            let schemes =
                foldedBindings
                |> List.map (fun (name, _, _) ->
                    let inferred = applyType sRec freshByName.[name]
                    name, Types.generalize envGeneralize inferred)

            let envBody =
                schemes
                |> List.fold (fun acc (name, scheme) -> Map.add name scheme acc) envGeneralize

            let sBody, tBody, _ =
                withLocalBindings recursiveNames (fun () ->
                    inferExpr typeDefs constructors envBody body)
            let s = compose sBody sRec
            s, tBody, asTyped expr tBody
        | EMatch (scrutinee, cases, span) ->
            let s1, tScrut, _ = inferExpr typeDefs constructors env scrutinee
            let mutable sAcc = s1
            let mutable resultTypeOpt : Type option = None
            for (pat, guard, body, caseSpan) in cases do
                let envPat, tPat = inferPattern typeDefs constructors pat
                let sPat =
                    match pat, applyType sAcc tScrut, tPat with
                    | PRecord _, TRecord scrutFields, TRecord patFields ->
                        patFields
                        |> Map.fold (fun sLocal field patTy ->
                            match scrutFields.TryFind field with
                            | Some scrutTy ->
                                let sField = unify typeDefs (applyType sLocal scrutTy) (applyType sLocal patTy) caseSpan
                                compose sField sLocal
                            | None ->
                                raise (TypeException { Message = sprintf "Record field '%s' not found" field; Span = caseSpan })) emptySubst
                    | PRecord _, TNamed typeName, TRecord patFields ->
                        match typeDefs.TryFind typeName with
                        | Some (TRecord scrutFields) ->
                            patFields
                            |> Map.fold (fun sLocal field patTy ->
                                match scrutFields.TryFind field with
                                | Some scrutTy ->
                                    let sField = unify typeDefs (applyType sLocal scrutTy) (applyType sLocal patTy) caseSpan
                                    compose sField sLocal
                                | None ->
                                    raise (TypeException { Message = sprintf "Record field '%s' not found" field; Span = caseSpan })) emptySubst
                        | _ ->
                            raise (TypeException { Message = "Record pattern requires a concrete record scrutinee type"; Span = caseSpan })
                    | PRecord _, _, _ ->
                        raise (TypeException { Message = "Record pattern requires a concrete record scrutinee type"; Span = caseSpan })
                    | _ ->
                        unify typeDefs (applyType sAcc tScrut) tPat caseSpan
                let envCase = applyEnv (compose sPat sAcc) env
                let envCase' = envPat |> Map.fold (fun acc k v -> Map.add k (Forall([], applyType sPat v)) acc) envCase
                let caseBoundNames = envPat |> Map.toList |> List.map fst
                let sBody, tBody, _ =
                    withLocalBindings caseBoundNames (fun () ->
                        let mutable envBodyCase = envCase'
                        match guard with
                        | Some guardExpr ->
                            let sGuard, tGuard, _ = inferExpr typeDefs constructors envBodyCase guardExpr
                            let sGuardBool = unify typeDefs (applyType sGuard tGuard) TBool caseSpan
                            sAcc <- compose sGuardBool (compose sGuard sAcc)
                            envBodyCase <- applyEnv (compose sGuardBool sGuard) envBodyCase
                        | None -> ()
                        inferExpr typeDefs constructors envBodyCase body)
                let tBody' = applyType sBody tBody
                resultTypeOpt <-
                    match resultTypeOpt with
                    | None -> Some tBody'
                    | Some tPrev ->
                        let sEq = unify typeDefs tPrev tBody' caseSpan
                        sAcc <- compose sEq sAcc
                        Some (applyType sEq tPrev)
                sAcc <- compose sBody (compose sPat sAcc)
            let tRes = resultTypeOpt |> Option.defaultValue (Types.freshVar())
            sAcc, tRes, asTyped expr tRes
        | EList (items, span) ->
            let tv = Types.freshVar()
            let mutable sAcc = emptySubst
            for item in items do
                let s1, t1, _ = inferExpr typeDefs constructors (applyEnv sAcc env) item
                let s2 = unify typeDefs (applyType s1 tv) t1 span
                sAcc <- compose s2 (compose s1 sAcc)
            let tRes = TList (applyType sAcc tv)
            sAcc, tRes, asTyped expr tRes
        | ERange (startExpr, endExpr, span) ->
            let s1, tStart, _ = inferExpr typeDefs constructors env startExpr
            let env2 = applyEnv s1 env
            let s2, tEnd, _ = inferExpr typeDefs constructors env2 endExpr
            let s3 = unify typeDefs (applyType s2 tStart) TInt span
            let s4 = unify typeDefs (applyType s3 (applyType s2 tEnd)) TInt span
            let s = compose s4 (compose s3 (compose s2 s1))
            let tRes = TList TInt
            s, tRes, asTyped expr tRes
        | ETuple (items, _) ->
            let mutable sAcc = emptySubst
            let mutable inferred : Type list = []
            for item in items do
                let s1, t1, _ = inferExpr typeDefs constructors (applyEnv sAcc env) item
                sAcc <- compose s1 sAcc
                inferred <- inferred @ [ applyType sAcc t1 ]
            let tRes = TTuple inferred
            sAcc, tRes, asTyped expr tRes
        | ERecord (fields, span) ->
            let mutable sAcc = emptySubst
            let mutable inferred : Map<string, Type> = Map.empty
            for (name, valueExpr) in fields do
                let s1, t1, _ = inferExpr typeDefs constructors (applyEnv sAcc env) valueExpr
                sAcc <- compose s1 sAcc
                inferred <- inferred.Add(name, applyType sAcc t1)
            let recordTypeName = resolveNamedRecordByFieldSet typeDefs (inferred |> Map.keys |> Set.ofSeq) span
            let expected = TNamed recordTypeName
            let sRecord = unify typeDefs (applyType sAcc (TRecord inferred)) expected span
            let s = compose sRecord sAcc
            let tRes = applyType s expected
            s, tRes, asTyped expr tRes
        | EStructuralRecord (fields, _) ->
            let mutable sAcc = emptySubst
            let mutable inferred : Map<string, Type> = Map.empty
            for (name, valueExpr) in fields do
                let s1, t1, _ = inferExpr typeDefs constructors (applyEnv sAcc env) valueExpr
                sAcc <- compose s1 sAcc
                inferred <- inferred.Add(name, applyType sAcc t1)
            let tRes = TRecord inferred
            sAcc, tRes, asTyped expr tRes
        | EMap (entries, span) ->
            let ensureSupportedMapKeyType (t: Type) =
                match t with
                | TString
                | TVar _ -> ()
                | _ ->
                    raise (TypeException { Message = $"Map key type must be string, got {Types.typeToString t}"; Span = span })

            let keyType = Types.freshVar()
            let valueType = Types.freshVar()
            let mutable sAcc = emptySubst
            for entry in entries do
                match entry with
                | MEKeyValue (keyExpr, valueExpr) ->
                    let envForKey = applyEnv sAcc env
                    let sKey, tKey, _ = inferExpr typeDefs constructors envForKey keyExpr
                    let sKeyType = unify typeDefs (applyType sKey tKey) (applyType sKey (applyType sAcc keyType)) (Ast.spanOfExpr keyExpr)
                    let sAfterKey = compose sKeyType (compose sKey sAcc)
                    ensureSupportedMapKeyType (applyType sAfterKey keyType)

                    let envForValue = applyEnv sAfterKey env
                    let sValue, tValue, _ = inferExpr typeDefs constructors envForValue valueExpr
                    let expectedValueType = applyType sValue (applyType sAfterKey valueType)
                    let sValueType = unify typeDefs expectedValueType tValue (Ast.spanOfExpr valueExpr)

                    sAcc <- compose sValueType (compose sValue sAfterKey)
                | MESpread spreadExpr ->
                    let envForSpread = applyEnv sAcc env
                    let sSpread, tSpread, _ = inferExpr typeDefs constructors envForSpread spreadExpr
                    let expectedSpreadType = TMap (applyType sSpread (applyType sAcc keyType), applyType sSpread (applyType sAcc valueType))
                    let sSpreadMap = unify typeDefs (applyType sSpread tSpread) expectedSpreadType (Ast.spanOfExpr spreadExpr)
                    sAcc <- compose sSpreadMap (compose sSpread sAcc)

            ensureSupportedMapKeyType (applyType sAcc keyType)
            let tRes = TMap (applyType sAcc keyType, applyType sAcc valueType)
            sAcc, tRes, asTyped expr tRes
        | ERecordUpdate (baseExpr, updates, span) ->
            let sBase, tBase, _ = inferExpr typeDefs constructors env baseExpr
            let baseFields =
                match applyType sBase tBase with
                | TRecord fields -> fields
                | TNamed name ->
                    match typeDefs.TryFind name with
                    | Some (TRecord fields) -> fields
                    | _ -> raise (TypeException { Message = "Record update requires a record value"; Span = span })
                | _ -> raise (TypeException { Message = "Record update requires a record value"; Span = span })
            let mutable sAcc = sBase
            for (name, valueExpr) in updates do
                let expectedFieldType =
                    match baseFields.TryFind name with
                    | Some t -> t
                    | None -> raise (TypeException { Message = sprintf "Record field '%s' not found" name; Span = Ast.spanOfExpr valueExpr })
                let envForField = applyEnv sAcc env
                let sField, tField, _ = inferExpr typeDefs constructors envForField valueExpr
                let expected = applyType sField (applyType sAcc expectedFieldType)
                let sEq = unify typeDefs expected tField (Ast.spanOfExpr valueExpr)
                sAcc <- compose sEq (compose sField sAcc)
            let resultFields = baseFields |> Map.map (fun _ t -> applyType sAcc t)
            let tRes = TRecord resultFields
            sAcc, tRes, asTyped expr tRes
        | EStructuralRecordUpdate (baseExpr, updates, span) ->
            let sBase, tBase, _ = inferExpr typeDefs constructors env baseExpr
            let baseFields =
                match applyType sBase tBase with
                | TRecord fields -> fields
                | TNamed name ->
                    match typeDefs.TryFind name with
                    | Some (TRecord fields) -> fields
                    | _ -> raise (TypeException { Message = "Structural record update requires a record value"; Span = span })
                | _ -> raise (TypeException { Message = "Structural record update requires a record value"; Span = span })
            let mutable sAcc = sBase
            let mutable resultFields = baseFields
            for (name, valueExpr) in updates do
                let envForField = applyEnv sAcc env
                let sField, tField, _ = inferExpr typeDefs constructors envForField valueExpr
                let tFieldApplied = applyType sField tField
                let expected =
                    match resultFields.TryFind name with
                    | Some existingFieldType -> applyType sField (applyType sAcc existingFieldType)
                    | None -> tFieldApplied
                let sEq = unify typeDefs expected tFieldApplied (Ast.spanOfExpr valueExpr)
                sAcc <- compose sEq (compose sField sAcc)
                resultFields <- resultFields |> Map.add name (applyType sAcc tFieldApplied)
            let tRes = TRecord (resultFields |> Map.map (fun _ t -> applyType sAcc t))
            sAcc, tRes, asTyped expr tRes
        | EFieldGet (target, fieldName, span) ->
            let inferRecordField s1 tTarget =
                match applyType s1 tTarget with
                | TRecord fields ->
                    match fields.TryFind fieldName with
                    | Some tField -> s1, tField
                    | None -> raise (TypeException { Message = sprintf "Record field '%s' not found" fieldName; Span = span })
                | TNamed name ->
                    match typeDefs.TryFind name with
                    | Some (TRecord fields) ->
                        match fields.TryFind fieldName with
                        | Some tField -> s1, tField
                        | None -> raise (TypeException { Message = sprintf "Record field '%s' not found" fieldName; Span = span })
                    | _ -> raise (TypeException { Message = "Field access requires a record value"; Span = span })
                | TVar _ as tv ->
                    let tField = Types.freshVar()
                    let sField = unify typeDefs tv (TRecord (Map.ofList [ fieldName, tField ])) span
                    let s = compose sField s1
                    s, applyType s tField
                | _ -> raise (TypeException { Message = "Field access requires a record value"; Span = span })

            match target with
            | EVar (moduleName, _) when not (env.ContainsKey moduleName) ->
                let qualifiedName = $"{moduleName}.{fieldName}"
                match env.TryFind qualifiedName with
                | Some scheme ->
                    let t = instantiate scheme
                    emptySubst, t, asTyped expr t
                | None ->
                    let s1, tTarget, _ = inferExpr typeDefs constructors env target
                    let sField, tField = inferRecordField s1 tTarget
                    sField, tField, asTyped expr tField
            | _ ->
                let s1, tTarget, _ = inferExpr typeDefs constructors env target
                let sField, tField = inferRecordField s1 tTarget
                sField, tField, asTyped expr tField
        | EIndexGet (target, keyExpr, span) ->
            let s1, tTarget, _ = inferExpr typeDefs constructors env target
            let env2 = applyEnv s1 env
            let s2, tKey, _ = inferExpr typeDefs constructors env2 keyExpr
            let keyType = Types.freshVar()
            let s3 = unify typeDefs (applyType s2 tKey) keyType span
            let valueType = Types.freshVar()
            let s4 = unify typeDefs (applyType s3 (applyType s2 tTarget)) (TMap (applyType s3 keyType, valueType)) span
            let s = compose s4 (compose s3 (compose s2 s1))
            match applyType s keyType with
            | TString
            | TVar _ -> ()
            | t ->
                raise (TypeException { Message = $"Map key type must be string, got {Types.typeToString t}"; Span = span })
            let tRes = TOption (applyType s valueType)
            s, tRes, asTyped expr tRes
        | EBinOp (op, a, b, span) ->
            match op with
            | "|>" ->
                let s1, tLeft, _ = inferExpr typeDefs constructors env a
                let env2 = applyEnv s1 env
                let s2, tRight, _ = inferExpr typeDefs constructors env2 b
                let tv = Types.freshVar()
                let s3 = unify typeDefs (applyType s2 tRight) (TFun (applyType s2 tLeft, tv)) span
                let s = compose s3 (compose s2 s1)
                let tRes = applyType s tv
                s, tRes, asTyped expr tRes
            | "::" ->
                let s1, tHead, _ = inferExpr typeDefs constructors env a
                let env2 = applyEnv s1 env
                let s2, tTail, _ = inferExpr typeDefs constructors env2 b
                let s3 = unify typeDefs (applyType s2 tTail) (TList tHead) span
                let s = compose s3 (compose s2 s1)
                let tRes = applyType s (TList tHead)
                s, tRes, asTyped expr tRes
            | "@" ->
                let s1, t1, _ = inferExpr typeDefs constructors env a
                let env2 = applyEnv s1 env
                let s2, t2, _ = inferExpr typeDefs constructors env2 b
                let tv = Types.freshVar()
                let s3 = unify typeDefs (applyType s2 t1) (TList tv) span
                let s4 = unify typeDefs (applyType s3 (applyType s2 t2)) (TList (applyType s3 tv)) span
                let s = compose s4 (compose s3 (compose s2 s1))
                let tRes = TList (applyType s tv)
                s, tRes, asTyped expr tRes
            | _ ->
                let s1, t1, _ = inferExpr typeDefs constructors env a
                let env2 = applyEnv s1 env
                let s2, t2, _ = inferExpr typeDefs constructors env2 b
                let s3 = unify typeDefs (applyType s2 t1) t2 span
                let t = applyType s3 t2
                let resultType =
                    match op with
                    | "+" | "-" | "*" | "/" | "%" -> numericResult t span
                    | "=" -> TBool
                    | "<" | ">" | "<=" | ">=" ->
                        let _ = numericResult t span
                        TBool
                    | "&&" | "||" ->
                        let _ = unify typeDefs t TBool span
                        TBool
                    | _ -> raise (TypeException { Message = sprintf "Unknown operator %s" op; Span = span })
                let s = compose s3 (compose s2 s1)
                s, resultType, asTyped expr resultType
        | ESome (value, _) ->
            let s1, t1, _ = inferExpr typeDefs constructors env value
            s1, TOption t1, asTyped expr (TOption t1)
        | ENone _ ->
            let tv = Types.freshVar()
            emptySubst, TOption tv, asTyped expr (TOption tv)
        | ECons (head, tail, span) ->
            let s1, t1, _ = inferExpr typeDefs constructors env head
            let env2 = applyEnv s1 env
            let s2, t2, _ = inferExpr typeDefs constructors env2 tail
            let s3 = unify typeDefs (applyType s2 t2) (TList t1) span
            let s = compose s3 (compose s2 s1)
            let tRes = applyType s (TList t1)
            s, tRes, asTyped expr tRes
        | EAppend (a, b, span) ->
            let s1, t1, _ = inferExpr typeDefs constructors env a
            let env2 = applyEnv s1 env
            let s2, t2, _ = inferExpr typeDefs constructors env2 b
            let tv = Types.freshVar()
            let s3 = unify typeDefs (applyType s2 t1) (TList tv) span
            let s4 = unify typeDefs (applyType s3 (applyType s2 t2)) (TList (applyType s3 tv)) span
            let s = compose s4 (compose s3 (compose s2 s1))
            let tRes = TList (applyType s tv)
            s, tRes, asTyped expr tRes

    let private envFromExterns (externs: ExternalFunction list) : Map<string, Scheme> =
        let builtins =
            [ "ignore", Forall([ 0 ], TFun (TVar 0, TUnit))
              "print", Forall([], TFun (TString, TUnit)) ]
            |> Map.ofList

        externs
        |> List.fold (fun acc ext -> acc.Add(ext.Name, ext.Scheme)) builtins

    let private topLevelBindingNames (program: Program) : string list =
        program
        |> List.collect (function
            | SLet(name, _, _, _, _, _) -> [ name ]
            | SLetRecGroup(bindings, _, _) -> bindings |> List.map (fun (name, _, _, _) -> name)
            | _ -> [])

    let private inferProgramWithExternsRawAndLocals (externs: ExternalFunction list) (program: Program) : TypedProgram * LocalVariableTypeInfo list =
        let decls =
            program
            |> List.choose (function | SType d -> Some (d.Name, d) | _ -> None)
            |> Map.ofList
        let typeDefs =
            decls
            |> Map.map (fun name d ->
                if not d.Cases.IsEmpty then
                    let cases =
                        d.Cases
                        |> List.map (fun (caseName, payload) ->
                            caseName, payload |> Option.map (typeFromRef decls [ name ]))
                        |> Map.ofList
                    TUnion(name, cases)
                else
                    d.Fields
                    |> List.map (fun (fieldName, t) -> fieldName, typeFromRef decls [ d.Name ] t)
                    |> Map.ofList
                    |> TRecord)

        let constructors =
            decls
            |> Map.toList
            |> List.collect (fun (typeName, def) ->
                def.Cases
                |> List.collect (fun (caseName, payload) ->
                    let payloadType = payload |> Option.map (typeFromRef decls [ typeName ])
                    let sigInfo = { UnionName = typeName; Payload = payloadType }
                    [ caseName, sigInfo
                      $"{typeName}.{caseName}", sigInfo ]))
            |> Map.ofList

        let constructorSchemes =
            constructors
            |> Map.toList
            |> List.map (fun (caseName, sigInfo) ->
                let unionType = TNamed sigInfo.UnionName
                let schemeType =
                    match sigInfo.Payload with
                    | None -> unionType
                    | Some payloadType -> TFun(payloadType, unionType)
                caseName, Forall([], schemeType))

        let mutable env : Map<string, Scheme> =
            (envFromExterns externs, constructorSchemes)
            ||> List.fold (fun acc (name, scheme) -> acc.Add(name, scheme))

        beginTelemetry ()
        try
            let typed = ResizeArray<TypedStmt>()
            for i, stmt in program |> List.indexed do
                match stmt with
                | SType def ->
                    typed.Add(TSType def)
                | SImport (_, span) ->
                    raise (TypeException { Message = "'import' must be resolved before type inference"; Span = span })
                | SLet(name, args, expr, isRec, isExported, span) ->
                    let exprVal = Seq.foldBack (fun arg acc -> ELambda(arg, acc, span)) args expr
                    let startIndex =
                        match telemetry with
                        | Some t -> t.LocalVariableTypes.Count
                        | None -> 0

                    if isRec then
                        let tv = Types.freshVar()
                        let envRec = env |> Map.add name (Forall([], tv))
                        let s1, t1, _ = inferExpr typeDefs constructors envRec exprVal
                        let s2 = unify typeDefs (applyType s1 tv) t1 span
                        let s = compose s2 s1
                        applySubstToCapturedLocals startIndex s
                        let env' = applyEnv s env
                        let inferred = applyType s t1
                        validateMapKeyTypes span inferred
                        let scheme = Types.generalize env' inferred
                        env <- env' |> Map.add name scheme
                        typed.Add(TSLet(name, exprVal, inferred, true, isExported, span))
                    else
                        let s1, t1, _ = inferExpr typeDefs constructors env exprVal
                        applySubstToCapturedLocals startIndex s1
                        let env' = applyEnv s1 env
                        let inferred = applyType s1 t1
                        validateMapKeyTypes span inferred
                        let scheme = Types.generalize env' inferred
                        env <- env' |> Map.add name scheme
                        typed.Add(TSLet(name, exprVal, inferred, false, isExported, span))
                | SLetRecGroup(bindings, isExported, span) ->
                    let startIndex =
                        match telemetry with
                        | Some t -> t.LocalVariableTypes.Count
                        | None -> 0

                    let foldedBindings =
                        bindings
                        |> List.map (fun (name, args, valueExpr, bindingSpan) ->
                            if args.IsEmpty then
                                raise (TypeException { Message = "'let rec ... and ...' requires function arguments for each binding"; Span = bindingSpan })
                            let folded = Seq.foldBack (fun arg acc -> ELambda(arg, acc, bindingSpan)) args valueExpr
                            name, folded, bindingSpan)

                    let freshByName =
                        foldedBindings
                        |> List.map (fun (name, _, _) -> name, Types.freshVar())
                        |> Map.ofList

                    let envRec =
                        freshByName
                        |> Map.fold (fun acc name tv -> Map.add name (Forall([], tv)) acc) env

                    let mutable sRec = emptySubst
                    for (name, exprVal, bindingSpan) in foldedBindings do
                        let envForBinding = applyEnv sRec envRec
                        let s1, t1, _ = inferExpr typeDefs constructors envForBinding exprVal
                        let expected = applyType s1 (applyType sRec freshByName.[name])
                        let s2 = unify typeDefs expected t1 bindingSpan
                        sRec <- compose s2 (compose s1 sRec)

                    applySubstToCapturedLocals startIndex sRec

                    let envGeneralize = applyEnv sRec env
                    let schemes =
                        foldedBindings
                        |> List.map (fun (name, _, _) ->
                            let inferred = applyType sRec freshByName.[name]
                            validateMapKeyTypes span inferred
                            name, Types.generalize envGeneralize inferred)

                    let typedBindings =
                        foldedBindings
                        |> List.map (fun (name, exprVal, bindingSpan) ->
                            name, exprVal, applyType sRec freshByName.[name], bindingSpan)

                    env <-
                        schemes
                        |> List.fold (fun acc (name, scheme) -> Map.add name scheme acc) envGeneralize
                    typed.Add(TSLetRecGroup(typedBindings, isExported, span))
                | SExpr expr ->
                    let startIndex =
                        match telemetry with
                        | Some t -> t.LocalVariableTypes.Count
                        | None -> 0

                    let s1, t1, typedExpr = inferExpr typeDefs constructors env expr
                    let sDiscard =
                        if i < (program.Length - 1) then
                            unify typeDefs (applyType s1 t1) TUnit (Ast.spanOfExpr expr)
                        else
                            emptySubst
                    let s = compose sDiscard s1
                    applySubstToCapturedLocals startIndex s
                    validateMapKeyTypes (Ast.spanOfExpr expr) (applyType s t1)
                    env <- applyEnv s env
                    typed.Add(TSExpr typedExpr)

            typed |> Seq.toList, endTelemetry ()
        with _ ->
            let _ = endTelemetry ()
            reraise ()

    let inferProgramWithExternsRaw (externs: ExternalFunction list) (program: Program) : TypedProgram =
        inferProgramWithExternsRawAndLocals externs program |> fst

    let inferProgramWithExterns (externs: ExternalFunction list) (program: Program) : TypedProgram =
        let stdlibProgram = Stdlib.loadProgram ()
        let reserved = Stdlib.reservedNames ()

        externs
        |> List.tryFind (fun ext -> Set.contains ext.Name reserved)
        |> Option.iter (fun ext ->
            raise (TypeException { Message = $"Host extern '{ext.Name}' collides with reserved stdlib symbol"; Span = unknownSpan }))

        topLevelBindingNames program
        |> List.tryFind (fun name -> Set.contains name reserved)
        |> Option.iter (fun name ->
            raise (TypeException { Message = $"Top-level binding '{name}' collides with reserved stdlib symbol"; Span = unknownSpan }))

        let typedCombined = inferProgramWithExternsRaw externs (stdlibProgram @ program)
        typedCombined |> List.skip (List.length stdlibProgram)

    let inferProgramWithExternsAndLocalVariableTypes (externs: ExternalFunction list) (program: Program) : TypedProgram * LocalVariableTypeInfo list =
        let stdlibProgram = Stdlib.loadProgram ()
        let reserved = Stdlib.reservedNames ()

        externs
        |> List.tryFind (fun ext -> Set.contains ext.Name reserved)
        |> Option.iter (fun ext ->
            raise (TypeException { Message = $"Host extern '{ext.Name}' collides with reserved stdlib symbol"; Span = unknownSpan }))

        topLevelBindingNames program
        |> List.tryFind (fun name -> Set.contains name reserved)
        |> Option.iter (fun name ->
            raise (TypeException { Message = $"Top-level binding '{name}' collides with reserved stdlib symbol"; Span = unknownSpan }))

        let typedCombined, localTypes =
            inferProgramWithExternsRawAndLocals externs (stdlibProgram @ program)

        typedCombined |> List.skip (List.length stdlibProgram), localTypes

    let inferProgramWithLocalVariableTypes (program: Program) : TypedProgram * LocalVariableTypeInfo list =
        inferProgramWithExternsAndLocalVariableTypes [] program

    let inferProgram (program: Program) : TypedProgram =
        inferProgramWithExterns [] program
