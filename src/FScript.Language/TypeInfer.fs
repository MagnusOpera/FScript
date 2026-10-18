namespace FScript.Language

module TypeInfer =
    let private unknownSpan = Span.mk (Span.pos 0 0) (Span.pos 0 0)
    type Subst = Map<int, Type>

    let private emptySubst : Subst = Map.empty

    let private applyType (s: Subst) (t: Type) : Type =
        if Map.isEmpty s then
            t
        else
            let rec apply current =
                match current with
                | TVar v
                | TNumeric v ->
                    match s |> Map.tryFind v with
                    | Some mapped when mapped <> current -> apply mapped
                    | Some _ -> current
                    | None -> current
                | TRecordShape (id, fields, constructible) ->
                    match s.TryFind id with
                    | Some (TRecordShape (other, mappedFields, mappedConstructible)) when other = id ->
                        TRecordShape(id, mappedFields |> Map.map (fun _ t -> apply t), mappedConstructible)
                    | Some mapped when mapped <> current -> apply mapped
                    | _ -> TRecordShape(id, fields |> Map.map (fun _ t -> apply t), constructible)
                | TList t1 -> TList (apply t1)
                | TTask t1 -> TTask (apply t1)
                | TTuple ts -> TTuple (ts |> List.map apply)
                | TRecord fields -> TRecord (fields |> Map.map (fun _ t1 -> apply t1))
                | TMap (tk, tv) -> TMap (apply tk, apply tv)
                | TOption t1 -> TOption (apply t1)
                | TFun (a, b) -> TFun (apply a, apply b)
                | _ -> current

            apply t

    let private applyScheme (s: Subst) ((Forall (vars, t)) as scheme) =
        if Map.isEmpty s then
            scheme
        else
            let s' = vars |> List.fold (fun acc v -> Map.remove v acc) s
            Forall (vars, applyType s' t)

    let private applyEnv (s: Subst) (env: Map<string, Scheme>) =
        if Map.isEmpty s then env
        else env |> Map.map (fun _ sc -> applyScheme s sc)

    let private compose (s1: Subst) (s2: Subst) : Subst =
        if Map.isEmpty s1 then
            s2
        elif Map.isEmpty s2 then
            s1
        else
            let s2' = s2 |> Map.map (fun _ t -> applyType s1 t)
            Map.fold (fun acc k v -> Map.add k v acc) s1 s2'

    let private occurs (v: int) (t: Type) =
        let rec loop current =
            match current with
            | TVar currentVar | TNumeric currentVar -> currentVar = v
            | TList item
            | TTask item
            | TOption item -> loop item
            | TTuple items -> items |> List.exists loop
            | TRecordShape (id, fields, _) -> id = v || (fields |> Map.exists (fun _ t -> loop t))
            | THostManaged _ -> false
            | TRecord fields -> fields |> Map.exists (fun _ fieldType -> loop fieldType)
            | TMap (keyType, valueType)
            | TFun (keyType, valueType) -> loop keyType || loop valueType
            | TUnit
            | TInt
            | TFloat
            | TBool
            | TString
            | TNamed _
            | TUnion _
            | TTypeToken -> false

        loop t

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
                | TTask x, TTask y -> uni seen' x y
                | TTuple xs, TTuple ys ->
                    if xs.Length <> ys.Length then
                        raise (TypeException { Message = "Tuple arity mismatch"; Span = span })
                    List.zip xs ys
                    |> List.fold (fun sAcc (x, y) ->
                        let s1 = uni seen' (applyType sAcc x) (applyType sAcc y)
                        compose s1 sAcc) emptySubst
                | TVar v, t
                | t, TVar v ->
                    if t = TVar v then emptySubst
                    elif t = TNumeric v then Map.ofList [ v, t ]
                    elif occurs v t then raise (TypeException { Message = "Occurs check failed"; Span = span })
                    else Map.ofList [ v, t ]
                | TRecordShape (id, xf, xc), TRecordShape (other, yf, yc) ->
                    let shared = xf |> Map.filter (fun name _ -> yf.ContainsKey name)
                    let s = uni seen' (TRecord shared) (TRecord (yf |> Map.filter (fun name _ -> shared.ContainsKey name)))
                    let fields = Map.fold (fun acc name t -> Map.add name t acc) xf yf |> Map.map (fun _ t -> applyType s t)
                    if fields |> Map.exists (fun _ t -> occurs id t || occurs other t) then
                        raise (TypeException { Message = "Occurs check failed"; Span = span })
                    let merged = TRecordShape(other, fields, xc || yc)
                    let mergedSubst =
                        if id = other then Map.ofList [other, merged]
                        else Map.ofList [id, merged; other, merged]
                    compose mergedSubst s
                | TRecordShape (id, required, constructible), concrete
                | concrete, TRecordShape (id, required, constructible) ->
                    let rec fieldsOf t =
                        match t with
                        | TNamed name -> typeDefs.TryFind name |> Option.bind fieldsOf
                        | TRecord fields -> Some fields
                        | THostManaged (_, fields) when not constructible -> Some fields
                        | _ -> None
                    if occurs id concrete then
                        raise (TypeException { Message = "Occurs check failed"; Span = span })
                    match fieldsOf concrete with
                    | Some fields when required |> Map.forall (fun name _ -> fields.ContainsKey name) ->
                        let selected = fields |> Map.filter (fun name _ -> required.ContainsKey name)
                        let s = uni seen' (TRecord required) (TRecord selected)
                        compose (Map.ofList [id, applyType s concrete]) s
                    | _ -> raise (TypeException { Message = "Record constraint requires compatible fields and a constructible value for updates"; Span = span })
                | THostManaged (x, _), THostManaged (y, _) when x = y -> emptySubst
                | TRecord xf, TRecord yf ->
                    let fieldsToCheck, otherFields =
                        if xf.Count <= yf.Count then xf, yf
                        else yf, xf

                    if fieldsToCheck |> Map.exists (fun name _ -> not (otherFields.ContainsKey name)) then
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
                | TNumeric v, TNumeric w ->
                    if v = w then emptySubst else Map.ofList [ v, TNumeric w ]
                | TNumeric v, (TInt | TFloat as concrete)
                | (TInt | TFloat as concrete), TNumeric v -> Map.ofList [ v, concrete ]
                | TNumeric _, other | other, TNumeric _ ->
                    raise (TypeException { Message = sprintf "Expected numeric type, got %s" (Types.typeToString other); Span = span })
                | _ ->
                    raise (TypeException { Message = sprintf "Type mismatch: %s vs %s" (Types.typeToString a) (Types.typeToString b); Span = span })
        uni Set.empty t1 t2

    let private instantiate (Forall (vars, t)) =
        let freshMap = vars |> List.map (fun v -> v, Types.freshVar()) |> Map.ofList
        let rec subst t =
            match t with
            | TVar v -> freshMap |> Map.tryFind v |> Option.defaultValue t
            | TNumeric v ->
                match freshMap |> Map.tryFind v with
                | Some (TVar fresh) -> TNumeric fresh
                | _ -> t
            | TRecordShape (id, fields, constructible) ->
                let fresh = match freshMap.TryFind id with Some (TVar v) -> v | _ -> id
                TRecordShape(fresh, fields |> Map.map (fun _ t -> subst t), constructible)
            | TList a -> TList (subst a)
            | TTask a -> TTask (subst a)
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
        | TSLetPattern of Pattern * Expr * Map<string, Type> * bool * Span
        | TSLetRecGroup of (string * Expr * Type * Span) list * bool * Span
        | TSExpr of TypedExpr
    type TypedProgram = TypedStmt list

    type private InferenceTelemetry =
        { mutable ScopeStack: Set<string> list
          LocalVariableTypes: ResizeArray<LocalVariableTypeCapture> }

    let private createTelemetry () =
        { ScopeStack = [ Set.empty ]
          LocalVariableTypes = ResizeArray() }

    let private capturedLocalVariableTypes (telemetry: InferenceTelemetry option) : LocalVariableTypeInfo list =
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

    let private withLocalBindings (telemetry: InferenceTelemetry option) (names: string list) (work: unit -> 'T) : 'T =
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

    let private recordLocalVariableUse (telemetry: InferenceTelemetry option) (name: string) (span: Span) (varType: Type) =
        match telemetry with
        | None -> ()
        | Some t ->
            let current =
                match t.ScopeStack with
                | head :: _ -> head
                | [] -> Set.empty

            if Set.contains name current then
                t.LocalVariableTypes.Add({ Name = name; Span = span; Type = varType })

    let private recordLocalVariableDeclaration (telemetry: InferenceTelemetry option) (name: string) (span: Span) (varType: Type) =
        if System.String.IsNullOrWhiteSpace(name) || name = "_" then
            ()
        else
            match telemetry with
            | None -> ()
            | Some t ->
                t.LocalVariableTypes.Add({ Name = name; Span = span; Type = varType })

    let private applySubstToCapturedLocals (telemetry: InferenceTelemetry option) (startIndex: int) (subst: Subst) =
        match telemetry with
        | None -> ()
        | Some t ->
            for i = startIndex to t.LocalVariableTypes.Count - 1 do
                let entry = t.LocalVariableTypes[i]
                entry.Type <- applyType subst entry.Type

    let private currentTelemetryCaptureCount (telemetry: InferenceTelemetry option) =
        match telemetry with
        | Some t -> t.LocalVariableTypes.Count
        | None -> 0

    type private ConstructorSig =
        { UnionName: string
          Payload: Type option }

    let private asTyped expr t = { Expr = expr; Type = t; Span = Ast.spanOfExpr expr }

    let private builtinType (name: string) : Type option =
        match name with
        | "unit" -> Some TUnit
        | "int" -> Some TInt
        | "float" -> Some TFloat
        | "numeric" -> Some (Types.freshNumeric())
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
        | TRName "numeric" -> Types.freshNumeric()
        | TRName "bool" -> TBool
        | TRName "string" -> TString
        | TRName name ->
            let resolvedName = resolveReferencedTypeName typeDefs.Keys name span
            TNamed resolvedName
        | TRTuple ts -> ts |> List.map (annotationTypeFromRef typeDefs span) |> TTuple
        | TRFun (a, b) -> TFun(annotationTypeFromRef typeDefs span a, annotationTypeFromRef typeDefs span b)
        | TRPostfix (inner, "list") -> TList (annotationTypeFromRef typeDefs span inner)
        | TRPostfix (inner, "task") -> TTask (annotationTypeFromRef typeDefs span inner)
        | TRPostfix (inner, "option") -> TOption (annotationTypeFromRef typeDefs span inner)
        | TRPostfix (inner, "map") -> TMap (TString, annotationTypeFromRef typeDefs span inner)
        | TRMap (keyType, valueType) ->
            let key = annotationTypeFromRef typeDefs span keyType
            let resolvedKey = match key with | TNumeric _ -> TInt | _ -> key
            TMap(resolvedKey, annotationTypeFromRef typeDefs span valueType)
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
            |> fun fields ->
                match Types.freshVar() with
                | TVar id -> TRecordShape(id, fields, false)
                | _ -> failwith "Expected type variable"

    let rec private functionReturnType (t: Type) : Type =
        match t with
        | TFun (_, ret) -> functionReturnType ret
        | _ -> t

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
            | "task" -> TTask resolved
            | "option" -> TOption resolved
            | "map" -> TMap (TString, resolved)
            | _ -> raise (TypeException { Message = $"Unknown type suffix '{suffix}'"; Span = unknownSpan })
        | TRMap (keyType, valueType) ->
            let key = typeFromRef decls stack keyType
            let resolvedKey = match key with | TNumeric _ -> TInt | _ -> key
            TMap(resolvedKey, typeFromRef decls stack valueType)
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

    let private ensureSupportedMapKeyType (typeDefs: Map<string, Type>) (span: Span) (t: Type) =
        match t with
        | TString | TInt | TBool | TVar _ | TNumeric _ -> ()
        | TNamed name ->
            match typeDefs.TryFind name with
            | Some (TUnion (_, cases)) when cases |> Map.forall (fun _ payload -> payload.IsNone) -> ()
            | _ -> raise (TypeException { Message = $"Map key type must be string, int, bool, or a payload-free union, got {Types.typeToString t}"; Span = span })
        | _ -> raise (TypeException { Message = $"Map key type must be string, int, bool, or a payload-free union, got {Types.typeToString t}"; Span = span })

    let private constrainMapKeyTypes typeDefs span t =
        let mutable subst = emptySubst
        let rec loop current =
            match applyType subst current with
            | TMap (keyType, valueType) ->
                match keyType with
                | TNumeric _ -> subst <- compose (unify typeDefs keyType TInt span) subst
                | _ -> ensureSupportedMapKeyType typeDefs span keyType
                loop valueType
            | TList inner | TTask inner | TOption inner -> loop inner
            | TTuple items -> List.iter loop items
            | TRecord fields
            | TRecordShape (_, fields, _)
            | THostManaged (_, fields) -> fields |> Map.values |> Seq.iter loop
            | TFun (arg, result) -> loop arg; loop result
            | _ -> ()
        loop t
        subst

    let rec private inferPattern (telemetry: InferenceTelemetry option) (typeDefs: Map<string, Type>) (constructors: Map<string, ConstructorSig>) (pat: Pattern) : Map<string, Type> * Type =
        match pat with
        | PWildcard _ -> Map.empty, Types.freshVar()
        | PVar (name, _) ->
            let tv = Types.freshVar()
            recordLocalVariableDeclaration telemetry name (Ast.spanOfPattern pat) tv
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
            let startIndex = currentTelemetryCaptureCount telemetry
            let env1, t1 = inferPattern telemetry typeDefs constructors p1
            let env2, t2 = inferPattern telemetry typeDefs constructors p2
            let s = unify Map.empty t2 (TList t1) span
            applySubstToCapturedLocals telemetry startIndex s
            let env = Map.fold (fun acc k v -> Map.add k v acc) env1 env2
            env |> Map.map (fun _ ty -> applyType s ty), applyType s (TList t1)
        | PTuple (ps, _) ->
            let env, types =
                ps
                |> List.fold (fun (envAcc, typesAcc) p ->
                    let envPart, tPart = inferPattern telemetry typeDefs constructors p
                    let merged = Map.fold (fun acc k v -> Map.add k v acc) envAcc envPart
                    merged, tPart :: typesAcc) (Map.empty, [])
            env, TTuple (List.rev types)
        | PRecord (fields, _) ->
            let env, fieldTypes =
                fields
                |> List.fold (fun (envAcc, fieldAcc) (name, p) ->
                    let envPart, tPart = inferPattern telemetry typeDefs constructors p
                    let merged = Map.fold (fun acc k v -> Map.add k v acc) envAcc envPart
                    merged, Map.add name tPart fieldAcc) (Map.empty, Map.empty)
            let shape =
                match Types.freshVar() with
                | TVar id -> TRecordShape(id, fieldTypes, false)
                | _ -> failwith "Expected type variable"
            env, shape
        | PMap (clauses, tailPattern, span) ->
            let startIndex = currentTelemetryCaptureCount telemetry
            let ensureSupportedMapKeyType = ensureSupportedMapKeyType typeDefs span

            let mapKeyType = Types.freshVar()
            let valueType = Types.freshVar()
            let mutable sAcc = emptySubst
            let mutable envAcc : Map<string, Type> = Map.empty

            for (keyPattern, valuePattern) in clauses do
                let keyEnv, keyTypePart = inferPattern telemetry typeDefs constructors keyPattern
                let valueEnv, currentValueType = inferPattern telemetry typeDefs constructors valuePattern

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
                let tailEnv, tailType = inferPattern telemetry typeDefs constructors tail
                let expectedTailType = TMap (applyType sAcc mapKeyType, applyType sAcc valueType)
                let sTail = unify typeDefs (applyType sAcc tailType) expectedTailType span
                sAcc <- compose sTail sAcc
                envAcc <- tailEnv |> Map.map (fun _ t -> applyType sAcc t) |> Map.fold (fun acc k v -> Map.add k v acc) envAcc
            | None -> ()

            ensureSupportedMapKeyType (applyType sAcc mapKeyType)
            applySubstToCapturedLocals telemetry startIndex sAcc
            envAcc |> Map.map (fun _ t -> applyType sAcc t), TMap (applyType sAcc mapKeyType, applyType sAcc valueType)
        | PSome (p, _) ->
            let env, t = inferPattern telemetry typeDefs constructors p
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
                    let startIndex = currentTelemetryCaptureCount telemetry
                    let envP, tP = inferPattern telemetry typeDefs constructors payloadPattern
                    let s = unify typeDefs tP expectedPayload span
                    applySubstToCapturedLocals telemetry startIndex s
                    envP |> Map.map (fun _ t -> applyType s t), unionType
                | None, Some _ ->
                    raise (TypeException { Message = sprintf "Union case '%s' does not take a payload" constructorName; Span = span })
                | Some _, None ->
                    raise (TypeException { Message = sprintf "Union case '%s' requires a payload" constructorName; Span = span })
        | PTypeRef (tref, span) ->
            Map.empty, annotationTypeFromRef typeDefs span tref

    let private constrainNumeric (typeDefs: Map<string, Type>) (t: Type) (span: Span) =
        match t with
        | TInt | TFloat | TNumeric _ -> emptySubst, t
        | _ ->
            let numeric = Types.freshNumeric()
            let subst = unify typeDefs t numeric span
            subst, applyType subst numeric

    let private validateMapKeyTypes (typeDefs: Map<string, Type>) (span: Span) (t: Type) : unit =
        let rec loop t =
            match t with
            | TMap (keyType, valueType) ->
                loop keyType
                loop valueType
                ensureSupportedMapKeyType typeDefs span keyType
            | TList inner -> loop inner
            | TTask inner -> loop inner
            | TTuple items -> items |> List.iter loop
            | TRecord fields
            | TRecordShape (_, fields, _)
            | THostManaged (_, fields) -> fields |> Map.values |> Seq.iter loop
            | TOption inner -> loop inner
            | TFun (a, b) -> loop a; loop b
            | _ -> ()
        loop t

    let rec private inferExpr (telemetry: InferenceTelemetry option) (typeDefs: Map<string, Type>) (constructors: Map<string, ConstructorSig>) (env: Map<string, Scheme>) (expr: Expr) : Subst * Type * TypedExpr =
        let subst, inferred, _ = inferExprRaw telemetry typeDefs constructors env expr
        let keySubst = constrainMapKeyTypes typeDefs (Ast.spanOfExpr expr) (applyType subst inferred)
        let combined = compose keySubst subst
        let resolved = applyType combined inferred
        combined, resolved, asTyped expr resolved

    and private inferExprRaw (telemetry: InferenceTelemetry option) (typeDefs: Map<string, Type>) (constructors: Map<string, ConstructorSig>) (env: Map<string, Scheme>) (expr: Expr) : Subst * Type * TypedExpr =
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
                recordLocalVariableUse telemetry name span t
                emptySubst, t, asTyped expr t
            | None -> raise (TypeException { Message = sprintf "Unbound variable '%s'" name; Span = span })
        | EParen (inner, _) ->
            let s, t, _ = inferExpr telemetry typeDefs constructors env inner
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
                    let s1, _, _ = inferExpr telemetry typeDefs constructors (applyEnv sAcc env) pexpr
                    sAcc <- compose s1 sAcc
            sAcc, TString, asTyped expr TString
        | ELambda (param, body, _) ->
            let tv =
                match param.Annotation with
                | Some tref -> annotationTypeFromRef typeDefs param.Span tref
                | None -> Types.freshVar()
            recordLocalVariableDeclaration telemetry param.Name param.Span tv
            let env' = env |> Map.add param.Name (Forall([], tv))
            let s1, tBody, _ =
                withLocalBindings telemetry [ param.Name ] (fun () ->
                    inferExpr telemetry typeDefs constructors env' body)
            let tArg = applyType s1 tv
            let tFun = TFun (tArg, tBody)
            s1, tFun, asTyped expr tFun
        | EApply (fn, arg, span) ->
            let s1, t1, _ = inferExpr telemetry typeDefs constructors env fn
            let env2 = applyEnv s1 env
            let s2, t2, _ = inferExpr telemetry typeDefs constructors env2 arg
            let tv = Types.freshVar()
            let s3 = unify typeDefs (applyType s2 t1) (TFun (t2, tv)) span
            let combined = compose s3 (compose s2 s1)
            let keySubst = constrainMapKeyTypes typeDefs span (applyType combined (TFun(t2, tv)))
            let s = compose keySubst combined
            let tRes = applyType s tv
            s, tRes, asTyped expr tRes
        | EIf (cond, tExpr, fExpr, span) ->
            let s1, tCond, _ = inferExpr telemetry typeDefs constructors env cond
            let sBool = unify typeDefs tCond TBool span
            let env2 = applyEnv (compose sBool s1) env
            let s2, tThen, _ = inferExpr telemetry typeDefs constructors env2 tExpr
            let env3 = applyEnv (compose s2 (compose sBool s1)) env
            let s3, tElse, _ = inferExpr telemetry typeDefs constructors env3 fExpr
            let s4 = unify typeDefs (applyType s3 tThen) tElse span
            let s = compose s4 (compose s3 (compose s2 (compose sBool s1)))
            let tRes = applyType s tElse
            s, tRes, asTyped expr tRes
        | ERaise (value, span) ->
            let s1, t1, _ = inferExpr telemetry typeDefs constructors env value
            let s2 = unify typeDefs (applyType s1 t1) TString span
            let s = compose s2 s1
            let tRes = Types.freshVar()
            s, tRes, asTyped expr tRes
        | EFor (name, source, body, span) ->
            let s1, tSource, _ = inferExpr telemetry typeDefs constructors env source
            let tv = Types.freshVar()
            let sList = unify typeDefs (applyType s1 tSource) (TList tv) span
            let sSource = compose sList s1
            let itemType = applyType sSource tv
            let envBody =
                applyEnv sSource env
                |> Map.add name (Forall([], itemType))
            let s2, tBody, _ =
                withLocalBindings telemetry [ name ] (fun () ->
                    inferExpr telemetry typeDefs constructors envBody body)
            let sBodyUnit = unify typeDefs (applyType s2 tBody) TUnit (Ast.spanOfExpr body)
            let s = compose sBodyUnit (compose s2 sSource)
            s, TUnit, asTyped expr TUnit
        | ELet (name, value, body, isRec, returnAnnotation, span) ->
            if isRec then
                let tv = Types.freshVar()
                let envRec = env |> Map.add name (Forall([], tv))
                let s1, t1, _ =
                    withLocalBindings telemetry [ name ] (fun () ->
                        inferExpr telemetry typeDefs constructors envRec value)
                let s2 = unify typeDefs (applyType s1 tv) t1 span
                let sRet =
                    match returnAnnotation with
                    | Some tref ->
                        let expectedReturn = annotationTypeFromRef typeDefs span tref
                        let inferredReturn = functionReturnType (applyType s2 t1)
                        unify typeDefs inferredReturn expectedReturn span
                    | None -> emptySubst
                let sValue = compose sRet (compose s2 s1)
                let env1 = applyEnv sValue env
                let scheme = Types.generalize env1 (applyType sValue t1)
                let env2 = env1 |> Map.add name scheme
                let s3, tBody, _ =
                    withLocalBindings telemetry [ name ] (fun () ->
                        inferExpr telemetry typeDefs constructors env2 body)
                let s = compose s3 sValue
                s, tBody, asTyped expr tBody
            else
                let s1, t1, _ = inferExpr telemetry typeDefs constructors env value
                let sDiscard =
                    if name = "_" then
                        unify typeDefs (applyType s1 t1) TUnit (Ast.spanOfExpr value)
                    else
                        emptySubst
                let sRet =
                    match returnAnnotation with
                    | Some tref ->
                        let expectedReturn = annotationTypeFromRef typeDefs span tref
                        let inferredReturn = functionReturnType (applyType sDiscard t1)
                        unify typeDefs inferredReturn expectedReturn span
                    | None -> emptySubst
                let sValue = compose sRet (compose sDiscard s1)
                let env1 = applyEnv sValue env
                let scheme = Types.generalize env1 (applyType sValue t1)
                let env2 = env1 |> Map.add name scheme
                let s2, t2, _ =
                    withLocalBindings telemetry [ name ] (fun () ->
                        inferExpr telemetry typeDefs constructors env2 body)
                let s = compose s2 sValue
                s, t2, asTyped expr t2
        | ELetPattern (pat, value, body, span) ->
            let startIndex = currentTelemetryCaptureCount telemetry
            let s1, tValue, _ = inferExpr telemetry typeDefs constructors env value
            let envPat, tPat = inferPattern telemetry typeDefs constructors pat
            let s2 = unify typeDefs (applyType s1 tValue) tPat span
            let sValue = compose s2 s1
            applySubstToCapturedLocals telemetry startIndex sValue
            let env1 = applyEnv sValue env
            let envPatTyped = envPat |> Map.map (fun _ ty -> applyType sValue ty)
            let env2 =
                envPatTyped
                |> Map.fold (fun state name bindingType ->
                    let scheme = Types.generalize env1 bindingType
                    Map.add name scheme state) env1
            let boundNames = envPatTyped |> Map.toList |> List.map fst
            let sBody, tBody, _ =
                withLocalBindings telemetry boundNames (fun () ->
                    inferExpr telemetry typeDefs constructors env2 body)
            let s = compose sBody sValue
            s, tBody, asTyped expr tBody
        | ELetRecGroup (bindings, body, span) ->
            let foldedBindings =
                bindings
                |> List.map (fun (name, args, returnAnnotation, valueExpr, bindingSpan) ->
                    if args.IsEmpty then
                        raise (TypeException { Message = "'let rec ... and ...' requires function arguments for each binding"; Span = bindingSpan })
                    let folded = Seq.foldBack (fun arg acc -> ELambda(arg, acc, bindingSpan)) args valueExpr
                    name, returnAnnotation, folded, bindingSpan)

            let freshByName =
                foldedBindings
                |> List.map (fun (name, _, _, _) -> name, Types.freshVar())
                |> Map.ofList

            let envRec =
                freshByName
                |> Map.fold (fun acc name tv -> Map.add name (Forall([], tv)) acc) env

            let mutable sRec = emptySubst
            let recursiveNames = freshByName |> Map.toList |> List.map fst
            withLocalBindings telemetry recursiveNames (fun () ->
                for (name, _, exprVal, bindingSpan) in foldedBindings do
                    let envForBinding = applyEnv sRec envRec
                    let s1, t1, _ = inferExpr telemetry typeDefs constructors envForBinding exprVal
                    let expected = applyType s1 (applyType sRec freshByName.[name])
                    let s2 = unify typeDefs expected t1 bindingSpan
                    sRec <- compose s2 (compose s1 sRec))

            for (name, returnAnnotation, _, bindingSpan) in foldedBindings do
                match returnAnnotation with
                | Some tref ->
                    let expectedReturn = annotationTypeFromRef typeDefs bindingSpan tref
                    let inferredReturn = functionReturnType (applyType sRec freshByName.[name])
                    let sRet = unify typeDefs inferredReturn expectedReturn bindingSpan
                    sRec <- compose sRet sRec
                | None -> ()

            let envGeneralize = applyEnv sRec env
            let schemes =
                foldedBindings
                |> List.map (fun (name, _, _, _) ->
                    let inferred = applyType sRec freshByName.[name]
                    name, Types.generalize envGeneralize inferred)

            let envBody =
                schemes
                |> List.fold (fun acc (name, scheme) -> Map.add name scheme acc) envGeneralize

            let sBody, tBody, _ =
                withLocalBindings telemetry recursiveNames (fun () ->
                    inferExpr telemetry typeDefs constructors envBody body)
            let s = compose sBody sRec
            s, tBody, asTyped expr tBody
        | EMatch (scrutinee, cases, span) ->
            let s1, tScrut, _ = inferExpr telemetry typeDefs constructors env scrutinee
            let mutable sAcc = s1
            let mutable resultTypeOpt : Type option = None
            for (pat, guard, body, caseSpan) in cases do
                let envPat, tPat = inferPattern telemetry typeDefs constructors pat
                let sPat = unify typeDefs (applyType sAcc tScrut) tPat caseSpan
                let envCase = applyEnv (compose sPat sAcc) env
                let envCase' = envPat |> Map.fold (fun acc k v -> Map.add k (Forall([], applyType sPat v)) acc) envCase
                let caseBoundNames = envPat |> Map.toList |> List.map fst
                let sBody, tBody, _ =
                    withLocalBindings telemetry caseBoundNames (fun () ->
                        let mutable envBodyCase = envCase'
                        match guard with
                        | Some guardExpr ->
                            let sGuard, tGuard, _ = inferExpr telemetry typeDefs constructors envBodyCase guardExpr
                            let sGuardBool = unify typeDefs (applyType sGuard tGuard) TBool caseSpan
                            sAcc <- compose sGuardBool (compose sGuard sAcc)
                            envBodyCase <- applyEnv (compose sGuardBool sGuard) envBodyCase
                        | None -> ()
                        inferExpr telemetry typeDefs constructors envBodyCase body)
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
                let s1, t1, _ = inferExpr telemetry typeDefs constructors (applyEnv sAcc env) item
                let s2 = unify typeDefs (applyType s1 tv) t1 span
                sAcc <- compose s2 (compose s1 sAcc)
            let tRes = TList (applyType sAcc tv)
            sAcc, tRes, asTyped expr tRes
        | ERange (startExpr, endExpr, span) ->
            let s1, tStart, _ = inferExpr telemetry typeDefs constructors env startExpr
            let env2 = applyEnv s1 env
            let s2, tEnd, _ = inferExpr telemetry typeDefs constructors env2 endExpr
            let s3 = unify typeDefs (applyType s2 tStart) TInt span
            let s4 = unify typeDefs (applyType s3 (applyType s2 tEnd)) TInt span
            let s = compose s4 (compose s3 (compose s2 s1))
            let tRes = TList TInt
            s, tRes, asTyped expr tRes
        | ETuple (items, _) ->
            let mutable sAcc = emptySubst
            let mutable inferred : Type list = []
            for item in items do
                let s1, t1, _ = inferExpr telemetry typeDefs constructors (applyEnv sAcc env) item
                sAcc <- compose s1 sAcc
                inferred <- inferred @ [ applyType sAcc t1 ]
            let tRes = TTuple inferred
            sAcc, tRes, asTyped expr tRes
        | ERecord (fields, span) ->
            let mutable sAcc = emptySubst
            let mutable inferred : Map<string, Type> = Map.empty
            for (name, valueExpr) in fields do
                let s1, t1, _ = inferExpr telemetry typeDefs constructors (applyEnv sAcc env) valueExpr
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
                let s1, t1, _ = inferExpr telemetry typeDefs constructors (applyEnv sAcc env) valueExpr
                sAcc <- compose s1 sAcc
                inferred <- inferred.Add(name, applyType sAcc t1)
            let tRes = TRecord inferred
            sAcc, tRes, asTyped expr tRes
        | EMap (entries, span) ->
            let ensureSupportedMapKeyType = ensureSupportedMapKeyType typeDefs span

            let keyType = Types.freshVar()
            let valueType = Types.freshVar()
            let mutable sAcc = emptySubst
            for entry in entries do
                match entry with
                | MEKeyValue (keyExpr, valueExpr) ->
                    let envForKey = applyEnv sAcc env
                    let sKey, tKey, _ = inferExpr telemetry typeDefs constructors envForKey keyExpr
                    let sKeyType = unify typeDefs (applyType sKey tKey) (applyType sKey (applyType sAcc keyType)) (Ast.spanOfExpr keyExpr)
                    let sAfterKey = compose sKeyType (compose sKey sAcc)
                    ensureSupportedMapKeyType (applyType sAfterKey keyType)

                    let envForValue = applyEnv sAfterKey env
                    let sValue, tValue, _ = inferExpr telemetry typeDefs constructors envForValue valueExpr
                    let expectedValueType = applyType sValue (applyType sAfterKey valueType)
                    let sValueType = unify typeDefs expectedValueType tValue (Ast.spanOfExpr valueExpr)

                    sAcc <- compose sValueType (compose sValue sAfterKey)
                | MESpread spreadExpr ->
                    let envForSpread = applyEnv sAcc env
                    let sSpread, tSpread, _ = inferExpr telemetry typeDefs constructors envForSpread spreadExpr
                    let expectedSpreadType = TMap (applyType sSpread (applyType sAcc keyType), applyType sSpread (applyType sAcc valueType))
                    let sSpreadMap = unify typeDefs (applyType sSpread tSpread) expectedSpreadType (Ast.spanOfExpr spreadExpr)
                    sAcc <- compose sSpreadMap (compose sSpread sAcc)

            ensureSupportedMapKeyType (applyType sAcc keyType)
            let tRes = TMap (applyType sAcc keyType, applyType sAcc valueType)
            sAcc, tRes, asTyped expr tRes
        | ERecordUpdate (baseExpr, updates, span) ->
            let sInitial, tBase, _ = inferExpr telemetry typeDefs constructors env baseExpr
            let sBase =
                match applyType sInitial tBase with
                | TRecordShape (id, fields, _) -> compose (Map.ofList [id, TRecordShape(id, fields, true)]) sInitial
                | _ -> sInitial
            let baseFields =
                match applyType sBase tBase with
                | TRecord fields
                | TRecordShape (_, fields, _) -> fields
                | TNamed name ->
                    match typeDefs.TryFind name with
                    | Some (TRecord fields) -> fields
                    | Some (THostManaged _) -> raise (TypeException { Message = $"Host-managed type '{name}' cannot be updated"; Span = span })
                    | _ -> raise (TypeException { Message = "Record update requires a record value"; Span = span })
                | _ -> raise (TypeException { Message = "Record update requires a record value"; Span = span })
            let mutable sAcc = sBase
            for (name, valueExpr) in updates do
                let expectedFieldType =
                    match baseFields.TryFind name with
                    | Some t -> t
                    | None -> raise (TypeException { Message = sprintf "Record field '%s' not found" name; Span = Ast.spanOfExpr valueExpr })
                let envForField = applyEnv sAcc env
                let sField, tField, _ = inferExpr telemetry typeDefs constructors envForField valueExpr
                let expected = applyType sField (applyType sAcc expectedFieldType)
                let sEq = unify typeDefs expected tField (Ast.spanOfExpr valueExpr)
                sAcc <- compose sEq (compose sField sAcc)
            let resultFields = baseFields |> Map.map (fun _ t -> applyType sAcc t)
            let tRes = TRecord resultFields
            sAcc, tRes, asTyped expr tRes
        | EStructuralRecordUpdate (baseExpr, updates, span) ->
            let sInitial, tBase, _ = inferExpr telemetry typeDefs constructors env baseExpr
            let sBase =
                match applyType sInitial tBase with
                | TRecordShape (id, fields, _) -> compose (Map.ofList [id, TRecordShape(id, fields, true)]) sInitial
                | _ -> sInitial
            let baseFields =
                match applyType sBase tBase with
                | TRecord fields
                | TRecordShape (_, fields, _) -> fields
                | TNamed name ->
                    match typeDefs.TryFind name with
                    | Some (TRecord fields) -> fields
                    | Some (THostManaged _) -> raise (TypeException { Message = $"Host-managed type '{name}' cannot be updated"; Span = span })
                    | _ -> raise (TypeException { Message = "Structural record update requires a record value"; Span = span })
                | _ -> raise (TypeException { Message = "Structural record update requires a record value"; Span = span })
            let mutable sAcc = sBase
            let mutable resultFields = baseFields
            for (name, valueExpr) in updates do
                let envForField = applyEnv sAcc env
                let sField, tField, _ = inferExpr telemetry typeDefs constructors envForField valueExpr
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
                | TRecordShape (id, fields, constructible) ->
                    match fields.TryFind fieldName with
                    | Some tField -> s1, tField
                    | None ->
                        let tField = Types.freshVar()
                        let s = compose (Map.ofList [id, TRecordShape(id, fields.Add(fieldName, tField), constructible)]) s1
                        s, tField
                | TRecord fields
                | THostManaged (_, fields) ->
                    match fields.TryFind fieldName with
                    | Some tField -> s1, tField
                    | None -> raise (TypeException { Message = sprintf "Record field '%s' not found" fieldName; Span = span })
                | TNamed name ->
                    match typeDefs.TryFind name with
                    | Some (TRecord fields)
                    | Some (THostManaged (_, fields)) ->
                        match fields.TryFind fieldName with
                        | Some tField -> s1, tField
                        | None -> raise (TypeException { Message = sprintf "Record field '%s' not found" fieldName; Span = span })
                    | _ -> raise (TypeException { Message = "Field access requires a record value"; Span = span })
                | TVar _ as tv ->
                    let tField = Types.freshVar()
                    let shape =
                        match Types.freshVar() with
                        | TVar id -> TRecordShape(id, Map.ofList [fieldName, tField], false)
                        | _ -> failwith "Expected type variable"
                    let sField = unify typeDefs tv shape span
                    let s = compose sField s1
                    s, applyType s tField
                | _ -> raise (TypeException { Message = "Field access requires a record value"; Span = span })

            let rec qualifiedTarget = function
                | EVar (name, _) when not (env.ContainsKey name) -> Some name
                | EFieldGet (parent, name, _) -> qualifiedTarget parent |> Option.map (fun prefix -> prefix + "." + name)
                | _ -> None
            match qualifiedTarget target with
            | Some moduleName ->
                let qualifiedName = $"{moduleName}.{fieldName}"
                match env.TryFind qualifiedName with
                | Some scheme ->
                    let t = instantiate scheme
                    emptySubst, t, asTyped expr t
                | None ->
                    let s1, tTarget, _ = inferExpr telemetry typeDefs constructors env target
                    let sField, tField = inferRecordField s1 tTarget
                    sField, tField, asTyped expr tField
            | _ ->
                let s1, tTarget, _ = inferExpr telemetry typeDefs constructors env target
                let sField, tField = inferRecordField s1 tTarget
                sField, tField, asTyped expr tField
        | EIndexGet (target, keyExpr, span) ->
            let s1, tTarget, _ = inferExpr telemetry typeDefs constructors env target
            let env2 = applyEnv s1 env
            let s2, tKey, _ = inferExpr telemetry typeDefs constructors env2 keyExpr
            let targetType = applyType s2 tTarget
            let keyType = applyType s2 tKey
            match targetType, keyType with
            | TList itemType, _ ->
                let s3 = unify typeDefs keyType TInt span
                let s = compose s3 (compose s2 s1)
                let tRes = TOption (applyType s itemType)
                s, tRes, asTyped expr tRes
            | TMap (mapKeyType, valueType), _ ->
                let s3 = unify typeDefs keyType mapKeyType span
                let s = compose s3 (compose s2 s1)
                ensureSupportedMapKeyType typeDefs span (applyType s mapKeyType)
                let tRes = TOption (applyType s valueType)
                s, tRes, asTyped expr tRes
            | TVar _, TInt ->
                let itemType = Types.freshVar()
                let s3 = unify typeDefs targetType (TList itemType) span
                let s = compose s3 (compose s2 s1)
                let tRes = TOption (applyType s itemType)
                s, tRes, asTyped expr tRes
            | TVar _, (TString | TBool | TNamed _) ->
                let valueType = Types.freshVar()
                ensureSupportedMapKeyType typeDefs span keyType
                let s3 = unify typeDefs targetType (TMap (keyType, valueType)) span
                let s = compose s3 (compose s2 s1)
                let tRes = TOption (applyType s valueType)
                s, tRes, asTyped expr tRes
            | TVar _, TVar _ ->
                raise (TypeException { Message = "Ambiguous index access: annotate the list or map target"; Span = span })
            | _ ->
                let expectedKind =
                    match keyType with
                    | TInt -> "list"
                    | TString -> "map"
                    | _ -> "list or map"

                raise (TypeException { Message = $"Index access requires a {expectedKind} target with a compatible index type"; Span = span })
        | EBinOp (op, a, b, span) ->
            match op with
            | "|>" ->
                let s1, tLeft, _ = inferExpr telemetry typeDefs constructors env a
                let env2 = applyEnv s1 env
                let s2, tRight, _ = inferExpr telemetry typeDefs constructors env2 b
                let tv = Types.freshVar()
                let s3 = unify typeDefs (applyType s2 tRight) (TFun (applyType s2 tLeft, tv)) span
                let combined = compose s3 (compose s2 s1)
                let keySubst = constrainMapKeyTypes typeDefs span (applyType combined (TFun(tLeft, tv)))
                let s = compose keySubst combined
                let tRes = applyType s tv
                s, tRes, asTyped expr tRes
            | "::" ->
                let s1, tHead, _ = inferExpr telemetry typeDefs constructors env a
                let env2 = applyEnv s1 env
                let s2, tTail, _ = inferExpr telemetry typeDefs constructors env2 b
                let s3 = unify typeDefs (applyType s2 tTail) (TList tHead) span
                let s = compose s3 (compose s2 s1)
                let tRes = applyType s (TList tHead)
                s, tRes, asTyped expr tRes
            | "@" ->
                let s1, t1, _ = inferExpr telemetry typeDefs constructors env a
                let env2 = applyEnv s1 env
                let s2, t2, _ = inferExpr telemetry typeDefs constructors env2 b
                let tv = Types.freshVar()
                let s3 = unify typeDefs (applyType s2 t1) (TList tv) span
                let s4 = unify typeDefs (applyType s3 (applyType s2 t2)) (TList (applyType s3 tv)) span
                let s = compose s4 (compose s3 (compose s2 s1))
                let tRes = TList (applyType s tv)
                s, tRes, asTyped expr tRes
            | _ ->
                let s1, t1, _ = inferExpr telemetry typeDefs constructors env a
                let env2 = applyEnv s1 env
                let s2, t2, _ = inferExpr telemetry typeDefs constructors env2 b
                let s3 = unify typeDefs (applyType s2 t1) t2 span
                let t = applyType s3 t2
                let sOp, resultType =
                    match op with
                    | "+" | "-" | "*" | "/" ->
                        constrainNumeric typeDefs t span
                    | "%" -> unify typeDefs t TInt span, TInt
                    | "=" -> emptySubst, TBool
                    | "<" | ">" | "<=" | ">=" ->
                        let constraintSubst, _ = constrainNumeric typeDefs t span
                        constraintSubst, TBool
                    | "&&" | "||" -> unify typeDefs t TBool span, TBool
                    | _ -> raise (TypeException { Message = sprintf "Unknown operator %s" op; Span = span })
                let s = compose sOp (compose s3 (compose s2 s1))
                s, resultType, asTyped expr resultType
        | ESome (value, _) ->
            let s1, t1, _ = inferExpr telemetry typeDefs constructors env value
            s1, TOption t1, asTyped expr (TOption t1)
        | ENone _ ->
            let tv = Types.freshVar()
            emptySubst, TOption tv, asTyped expr (TOption tv)
        | ECons (head, tail, span) ->
            let s1, t1, _ = inferExpr telemetry typeDefs constructors env head
            let env2 = applyEnv s1 env
            let s2, t2, _ = inferExpr telemetry typeDefs constructors env2 tail
            let s3 = unify typeDefs (applyType s2 t2) (TList t1) span
            let s = compose s3 (compose s2 s1)
            let tRes = applyType s (TList t1)
            s, tRes, asTyped expr tRes
        | EAppend (a, b, span) ->
            let s1, t1, _ = inferExpr telemetry typeDefs constructors env a
            let env2 = applyEnv s1 env
            let s2, t2, _ = inferExpr telemetry typeDefs constructors env2 b
            let tv = Types.freshVar()
            let s3 = unify typeDefs (applyType s2 t1) (TList tv) span
            let s4 = unify typeDefs (applyType s3 (applyType s2 t2)) (TList (applyType s3 tv)) span
            let s = compose s4 (compose s3 (compose s2 s1))
            let tRes = TList (applyType s tv)
            s, tRes, asTyped expr tRes

    let private envFromExterns (externs: ExternalFunction list) : Map<string, Scheme> =
        externs
        |> List.fold (fun acc ext -> acc.Add(ext.Name, ext.Scheme)) BuiltinSignatures.builtinSchemes

    let private patternBindingNames (pat: Pattern) : string list =
        let rec loop acc pattern =
            match pattern with
            | PWildcard _
            | PLiteral _
            | PNil _
            | PNone _
            | PTypeRef _ -> acc
            | PVar (name, _) -> name :: acc
            | PCons (head, tail, _) -> loop (loop acc head) tail
            | PTuple (items, _) -> items |> List.fold loop acc
            | PRecord (fields, _) -> fields |> List.fold (fun state (_, p) -> loop state p) acc
            | PMap (clauses, tailPattern, _) ->
                let withClauses =
                    clauses
                    |> List.fold (fun state (k, v) -> loop (loop state k) v) acc
                match tailPattern with
                | Some tail -> loop withClauses tail
                | None -> withClauses
            | PSome (inner, _) -> loop acc inner
            | PUnionCase (_, _, payload, _) ->
                match payload with
                | Some inner -> loop acc inner
                | None -> acc
        loop [] pat

    let private topLevelBindingNames (program: Program) : string list =
        program
        |> List.collect (function
            | SLet(name, _, _, _, _, _, _) -> [ name ]
            | SLetPattern(pat, _, _, _) -> patternBindingNames pat
            | SLetRecGroup(bindings, _, _) -> bindings |> List.map (fun (name, _, _, _, _) -> name)
            | _ -> [])

    let private inferProgramWithExternsRawAndLocals (captureLocalVariables: bool) (registration: ExtensionRegistration) (program: Program) : TypedProgram * LocalVariableTypeInfo list =
        let externs = registration.Functions
        let hostDefinitions =
            try HostInterop.typeDefinitions registration ((BuiltinTypes.typeDefs |> List.map (fun d -> d.Name)) @ (program |> List.choose (function SType d -> Some d.Name | _ -> None)))
            with EvalException error -> raise (TypeException { Message = error.Message; Span = error.Span })
        let decls =
            (BuiltinTypes.typeDefs |> List.map (fun d -> d.Name, d))
            @ (program |> List.choose (function | SType d -> Some (d.Name, d) | _ -> None))
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

        let typeDefs = Map.fold (fun acc name t -> Map.add name t acc) typeDefs hostDefinitions

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
            |> List.append (registration.Unions |> List.collect (fun descriptor ->
                descriptor.Cases |> Map.toList |> List.collect (fun (caseName, payload) ->
                    let info = { UnionName = descriptor.Name; Payload = payload }
                    [caseName, info; $"{descriptor.Name}.{caseName}", info])))
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

        let telemetry =
            if captureLocalVariables then Some(createTelemetry ())
            else None

        let typed = ResizeArray<TypedStmt>()
        try
            for i, stmt in program |> List.indexed do
                match stmt with
                | SType def ->
                    typed.Add(TSType def)
                | SImport (_, _, span) ->
                    raise (TypeException { Message = "'import' must be resolved before type inference"; Span = span })
                | SLet(name, args, returnAnnotation, expr, isRec, isExported, span) ->
                    let exprVal = Seq.foldBack (fun arg acc -> ELambda(arg, acc, span)) args expr
                    let startIndex =
                        match telemetry with
                        | Some t -> t.LocalVariableTypes.Count
                        | None -> 0

                    if isRec then
                        let tv = Types.freshVar()
                        let envRec = env |> Map.add name (Forall([], tv))
                        let s1, t1, _ = inferExpr telemetry typeDefs constructors envRec exprVal
                        let s2 = unify typeDefs (applyType s1 tv) t1 span
                        let sRet =
                            match returnAnnotation with
                            | Some tref ->
                                let expectedReturn = annotationTypeFromRef typeDefs span tref
                                let inferredReturn = functionReturnType (applyType s2 t1)
                                unify typeDefs inferredReturn expectedReturn span
                            | None -> emptySubst
                        let s = compose sRet (compose s2 s1)
                        applySubstToCapturedLocals telemetry startIndex s
                        let env' = applyEnv s env
                        let inferred = applyType s t1
                        validateMapKeyTypes typeDefs span inferred
                        let scheme = Types.generalize env' inferred
                        env <- env' |> Map.add name scheme
                        typed.Add(TSLet(name, exprVal, inferred, true, isExported, span))
                    else
                        let s1, t1, _ = inferExpr telemetry typeDefs constructors env exprVal
                        let sRet =
                            match returnAnnotation with
                            | Some tref ->
                                let expectedReturn = annotationTypeFromRef typeDefs span tref
                                let inferredReturn = functionReturnType (applyType s1 t1)
                                unify typeDefs inferredReturn expectedReturn span
                            | None -> emptySubst
                        let s = compose sRet s1
                        applySubstToCapturedLocals telemetry startIndex s
                        let env' = applyEnv s env
                        let inferred = applyType s t1
                        validateMapKeyTypes typeDefs span inferred
                        let scheme = Types.generalize env' inferred
                        env <- env' |> Map.add name scheme
                        typed.Add(TSLet(name, exprVal, inferred, false, isExported, span))
                | SLetPattern(pattern, expr, isExported, span) ->
                    let startIndex =
                        match telemetry with
                        | Some t -> t.LocalVariableTypes.Count
                        | None -> 0

                    let s1, tValue, _ = inferExpr telemetry typeDefs constructors env expr
                    let envPat, tPat = inferPattern telemetry typeDefs constructors pattern
                    let s2 = unify typeDefs (applyType s1 tValue) tPat span
                    let s = compose s2 s1
                    applySubstToCapturedLocals telemetry startIndex s
                    let envBase = applyEnv s env
                    let typedBindings = envPat |> Map.map (fun _ ty -> applyType s ty)
                    typedBindings
                    |> Map.iter (fun _ ty -> validateMapKeyTypes typeDefs span ty)
                    env <-
                        typedBindings
                        |> Map.fold (fun state name bindingType ->
                            let scheme = Types.generalize envBase bindingType
                            Map.add name scheme state) envBase
                    typed.Add(TSLetPattern(pattern, expr, typedBindings, isExported, span))
                | SLetRecGroup(bindings, isExported, span) ->
                    let startIndex =
                        match telemetry with
                        | Some t -> t.LocalVariableTypes.Count
                        | None -> 0

                    let foldedBindings =
                        bindings
                        |> List.map (fun (name, args, returnAnnotation, valueExpr, bindingSpan) ->
                            if args.IsEmpty then
                                raise (TypeException { Message = "'let rec ... and ...' requires function arguments for each binding"; Span = bindingSpan })
                            let folded = Seq.foldBack (fun arg acc -> ELambda(arg, acc, bindingSpan)) args valueExpr
                            name, returnAnnotation, folded, bindingSpan)

                    let freshByName =
                        foldedBindings
                        |> List.map (fun (name, _, _, _) -> name, Types.freshVar())
                        |> Map.ofList

                    let envRec =
                        freshByName
                        |> Map.fold (fun acc name tv -> Map.add name (Forall([], tv)) acc) env

                    let mutable sRec = emptySubst
                    for (name, _, exprVal, bindingSpan) in foldedBindings do
                        let envForBinding = applyEnv sRec envRec
                        let s1, t1, _ = inferExpr telemetry typeDefs constructors envForBinding exprVal
                        let expected = applyType s1 (applyType sRec freshByName.[name])
                        let s2 = unify typeDefs expected t1 bindingSpan
                        sRec <- compose s2 (compose s1 sRec)

                    for (name, returnAnnotation, _, bindingSpan) in foldedBindings do
                        match returnAnnotation with
                        | Some tref ->
                            let expectedReturn = annotationTypeFromRef typeDefs bindingSpan tref
                            let inferredReturn = functionReturnType (applyType sRec freshByName.[name])
                            let sRet = unify typeDefs inferredReturn expectedReturn bindingSpan
                            sRec <- compose sRet sRec
                        | None -> ()

                    applySubstToCapturedLocals telemetry startIndex sRec

                    let envGeneralize = applyEnv sRec env
                    let schemes =
                        foldedBindings
                        |> List.map (fun (name, _, _, _) ->
                            let inferred = applyType sRec freshByName.[name]
                            validateMapKeyTypes typeDefs span inferred
                            name, Types.generalize envGeneralize inferred)

                    let typedBindings =
                        foldedBindings
                        |> List.map (fun (name, _, exprVal, bindingSpan) ->
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

                    let s1, t1, typedExpr = inferExpr telemetry typeDefs constructors env expr
                    let sDiscard =
                        if i < (program.Length - 1) then
                            unify typeDefs (applyType s1 t1) TUnit (Ast.spanOfExpr expr)
                        else
                            emptySubst
                    let s = compose sDiscard s1
                    applySubstToCapturedLocals telemetry startIndex s
                    validateMapKeyTypes typeDefs (Ast.spanOfExpr expr) (applyType s t1)
                    env <- applyEnv s env
                    typed.Add(TSExpr typedExpr)

            typed |> Seq.toList, capturedLocalVariableTypes telemetry
        with _ ->
            reraise ()

    let inferProgramWithExternsRaw (externs: ExternalFunction list) (program: Program) : TypedProgram =
        inferProgramWithExternsRawAndLocals false (HostInterop.ofExterns externs) program |> fst

    let inferProgramWithExterns (externs: ExternalFunction list) (program: Program) : TypedProgram =
        let reserved = BuiltinSignatures.builtinReservedNames

        externs
        |> List.tryFind (fun ext -> Set.contains ext.Name reserved)
        |> Option.iter (fun ext ->
            raise (TypeException { Message = $"Host extern '{ext.Name}' collides with reserved stdlib symbol"; Span = unknownSpan }))

        topLevelBindingNames program
        |> List.tryFind (fun name -> Set.contains name reserved)
        |> Option.iter (fun name ->
            raise (TypeException { Message = $"Top-level binding '{name}' collides with reserved stdlib symbol"; Span = unknownSpan }))

        inferProgramWithExternsRaw externs program

    let inferProgramWithExternsAndLocalVariableTypes (externs: ExternalFunction list) (program: Program) : TypedProgram * LocalVariableTypeInfo list =
        let reserved = BuiltinSignatures.builtinReservedNames

        externs
        |> List.tryFind (fun ext -> Set.contains ext.Name reserved)
        |> Option.iter (fun ext ->
            raise (TypeException { Message = $"Host extern '{ext.Name}' collides with reserved stdlib symbol"; Span = unknownSpan }))

        topLevelBindingNames program
        |> List.tryFind (fun name -> Set.contains name reserved)
        |> Option.iter (fun name ->
            raise (TypeException { Message = $"Top-level binding '{name}' collides with reserved stdlib symbol"; Span = unknownSpan }))

        inferProgramWithExternsRawAndLocals true (HostInterop.ofExterns externs) program

    let inferProgramWithLocalVariableTypes (program: Program) : TypedProgram * LocalVariableTypeInfo list =
        inferProgramWithExternsAndLocalVariableTypes [] program

    let inferProgram (program: Program) : TypedProgram =
        inferProgramWithExterns [] program

    let inferProgramWithRegistration (registration: ExtensionRegistration) program =
        // Preserve the existing reserved-name checks before the registration-aware inference pass.
        let reserved = BuiltinSignatures.builtinReservedNames
        if registration.Functions |> List.exists (fun ext -> Set.contains ext.Name reserved) then
            raise (TypeException { Message = "Host extern collides with reserved stdlib symbol"; Span = unknownSpan })
        if topLevelBindingNames program |> List.exists (fun name -> Set.contains name reserved) then
            raise (TypeException { Message = "Top-level binding collides with reserved stdlib symbol"; Span = unknownSpan })
        inferProgramWithExternsRawAndLocals false registration program |> fst

    let inferProgramWithRegistrationAndLocalVariableTypes (registration: ExtensionRegistration) program =
        let reserved = BuiltinSignatures.builtinReservedNames
        if registration.Functions |> List.exists (fun ext -> Set.contains ext.Name reserved) then
            raise (TypeException { Message = "Host extern collides with reserved stdlib symbol"; Span = unknownSpan })
        if topLevelBindingNames program |> List.exists (fun name -> Set.contains name reserved) then
            raise (TypeException { Message = "Top-level binding collides with reserved stdlib symbol"; Span = unknownSpan })
        inferProgramWithExternsRawAndLocals true registration program
