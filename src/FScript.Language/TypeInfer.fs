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
                | TVar v ->
                    match s |> Map.tryFind v with
                    | Some mapped -> apply mapped
                    | None -> current
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
            | TVar currentVar -> currentVar = v
            | TList item
            | TTask item
            | TOption item -> loop item
            | TTuple items -> items |> List.exists loop
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

    type ResolvedExpressionType =
        { Expr: Expr
          Type: Type
          Span: Span }

    type ResolvedSymbolReference =
        { SymbolId: int
          Name: string
          Kind: ResolvedSymbolKind
          Type: Type
          Span: Span }

    and ResolvedSymbolKind =
        | TypeSymbol
        | DeclarationSymbol
        | ParameterSymbol
        | PatternBindingSymbol
        | LocalSymbol
        | BuiltinSymbol
        | ExternSymbol
        | UnionCaseSymbol

    type ResolvedSymbol =
        { SymbolId: int
          Name: string
          Kind: ResolvedSymbolKind
          Span: Span
          Scheme: Scheme option }

    type ResolvedPrimitiveOperation =
        | IntegerAddition | FloatingAddition
        | IntegerSubtraction | FloatingSubtraction
        | IntegerMultiplication | FloatingMultiplication
        | IntegerDivision | FloatingDivision
        | IntegerRemainder | StructuralEquality
        | NumericComparison of string
        | BooleanOperation of string

    type RecordCoercionEvidence =
        { SourceType: Type
          TargetType: Type
          TargetName: string option }

    type ResolvedLiteral =
        | ResolvedUnit
        | ResolvedInt of int64
        | ResolvedFloat of float
        | ResolvedBool of bool
        | ResolvedString of string

    type ResolvedBinding =
        { SymbolId: int
          Name: string
          Type: Type
          Span: Span }

    type ResolvedExpression =
        { Type: Type
          Span: Span
          Reference: ResolvedSymbolReference option
          Primitive: ResolvedPrimitiveOperation option
          RecordCoercion: RecordCoercionEvidence option
          Node: ResolvedExpressionNode
          Children: ResolvedExpression list }

    and ResolvedExpressionNode =
        | ResolvedConstant of ResolvedLiteral
        | ResolvedReference of ResolvedSymbolReference
        | ResolvedLambda of ResolvedBinding * ResolvedExpression
        | ResolvedApply of ResolvedExpression * ResolvedExpression
        | ResolvedIf of ResolvedExpression * ResolvedExpression * ResolvedExpression
        | ResolvedPrimitive of ResolvedPrimitiveOperation * ResolvedExpression list
        | ResolvedTuple of ResolvedExpression list
        | ResolvedList of ResolvedExpression list
        | ResolvedRecord of (string * ResolvedExpression) list
        | ResolvedFieldGet of ResolvedExpression * string
        | ResolvedSome of ResolvedExpression
        | ResolvedNone
        | ResolvedCons of ResolvedExpression * ResolvedExpression
        | ResolvedAppend of ResolvedExpression * ResolvedExpression
        | ResolvedLet of ResolvedBinding * bool * ResolvedExpression * ResolvedExpression
        | ResolvedLetRecursiveGroup of ResolvedRecursiveBinding list * ResolvedExpression
        | ResolvedFail of ResolvedExpression
        | ResolvedMatch of ResolvedExpression * ResolvedMatchCase list
        | ResolvedRange of ResolvedExpression * ResolvedExpression
        | ResolvedRecordUpdate of bool * ResolvedExpression * (string * ResolvedExpression) list
        | ResolvedIndexGet of ResolvedExpression * ResolvedExpression
        | ResolvedInterpolate of ResolvedInterpolationPart list
        | ResolvedMap of ResolvedMapEntry list
        | ResolvedForEach of ResolvedBinding * ResolvedExpression * ResolvedExpression
        | ResolvedTypeToken of Type

    and ResolvedRecursiveBinding =
        { Binding: ResolvedBinding
          Value: ResolvedExpression }

    and ResolvedInterpolationPart =
        | ResolvedTextPart of string
        | ResolvedValuePart of ResolvedExpression

    and ResolvedMapEntry =
        | ResolvedKeyValue of ResolvedExpression * ResolvedExpression
        | ResolvedSpread of ResolvedExpression

    and ResolvedPattern =
        { Type: Type
          Span: Span
          Node: ResolvedPatternNode }

    and ResolvedPatternNode =
        | ResolvedWildcard
        | ResolvedPatternBinding of ResolvedBinding
        | ResolvedLiteralPattern of ResolvedLiteral
        | ResolvedEmptyList
        | ResolvedConsPattern of ResolvedPattern * ResolvedPattern
        | ResolvedTuplePattern of ResolvedPattern list
        | ResolvedRecordPattern of (string * ResolvedPattern) list
        | ResolvedMapPattern of (ResolvedPattern * ResolvedPattern) list * ResolvedPattern option
        | ResolvedSomePattern of ResolvedPattern
        | ResolvedNonePattern
        | ResolvedUnionPattern of string * string * ResolvedPattern option
        | ResolvedTypePattern of Type

    and ResolvedMatchCase =
        { Pattern: ResolvedPattern
          Guard: ResolvedExpression option
          Body: ResolvedExpression
          Span: Span }

    type private LocalVariableTypeCapture =
        { Name: string
          Span: Span
          mutable Type: Type }
    type TypedStmt =
        | TSType of TypeDef
        | TSLet of string * Expr * Type * bool * bool * Span
        | TSLetPattern of Pattern * Expr * Map<string, Type> * bool * Span
        | TSLetRecGroup of (string * Expr * Type * Span) list * bool * Span
        | TSBackendImport of string * Type * BackendImport * Span
        | TSExpr of TypedExpr
    type TypedProgram = TypedStmt list

    type ResolvedBackendImport =
        { ModulePath: string
          MemberName: string
          Span: Span }

    type ResolvedTypeDeclaration =
        { Binding: ResolvedBinding
          IsRecursive: bool
          Definition: Type
          Fields: (string * Type) list
          Cases: (string * Type option) list
          Import: ResolvedBackendImport option }

    type ResolvedValueDeclaration =
        { Binding: ResolvedBinding
          Scheme: Scheme
          IsRecursive: bool
          Value: ResolvedExpression }

    type ResolvedPatternDeclaration =
        { Pattern: ResolvedPattern
          Value: ResolvedExpression
          Bindings: (ResolvedBinding * Scheme) list }

    type ResolvedImportedDeclaration =
        { Binding: ResolvedBinding
          Scheme: Scheme
          Import: ResolvedBackendImport }

    type ResolvedStatementNode =
        | ResolvedTypeDeclaration of ResolvedTypeDeclaration
        | ResolvedValueDeclaration of ResolvedValueDeclaration
        | ResolvedPatternDeclaration of ResolvedPatternDeclaration
        | ResolvedRecursiveDeclarations of ResolvedValueDeclaration list
        | ResolvedImportedDeclaration of ResolvedImportedDeclaration
        | ResolvedExpressionStatement of ResolvedExpression

    type ResolvedStatement =
        { Span: Span
          Attributes: Attribute list
          Export: ResolvedExport option
          Node: ResolvedStatementNode
          Patterns: ResolvedPattern list
          Expressions: ResolvedExpression list }

    and ResolvedExport =
        { Name: string
          Span: Span }

    type ResolvedProgram =
        { TypedProgram: TypedProgram
          ResolvedStatements: ResolvedStatement list
          ExpressionTypes: ResolvedExpressionType list
          Symbols: ResolvedSymbol list
          SymbolReferences: ResolvedSymbolReference list }

    type private InferenceTelemetry =
        { mutable ScopeStack: Set<string> list
          LocalVariableTypes: ResizeArray<LocalVariableTypeCapture>
          ExpressionTypes: ResizeArray<ResolvedExpressionType> }

    let private createTelemetry () =
        { ScopeStack = [ Set.empty ]
          LocalVariableTypes = ResizeArray()
          ExpressionTypes = ResizeArray() }

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

    let private applySubstToCapturedExpressions (telemetry: InferenceTelemetry option) (startIndex: int) (subst: Subst) =
        match telemetry with
        | Some t ->
            for i = startIndex to t.ExpressionTypes.Count - 1 do
                let item = t.ExpressionTypes.[i]
                t.ExpressionTypes.[i] <- { item with Type = applyType subst item.Type }
        | None -> ()

    let private currentTelemetryCaptureCount (telemetry: InferenceTelemetry option) =
        match telemetry with
        | Some t -> t.LocalVariableTypes.Count
        | None -> 0

    type private ConstructorSig =
        { UnionName: string
          Payload: Type option }

    let private asTyped (telemetry: InferenceTelemetry option) expr t : TypedExpr =
        let span = Ast.spanOfExpr expr
        match telemetry with
        | Some capture ->
            capture.ExpressionTypes.Add
                { Expr = expr
                  Type = t
                  Span = span }
        | None -> ()
        { Expr = expr; Type = t; Span = span }

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
        | TRPostfix (inner, "task") -> TTask (annotationTypeFromRef typeDefs span inner)
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
            env, TRecord fieldTypes
        | PMap (clauses, tailPattern, span) ->
            let startIndex = currentTelemetryCaptureCount telemetry
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
            | TTask inner -> loop inner
            | TTuple items -> items |> List.iter loop
            | TRecord fields -> fields |> Map.values |> Seq.iter loop
            | TOption inner -> loop inner
            | TFun (a, b) -> loop a; loop b
            | _ -> ()
        loop t

    let rec private inferExpr (telemetry: InferenceTelemetry option) (typeDefs: Map<string, Type>) (constructors: Map<string, ConstructorSig>) (env: Map<string, Scheme>) (expr: Expr) : Subst * Type * TypedExpr =
        match expr with
        | EUnit _ ->
            emptySubst, TUnit, asTyped telemetry expr TUnit
        | ELiteral (lit, _) ->
            let t =
                match lit with
                | LInt _ -> TInt
                | LFloat _ -> TFloat
                | LBool _ -> TBool
                | LString _ -> TString
            emptySubst, t, asTyped telemetry expr t
        | EVar (name, span) ->
            match env |> Map.tryFind name with
            | Some scheme ->
                let t = instantiate scheme
                recordLocalVariableUse telemetry name span t
                emptySubst, t, asTyped telemetry expr t
            | None -> raise (TypeException { Message = sprintf "Unbound variable '%s'" name; Span = span })
        | EParen (inner, _) ->
            let s, t, _ = inferExpr telemetry typeDefs constructors env inner
            s, t, asTyped telemetry expr t
        | ETypeOf (name, span) ->
            if typeDefs.ContainsKey name then
                emptySubst, TTypeToken, asTyped telemetry expr TTypeToken
            else
                raise (TypeException { Message = sprintf "Unknown type '%s'" name; Span = span })
        | ENameOf (name, span) ->
            if env.ContainsKey name then
                emptySubst, TString, asTyped telemetry expr TString
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
            sAcc, TString, asTyped telemetry expr TString
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
            s1, tFun, asTyped telemetry expr tFun
        | EApply (fn, arg, span) ->
            let s1, t1, _ = inferExpr telemetry typeDefs constructors env fn
            let env2 = applyEnv s1 env
            let s2, t2, _ = inferExpr telemetry typeDefs constructors env2 arg
            let tv = Types.freshVar()
            let s3 = unify typeDefs (applyType s2 t1) (TFun (t2, tv)) span
            let s = compose s3 (compose s2 s1)
            let tRes = applyType s tv
            s, tRes, asTyped telemetry expr tRes
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
            s, tRes, asTyped telemetry expr tRes
        | ERaise (value, span) ->
            let s1, t1, _ = inferExpr telemetry typeDefs constructors env value
            let s2 = unify typeDefs (applyType s1 t1) TString span
            let s = compose s2 s1
            let tRes = Types.freshVar()
            s, tRes, asTyped telemetry expr tRes
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
            s, TUnit, asTyped telemetry expr TUnit
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
                s, tBody, asTyped telemetry expr tBody
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
                s, t2, asTyped telemetry expr t2
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
            s, tBody, asTyped telemetry expr tBody
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
            s, tBody, asTyped telemetry expr tBody
        | EMatch (scrutinee, cases, span) ->
            let s1, tScrut, _ = inferExpr telemetry typeDefs constructors env scrutinee
            let mutable sAcc = s1
            let mutable resultTypeOpt : Type option = None
            for (pat, guard, body, caseSpan) in cases do
                let envPat, tPat = inferPattern telemetry typeDefs constructors pat
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
            sAcc, tRes, asTyped telemetry expr tRes
        | EList (items, span) ->
            let tv = Types.freshVar()
            let mutable sAcc = emptySubst
            for item in items do
                let s1, t1, _ = inferExpr telemetry typeDefs constructors (applyEnv sAcc env) item
                let s2 = unify typeDefs (applyType s1 tv) t1 span
                sAcc <- compose s2 (compose s1 sAcc)
            let tRes = TList (applyType sAcc tv)
            sAcc, tRes, asTyped telemetry expr tRes
        | ERange (startExpr, endExpr, span) ->
            let s1, tStart, _ = inferExpr telemetry typeDefs constructors env startExpr
            let env2 = applyEnv s1 env
            let s2, tEnd, _ = inferExpr telemetry typeDefs constructors env2 endExpr
            let s3 = unify typeDefs (applyType s2 tStart) TInt span
            let s4 = unify typeDefs (applyType s3 (applyType s2 tEnd)) TInt span
            let s = compose s4 (compose s3 (compose s2 s1))
            let tRes = TList TInt
            s, tRes, asTyped telemetry expr tRes
        | ETuple (items, _) ->
            let mutable sAcc = emptySubst
            let mutable inferred : Type list = []
            for item in items do
                let s1, t1, _ = inferExpr telemetry typeDefs constructors (applyEnv sAcc env) item
                sAcc <- compose s1 sAcc
                inferred <- inferred @ [ applyType sAcc t1 ]
            let tRes = TTuple inferred
            sAcc, tRes, asTyped telemetry expr tRes
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
            s, tRes, asTyped telemetry expr tRes
        | EStructuralRecord (fields, _) ->
            let mutable sAcc = emptySubst
            let mutable inferred : Map<string, Type> = Map.empty
            for (name, valueExpr) in fields do
                let s1, t1, _ = inferExpr telemetry typeDefs constructors (applyEnv sAcc env) valueExpr
                sAcc <- compose s1 sAcc
                inferred <- inferred.Add(name, applyType sAcc t1)
            let tRes = TRecord inferred
            sAcc, tRes, asTyped telemetry expr tRes
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
            sAcc, tRes, asTyped telemetry expr tRes
        | ERecordUpdate (baseExpr, updates, span) ->
            let sBase, tBase, _ = inferExpr telemetry typeDefs constructors env baseExpr
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
                let sField, tField, _ = inferExpr telemetry typeDefs constructors envForField valueExpr
                let expected = applyType sField (applyType sAcc expectedFieldType)
                let sEq = unify typeDefs expected tField (Ast.spanOfExpr valueExpr)
                sAcc <- compose sEq (compose sField sAcc)
            let resultFields = baseFields |> Map.map (fun _ t -> applyType sAcc t)
            let tRes = TRecord resultFields
            sAcc, tRes, asTyped telemetry expr tRes
        | EStructuralRecordUpdate (baseExpr, updates, span) ->
            let sBase, tBase, _ = inferExpr telemetry typeDefs constructors env baseExpr
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
            sAcc, tRes, asTyped telemetry expr tRes
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
                    emptySubst, t, asTyped telemetry expr t
                | None ->
                    let s1, tTarget, _ = inferExpr telemetry typeDefs constructors env target
                    let sField, tField = inferRecordField s1 tTarget
                    sField, tField, asTyped telemetry expr tField
            | _ ->
                let s1, tTarget, _ = inferExpr telemetry typeDefs constructors env target
                let sField, tField = inferRecordField s1 tTarget
                sField, tField, asTyped telemetry expr tField
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
                s, tRes, asTyped telemetry expr tRes
            | TMap (mapKeyType, valueType), _ ->
                let s3 = unify typeDefs keyType mapKeyType span
                let s = compose s3 (compose s2 s1)
                match applyType s mapKeyType with
                | TString
                | TVar _ -> ()
                | t ->
                    raise (TypeException { Message = $"Map key type must be string, got {Types.typeToString t}"; Span = span })
                let tRes = TOption (applyType s valueType)
                s, tRes, asTyped telemetry expr tRes
            | TVar _, TInt ->
                let itemType = Types.freshVar()
                let s3 = unify typeDefs targetType (TList itemType) span
                let s = compose s3 (compose s2 s1)
                let tRes = TOption (applyType s itemType)
                s, tRes, asTyped telemetry expr tRes
            | TVar _, TString ->
                let valueType = Types.freshVar()
                let s3 = unify typeDefs targetType (TMap (TString, valueType)) span
                let s = compose s3 (compose s2 s1)
                let tRes = TOption (applyType s valueType)
                s, tRes, asTyped telemetry expr tRes
            | TVar _, TVar _ ->
                raise (TypeException { Message = "Ambiguous index access: use a list with int index or a map with string key"; Span = span })
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
                let s = compose s3 (compose s2 s1)
                let tRes = applyType s tv
                s, tRes, asTyped telemetry expr tRes
            | "::" ->
                let s1, tHead, _ = inferExpr telemetry typeDefs constructors env a
                let env2 = applyEnv s1 env
                let s2, tTail, _ = inferExpr telemetry typeDefs constructors env2 b
                let s3 = unify typeDefs (applyType s2 tTail) (TList tHead) span
                let s = compose s3 (compose s2 s1)
                let tRes = applyType s (TList tHead)
                s, tRes, asTyped telemetry expr tRes
            | "@" ->
                let s1, t1, _ = inferExpr telemetry typeDefs constructors env a
                let env2 = applyEnv s1 env
                let s2, t2, _ = inferExpr telemetry typeDefs constructors env2 b
                let tv = Types.freshVar()
                let s3 = unify typeDefs (applyType s2 t1) (TList tv) span
                let s4 = unify typeDefs (applyType s3 (applyType s2 t2)) (TList (applyType s3 tv)) span
                let s = compose s4 (compose s3 (compose s2 s1))
                let tRes = TList (applyType s tv)
                s, tRes, asTyped telemetry expr tRes
            | _ ->
                let s1, t1, _ = inferExpr telemetry typeDefs constructors env a
                let env2 = applyEnv s1 env
                let s2, t2, _ = inferExpr telemetry typeDefs constructors env2 b
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
                s, resultType, asTyped telemetry expr resultType
        | ESome (value, _) ->
            let s1, t1, _ = inferExpr telemetry typeDefs constructors env value
            s1, TOption t1, asTyped telemetry expr (TOption t1)
        | ENone _ ->
            let tv = Types.freshVar()
            emptySubst, TOption tv, asTyped telemetry expr (TOption tv)
        | ECons (head, tail, span) ->
            let s1, t1, _ = inferExpr telemetry typeDefs constructors env head
            let env2 = applyEnv s1 env
            let s2, t2, _ = inferExpr telemetry typeDefs constructors env2 tail
            let s3 = unify typeDefs (applyType s2 t2) (TList t1) span
            let s = compose s3 (compose s2 s1)
            let tRes = applyType s (TList t1)
            s, tRes, asTyped telemetry expr tRes
        | EAppend (a, b, span) ->
            let s1, t1, _ = inferExpr telemetry typeDefs constructors env a
            let env2 = applyEnv s1 env
            let s2, t2, _ = inferExpr telemetry typeDefs constructors env2 b
            let tv = Types.freshVar()
            let s3 = unify typeDefs (applyType s2 t1) (TList tv) span
            let s4 = unify typeDefs (applyType s3 (applyType s2 t2)) (TList (applyType s3 tv)) span
            let s = compose s4 (compose s3 (compose s2 s1))
            let tRes = TList (applyType s tv)
            s, tRes, asTyped telemetry expr tRes

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

    let rec private unwrapStatementAttributes statement =
        match statement with
        | SAttributed(attributes, inner, _) ->
            let nested, unwrapped = unwrapStatementAttributes inner
            attributes @ nested, unwrapped
        | value -> [], value

    let private unwrapProgramAttributes (program: Program) =
        program |> List.map unwrapStatementAttributes |> List.unzip

    let private topLevelBindingNames (program: Program) : string list =
        program
        |> List.collect (unwrapStatementAttributes >> snd >> function
            | SLet(name, _, _, _, _, _, _) -> [ name ]
            | SLetPattern(pat, _, _, _) -> patternBindingNames pat
            | SLetRecGroup(bindings, _, _) -> bindings |> List.map (fun (name, _, _, _, _) -> name)
            | _ -> [])

    let private inferProgramWithExternsRawAndLocals (captureTelemetry: bool) (externs: ExternalFunction list) (program: Program) : TypedProgram * LocalVariableTypeInfo list * ResolvedExpressionType list =
        let _, program = unwrapProgramAttributes program
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

        let telemetry =
            if captureTelemetry then Some(createTelemetry ())
            else None

        let typed = ResizeArray<TypedStmt>()
        try
            for i, stmt in program |> List.indexed do
                let expressionStartIndex =
                    match telemetry with
                    | Some t -> t.ExpressionTypes.Count
                    | None -> 0
                match stmt with
                | SType def ->
                    typed.Add(TSType def)
                | SImport (_, _, span) ->
                    raise (TypeException { Message = "'import' must be resolved before type inference"; Span = span })
                | SAttributed(_, _, span) ->
                    raise (TypeException { Message = "Attributed statements must be normalized before type inference"; Span = span })
                | SBackendImport(name, signature, import, span) ->
                    let importedType = annotationTypeFromRef typeDefs span signature
                    match importedType with
                    | TFun _ ->
                        env <- Map.add name (Forall([], importedType)) env
                        typed.Add(TSBackendImport(name, importedType, import, span))
                    | _ -> raise (TypeException { Message = "Backend imports require a complete function signature"; Span = span })
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
                        applySubstToCapturedExpressions telemetry expressionStartIndex s
                        let env' = applyEnv s env
                        let inferred = applyType s t1
                        validateMapKeyTypes span inferred
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
                        applySubstToCapturedExpressions telemetry expressionStartIndex s
                        let env' = applyEnv s env
                        let inferred = applyType s t1
                        validateMapKeyTypes span inferred
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
                    applySubstToCapturedExpressions telemetry expressionStartIndex s
                    let envBase = applyEnv s env
                    let typedBindings = envPat |> Map.map (fun _ ty -> applyType s ty)
                    typedBindings
                    |> Map.iter (fun _ ty -> validateMapKeyTypes span ty)
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
                    applySubstToCapturedExpressions telemetry expressionStartIndex sRec

                    let envGeneralize = applyEnv sRec env
                    let schemes =
                        foldedBindings
                        |> List.map (fun (name, _, _, _) ->
                            let inferred = applyType sRec freshByName.[name]
                            validateMapKeyTypes span inferred
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
                    applySubstToCapturedExpressions telemetry expressionStartIndex s
                    validateMapKeyTypes (Ast.spanOfExpr expr) (applyType s t1)
                    env <- applyEnv s env
                    typed.Add(TSExpr typedExpr)

            let expressionTypes =
                match telemetry with
                | Some value -> value.ExpressionTypes |> Seq.toList
                | None -> []
            typed |> Seq.toList, capturedLocalVariableTypes telemetry, expressionTypes
        with _ ->
            reraise ()

    let inferProgramWithExternsRaw (externs: ExternalFunction list) (program: Program) : TypedProgram =
        let typed, _, _ = inferProgramWithExternsRawAndLocals false externs program
        typed

    let private validateProgramBindings (externs: ExternalFunction list) (program: Program) =
        let reserved = BuiltinSignatures.builtinReservedNames

        externs
        |> List.tryFind (fun ext -> Set.contains ext.Name reserved)
        |> Option.iter (fun ext ->
            raise (TypeException { Message = $"Host extern '{ext.Name}' collides with reserved stdlib symbol"; Span = unknownSpan }))

        topLevelBindingNames program
        |> List.tryFind (fun name -> Set.contains name reserved)
        |> Option.iter (fun name ->
            raise (TypeException { Message = $"Top-level binding '{name}' collides with reserved stdlib symbol"; Span = unknownSpan }))

    let inferProgramWithExternsAndLocalVariableTypes (externs: ExternalFunction list) (program: Program) : TypedProgram * LocalVariableTypeInfo list =
        validateProgramBindings externs program
        let typed, locals, _ = inferProgramWithExternsRawAndLocals true externs program
        typed, locals

    let inferResolvedProgramWithExterns (externs: ExternalFunction list) (program: Program) : ResolvedProgram =
        validateProgramBindings externs program
        let statementAttributes, semanticProgram = unwrapProgramAttributes program
        let typed, _, expressions = inferProgramWithExternsRawAndLocals true externs semanticProgram
        let expressionTypes = expressions |> List.fold (fun state item -> Map.add item.Expr item.Type state) Map.empty
        let mutable nextSymbol = 0
        let symbols = ResizeArray<ResolvedSymbol>()
        let references = ResizeArray<ResolvedSymbolReference>()
        let patternBindingIds = ResizeArray<Span * int>()
        let normalizeScheme (Forall(variables, schemeType)) =
            let mapping = variables |> List.mapi (fun index variable -> variable, index) |> Map.ofList
            let rec normalize = function
                | TVar variable -> TVar(mapping |> Map.tryFind variable |> Option.defaultValue variable)
                | TList item -> TList(normalize item) | TTask item -> TTask(normalize item) | TOption item -> TOption(normalize item)
                | TTuple items -> TTuple(List.map normalize items)
                | TRecord fields -> TRecord(Map.map (fun _ value -> normalize value) fields)
                | TMap(key, value) -> TMap(normalize key, normalize value)
                | TFun(argument, result) -> TFun(normalize argument, normalize result)
                | TUnion(name, cases) -> TUnion(name, Map.map (fun _ payload -> Option.map normalize payload) cases)
                | value -> value
            Forall([ 0 .. variables.Length - 1 ], normalize schemeType)
        let normalizeTypeVariables value =
            let mutable next = 0
            let mutable mapping = Map.empty
            let rec normalize = function
                | TVar variable ->
                    match Map.tryFind variable mapping with
                    | Some normalized -> TVar normalized
                    | None ->
                        let normalized = next
                        next <- next + 1
                        mapping <- Map.add variable normalized mapping
                        TVar normalized
                | TList item -> TList(normalize item) | TTask item -> TTask(normalize item) | TOption item -> TOption(normalize item)
                | TTuple items -> TTuple(List.map normalize items)
                | TRecord fields -> TRecord(Map.map (fun _ item -> normalize item) fields)
                | TMap(key, item) -> TMap(normalize key, normalize item)
                | TFun(argument, result) -> TFun(normalize argument, normalize result)
                | TUnion(name, cases) -> TUnion(name, Map.map (fun _ payload -> Option.map normalize payload) cases)
                | item -> item
            normalize value
        let allocate name kind span scheme =
            let id = nextSymbol
            nextSymbol <- nextSymbol + 1
            symbols.Add { SymbolId = id; Name = name; Kind = kind; Span = span; Scheme = Option.map normalizeScheme scheme }
            id
        let syntheticSpan = Span.mk (Span.pos 0 0) (Span.pos 0 0)
        let mutable globalEnvironment = Map.empty
        for name, scheme in BuiltinSignatures.builtinSchemes |> Map.toList |> List.sortBy fst do
            globalEnvironment <- Map.add name (allocate name BuiltinSymbol syntheticSpan (Some scheme)) globalEnvironment
        for external in externs |> List.sortBy _.Name do
            globalEnvironment <- Map.add external.Name (allocate external.Name ExternSymbol syntheticSpan (Some external.Scheme)) globalEnvironment
        for statement in typed do
            match statement with
            | TSType definition ->
                globalEnvironment <- Map.add definition.Name (allocate definition.Name TypeSymbol definition.Span None) globalEnvironment
                for caseName, _ in definition.Cases do
                    let id = allocate $"{definition.Name}.{caseName}" UnionCaseSymbol definition.Span None
                    globalEnvironment <- globalEnvironment |> Map.add caseName id |> Map.add $"{definition.Name}.{caseName}" id
            | TSLet(name, _, inferred, _, _, span) ->
                globalEnvironment <- Map.add name (allocate name DeclarationSymbol span (Some(Types.generalize Map.empty inferred))) globalEnvironment
            | TSLetPattern(_, _, bindings, _, span) ->
                for name, inferred in bindings |> Map.toList do
                    globalEnvironment <- Map.add name (allocate name DeclarationSymbol span (Some(Types.generalize Map.empty inferred))) globalEnvironment
            | TSLetRecGroup(bindings, _, _) ->
                for name, _, inferred, span in bindings do
                    globalEnvironment <- Map.add name (allocate name DeclarationSymbol span (Some(Types.generalize Map.empty inferred))) globalEnvironment
            | TSBackendImport(name, importedType, _, span) ->
                globalEnvironment <- Map.add name (allocate name DeclarationSymbol span (Some(Forall([], importedType)))) globalEnvironment
            | TSExpr _ -> ()
        let recordReference environment name expression span =
            environment |> Map.tryFind name |> Option.iter (fun id ->
                let symbol = symbols |> Seq.find (fun symbol -> symbol.SymbolId = id)
                references.Add
                    { SymbolId = id
                      Name = name
                      Kind = symbol.Kind
                      Type = expressionTypes |> Map.tryFind expression |> Option.defaultValue TUnit |> normalizeTypeVariables
                      Span = span })
        let bindPattern kind environment pattern =
            let rec bind bindings current pattern =
                match pattern with
                | PVar(name, span) ->
                    match Map.tryFind name bindings with
                    | Some id ->
                        patternBindingIds.Add(span, id)
                        bindings, Map.add name id current
                    | None ->
                        let id = allocate name kind span None
                        patternBindingIds.Add(span, id)
                        Map.add name id bindings, Map.add name id current
                | PCons(head, tail, _) ->
                    let bindings, current = bind bindings current head
                    bind bindings current tail
                | PTuple(items, _) -> items |> List.fold (fun (bindings, current) item -> bind bindings current item) (bindings, current)
                | PRecord(fields, _) -> fields |> List.fold (fun (bindings, current) (_, item) -> bind bindings current item) (bindings, current)
                | PMap(clauses, tail, _) ->
                    let bindings, current =
                        clauses
                        |> List.fold (fun (bindings, current) (key, value) ->
                            let bindings, current = bind bindings current key
                            bind bindings current value) (bindings, current)
                    tail |> Option.map (bind bindings current) |> Option.defaultValue (bindings, current)
                | PSome(item, _) -> bind bindings current item
                | PUnionCase(_, _, payload, _) -> payload |> Option.map (bind bindings current) |> Option.defaultValue (bindings, current)
                | _ -> bindings, current
            bind Map.empty environment pattern |> snd
        let rec visit environment expression =
            let visitSame item = visit environment item
            match expression with
            | EVar(name, span) -> recordReference environment name expression span
            | ENameOf(name, span) -> recordReference environment name expression span
            | ETypeOf(name, span) -> recordReference environment name expression span
            | ELambda(parameter, body, _) ->
                let id = allocate parameter.Name ParameterSymbol parameter.Span None
                visit (Map.add parameter.Name id environment) body
            | EApply(fn, argument, _) | EBinOp(_, fn, argument, _) | EAppend(fn, argument, _) | ECons(fn, argument, _) | EIndexGet(fn, argument, _) -> visitSame fn; visitSame argument
            | EIf(condition, whenTrue, whenFalse, _) -> visitSame condition; visitSame whenTrue; visitSame whenFalse
            | ELet(name, binding, body, recursive, _, span) ->
                let id = allocate name LocalSymbol span None
                visit (if recursive then Map.add name id environment else environment) binding
                visit (Map.add name id environment) body
            | ELetPattern(pattern, binding, body, _) -> visitSame binding; visit (bindPattern PatternBindingSymbol environment pattern) body
            | ELetRecGroup(bindings, body, _) ->
                let recursiveEnvironment = bindings |> List.fold (fun env (name, _, _, _, span) -> Map.add name (allocate name LocalSymbol span None) env) environment
                for _, parameters, _, value, span in bindings do
                    let folded = Seq.foldBack (fun parameter inner -> ELambda(parameter, inner, span)) parameters value
                    visit recursiveEnvironment folded
                visit recursiveEnvironment body
            | EMatch(target, cases, _) ->
                visitSame target
                for pattern, guard, body, _ in cases do
                    let caseEnvironment = bindPattern PatternBindingSymbol environment pattern
                    Option.iter (visit caseEnvironment) guard
                    visit caseEnvironment body
            | EFor(name, source, body, span) ->
                visitSame source
                let id = allocate name LocalSymbol span None
                visit (Map.add name id environment) body
            | EList(items, _) | ETuple(items, _) -> List.iter visitSame items
            | ERecord(fields, _) | EStructuralRecord(fields, _) -> fields |> List.iter (snd >> visitSame)
            | EMap(entries, _) -> entries |> List.iter (function MEKeyValue(key, value) -> visitSame key; visitSame value | MESpread value -> visitSame value)
            | ERecordUpdate(target, fields, _) | EStructuralRecordUpdate(target, fields, _) -> visitSame target; fields |> List.iter (snd >> visitSame)
            | EFieldGet(EVar(moduleName, _), fieldName, span) when Map.containsKey $"{moduleName}.{fieldName}" environment -> recordReference environment $"{moduleName}.{fieldName}" expression span
            | EFieldGet(target, _, _) -> visitSame target
            | ESome(item, _) | EParen(item, _) | ERaise(item, _) -> visitSame item
            | ERange(first, last, _) -> visitSame first; visitSame last
            | EInterpolatedString(parts, _) -> parts |> List.iter (function IPText _ -> () | IPExpr item -> visitSame item)
            | EUnit _ | ELiteral _ | ENone _ -> ()
        for statement in typed do
            match statement with
            | TSLet(_, expression, _, _, _, _) -> visit globalEnvironment expression
            | TSLetPattern(pattern, expression, _, _, _) -> visit globalEnvironment expression; bindPattern PatternBindingSymbol globalEnvironment pattern |> ignore
            | TSLetRecGroup(bindings, _, _) -> bindings |> List.iter (fun (_, expression, _, _) -> visit globalEnvironment expression)
            | TSExpr expression -> visit globalEnvironment expression.Expr
            | TSBackendImport _ -> ()
            | TSType _ -> ()
        let resolvedReferences = references |> Seq.toList
        let finalType expression = expressionTypes |> Map.tryFind expression |> Option.defaultValue TUnit |> normalizeTypeVariables
        let declarations =
            (BuiltinTypes.typeDefs |> List.map (fun definition -> definition.Name, definition))
            @ (typed |> List.choose (function TSType definition -> Some(definition.Name, definition) | _ -> None))
            |> Map.ofList
        let resolvedTypeDefinitions =
            declarations
            |> Map.map (fun name definition ->
                if definition.Cases.IsEmpty then
                    definition.Fields
                    |> List.map (fun (fieldName, fieldType) -> fieldName, typeFromRef declarations [ name ] fieldType)
                    |> Map.ofList
                    |> TRecord
                else
                    definition.Cases
                    |> List.map (fun (caseName, payload) -> caseName, Option.map (typeFromRef declarations [ name ]) payload)
                    |> Map.ofList
                    |> fun cases -> TUnion(name, cases))
        let resolvedLiteral = function
            | LInt value -> ResolvedInt value
            | LFloat value -> ResolvedFloat value
            | LBool value -> ResolvedBool value
            | LString value -> ResolvedString value
        let symbolAt kind name span =
            symbols
            |> Seq.find (fun symbol -> symbol.Kind = kind && symbol.Name = name && symbol.Span = span)
        let bindingAt kind name span bindingType =
            let id =
                if kind = PatternBindingSymbol then
                    patternBindingIds
                    |> Seq.tryFind (fst >> (=) span)
                    |> Option.map snd
                    |> Option.defaultWith (fun () -> (symbolAt kind name span).SymbolId)
                else
                    (symbolAt kind name span).SymbolId
            { SymbolId = id; Name = name; Type = bindingType; Span = span }
        let referenceAt expression =
            let span = Ast.spanOfExpr expression
            resolvedReferences |> List.tryFind (fun item -> item.Span = span)
        let rec resolvePattern (expected: Type) (pattern: Pattern) : ResolvedPattern =
            let span = Ast.spanOfPattern pattern
            let make node : ResolvedPattern = { Type = expected; Span = span; Node = node }
            match pattern with
            | PWildcard _ -> make ResolvedWildcard
            | PVar(name, _) -> make (ResolvedPatternBinding(bindingAt PatternBindingSymbol name span expected))
            | PLiteral(value, _) -> make (ResolvedLiteralPattern(resolvedLiteral value))
            | PNil _ -> make ResolvedEmptyList
            | PCons(head, tail, _) ->
                let itemType = match expected with TList item -> item | _ -> Types.freshVar()
                make (ResolvedConsPattern(resolvePattern itemType head, resolvePattern expected tail))
            | PTuple(items, _) ->
                let itemTypes = match expected with TTuple types when types.Length = items.Length -> types | _ -> List.replicate items.Length (Types.freshVar())
                make (ResolvedTuplePattern(List.map2 resolvePattern itemTypes items))
            | PRecord(fields, _) ->
                let fieldTypes =
                    match expected with
                    | TRecord values -> values
                    | TNamed name -> match Map.tryFind name resolvedTypeDefinitions with Some(TRecord values) -> values | _ -> Map.empty
                    | _ -> Map.empty
                fields
                |> List.map (fun (name, item) -> name, resolvePattern (fieldTypes |> Map.tryFind name |> Option.defaultValue (Types.freshVar())) item)
                |> ResolvedRecordPattern
                |> make
            | PMap(clauses, tail, _) ->
                let keyType, valueType = match expected with TMap(key, value) -> key, value | _ -> Types.freshVar(), Types.freshVar()
                let resolvedClauses = clauses |> List.map (fun (key, value) -> resolvePattern keyType key, resolvePattern valueType value)
                make (ResolvedMapPattern(resolvedClauses, Option.map (resolvePattern expected) tail))
            | PSome(item, _) ->
                let itemType = match expected with TOption value -> value | _ -> Types.freshVar()
                make (ResolvedSomePattern(resolvePattern itemType item))
            | PNone _ -> make ResolvedNonePattern
            | PUnionCase(qualifier, caseName, payload, _) ->
                let unionName, payloadType =
                    match expected with
                    | TNamed name ->
                        match Map.tryFind name resolvedTypeDefinitions with
                        | Some(TUnion(_, cases)) -> name, cases |> Map.tryFind caseName |> Option.flatten
                        | _ -> qualifier |> Option.defaultValue name, None
                    | TUnion(name, cases) -> name, cases |> Map.tryFind caseName |> Option.flatten
                    | _ ->
                        resolvedTypeDefinitions
                        |> Map.toSeq
                        |> Seq.tryPick (fun (name, definition) ->
                            match definition with
                            | TUnion(_, cases) when Map.containsKey caseName cases -> Some(name, cases.[caseName])
                            | _ -> None)
                        |> Option.defaultValue (qualifier |> Option.defaultValue "", None)
                make (ResolvedUnionPattern(unionName, caseName, Option.map (resolvePattern (payloadType |> Option.defaultValue (Types.freshVar()))) payload))
            | PTypeRef(typeRef, _) -> make (ResolvedTypePattern(annotationTypeFromRef resolvedTypeDefinitions span typeRef))
        let rec resolveExpression (expression: Expr) : ResolvedExpression =
            let expressionType = finalType expression
            let reference = referenceAt expression
            let primitive =
                match expression with
                | EBinOp(op, left, _, _) ->
                    match op, expressionType with
                    | "+", TInt -> Some IntegerAddition | "+", TFloat -> Some FloatingAddition
                    | "-", TInt -> Some IntegerSubtraction | "-", TFloat -> Some FloatingSubtraction
                    | "*", TInt -> Some IntegerMultiplication | "*", TFloat -> Some FloatingMultiplication
                    | "/", TInt -> Some IntegerDivision | "/", TFloat -> Some FloatingDivision
                    | "%", TInt -> Some IntegerRemainder | "=", _ -> Some StructuralEquality
                    | ("<" | ">" | "<=" | ">="), _ -> Some(NumericComparison op)
                    | ("&&" | "||"), _ -> Some(BooleanOperation op)
                    | _ -> None
                | _ -> None
            let node : ResolvedExpressionNode =
                match expression with
                | EUnit _ -> ResolvedConstant ResolvedUnit
                | ELiteral(value, _) -> ResolvedConstant(resolvedLiteral value)
                | EVar _ -> ResolvedReference(Option.get reference)
                | EParen(inner, _) -> (resolveExpression inner).Node
                | ELambda(parameter, body, _) ->
                    let parameterType = match expressionType with TFun(argument, _) -> argument | _ -> Types.freshVar()
                    ResolvedLambda(bindingAt ParameterSymbol parameter.Name parameter.Span parameterType, resolveExpression body)
                | EApply(fn, argument, _) -> ResolvedApply(resolveExpression fn, resolveExpression argument)
                | EIf(condition, whenTrue, whenFalse, _) -> ResolvedIf(resolveExpression condition, resolveExpression whenTrue, resolveExpression whenFalse)
                | EBinOp("@", left, right, _) | EAppend(left, right, _) -> ResolvedAppend(resolveExpression left, resolveExpression right)
                | EBinOp(_, left, right, _) -> ResolvedPrimitive(Option.get primitive, [ resolveExpression left; resolveExpression right ])
                | ECons(head, tail, _) -> ResolvedCons(resolveExpression head, resolveExpression tail)
                | ETuple(items, _) -> ResolvedTuple(List.map resolveExpression items)
                | EList(items, _) -> ResolvedList(List.map resolveExpression items)
                | ERecord(fields, _) | EStructuralRecord(fields, _) -> ResolvedRecord(fields |> List.map (fun (name, value) -> name, resolveExpression value))
                | EFieldGet(_, _, _) when reference.IsSome -> ResolvedReference(Option.get reference)
                | EFieldGet(target, name, _) -> ResolvedFieldGet(resolveExpression target, name)
                | ESome(item, _) -> ResolvedSome(resolveExpression item)
                | ENone _ -> ResolvedNone
                | ELet(name, value, body, recursive, _, span) ->
                    let valueType = finalType value
                    ResolvedLet(bindingAt LocalSymbol name span valueType, recursive, resolveExpression value, resolveExpression body)
                | ELetPattern(pattern, value, body, _) ->
                    let resolvedValue = resolveExpression value
                    ResolvedMatch(resolvedValue, [ { Pattern = resolvePattern resolvedValue.Type pattern; Guard = None; Body = resolveExpression body; Span = Ast.spanOfPattern pattern } ])
                | ELetRecGroup(bindings, body, _) ->
                    let values =
                        bindings |> List.map (fun (name, parameters, _, value, span) ->
                            let folded = Seq.foldBack (fun parameter inner -> ELambda(parameter, inner, span)) parameters value
                            let resolvedValue = resolveExpression folded
                            { Binding = bindingAt LocalSymbol name span resolvedValue.Type; Value = resolvedValue })
                    ResolvedLetRecursiveGroup(values, resolveExpression body)
                | ERaise(value, _) -> ResolvedFail(resolveExpression value)
                | EFor(name, source, body, span) ->
                    let resolvedSource = resolveExpression source
                    let itemType = match resolvedSource.Type with TList item -> item | _ -> Types.freshVar()
                    ResolvedForEach(bindingAt LocalSymbol name span itemType, resolvedSource, resolveExpression body)
                | EMatch(target, cases, _) ->
                    let resolvedTarget = resolveExpression target
                    let resolvedCases =
                        cases |> List.map (fun (pattern, guard, body, caseSpan) ->
                            { Pattern = resolvePattern resolvedTarget.Type pattern
                              Guard = Option.map resolveExpression guard
                              Body = resolveExpression body
                              Span = caseSpan })
                    ResolvedMatch(resolvedTarget, resolvedCases)
                | ERange(first, last, _) -> ResolvedRange(resolveExpression first, resolveExpression last)
                | ERecordUpdate(target, fields, _) ->
                    ResolvedRecordUpdate(true, resolveExpression target, fields |> List.map (fun (name, value) -> name, resolveExpression value))
                | EStructuralRecordUpdate(target, fields, _) ->
                    ResolvedRecordUpdate(false, resolveExpression target, fields |> List.map (fun (name, value) -> name, resolveExpression value))
                | EIndexGet(target, index, _) -> ResolvedIndexGet(resolveExpression target, resolveExpression index)
                | ENameOf(name, _) -> ResolvedConstant(ResolvedString name)
                | ETypeOf(name, _) -> ResolvedTypeToken(Map.find name resolvedTypeDefinitions)
                | EInterpolatedString(parts, _) ->
                    parts
                    |> List.map (function IPText text -> ResolvedTextPart text | IPExpr item -> ResolvedValuePart(resolveExpression item))
                    |> ResolvedInterpolate
                | EMap(entries, _) ->
                    entries
                    |> List.map (function MEKeyValue(key, value) -> ResolvedKeyValue(resolveExpression key, resolveExpression value) | MESpread value -> ResolvedSpread(resolveExpression value))
                    |> ResolvedMap
            let children =
                match node with
                | ResolvedLambda(_, body) | ResolvedSome body | ResolvedFail body -> [ body ]
                | ResolvedApply(left, right) | ResolvedCons(left, right) | ResolvedAppend(left, right)
                | ResolvedRange(left, right) | ResolvedIndexGet(left, right) -> [ left; right ]
                | ResolvedIf(condition, whenTrue, whenFalse) -> [ condition; whenTrue; whenFalse ]
                | ResolvedPrimitive(_, values) | ResolvedTuple values | ResolvedList values -> values
                | ResolvedRecord fields -> List.map snd fields
                | ResolvedFieldGet(target, _) -> [ target ]
                | ResolvedLet(_, _, value, body) -> [ value; body ]
                | ResolvedLetRecursiveGroup(bindings, body) -> (bindings |> List.map _.Value) @ [ body ]
                | ResolvedMatch(target, cases) -> target :: (cases |> List.collect (fun case -> Option.toList case.Guard @ [ case.Body ]))
                | ResolvedRecordUpdate(_, target, fields) -> target :: List.map snd fields
                | ResolvedInterpolate parts -> parts |> List.choose (function ResolvedTextPart _ -> None | ResolvedValuePart value -> Some value)
                | ResolvedMap entries -> entries |> List.collect (function ResolvedKeyValue(key, value) -> [ key; value ] | ResolvedSpread value -> [ value ])
                | ResolvedForEach(_, source, body) -> [ source; body ]
                | ResolvedConstant _ | ResolvedReference _ | ResolvedNone | ResolvedTypeToken _ -> []
            let coercion =
                match expression with
                | EApply(fn, argument, _) ->
                    match finalType fn, finalType argument with
                    | TFun((TNamed target as targetType), _), (TRecord _ as sourceType) -> Some { SourceType = sourceType; TargetType = targetType; TargetName = Some target }
                    | TFun((TRecord _ as targetType), _), (TNamed source as sourceType) -> Some { SourceType = sourceType; TargetType = targetType; TargetName = Some source }
                    | _ -> None
                | _ -> None
            { Type = expressionType; Span = Ast.spanOfExpr expression; Reference = reference
              Primitive = primitive; RecordCoercion = coercion; Node = node; Children = children }
        let resolvedStatements =
            (statementAttributes, typed) ||> List.map2 (fun declaredAttributes statement ->
                let roots =
                    match statement with
                    | TSLet(_, expression, _, _, _, _) | TSLetPattern(_, expression, _, _, _) -> [ expression ]
                    | TSLetRecGroup(bindings, _, _) -> bindings |> List.map (fun (_, expression, _, _) -> expression)
                    | TSExpr expression -> [ expression.Expr ]
                    | TSBackendImport _ | TSType _ -> []
                let export =
                    match statement with
                    | TSLet(name, _, _, _, true, span) -> Some { Name = name; Span = span }
                    | TSLetPattern(_, _, _, true, span) -> Some { Name = "<pattern>"; Span = span }
                    | TSLetRecGroup(bindings, true, span) -> Some { Name = bindings |> List.map (fun (name, _, _, _) -> name) |> String.concat ","; Span = span }
                    | _ -> None
                let attributes =
                    match statement with
                    | TSLet(_, _, _, _, true, span) | TSLetPattern(_, _, _, true, span) | TSLetRecGroup(_, true, span) ->
                        [ { Name = "Export"; Arguments = []; Span = span } ]
                    | TSBackendImport(_, _, import, _) ->
                        [ { Name = "Import"; Arguments = [ import.ModulePath; import.MemberName ]; Span = import.Span } ]
                    | TSType definition ->
                        definition.Import
                        |> Option.map (fun import -> [ { Name = "Import"; Arguments = [ import.ModulePath; import.MemberName ]; Span = import.Span } ])
                        |> Option.defaultValue []
                    | _ -> []
                let patterns =
                    match statement with
                    | TSLetPattern(pattern, expression, _, _, _) -> [ resolvePattern (finalType expression) pattern ]
                    | _ -> []
                let resolvedRoots = List.map resolveExpression roots
                let declarationBinding name span bindingType = bindingAt DeclarationSymbol name span bindingType
                let declarationScheme name span fallback =
                    (symbolAt DeclarationSymbol name span).Scheme |> Option.defaultValue fallback
                let importMetadata (import: BackendImport) : ResolvedBackendImport =
                    { ModulePath = import.ModulePath; MemberName = import.MemberName; Span = import.Span }
                let node =
                    match statement with
                    | TSType definition ->
                        ResolvedTypeDeclaration
                            { Binding = bindingAt TypeSymbol definition.Name definition.Span (TNamed definition.Name)
                              IsRecursive = definition.IsRecursive
                              Definition = resolvedTypeDefinitions.[definition.Name]
                              Fields =
                                definition.Fields
                                |> List.map (fun (name, fieldType) -> name, typeFromRef declarations [ definition.Name ] fieldType)
                              Cases =
                                definition.Cases
                                |> List.map (fun (name, payload) -> name, Option.map (typeFromRef declarations [ definition.Name ]) payload)
                              Import = Option.map importMetadata definition.Import }
                    | TSLet(name, _, inferred, recursive, _, span) ->
                        ResolvedValueDeclaration
                            { Binding = declarationBinding name span (normalizeTypeVariables inferred)
                              Scheme = declarationScheme name span (Types.generalize Map.empty inferred)
                              IsRecursive = recursive
                              Value = List.exactlyOne resolvedRoots }
                    | TSLetPattern(_, _, bindings, _, span) ->
                        let resolvedBindings =
                            bindings
                            |> Map.toList
                            |> List.map (fun (name, bindingType) ->
                                let normalized = normalizeTypeVariables bindingType
                                declarationBinding name span normalized,
                                declarationScheme name span (Types.generalize Map.empty normalized))
                        ResolvedPatternDeclaration
                            { Pattern = List.exactlyOne patterns
                              Value = List.exactlyOne resolvedRoots
                              Bindings = resolvedBindings }
                    | TSLetRecGroup(bindings, _, _) ->
                        List.zip bindings resolvedRoots
                        |> List.map (fun ((name, _, inferred, span), value) ->
                            { Binding = declarationBinding name span (normalizeTypeVariables inferred)
                              Scheme = declarationScheme name span (Types.generalize Map.empty inferred)
                              IsRecursive = true
                              Value = value })
                        |> ResolvedRecursiveDeclarations
                    | TSBackendImport(name, importedType, import, span) ->
                        let normalized = normalizeTypeVariables importedType
                        ResolvedImportedDeclaration
                            { Binding = declarationBinding name span normalized
                              Scheme = declarationScheme name span (Forall([], normalized))
                              Import = importMetadata import }
                    | TSExpr _ -> ResolvedExpressionStatement(List.exactlyOne resolvedRoots)
                { Span =
                    match statement with
                    | TSType definition -> definition.Span
                    | TSLet(_, _, _, _, _, span) | TSLetPattern(_, _, _, _, span)
                    | TSLetRecGroup(_, _, span) | TSBackendImport(_, _, _, span) -> span
                    | TSExpr expression -> expression.Span
                  Attributes =
                    declaredAttributes @ attributes
                    |> List.sortBy (fun attribute -> attribute.Span.Start.Line, attribute.Span.Start.Column)
                  Export = export
                  Node = node
                  Patterns = patterns
                  Expressions = resolvedRoots })
        { TypedProgram = typed
          ResolvedStatements = resolvedStatements
          ExpressionTypes = expressions
          Symbols = symbols |> Seq.toList
          SymbolReferences = resolvedReferences }

    let inferResolvedProgram (program: Program) : ResolvedProgram =
        inferResolvedProgramWithExterns [] program

    /// The legacy typed-program API is a projection of the semantic resolved
    /// result, so both compiler consumers and existing evaluators observe the
    /// same inference pass and substitutions.
    let inferProgramWithExterns (externs: ExternalFunction list) (program: Program) : TypedProgram =
        (inferResolvedProgramWithExterns externs program).TypedProgram

    let inferProgramWithLocalVariableTypes (program: Program) : TypedProgram * LocalVariableTypeInfo list =
        inferProgramWithExternsAndLocalVariableTypes [] program

    let inferProgram (program: Program) : TypedProgram =
        inferProgramWithExterns [] program
