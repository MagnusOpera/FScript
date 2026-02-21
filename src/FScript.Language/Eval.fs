namespace FScript.Language

open System.Globalization

module Eval =
    type ProgramState =
        { TypeDefs: Map<string, Type>
          Env: Env
          LastValue: Value }

    let private builtinIgnore : ExternalFunction =
        { Name = "ignore"
          Scheme = Forall([ 0 ], TFun (TVar 0, TUnit))
          Arity = 1
          Impl = (fun _ _ -> VUnit) }

    let private builtinPrint : ExternalFunction =
        { Name = "print"
          Scheme = Forall([], TFun (TString, TUnit))
          Arity = 1
          Impl =
            (fun _ args ->
                match args with
                | [ VString text ] ->
                    System.Console.WriteLine(text)
                    VUnit
                | _ ->
                    let span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
                    raise (EvalException { Message = "print expects (string)"; Span = span })) }

    let private builtinIntTryParse : ExternalFunction =
        { Name = "Int.tryParse"
          Scheme = Forall([], TFun (TString, TOption TInt))
          Arity = 1
          Impl =
            (fun _ args ->
                match args with
                | [ VString text ] ->
                    match System.Int64.TryParse(text, NumberStyles.Integer, CultureInfo.InvariantCulture) with
                    | true, value -> VOption (Some (VInt value))
                    | _ -> VOption None
                | _ ->
                    let span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
                    raise (EvalException { Message = "Int.tryParse expects (string)"; Span = span })) }

    let private builtinFloatTryParse : ExternalFunction =
        { Name = "Float.tryParse"
          Scheme = Forall([], TFun (TString, TOption TFloat))
          Arity = 1
          Impl =
            (fun _ args ->
                match args with
                | [ VString text ] ->
                    match System.Double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture) with
                    | true, value -> VOption (Some (VFloat value))
                    | _ -> VOption None
                | _ ->
                    let span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
                    raise (EvalException { Message = "Float.tryParse expects (string)"; Span = span })) }

    let private builtinBoolTryParse : ExternalFunction =
        { Name = "Bool.tryParse"
          Scheme = Forall([], TFun (TString, TOption TBool))
          Arity = 1
          Impl =
            (fun _ args ->
                match args with
                | [ VString text ] ->
                    match System.Boolean.TryParse(text) with
                    | true, value -> VOption (Some (VBool value))
                    | _ -> VOption None
                | _ ->
                    let span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
                    raise (EvalException { Message = "Bool.tryParse expects (string)"; Span = span })) }

    let private builtinIntToString : ExternalFunction =
        { Name = "Int.toString"
          Scheme = Forall([], TFun (TInt, TString))
          Arity = 1
          Impl =
            (fun _ args ->
                match args with
                | [ VInt value ] -> VString (value.ToString(CultureInfo.InvariantCulture))
                | _ ->
                    let span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
                    raise (EvalException { Message = "Int.toString expects (int)"; Span = span })) }

    let private builtinFloatToString : ExternalFunction =
        { Name = "Float.toString"
          Scheme = Forall([], TFun (TFloat, TString))
          Arity = 1
          Impl =
            (fun _ args ->
                match args with
                | [ VFloat value ] -> VString (value.ToString("G17", CultureInfo.InvariantCulture))
                | _ ->
                    let span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
                    raise (EvalException { Message = "Float.toString expects (float)"; Span = span })) }

    let private builtinBoolToString : ExternalFunction =
        { Name = "Bool.toString"
          Scheme = Forall([], TFun (TBool, TString))
          Arity = 1
          Impl =
            (fun _ args ->
                match args with
                | [ VBool true ] -> VString "true"
                | [ VBool false ] -> VString "false"
                | _ ->
                    let span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
                    raise (EvalException { Message = "Bool.toString expects (bool)"; Span = span })) }

    let private builtinStringReplace : ExternalFunction =
        { Name = "String.replace"
          Scheme = Forall([], TFun(TString, TFun(TString, TFun(TString, TString))))
          Arity = 3
          Impl =
            (fun _ args ->
                match args with
                | [ VString source; VString oldValue; VString newValue ] ->
                    VString(source.Replace(oldValue, newValue))
                | _ ->
                    let span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
                    raise (EvalException { Message = "String.replace expects (string, string, string)"; Span = span })) }

    let private builtinStringIndexOf : ExternalFunction =
        { Name = "String.indexOf"
          Scheme = Forall([], TFun(TString, TFun(TString, TOption TInt)))
          Arity = 2
          Impl =
            (fun _ args ->
                match args with
                | [ VString source; VString value ] ->
                    let index = source.IndexOf(value, System.StringComparison.Ordinal)
                    if index >= 0 then VOption (Some (VInt (int64 index))) else VOption None
                | _ ->
                    let span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
                    raise (EvalException { Message = "String.indexOf expects (string, string)"; Span = span })) }

    let private builtinStringToLower : ExternalFunction =
        { Name = "String.toLower"
          Scheme = Forall([], TFun(TString, TString))
          Arity = 1
          Impl =
            (fun _ args ->
                match args with
                | [ VString source ] -> VString(source.ToLowerInvariant())
                | _ ->
                    let span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
                    raise (EvalException { Message = "String.toLower expects (string)"; Span = span })) }

    let private builtinStringToUpper : ExternalFunction =
        { Name = "String.toUpper"
          Scheme = Forall([], TFun(TString, TString))
          Arity = 1
          Impl =
            (fun _ args ->
                match args with
                | [ VString source ] -> VString(source.ToUpperInvariant())
                | _ ->
                    let span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
                    raise (EvalException { Message = "String.toUpper expects (string)"; Span = span })) }

    let private builtinStringSubstring : ExternalFunction =
        { Name = "String.substring"
          Scheme = Forall([], TFun(TString, TFun(TInt, TFun(TInt, TOption TString))))
          Arity = 3
          Impl =
            (fun _ args ->
                match args with
                | [ VString source; VInt start; VInt length ] ->
                    let startIndex = int start
                    let sliceLength = int length
                    if startIndex < 0 || sliceLength < 0 || startIndex > source.Length || startIndex + sliceLength > source.Length then
                        VOption None
                    else
                        VOption (Some (VString (source.Substring(startIndex, sliceLength))))
                | _ ->
                    let span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
                    raise (EvalException { Message = "String.substring expects (string, int, int)"; Span = span })) }

    let private builtinStringConcat : ExternalFunction =
        { Name = "String.concat"
          Scheme = Forall([], TFun(TString, TFun(TList TString, TString)))
          Arity = 2
          Impl =
            (fun _ args ->
                match args with
                | [ VString separator; VList values ] ->
                    let rec collect (remaining: Value list) (acc: string list) =
                        match remaining with
                        | [] -> Some(List.rev acc)
                        | VString value :: tail -> collect tail (value :: acc)
                        | _ -> None

                    match collect values [] with
                    | Some strings -> VString(System.String.Join(separator, strings))
                    | None ->
                        let span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
                        raise (EvalException { Message = "String.concat expects (string, string list)"; Span = span })
                | _ ->
                    let span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
                    raise (EvalException { Message = "String.concat expects (string, string list)"; Span = span })) }

    let private builtinStringSplit : ExternalFunction =
        { Name = "String.split"
          Scheme = Forall([], TFun(TString, TFun(TString, TList TString)))
          Arity = 2
          Impl =
            (fun _ args ->
                match args with
                | [ VString source; VString separator ] ->
                    source.Split([| separator |], System.StringSplitOptions.None)
                    |> Array.toList
                    |> List.map VString
                    |> VList
                | _ ->
                    let span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
                    raise (EvalException { Message = "String.split expects (string, string)"; Span = span })) }

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
        | VMap fields ->
            let mapKeyToString key =
                match key with
                | MKString s -> $"\"{s}\""
                | MKInt i -> string i
            fields
            |> Map.toList
            |> List.map (fun (key, value) -> sprintf "%s => %s" (mapKeyToString key) (valueToInterpolationString value))
            |> String.concat "; "
            |> sprintf "map { %s }"
        | VOption None -> "None"
        | VOption (Some value) -> sprintf "Some %s" (valueToInterpolationString value)
        | VUnionCase (_, caseName, None) -> caseName
        | VUnionCase (_, caseName, Some value) -> sprintf "%s %s" caseName (valueToInterpolationString value)
        | VTypeToken t -> sprintf "<type %s>" (Types.typeToString t)
        | VClosure _ -> "<fun>"
        | VUnionCtor (_, caseName) -> sprintf "<ctor %s>" caseName
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
        | VMap xf, VMap yf ->
            xf.Count = yf.Count
            && (Set.ofSeq xf.Keys = Set.ofSeq yf.Keys)
            && (xf |> Map.forall (fun k xv -> valueEquals xv yf.[k]))
        | VOption None, VOption None -> true
        | VOption (Some x), VOption (Some y) -> valueEquals x y
        | VUnionCase (tx, cx, px), VUnionCase (ty, cy, py) ->
            tx = ty && cx = cy &&
            match px, py with
            | None, None -> true
            | Some xv, Some yv -> valueEquals xv yv
            | _ -> false
        | VTypeToken tx, VTypeToken ty -> tx = ty
        | _ -> false

    let private mapKeyToValue (key: MapKey) : Value =
        match key with
        | MKString s -> VString s
        | MKInt i -> VInt i

    let private valueToMapKey (span: Span) (value: Value) : MapKey =
        match value with
        | VString s -> MKString s
        | VInt i -> MKInt i
        | _ -> raise (EvalException { Message = "Map key must be string or int"; Span = span })

    let rec private valueMatchesTypeRef (typeDefs: Map<string, Type>) (tref: TypeRef) (value: Value) : bool =
        let rec resolve (tref: TypeRef) : Type option =
            match tref with
            | TRName "unit" -> Some TUnit
            | TRName "int" -> Some TInt
            | TRName "float" -> Some TFloat
            | TRName "bool" -> Some TBool
            | TRName "string" -> Some TString
            | TRName name ->
                typeDefs |> Map.tryFind name
            | TRTuple ts ->
                resolveTypeList ts |> Option.map TTuple
            | TRFun _ -> None
            | TRPostfix (inner, "list") -> resolve inner |> Option.map TList
            | TRPostfix (inner, "option") -> resolve inner |> Option.map TOption
            | TRPostfix (inner, "map") -> resolve inner |> Option.map (fun t -> TMap (TString, t))
            | TRPostfix _ -> None
            | TRRecord fields ->
                let shape = resolveFieldList fields |> Option.map Map.ofList
                match shape with
                | None -> None
                | Some s ->
                    typeDefs
                    |> Map.toSeq
                    |> Seq.tryPick (fun (name, t) ->
                        match t with
                        | TRecord f when f = s -> Some (TNamed name)
                        | _ -> None)
            | TRStructuralRecord fields ->
                resolveFieldList fields |> Option.map (Map.ofList >> TRecord)
        and resolveTypeList (items: TypeRef list) : Type list option =
            match items with
            | [] -> Some []
            | head :: tail ->
                match resolve head, resolveTypeList tail with
                | Some h, Some t -> Some (h :: t)
                | _ -> None
        and resolveFieldList (fields: (string * TypeRef) list) : (string * Type) list option =
            match fields with
            | [] -> Some []
            | (name, tref) :: tail ->
                match resolve tref, resolveFieldList tail with
                | Some t, Some rest -> Some ((name, t) :: rest)
                | _ -> None

        let rec valueHasType (value: Value) (t: Type) : bool =
            match t, value with
            | TUnit, VUnit -> true
            | TInt, VInt _ -> true
            | TFloat, VFloat _ -> true
            | TBool, VBool _ -> true
            | TString, VString _ -> true
            | TList inner, VList items -> items |> List.forall (fun v -> valueHasType v inner)
            | TTuple inner, VTuple items ->
                inner.Length = items.Length && List.forall2 valueHasType items inner
            | TOption inner, VOption None -> true
            | TOption inner, VOption (Some v) -> valueHasType v inner
            | TMap (_, _), VMap _ -> true
            | TRecord fields, VRecord values ->
                // Structural superset: all required fields must exist and match.
                fields
                |> Map.forall (fun name ft ->
                    match values.TryFind name with
                    | Some fv -> valueHasType fv ft
                    | None -> false)
            | TNamed name, v ->
                match typeDefs.TryFind name with
                | Some expanded -> valueHasType v expanded
                | None -> false
            | _ -> false

        match tref with
        | TRRecord _ ->
            // Declared-type-only check for { ... }.
            match resolve tref with
            | Some t -> valueHasType value t
            | None -> false
        | TRStructuralRecord fields ->
            // Structural-only check for {| ... |}.
            let structuralType = TRStructuralRecord fields
            match resolve structuralType with
            | Some t -> valueHasType value t
            | None -> false
        | _ ->
            match resolve tref with
            | Some t -> valueHasType value t
            | None -> false

    let rec private patternMatch (typeDefs: Map<string, Type>) (pat: Pattern) (value: Value) : Env option =
        let mergeBindings (left: Env) (right: Env) : Env option =
            let mutable ok = true
            let mutable merged = left
            for KeyValue(k, v) in right do
                match merged.TryFind k with
                | Some existing when not (valueEquals existing v) ->
                    ok <- false
                | Some _ -> ()
                | None -> merged <- Map.add k v merged
            if ok then Some merged else None

        let rec matchMapClauses (hasExplicitClauses: bool) (clauses: (Pattern * Pattern) list) (tailPattern: Pattern option) (remaining: Map<MapKey, Value>) (envAcc: Env) : Env option =
            let applyTail () =
                match tailPattern with
                | Some tail ->
                    match patternMatch typeDefs tail (VMap remaining) with
                    | Some tailEnv -> mergeBindings envAcc tailEnv
                    | None -> None
                | None ->
                    if hasExplicitClauses || remaining.IsEmpty then Some envAcc else None

            match clauses with
            | [] -> applyTail ()
            | (keyPattern, valuePattern) :: rest ->
                match keyPattern with
                | PVar (name, _) when not (envAcc.ContainsKey name) ->
                    if remaining.IsEmpty then None
                    else
                        let head = remaining |> Seq.head
                        let key = head.Key
                        let value = head.Value
                        let nextRemaining = remaining.Remove key
                        match patternMatch typeDefs keyPattern (mapKeyToValue key), patternMatch typeDefs valuePattern value with
                        | Some keyEnv, Some valueEnv ->
                            match mergeBindings envAcc keyEnv with
                            | Some merged1 ->
                                match mergeBindings merged1 valueEnv with
                                | Some merged2 -> matchMapClauses hasExplicitClauses rest tailPattern nextRemaining merged2
                                | None -> None
                            | None -> None
                        | _ -> None
                | PVar (name, _) ->
                    match envAcc.TryFind name with
                    | Some targetKeyValue ->
                        let targetKey =
                            match targetKeyValue with
                            | VString s -> MKString s
                            | VInt i -> MKInt i
                            | _ -> raise (EvalException { Message = "Map key must be string or int"; Span = Ast.spanOfPattern keyPattern })
                        if not (remaining.ContainsKey targetKey) then
                            None
                        else
                            let value = remaining.[targetKey]
                            match patternMatch typeDefs valuePattern value with
                            | Some valueEnv ->
                                match mergeBindings envAcc valueEnv with
                                | Some merged -> matchMapClauses hasExplicitClauses rest tailPattern (remaining.Remove targetKey) merged
                                | None -> None
                            | None -> None
                    | _ -> None
                | _ ->
                    let tryKey (kv: System.Collections.Generic.KeyValuePair<MapKey, Value>) : (MapKey * Env) option =
                        match patternMatch typeDefs keyPattern (mapKeyToValue kv.Key) with
                        | Some keyEnv ->
                            match mergeBindings envAcc keyEnv with
                            | Some merged1 ->
                                match patternMatch typeDefs valuePattern kv.Value with
                                | Some valueEnv ->
                                    match mergeBindings merged1 valueEnv with
                                    | Some merged2 -> Some (kv.Key, merged2)
                                    | None -> None
                                | None -> None
                            | None -> None
                        | None -> None

                    remaining
                    |> Seq.tryPick tryKey
                    |> Option.bind (fun (matchedKey, mergedEnv) ->
                        matchMapClauses hasExplicitClauses rest tailPattern (remaining.Remove matchedKey) mergedEnv)

        match pat, value with
        | PWildcard _, _ -> Some Map.empty
        | PVar (name, _), v -> Some (Map.ofList [ name, v ])
        | PLiteral (lit, _), v ->
            if valueEquals (literalToValue lit) v then Some Map.empty else None
        | PNil _, VList [] -> Some Map.empty
        | PCons (p1, p2, _), VList (h :: t) ->
            match patternMatch typeDefs p1 h, patternMatch typeDefs p2 (VList t) with
            | Some env1, Some env2 ->
                Some (Map.fold (fun acc k v -> Map.add k v acc) env1 env2)
            | _ -> None
        | PTuple (patterns, _), VTuple values when patterns.Length = values.Length ->
            (Some Map.empty, List.zip patterns values)
            ||> List.fold (fun accOpt (p, v) ->
                match accOpt, patternMatch typeDefs p v with
                | Some acc, Some next ->
                    Some (Map.fold (fun state k value -> Map.add k value state) acc next)
                | _ -> None)
        | PRecord (fields, _), VRecord values ->
            (Some Map.empty, fields)
            ||> List.fold (fun accOpt (name, p) ->
                match accOpt, values.TryFind name with
                | Some acc, Some value ->
                    match patternMatch typeDefs p value with
                    | Some next -> Some (Map.fold (fun state k v -> Map.add k v state) acc next)
                    | None -> None
                | _ -> None)
        | PMap (clauses, tailPattern, _), VMap values ->
            matchMapClauses (not clauses.IsEmpty) clauses tailPattern values Map.empty
        | PSome (p, _), VOption (Some v) ->
            patternMatch typeDefs p v
        | PNone _, VOption None -> Some Map.empty
        | PUnionCase (qualifier, caseName, payload, _), VUnionCase (valueTypeName, valueCaseName, valuePayload) ->
            let caseMatches = caseName = valueCaseName
            let qualifierMatches =
                match qualifier with
                | Some qualifiedType -> qualifiedType = valueTypeName
                | None -> true
            if caseMatches && qualifierMatches then
                    match payload, valuePayload with
                    | None, None -> Some Map.empty
                    | Some p, Some v -> patternMatch typeDefs p v
                    | _ -> None
            else
                None
        | PTypeRef (tref, _), v ->
            if valueMatchesTypeRef typeDefs tref v then Some Map.empty else None
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
            let env' = closureEnv.Value |> Map.add argName argValue
            eval typeDefs env' body
        | VUnionCtor (typeName, caseName) ->
            VUnionCase(typeName, caseName, Some argValue)
        | VExternal (ext, args) ->
            let args' = args @ [ argValue ]
            if args'.Length = ext.Arity then
                ext.Impl { Apply = applyFunctionValue eval typeDefs span } args'
            elif args'.Length < ext.Arity then
                VExternal (ext, args')
            else
                raise (EvalException { Message = sprintf "External function '%s' received too many arguments" ext.Name; Span = span })
        | _ -> raise (EvalException { Message = "Attempted to apply non-function"; Span = span })

    let rec private evalExpr (typeDefs: Map<string, Type>) (env: Env) (expr: Expr) : Value =
        match expr with
        | EUnit _ -> VUnit
        | ELiteral (lit, _) -> literalToValue lit
        | EVar (name, span) ->
            match env |> Map.tryFind name with
            | Some v -> v
            | None -> raise (EvalException { Message = sprintf "Unbound variable '%s'" name; Span = span })
        | EParen (inner, _) ->
            evalExpr typeDefs env inner
        | ELambda (param, body, _) -> VClosure (param.Name, body, ref env)
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
        | ELet (name, value, body, isRec, _, span) ->
            if isRec then
                match value with
                | ELambda (param, lambdaBody, _) ->
                    let recEnv = ref env
                    let selfValue : Value = VClosure (param.Name, lambdaBody, recEnv)
                    recEnv.Value <- env |> Map.add name selfValue
                    evalExpr typeDefs recEnv.Value body
                | _ ->
                    raise (EvalException { Message = "let rec requires a function binding"; Span = span })
            else
                let v = evalExpr typeDefs env value
                let env' = env |> Map.add name v
                evalExpr typeDefs env' body
        | ELetPattern (pattern, value, body, span) ->
            let v = evalExpr typeDefs env value
            match patternMatch typeDefs pattern v with
            | Some bindings ->
                let env' = Map.fold (fun acc k value -> Map.add k value acc) env bindings
                evalExpr typeDefs env' body
            | None ->
                raise (EvalException { Message = "Let pattern did not match value"; Span = span })
        | ELetRecGroup (bindings, body, span) ->
            if bindings.IsEmpty then
                evalExpr typeDefs env body
            else
                let recEnv = ref env
                let recEntries =
                    bindings
                    |> List.map (fun (name, args, _, valueExpr, bindingSpan) ->
                        if args.IsEmpty then
                            raise (EvalException { Message = "'let rec ... and ...' requires function arguments for each binding"; Span = bindingSpan })
                        let folded = Seq.foldBack (fun arg accExpr -> ELambda(arg, accExpr, bindingSpan)) args valueExpr
                        match folded with
                        | ELambda (param, lambdaBody, _) ->
                            name, VClosure (param.Name, lambdaBody, recEnv)
                        | _ ->
                            raise (EvalException { Message = "let rec requires a function binding"; Span = span }))
                let finalEnv = recEntries |> List.fold (fun acc (name, value) -> Map.add name value acc) env
                recEnv.Value <- finalEnv
                evalExpr typeDefs finalEnv body
        | EMatch (scrutinee, cases, span) ->
            let v = evalExpr typeDefs env scrutinee
            let rec tryCases cs =
                match cs with
                | [] -> raise (EvalException { Message = "No match cases matched"; Span = span })
                | (pat, guard, body, _) :: rest ->
                    match patternMatch typeDefs pat v with
                    | Some bindings ->
                        let env' = Map.fold (fun acc k v -> Map.add k v acc) env bindings
                        match guard with
                        | Some guardExpr ->
                            match evalExpr typeDefs env' guardExpr with
                            | VBool true -> evalExpr typeDefs env' body
                            | VBool false -> tryCases rest
                            | _ -> raise (EvalException { Message = "Match guard must evaluate to bool"; Span = span })
                        | None ->
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
        | EStructuralRecord (fields, _) ->
            fields
            |> List.map (fun (name, valueExpr) -> name, evalExpr typeDefs env valueExpr)
            |> Map.ofList
            |> VRecord
        | EMap (entries, _) ->
            let mergeWithLeftPrecedence (left: Map<MapKey, Value>) (right: Map<MapKey, Value>) =
                right
                |> Map.fold (fun (state: Map<MapKey, Value>) key value ->
                    if state.ContainsKey key then state else Map.add key value state) left

            let evaluated =
                entries
                |> List.fold (fun (acc: Map<MapKey, Value>) entry ->
                    match entry with
                    | MEKeyValue (keyExpr, valueExpr) ->
                        let keyValue = evalExpr typeDefs env keyExpr
                        match keyValue with
                        | VString _
                        | VInt _ ->
                            let key = valueToMapKey (Ast.spanOfExpr keyExpr) keyValue
                            let value = evalExpr typeDefs env valueExpr
                            acc.Add(key, value)
                        | _ ->
                            // Type checker guarantees string/int keys for map literals.
                            raise (EvalException { Message = "Map literal keys must be string or int"; Span = Ast.spanOfExpr keyExpr })
                    | MESpread spreadExpr ->
                        match evalExpr typeDefs env spreadExpr with
                        | VMap spreadMap -> mergeWithLeftPrecedence acc spreadMap
                        | _ ->
                            // Type checker guarantees spread operands are maps.
                            raise (EvalException { Message = "Map spread value must be a map"; Span = Ast.spanOfExpr spreadExpr }))
                    (Map.empty<MapKey, Value>)
            VMap evaluated
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
        | EStructuralRecordUpdate (target, updates, span) ->
            match evalExpr typeDefs env target with
            | VRecord fields ->
                let updated =
                    updates
                    |> List.fold (fun acc (name, valueExpr) ->
                        Map.add name (evalExpr typeDefs env valueExpr) acc) fields
                VRecord updated
            | _ -> raise (EvalException { Message = "Structural record update requires a record value"; Span = span })
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
        | EIndexGet (target, keyExpr, span) ->
            let targetValue = evalExpr typeDefs env target
            let keyValue = evalExpr typeDefs env keyExpr
            match targetValue, keyValue with
            | VMap mapValue, (VString _ | VInt _) ->
                let key = valueToMapKey span keyValue
                VOption (mapValue.TryFind key)
            | VMap _, _ ->
                raise (EvalException { Message = "Map index key must be string or int"; Span = span })
            | _ ->
                raise (EvalException { Message = "Index access requires a map value"; Span = span })
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
        | ENameOf (name, span) ->
            if env.ContainsKey name then
                VString name
            else
                raise (EvalException { Message = $"Unbound variable '{name}'"; Span = span })
        | EInterpolatedString (parts, span) ->
            let sb = System.Text.StringBuilder()
            for part in parts do
                match part with
                | IPText text -> sb.Append(text) |> ignore
                | IPExpr pexpr ->
                    let rendered = evalExpr typeDefs env pexpr |> valueToInterpolationString
                    sb.Append(rendered) |> ignore
            VString (sb.ToString())

    let invokeValue (typeDefs: Map<string, Type>) (fnValue: Value) (args: Value list) : Value =
        let span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
        args |> List.fold (fun state arg -> applyFunctionValue evalExpr typeDefs span state arg) fnValue

    let evalProgramWithExternsState (externs: ExternalFunction list) (program: TypeInfer.TypedProgram) : ProgramState =
        let reserved = Stdlib.reservedNames ()
        let unknownSpan = Span.mk (Span.pos 0 0) (Span.pos 0 0)

        externs
        |> List.tryFind (fun ext -> Set.contains ext.Name reserved)
        |> Option.iter (fun ext ->
            raise (EvalException { Message = $"Host extern '{ext.Name}' collides with reserved stdlib symbol"; Span = unknownSpan }))

        program
        |> List.tryPick (function
            | TypeInfer.TSLet(name, _, _, _, _, _) when Set.contains name reserved -> Some name
            | TypeInfer.TSLetPattern(_, _, bindings, _, _) ->
                bindings
                |> Map.toList
                |> List.tryFind (fun (name, _) -> Set.contains name reserved)
                |> Option.map fst
            | TypeInfer.TSLetRecGroup(bindings, _, _) ->
                bindings
                |> List.tryFind (fun (name, _, _, _) -> Set.contains name reserved)
                |> Option.map (fun (name, _, _, _) -> name)
            | _ -> None)
        |> Option.iter (fun name ->
            raise (EvalException { Message = $"Top-level binding '{name}' collides with reserved stdlib symbol"; Span = unknownSpan }))

        let stdlibTyped = TypeInfer.inferProgramWithExternsRaw externs (Stdlib.loadProgram ())
        let combinedProgram = stdlibTyped @ program

        let decls =
            combinedProgram
            |> List.choose (function | TypeInfer.TSType def -> Some(def.Name, def) | _ -> None)
            |> Map.ofList

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
                        if not d.Cases.IsEmpty then
                            let cases =
                                d.Cases
                                |> List.map (fun (caseName, payload) ->
                                    caseName, payload |> Option.map (fromRef (n :: stack)))
                                |> Map.ofList
                            TUnion(n, cases)
                        else
                            d.Fields
                            |> List.map (fun (fname, ft) -> fname, fromRef (n :: stack) ft)
                            |> Map.ofList
                            |> TRecord
                | None -> TNamed n
            | TRTuple ts -> ts |> List.map (fromRef stack) |> TTuple
            | TRFun (a, b) -> TFun(fromRef stack a, fromRef stack b)
            | TRPostfix (inner, "list") -> TList (fromRef stack inner)
            | TRPostfix (inner, "option") -> TOption (fromRef stack inner)
            | TRPostfix (inner, "map") -> TMap (TString, fromRef stack inner)
            | TRPostfix (_, suffix) ->
                raise (EvalException { Message = $"Unsupported type suffix {suffix}"; Span = unknownSpan })
            | TRRecord fields ->
                fields
                |> List.map (fun (name, t) -> name, fromRef stack t)
                |> Map.ofList
                |> TRecord
            | TRStructuralRecord fields ->
                fields
                |> List.map (fun (name, t) -> name, fromRef stack t)
                |> Map.ofList
                |> TRecord

        let typeDefs =
            decls
            |> Map.map (fun name def ->
                if not def.Cases.IsEmpty then
                    let cases =
                        def.Cases
                        |> List.map (fun (caseName, payload) ->
                            caseName, payload |> Option.map (fromRef [ name ]))
                        |> Map.ofList
                    TUnion(name, cases)
                else
                    def.Fields
                    |> List.map (fun (fname, ft) -> fname, fromRef [ name ] ft)
                    |> Map.ofList
                    |> TRecord)

        let constructorValues =
            decls
            |> Map.toList
            |> List.collect (fun (typeName, def) ->
                def.Cases
                |> List.collect (fun (caseName, payload) ->
                    let value =
                        match payload with
                        | None -> VUnionCase(typeName, caseName, None)
                        | Some _ -> VUnionCtor(typeName, caseName)
                    [ caseName, value
                      $"{typeName}.{caseName}", value ]))

        let externContext =
            { Apply = applyFunctionValue evalExpr typeDefs unknownSpan }

        let mutable env : Env =
            (builtinIgnore
             :: builtinPrint
             :: builtinIntTryParse
             :: builtinFloatTryParse
             :: builtinBoolTryParse
             :: builtinIntToString
             :: builtinFloatToString
             :: builtinBoolToString
             :: builtinStringReplace
             :: builtinStringIndexOf
             :: builtinStringToLower
             :: builtinStringToUpper
             :: builtinStringSubstring
             :: builtinStringConcat
             :: builtinStringSplit
             :: externs)
            |> List.fold (fun acc ext ->
                if ext.Arity = 0 then
                    let value = ext.Impl externContext []
                    acc.Add(ext.Name, value)
                else
                    acc.Add(ext.Name, VExternal (ext, []))) Map.empty
        env <-
            constructorValues
            |> List.fold (fun acc (name, value) -> acc.Add(name, value)) env
        let mutable lastValue = VUnit
        for stmt in combinedProgram do
            match stmt with
            | TypeInfer.TSType _ ->
                ()
            | TypeInfer.TSLet(name, expr, _, isRec, _, span) ->
                if isRec then
                    match expr with
                    | ELambda (param, lambdaBody, _) ->
                        let recEnv = ref env
                        let selfValue : Value = VClosure (param.Name, lambdaBody, recEnv)
                        let finalEnv = env |> Map.add name selfValue
                        recEnv.Value <- finalEnv
                        env <- finalEnv
                    | _ ->
                        raise (EvalException { Message = "let rec requires a function binding"; Span = span })
                else
                    let v = evalExpr typeDefs env expr
                    env <- env |> Map.add name v
            | TypeInfer.TSLetPattern(pattern, expr, _, _, span) ->
                let v = evalExpr typeDefs env expr
                match patternMatch typeDefs pattern v with
                | Some bindings ->
                    env <- Map.fold (fun acc k value -> Map.add k value acc) env bindings
                | None ->
                    raise (EvalException { Message = "Let pattern did not match value"; Span = span })
            | TypeInfer.TSLetRecGroup(bindings, _, span) ->
                if bindings.IsEmpty then
                    ()
                else
                    let recEnv = ref env
                    let recEntries =
                        bindings
                        |> List.map (fun (name, expr, _, bindingSpan) ->
                            match expr with
                            | ELambda (param, lambdaBody, _) ->
                                name, VClosure (param.Name, lambdaBody, recEnv)
                            | _ ->
                                raise (EvalException { Message = "let rec requires a function binding"; Span = bindingSpan }))
                    let finalEnv = recEntries |> List.fold (fun acc (name, value) -> Map.add name value acc) env
                    recEnv.Value <- finalEnv
                    env <- finalEnv
            | TypeInfer.TSExpr texpr ->
                lastValue <- evalExpr typeDefs env texpr.Expr
        { TypeDefs = typeDefs
          Env = env
          LastValue = lastValue }

    let evalProgramWithExterns (externs: ExternalFunction list) (program: TypeInfer.TypedProgram) : Value =
        (evalProgramWithExternsState externs program).LastValue

    let evalProgram (program: TypeInfer.TypedProgram) : Value =
        evalProgramWithExterns [] program
