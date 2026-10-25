namespace FScript.Language

module Eval =
    open HostInterop
    open Execution
#if FABLE_COMPILER
#else
    open System.Threading.Tasks
#endif

    type ProgramState =
        { TypeDefs: Map<string, Type>
          Env: Env
          LastValue: Value
          ExitCode: int }

    type InvocationResult = { Value: Value; ExitCode: int }

    type private RuntimeState =
        { SyncRoot: obj
          ExitCode: int ref
          Active: bool ref
          PendingTasks: ResizeArray<TaskHandle>
          BackgroundFailure: EvalError option ref
#if !FABLE_COMPILER
          Debug: IDebugObserver option
          TaskId: int
#endif
        }

    let private unknownSpan = Span.mk (Span.pos 0 0) (Span.pos 0 0)

    let private wrapTaskFailure (error: EvalError) =
        { Message = $"Task failed: {error.Message}"
          Span = error.Span }

    let private wrapUnexpectedTaskFailure (ex: exn) =
        { Message = $"Task failed: {ex.Message}"
          Span = unknownSpan }

    let inline private withRuntimeLock (runtime: RuntimeState) ([<InlineIfLambda>] f) =
#if FABLE_COMPILER
        f ()
#else
        lock runtime.SyncRoot f
#endif

    let private raiseIfBackgroundFailed (runtime: RuntimeState) =
#if !FABLE_COMPILER
        match runtime.Debug with Some observer -> observer.CheckCancellation() | None -> ()
#endif
        withRuntimeLock runtime (fun () ->
            match runtime.BackgroundFailure.Value with
            | Some error -> raise (EvalException error)
            | None -> ())

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
        | VRecord fields
        | VHostManaged { Fields = fields } ->
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
                | MKBool b -> if b then "true" else "false"
                | MKUnionCase (typeName, caseName) -> $"{typeName}.{caseName}"
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
        | VTask _ -> "<task>"
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
        | RecordFields xf, RecordFields yf ->
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
        | VTask tx, VTask ty -> obj.ReferenceEquals(tx, ty)
        | _ -> false

    let private mapKeyToValue (key: MapKey) : Value =
        match key with
        | MKString s -> VString s
        | MKInt i -> VInt i
        | MKBool b -> VBool b
        | MKUnionCase (typeName, caseName) -> VUnionCase(typeName, caseName, None)

    let private valueToMapKey (span: Span) (value: Value) : MapKey =
        match value with
        | VString s -> MKString s
        | VInt i -> MKInt i
        | VBool b -> MKBool b
        | VUnionCase (typeName, caseName, None) -> MKUnionCase(typeName, caseName)
        | _ -> raise (EvalException { Message = "Map key must be string, int, bool, or a payload-free union case"; Span = span })

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
            | TRPostfix (inner, "task") -> resolve inner |> Option.map TTask
            | TRPostfix (inner, "option") -> resolve inner |> Option.map TOption
            | TRPostfix (inner, "map") -> resolve inner |> Option.map (fun t -> TMap (TString, t))
            | TRMap (keyType, valueType) ->
                match (if keyType = TRName "numeric" then Some TInt else resolve keyType), resolve valueType with
                | Some key, Some value -> Some (TMap(key, value))
                | _ -> None
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
            | TTask _, VTask _ -> true
            | TTuple inner, VTuple items ->
                inner.Length = items.Length && List.forall2 valueHasType items inner
            | TOption inner, VOption None -> true
            | TOption inner, VOption (Some v) -> valueHasType v inner
            | TMap (_, _), VMap _ -> true
            | TRecord fields, RecordFields values ->
                // Structural superset: all required fields must exist and match.
                fields
                |> Map.forall (fun name ft ->
                    match values.TryFind name with
                    | Some fv -> valueHasType fv ft
                    | None -> false)
            | THostManaged _, _ -> HostInterop.matches typeDefs t value
            | TRecordShape (_, fields, _), RecordFields values ->
                fields |> Map.forall (fun name ft -> values.TryFind name |> Option.exists (fun fv -> valueHasType fv ft))
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
                        let targetKey = valueToMapKey (Ast.spanOfPattern keyPattern) targetKeyValue
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
        | PRecord (fields, _), RecordFields values ->
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

    let private binaryOperator span op av bv =
        let arith fInt fFloat =
            match av, bv with
            | VInt x, VInt y -> VInt (fInt x y)
            | VFloat x, VFloat y -> VFloat (fFloat x y)
            | _ -> raise (EvalException { Message = "Numeric operands required"; Span = span })
        match op with
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

#if !FABLE_COMPILER
    let private debugPoint runtime expression environment phase =
        match runtime.Debug with
        | Some observer ->
            observer.CheckCancellation()
            Execution.awaitUnit(observer.Expression(runtime.TaskId,expression,environment,phase))
        | None -> Execution.result ()
#endif

    let rec private createExternContext (runtime: RuntimeState) (typeDefs: Map<string, Type>) (span: Span) : ExternalCallContext =
        { Session = runtime.SyncRoot
          IsActive = fun () -> withRuntimeLock runtime (fun () -> runtime.Active.Value)
          Apply = fun f a -> applyFunctionValue runtime typeDefs span f a |> Execution.run
          SpawnTask = spawnTaskValue runtime typeDefs
          AwaitTask = awaitTaskValue runtime
#if FABLE_COMPILER
          SetExitCode = ignore
#else
          SetExitCode = fun code -> withRuntimeLock runtime (fun () -> runtime.ExitCode.Value <- code)
#endif
          CheckRuntime = fun () ->
              raiseIfBackgroundFailed runtime
#if !FABLE_COMPILER
              match runtime.Debug with Some observer -> observer.CheckCancellation() | None -> ()
#endif
        }

    and private spawnTaskValue (runtime: RuntimeState) (typeDefs: Map<string, Type>) (thunk: Value) : Value =
        raiseIfBackgroundFailed runtime
#if FABLE_COMPILER
        raise (EvalException { Message = "Task.spawn is not supported in Fable builds"; Span = unknownSpan })
#else
        let child =
            match runtime.Debug with
            | Some observer -> { runtime with TaskId = observer.TaskStarted(Some runtime.TaskId) }
            | None -> runtime
        let scheduler = match runtime.Debug with Some observer -> observer.Scheduler | None -> TaskScheduler.Default
        let worker = Execution.runTask scheduler (fun () ->
            execution {
                let mutable failure = None
                try
                    try
                        let! result = applyFunctionValue child typeDefs unknownSpan thunk VUnit
                        return TaskSucceeded result
                    with error ->
                        failure <- Some error
                        match child.Debug with
                        | Some observer -> do! Execution.awaitUnit(observer.Fault(child.TaskId,error))
                        | None -> ()
                        let taskError = match error with EvalException e -> wrapTaskFailure e | _ -> wrapUnexpectedTaskFailure error
                        withRuntimeLock runtime (fun () ->
                            if runtime.BackgroundFailure.Value.IsNone then runtime.BackgroundFailure.Value <- Some taskError)
                        return TaskFailed taskError
                finally
                    match child.Debug with Some observer -> observer.TaskExited(child.TaskId, failure) | None -> ()
            })

        let handle = { Worker = worker; DebugTaskId = (if runtime.Debug.IsSome then Some child.TaskId else None); Awaited = false }
        withRuntimeLock runtime (fun () -> runtime.PendingTasks.Add(handle))
        VTask handle
#endif

    and private awaitTaskValue (runtime: RuntimeState) (taskValue: Value) : Value =
        raiseIfBackgroundFailed runtime
#if FABLE_COMPILER
        raise (EvalException { Message = "Task.await is not supported in Fable builds"; Span = unknownSpan })
#else
        match taskValue with
        | VTask handle ->
            handle.Awaited <- true
            match handle.Worker.GetAwaiter().GetResult() with
            | TaskSucceeded value ->
                raiseIfBackgroundFailed runtime
                value
            | TaskFailed error ->
                withRuntimeLock runtime (fun () ->
                    if runtime.BackgroundFailure.Value.IsNone then
                        runtime.BackgroundFailure.Value <- Some error)
                raise (EvalException error)
        | _ ->
            raise (EvalException { Message = "Task.await expects a task"; Span = unknownSpan })
#endif

    and private applyFunctionValue (runtime: RuntimeState) (typeDefs: Map<string, Type>) (span: Span) (fnValue: Value) (argValue: Value) : Execution<Value> = execution {
        raiseIfBackgroundFailed runtime
        match fnValue with
        | VClosure(argName, ELambda(parameter,body,_), closureEnv) ->
            return VClosure(parameter.Name,body,ref (closureEnv.Value.Add(argName,argValue)))
        | VClosure(argName, body, closureEnv) ->
            let scope = closureEnv.Value.Add(argName, argValue)
#if !FABLE_COMPILER
            match runtime.Debug with
            | None -> return! evalExpr runtime typeDefs scope body
            | Some observer ->
                let name = closureEnv.Value |> Map.toSeq |> Seq.tryPick (fun (name, value) -> if obj.ReferenceEquals(value, fnValue) then Some name else None) |> Option.defaultValue "<lambda>"
                observer.EnterFrame(runtime.TaskId, name, Ast.spanOfExpr body, Map.ofList [argName, argValue], closureEnv.Value)
                try
                    try return! evalExpr runtime typeDefs scope body
                    with error ->
                        match runtime.Debug with
                        | Some observer -> observer.RecordFault(runtime.TaskId,error)
                        | None -> ()
                        return raise error
                finally
                    match runtime.Debug with Some observer -> observer.LeaveFrame(runtime.TaskId) | None -> ()
#else
            return! evalExpr runtime typeDefs scope body
#endif
        | VUnionCtor(typeName, caseName) -> return VUnionCase(typeName, caseName, Some argValue)
        | VExternal(ext, args) ->
            let arguments = argValue :: args
            if arguments.Length = ext.Arity then
                return! invokeExternal runtime typeDefs span ext (List.rev arguments)
            elif arguments.Length < ext.Arity then return VExternal(ext, arguments)
            else return raise (EvalException { Message = $"External function '{ext.Name}' received too many arguments"; Span = span })
        | _ -> return raise (EvalException { Message = "Attempted to apply non-function"; Span = span })
    }

    and private invokeExternal runtime typeDefs span (ext: ExternalFunction) args : Execution<Value> = execution {
        let (Forall(_,signature)) = ext.Scheme
        let validateResult = HostInterop.prepareCall typeDefs signature args
        let! result = invokeExternalUnchecked runtime typeDefs span ext args
        return validateResult result
    }

    and private invokeExternalUnchecked runtime typeDefs span ext args : Execution<Value> = execution {
        if obj.ReferenceEquals(ext, TaskFunctions.spawn) then
            return spawnTaskValue runtime typeDefs args.Head
        elif obj.ReferenceEquals(ext, TaskFunctions.awaitTask) then
#if FABLE_COMPILER
            return awaitTaskValue runtime args.Head
#else
            match args.Head with
            | VTask handle ->
                handle.Awaited <- true
                match runtime.Debug with Some observer -> observer.Awaiting(runtime.TaskId,handle.DebugTaskId,true) | None -> ()
                let! outcome = execution {
                    try return! Execution.awaitTask handle.Worker
                    finally
                        match runtime.Debug with Some observer -> observer.Awaiting(runtime.TaskId,handle.DebugTaskId,false) | None -> ()
                }
                match outcome with
                | TaskSucceeded value ->
                    raiseIfBackgroundFailed runtime
                    return value
                | TaskFailed error -> return raise (EvalException error)
            | _ -> return raise (EvalException { Message = "Task.await expects a task"; Span = span })
#endif
        else
          match BuiltinFunctions.tryExecution ext with
          | Some implementation ->
              return! implementation (applyFunctionValue runtime typeDefs span) args
          | None ->
#if !FABLE_COMPILER
              match runtime.Debug with
              | Some observer ->
                  return! Execution.awaitTask(observer.InvokeHost(runtime.TaskId, fun () -> ext.Impl (createExternContext runtime typeDefs span) args))
              | None ->
#endif
                  return ext.Impl (createExternContext runtime typeDefs span) args
    }

#if !FABLE_COMPILER
    // Isolate debug-only continuations so ordinary expression evaluation does not
    // compile frame/stop orchestration into its hot application path.
    and private debugApplication runtime typeDefs span expression environment fnValue argument : Execution<Value> = execution {
        do! debugPoint runtime expression environment "before"
        let! value = applyFunctionValue runtime typeDefs span fnValue argument
        do! debugPoint runtime expression (environment.Add("$result",value)) "after"
        return value
    }
#endif

    and private updateRecord runtime typeDefs env target updates span structural : Execution<Value> = execution {
        let! targetValue = evalExpr runtime typeDefs env target
        match targetValue with
        | VRecord fields ->
            let mutable result = fields
            for (name, expression) in updates do
                if not structural && not (Map.containsKey name result) then
                    raise (EvalException { Message = sprintf "Record field '%s' not found" name; Span = span })
                let! value = evalExpr runtime typeDefs env expression
                result <- Map.add name value result
            return VRecord result
        | _ ->
            let message = if structural then "Structural record update requires a record value" else "Record update requires a record value"
            return raise (EvalException { Message = message; Span = span })
    }

    and private evalExpr (runtime: RuntimeState) (typeDefs: Map<string,Type>) (env: Env) (expr: Expr) : Execution<Value> =
        raiseIfBackgroundFailed runtime
        let immediate value =
#if !FABLE_COMPILER
            if runtime.Debug.IsSome then execution {
                do! debugPoint runtime expr env "before"
                return value
            }
            else
#endif
                Execution.result value
        match expr with
        | EUnit _ -> immediate VUnit
        | ELiteral(literal,_) -> immediate (literalToValue literal)
        | EVar(name,span) ->
            match env.TryFind name with
            | Some value -> immediate value
            | None -> raise (EvalException { Message = $"Unbound variable '{name}'"; Span = span })
        | ELambda(parameter,body,_) -> immediate (VClosure(parameter.Name,body,ref env))
        | ENone _ -> immediate (VOption None)
        | ETypeOf(name,span) ->
            match typeDefs.TryFind name with Some t -> immediate (VTypeToken t) | None -> raise (EvalException { Message = $"Unknown type '{name}'"; Span = span })
        | ENameOf(name,span) ->
            if env.ContainsKey name then immediate (VString name)
            else raise (EvalException { Message = $"Unbound variable '{name}'"; Span = span })
        | _ -> evalCompound runtime typeDefs env expr

    and private evalCompound (runtime: RuntimeState) (typeDefs: Map<string, Type>) (env: Env) (expr: Expr) : Execution<Value> = execution {
#if !FABLE_COMPILER
        if runtime.Debug.IsSome then
            match expr with
            | EApply _ | EBinOp("|>",_,_,_) -> ()
            | _ -> do! debugPoint runtime expr env "before"
#endif
        match expr with
        | EParen(inner,_) -> return! evalExpr runtime typeDefs env inner
        | EApply(fn, arg, span) ->
            let! f = evalExpr runtime typeDefs env fn
            let! a = evalExpr runtime typeDefs env arg
#if !FABLE_COMPILER
            let partial = runtime.Debug.IsNone || (match f with VExternal(ext,args) -> args.Length + 1 < ext.Arity | VClosure(_,ELambda _,_) -> true | _ -> false)
            if not partial then
                return! debugApplication runtime typeDefs span expr env f a
            else
                return! applyFunctionValue runtime typeDefs span f a
#else
            return! applyFunctionValue runtime typeDefs span f a
#endif
        | EIf(cond, yes, no, span) ->
            let! value = evalExpr runtime typeDefs env cond
            match value with
            | VBool true -> return! evalExpr runtime typeDefs env yes
            | VBool false -> return! evalExpr runtime typeDefs env no
            | _ -> return raise (EvalException { Message = "Condition must be bool"; Span = span })
        | ERaise(valueExpr, span) ->
            let! value = evalExpr runtime typeDefs env valueExpr
            match value with
            | VString message -> return raise (EvalException { Message = message; Span = span })
            | _ -> return raise (EvalException { Message = "raise expects a string"; Span = span })
        | EFor(name, source, body, span) ->
            let! value = evalExpr runtime typeDefs env source
            match value with
            | VList items ->
                for item in items do
                    let! _ = evalExpr runtime typeDefs (env.Add(name, item)) body
                    ()
                return VUnit
            | _ -> return raise (EvalException { Message = "For loop source must be list"; Span = span })
        | ELet(name, value, body, isRec, _, span) ->
            if isRec then
                match value with
                | ELambda(param, lambdaBody, _) ->
                    let recEnv = ref env
                    let self = VClosure(param.Name, lambdaBody, recEnv)
                    recEnv.Value <- env.Add(name, self)
                    return! evalExpr runtime typeDefs recEnv.Value body
                | _ -> return raise (EvalException { Message = "let rec requires a function binding"; Span = span })
            else
                let! result = evalExpr runtime typeDefs env value
                return! evalExpr runtime typeDefs (env.Add(name, result)) body
        | ELetPattern(pattern, value, body, span) ->
            let! result = evalExpr runtime typeDefs env value
            match patternMatch typeDefs pattern result with
            | Some bindings -> return! evalExpr runtime typeDefs (Map.fold (fun state k v -> Map.add k v state) env bindings) body
            | None -> return raise (EvalException { Message = "Let pattern did not match value"; Span = span })
        | ELetRecGroup(bindings, body, span) ->
            let recEnv = ref env
            let entries = bindings |> List.map (fun (name, args, _, value, bindingSpan) ->
                if args.IsEmpty then raise (EvalException { Message = "'let rec ... and ...' requires function arguments for each binding"; Span = bindingSpan })
                match Seq.foldBack (fun arg acc -> ELambda(arg, acc, bindingSpan)) args value with
                | ELambda(param, lambdaBody, _) -> name, VClosure(param.Name, lambdaBody, recEnv)
                | _ -> raise (EvalException { Message = "let rec requires a function binding"; Span = span }))
            recEnv.Value <- List.fold (fun state (name, value) -> Map.add name value state) env entries
            return! evalExpr runtime typeDefs recEnv.Value body
        | EMatch(scrutinee, cases, span) ->
            let! value = evalExpr runtime typeDefs env scrutinee
            let rec tryCases cases = execution {
                match cases with
                | [] -> return raise (EvalException { Message = "No match cases matched"; Span = span })
                | (pattern, guard, body, _) :: rest ->
                    match patternMatch typeDefs pattern value with
                    | None -> return! tryCases rest
                    | Some bindings ->
                        let scope = Map.fold (fun state k v -> Map.add k v state) env bindings
                        let! accepted = match guard with Some g -> evalExpr runtime typeDefs scope g | None -> Execution.result(VBool true)
                        match accepted with
                        | VBool true -> return! evalExpr runtime typeDefs scope body
                        | VBool false -> return! tryCases rest
                        | _ -> return raise (EvalException { Message = "Match guard must evaluate to bool"; Span = span })
            }
            return! tryCases cases
        | EList(items, _) ->
            let! values = Execution.map (evalExpr runtime typeDefs env) items
            return VList values
        | ETuple(items, _) ->
            let! values = Execution.map (evalExpr runtime typeDefs env) items
            return VTuple values
        | ERange(first, last, span) ->
            let! a = evalExpr runtime typeDefs env first
            let! b = evalExpr runtime typeDefs env last
            match a, b with
            | VInt s, VInt e ->
                let step = if s <= e then 1L else -1L
                let rec build acc current =
                    if (step > 0L && current > e) || (step < 0L && current < e) then List.rev acc
                    else build (VInt current :: acc) (current + step)
                return VList(build [] s)
            | _ -> return raise (EvalException { Message = "Range endpoints must be int"; Span = span })
        | ERecord(fields, _) | EStructuralRecord(fields, _) ->
            let! values =
                fields |> Execution.map (fun (name, value) -> execution {
                    let! result = evalExpr runtime typeDefs env value
                    return name, result
                })
            return VRecord(Map.ofList values)
        | EMap(entries, _) ->
            let! result =
                entries |> Execution.fold (fun acc entry -> execution {
                    match entry with
                    | MEKeyValue(keyExpr, valueExpr) ->
                        let! key = evalExpr runtime typeDefs env keyExpr
                        match key with
                        | VString _ | VInt _ | VBool _ | VUnionCase(_, _, None) ->
                            let converted = valueToMapKey (Ast.spanOfExpr keyExpr) key
                            let! value = evalExpr runtime typeDefs env valueExpr
                            return Map.add converted value acc
                        | _ -> return raise (EvalException { Message = "Unsupported map literal key"; Span = Ast.spanOfExpr keyExpr })
                    | MESpread spread ->
                        let! value = evalExpr runtime typeDefs env spread
                        match value with
                        | VMap fields -> return Map.fold (fun state k v -> if Map.containsKey k state then state else Map.add k v state) acc fields
                        | _ -> return raise (EvalException { Message = "Map spread value must be a map"; Span = Ast.spanOfExpr spread })
                }) Map.empty
            return VMap result
        | ERecordUpdate(target, updates, span) -> return! updateRecord runtime typeDefs env target updates span false
        | EStructuralRecordUpdate(target, updates, span) -> return! updateRecord runtime typeDefs env target updates span true
        | EFieldGet(target, field, span) ->
            let rec qualified = function
                | EVar("Env", _) when field="setExitCode" -> Some "Env"
                | EVar(name, _) when not (env.ContainsKey name) -> Some name
                | EFieldGet(parent, name, _) -> qualified parent |> Option.map (fun prefix -> prefix + "." + name)
                | _ -> None
            let direct = qualified target |> Option.bind (fun prefix -> env.TryFind(prefix + "." + field))
            match direct with
            | Some value -> return value
            | None ->
                let! value = evalExpr runtime typeDefs env target
                match value with
                | VRecord fields | VHostManaged { Fields = fields } ->
                    match fields.TryFind field with
                    | Some result -> return result
                    | None -> return raise (EvalException { Message = $"Record field '{field}' not found"; Span = span })
                | _ -> return raise (EvalException { Message = "Field access requires a record value"; Span = span })
        | EIndexGet(target, keyExpr, span) ->
            let! targetValue = evalExpr runtime typeDefs env target
            let! key = evalExpr runtime typeDefs env keyExpr
            match targetValue, key with
            | VList values, VInt index ->
                let rec find items current =
                    match items with [] -> None | head :: tail -> if current = index then Some head else find tail (current + 1L)
                return VOption(if index < 0L then None else find values 0L)
            | VMap values, (VString _ | VInt _ | VBool _ | VUnionCase(_, _, None)) -> return VOption(values.TryFind(valueToMapKey span key))
            | VMap _, _ -> return raise (EvalException { Message = "Map index key must be string, int, bool, or a payload-free union case"; Span = span })
            | VList _, _ -> return raise (EvalException { Message = "List index must be int"; Span = span })
            | _ -> return raise (EvalException { Message = "Index access requires a list or map value"; Span = span })
        | EBinOp(("&&" | "||") as op, a, b, span) ->
            let! first = evalExpr runtime typeDefs env a
            match first with
            | VBool value when (op = "&&" && not value) || (op = "||" && value) -> return VBool value
            | VBool _ ->
                let! second = evalExpr runtime typeDefs env b
                match second with
                | VBool _ -> return second
                | _ -> return raise (EvalException { Message = "Boolean operands required"; Span = span })
            | _ -> return raise (EvalException { Message = "Boolean operands required"; Span = span })
        | ECons(a, b, span) | EAppend(a, b, span) | EBinOp(_, a, b, span) ->
            let! av = evalExpr runtime typeDefs env a
            let! bv = evalExpr runtime typeDefs env b
            let op = match expr with ECons _ -> "::" | EAppend _ -> "@" | EBinOp(op, _, _, _) -> op | _ -> ""
            if op = "|>" then
#if !FABLE_COMPILER
                if runtime.Debug.IsSome then
                    return! debugApplication runtime typeDefs span expr env bv av
                else
                    return! applyFunctionValue runtime typeDefs span bv av
#else
                return! applyFunctionValue runtime typeDefs span bv av
#endif
            else return binaryOperator span op av bv
        | ESome(value, _) ->
            let! result = evalExpr runtime typeDefs env value
            return VOption(Some result)
        | EInterpolatedString(parts, _) ->
            let sb = System.Text.StringBuilder()
            for part in parts do
                match part with
                | IPText text -> sb.Append(text) |> ignore
                | IPExpr expression ->
                    let! value = evalExpr runtime typeDefs env expression
                    sb.Append(valueToInterpolationString value) |> ignore
            return VString(sb.ToString())
        | EUnit _ | ELiteral _ | EVar _ | ELambda _ | ENone _ | ETypeOf _ | ENameOf _ ->
            return invalidOp "Immediate expressions are handled before compound evaluation."
    }

    let invokeValueWithExitCode (typeDefs: Map<string, Type>) (fnValue: Value) (args: Value list) : InvocationResult =
        let runtime =
            { SyncRoot = obj ()
              ExitCode = ref 0
              Active = ref true
              PendingTasks = ResizeArray()
              BackgroundFailure = ref None
#if !FABLE_COMPILER
              Debug = None
              TaskId = 1
#endif
            }

        let applyExternal (ext: ExternalFunction) (existingArgsRev: Value list) (newArgs: Value list) =
            let allArgs = (existingArgsRev |> List.rev) @ newArgs
            if allArgs.Length = ext.Arity then
                HostInterop.invoke typeDefs ext (createExternContext runtime typeDefs unknownSpan) allArgs
            elif allArgs.Length < ext.Arity then
                VExternal (ext, allArgs |> List.rev)
            else
                raise (EvalException { Message = sprintf "External function '%s' received too many arguments" ext.Name; Span = unknownSpan })

        let rec applyMany (currentValue: Value) (remainingArgs: Value list) =
            match currentValue, remainingArgs with
            | value, [] -> value
            | VExternal (ext, existingArgsRev), argsLeft ->
                applyExternal ext existingArgsRev argsLeft
            | VClosure (argName, body, closureEnv), firstArg :: tailArgs ->
                let initialEnv = closureEnv.Value |> Map.add argName firstArg

                let rec bindLambdaChain (envAcc: Env) (exprAcc: Expr) (argsAcc: Value list) =
                    match exprAcc, argsAcc with
                    | ELambda (param, nextBody, _), nextArg :: rest ->
                        bindLambdaChain (envAcc |> Map.add param.Name nextArg) nextBody rest
                    | _ ->
                        envAcc, exprAcc, argsAcc

                let boundEnv, boundBody, argsLeft = bindLambdaChain initialEnv body tailArgs
                let evaluated = evalExpr runtime typeDefs boundEnv boundBody |> Execution.run
                applyMany evaluated argsLeft
            | value, nextArg :: tailArgs ->
                let next = applyFunctionValue runtime typeDefs unknownSpan value nextArg |> Execution.run
                applyMany next tailArgs

        try
            let value=applyMany fnValue args
            {Value=value;ExitCode=withRuntimeLock runtime (fun () -> runtime.ExitCode.Value)}
        finally withRuntimeLock runtime (fun () -> runtime.Active.Value <- false)

    let invokeValue typeDefs fnValue args = (invokeValueWithExitCode typeDefs fnValue args).Value

    let private validateTopLevelBindings (program: TypeInfer.TypedProgram) =
        let reserved = BuiltinSignatures.builtinReservedNames

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

    let private evaluateStatements (runtime: RuntimeState) (typeDefs: Map<string, Type>) (initialEnv: Env) (program: TypeInfer.TypedProgram) =
        validateTopLevelBindings program

        let mutable env = initialEnv
        let mutable lastValue = VUnit
        for stmt in program do
            raiseIfBackgroundFailed runtime
            match stmt with
            | TypeInfer.TSType _ ->
                ()
            | TypeInfer.TSBackendImport(name, importedType, import, span) ->
                let rec arity = function TFun(_, result) -> 1 + arity result | _ -> 0
                let external =
                    { Name = name
                      Scheme = Forall([], importedType)
                      Arity = arity importedType
                      Impl = fun _ _ -> raise (EvalException { Message = $"Backend import '{import.ModulePath}.{import.MemberName}' is not executable in the legacy evaluator"; Span = span }) }
                env <- Map.add name (VExternal(external, [])) env
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
                    let v = evalExpr runtime typeDefs env expr |> Execution.run
                    env <- env |> Map.add name v
            | TypeInfer.TSLetPattern(pattern, expr, _, _, span) ->
                let v = evalExpr runtime typeDefs env expr |> Execution.run
                match patternMatch typeDefs pattern v with
                | Some bindings ->
                    env <- Map.fold (fun acc k value -> Map.add k value acc) env bindings
                | None ->
                    raise (EvalException { Message = "Let pattern did not match value"; Span = span })
            | TypeInfer.TSLetRecGroup(bindings, _, _) ->
                if not bindings.IsEmpty then
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
                lastValue <- evalExpr runtime typeDefs env texpr.Expr |> Execution.run

        raiseIfBackgroundFailed runtime

        let unawaitedCount =
            runtime.PendingTasks
            |> Seq.filter (fun handle -> not handle.Awaited)
            |> Seq.length

        if unawaitedCount > 0 then
            let noun = if unawaitedCount = 1 then "task" else "tasks"
            raise (EvalException { Message = $"Program completed with {unawaitedCount} unawaited {noun}"; Span = unknownSpan })

        { TypeDefs = typeDefs
          Env = env
          LastValue = lastValue
          ExitCode = withRuntimeLock runtime (fun () -> runtime.ExitCode.Value) }

    let evalProgramFromState (state: ProgramState) (program: TypeInfer.TypedProgram) : ProgramState =
        let runtime =
            { SyncRoot = obj ()
              ExitCode = ref state.ExitCode
              Active = ref true
              PendingTasks = ResizeArray()
              BackgroundFailure = ref None
#if !FABLE_COMPILER
              Debug = None
              TaskId = 1
#endif
            }

        try evaluateStatements runtime state.TypeDefs state.Env program
        finally withRuntimeLock runtime (fun () -> runtime.Active.Value <- false)

    let private evaluateRegistered
#if !FABLE_COMPILER
        (debugger: IDebugObserver option)
#endif
        (registration: ExtensionRegistration) (program: TypeInfer.TypedProgram) : ProgramState =
        let externs = registration.Functions
        let reserved = BuiltinSignatures.builtinReservedNames
        let runtime =
            { SyncRoot = obj ()
              ExitCode = ref 0
              Active = ref true
              PendingTasks = ResizeArray()
              BackgroundFailure = ref None
#if !FABLE_COMPILER
              Debug = debugger
              TaskId = (match debugger with Some observer -> observer.TaskStarted None | None -> 1)
#endif
            }

        externs
        |> List.tryFind (fun ext -> Set.contains ext.Name reserved)
        |> Option.iter (fun ext ->
            raise (EvalException { Message = $"Host extern '{ext.Name}' collides with reserved stdlib symbol"; Span = unknownSpan }))

        let decls =
            (BuiltinTypes.typeDefs |> List.map (fun def -> def.Name, def))
            @ (program |> List.choose (function | TypeInfer.TSType def -> Some(def.Name, def) | _ -> None))
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
            | TRPostfix (inner, "task") -> TTask (fromRef stack inner)
            | TRPostfix (inner, "option") -> TOption (fromRef stack inner)
            | TRPostfix (inner, "map") -> TMap (TString, fromRef stack inner)
            | TRMap (keyType, valueType) -> TMap((if keyType = TRName "numeric" then TInt else fromRef stack keyType), fromRef stack valueType)
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

        let hostDefinitions = HostInterop.typeDefinitions registration (decls |> Map.keys |> Seq.toList)
        let typeDefs = Map.fold (fun acc name t -> Map.add name t acc) typeDefs hostDefinitions

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

        let constructorValues =
            (registration.Unions |> List.collect (fun descriptor ->
                descriptor.Cases |> Map.toList |> List.collect (fun (caseName, payload) ->
                    let value = if payload.IsNone then VUnionCase(descriptor.Name, caseName, None) else VUnionCtor(descriptor.Name, caseName)
                    [caseName, value; $"{descriptor.Name}.{caseName}", value]))) @ constructorValues

        let externContext = createExternContext runtime typeDefs unknownSpan

        try
            let mutable env : Env =
                (BuiltinFunctions.builtinExterns @ externs)
                |> List.fold (fun acc ext ->
                    if ext.Arity = 0 then
                        let value = HostInterop.invoke typeDefs ext externContext []
                        acc.Add(ext.Name, value)
                    else
                        acc.Add(ext.Name, VExternal (ext, []))) Map.empty
            env <-
                constructorValues
                |> List.fold (fun acc (name, value) -> acc.Add(name, value)) env
#if !FABLE_COMPILER
            match debugger with Some observer -> observer.EnterFrame(runtime.TaskId, "<main>", unknownSpan, Map.empty, Map.empty) | None -> ()
#endif
            try
                let result = evaluateStatements runtime typeDefs env program
#if !FABLE_COMPILER
                match debugger with Some observer -> observer.TaskExited(runtime.TaskId, None) | None -> ()
#endif
                result
            with error ->
#if !FABLE_COMPILER
                match debugger with
                | Some observer ->
                    observer.Fault(runtime.TaskId, error).GetAwaiter().GetResult()
                    observer.TaskExited(runtime.TaskId, Some error)
                | None -> ()
#endif
                reraise ()
        finally withRuntimeLock runtime (fun () -> runtime.Active.Value <- false)

    let evalProgramWithRegistrationState registration program =
        evaluateRegistered
#if !FABLE_COMPILER
            None
#endif
            registration program

#if !FABLE_COMPILER
    let internal evalProgramWithDebugger debugger registration program = evaluateRegistered (Some debugger) registration program
#endif

    let evalProgramWithExterns (externs: ExternalFunction list) (program: TypeInfer.TypedProgram) : Value =
        (evalProgramWithRegistrationState (HostInterop.ofExterns externs) program).LastValue

    let evalProgram (program: TypeInfer.TypedProgram) : Value =
        evalProgramWithExterns [] program

    let evalProgramWithExternsState externs program =
        evalProgramWithRegistrationState (HostInterop.ofExterns externs) program

    let evalProgramWithRegistration registration program =
        (evalProgramWithRegistrationState registration program).LastValue
