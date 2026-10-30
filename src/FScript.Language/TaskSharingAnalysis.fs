namespace FScript.Language

open TypeInfer

/// Optional, warning-only analysis. No evaluator or task scheduler instrumentation.
module TaskSharingAnalysis =
    type Options =
        { ThreadSafeTypes: Set<string>
          ThreadSafeFunctions: Set<string> }

    let defaults = { ThreadSafeTypes = Set.empty; ThreadSafeFunctions = Set.empty }

    type Diagnostic =
        { Code: string
          Message: string
          Span: Span
          RelatedSpan: Span }

    type private Resource = { Key: string; Name: string; IsFunction: bool }
    type private Clock = Map<int, int>
    type private AbstractValue =
        { Resources: Set<Resource>
          Fields: Map<string, AbstractValue>
          Callables: Callable list
          Tasks: Set<int>
          Count: int option }
    and private Callable =
        | Closure of int * ResolvedBinding * ResolvedExpression * Map<int, AbstractValue> ref
        | Function of string * int * bool * Type * AbstractValue list

    let private empty = { Resources = Set.empty; Fields = Map.empty; Callables = []; Tasks = Set.empty; Count = None }
    let private combine values =
        match values with
        | [value] -> value
        | _ ->
            { empty with
                Resources = values |> List.map _.Resources |> List.fold Set.union Set.empty
                Callables = values |> List.collect _.Callables
                Tasks = values |> List.map _.Tasks |> List.fold Set.union Set.empty }

    let private container key count value =
        { empty with Resources = value.Resources; Fields = Map.ofList [key, value]; Count = count }
    let private contents value = value.Fields |> Map.tryFind "$items" |> Option.orElseWith (fun () -> value.Fields |> Map.tryFind "$value") |> Option.defaultValue value

    /// Analyze a previously inferred tree. Provider implementations are never invoked.
    /// Unknown FFI results may alias arguments or other results from the same binding.
    let analyze (registration: ExtensionRegistration) (options: Options) (program: ResolvedProgram) : Diagnostic list =
        let hostTypes = registration.Types |> List.map (fun item -> item.Name, item) |> Map.ofList
        let declaredTypes =
            program.ResolvedStatements
            |> List.choose (fun statement -> match statement.Node with ResolvedTypeDeclaration declaration -> Some(declaration.Binding.Name, declaration) | _ -> None)
            |> Map.ofList
        let hostUnions = registration.Unions |> List.map (fun item -> item.Name, item.Cases) |> Map.ofList
        let functions = registration.Functions |> List.map (fun item -> item.Name, item.Arity) |> Map.ofList
        let knownTypes = Set.union (hostTypes |> Map.toSeq |> Seq.map fst |> Set.ofSeq) (registration.Unions |> List.map _.Name |> Set.ofList)
        let knownFunctions = Set.union (functions |> Map.toSeq |> Seq.map fst |> Set.ofSeq) (program.Symbols |> List.filter (fun symbol -> symbol.Kind = ExternSymbol || symbol.Kind = BuiltinSymbol) |> List.map _.Name |> Set.ofList)
        let invalid = Set.union (Set.difference options.ThreadSafeTypes knownTypes) (Set.difference options.ThreadSafeFunctions knownFunctions)
        if not invalid.IsEmpty then invalidArg "options" ("Unknown thread-safety contract names: " + String.concat ", " (Set.toList invalid))
        let safeTypes = Set.union options.ThreadSafeTypes program.Purity.PureTypes
        let safeFunctions = Set.union options.ThreadSafeFunctions program.Purity.PureFunctions
        let diagnostics = ResizeArray<Diagnostic>()
        let reported = System.Collections.Generic.HashSet<string * Span * Span>()
        let histories = System.Collections.Generic.Dictionary<Resource, ResizeArray<int * Clock * Span>>()
        let tasks = System.Collections.Generic.Dictionary<int, Clock * AbstractValue>()
        let activeCalls = System.Collections.Generic.HashSet<int>()
        let mutable callSites: Span list = []
        let mutable nextTask = 0
        let mutable task = 0
        let mutable clock: Clock = Map.ofList [0, 0]
        let tick () = clock <- Map.add task (1 + (Map.tryFind task clock |> Option.defaultValue 0)) clock
        let mergeClock left right = right |> Map.fold (fun state key value -> Map.add key (max value (Map.tryFind key state |> Option.defaultValue 0)) state) left
        let before left right = left |> Map.forall (fun key value -> value <= (Map.tryFind key right |> Option.defaultValue 0))
        // Only ordering facts established in every branch survive a join.
        let commonClock clocks =
            match clocks with
            | [] -> clock
            | first :: rest -> rest |> List.fold (fun state other -> state |> Map.map (fun key value -> min value (Map.tryFind key other |> Option.defaultValue 0))) first
        let access span resources =
            tick ()
            for resource in resources do
                let history =
                    match histories.TryGetValue resource with
                    | true, values -> values
                    | _ -> let values = ResizeArray() in histories.Add(resource, values); values
                for owner, previous, related in history do
                    if owner <> task && not (before previous clock) && not (before clock previous) then
                        if reported.Add(resource.Key, related, span) then
                            diagnostics.Add
                                { Code = if resource.IsFunction then "FST1002" else "FST1001"
                                  Message =
                                    if resource.IsFunction then $"Host function '{resource.Name}' may be called by concurrent tasks; its thread safety is unknown."
                                    else $"Host value '{resource.Name}' may be shared by concurrent tasks; its thread safety is unknown."
                                  Span = span; RelatedSpan = related }
                history.Add(task, clock, span)
        let rec fromType source visited typ =
            let nested typ = fromType source visited typ
            match typ with
            | THostManaged(name, _) when not (Set.contains name safeTypes) ->
                { empty with Resources = Set.singleton { Key = source + ":" + name; Name = name; IsFunction = false } }
            | TNamed name when not (Set.contains name visited) ->
                match Map.tryFind name hostTypes with
                | Some descriptor when not (Set.contains name safeTypes) ->
                    let value = { empty with Resources = Set.singleton { Key = source + ":" + name; Name = name; IsFunction = false } }
                    if descriptor.HostManaged then value
                    else combine (value :: (descriptor.Fields |> Map.toList |> List.map (fun (_, typ) -> fromType source (Set.add name visited) typ)))
                | _ when Set.contains name safeTypes -> empty
                | _ ->
                    let visited = Set.add name visited
                    match Map.tryFind name hostUnions, Map.tryFind name declaredTypes with
                    | Some cases, _ -> cases |> Map.toList |> List.choose (snd >> Option.map (fromType source visited)) |> combine
                    | _, Some declaration ->
                        let fields = declaration.Fields |> List.map (snd >> fromType source visited)
                        let cases = declaration.Cases |> List.choose (snd >> Option.map (fromType source visited))
                        combine (fields @ cases)
                    | _ -> empty
            | TList typ -> container "$items" None (nested typ)
            | TOption typ -> container "$value" None (nested typ)
            | TTask typ -> nested typ
            | TTuple types -> types |> List.map nested |> combine
            | TRecord fields | TRecordShape(_, fields, _) ->
                let values = fields |> Map.map (fun _ typ -> nested typ)
                { combine (values |> Map.toList |> List.map snd) with Fields = values }
            | TUnion(_, cases) -> cases |> Map.toList |> List.choose (snd >> Option.map nested) |> combine
            | TMap(key, value) -> combine [nested key; nested value]
            | _ -> empty
        let unknown source typ = fromType source Set.empty typ
        let rec references (expression: ResolvedExpression) =
            seq {
                match expression.Node with ResolvedReference reference -> yield reference.SymbolId | _ -> ()
                for child in expression.Children do yield! references child
            }
        let makeClosure env binding body =
            let capturedIds = references body |> Set.ofSeq
            let captured =
                capturedIds
                |> Set.fold (fun state id ->
                    match Map.tryFind id env with
                    | Some value -> Map.add id value state
                    | None -> state) Map.empty
            let captures = captured |> Map.toList |> List.map snd |> combine
            { captures with Callables = [Closure(binding.SymbolId, binding, body, ref captured)] }
        let rec bindPattern env (pattern: ResolvedPattern) value =
            let bind env pattern = bindPattern env pattern value
            match pattern.Node with
            | ResolvedPatternBinding binding -> Map.add binding.SymbolId value env
            | ResolvedTuplePattern patterns -> patterns |> List.indexed |> List.fold (fun env (index, pattern) -> bindPattern env pattern (value.Fields |> Map.tryFind (string index) |> Option.defaultValue value)) env
            | ResolvedRecordPattern fields -> fields |> List.fold (fun env (name, pattern) -> bindPattern env pattern (value.Fields |> Map.tryFind name |> Option.defaultValue value)) env
            | ResolvedConsPattern(a,b) -> bindPattern (bindPattern env a (contents value)) b value
            | ResolvedSomePattern pattern -> bindPattern env pattern (contents value)
            | ResolvedUnionPattern(_,_,Some pattern) -> bind env pattern
            | ResolvedMapPattern(entries, tail) ->
                let env = entries |> List.fold (fun env (key, item) -> bindPattern (bindPattern env key (contents value)) item (contents value)) env
                tail |> Option.map (bind env) |> Option.defaultValue env
            | _ -> env
        let rec evaluate env (expression: ResolvedExpression) =
            let eval = evaluate env
            match expression.Node with
            | ResolvedConstant _ | ResolvedNone | ResolvedTypeToken _ -> empty
            | ResolvedReference reference ->
                match Map.tryFind reference.SymbolId env with
                | Some value -> value
                | None when reference.Kind = ExternSymbol || reference.Kind = BuiltinSymbol ->
                    let rec arity = function TFun(_, result) -> 1 + arity result | _ -> 0
                    let count = functions |> Map.tryFind reference.Name |> Option.defaultValue (arity reference.Type)
                    let value = { empty with Callables = [Function(reference.Name, count, reference.Kind = ExternSymbol, reference.Type, [])] }
                    if count = 0 then invokeFunction expression.Span expression.Type reference.Name (reference.Kind = ExternSymbol) [] else value
                | None -> unknown ("symbol:" + string reference.SymbolId) expression.Type
            | ResolvedLambda(binding, body) -> makeClosure env binding body
            | ResolvedApply(fn, argument) ->
                let fn = eval fn
                let argument = eval argument
                apply expression.Span expression.Type fn argument
            | ResolvedPipe(argument, fn) ->
                let argument = eval argument
                let fn = eval fn
                apply expression.Span expression.Type fn argument
            | ResolvedLet(binding, recursive, value, body) ->
                let value = eval value
                let next = Map.add binding.SymbolId value env
                if recursive then wireRecursive next [value]
                evaluate next body
            | ResolvedLetRecursiveGroup(bindings, body) ->
                let values = bindings |> List.map (fun binding -> binding.Binding.SymbolId, eval binding.Value)
                let next = values |> List.fold (fun state (id, value) -> Map.add id value state) env
                wireRecursive next (values |> List.map snd)
                evaluate next body
            | ResolvedIf(condition, yes, no) ->
                eval condition |> ignore
                match condition.Node with
                | ResolvedConstant(ResolvedBool true) -> eval yes
                | ResolvedConstant(ResolvedBool false) -> eval no
                | _ -> branches [(fun () -> eval yes); (fun () -> eval no)]
            | ResolvedMatch(target, cases) ->
                let value = eval target
                branches (cases |> List.map (fun case -> fun () ->
                    let next = bindPattern env case.Pattern value
                    case.Guard |> Option.iter (fun guard -> evaluate next guard |> ignore)
                    evaluate next case.Body))
            | ResolvedSome value -> container "$value" None (eval value)
            | ResolvedList expressions -> container "$items" (Some expressions.Length) (expressions |> List.map eval |> combine)
            | ResolvedRange(first, last) ->
                eval first |> ignore
                eval last |> ignore
                let count = match first.Node, last.Node with ResolvedConstant(ResolvedInt a), ResolvedConstant(ResolvedInt b) -> Some(if a = b then 1 else 2) | _ -> None
                container "$items" count empty
            | ResolvedCons(first, rest) ->
                let first, rest = eval first, eval rest
                container "$items" (rest.Count |> Option.map ((+) 1)) (combine [first; contents rest])
            | ResolvedAppend(first, rest) ->
                let first, rest = eval first, eval rest
                container "$items" None (combine [contents first; contents rest])
            | ResolvedIndexGet(target, index) -> eval index |> ignore; contents (eval target)
            | ResolvedMap entries ->
                let values = entries |> List.collect (function ResolvedKeyValue(key, value) -> [eval key; eval value] | ResolvedSpread value -> [contents (eval value)])
                container "$items" None (combine values)
            | ResolvedTuple expressions ->
                let fields = expressions |> List.mapi (fun index value -> string index, eval value) |> Map.ofList
                { combine (fields |> Map.toList |> List.map snd) with Fields = fields; Callables = []; Tasks = Set.empty }
            | ResolvedRecord expressions ->
                let fields = expressions |> List.map (fun (name, value) -> name, eval value) |> Map.ofList
                { combine (fields |> Map.toList |> List.map snd) with Fields = fields; Callables = []; Tasks = Set.empty }
            | ResolvedFieldGet(target, name) ->
                let value = eval target
                access expression.Span value.Resources
                match value.Fields |> Map.tryFind name with
                | Some field -> field
                | None ->
                    match expression.Type with
                    | TUnit | TInt | TFloat | TBool | TString | TTypeToken -> empty
                    | _ -> combine [value; unknown ("field:" + name) expression.Type]
            | ResolvedRecordUpdate(_, target, fields) ->
                let target = eval target
                access expression.Span target.Resources
                let fields = fields |> List.fold (fun state (name, value) -> Map.add name (eval value) state) target.Fields
                { combine (target :: (fields |> Map.toList |> List.map snd)) with Fields = fields; Callables = []; Tasks = Set.empty }
            | ResolvedPrimitive(_, values) ->
                let values = values |> List.map eval |> combine
                access expression.Span values.Resources
                empty
            | ResolvedInterpolate parts ->
                for part in parts do
                    match part with
                    | ResolvedTextPart _ -> ()
                    | ResolvedValuePart value -> access expression.Span (eval value).Resources
                empty
            | ResolvedForEach(binding, source, body) ->
                let items = eval source
                // Two symbolic iterations expose sharing across repeated spawn sites.
                let next = Map.add binding.SymbolId (contents items) env
                for _ in 1 .. (items.Count |> Option.map (min 2) |> Option.defaultValue 2) do
                    evaluate next body |> ignore
                empty
            | ResolvedFail value -> eval value |> ignore; empty
        and branches alternatives =
            let snapshot () = histories |> Seq.map (fun pair -> pair.Key, List.ofSeq pair.Value) |> Seq.toList
            let restore (snapshot: (Resource * (int * Clock * Span) list) list) =
                histories.Clear()
                for key, values in snapshot do histories.[key] <- ResizeArray(values)
            let initial, initialHistory = clock, snapshot ()
            let results = alternatives |> List.map (fun branch ->
                clock <- initial
                restore initialHistory
                let value = branch ()
                value, clock, snapshot ())
            // Mutually exclusive branches must not be compared with each other.
            // Keep all their possible accesses for subsequent task interactions.
            histories.Clear()
            for _, _, history in results do
                for key, values in history do
                    match histories.TryGetValue key with
                    | true, previous -> previous.AddRange values
                    | _ -> histories.[key] <- ResizeArray(values)
            for pair in histories do
                let distinct = pair.Value |> Seq.distinct |> Seq.toArray
                pair.Value.Clear()
                pair.Value.AddRange distinct
            clock <- results |> List.map (fun (_, clock, _) -> clock) |> commonClock
            // A branch join cannot roll back this task's own event sequence.
            let ownTime = results |> List.map (fun (_, branchClock, _) -> Map.tryFind task branchClock |> Option.defaultValue 0) |> List.fold max 0
            clock <- Map.add task ownTime clock
            results |> List.map (fun (value, _, _) -> value) |> combine
        and wireRecursive env values =
            for value in values do
                for callable in value.Callables do
                    match callable with
                    | Closure(_, _, body, captured) ->
                        let ids = references body |> Set.ofSeq
                        captured.Value <- env |> Map.filter (fun id _ -> Set.contains id ids)
                    | _ -> ()
        and apply span resultType fn argument =
            if fn.Callables.IsEmpty then
                access span (Set.union fn.Resources argument.Resources)
                combine [fn; argument; unknown "unknown-call" resultType]
            else
                let invoke = function
                    | Closure(id, binding, body, captured) ->
                        if activeCalls.Add id then
                            let previous = callSites
                            callSites <- span :: previous
                            try evaluate (Map.add binding.SymbolId argument captured.Value) body
                            finally
                                callSites <- previous
                                activeCalls.Remove id |> ignore
                        else combine [fn; argument; unknown ("recursive:" + string id) resultType]
                    | Function(name, arity, external, signature, arguments) ->
                        let arguments = arguments @ [argument]
                        if arguments.Length >= arity then
                            let rec returned count typ =
                                match count, typ with
                                | 0, _ -> typ
                                | _, TFun(_, typ) -> returned (count - 1) typ
                                | _ -> resultType
                            invokeFunction span (returned arity signature) name external arguments
                        else { combine arguments with Callables = [Function(name, arity, external, signature, arguments)] }
                match fn.Callables with
                | [only] -> invoke only
                | alternatives -> branches (alternatives |> List.map (fun callable -> fun () -> invoke callable))
        and invokeFunction span resultType name external arguments =
            let completed value = value.Tasks |> Set.toList |> List.choose (fun id -> match tasks.TryGetValue id with true,result -> Some result | _ -> None)
            let join completions =
                if not (List.isEmpty completions) then clock <- mergeClock clock (completions |> List.map fst |> List.fold mergeClock Map.empty)
                tick ()
            let ready value =
                nextTask <- nextTask + 1
                tasks.[nextTask] <- clock,value
                { empty with Tasks=Set.singleton nextTask }
            let fork callback run =
                let parent,parentClock=task,clock
                tick ()
                let forkClock=clock
                nextTask <- nextTask + 1
                let child=nextTask
                task <- child
                clock <- Map.add child 0 forkClock
                let result=run (fun argument ->
                    access (callSites |> List.tryHead |> Option.defaultValue span) callback.Resources
                    apply span (match resultType with TTask typ -> typ | _ -> TUnit) callback argument)
                tasks.[child] <- clock,result
                task <- parent
                clock <- mergeClock parentClock forkClock
                tick ()
                { empty with Tasks=Set.singleton child }
            match name, arguments with
            | "Task.fromResult",[value] -> ready value
            | "Task.delay",[_] -> ready empty
            | "Task.continueWith",[callback;value] ->
                let completions=completed value
                fork callback (fun invoke -> join completions; invoke (completions |> List.map snd |> combine))
            | "Task.whenAll",[callback;values] ->
                let inputs=contents values
                let completions=completed inputs
                fork callback (fun invoke ->
                    join completions
                    invoke (container "$items" values.Count (completions |> List.map snd |> combine)))
            | "Task.whenAny",[callback;values] ->
                let completions=completed (contents values)
                fork callback (fun invoke ->
                    match completions with
                    | [] -> invoke (contents values)
                    | _ -> branches (completions |> List.map (fun (finished,value) -> fun () -> join [finished,value]; invoke value)))
            | "Task.awaitAll",[values] ->
                let completions=completed (contents values)
                join completions
                container "$items" values.Count (completions |> List.map snd |> combine)
            | "Task.spawn", [thunk] ->
                let parent, parentClock = task, clock
                tick ()
                let forkClock = clock
                nextTask <- nextTask + 1
                let child = nextTask
                task <- child
                clock <- Map.add child 0 forkClock
                access (callSites |> List.tryHead |> Option.defaultValue span) thunk.Resources
                let result = apply span (match resultType with TTask typ -> typ | _ -> TUnit) thunk empty
                tasks.[child] <- clock, result
                task <- parent
                clock <- mergeClock parentClock forkClock
                tick ()
                { empty with Tasks = Set.singleton child }
            | "Task.await", [value] ->
                let completed = value.Tasks |> Set.toList |> List.choose (fun id -> match tasks.TryGetValue id with true, result -> Some result | _ -> None)
                if not completed.IsEmpty then clock <- mergeClock clock (completed |> List.map fst |> commonClock)
                tick ()
                completed |> List.map snd |> combine
            | _ ->
                let inputs = combine arguments
                if external && not (Set.contains name safeFunctions) then
                    access span (Set.add {Key = "function:" + name; Name = name; IsFunction = true} inputs.Resources)
                // Only declared callback parameters of known combinators are invoked.
                // A function stored in a collection or passed to ignore is not a call.
                let rec parameterTypes = function TFun(argument, result) -> argument :: parameterTypes result | _ -> []
                let signature = BuiltinSignatures.builtinSchemes |> Map.tryFind name |> Option.map (fun (Forall(_, typ)) -> parameterTypes typ) |> Option.defaultValue []
                let items = arguments |> List.filter (fun value -> value.Callables.IsEmpty) |> List.map contents |> combine
                let callbackResults =
                    arguments |> List.indexed |> List.collect (fun (index, callback) ->
                        let callbackParameters = signature |> List.tryItem index |> Option.map parameterTypes |> Option.defaultValue []
                        if callback.Callables.IsEmpty || callbackParameters.IsEmpty then []
                        else
                            let iterations =
                                if name.StartsWith("List.") || name.StartsWith("Map.") then
                                    arguments |> List.tryPick _.Count |> Option.map (min 2) |> Option.defaultValue 2
                                else 1
                            [for _ in 1 .. iterations do
                                yield callbackParameters |> List.fold (fun fn typ -> apply span typ fn items) callback])
                let result = unknown ("result:" + name) resultType
                // Scalars cannot retain host objects; aggregates/results may alias inputs.
                match resultType with
                | TUnit | TInt | TFloat | TBool | TString | TTypeToken -> empty
                | TList _ ->
                    let count = arguments |> List.tryPick _.Count
                    let mapped = ["List.map"; "List.choose"; "List.collect"] |> List.contains name
                    let values = if mapped then callbackResults |> List.map contents |> combine else combine [items; contents result]
                    container "$items" count values
                | TOption _ ->
                    let values = if name = "Option.map" then combine callbackResults else combine [items; contents result]
                    container "$value" None values
                | TMap _ -> container "$items" None (combine (contents inputs :: result :: callbackResults))
                | TFun _ when external ->
                    let rec arity = function TFun(_, result) -> 1 + arity result | _ -> 0
                    { combine [result; inputs] with Callables = [Function(name + " (returned function)", arity resultType, true, resultType, [])] }
                | _ -> combine (result :: inputs :: callbackResults)
        let mutable environment = Map.empty
        let entryFunctions = ResizeArray<AbstractValue * Type * Span>()
        let remember binding value =
            environment <- Map.add binding.SymbolId value environment
            if not value.Callables.IsEmpty then entryFunctions.Add(value, binding.Type, binding.Span)
        for statement in program.ResolvedStatements do
            match statement.Node with
            | ResolvedValueDeclaration declaration ->
                let value = evaluate environment declaration.Value
                remember declaration.Binding value
                if declaration.IsRecursive then wireRecursive environment [value]
            | ResolvedRecursiveDeclarations declarations ->
                let values = declarations |> List.map (fun declaration -> declaration, evaluate environment declaration.Value)
                for declaration, value in values do remember declaration.Binding value
                wireRecursive environment (values |> List.map snd)
            | ResolvedPatternDeclaration declaration ->
                let value = evaluate environment declaration.Value
                let bound = bindPattern environment declaration.Pattern value
                let rec bindings (pattern: ResolvedPattern) =
                    match pattern.Node with
                    | ResolvedPatternBinding binding -> [binding]
                    | ResolvedTuplePattern patterns -> List.collect bindings patterns
                    | ResolvedRecordPattern fields -> fields |> List.collect (snd >> bindings)
                    | ResolvedConsPattern(a,b) -> bindings a @ bindings b
                    | ResolvedSomePattern item | ResolvedUnionPattern(_,_,Some item) -> bindings item
                    | ResolvedMapPattern(entries, tail) -> (entries |> List.collect (fun (key, value) -> bindings key @ bindings value)) @ (tail |> Option.map bindings |> Option.defaultValue [])
                    | _ -> []
                let patternIds = bindings declaration.Pattern |> List.map (fun binding -> binding.Name, binding.SymbolId) |> Map.ofList
                environment <- declaration.Bindings |> List.fold (fun env (binding, _) ->
                    let value = patternIds |> Map.tryFind binding.Name |> Option.bind (fun id -> Map.tryFind id bound) |> Option.defaultValue value
                    Map.add binding.SymbolId value env) bound
            | ResolvedExpressionStatement expression -> evaluate environment expression |> ignore
            | _ -> ()
        // Analyze callable entry points without assuming unrelated host invocations overlap.
        for value, typ, span in entryFunctions do
            histories.Clear(); tasks.Clear(); activeCalls.Clear()
            task <- 0; clock <- Map.ofList [0, 0]
            let rec enter value typ index =
                match typ with
                | TFun(argument, result) ->
                    let argument = unknown ("entry:" + string index + ":" + string span.Start.Line) argument
                    enter (apply span result value argument) result (index + 1)
                | _ -> ()
            enter value typ 0
        diagnostics |> Seq.distinct |> Seq.sortBy (fun diagnostic -> diagnostic.Span.Start.File, diagnostic.Span.Start.Line, diagnostic.Span.Start.Column, diagnostic.Code, diagnostic.RelatedSpan.Start.Line) |> Seq.toList
