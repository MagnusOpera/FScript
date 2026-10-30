namespace FScript.Runtime

open System
open System.IO
open System.Collections.Concurrent
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open FScript.Language

/// Safe debugger DTOs deliberately contain no evaluator values or host payloads.
type DebugLocation = { File: string; Line: int; Column: int; EndLine: int; EndColumn: int }
type DebugBreakpoint = { Id: int; Verified: bool; Location: DebugLocation; Message: string }
type DebugThread = { Id: int; Name: string; State: string }
type DebugStackFrame = { Id: int; Name: string; Location: DebugLocation }
type DebugVariable = { Name: string; Value: string; Type: string; VariablesReference: int; NamedVariables: int; IndexedVariables: int }
type DebugScope = { Name: string; VariablesReference: int; Expensive: bool }
type DebugEvent = { Kind: string; TaskId: int; Reason: string; Message: string }

type private Frame =
    { Id: int; Name: string; mutable Span: Span; Parameters: Env; Captures: Env; mutable Environment: Env; mutable LocalNames: Set<string> }
type private LogicalTask =
    { Id: int; Frames: ResizeArray<Frame>; mutable State: string; mutable Gate: TaskCompletionSource<unit> option
      mutable Mode: string; mutable Depth: int; mutable Generation: int; mutable LastException: exn option; mutable FaultFrames: Frame array option }
type private Reference = { TaskId: int; Generation: int; Values: seq<string * Value>; Indexed: bool }

/// One session owns all task, frame and variable identities. Inspection never invokes host code.
type DebugSession(registration: ExtensionRegistration, program: Program, stopOnEntry: bool, ?sourceAliases: Map<string, Map<string,string>>, ?taskScheduler: TaskScheduler) =
    let taskScheduler = defaultArg taskScheduler TaskScheduler.Default
    let sourceAliases = defaultArg sourceAliases Map.empty
    let typed = TypeInfer.inferProgramWithRegistration registration program
    let sync = obj ()
    let cancelled = new CancellationTokenSource()
    let changed = Event<DebugEvent>()
    let tasks = Dictionary<int, LogicalTask>()
    let capturedFaults = System.Runtime.CompilerServices.ConditionalWeakTable<exn,Frame array>()
    let references = Dictionary<int, Reference>()
    let pathComparison = if System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Windows) then StringComparison.OrdinalIgnoreCase else StringComparison.Ordinal
    let samePath left right = String.Equals(left,right,pathComparison)
    let breakpoints = Dictionary<string, Expr list>(if pathComparison = StringComparison.OrdinalIgnoreCase then StringComparer.OrdinalIgnoreCase else StringComparer.Ordinal)
    let nodes = ResizeArray<Expr>()
    let lexicalLocals = Dictionary<Expr,Set<string>>(HashIdentity.Reference)
    let roots = HashSet<Expr>(HashIdentity.Reference)
    let names = Dictionary<string, string>()
    let parametersByLocation = Dictionary<string, string list>()
    let capturesByLocation = Dictionary<string, Set<string>>()
    let hostThreads = Dictionary<int,Thread>()
    let hostQueues = Dictionary<int, BlockingCollection<(unit -> Value) * TaskCompletionSource<Value>>>()
    let mutable nextId = 0
    let mutable mainThread = 0
    let mutable started = false
    let mutable breakOnException = true
    let mutable disposed = false
    let mutable resultSummary = ""
    let id () = nextId <- nextId + 1; nextId
    let publish kind task reason message = changed.Trigger { Kind = kind; TaskId = task; Reason = reason; Message = message }
    let location (span: Span) =
        { File = defaultArg span.Start.File ""; Line = span.Start.Line; Column = span.Start.Column
          EndLine = span.End.Line; EndColumn = span.End.Column }
    let key (span: Span) = sprintf "%s:%d:%d:%d:%d" (defaultArg span.Start.File "") span.Start.Line span.Start.Column span.End.Line span.End.Column
    let displayName (name: string) =
        name.Split('.') |> Array.filter (fun part -> not (part.StartsWith("__imp", StringComparison.Ordinal) && part.Length > 5 && (part.Substring(5) |> String.forall Char.IsDigit))) |> String.concat "."
    let children expression =
        match expression with
        | EParen(x, _) | ELambda(_, x, _) | ERaise(x, _) | ESome(x, _) | EFieldGet(x, _, _) -> [x]
        | EApply(a,b,_) | ERange(a,b,_) | EIndexGet(a,b,_) | ECons(a,b,_) | EAppend(a,b,_) | EBinOp(_,a,b,_) -> [a;b]
        | EIf(a,b,c,_) -> [a;b;c]
        | EFor(_,a,b,_) | ELet(_,a,b,_,_,_) | ELetPattern(_,a,b,_) -> [a;b]
        | ELetRecGroup(bindings, body, _) -> (bindings |> List.map (fun (_,_,_,body,_) -> body)) @ [body]
        | EMatch(value, cases, _) -> value :: (cases |> List.collect (fun (_,guard,body,_) -> Option.toList guard @ [body]))
        | EList(items, _) | ETuple(items, _) -> items
        | ERecord(fields, _) | EStructuralRecord(fields, _) -> List.map snd fields
        | ERecordUpdate(target, fields, _) | EStructuralRecordUpdate(target, fields, _) -> target :: List.map snd fields
        | EMap(entries, _) -> entries |> List.collect (function MEKeyValue(a,b) -> [a;b] | MESpread x -> [x])
        | EInterpolatedString(parts, _) -> parts |> List.choose (function IPExpr x -> Some x | _ -> None)
        | _ -> []
    let rec walk expression = seq {
        yield expression
        for child in children expression do yield! walk child
    }
    let rec functionBody expression = match expression with ELambda(_,body,_) -> functionBody body | _ -> expression
    let addFunction name expression =
        let rec parameters = function ELambda(parameter,body,_) -> parameter.Name :: parameters body | _ -> []
        let body = functionBody expression
        let position = key (Ast.spanOfExpr body)
        names[position] <- displayName name
        parametersByLocation[position] <- parameters expression
        capturesByLocation[position] <- walk body |> Seq.choose (function EVar(name,_) -> Some name | _ -> None) |> Set.ofSeq
    let rec boundNames pattern =
        match pattern with
        | PVar(name,_) -> Set.singleton name
        | PCons(a,b,_) -> Set.union (boundNames a) (boundNames b)
        | PTuple(patterns,_) -> patterns |> List.map boundNames |> Set.unionMany
        | PRecord(fields,_) -> fields |> List.map (snd >> boundNames) |> Set.unionMany
        | PMap(clauses,tail,_) -> ((clauses |> List.collect (fun (key,value) -> [boundNames key;boundNames value])) @ (tail |> Option.toList |> List.map boundNames)) |> Set.unionMany
        | PSome(pattern,_) | PUnionCase(_,_,Some pattern,_) -> boundNames pattern
        | _ -> Set.empty
    let rec annotate expression locals =
        lexicalLocals[expression] <- locals
        match expression with
        | ELambda(_,body,_) -> annotate body Set.empty
        | ELet(name,value,body,_,_,_) -> annotate value locals; annotate body (locals.Add name)
        | ELetPattern(pattern,value,body,_) -> annotate value locals; annotate body (Set.union locals (boundNames pattern))
        | EFor(name,source,body,_) -> annotate source locals; annotate body (locals.Add name)
        | ELetRecGroup(bindings,body,_) ->
            let scope = Set.union locals (bindings |> List.map (fun (name,_,_,_,_) -> name) |> Set.ofList)
            for _,_,_,value,_ in bindings do annotate value scope
            annotate body scope
        | EMatch(value,cases,_) ->
            annotate value locals
            for pattern,guard,body,_ in cases do
                let scope = Set.union locals (boundNames pattern)
                guard |> Option.iter (fun expression -> annotate expression scope)
                annotate body scope
        | _ -> for child in children expression do annotate child locals
    let addRoot name expression =
        annotate expression Set.empty
        roots.Add expression |> ignore
        for node in walk expression do
            nodes.Add node
            match node with
            | ELambda(_, body, _) -> roots.Add body |> ignore
            | EFor(_,_,body,_) -> roots.Add body |> ignore
            | EIf(_,yes,no,_) -> roots.Add yes |> ignore; roots.Add no |> ignore
            | EMatch(_,cases,_) -> for _,_,body,_ in cases do roots.Add body |> ignore
            | ELet(binding,value,_,_,_,_) -> addFunction binding value
            | _ -> ()
        if name <> "" then addFunction name expression
    do
        for statement in typed do
            match statement with
            | TypeInfer.TSLet(name,expression,_,_,_,_) -> addRoot name expression
            | TypeInfer.TSLetPattern(_,expression,_,_,_) -> addRoot "" expression
            | TypeInfer.TSLetRecGroup(bindings,_,_) -> for name,expression,_,_ in bindings do addRoot name expression
            | TypeInfer.TSExpr expression -> addRoot "" expression.Expr
            | _ -> ()
    let pointSpan expression = match expression with EBinOp("|>",_,stage,_) -> Ast.spanOfExpr stage | _ -> Ast.spanOfExpr expression
    let semantic expression =
        match expression with
        | EParen _ -> false
        | _ when roots.Contains expression -> true
        | EUnit _ | ELiteral _ | EVar _ | ELambda _ | EParen _ | EFieldGet _ | ETypeOf _ | ENameOf _ -> false
        | _ -> true
    let getTask taskId =
        match tasks.TryGetValue taskId with
        | true, task when task.State <> "exited" -> task
        | _ -> invalidArg "taskId" "Unknown or exited FScript task."
    let getPaused taskId =
        cancelled.Token.ThrowIfCancellationRequested()
        let task = getTask taskId
        if task.State <> "paused" then invalidOp "The FScript task must be paused for inspection."
        task
    let inspectionFrames task = task.FaultFrames |> Option.map Array.toSeq |> Option.defaultWith (fun () -> task.Frames :> seq<Frame>)
    let getFrame frameId =
        tasks.Values |> Seq.tryPick (fun task ->
            inspectionFrames task |> Seq.tryFind (fun frame -> frame.Id = frameId) |> Option.map (fun frame -> task,frame))
        |> Option.map (fun (task,frame) -> getPaused task.Id |> ignore; task,frame)
        |> Option.defaultWith (fun () -> invalidArg "frameId" "Stale or unknown frame reference.")
    let retain task values indexed =
        let reference = id ()
        references.Add(reference, { TaskId = task.Id; Generation = task.Generation; Values = values; Indexed = indexed })
        reference
    let visibleFields (host: HostManagedValue) =
        let allowed = registration.Types |> List.tryFind (fun descriptor -> descriptor.Name = host.Descriptor.Name) |> Option.map (fun descriptor -> descriptor.Fields) |> Option.defaultValue Map.empty
        host.Fields |> Map.filter (fun name _ -> allowed.ContainsKey name)
    let summary value =
        match value with
        | VList items -> $"[{items.Length} items]"
        | VTuple items -> $"({items.Length} items)"
        | VRecord fields -> $"record ({fields.Count} fields)"
        | VHostManaged host -> $"{host.Descriptor.Name} ({(visibleFields host).Count} visible fields)"
        | VMap fields -> $"map ({fields.Count} items)"
        | VOption(Some _) -> "Some (…)"
        | VUnionCase(_,name,Some _) -> name + " (…)"
        | VTask handle ->
            match handle.DebugTaskId with
            | Some taskId ->
                let state = match tasks.TryGetValue taskId with true,task -> task.State | _ -> if handle.Worker.IsCompleted then "completed" else "running"
                $"Task {taskId} ({state})"
            | None -> "<task>"
        | VClosure _ | VExternal _ -> "<function>"
        | VString text when text.Length > 1024 -> sprintf "%A…" (text.Substring(0,1024))
        | _ -> Pretty.valueToString value
    let expand value =
        match value with
        | VList items | VTuple items -> true, (items |> Seq.mapi (fun i value -> string i,value))
        | VRecord fields -> false, Map.toSeq fields
        | VHostManaged host -> false, Map.toSeq (visibleFields host)
        | VMap fields -> false, (fields |> Map.toSeq |> Seq.map (fun (key,value) -> sprintf "%A" key,value))
        | VTask handle -> false, seq {
            match handle.DebugTaskId with Some taskId -> yield "Id",VInt(int64 taskId) | None -> ()
            yield "Awaited",VBool handle.Awaited
            yield "Completed",VBool handle.Worker.IsCompleted
          }
        | VOption(Some value) -> false, seq { "Value",value }
        | VUnionCase(_,_,Some value) -> false, seq { "Payload",value }
        | _ -> false, Seq.empty
    let variable task name value =
        let indexed,values = expand value
        let count = Seq.length values
        let reference = if count = 0 then 0 else retain task values indexed
        let typeName =
            match value with
            | VHostManaged host -> host.Descriptor.Name
            | VUnionCase(name,_,_) -> displayName name
            | VInt _ -> "int" | VFloat _ -> "float" | VBool _ -> "bool" | VString _ -> "string" | VUnit -> "unit"
            | VList _ -> "list" | VTuple _ -> "tuple" | VMap _ -> "map" | VRecord _ -> "record" | VOption _ -> "option"
            | VClosure _ | VExternal _ | VUnionCtor _ -> "function" | VTask _ -> "task" | VTypeToken _ -> "type"
        { Name = name; Value = summary value; Type = typeName; VariablesReference = reference
          NamedVariables = (if indexed then 0 else count); IndexedVariables = (if indexed then count else 0) }
    let pause task reason =
        let gate = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)
        task.State <- "paused"
        task.Gate <- Some gate
        publish "stopped" task.Id reason ""
        gate.Task :> Task
    let invalidate task =
        let stale = references |> Seq.filter (fun pair -> pair.Value.TaskId = task.Id) |> Seq.map (fun pair -> pair.Key) |> Seq.toArray
        for reference in stale do references.Remove reference |> ignore
        // New frame IDs prevent references from an earlier pause being reused.
        for index in 0 .. task.Frames.Count - 1 do
            let frame = task.Frames[index]
            task.Frames[index] <- { frame with Id = id () }
        task.FaultFrames <- None
        task.Generation <- task.Generation + 1
    let resume task mode =
        task.Mode <- mode
        task.Depth <- task.Frames.Count
        invalidate task
        task.State <- "running"
        let gate = task.Gate
        task.Gate <- None
        publish "continued" task.Id "" ""
        gate |> Option.iter (fun gate -> gate.TrySetResult(()) |> ignore)
    let observer =
        { new IDebugObserver with
            member _.CancellationToken = cancelled.Token
            member _.Scheduler = taskScheduler
            member _.TaskStarted parent = lock sync (fun () ->
                let taskId = id ()
                tasks.Add(taskId, { Id = taskId; Frames = ResizeArray(); State = "running"; Gate = None
                                    Mode = if parent.IsNone && stopOnEntry then "in" else "continue"
                                    Depth = 0; Generation = 0; LastException = None; FaultFrames = None })
                publish "threadStarted" taskId "" ""
                taskId)
            member _.EnterFrame(taskId,name,span,parameters,captures) = lock sync (fun () ->
                let task = getTask taskId
                let position = key span
                let actualName = match names.TryGetValue position with true,name -> name | _ -> displayName name
                let allParameters =
                    match parametersByLocation.TryGetValue position with
                    | true,parameterNames ->
                        let previous = captures |> Map.filter (fun name _ -> List.contains name parameterNames)
                        Map.fold (fun state name value -> Map.add name value state) previous parameters
                    | _ -> parameters
                let visibleCaptures =
                    match capturesByLocation.TryGetValue position with
                    | true,used -> captures |> Map.filter (fun name _ -> used.Contains name && not (allParameters.ContainsKey name))
                    | _ -> captures |> Map.filter (fun name _ -> not (allParameters.ContainsKey name))
                task.Frames.Add { Id = id (); Name = actualName; Span = span; Parameters = allParameters; Captures = visibleCaptures; Environment = captures; LocalNames = Set.empty })
            member _.LeaveFrame taskId = lock sync (fun () ->
                let task = getTask taskId
                if task.Frames.Count > 0 then task.Frames.RemoveAt(task.Frames.Count - 1))
            member _.Expression(taskId,expression,environment,phase) = lock sync (fun () ->
                cancelled.Token.ThrowIfCancellationRequested()
                let task = getTask taskId
                let span = pointSpan expression
                if task.Frames.Count > 0 then
                    let frame = task.Frames[task.Frames.Count - 1]
                    frame.Span <- span
                    frame.Environment <- environment
                    frame.LocalNames <- match lexicalLocals.TryGetValue expression with true,names -> names | _ -> Set.empty
                let breakpoint =
                    match span.Start.File with
                    | Some file -> match breakpoints.TryGetValue file with true,points -> phase = "before" && List.exists (fun node -> obj.ReferenceEquals(node,expression)) points | _ -> false
                    | None -> false
                let stepping = semantic expression && (task.Mode = "in" || task.Mode = "pause" || (task.Mode = "over" && task.Frames.Count <= task.Depth) || (task.Mode = "out" && task.Frames.Count < task.Depth))
                if span.Start.Line > 0 && not ((defaultArg span.Start.File "").StartsWith("<", StringComparison.Ordinal)) && (breakpoint || stepping) then
                    pause task (if breakpoint then "breakpoint" elif task.Mode = "pause" then "pause" elif task.Depth = 0 then "entry" else "step")
                else Task.CompletedTask)
            member _.Awaiting(taskId,_,waiting) = lock sync (fun () ->
                let task = getTask taskId
                task.State <- if waiting then "waiting" else "running")
            member _.RecordFault(taskId,error) = lock sync (fun () ->
                if not cancelled.IsCancellationRequested then
                    match capturedFaults.TryGetValue error with
                    | true,_ -> ()
                    | _ ->
                        let task = getTask taskId
                        capturedFaults.Add(error,task.Frames |> Seq.map (fun frame -> { frame with Environment = frame.Environment }) |> Seq.toArray))
            member _.Fault(taskId,error) = lock sync (fun () ->
                if cancelled.IsCancellationRequested then Task.CompletedTask
                else
                    let task = getTask taskId
                    if task.LastException |> Option.exists (fun previous -> obj.ReferenceEquals(previous,error)) then Task.CompletedTask
                    else
                        task.LastException <- Some error
                        let frames = match capturedFaults.TryGetValue error with true,frames -> frames | _ -> task.Frames.ToArray()
                        task.FaultFrames <- Some frames
                        match error with
                        | EvalException detail when frames.Length > 0 && detail.Span.Start.Line > 0 -> frames[frames.Length - 1].Span <- detail.Span
                        | _ -> ()
                        if breakOnException then pause task "exception" else Task.CompletedTask)
            member _.TaskExited(taskId,error) = lock sync (fun () ->
                let task = getTask taskId
                task.State <- "exited"
                task.LastException <- error
                task.Frames.Clear()
                invalidate task
                publish "threadExited" taskId (if cancelled.IsCancellationRequested then "cancelled" elif error.IsSome then "fault" else "completed") (error |> Option.map (fun e -> e.Message) |> Option.defaultValue "")
                tasks.Remove taskId |> ignore
                hostThreads.Remove taskId |> ignore
                Monitor.PulseAll sync
                match hostQueues.TryGetValue taskId with
                | true,queue -> queue.CompleteAdding(); hostQueues.Remove taskId |> ignore
                | _ -> ())
            member _.CheckCancellation() = cancelled.Token.ThrowIfCancellationRequested()
            member _.InvokeHost(taskId,invocation) =
                if Thread.CurrentThread.ManagedThreadId = mainThread || lock sync (fun () ->
                    match hostThreads.TryGetValue taskId with true,thread -> obj.ReferenceEquals(Thread.CurrentThread,thread) | _ -> false) then
                    try Task.FromResult(invocation ()) with error -> Task.FromException<Value> error
                else
                    let completion = TaskCompletionSource<Value>(TaskCreationOptions.RunContinuationsAsynchronously)
                    let queue = lock sync (fun () ->
                        match hostQueues.TryGetValue taskId with
                        | true,queue -> queue
                        | _ ->
                            let queue = new BlockingCollection<(unit -> Value) * TaskCompletionSource<Value>>()
                            hostQueues.Add(taskId,queue)
                            let thread = Thread(ThreadStart(fun () ->
                                for invoke,result in queue.GetConsumingEnumerable() do
                                    try result.TrySetResult(invoke ()) |> ignore
                                    with error -> result.TrySetException(error) |> ignore))
                            thread.IsBackground <- true
                            thread.Name <- $"FScript host {taskId}"
                            hostThreads[taskId] <- thread
                            thread.Start()
                            queue)
                    queue.Add((invocation,completion),cancelled.Token)
                    completion.Task }

    member _.Events = changed.Publish
    member _.Sources = nodes |> Seq.choose (fun node -> (Ast.spanOfExpr node).Start.File) |> Seq.filter (fun file -> not (file.StartsWith("<",StringComparison.Ordinal))) |> Seq.distinct |> Seq.toArray
    member _.BreakpointLocations(file: string, firstLine: int, lastLine: int) =
        let fullPath = Path.GetFullPath file
        nodes |> Seq.map pointSpan |> Seq.filter (fun span -> (span.Start.File |> Option.exists (fun file -> samePath file fullPath)) && span.Start.Line >= firstLine && span.Start.Line <= lastLine) |> Seq.distinct |> Seq.map location |> Seq.toArray
    member _.SetExceptionBreakpoints(enabled) = lock sync (fun () -> breakOnException <- enabled)
    member _.SetBreakpoints(file: string, positions: (int * int) list) = lock sync (fun () ->
        let path = Path.GetFullPath file
        let available = nodes |> Seq.map pointSpan |> Seq.filter (fun span -> (span.Start.File |> Option.exists (fun file -> samePath file path)) && span.Start.Line > 0) |> Seq.sortBy (fun span -> span.Start.Line,span.Start.Column) |> Seq.toArray
        let results = positions |> List.map (fun (line,column) ->
            let resolved = available |> Array.tryFind (fun span -> span.Start.Line = line && span.Start.Column >= max 1 column)
            match resolved with
            | Some span -> { Id = id (); Verified = true; Location = location span; Message = "" }
            | None -> { Id = id (); Verified = false; Location = { File = path; Line = line; Column = column; EndLine = line; EndColumn = column }; Message = "No executable expression at this location." })
        breakpoints[path] <- results |> List.filter (fun breakpoint -> breakpoint.Verified) |> List.choose (fun breakpoint ->
            nodes |> Seq.tryFind (fun node -> let span = pointSpan node in (span.Start.File |> Option.exists (fun file -> samePath file path)) && span.Start.Line = breakpoint.Location.Line && span.Start.Column = breakpoint.Location.Column))
        results)
    member _.Threads() = lock sync (fun () -> tasks.Values |> Seq.filter (fun task -> task.State <> "exited") |> Seq.map (fun task -> { Id = task.Id; Name = (if task.Id = 1 then "Main" else $"Task {task.Id}"); State = task.State }) |> Seq.toArray)
    member _.StackTrace(taskId) = lock sync (fun () ->
        let task = getPaused taskId
        inspectionFrames task |> Seq.rev |> Seq.map (fun frame -> { Id = frame.Id; Name = frame.Name; Location = location frame.Span }) |> Seq.toArray)
    member _.Scopes(frameId) = lock sync (fun () ->
        let task,frame = getFrame frameId
        let locals = frame.Environment |> Map.filter (fun name value ->
            (name = "$result" || frame.LocalNames.Contains name || (frame.Name = "<main>" && not (BuiltinSignatures.builtinReservedNames.Contains name) && (match value with VExternal _ -> false | _ -> true))) && (not (frame.Parameters.ContainsKey name) || frame.LocalNames.Contains name))
        let captures = frame.Captures |> Map.filter (fun _ value -> match value with VExternal _ -> false | _ -> true)
        [| { Name = "Parameters"; VariablesReference = retain task (Map.toSeq frame.Parameters) false; Expensive = false }
           { Name = "Locals"; VariablesReference = retain task (Map.toSeq locals) false; Expensive = false }
           { Name = "Captured"; VariablesReference = retain task (Map.toSeq captures) false; Expensive = false } |])
    member _.Variables(reference, start, count) = lock sync (fun () ->
        match references.TryGetValue reference with
        | true,reference when (getPaused reference.TaskId).Generation = reference.Generation ->
            let task = getPaused reference.TaskId
            reference.Values |> Seq.skip (min (max 0 start) (Seq.length reference.Values)) |> Seq.truncate (if count <= 0 then 100 else min 1000 count) |> Seq.map (fun (name,value) -> variable task (displayName name) value) |> Seq.toArray
        | _ -> invalidArg "reference" "Stale or unknown variable reference.")
    member _.Evaluate(frameId, text: string) = lock sync (fun () ->
        let task,frame = getFrame frameId
        let parsed = FScript.parse text
        let lookup name =
            let aliases = frame.Span.Start.File |> Option.bind (fun file -> sourceAliases.TryFind file) |> Option.defaultValue Map.empty
            let actual = aliases |> Map.toSeq |> Seq.tryPick (fun (alias,prefix) -> if name = alias || name.StartsWith(alias + ".",StringComparison.Ordinal) then Some(prefix + name.Substring(alias.Length)) else None) |> Option.defaultValue name
            frame.Environment.TryFind actual
        let rec inspect expression =
            let rec qualified = function EVar(name,_) -> Some name | EFieldGet(parent,name,_) -> qualified parent |> Option.map (fun p -> p + "." + name) | _ -> None
            match expression with
            | EParen(inner,_) -> inspect inner
            | EUnit _ -> VUnit
            | ELiteral(LInt value,_) -> VInt value
            | ELiteral(LFloat value,_) -> VFloat value
            | ELiteral(LBool value,_) -> VBool value
            | ELiteral(LString value,_) -> VString value
            | EVar(name,_) -> lookup name |> Option.defaultWith (fun () -> invalidArg "text" $"Unknown variable '{name}'.")
            | EFieldGet(parent,field,_) ->
                match qualified expression |> Option.bind lookup with
                | Some value -> value
                | None ->
                    match inspect parent with
                    | VRecord fields -> fields.TryFind field |> Option.defaultWith (fun () -> invalidArg "text" $"Unknown visible field '{field}'.")
                    | VHostManaged host -> (visibleFields host).TryFind field |> Option.defaultWith (fun () -> invalidArg "text" $"Unknown visible field '{field}'.")
                    | _ -> invalidArg "text" "Field inspection requires a record or host value."
            | _ -> invalidArg "text" "Safe watches support names, visible fields, parentheses, and literals. Function calls and effects are disabled."
        match parsed with
        | [SExpr expression] -> variable task text (inspect expression)
        | _ -> invalidArg "text" "A watch must be a single safe expression.")
    member _.ExceptionInfo(taskId) = lock sync (fun () -> (getPaused taskId).LastException |> Option.map (fun error -> error.Message) |> Option.defaultValue "")
    member _.ValidateResume(taskId) = lock sync (fun () -> getPaused taskId |> ignore)
    member _.Resume(taskId, mode, singleThread) = lock sync (fun () ->
        let selected = getPaused taskId
        if not (["continue";"in";"over";"out"] |> List.contains mode) then invalidArg "mode" "Unknown execution command."
        if not singleThread then
            for task in tasks.Values do if task.Id <> taskId && task.State = "paused" then resume task "continue"
        resume selected mode)
    member _.Pause(taskId) = lock sync (fun () -> (getTask taskId).Mode <- "pause")
    member _.Cancel() = lock sync (fun () ->
        cancelled.Cancel()
        references.Clear()
        for task in tasks.Values do task.Gate |> Option.iter (fun gate -> gate.TrySetResult(()) |> ignore)
        for queue in hostQueues.Values do queue.CompleteAdding())
    member _.ResultSummary = resultSummary
    member this.Run() =
        lock sync (fun () ->
            if started then invalidOp "A debug session can only execute once."
            started <- true)
        mainThread <- Thread.CurrentThread.ManagedThreadId
        try
            try
                cancelled.Token.ThrowIfCancellationRequested()
                let result = Eval.evalProgramWithDebugger observer registration typed
                resultSummary <- Pretty.valueToString result.LastValue
            with error when cancelled.IsCancellationRequested ->
                raise (OperationCanceledException("FScript debugging cancelled.",error,cancelled.Token))
        finally
            this.Cancel()
            lock sync (fun () ->
                let deadline = DateTime.UtcNow.AddSeconds(2.)
                while tasks.Count > 0 && DateTime.UtcNow < deadline do
                    Monitor.Wait(sync,max 1 (int (deadline - DateTime.UtcNow).TotalMilliseconds)) |> ignore
                capturedFaults.Clear())
            publish "terminated" 0 "" ""
    interface IDisposable with
        member this.Dispose() = if not disposed then disposed <- true; this.Cancel()

/// File launches share configuration loading and inference with ordinary CLI execution.
module DebugLaunch =
    let create (programPath: string) (configuration: string option) (noConfiguration: bool) (root: string option) (arguments: string array) stopOnEntry =
        let path = Path.GetFullPath programPath
        let defaultRoot = if configuration.IsSome then defaultArg root (Path.GetDirectoryName path) else Path.GetDirectoryName path
        let settings =
            EnvironmentConfiguration.resolveForCli configuration (noConfiguration && configuration.IsNone) (Directory.GetCurrentDirectory()) defaultRoot None false []
            |> Result.defaultWith (fun error -> invalidOp error)
        let registration = EnvironmentLoader.load settings |> Result.defaultWith (fun error -> invalidOp error)
        let escape (text: string) = text.Replace("\\","\\\\").Replace("\"","\\\"").Replace("\r","\\r").Replace("\n","\\n").Replace("\t","\\t")
        let args = arguments |> Array.map (fun arg -> "\"" + escape arg + "\"") |> String.concat "; "
        let name = escape (Path.GetFileName path)
        let prelude = FScript.parseWithSourceName (Some "<cli-environment>") $"let asEnvironment (value: Environment) = value\nlet Env = asEnvironment {{ ScriptName = Some \"{name}\"; Arguments = [{args}] }}"
        let sources = Dictionary<string,string>()
        let read file =
            let text = File.ReadAllText file
            sources[file] <- text
            Some text
        let entry = read path |> Option.get
        let program,prefixes = IncludeResolver.parseProgramFromSourceWithIncludesResolverAndMetadata settings.RootDirectory path entry read
        let aliases = sources |> Seq.map (fun pair ->
            let mappings = FScript.parseWithSourceName (Some pair.Key) pair.Value |> List.choose (function
                | SImport(alias,importPath,_) ->
                    let resolved = Path.GetFullPath(Path.Combine(Path.GetDirectoryName pair.Key,importPath))
                    prefixes.TryFind resolved |> Option.map (fun prefix -> alias,prefix)
                | _ -> None) |> Map.ofList
            pair.Key,mappings) |> Map.ofSeq
        new DebugSession(registration,prelude @ program,stopOnEntry,sourceAliases = aliases)
