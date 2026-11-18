namespace FScript.Runtime

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open FScript.Language

/// Private state follows the execution tree, not the path or a reusable registration.
type internal ScriptExecutionScope = {Depth: int; Cancellation: CancellationToken}

module internal ScriptRunner =
    exception private Failure of string
    let initialScope = {Depth=0; Cancellation=CancellationToken.None}
    let private fail prefix message = raise (Failure(prefix+": "+message))
    let private text = function VString value -> value | _ -> fail "InvalidOptions" "Expected a string."
    let private integer = function VInt value -> value | _ -> fail "InvalidOptions" "Expected an integer."
    let private optional project = function VOption value -> Option.map project value | _ -> fail "InvalidOptions" "Expected an option."
    let private permission name =
        match name with
        | "None" -> HostPermissions.None
        | "Interactive" -> HostPermissions.Interactive
        | "Threading" -> HostPermissions.Threading
        | "IO" -> HostPermissions.IO
        | "Native" -> HostPermissions.Native
        | _ -> fail "InvalidOptions" $"Unknown permission: {name}"
    let private permissionList = function
        | VList values -> values |> List.fold (fun flags value -> flags ||| permission (text value)) HostPermissions.None
        | _ -> fail "InvalidOptions" "Expected a list of permission names."
    let private location (span: Span) message =
        match span.Start.File with
        | Some file -> $"{file}({span.Start.Line},{span.Start.Column}): {message}"
        | None -> message
    let rec private runtimeError (error: exn) =
        match error with
        | EvalException detail -> location detail.Span detail.Message
        | :? AggregateException as aggregate -> aggregate.Flatten().InnerExceptions |> Seq.map runtimeError |> String.concat "\n"
        | _ -> error.Message

    let run (makeRegistration: HostContext -> FileSystemPolicy -> ScriptExecutionScope -> ExtensionRegistration)
        (context: HostContext) (policy: FileSystemPolicy) (scope: ScriptExecutionScope)
        permissions (path: string) (arguments: string list) (options: Value) : Task<Value> =
        Task.Factory.StartNew<Value>(Func<Value>(fun () ->
            scope.Cancellation.ThrowIfCancellationRequested()
            try
                if scope.Depth >= 32 then fail "DepthLimitExceeded" "At most 32 child script executions may be nested."
                let fields = match options with VRecord fields -> fields | _ -> fail "InvalidOptions" "Expected FScriptOptions."
                let directory = optional text fields.["rootDirectory"]
                let requested = optional permissionList fields.["permissions"] |> Option.defaultValue permissions
                let timeout = optional integer fields.["timeoutMilliseconds"]
                if not (HostInterop.allows permissions requested) then
                    fail "PermissionDenied" "Child permissions must be a subset of the active parent's permissions."
                if timeout |> Option.exists (fun value -> value <= 0L || value > int64 Int32.MaxValue) then
                    fail "InvalidOptions" "Timeout must be between 1 and 2147483647 milliseconds."
                if String.IsNullOrWhiteSpace path || path.IndexOf('\000') >= 0 then fail "InvalidOptions" "Expected a script path."
                if not (path.EndsWith(".fss", StringComparison.OrdinalIgnoreCase)) then fail "InvalidOptions" "Script paths must end in .fss."
                use timer = new CancellationTokenSource()
                use linked = CancellationTokenSource.CreateLinkedTokenSource(scope.Cancellation, timer.Token)
                timeout |> Option.iter (int >> timer.CancelAfter)
                let cancellation = linked.Token
                let check () = cancellation.ThrowIfCancellationRequested()
                try
                    check ()
                    let root =
                        FileSystemPolicy.tryResolvePath policy (directory |> Option.defaultValue ".")
                        |> Option.defaultWith (fun () -> fail "PathDenied" "Child root must be within the parent root without symlinks.")
                    if FileSystemPolicy.isDeniedPath policy root then fail "PathDenied" $"Child root is denied: {root}"
                    if not (Directory.Exists root) then fail "LoadFailed" $"Child root does not exist: {root}"
                    let childContext = {Permissions=requested; RootDirectory=root; DeniedPathGlobs=[]}
                    let childPolicy = FileSystemPolicy.narrow childContext policy
                    let entry =
                        FileSystemPolicy.tryResolvePath policy path
                        |> Option.defaultWith (fun () -> fail "PathDenied" $"Script is outside the parent root or uses symlinks: {path}")
                    let read sourcePath =
                        check ()
                        if not (FileSystemPolicy.isSafePath childPolicy sourcePath) || FileSystemPolicy.isDeniedPath childPolicy sourcePath then
                            fail "PathDenied" $"Script source is outside the child sandbox or denied: {sourcePath}"
                        try
                            let source = File.ReadAllText sourcePath
                            check ()
                            source
                        with
                        | :? OperationCanceledException -> reraise ()
                        | error -> fail "LoadFailed" $"{sourcePath}: {error.Message}"
                    let source = read entry
                    let program = FScript.parseSourceWithIncludesResolver root entry source (read >> Some)
                    check ()
                    let childScope = {Depth=scope.Depth+1; Cancellation=cancellation}
                    let registration = makeRegistration childContext childPolicy childScope
                    let environment : ExternalFunction =
                        {Name="Env"; Scheme=Forall([],TNamed "Environment"); Arity=0; RequiredPermissions=HostPermissions.None
                         Impl=fun _ _ -> VRecord(Map.ofList ["ScriptName",VOption(Some(VString entry)); "Arguments",VList(List.map VString arguments)])}
                    let registration = {registration with Functions=environment::registration.Functions}
                    let typed = TypeInfer.inferProgramWithRegistration registration program
                    check ()
                    let state = Eval.evalProgramWithCancellation cancellation registration typed
                    check ()
                    match state.LastValue with
                    | VString value -> FScriptSignatures.completed value state.ExitCode
                    | _ -> FScriptSignatures.failed "ResultTypeMismatch: The child's final value must be a string."
                with
                | _ when scope.Cancellation.IsCancellationRequested -> raise (OperationCanceledException(scope.Cancellation))
                | _ when cancellation.IsCancellationRequested -> FScriptSignatures.failed "TimedOut: Child execution exceeded its cooperative timeout."
                | Failure message -> FScriptSignatures.failed message
                | ParseException detail when detail.Message.StartsWith("Imported file '", StringComparison.Ordinal) && (detail.Message.Contains("outside of sandbox root") || detail.Message.Contains("symlink or reparse point")) ->
                    FScriptSignatures.failed ("PathDenied: "+location detail.Span detail.Message)
                | ParseException detail -> FScriptSignatures.failed ("ParseFailed: "+location detail.Span detail.Message)
                | TypeException detail -> FScriptSignatures.failed ("TypeFailed: "+location detail.Span detail.Message)
                | error -> FScriptSignatures.failed ("ExecutionFailed: "+runtimeError error)
            with
            | Failure message -> FScriptSignatures.failed message
            | :? OperationCanceledException -> reraise ()
            | error -> FScriptSignatures.failed ("InvalidOptions: "+error.Message)), CancellationToken.None, TaskCreationOptions.LongRunning, TaskScheduler.Default)
