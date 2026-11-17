namespace FScript.Runtime

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text
open System.Threading
open System.Threading.Tasks

type internal ProcessOutcome =
    { ExitCode: int64
      Message: string | null
      Stdout: string
      Stderr: string }

/// Direct child process execution; filesystem sandbox rules do not contain child code.
module internal ProcessRunner =
    type private Capture() =
        let bytes = new MemoryStream()
        let gate = obj()

        member _.Drain(stream: Stream, limit: int64, overflow: unit -> unit, stop: CancellationToken) : Task = task {
            let buffer = Array.zeroCreate<byte> 8192
            let mutable reading = true
            while reading do
                let! count = stream.ReadAsync(buffer, 0, buffer.Length, stop).ConfigureAwait(false)
                if count = 0 then reading <- false
                else
                    let exceeded = lock gate (fun () ->
                        let retained = int (min (int64 count) (limit - bytes.Length))
                        bytes.Write(buffer, 0, retained)
                        retained < count)
                    if exceeded then
                        overflow ()
                        reading <- false
        }

        member _.Text() = lock gate (fun () -> Encoding.UTF8.GetString(bytes.ToArray()))
        interface IDisposable with
            member _.Dispose() = lock gate (fun () -> bytes.Dispose())

    let private hasNul (text: string) = text.IndexOf('\000') >= 0

    let private terminate (child: Process) =
        // Kill(bool) is outside .NET Standard 2.1; prefer it on supporting runtimes.
        try
            match typeof<Process>.GetMethod("Kill", [|typeof<bool>|]) with
            | null -> child.Kill()
            | method -> method.Invoke(child, [|box true|]) |> ignore
        with _ ->
            try if not child.HasExited then child.Kill() with _ -> ()

    let private writeInput (child: Process) (input: string | null) (stop: CancellationToken) : Task = task {
        try
            try
                match input with
                | null -> ()
                | text ->
                    let bytes = UTF8Encoding(false).GetBytes(text)
                    do! child.StandardInput.BaseStream.WriteAsync(bytes, 0, bytes.Length, stop).ConfigureAwait(false)
                    do! child.StandardInput.BaseStream.FlushAsync(stop).ConfigureAwait(false)
            with :? IOException -> () // Programs may close input without consuming it.
        finally
            child.StandardInput.Close()
    }

    let private prepare root executable (arguments: string array) (directory: string | null)
        (environment: IEnumerable<KeyValuePair<string,string>>) (timeout: Nullable<int64>) limit =
        try
            if String.IsNullOrWhiteSpace executable || hasNul executable || limit <= 0L || limit > int64 Int32.MaxValue
               || (timeout.HasValue && (timeout.Value <= 0L || timeout.Value > int64 Int32.MaxValue))
               || (match directory with null -> false | value -> hasNul value) then
                invalidArg "options" "Invalid executable, directory, timeout, or output limit."
            if arguments |> Array.exists hasNul then invalidArg "arguments" "Arguments cannot contain NUL."
            let start = ProcessStartInfo(UseShellExecute=false, RedirectStandardInput=true, RedirectStandardOutput=true,
                                         RedirectStandardError=true, CreateNoWindow=true)
            start.WorkingDirectory <- Path.GetFullPath(Path.Combine(root, match directory with null -> "." | value -> value))
            start.FileName <-
                if not (Path.IsPathRooted executable) && (executable.Contains('/') || executable.Contains(Path.DirectorySeparatorChar)) then
                    Path.GetFullPath(Path.Combine(start.WorkingDirectory, executable))
                else executable
            for argument in arguments do start.ArgumentList.Add argument
            for entry in environment do
                if String.IsNullOrEmpty entry.Key || entry.Key.Contains('=') || hasNul entry.Key || hasNul entry.Value then
                    invalidArg "environment" "Invalid environment variable."
                start.Environment.[entry.Key] <- entry.Value
            Ok start
        with error -> Error ("InvalidOptions: " + error.Message)

    let Run(root: string, executable: string, arguments: string array, workingDirectory: string | null,
            environment: IEnumerable<KeyValuePair<string,string>>, stdin: string | null,
            timeoutMilliseconds: Nullable<int64>, maxOutputBytes: int64, cancellation: CancellationToken) = task {
        use stdout = new Capture()
        use stderr = new Capture()
        let failed message = {ExitCode=0L; Message=message; Stdout=stdout.Text(); Stderr=stderr.Text()}
        match prepare root executable arguments workingDirectory environment timeoutMilliseconds maxOutputBytes with
        | Error message -> return failed message
        | Ok start ->
            cancellation.ThrowIfCancellationRequested()
            use child = new Process(StartInfo=start, EnableRaisingEvents=true)
            let exited = TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously)
            child.Exited.Add(fun _ -> exited.TrySetResult true |> ignore)
            let launched =
                try if child.Start() then Ok () else Error "StartFailed: child did not start."
                with error -> Error ("StartFailed: " + error.Message)
            match launched with
            | Error message -> return failed message
            | Ok () ->
                use stop = new CancellationTokenSource()
                use timer = new CancellationTokenSource()
                let interrupted = TaskCompletionSource<string>(TaskCreationOptions.RunContinuationsAsynchronously)
                let interrupt message = interrupted.TrySetResult message |> ignore
                use cancelRegistration = cancellation.Register(fun () -> interrupt "Cancelled")
                use timeoutRegistration = timer.Token.Register(fun () -> interrupt "TimedOut: child execution timed out.")
                if timeoutMilliseconds.HasValue then timer.CancelAfter(int timeoutMilliseconds.Value)
                let output = stdout.Drain(child.StandardOutput.BaseStream, maxOutputBytes,
                                          (fun () -> interrupt "OutputLimitExceeded: stdout exceeded maxOutputBytes."), stop.Token)
                let errors = stderr.Drain(child.StandardError.BaseStream, maxOutputBytes,
                                          (fun () -> interrupt "OutputLimitExceeded: stderr exceeded maxOutputBytes."), stop.Token)
                let input = writeInput child stdin stop.Token
                let watch (operation: Task) : Task = task {
                    try do! operation.ConfigureAwait(false)
                    with error -> interrupt ("IOFailed: " + error.Message)
                }
                let pipes = Task.WhenAll [|watch output; watch errors; watch input|]
                let complete = Task.WhenAll [|exited.Task :> Task; pipes|]
                try
                    let! _ = Task.WhenAny([|complete; interrupted.Task :> Task|]).ConfigureAwait(false)
                    if not interrupted.Task.IsCompleted then
                        do! complete.ConfigureAwait(false)
                        return {ExitCode=int64 child.ExitCode; Message=null; Stdout=stdout.Text(); Stderr=stderr.Text()}
                    else
                        let! message = interrupted.Task.ConfigureAwait(false)
                        terminate child
                        stop.Cancel()
                        child.StandardInput.Close()
                        child.StandardOutput.Close()
                        child.StandardError.Close()
                        // Local pipe closure releases reads even when descendants inherited write ends.
                        do! pipes.ConfigureAwait(false)
                        let! _ = exited.Task.ConfigureAwait(false)
                        cancellation.ThrowIfCancellationRequested()
                        return failed message
                finally
                    if not exited.Task.IsCompleted then terminate child
    }
