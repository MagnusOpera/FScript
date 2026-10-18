using System.Diagnostics;
using System.Net;
using System.Net.Sockets;
using System.Security.Cryptography;
using System.Text.Json;
using System.Text.Json.Nodes;
using System.Threading.Channels;
using FScript.Runtime;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;

namespace FScript.DebugAdapter;

/// CLI entry points. DAP stdio is isolated from the terminal-hosted script process.
public static class DebuggerEntry
{
    private static int Integer(JsonNode? node, string name, int fallback = 0) => node?[name]?.GetValue<int>() ?? fallback;
    private static bool Boolean(JsonNode? node, string name, bool fallback = false) => node?[name]?.GetValue<bool>() ?? fallback;
    private static string Text(JsonNode? node, string name, string fallback = "") => node?[name]?.GetValue<string>() ?? fallback;
    private static FSharpOption<string>? Option(string value) => string.IsNullOrWhiteSpace(value) ? null : FSharpOption<string>.Some(value);
    private static object Source(DebugLocation location) => new { name = Path.GetFileName(location.File), path = location.File };
    private static object Variable(DebugVariable value) => new { name = value.Name, value = value.Value, type = value.Type, variablesReference = value.VariablesReference, namedVariables = value.NamedVariables, indexedVariables = value.IndexedVariables, presentationHint = new { attributes = new[] { "readOnly" } } };

    private static void Coordinates(JsonNode? node, int lineDelta, int columnDelta)
    {
        if (node is JsonObject map)
        {
            foreach (var key in map.Select(pair => pair.Key).ToArray())
            {
                if (map[key] is JsonValue value && value.TryGetValue<int>(out var number))
                {
                    if (key is "line" or "endLine") map[key] = Math.Max(0,number + lineDelta);
                    else if (key is "column" or "endColumn") map[key] = Math.Max(0,number + columnDelta);
                }
                else Coordinates(map[key],lineDelta,columnDelta);
            }
        }
        else if (node is JsonArray list) foreach (var item in list) Coordinates(item,lineDelta,columnDelta);
    }

    public static int RunAdapter() => RunAdapterAsync().GetAwaiter().GetResult();
    private static async Task<int> RunAdapterAsync()
    {
        var dap = new Protocol(Console.OpenStandardInput(), Console.OpenStandardOutput());
        TcpListener? listener = null;
        TcpClient? host = null;
        Protocol? runtime = null;
        Task? forwarding = null;
        JsonObject? pendingLaunch = null;
        var launchStarted = false;
        var supportsTerminal = false;
        var lineDelta = 0;
        var columnDelta = 0;
        var cancellation = new CancellationTokenSource();
        try
        {
            while (await dap.ReadAsync() is { } request)
            {
                if (Text(request, "type") == "response")
                {
                    if (request["success"]?.GetValue<bool>() == false)
                    {
                        await dap.Event("output", new { category = "stderr", output = "Could not launch FScript in the integrated terminal.\n" });
                        var failedLaunch = Interlocked.Exchange(ref pendingLaunch,null);
                        if (failedLaunch is not null) await dap.Response(failedLaunch,error: "VS Code could not launch the FScript integrated terminal.");
                        await dap.Event("terminated");
                        cancellation.Cancel();
                    }
                    continue;
                }
                var command = Text(request, "command");
                var args = request["arguments"];
                if (command == "initialize")
                {
                    supportsTerminal = Boolean(args, "supportsRunInTerminalRequest");
                    lineDelta = Boolean(args,"linesStartAt1",true) ? 0 : 1;
                    columnDelta = Boolean(args,"columnsStartAt1",true) ? 0 : 1;
                    await dap.Response(request, new
                    {
                        supportsConfigurationDoneRequest = true, supportsSingleThreadExecutionRequests = true,
                        supportsEvaluateForHovers = true, supportsExceptionInfoRequest = true, supportsTerminateRequest = true,
                        supportsBreakpointLocationsRequest = true,
                        exceptionBreakpointFilters = new[] { new { filter = "uncaught", label = "Uncaught FScript exceptions", @default = true } }
                    });
                }
                else if (command == "launch")
                {
                    if (launchStarted) { await dap.Response(request, error: "This adapter already owns a launch."); continue; }
                    launchStarted = true;
                    pendingLaunch = (JsonObject)request.DeepClone();
                    if (!supportsTerminal) { await dap.Response(request, error: "FScript debugging requires a client supporting runInTerminal."); continue; }
                    var program = Text(args, "program");
                    if (!File.Exists(program)) { await dap.Response(request, error: $"Script not found: {program}"); continue; }
                    listener = new TcpListener(IPAddress.Loopback, 0);
                    listener.Start();
                    var port = ((IPEndPoint)listener.LocalEndpoint).Port;
                    var token = Convert.ToHexString(RandomNumberGenerator.GetBytes(32));
                    var executable = Environment.ProcessPath ?? throw new InvalidOperationException("Cannot determine CLI executable.");
                    var childArgs = new List<string> { executable };
                    // Framework-dependent tools run through dotnet; native distributions invoke themselves.
                    if (Path.GetFileNameWithoutExtension(executable).Equals("dotnet", StringComparison.OrdinalIgnoreCase))
                        childArgs.Add(Environment.GetCommandLineArgs()[0]);
                    childArgs.AddRange(["--debug-host", port.ToString(System.Globalization.CultureInfo.InvariantCulture), token]);
                    await dap.WriteAsync(new JsonObject
                    {
                        ["type"] = "request", ["command"] = "runInTerminal",
                        ["arguments"] = JsonSerializer.SerializeToNode(new { kind = "integrated", title = "FScript Debug", cwd = Text(args,"cwd",Path.GetDirectoryName(Path.GetFullPath(program))!), args = childArgs })
                    });
                    // Keep accepting DAP traffic while the terminal process starts and loads providers.
                    forwarding = Task.Run(async () =>
                    {
                        try
                        {
                            using var timeout = CancellationTokenSource.CreateLinkedTokenSource(cancellation.Token);
                            timeout.CancelAfter(TimeSpan.FromSeconds(30));
                            host = await listener.AcceptTcpClientAsync(timeout.Token);
                            runtime = new Protocol(host.GetStream(),host.GetStream());
                            var hello = await runtime.ReadAsync(timeout.Token);
                            if (Text(hello,"token") != token) throw new InvalidDataException("Invalid debugger host authentication.");
                            listener.Stop();
                            request["client_seq"] = request["seq"]!.GetValue<int>();
                            await runtime.WriteAsync(request);
                            while (await runtime.ReadAsync(cancellation.Token) is { } message)
                            {
                                if (Text(message,"type") == "response" && Text(message,"command") == "launch") Interlocked.Exchange(ref pendingLaunch,null);
                                Coordinates(message,-lineDelta,-columnDelta);
                                await dap.WriteAsync(message);
                            }
                        }
                        catch (Exception error)
                        {
                            if (!cancellation.IsCancellationRequested)
                            {
                                var failedLaunch = Interlocked.Exchange(ref pendingLaunch,null);
                                if (failedLaunch is not null) await dap.Response(failedLaunch,error: error.Message);
                                await dap.Event("terminated");
                            }
                        }
                    });
                }
                else if (runtime is not null)
                {
                    // Preserve client request IDs across the private channel.
                    request["client_seq"] = request["seq"]!.GetValue<int>();
                    Coordinates(request["arguments"],lineDelta,columnDelta);
                    await runtime.WriteAsync(request);
                }
                else if (command is "disconnect" or "terminate") { await dap.Response(request); break; }
                else await dap.Response(request,error: "No FScript debug host is connected.");
            }
        }
        finally
        {
            cancellation.Cancel();
            host?.Dispose();
            listener?.Stop();
            if (forwarding is not null) { try { await forwarding; } catch (OperationCanceledException) { } }
            cancellation.Dispose();
        }
        return 0;
    }

    public static int RunHost(int port, string token)
    {
        using var connection = new TcpClient();
        connection.Connect(IPAddress.Loopback,port);
        var protocol = new Protocol(connection.GetStream(),connection.GetStream());
        protocol.WriteAsync(new JsonObject { ["token"] = token }).GetAwaiter().GetResult();
        var launch = protocol.ReadAsync().GetAwaiter().GetResult() ?? throw new IOException("Debugger disconnected before launch.");
        if (launch["client_seq"] is { } originalLaunch) launch["seq"] = originalLaunch.GetValue<int>();
        var args = launch["arguments"];
        DebugSession session;
        try
        {
            session = DebugLaunch.create(Text(args,"program"),Option(Text(args,"configurationPath")),true,Option(Text(args,"rootDirectory")),
                args?["args"]?.AsArray().Select(value => value!.GetValue<string>()).ToArray() ?? [],Boolean(args,"stopOnEntry"));
        }
        catch (Exception error)
        {
            protocol.Response(launch,error: error.Message).GetAwaiter().GetResult();
            protocol.Event("terminated").GetAwaiter().GetResult();
            return 1;
        }
        using (session)
        using (var configured = new ManualResetEventSlim())
        {
            var terminated = 0;
            var events = Channel.CreateUnbounded<DebugEvent>(new UnboundedChannelOptions { SingleReader = true, AllowSynchronousContinuations = false });
            session.Events.AddHandler((_,value) => events.Writer.TryWrite(value));
            var eventPump = Task.Run(async () =>
            {
                await foreach (var value in events.Reader.ReadAllAsync())
                {
                    var send = value.Kind switch
                {
                    "threadStarted" => protocol.Event("thread",new { reason = "started", threadId = value.TaskId }),
                    "threadExited" => protocol.Event("thread",new { reason = "exited", threadId = value.TaskId }),
                    "stopped" => protocol.Event("stopped",new { reason = value.Reason, threadId = value.TaskId, allThreadsStopped = false }),
                    "continued" => protocol.Event("continued",new { threadId = value.TaskId, allThreadsContinued = false }),
                    _ => Task.CompletedTask
                };
                    await send;
                }
            });
            protocol.Response(launch).GetAwaiter().GetResult();
            protocol.Event("initialized").GetAwaiter().GetResult();
            var reader = Task.Run(async () =>
            {
                try
                {
                    while (await protocol.ReadAsync() is { } request)
                    {
                        // Protocol transport has its own sequence; restore the original DAP request sequence.
                        if (request["client_seq"] is { } original) request["seq"] = original.GetValue<int>();
                        var command = Text(request,"command");
                        var argument = request["arguments"];
                        try
                        {
                            object? body = null;
                            switch (command)
                            {
                                case "configurationDone":
                                    await protocol.Response(request);
                                    configured.Set();
                                    continue;
                                case "setBreakpoints":
                                    if (argument?["breakpoints"]?.AsArray().Any(point => point?["condition"] is not null || point?["hitCondition"] is not null || point?["logMessage"] is not null) == true)
                                        throw new NotSupportedException("Conditional breakpoints, hit counts, and logpoints are not supported.");
                                    var path = Text(argument?["source"],"path");
                                    var points = argument?["breakpoints"]?.AsArray().Select(point => Tuple.Create(Integer(point,"line"),Integer(point,"column",1))) ?? [];
                                    body = new { breakpoints = session.SetBreakpoints(path,ListModule.OfSeq(points)).Select(point => new { id = point.Id, verified = point.Verified, message = point.Message, source = Source(point.Location), line = point.Location.Line, column = point.Location.Column, endLine = point.Location.EndLine, endColumn = point.Location.EndColumn }) };
                                    break;
                                case "breakpointLocations":
                                    body = new { breakpoints = session.BreakpointLocations(Text(argument?["source"],"path"),Integer(argument,"line"),Integer(argument,"endLine",Integer(argument,"line"))).Select(point => new { line = point.Line, column = point.Column, endLine = point.EndLine, endColumn = point.EndColumn }) };
                                    break;
                                case "setExceptionBreakpoints": session.SetExceptionBreakpoints(argument?["filters"]?.AsArray().Any(filter => filter?.GetValue<string>() == "uncaught") == true); break;
                                case "threads": body = new { threads = session.Threads().Select(task => new { id = task.Id, name = task.Name + (task.State == "running" ? "" : $" ({task.State})") }) }; break;
                                case "stackTrace":
                                    var frames = session.StackTrace(Integer(argument,"threadId"));
                                    body = new { stackFrames = frames.Skip(Integer(argument,"startFrame")).Take(Integer(argument,"levels",frames.Length) is 0 ? frames.Length : Integer(argument,"levels",frames.Length)).Select(frame => new { id = frame.Id, name = frame.Name, source = Source(frame.Location), line = Math.Max(1,frame.Location.Line), column = Math.Max(1,frame.Location.Column), endLine = Math.Max(1,frame.Location.EndLine), endColumn = Math.Max(1,frame.Location.EndColumn) }), totalFrames = frames.Length };
                                    break;
                                case "scopes": body = new { scopes = session.Scopes(Integer(argument,"frameId")).Select(scope => new { name = scope.Name, variablesReference = scope.VariablesReference, expensive = scope.Expensive }) }; break;
                                case "variables": body = new { variables = session.Variables(Integer(argument,"variablesReference"),Integer(argument,"start"),Integer(argument,"count")).Select(Variable) }; break;
                                case "evaluate":
                                    var result = session.Evaluate(Integer(argument,"frameId"),Text(argument,"expression"));
                                    body = new { result = result.Value, type = result.Type, variablesReference = result.VariablesReference, namedVariables = result.NamedVariables, indexedVariables = result.IndexedVariables };
                                    break;
                                case "exceptionInfo": body = new { exceptionId = "FScript", description = session.ExceptionInfo(Integer(argument,"threadId")), breakMode = "unhandled" }; break;
                                case "continue": case "next": case "stepIn": case "stepOut":
                                    session.ValidateResume(Integer(argument,"threadId"));
                                    // Acknowledge before execution can produce its next stop event.
                                    await protocol.Response(request,command == "continue" ? new { allThreadsContinued = !Boolean(argument,"singleThread") } : null);
                                    session.Resume(Integer(argument,"threadId"),command switch { "next" => "over", "stepIn" => "in", "stepOut" => "out", _ => "continue" },Boolean(argument,"singleThread"));
                                    continue;
                                case "pause": session.Pause(Integer(argument,"threadId")); break;
                                case "disconnect": case "terminate":
                                    await protocol.Response(request);
                                    session.Cancel(); configured.Set();
                                    _ = Task.Delay(2000).ContinueWith(_ => Environment.Exit(0));
                                    return;
                                default: throw new NotSupportedException($"Unsupported debugger request '{command}'.");
                            }
                            await protocol.Response(request,body);
                        }
                        catch (Exception error) { await protocol.Response(request,error: error.Message); }
                    }
                }
                finally
                {
                    session.Cancel(); configured.Set();
                    if (Volatile.Read(ref terminated) == 0) _ = Task.Delay(2000).ContinueWith(_ => Environment.Exit(0));
                }
            });
            var exit = 0;
            try { configured.Wait(); session.Run(); Console.WriteLine(session.ResultSummary); }
            catch (OperationCanceledException) { }
            catch (Exception error) { Console.Error.WriteLine(error.Message); exit = 3; }
            finally
            {
                Interlocked.Exchange(ref terminated,1);
                try
                {
                    events.Writer.TryComplete();
                    eventPump.GetAwaiter().GetResult();
                    protocol.Event("exited",new { exitCode = exit }).GetAwaiter().GetResult();
                    protocol.Event("terminated").GetAwaiter().GetResult();
                }
                catch (Exception error) when (error is IOException or ObjectDisposedException) { }
                session.Cancel();
            }
            return exit;
        }
    }
}
