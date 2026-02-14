using System.Diagnostics;
using System.Text.Json.Nodes;

namespace FScript.LanguageServer.Tests;

internal static class LspClient
{
    internal sealed class Client
    {
        public required Process Process { get; init; }
        public required Stream Input { get; init; }
        public required Stream Output { get; init; }
    }

    private static string FindRepoRoot()
    {
        DirectoryInfo? current = new(AppContext.BaseDirectory);
        while (current is not null)
        {
            var candidate = Path.Combine(current.FullName, "FScript.sln");
            if (File.Exists(candidate))
            {
                return current.FullName;
            }

            current = current.Parent;
        }

        throw new Exception("Unable to locate repository root from test base directory");
    }

    private static readonly Lazy<string> EnsureServerDllBuilt = new(() =>
    {
        var root = FindRepoRoot();
        var serverProject = Path.Combine(root, "src", "FScript.LanguageServer", "FScript.LanguageServer.csproj");
        var serverDll = Path.Combine(root, "src", "FScript.LanguageServer", "bin", "Release", "net10.0", "FScript.LanguageServer.dll");

        var buildPsi = new ProcessStartInfo
        {
            FileName = "dotnet",
            Arguments = $"build \"{serverProject}\" -c Release -nologo -v q",
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            CreateNoWindow = true
        };

        using var buildProc = new Process { StartInfo = buildPsi };
        if (!buildProc.Start())
        {
            throw new Exception("Unable to start dotnet build for C# language server test setup.");
        }

        buildProc.WaitForExit();
        if (buildProc.ExitCode != 0 || !File.Exists(serverDll))
        {
            var output = buildProc.StandardOutput.ReadToEnd();
            var err = buildProc.StandardError.ReadToEnd();
            throw new Exception($"Failed to build C# language server test target. stdout: {output}\nstderr: {err}");
        }

        return serverDll;
    });

    public static Client Start()
    {
        var serverDll = EnsureServerDllBuilt.Value;

        var psi = new ProcessStartInfo
        {
            FileName = "dotnet",
            Arguments = $"\"{serverDll}\"",
            RedirectStandardInput = true,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            CreateNoWindow = true
        };

        var proc = new Process { StartInfo = psi };
        if (!proc.Start())
        {
            throw new Exception("Unable to start FScript C# language server process");
        }

        return new Client
        {
            Process = proc,
            Input = proc.StandardInput.BaseStream,
            Output = proc.StandardOutput.BaseStream
        };
    }

    public static Client StartCSharp() => Start();
    public static Client StartFSharp() => Start();

    public static void Stop(Client client)
    {
        if (!client.Process.HasExited)
        {
            try
            {
                client.Process.Kill(true);
            }
            catch
            {
                // Ignore
            }
        }

        client.Process.Dispose();
    }

    public static void SendRequest(Client client, int id, string methodName, JsonNode? parameters)
    {
        var payload = new JsonObject
        {
            ["jsonrpc"] = "2.0",
            ["id"] = id,
            ["method"] = methodName,
            ["params"] = parameters ?? new JsonObject()
        };
        LspWire.WriteMessage(client.Input, payload.ToJsonString());
    }

    public static void SendNotification(Client client, string methodName, JsonNode? parameters)
    {
        var payload = new JsonObject
        {
            ["jsonrpc"] = "2.0",
            ["method"] = methodName,
            ["params"] = parameters ?? new JsonObject()
        };
        LspWire.WriteMessage(client.Input, payload.ToJsonString());
    }

    public static JsonObject ReadUntil(Client client, int timeoutMs, Func<JsonObject, bool> predicate)
    {
        var deadline = DateTime.UtcNow.AddMilliseconds(timeoutMs);
        while (DateTime.UtcNow < deadline)
        {
            var remaining = (int)(deadline - DateTime.UtcNow).TotalMilliseconds;
            if (remaining <= 0)
            {
                break;
            }

            var raw = LspWire.ReadMessageWithTimeout(client.Output, remaining);
            var node = JsonNode.Parse(raw);
            if (node is JsonObject obj && predicate(obj))
            {
                return obj;
            }
        }

        throw new Exception("Timed out waiting for expected LSP message");
    }
}
