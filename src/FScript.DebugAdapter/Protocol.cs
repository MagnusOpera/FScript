using System.Globalization;
using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;

namespace FScript.DebugAdapter;

// Shared only as transport: runtime inspection semantics remain in DebugSession.
internal sealed class Protocol(Stream input, Stream output)
{
    private readonly SemaphoreSlim writeLock = new(1);
    private int sequence;
    internal async Task<JsonObject?> ReadAsync(CancellationToken cancellation = default)
    {
        var header = new List<byte>();
        while (header.Count < 8192)
        {
            var single = new byte[1];
            if (await input.ReadAsync(single, cancellation) == 0)
            {
                if (header.Count == 0) return null;
                throw new EndOfStreamException("Incomplete debugger header.");
            }
            header.Add(single[0]);
            var n = header.Count;
            if (n >= 4 && header[n-4] == 13 && header[n-3] == 10 && header[n-2] == 13 && header[n-1] == 10) break;
        }
        var lengthText = Encoding.ASCII.GetString(header.ToArray()).Split("\r\n")
            .FirstOrDefault(line => line.StartsWith("Content-Length:", StringComparison.OrdinalIgnoreCase));
        if (lengthText is null || !int.TryParse(lengthText[15..].Trim(), NumberStyles.None, CultureInfo.InvariantCulture, out var length) || length < 0 || length > 16 * 1024 * 1024)
            throw new InvalidDataException("Invalid debugger Content-Length.");
        var body = new byte[length];
        await input.ReadExactlyAsync(body, cancellation);
        return JsonNode.Parse(body)?.AsObject() ?? throw new InvalidDataException("Expected a debugger message object.");
    }
    internal async Task WriteAsync(JsonObject message)
    {
        await writeLock.WaitAsync();
        try
        {
            message["seq"] = ++sequence;
            var body = Encoding.UTF8.GetBytes(message.ToJsonString());
            await output.WriteAsync(Encoding.ASCII.GetBytes($"Content-Length: {body.Length}\r\n\r\n"));
            await output.WriteAsync(body);
            await output.FlushAsync();
        }
        finally { writeLock.Release(); }
    }
    internal Task Response(JsonObject request, object? body = null, string? error = null) => WriteAsync(new JsonObject
    {
        ["type"] = "response", ["request_seq"] = request["seq"]!.GetValue<int>(), ["command"] = request["command"]!.GetValue<string>(),
        ["success"] = error is null, ["body"] = body is null ? null : JsonSerializer.SerializeToNode(body), ["message"] = error
    });
    internal Task Event(string name, object? body = null) => WriteAsync(new JsonObject
    {
        ["type"] = "event", ["event"] = name, ["body"] = body is null ? null : JsonSerializer.SerializeToNode(body)
    });
}
