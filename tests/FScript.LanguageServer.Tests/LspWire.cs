using System.Text;

namespace FScript.LanguageServer.Tests;

internal static class LspWire
{
    private static readonly Encoding Utf8 = new UTF8Encoding(false);
    private static byte[] _pending = Array.Empty<byte>();

    private static void ReadExactWithTimeout(Stream stream, byte[] buffer, int offset, int count, int timeoutMs)
    {
        using var cts = new CancellationTokenSource(timeoutMs);
        var readTotal = 0;
        while (readTotal < count)
        {
            var read = stream.ReadAsync(buffer.AsMemory(offset + readTotal, count - readTotal), cts.Token)
                .GetAwaiter()
                .GetResult();
            if (read <= 0)
            {
                throw new Exception("Unexpected end of stream while reading LSP message.");
            }

            readTotal += read;
        }
    }

    public static string ReadMessageWithTimeout(Stream stream, int timeoutMs)
    {
        using var cts = new CancellationTokenSource(timeoutMs);
        var headerBytes = new List<byte>();
        var one = new byte[1];
        var marker = new[] { (byte)'\r', (byte)'\n', (byte)'\r', (byte)'\n' };
        var matched = 0;
        var doneHeader = false;

        if (_pending.Length > 0)
        {
            headerBytes.AddRange(_pending);
            _pending = Array.Empty<byte>();
        }

        while (!doneHeader)
        {
            if (headerBytes.Count >= marker.Length)
            {
                var c = headerBytes.Count;
                var tail = new[] { headerBytes[c - 4], headerBytes[c - 3], headerBytes[c - 2], headerBytes[c - 1] };
                if (tail.SequenceEqual(marker))
                {
                    doneHeader = true;
                    continue;
                }
            }

            var n = stream.ReadAsync(one.AsMemory(0, 1), cts.Token).GetAwaiter().GetResult();
            if (n <= 0)
            {
                throw new Exception("Unexpected end of stream while reading LSP headers.");
            }

            var b = one[0];
            headerBytes.Add(b);
            if (b == marker[matched])
            {
                matched++;
                if (matched == marker.Length)
                {
                    doneHeader = true;
                }
            }
            else
            {
                matched = b == marker[0] ? 1 : 0;
            }
        }

        var header = Encoding.ASCII.GetString(headerBytes.ToArray());
        var contentLength = header.Split(["\r\n"], StringSplitOptions.RemoveEmptyEntries)
            .Select(line =>
            {
                if (line.StartsWith("Content-Length:", StringComparison.OrdinalIgnoreCase))
                {
                    return int.Parse(line["Content-Length:".Length..].Trim());
                }

                return -1;
            })
            .FirstOrDefault(x => x >= 0);
        if (contentLength < 0)
        {
            throw new Exception("Missing Content-Length header");
        }

        var payload = new byte[contentLength];
        ReadExactWithTimeout(stream, payload, 0, contentLength, timeoutMs);
        return Utf8.GetString(payload);
    }

    public static void WriteMessage(Stream stream, string payload)
    {
        var payloadBytes = Utf8.GetBytes(payload);
        var header = $"Content-Length: {payloadBytes.Length}\r\n\r\n";
        var headerBytes = Encoding.ASCII.GetBytes(header);
        stream.Write(headerBytes, 0, headerBytes.Length);
        stream.Write(payloadBytes, 0, payloadBytes.Length);
        stream.Flush();
    }
}
