using System.Text;

namespace FScript.LanguageServer.CSharp;

internal static class JsonRpcWire
{
    internal static string? ReadMessage(Stream input)
    {
        var contentLength = -1;

        while (true)
        {
            var line = ReadHeaderLine(input);
            if (line is null)
            {
                return null;
            }

            if (line.Length == 0)
            {
                break;
            }

            const string prefix = "Content-Length:";
            if (line.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
            {
                var value = line[prefix.Length..].Trim();
                if (int.TryParse(value, out var parsed))
                {
                    contentLength = parsed;
                }
            }
        }

        if (contentLength < 0)
        {
            return null;
        }

        var payload = new byte[contentLength];
        var offset = 0;
        while (offset < payload.Length)
        {
            var read = input.Read(payload, offset, payload.Length - offset);
            if (read <= 0)
            {
                return null;
            }

            offset += read;
        }

        return Encoding.UTF8.GetString(payload);
    }

    internal static void WriteMessage(Stream output, string json)
    {
        var bytes = Encoding.UTF8.GetBytes(json);
        var header = Encoding.ASCII.GetBytes($"Content-Length: {bytes.Length}\r\n\r\n");
        output.Write(header, 0, header.Length);
        output.Write(bytes, 0, bytes.Length);
        output.Flush();
    }

    private static string? ReadHeaderLine(Stream input)
    {
        using var buffer = new MemoryStream();

        while (true)
        {
            var value = input.ReadByte();
            if (value < 0)
            {
                return buffer.Length == 0 ? null : Encoding.ASCII.GetString(buffer.ToArray()).TrimEnd('\r');
            }

            if (value == '\n')
            {
                return Encoding.ASCII.GetString(buffer.ToArray()).TrimEnd('\r');
            }

            buffer.WriteByte((byte)value);
        }
    }
}
