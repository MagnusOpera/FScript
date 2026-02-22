using System.Text.Json.Nodes;
using FScript.CSharpInterop;
using FScript.Language;
using Microsoft.FSharp.Core;

namespace FScript.LanguageServer.CSharp;

internal static class LspHandlers
{
    private static readonly Position FallbackPosition = new(FSharpOption<string>.None, 1, 1);
    private static readonly Span FallbackSpan = new(FallbackPosition, FallbackPosition);

    internal static JsonObject CreateInitializeResult()
    {
        var sync = new JsonObject
        {
            ["openClose"] = true,
            ["change"] = 1
        };

        var capabilities = new JsonObject
        {
            ["textDocumentSync"] = sync,
            ["hoverProvider"] = true
        };

        return new JsonObject
        {
            ["capabilities"] = capabilities,
            ["serverInfo"] = new JsonObject
            {
                ["name"] = "FScript Language Server (C#)"
            }
        };
    }

    internal static JsonObject HandleViewAst(JsonObject? @params, Func<string, string?> tryLoadSource)
    {
        var uri = TryGetCommandUri(@params);
        if (string.IsNullOrWhiteSpace(uri))
        {
            return Error("internal", "Missing document URI.");
        }

        if (!Uri.TryCreate(uri, UriKind.Absolute, out var parsed) ||
            !string.Equals(parsed.Scheme, "file", StringComparison.OrdinalIgnoreCase))
        {
            return Error("internal", "AST commands support file-based scripts only.");
        }

        var sourcePath = parsed.LocalPath;
        var sourceText = tryLoadSource(uri);
        if (sourceText is null)
        {
            return Error("internal", $"Unable to read source file '{sourcePath}'.");
        }

        try
        {
            var program = InteropServices.parseProgramFromSourceWithIncludes(sourcePath, sourceText);
            var data = global::FScript.LanguageServer.AstJson.programToJson(sourcePath, program);
            return new JsonObject
            {
                ["ok"] = true,
                ["data"] = data
            };
        }
        catch (ParseException ex)
        {
            return Error("parse", ex.Message);
        }
        catch (Exception ex)
        {
            return Error("internal", ex.Message);
        }
    }

    internal static JsonObject HandleViewInferredAst(JsonObject? @params, Func<string, string?> tryLoadSource)
    {
        var uri = TryGetCommandUri(@params);
        if (string.IsNullOrWhiteSpace(uri))
        {
            return Error("internal", "Missing document URI.");
        }

        if (!Uri.TryCreate(uri, UriKind.Absolute, out var parsed) ||
            !string.Equals(parsed.Scheme, "file", StringComparison.OrdinalIgnoreCase))
        {
            return Error("internal", "AST commands support file-based scripts only.");
        }

        var sourcePath = parsed.LocalPath;
        var sourceText = tryLoadSource(uri);
        if (sourceText is null)
        {
            return Error("internal", $"Unable to read source file '{sourcePath}'.");
        }

        try
        {
            var program = InteropServices.parseProgramFromSourceWithIncludes(sourcePath, sourceText);
            var externs = InteropServices.runtimeExternsForSourcePath(sourcePath);
            var typed = InteropServices.inferProgramWithExterns(externs, program);
            var data = global::FScript.LanguageServer.AstJson.typedProgramToJson(sourcePath, typed);
            return new JsonObject
            {
                ["ok"] = true,
                ["data"] = data
            };
        }
        catch (ParseException ex)
        {
            return Error("parse", ex.Message);
        }
        catch (TypeException ex)
        {
            return Error("type", ex.Message);
        }
        catch (Exception ex)
        {
            return Error("internal", ex.Message);
        }
    }

    internal static JsonObject CreateDiagnosticsParams(string uri, string text)
    {
        var diagnostics = new JsonArray();
        var sourcePath = ResolveSourcePath(uri);

        try
        {
            var program = InteropServices.parseProgramFromSourceWithIncludes(sourcePath, text);
            var externs = InteropServices.runtimeExternsForSourcePath(sourcePath);
            _ = InteropServices.inferProgramWithExterns(externs, program);
        }
        catch (ParseException ex)
        {
            diagnostics.Add(CreateDiagnostic("parse", ex.Message, ExtractSpan(ex)));
        }
        catch (TypeException ex)
        {
            diagnostics.Add(CreateDiagnostic("type", ex.Message, ExtractSpan(ex)));
        }

        return new JsonObject
        {
            ["uri"] = uri,
            ["diagnostics"] = diagnostics
        };
    }

    private static string ResolveSourcePath(string uri)
    {
        if (Uri.TryCreate(uri, UriKind.Absolute, out var parsed) &&
            string.Equals(parsed.Scheme, "file", StringComparison.OrdinalIgnoreCase))
        {
            return parsed.LocalPath;
        }

        return uri;
    }

    private static JsonObject CreateDiagnostic(string code, string message, Span span)
    {
        var startLine = Math.Max(0, span.Start.Line - 1);
        var startCharacter = Math.Max(0, span.Start.Column - 1);
        var endLine = Math.Max(0, span.End.Line - 1);
        var endCharacter = Math.Max(0, span.End.Column - 1);

        return new JsonObject
        {
            ["range"] = new JsonObject
            {
                ["start"] = new JsonObject
                {
                    ["line"] = startLine,
                    ["character"] = startCharacter
                },
                ["end"] = new JsonObject
                {
                    ["line"] = endLine,
                    ["character"] = endCharacter
                }
            },
            ["severity"] = 1,
            ["code"] = code,
            ["source"] = "fscript-lsp",
            ["message"] = message
        };
    }

    private static Span ExtractSpan(Exception ex)
    {
        try
        {
            var data0 = ex.GetType().GetProperty("Data0")?.GetValue(ex);
            if (data0 is null)
            {
                return FallbackSpan;
            }

            var spanObj = data0.GetType().GetProperty("Span")?.GetValue(data0);
            if (spanObj is Span span)
            {
                return span;
            }
        }
        catch
        {
            // Ignore and fallback to default span.
        }

        return FallbackSpan;
    }

    private static string? TryGetCommandUri(JsonObject? @params)
    {
        var fromTextDocument = (@params?["textDocument"] as JsonObject)?["uri"]?.GetValue<string>();
        if (!string.IsNullOrWhiteSpace(fromTextDocument))
        {
            return fromTextDocument;
        }

        return @params?["uri"]?.GetValue<string>();
    }

    private static JsonObject Error(string kind, string message)
    {
        return new JsonObject
        {
            ["ok"] = false,
            ["error"] = new JsonObject
            {
                ["kind"] = kind,
                ["message"] = message
            }
        };
    }
}
