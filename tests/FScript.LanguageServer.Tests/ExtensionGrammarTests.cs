using System.Text.Json.Nodes;
using System.Text.RegularExpressions;
using NUnit.Framework;

namespace FScript.LanguageServer.Tests;

[TestFixture]
public sealed class ExtensionGrammarTests
{
    [Test]
    public void TextMate_grammar_highlights_numeric_annotations_without_matching_identifier_substrings()
    {
        DirectoryInfo? root = new(AppContext.BaseDirectory);
        while (root is not null && !File.Exists(Path.Combine(root.FullName, "FScript.sln")))
            root = root.Parent;
        Assert.That(root, Is.Not.Null, "Unable to locate repository root");

        var grammarPath = Path.Combine(root!.FullName, "vscode-fscript", "syntaxes", "fscript.tmLanguage.json");
        var grammar = JsonNode.Parse(File.ReadAllText(grammarPath))!;
        var patterns = grammar["repository"]!["types"]!["patterns"]!.AsArray();
        var typePattern = patterns.Single(p => p?["name"]?.GetValue<string>() == "storage.type.fscript")!;
        var regex = new Regex(typePattern["match"]!.GetValue<string>());

        var matches = regex.Matches("let add (x: numeric) (y: numeric) : numeric = x + y");
        Assert.That(matches.Select(m => m.Value), Is.EqualTo(new[] { "numeric", "numeric", "numeric" }));
        Assert.That(regex.IsMatch("numericValue numeric_value nonnumeric"), Is.False);
        Assert.That(regex.Matches("int float string bool unit list option map").Count, Is.EqualTo(8));
    }
}
