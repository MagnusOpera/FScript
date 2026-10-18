using System.Text.Json.Nodes;
using FScript.Cli.Tests.Fixtures.Valid;
using NUnit.Framework;

namespace FScript.LanguageServer.Tests;

public class NuGetEnvironmentTests
{
    [TestCase(true)]
    [TestCase(false)]
    public void NuGet_providers_use_existing_editor_environment_and_trust(bool trusted)
    {
        using var fixture = new NuGetFixture();
        fixture.AddProvider("1.0.0");
        fixture.WriteManifest("");
        File.WriteAllText(Path.Combine(fixture.Directory,"provider-registration.requested"),"");
        var client = LspClient.Start();
        try
        {
            LspTestFixture.InitializeWith(client,new JsonObject {
                ["configurationPath"] = fixture.ConfigPath,
                ["workspaceFolderUri"] = new Uri(fixture.Directory+Path.DirectorySeparatorChar).AbsoluteUri,
                ["workspaceTrusted"] = trusted
            });
            LspClient.SendRequest(client,30,"fscript/environmentInfo",new JsonObject());
            var info = LspClient.ReadUntil(client,20_000,m => m["id"]?.ToString()=="30")["result"]!;
            Assert.That(info["failed"]!.GetValue<bool>(), Is.False, info.ToJsonString());
            Assert.That(info["blockedByTrust"]!.GetValue<bool>(), Is.EqualTo(!trusted));
            Assert.That(File.Exists(Path.Combine(fixture.Directory,"provider-registration.executed")),Is.EqualTo(trusted));
            Assert.That(info["packages"]!.AsArray().Count,Is.EqualTo(trusted ? 1 : 0));
            Assert.That(Directory.Exists(Path.Combine(fixture.Directory,".fscript")),Is.EqualTo(trusted));
            if (trusted) Assert.That(info["packages"]![0]!["version"]!.GetValue<string>(), Is.EqualTo("1.0.0"));
            var uri = new Uri(Path.Combine(fixture.Directory,"main.fss")).AbsoluteUri;
            LspClient.SendNotification(client,"textDocument/didOpen",new JsonObject { ["textDocument"]=new JsonObject {
                ["uri"]=uri,["languageId"]="fscript",["version"]=1,["text"]="let node = Ext.node 42\nlet value = node.Value\nExt.answer\n"
            }});
            var diagnostics=LspClient.ReadUntil(client,10_000,m => m["method"]?.ToString()=="textDocument/publishDiagnostics" && m["params"]?["uri"]?.ToString()==uri)["params"]!["diagnostics"]!.AsArray();
            Assert.That(diagnostics.Count, trusted ? Is.EqualTo(0) : Is.GreaterThan(0));
            if (trusted)
            {
                JsonNode? Request(string method,int line,int character) {
                    LspClient.SendRequest(client,40,method,new JsonObject { ["textDocument"]=new JsonObject {["uri"]=uri},["position"]=new JsonObject {["line"]=line,["character"]=character} });
                    return LspClient.ReadUntil(client,10_000,m=>m["id"]?.ToString()=="40")["result"];
                }
                Assert.That(Request("textDocument/completion",2,4)?.ToJsonString(),Does.Contain("Ext.answer"));
                Assert.That(Request("textDocument/hover",1,19)?.ToJsonString(),Does.Contain("int"));
                Assert.That(Request("textDocument/completion",1,17)?.ToJsonString(),Does.Contain("Value").And.Not.Contain("Payload"));
            }
        }
        finally { LspClient.Stop(client); }
    }
}
