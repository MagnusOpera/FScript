namespace FScript.LanguageServer.Tests

open System
open System.IO
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open System.Diagnostics
open System.Threading
open NUnit.Framework
open FsUnit
module internal LspClient =
    type Client =
        { Process: Process
          Input: Stream
          Output: Stream }

    let private findRepoRoot () =
        let mutable current : DirectoryInfo option = Some (DirectoryInfo(AppContext.BaseDirectory))
        let mutable found: string option = None

        while current.IsSome && found.IsNone do
            let directory = current.Value
            let candidate = Path.Combine(directory.FullName, "FScript.sln")
            if File.Exists(candidate) then
                found <- Some directory.FullName
            else
                current <- Option.ofObj directory.Parent

        found |> Option.defaultWith (fun () -> failwith "Unable to locate repository root from test base directory")

    let private ensureServerDllBuilt =
        lazy (
            let root = findRepoRoot ()
            let serverProject = Path.Combine(root, "src", "FScript.LanguageServer", "FScript.LanguageServer.fsproj")
            let serverDll = Path.Combine(root, "src", "FScript.LanguageServer", "bin", "Release", "net10.0", "FScript.LanguageServer.dll")

            let buildPsi =
                ProcessStartInfo(
                    FileName = "dotnet",
                    Arguments = $"build \"{serverProject}\" -c Release -nologo -v q",
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    UseShellExecute = false,
                    CreateNoWindow = true)

            use buildProc = new Process(StartInfo = buildPsi)
            if not (buildProc.Start()) then
                failwith "Unable to start dotnet build for language server test setup."
            buildProc.WaitForExit()
            if buildProc.ExitCode <> 0 || not (File.Exists(serverDll)) then
                let out = buildProc.StandardOutput.ReadToEnd()
                let err = buildProc.StandardError.ReadToEnd()
                failwith $"Failed to build language server test target. stdout: {out}\nstderr: {err}"

            serverDll)

    let start () =
        let serverDll = ensureServerDllBuilt.Value

        let psi =
            ProcessStartInfo(
                FileName = "dotnet",
                Arguments = $"\"{serverDll}\"",
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                CreateNoWindow = true)

        let proc = new Process(StartInfo = psi)
        let started = proc.Start()
        if not started then failwith "Unable to start FScript language server process"

        { Process = proc
          Input = proc.StandardInput.BaseStream
          Output = proc.StandardOutput.BaseStream }

    let stop (client: Client) =
        if not client.Process.HasExited then
            try
                client.Process.Kill(true)
            with _ -> ()
        client.Process.Dispose()

    let sendRequest (client: Client) (id: int) (methodName: string) (parameters: JsonNode option) =
        let payload = JsonObject()
        payload["jsonrpc"] <- JsonValue.Create("2.0")
        payload["id"] <- JsonValue.Create(id)
        payload["method"] <- JsonValue.Create(methodName)
        payload["params"] <- (parameters |> Option.defaultValue (JsonObject()))
        LspWire.writeMessage client.Input (payload.ToJsonString())

    let sendNotification (client: Client) (methodName: string) (parameters: JsonNode option) =
        let payload = JsonObject()
        payload["jsonrpc"] <- JsonValue.Create("2.0")
        payload["method"] <- JsonValue.Create(methodName)
        payload["params"] <- (parameters |> Option.defaultValue (JsonObject()))
        LspWire.writeMessage client.Input (payload.ToJsonString())

    let readUntil (client: Client) (timeoutMs: int) (predicate: JsonObject -> bool) =
        let deadline = DateTime.UtcNow.AddMilliseconds(float timeoutMs)
        let mutable found: JsonObject option = None

        while found.IsNone && DateTime.UtcNow < deadline do
            let remaining = int (deadline - DateTime.UtcNow).TotalMilliseconds
            if remaining <= 0 then
                ()
            else
                let raw = LspWire.readMessageWithTimeout client.Output remaining
                let node = JsonNode.Parse(raw)
                match node with
                | :? JsonObject as obj when predicate obj ->
                    found <- Some obj
                | _ -> ()

        found |> Option.defaultWith (fun () -> failwith "Timed out waiting for expected LSP message")
