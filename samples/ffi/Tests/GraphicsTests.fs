namespace FScript.Ffi.Tests

open System
open System.IO
open NUnit.Framework
open FScript.Language
open FScript.Runtime
open FScript.Extensibility

module ScriptHost = FScript.Testing.SdkTestHost
open FScript.Ffi.Sample
open FScript.Ffi.Rendering

[<TestFixture>]
type GraphicsTests() =
    let loaded = lazy (ScriptHost.loadFileWithRegistration Graphics.registration (Path.Combine(__SOURCE_DIRECTORY__, "..", "scene.fss")))
    let invoke name values = ScriptHost.invoke loaded.Value name values
    let wrap value = HostTypes.wrap Graphics.registration value
    let window backend = wrap { Title = "test"; Backend = backend }
    let zero : Vec3 = { X = 0.0; Y = 0.0; Z = 0.0 }
    let white : Color = { R = 1.0; G = 1.0; B = 1.0 }
    let triangle : Geometry =
        { Name = "triangle"
          Vertices = [| { Position = zero; Normal = { X = 0.0; Y = 1.0; Z = 0.0 } };
                       { Position = { zero with X = 1.0 }; Normal = { X = 0.0; Y = 1.0; Z = 0.0 } };
                       { Position = { zero with Z = 1.0 }; Normal = { X = 0.0; Y = 1.0; Z = 0.0 } } |]
          Indices = [|0L; 1L; 2L|] }
    let call name args =
        let fn = Graphics.registration.Functions |> List.find (fun f -> f.Name = name)
        let context = ExternalCallContext((fun _ _ -> failwith "unused"), (fun _ -> failwith "unused"), (fun _ -> failwith "unused"), ignore)
        fn.Impl context args

    [<Test>]
    member _.``provider discovery builds descriptors without a window``() =
        let registration = GraphicsProvider.Provide()
        Assert.That(registration.Types.Length, Is.EqualTo 9)
        Assert.That(registration.Functions.Length, Is.EqualTo 10)
        for descriptor in registration.Types do
            Assert.That(descriptor.HostManaged, Is.EqualTo(descriptor.Name = "Window" || descriptor.Name = "Mesh"))
        let mesh = registration.Types |> List.find (fun t -> t.Name = "Mesh")
        Assert.That(mesh.Fields.ContainsKey "Resource", Is.False)
        Assert.That(mesh.Fields.ContainsKey "Backend", Is.False)

    [<Test>]
    member _.``assembly environment loads the real provider without native initialization``() =
        let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
        let settings = EnvironmentConfiguration.merge root root None None true [typeof<GraphicsProvider>.Assembly.Location]
        match EnvironmentLoader.load settings with
        | Error error -> Assert.Fail error
        | Ok registration ->
            Assert.That(registration.Types.Length, Is.EqualTo 9)
            Assert.That(registration.Functions.Length, Is.EqualTo 10)
            let scene = FScript.Runtime.ScriptHost.loadFileWithRegistration registration (Path.Combine(root, "scene.fss"))
            Assert.That(scene.ExportedFunctionNames, Does.Contain "render")

    [<Test>]
    member _.``real FScript teapot generates bounded indexed geometry with unit normals``() =
        let backend = RecordingGraphics()
        let mesh = invoke "initialize" [window backend] |> HostTypes.unwrap<Mesh>
        Assert.That(mesh.VertexCount, Is.EqualTo(32L * 9L * 9L))
        Assert.That(mesh.TriangleCount, Is.EqualTo(32L * 8L * 8L * 2L))
        Assert.That(mesh.Name, Is.EqualTo "Utah teapot")
        let struct (vertices, indices) = backend.Meshes[0]
        Assert.That(vertices.Length, Is.EqualTo(int mesh.VertexCount * 6))
        Assert.That(indices |> Array.forall (fun i -> i < uint32 mesh.VertexCount), Is.True)
        Assert.That(vertices |> Array.forall Single.IsFinite, Is.True)
        let xs = [| for i in 0 .. 6 .. vertices.Length - 1 -> vertices[i] |]
        Assert.That(Array.min xs, Is.LessThan(-2.5f)) // handle
        Assert.That(Array.max xs, Is.GreaterThan(3.0f)) // spout
        for i in 0 .. 6 .. vertices.Length - 1 do
            let x, y, z = vertices[i+3], vertices[i+4], vertices[i+5]
            Assert.That(x*x+y*y+z*z, Is.EqualTo(1.0f).Within(0.001f))
        Assert.That(Object.ReferenceEquals(mesh.Backend, backend), Is.True)

    [<Test>]
    member _.``scene sends camera lighting and changing rotations without another upload``() =
        let backend = RecordingGraphics()
        let w = window backend
        let mesh = invoke "initialize" [w]
        for seconds in [0.0; 1.0; 2.0] do
            invoke "render" [w; mesh; VFloat seconds] |> ignore
        Assert.That(backend.Meshes.Count, Is.EqualTo 1)
        Assert.That(backend.Cameras, Is.EqualTo 3)
        Assert.That(backend.Lights, Is.EqualTo 3)
        Assert.That(backend.Clears, Is.EqualTo 3)
        Assert.That(backend.Rotations.Count, Is.EqualTo 3)
        Assert.That(backend.Rotations[0], Is.Not.EqualTo(backend.Rotations[2]))

    [<TestCase("let x = { Title = \"fake\" }\nGraphics.clear x { R = 0.0; G = 0.0; B = 0.0 }")>]
    [<TestCase("export let inspect (w: Window) = w.Backend")>]
    [<TestCase("export let inspect (w: Window) = { w with Title = \"changed\" }")>]
    member _.``opaque resources cannot be forged inspected or updated`` source =
        Assert.That(Action(fun () -> ScriptHost.loadSourceWithRegistration Graphics.registration source |> ignore), Throws.Exception)

    [<TestCase(-1L)>]
    [<TestCase(3L)>]
    member _.``bad triangle indices fail before backend allocation`` index =
        let backend = RecordingGraphics()
        let geometry = { triangle with Indices = [|0L; 1L; index|] }
        Assert.That(Action(fun () -> call "Graphics.uploadMesh" [window backend; wrap geometry] |> ignore), Throws.Exception)
        Assert.That(backend.Meshes.Count, Is.Zero)

    [<Test>]
    member _.``invalid graphics values fail before submission``() =
        let backend = RecordingGraphics()
        let bad = { triangle with Vertices = [|{ triangle.Vertices[0] with Position = { zero with X = Double.NaN } }|] }
        Assert.That(Action(fun () -> call "Graphics.uploadMesh" [window backend; wrap bad] |> ignore), Throws.Exception)
        Assert.That(backend.Meshes.Count, Is.Zero)
        let camera : Camera = { Eye = zero; Target = zero; FieldOfView = 0.6 }
        Assert.That(Action(fun () -> call "Graphics.setCamera" [window backend; wrap camera] |> ignore), Throws.Exception)
        Assert.That(backend.Cameras, Is.Zero)

    [<Test>]
    member _.``mesh cannot be drawn by a different window backend``() =
        let first, second = RecordingGraphics(), RecordingGraphics()
        let mesh = call "Graphics.uploadMesh" [window first; wrap triangle]
        let material : Material = { Color = white; Shininess = 32.0 }
        Assert.That(Action(fun () -> call "Graphics.draw" [window second; mesh; wrap zero; wrap material] |> ignore), Throws.Exception)
        Assert.That(second.Rotations.Count, Is.Zero)

    [<Test>]
    member _.``script entry runs callbacks in the current interpreter``() =
        let source = """
let start (window: Window) =
    let title = window.Title
    fun seconds ->
        if title = "FScript FFI — Utah teapot" then
            Graphics.clear window { R = seconds; G = 0.0; B = 0.0 }
        else Graphics.clear window { R = 2.0; G = 0.0; B = 0.0 }
Graphics.runHeadless 3 start
"""
        let script = ScriptHost.loadSourceWithRegistration Graphics.registration source
        Assert.That(script.LastValue, Is.EqualTo VUnit)

    [<Test>]
    member _.``render callback failures propagate to the script caller``() =
        let source = """
Graphics.runHeadless 3 (fun window -> fun seconds -> Graphics.clear window { R = 2.0; G = 0.0; B = 0.0 })
"""
        Assert.That(Action(fun () -> ScriptHost.loadSourceWithRegistration Graphics.registration source |> ignore), Throws.Exception)

    [<TestCase(0L)>]
    [<TestCase(-1L)>]
    member _.``headless frame limits must be positive`` frames =
        Assert.That(Action(fun () -> call "Graphics.runHeadless" [VInt frames; VUnit] |> ignore), Throws.Exception)

    [<TestCase("--headless", 0)>]
    [<TestCase("--bad-option", 3)>]
    member _.``real CLI executes the entry script with provider loading`` (argument: string, expectedExit: int) =
        let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "../../.."))
#if DEBUG
        let configuration = "Debug"
#else
        let configuration = "Release"
#endif
        let cli = Path.Combine(root, "src/FScript/bin", configuration, "net10.0/fscript.dll")
        let start = Diagnostics.ProcessStartInfo("dotnet", RedirectStandardOutput = true, RedirectStandardError = true)
        for argument in [cli; "--no-config"; "--extern-assembly"; typeof<GraphicsProvider>.Assembly.Location;
                         Path.Combine(root, "samples/ffi/main.fss"); "--"; argument] do
            start.ArgumentList.Add argument
        use cliProcess = new Diagnostics.Process(StartInfo = start)
        cliProcess.Start() |> ignore
        let output = cliProcess.StandardOutput.ReadToEndAsync()
        let errors = cliProcess.StandardError.ReadToEndAsync()
        if not (cliProcess.WaitForExit 30000) then
            cliProcess.Kill(true)
            Assert.Fail "CLI did not terminate."
        Assert.That(cliProcess.ExitCode, Is.EqualTo expectedExit, errors.GetAwaiter().GetResult())
        if expectedExit = 0 then
            Assert.That(output.GetAwaiter().GetResult(), Does.Contain "1 mesh uploads; 3 frames")
        else Assert.That(errors.GetAwaiter().GetResult(), Does.Contain "frame limit")

    [<Test>]
    member _.``native executable compiles the real teapot with automatic FFI configuration and runs alone``() =
        let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "../../.."))
#if DEBUG
        let configuration = "Debug"
#else
        let configuration = "Release"
#endif
        let cli = Path.Combine(root, "src/FScript/bin", configuration, "net10.0/fscript.dll")
        let directory = Path.Combine(Path.GetTempPath(), "fscript-teapot-native-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(directory) |> ignore
        let run executable arguments timeout =
            let start = Diagnostics.ProcessStartInfo(executable, WorkingDirectory=directory, RedirectStandardOutput=true, RedirectStandardError=true)
            for argument in arguments do start.ArgumentList.Add(argument)
            start.Environment["DOTNET_ROOT"] <- Path.Combine(directory, "absent-runtime")
            use proc = Diagnostics.Process.Start(start) |> nonNull
            let output = proc.StandardOutput.ReadToEndAsync()
            let errors = proc.StandardError.ReadToEndAsync()
            if not (proc.WaitForExit(timeout: int)) then proc.Kill(true); Assert.Fail "Teapot command did not terminate."
            proc.ExitCode, output.Result, errors.Result
        try
            let source = Path.Combine(directory, "source")
            let output = Path.Combine(directory, "output")
            Directory.CreateDirectory(source) |> ignore
            for path in Directory.GetFiles(Path.Combine(root, "samples/ffi"), "*.fss") do
                File.Copy(path, Path.Combine(source, Path.GetFileName path |> nonNull))
            let provider = typeof<GraphicsProvider>.Assembly.Location.Replace('\\', '/')
            File.WriteAllText(Path.Combine(source, "fscript.toml"), $"[runtime]\nextern-assemblies = ['{provider}']")
            let code, stdout, error = run "dotnet" [cli; "compile"; Path.Combine(source, "main.fss"); "--target"; "dotnet"; "--aot"; "--output"; output] 300000
            Assert.That(code, Is.Zero, stdout + error)
            let name = if OperatingSystem.IsWindows() then "main.exe" else "main"
            Assert.That(Directory.GetFiles(output) |> Array.map Path.GetFileName, Is.EqualTo(box [|name|]))
            let executable = Path.Combine(directory, name)
            File.Move(Path.Combine(output, name), executable)
            Directory.Delete(source, true)
            Directory.Delete(output, true)
            let code, stdout, error = run executable ["--headless"; "5"] 60000
            Assert.That(code, Is.Zero, error)
            Assert.That(stdout, Does.Contain("1 mesh uploads; 5 frames"))
            let code, _, error = run executable ["--headless"; "0"] 60000
            Assert.That(code, Is.Not.Zero)
            Assert.That(error, Does.Contain("frame limit"))
        finally Directory.Delete(directory, true)

    [<Test>]
    member _.``renderer carries native GLFW resources without initializing the library``() =
        let resources = typeof<IGraphics>.Assembly.GetManifestResourceNames() |> Array.map (fun name -> name.Replace('\\', '/'))
        Assert.That(resources |> Array.exists (fun name -> name.Contains("osx-arm64/native/libglfw.3.dylib")), Is.True)
        Assert.That(resources |> Array.exists (fun name -> name.Contains("win-x64/native/glfw3.dll")), Is.True)
        Assert.That(resources |> Array.exists (fun name -> name.Contains("linux-x64/native/libglfw.so.3")), Is.True)
