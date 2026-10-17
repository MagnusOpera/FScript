namespace FScript.Ffi.Sample

open System
open System.IO
open FScript.Language
open FScript.Runtime
open FScript.Ffi.Rendering

module Scene =
    let load () =
        ScriptHost.loadFileWithRegistration Graphics.registration (Path.Combine(AppContext.BaseDirectory, "scene.fss"))

    let initialize loaded (backend: IGraphics) =
        let window = HostTypes.wrap Graphics.registration { Title = "FScript FFI — Utah teapot"; Backend = backend }
        let mesh = ScriptHost.invoke loaded "initialize" [window]
        window, mesh

    let render loaded window mesh seconds =
        ScriptHost.invoke loaded "render" [window; mesh; VFloat seconds] |> ignore

module Program =
    [<EntryPoint>]
    let main args =
        try
            let headless, frames =
                match Array.toList args with
                | [] -> false, 0
                | ["--headless"] -> true, 3
                | ["--frames"; count] ->
                    match Int32.TryParse count with
                    | true, value when value > 0 -> false, value
                    | _ -> invalidArg "count" "Frame count must be a positive integer."
                | _ -> invalidArg "args" "Usage: dotnet run --project samples/ffi -c Release -- [--headless | --frames N]"
            let loaded = Scene.load ()
            if headless then
                let backend = RecordingGraphics()
                let window, mesh = Scene.initialize loaded backend
                for frame in 0 .. frames - 1 do Scene.render loaded window mesh (float frame / 60.0)
                let info = HostTypes.unwrap<Mesh> mesh
                printfn "Headless: %s, %d vertices, %d triangles; %d frames" info.Name info.VertexCount info.TriangleCount backend.Rotations.Count
            else
                use backend = new OpenGlGraphics()
                let mutable state = None
                backend.Run(
                    (fun () -> state <- Some(Scene.initialize loaded backend)),
                    (fun seconds ->
                        match state with
                        | Some(window, mesh) -> Scene.render loaded window mesh seconds
                        | None -> invalidOp "Scene has not been initialized."), frames)
                if frames > 0 then
                    printfn "OpenGL: %d frames; %d teapot-colored pixels" backend.FramesRendered backend.ColoredPixels
                    if backend.ColoredPixels < 100 then failwith "The framebuffer did not contain the teapot."
            0
        with error ->
            eprintfn "%s" error.Message
            1
