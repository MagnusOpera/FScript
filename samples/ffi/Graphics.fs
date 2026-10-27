namespace FScript.Ffi.Sample

open System
open System.Numerics
open FScript.Extensibility
open FScript.Ffi.Rendering

// Explicit printing and array-backed geometry keep reflection-based F# formatting
// and collection metadata out of the native runtime. Script records retain their
// descriptor-defined equality and list semantics.
[<NoEquality; NoComparison; FScriptHostType("Vec3", ScriptConstructible = true)>]
type Vec3 =
    { [<FScriptVisible>] X: float
      [<FScriptVisible>] Y: float
      [<FScriptVisible>] Z: float }
    override _.ToString() = "Vec3"

[<NoEquality; NoComparison; FScriptHostType("Color", ScriptConstructible = true)>]
type Color =
    { [<FScriptVisible>] R: float
      [<FScriptVisible>] G: float
      [<FScriptVisible>] B: float }
    override _.ToString() = "Color"

[<NoEquality; NoComparison; FScriptHostType("Vertex", ScriptConstructible = true)>]
type Vertex =
    { [<FScriptVisible>] Position: Vec3
      [<FScriptVisible>] Normal: Vec3 }
    override _.ToString() = "Vertex"

[<NoEquality; NoComparison; FScriptHostType("Geometry", ScriptConstructible = true)>]
type Geometry =
    { [<FScriptVisible>] Name: string
      [<FScriptVisible>] Vertices: Vertex array
      [<FScriptVisible>] Indices: int64 array }
    override _.ToString() = "Geometry"

[<NoEquality; NoComparison; FScriptHostType("Camera", ScriptConstructible = true)>]
type Camera =
    { [<FScriptVisible>] Eye: Vec3
      [<FScriptVisible>] Target: Vec3
      [<FScriptVisible>] FieldOfView: float }
    override _.ToString() = "Camera"

[<NoEquality; NoComparison; FScriptHostType("Light", ScriptConstructible = true)>]
type Light =
    { [<FScriptVisible>] Position: Vec3
      [<FScriptVisible>] Color: Color
      [<FScriptVisible>] Ambient: float }
    override _.ToString() = "Light"

[<NoEquality; NoComparison; FScriptHostType("Material", ScriptConstructible = true)>]
type Material =
    { [<FScriptVisible>] Color: Color
      [<FScriptVisible>] Shininess: float }
    override _.ToString() = "Material"

[<NoEquality; NoComparison; FScriptHostType("Window")>]
type Window =
    { [<FScriptVisible>] Title: string
      Backend: IGraphics }
    override _.ToString() = "Window"

[<NoEquality; NoComparison; FScriptHostType("Mesh")>]
type Mesh =
    { [<FScriptVisible>] Name: string
      [<FScriptVisible>] VertexCount: int64
      [<FScriptVisible>] TriangleCount: int64
      Backend: IGraphics
      Resource: int }
    override _.ToString() = "Mesh"

module Graphics =
    let private scalar name value =
        if not (Double.IsFinite value) || abs value > float Single.MaxValue then
            invalidArg name "Expected a finite single-precision graphics value."
        float32 value

    let private vector name (value: Vec3) =
        Vector3(scalar name value.X, scalar name value.Y, scalar name value.Z)

    let private color (value: Color) =
        let channels = [|value.R; value.G; value.B|]
        if channels |> Array.exists (fun v -> not (Double.IsFinite v) || v < 0.0 || v > 1.0) then
            invalidArg "color" "Color channels must be between zero and one."
        Vector3(float32 value.R, float32 value.G, float32 value.B)

    let upload (window: Window) (geometry: Geometry) : Mesh =
        if geometry.Vertices.Length = 0 || geometry.Indices.Length = 0 || geometry.Indices.Length % 3 <> 0 then
            invalidArg "geometry" "Geometry requires vertices and complete index triangles."
        let vertexCount = geometry.Vertices.Length
        let vertices = GC.AllocateUninitializedArray<float32>(Checked.(*) vertexCount 6)
        for index = 0 to vertexCount - 1 do
            let vertex = geometry.Vertices[index]
            let p = vector "Position" vertex.Position
            let n = vector "Normal" vertex.Normal
            if n.LengthSquared() < 0.000001f then invalidArg "Normal" "Normals must be nonzero."
            let offset = index * 6
            vertices[offset] <- p.X
            vertices[offset + 1] <- p.Y
            vertices[offset + 2] <- p.Z
            vertices[offset + 3] <- n.X
            vertices[offset + 4] <- n.Y
            vertices[offset + 5] <- n.Z
        let indices = geometry.Indices |> Array.map (fun index ->
            if index < 0L || index >= int64 vertexCount then
                invalidArg "Indices" "A triangle index is outside the vertex list."
            uint32 index)
        let resource = window.Backend.Upload(vertices, indices)
        { Name = geometry.Name; VertexCount = int64 vertexCount
          TriangleCount = int64 (indices.Length / 3); Backend = window.Backend; Resource = resource }

    let clear (window: Window) (background: Color) = window.Backend.Clear(color background)

    let setCamera (window: Window) (camera: Camera) =
        let eye, target = vector "Eye" camera.Eye, vector "Target" camera.Target
        let direction = target - eye
        if direction.LengthSquared() < 0.000001f || Vector3.Cross(direction, Vector3.UnitY).LengthSquared() < 0.000001f then
            invalidArg "camera" "Camera direction must be nonzero and cannot be parallel to the vertical axis."
        if not (Double.IsFinite camera.FieldOfView) || camera.FieldOfView <= 0.0 || camera.FieldOfView >= Math.PI then
            invalidArg "FieldOfView" "Field of view must be in radians between zero and pi."
        window.Backend.Camera(eye, target, scalar "FieldOfView" camera.FieldOfView)

    let setLight (window: Window) (light: Light) =
        if not (Double.IsFinite light.Ambient) || light.Ambient < 0.0 || light.Ambient > 1.0 then
            invalidArg "Ambient" "Ambient light must be between zero and one."
        window.Backend.Light(vector "Position" light.Position, color light.Color, float32 light.Ambient)

    let draw (window: Window) (mesh: Mesh) (rotation: Vec3) (material: Material) =
        if not (Object.ReferenceEquals(window.Backend, mesh.Backend)) then
            invalidArg "mesh" "The mesh belongs to a different window."
        if not (Double.IsFinite material.Shininess) || material.Shininess < 1.0 || material.Shininess > 256.0 then
            invalidArg "Shininess" "Shininess must be between one and 256."
        window.Backend.Draw(mesh.Resource, vector "rotation" rotation, color material.Color, float32 material.Shininess)

    let private runFrames headless frames (initialize: IGraphics -> unit) frame =
        if frames < (if headless then 1L else 0L) || frames > int64 Int32.MaxValue then
            invalidArg "frames" "Use a nonnegative frame limit (zero runs until close); headless mode requires a positive limit."
        if headless then
            let backend = RecordingGraphics()
            initialize backend
            for index in 0 .. int frames - 1 do frame (float index / 60.0)
            Console.WriteLine("Headless: {0} mesh uploads; {1} frames", backend.Meshes.Count, backend.Rotations.Count)
        else
            use backend = new OpenGlGraphics()
            backend.Run((fun () -> initialize backend), frame, int frames)
            if frames > 0L then
                Console.WriteLine("OpenGL: {0} frames; {1} teapot-colored pixels", backend.FramesRendered, backend.ColoredPixels)
                if backend.ColoredPixels < 100 then invalidOp "The framebuffer did not contain the teapot."

    let runCompiled headless frames (setup: Func<Window, Action<float>>) =
        try
            // An array slot keeps mutation out of F# ref cells and their structural equality.
            let render = [| Action<float>(fun _ -> invalidOp "The scene has not been initialized.") |]
            let initialize backend =
                render[0] <- setup.Invoke { Title = "FScript FFI — Utah teapot"; Backend = backend }
            runFrames headless frames initialize (fun seconds -> render[0].Invoke seconds)
        with error -> raise (InvalidOperationException("Graphics window loop failed: " + error.Message, error))

    // Callbacks use the explicit descriptor API: typed adapters deliberately do not
    // marshal script functions. Apply keeps execution in the current interpreter.
    let private run headless : ExternalFunction =
        { Name = if headless then "Graphics.runHeadless" else "Graphics.run"
          Scheme = Forall([], TFun(TInt, TFun(TFun(TNamed "Window", TFun(TFloat, TUnit)), TUnit)))
          Arity = 2
          Impl = fun context arguments ->
              try
                  match arguments with
                  | [VInt frames; setup] when frames >= (if headless then 1L else 0L) && frames <= int64 Int32.MaxValue ->
                      let mutable render = None
                      let initialize (backend: IGraphics) =
                          let windowTypes = { HostInterop.empty with Types = [HostTypes.describe<Window>()] }
                          let window = HostTypes.wrap windowTypes { Title = "FScript FFI — Utah teapot"; Backend = backend }
                          render <- Some(context.Apply setup window)
                      let frame seconds =
                          context.CheckRuntime()
                          match render with
                          | Some callback ->
                              match context.Apply callback (VFloat seconds) with
                              | VUnit -> ()
                              | _ -> invalidOp "The render callback must return unit."
                          | None -> invalidOp "The scene has not been initialized."
                      runFrames headless frames initialize frame
                      VUnit
                  | _ -> invalidArg "frames" "Use a nonnegative frame limit (zero runs until close); headless mode requires a positive limit."
              with
              | :? ExtensionException -> reraise()
              | error -> raise (ExtensionException($"Graphics window loop failed: {error.Message}", error)) }


    let registration =
        let types =
            { HostInterop.empty with
                Types =
                    [ HostTypes.describe<Vec3>(); HostTypes.describe<Color>(); HostTypes.describe<Vertex>()
                      HostTypes.describe<Geometry>(); HostTypes.describe<Camera>(); HostTypes.describe<Light>()
                      HostTypes.describe<Material>(); HostTypes.describe<Window>(); HostTypes.describe<Mesh>() ] }
        { types with
            Functions =
                [ HostFunctions.create types "Graphics.uploadMesh" upload
                  HostFunctions.create types "Graphics.clear" clear
                  HostFunctions.create types "Graphics.setCamera" setCamera
                  HostFunctions.create types "Graphics.setLight" setLight
                  HostFunctions.create types "Graphics.draw" draw
                  HostFunctions.create types "Math.toFloat" (fun (value: int64) -> float value)
                  HostFunctions.create types "Math.sqrt" (fun (value: float) -> Math.Sqrt value)
                  HostFunctions.create types "Math.sin" (fun (value: float) -> Math.Sin value)
                  run false
                  run true ] }

type GraphicsProvider =
    // Discovery only builds descriptors and adapters. It never opens a window.
    [<FScriptExternProvider>]
    static member Provide() : ExtensionRegistration = Graphics.registration

    // Compiler companions bind typed calls directly and let AOT discard SDK discovery.
    static member Upload(window: Window, geometry: Geometry) = Graphics.upload window geometry
    static member Clear(window: Window, color: Color) = Graphics.clear window color
    static member SetCamera(window: Window, camera: Camera) = Graphics.setCamera window camera
    static member SetLight(window: Window, light: Light) = Graphics.setLight window light
    static member Draw(window: Window, mesh: Mesh, rotation: Vec3, material: Material) = Graphics.draw window mesh rotation material
    static member ToFloat(value: int64) = float value
    static member Sqrt(value: float) = Math.Sqrt value
    static member Sin(value: float) = Math.Sin value
    static member Run(frames: int64, setup: Func<Window, Action<float>>) = Graphics.runCompiled false frames setup
    static member RunHeadless(frames: int64, setup: Func<Window, Action<float>>) = Graphics.runCompiled true frames setup
    static member FScriptCompiledTypes() : Collections.Generic.IEnumerable<Collections.Generic.KeyValuePair<string, Type>> =
        [| Collections.Generic.KeyValuePair("Window", typeof<Window>)
           Collections.Generic.KeyValuePair("Mesh", typeof<Mesh>) |]
    static member FScriptCompiledFunctions() : Collections.Generic.IEnumerable<Collections.Generic.KeyValuePair<string, Reflection.MethodInfo>> =
        [| for name, methodName in
               [ "Graphics.uploadMesh", "Upload"; "Graphics.clear", "Clear"
                 "Graphics.setCamera", "SetCamera"; "Graphics.setLight", "SetLight"
                 "Graphics.draw", "Draw"; "Math.toFloat", "ToFloat"
                 "Math.sqrt", "Sqrt"; "Math.sin", "Sin"
                 "Graphics.run", "Run"; "Graphics.runHeadless", "RunHeadless" ] do
               yield Collections.Generic.KeyValuePair(name, typeof<GraphicsProvider>.GetMethod(methodName) |> nonNull) |]
