namespace FScript.Ffi.Sample

open System
open System.Numerics
open FScript.Language
open FScript.Runtime
open FScript.Ffi.Rendering

[<FScriptHostType("Vec3", ScriptConstructible = true)>]
type Vec3 =
    { [<FScriptVisible>] X: float
      [<FScriptVisible>] Y: float
      [<FScriptVisible>] Z: float }

[<FScriptHostType("Color", ScriptConstructible = true)>]
type Color =
    { [<FScriptVisible>] R: float
      [<FScriptVisible>] G: float
      [<FScriptVisible>] B: float }

[<FScriptHostType("Vertex", ScriptConstructible = true)>]
type Vertex =
    { [<FScriptVisible>] Position: Vec3
      [<FScriptVisible>] Normal: Vec3 }

[<FScriptHostType("Geometry", ScriptConstructible = true)>]
type Geometry =
    { [<FScriptVisible>] Name: string
      [<FScriptVisible>] Vertices: Vertex list
      [<FScriptVisible>] Indices: int64 list }

[<FScriptHostType("Camera", ScriptConstructible = true)>]
type Camera =
    { [<FScriptVisible>] Eye: Vec3
      [<FScriptVisible>] Target: Vec3
      [<FScriptVisible>] FieldOfView: float }

[<FScriptHostType("Light", ScriptConstructible = true)>]
type Light =
    { [<FScriptVisible>] Position: Vec3
      [<FScriptVisible>] Color: Color
      [<FScriptVisible>] Ambient: float }

[<FScriptHostType("Material", ScriptConstructible = true)>]
type Material =
    { [<FScriptVisible>] Color: Color
      [<FScriptVisible>] Shininess: float }

[<FScriptHostType("Window")>]
type Window =
    { [<FScriptVisible>] Title: string
      Backend: IGraphics }

[<FScriptHostType("Mesh")>]
type Mesh =
    { [<FScriptVisible>] Name: string
      [<FScriptVisible>] VertexCount: int64
      [<FScriptVisible>] TriangleCount: int64
      Backend: IGraphics
      Resource: int }

module Graphics =
    let private scalar name value =
        if not (Double.IsFinite value) || abs value > float Single.MaxValue then
            invalidArg name "Expected a finite single-precision graphics value."
        float32 value

    let private vector name (value: Vec3) =
        Vector3(scalar name value.X, scalar name value.Y, scalar name value.Z)

    let private color (value: Color) =
        let channels = [value.R; value.G; value.B]
        if channels |> List.exists (fun v -> not (Double.IsFinite v) || v < 0.0 || v > 1.0) then
            invalidArg "color" "Color channels must be between zero and one."
        Vector3(float32 value.R, float32 value.G, float32 value.B)

    let upload (window: Window) (geometry: Geometry) : Mesh =
        if List.isEmpty geometry.Vertices || List.isEmpty geometry.Indices || geometry.Indices.Length % 3 <> 0 then
            invalidArg "geometry" "Geometry requires vertices and complete index triangles."
        let vertexCount = geometry.Vertices.Length
        let vertices =
            geometry.Vertices |> List.collect (fun vertex ->
                let p = vector "Position" vertex.Position
                let n = vector "Normal" vertex.Normal
                if n.LengthSquared() < 0.000001f then invalidArg "Normal" "Normals must be nonzero."
                [p.X; p.Y; p.Z; n.X; n.Y; n.Z]) |> List.toArray
        let indices = geometry.Indices |> List.map (fun index ->
            if index < 0L || index >= int64 vertexCount then
                invalidArg "Indices" "A triangle index is outside the vertex list."
            uint32 index) |> List.toArray
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
                  HostFunctions.create types "Math.sin" (fun (value: float) -> Math.Sin value) ] }

type GraphicsProvider =
    // Discovery only builds descriptors and adapters. It never opens a window.
    [<FScriptExternProvider>]
    static member Provide() : ExtensionRegistration = Graphics.registration
