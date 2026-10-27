---
id: ffi-graphics
title: FFI Graphics — A Rotating Teapot
slug: /examples/ffi-graphics
---

This example uses FScript to generate a Utah teapot and animate it in an OpenGL window. The script supplies vertices, normals, triangle indices, camera, lighting, and material. A .NET host exposes typed functions to upload and draw the geometry.

Start here after [imports and exports](./guided-examples.md#5-imports--exports-multi-file). You will see how script-created data records and opaque host resources work together through the same FFI bridge.

## Run the sample

Run the sample directly with the installed FScript CLI (0.81.5 or later), from the repository root:

```sh
fscript samples/ffi/main.fss
```

The CLI finds `samples/ffi/fscript.toml` beside `main.fss` automatically. With CLI 0.81.4 or earlier, pass `--config samples/ffi/fscript.toml` explicitly.

Execution starts in `main.fss`, which loads the scene and starts rendering through the graphics provider.

### One-time provider setup

Check out the [FScript repository](https://github.com/MagnusOpera/FScript) and build the provider library with the .NET 10 SDK before the first run:

```sh
dotnet build samples/ffi/FfiSample.fsproj -c Release
```

This compiles the FFI library used by `fscript`. Rebuild it when changing provider code; script edits only require running the `fscript` command again. Version 0.81.3 includes the macOS signing entitlement required to load GLFW.

### Window and headless execution

A copper-colored teapot rotates until you close the window or press Escape. The window can be resized. You need a desktop display and an OpenGL 3.3 core-capable driver. Linux also needs GLFW's system windowing dependencies. macOS uses a forward-compatible core context.

For a machine without a display, run the same scripts and FFI adapters against a recording backend:

```sh
fscript samples/ffi/main.fss -- --headless
```

The default scene generates 2,592 vertices and 4,096 triangles and renders three recorded frames in headless mode. This exercises script execution and conversions; it does not test OpenGL. For a bounded graphical check, use `-- --frames 3` to render three frames, check for teapot-colored pixels in the framebuffer, and exit.

### Compile to Native AOT

With the provider built, compile the same script without explicit assembly flags:

```sh
dotnet run --project src/FScript -- compile samples/ffi/main.fss --target dotnet --aot --output out/ffi
out/ffi/main --headless
out/ffi/main --frames 3
```

Add `--size-optimized` to trade culture support and framework diagnostics for a smaller executable. It enables invariant globalization, replaces framework exception messages with resource keys, and disables runtime stack traces and EventSource. Ordinary `--aot` keeps those facilities. Both modes optimize native code for size and embed only the selected platform's GLFW library; the provider DLL remains portable for interpreted execution.


On macOS ARM64 with .NET SDK 10.0.403, the executable shrank from 4,378,616 bytes to 2,181,464 bytes with ordinary AOT, or 1,882,776 bytes with `--size-optimized`. Both profiles passed headless and real OpenGL checks. Sizes depend on SDK and platform; see [Native AOT executable size](../compiler/compile-to-dotnet.md#native-aot-executable-size) for the investigation and runtime tradeoffs.

The compiler reports its phases and output path. The result is one executable, `main.exe` on Windows. Publishing requires the .NET 10 SDK and the platform's native compiler; running the result requires no .NET installation. The provider supplies static CLR companions and typed setup/render delegates. GLFW binaries are embedded in the renderer and extracted only for graphical startup. Desktop graphics and OpenGL drivers are still required for a window.

## Find your way through the files

All sample code lives under [`samples/ffi`](https://github.com/MagnusOpera/FScript/tree/main/samples/ffi).

| File | Purpose |
| --- | --- |
| `scene.fss` | Chooses scene data, uploads the generated mesh, and animates it. |
| `teapot.fss` | Holds the Bézier patch data and generates vertices, normals, and triangle indices. |
| `Graphics.fs` | Declares the script-visible types and typed FFI functions. |
| `main.fss` | Starts the window loop and returns a render closure that retains the mesh. |
| `Renderer/` | Implements the native window, GPU buffers, shaders, and rendering. |
| `fscript.toml` | Makes the provider available to CLI and editor analysis. |
| `Tests/` | Tests geometry generation, scene calls, conversions, and resource boundaries without a display. |

## 1. Describe the scene in FScript

Here is the complete `scene.fss`:

```fsharp
import "teapot.fss" as Teapot

// These are script-created instances of types supplied by the graphics module.
let background = { R = 0.055; G = 0.075; B = 0.11 }
let camera =
    { Eye = { X = 6.0; Y = 3.0; Z = 7.0 }
      Target = { X = 0.0; Y = 0.0; Z = 0.0 }
      FieldOfView = 0.65 }
let light =
    { Position = { X = 4.0; Y = 6.0; Z = 5.0 }
      Color = { R = 1.0; G = 0.94; B = 0.85 }
      Ambient = 0.22 }
let copper =
    { Color = { R = 0.78; G = 0.30; B = 0.12 }
      Shininess = 56.0 }

export let initialize (window: Window) =
    // Generate and upload only once. The returned Mesh retains hidden GPU state.
    let geometry = Teapot.generate 8
    Graphics.uploadMesh window geometry

export let render (window: Window) (mesh: Mesh) (seconds: float) =
    Graphics.clear window background
    Graphics.setCamera window camera
    Graphics.setLight window light
    let rotation =
        { X = 0.12 * Math.sin seconds
          Y = seconds * 0.45
          Z = 0.0 }
    Graphics.draw window mesh rotation copper
```

`Color`, `Camera`, `Light`, `Material`, and `Vec3` are types supplied by the host module. They allow script construction, so their values use ordinary record literals. The camera looks from `Eye` toward `Target` with Y up; `FieldOfView` and rotation components are in radians.

`Window` and `Mesh` are opaque types. The host supplies the window, and `Graphics.uploadMesh` creates the mesh. The script can read metadata such as `window.Title` and `mesh.VertexCount`, but cannot construct these types, update them, or access their hidden graphics state.

The entry script calls `Scene.initialize` once and retains the returned mesh in a closure. `Graphics.run` pumps window events and invokes that closure each frame through `ExternalCallContext.Apply`, passing elapsed seconds. Only small scene records cross the boundary during animation; geometry is generated and uploaded once.

The executable entry point is `main.fss`:

```fsharp
import "scene.fss" as Scene

// The script owns startup and retains the mesh in a render closure.
let start (window: Window) =
    let mesh = Scene.initialize window
    fun seconds -> Scene.render window mesh seconds

let parseFrames text = Int.tryParse text |> Option.defaultValue (0 - 1)

match Env.Arguments with
| [] -> Graphics.run 0 start
| ["--headless"] -> Graphics.runHeadless 3 start
| ["--headless"; count] -> Graphics.runHeadless (parseFrames count) start
| ["--frames"; count] -> Graphics.run (parseFrames count) start
| _ -> Graphics.run (0 - 1) start
```

`Graphics.run 0 start` runs until the window closes; a positive limit stops after that many frames. `Graphics.runHeadless` requires a positive limit and uses a recording backend.

## 2. Generate the model in FScript

The teapot consists of 32 bicubic Bézier patches. Each patch references a four-by-four grid of control points. `teapot.fss` contains both the control points and the patch connectivity.

For each patch, the script:

1. Samples parameters `u` and `v` between zero and one.
2. Evaluates cubic Bézier curves in both directions to find a position.
3. Computes surface tangents and their cross product to find a normal.
4. Connects neighboring samples into indexed triangles.

For example, cubic curve evaluation is ordinary script arithmetic:

```fsharp
let curve points t =
    let (a, b, c, d) = points
    let s = 1.0 - t
    let first = add (scale (s * s * s) a) (scale (3.0 * s * s * t) b)
    let second = add (scale (3.0 * s * t * t) c) (scale (t * t * t) d)
    add first second
```

The result is a `Geometry` record containing a name, a list of `Vertex` records, and an integer list of zero-based triangle indices. Each vertex has a position and a normal. The host receives the finished geometry; it contains no teapot primitive or model-generation code.

The sample supplies `Math.toFloat`, `Math.sqrt`, and `Math.sin` as typed extern functions for parameter conversion, normal normalization, and animation. These functions belong to this provider, rather than the standard library. The dataset's attribution and license are included in `THIRD-PARTY-NOTICES.md`.

## 3. Expose data and resources as host types

`Graphics.fs` uses attributes on real F# types. Here are two representative definitions:

```fsharp
[<FScriptHostType("Vec3", ScriptConstructible = true)>]
type Vec3 =
    { [<FScriptVisible>] X: float
      [<FScriptVisible>] Y: float
      [<FScriptVisible>] Z: float }

[<FScriptHostType("Window")>]
type Window =
    { [<FScriptVisible>] Title: string
      Backend: IGraphics }
```

Every constructor field of a script-constructible record must be visible. Opaque types can retain unmarked fields: `Window.Backend` stays in the host and never becomes a script field.

| Types | Script construction | Role |
| --- | --- | --- |
| `Vec3`, `Color` | Allowed | Small numeric values. |
| `Vertex`, `Geometry` | Allowed | Script-generated mesh data. |
| `Camera`, `Light`, `Material` | Allowed | Script-controlled scene settings. |
| `Window`, `Mesh` | Forbidden | Host-created resources with visible metadata and hidden state. |

`HostTypes.describe` turns each attributed type into a plain `HostTypeDescriptor`. For `Vec3`, the generated data is:

```fsharp
{ Name = "Vec3"
  Fields = Map.ofList [ ("X", TFloat); ("Y", TFloat); ("Z", TFloat) ]
  HostManaged = false }
```

A `Window` descriptor instead has `HostManaged = true` and only a visible `Title: string` field. Neither descriptor contains a native resource or conversion function. See [Host-Provided Types](../embedding/host-managed-types.md#the-type-descriptor-behind-the-helper) for the descriptor fields and conversion rules.

## 4. Turn typed handlers into function descriptors

The upload handler has the concrete F# type `Window -> Geometry -> Mesh`. It validates geometry, uploads it through `window.Backend`, and returns a mesh retaining that backend and its resource handle. The complete implementation is in `Graphics.fs`.

Register it with a registration that already contains the referenced type descriptors:

```fsharp
HostFunctions.create types "Graphics.uploadMesh" upload
```

The helper builds an `ExternalFunction` record:

| Field | Generated value or behavior |
| --- | --- |
| `Name` | `Graphics.uploadMesh` |
| `Scheme` | `Forall([], TFun(TNamed "Window", TFun(TNamed "Geometry", TNamed "Mesh")))` |
| `Arity` | `2` |
| `Impl` | Validates and decodes script arguments, calls `upload`, and wraps the returned opaque mesh. |

An `ExtensionRegistration` groups the nine type descriptors in `Types` and the ten function descriptors in `Functions`. The assembly provider returns this registration for editor and CLI discovery. The two window-loop functions use explicit `ExternalFunction` records because they accept script callbacks; geometry, rendering, and math functions continue to use `HostFunctions.create`.

The renderer-facing functions are:

| Function | Script type | Purpose |
| --- | --- | --- |
| `Graphics.uploadMesh` | `Window -> Geometry -> Mesh` | Upload geometry after validating vertices, normals, and indices. |
| `Graphics.clear` | `Window -> Color -> unit` | Clear color and depth buffers. |
| `Graphics.setCamera` | `Window -> Camera -> unit` | Set the view and perspective projection. |
| `Graphics.setLight` | `Window -> Light -> unit` | Set point lighting and ambient strength. |
| `Graphics.draw` | `Window -> Mesh -> Vec3 -> Material -> unit` | Draw the mesh with an Euler rotation and material. |

The host rejects invalid graphics inputs and attempts to draw a mesh through another window's backend. The window loop stays on its launching thread, and GPU resources are released before its context is destroyed, including when a script call fails. For the general adapter API, see [Register Extern Functions](../embedding/register-externs.md).

## 5. Enable editor discovery

Build the sample in Release, then run **FScript: Select Configuration** in VS Code and choose `samples/ffi/fscript.toml`:

```toml
[runtime]
extern-assemblies = ["bin/Release/net10.0/FScript.Ffi.Sample.dll"]
```

The path is relative to the manifest. In a trusted workspace, the editor loads the provider's descriptors and schemes for completion, hover, and diagnostics. Provider discovery creates no window or GPU resources. If several manifests exist in the workspace, select this one explicitly.

The CLI can load the same environment:

```sh
fscript samples/ffi/main.fss
```

This runs `main.fss` and opens the window. In VS Code, open `main.fss` and use **FScript: Run Current Script** after selecting the manifest. The command saves the file and runs a fresh CLI process with the selected configuration. It requires workspace trust; REPL commands remain separate. See [Environment Configuration](../reference/environment-configuration.md) for selection, trust, and reload behavior.

## Try changing the scene

- Change the material color or shininess in `scene.fss`.
- Move the camera or light, or change the rotation speed.
- Change `Teapot.generate 8` to `Teapot.generate 16` for a denser mesh. Resolution is clamped to 2–32 segments per patch edge.
- Adjust the Bézier control points in `teapot.fss` to alter the model itself.

Re-run the sample after editing; it loads scripts at startup. Keep mesh uploads in initialization so the render loop reuses GPU buffers.

To run the sample's automated checks:

```sh
dotnet test samples/ffi/Tests/FfiSample.Tests.fsproj -c Release
```

## Extension SDK dependency

The provider's only FScript package reference is `MagnusOpera.FScript.Extensibility` 1.0.0. Open `FScript.Extensibility` for attributes, descriptors, `HostTypes`, and `HostFunctions`. This independently versioned SDK contains no interpreter implementation. The renderer remains a separate project; only the integration test harness references Runtime. Interpreted window callbacks use opaque SDK handles through `ExternalCallContext`; compiled callbacks use `Func<Window, Action<double>>`. Both paths share validation and the frame loop. The renderer calls GLFW/OpenGL through unmanaged function pointers so Native AOT does not need reflection or dynamic delegate generation.
