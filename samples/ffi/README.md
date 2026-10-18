# FFI graphics: a teapot made in FScript

FScript builds a Utah teapot from 32 bicubic Bézier patches, computes surface normals and triangle indices, and supplies the camera, lighting, material, and animated rotation. A small .NET host uploads that geometry once and renders it with OpenGL 3.3 through Silk.NET.

## Run

From the repository root, build the provider library once with the .NET 10 SDK:

```sh
dotnet build samples/ffi/FfiSample.fsproj -c Release
```

Then use the installed FScript CLI (0.81.3 or later):

```sh
fscript --config samples/ffi/fscript.toml samples/ffi/main.fss
```

The provider is a library, so `dotnet build` is a one-time setup step; execution starts in `main.fss` through the `fscript` command above. Version 0.81.3 includes the macOS signing entitlement required to load GLFW.

Close the window or press Escape to exit. The window can be resized. Run on a desktop with an OpenGL 3.3 core-capable driver; Linux also needs a working display and GLFW's system windowing dependencies. macOS uses a forward-compatible core context. Window creation and graphics calls run on the launching thread.

For a bounded graphical check, render three frames and verify that the framebuffer contains teapot-colored pixels:

```sh
fscript --config samples/ffi/fscript.toml samples/ffi/main.fss -- --frames 3
```

For machines without a display, run the same script and typed FFI adapters against a recording backend:

```sh
fscript --config samples/ffi/fscript.toml samples/ffi/main.fss -- --headless
dotnet test samples/ffi/Tests/FfiSample.Tests.fsproj -c Release
```

Headless mode checks script execution and conversion; it does not exercise shaders or GPU rendering. Sample tests are included in `make test`, and `make smoke-tests` runs the headless sample. Native graphics libraries are loaded only when starting the graphical host.

## Follow the example

1. Read `scene.fss`: ordinary record literals create the camera, light, material, and rotation. `initialize` generates and uploads a mesh; `render` changes its rotation each frame.
2. Read `teapot.fss`: the control points and patch connectivity are script data. Cubic Bézier evaluation, derivatives, cross products, normalized normals, and indexed tessellation all run in FScript. Change `Teapot.generate 8` in the scene to change the resolution (clamped to 2–32 segments per patch edge). Re-run the host after editing; this sample does not hot reload.
3. Read `Graphics.fs`: attributes expose real F# records as script types. `HostTypes.describe` creates descriptors, and `HostFunctions.create` derives function schemes, arity, and conversions from typed handlers.
4. Read `main.fss`: the entry script calls `Graphics.run` with a setup callback. That callback uploads the mesh and returns a render closure retaining it. The provider pumps native events and invokes the closure through `ExternalCallContext.Apply` until the window closes.
5. Read `Renderer/`: the C# backend contains shaders, view/projection matrix construction, GPU buffers, and window/input handling. It contains no teapot model or scene choices.

The dataset's provenance and license are in [THIRD-PARTY-NOTICES.md](THIRD-PARTY-NOTICES.md). This sample keeps collapsed pole triangles in the index buffer (the rasterizer ignores zero-area triangles) and supplies defined pole normals. The renderer draws both sides of the open teapot surfaces.

## Script-visible types

| Type | Visible fields | Purpose | Script-constructible |
| --- | --- | --- | --- |
| `Vec3` | `X`, `Y`, `Z`: float | Position, normal, or Euler rotation in radians | Yes |
| `Color` | `R`, `G`, `B`: float | RGB channels in `[0, 1]` | Yes |
| `Vertex` | `Position`, `Normal`: Vec3 | One surface sample | Yes |
| `Geometry` | `Name`: string; `Vertices`: Vertex list; `Indices`: int list | CPU geometry with zero-based triangle indices | Yes |
| `Camera` | `Eye`, `Target`: Vec3; `FieldOfView`: float | Perspective view with vertical field of view in radians, Y up | Yes |
| `Light` | `Position`: Vec3; `Color`: Color; `Ambient`: float | Point lighting plus ambient strength in `[0, 1]` | Yes |
| `Material` | `Color`: Color; `Shininess`: float | Surface color and specular exponent in `[1, 256]` | Yes |
| `Window` | `Title`: string | Opaque host-owned rendering context | No |
| `Mesh` | `Name`: string; `VertexCount`, `TriangleCount`: int | Opaque uploaded geometry owned by its window | No |

`Window.Backend`, `Mesh.Backend`, and `Mesh.Resource` are hidden host state. Scripts can read visible fields, but cannot construct or update these two opaque types. Passing a complete mesh preserves its hidden state. A mesh from another window is rejected. The host frees all uploaded meshes and shaders on exit, including script/render failures; the sample exposes no individual disposal API.

## Script-visible functions

| Function | Script type | Purpose |
| --- | --- | --- |
| `Graphics.run` | `int -> (Window -> float -> unit) -> unit` | Open a window and run a script setup/render closure; zero frames means until close |
| `Graphics.runHeadless` | `int -> (Window -> float -> unit) -> unit` | Run a positive number of frames without a display |
| `Graphics.uploadMesh` | `Window -> Geometry -> Mesh` | Validate and upload positions, normals, and triangle indices once |
| `Graphics.clear` | `Window -> Color -> unit` | Clear color and depth buffers |
| `Graphics.setCamera` | `Window -> Camera -> unit` | Set the view and perspective; aspect ratio follows framebuffer size |
| `Graphics.setLight` | `Window -> Light -> unit` | Set the point light and ambient contribution |
| `Graphics.draw` | `Window -> Mesh -> Vec3 -> Material -> unit` | Draw a mesh with X/Y/Z Euler rotations |
| `Math.toFloat` | `int -> float` | Convert tessellation indices to floating-point parameters |
| `Math.sqrt` | `float -> float` | Normalize script-computed surface normals |
| `Math.sin` | `float -> float` | Animate the scene's slight X-axis tilt |

The `Math` functions are sample-provided externs, not additions to the language's standard library. Graphics handlers reject non-finite/out-of-range values, incomplete triangles, invalid indices, zero normals, and invalid cameras before submitting work to the backend. Meshes are kept until the window closes, so upload stable geometry during initialization rather than every frame.

## Descriptors behind the helpers

These remain ordinary `FScript.Language` data structures:

| Structure | Fields | Purpose in this sample |
| --- | --- | --- |
| `HostTypeDescriptor` | `Name`, `Fields`, `HostManaged` | `Vec3` has visible float fields and `HostManaged = false`; `Mesh` exposes metadata with `HostManaged = true`. No GPU handles appear in descriptors. |
| `ExternalFunction` | `Name`, `Scheme`, `Arity`, `Impl` | `Graphics.uploadMesh` gets scheme `Window -> Geometry -> Mesh`, arity two, and an adapter that decodes records and wraps the opaque result. |
| `ExtensionRegistration` | `Types`, `Functions` | Groups the nine type descriptors and ten function descriptors for the host, CLI, and editor. |

The generated upload scheme is `Forall([], TFun(TNamed "Window", TFun(TNamed "Geometry", TNamed "Mesh")))`. Registration is explicit and local; the helpers add no global functions or types.

## VS Code and CLI discovery

Build the sample in Release, then use **FScript: Select Configuration** to choose this directory's `fscript.toml`. Its existing extension provider protocol exposes `Graphics`, `Math`, and the named types for completion, hover, and diagnostics in both scripts. Provider discovery builds descriptors and adapters without opening a window. If multiple manifests exist, select this one explicitly. Provider execution requires a trusted workspace.

You can also load the environment through the ordinary CLI:

```sh
fscript --config samples/ffi/fscript.toml samples/ffi/main.fss
```

That command runs the entry script, which opens the window through the provider. In VS Code, open `main.fss`, select this manifest, and use **FScript: Run Current Script**. The run command saves the file and starts a fresh CLI process with the owning folder’s selected configuration; trust is required. REPL commands remain separate. The manifest points to the Release output relative to itself. After changing provider code, rebuild and reload the environment. On Windows, stopping the language server may be necessary before rebuilding a locked assembly.
