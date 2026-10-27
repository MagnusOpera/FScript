# Native AOT size investigation

The FFI teapot originally published a 4,378,616-byte executable on macOS ARM64. Most removable bytes came from embedded GLFW libraries for other operating systems and architectures. FSharp.Core was already trimmed. Comparing its ordinary managed DLL size with the final Native AOT executable does not show how much survived compilation.

## Measurements

Measured on macOS ARM64 with .NET SDK 10.0.403 and runtime/compiler pack 10.0.14. These are executable file lengths, excluding optional dSYM files. Every measured executable uses the same generated teapot PE and supports graphical rendering and headless mode.

| Configuration | Bytes | Reduction from original |
| --- | ---: | ---: |
| Original publishing | 4,378,616 | 0% |
| Target GLFW resources and native size optimization | 2,347,512 | 46.4% |
| Also remove unnecessary F# runtime dependencies, ordinary `--aot` | 2,181,464 | 50.2% |
| Also use `--size-optimized` | 1,882,776 | 57.0% |

The original NativeAOT map reported 2,252,803 bytes of embedded resource data. GLFW's eight libraries alone total 2,217,604 bytes. The target macOS ARM64 library is 236,928 bytes. The remaining resource data includes provider compiler metadata and licenses. Method maps initially attributed about 14 KB of native instructions to FSharp.Core, nowhere near its 2.4 MB managed assembly size. After the provider changes, ordinary AOT attributed only 360 bytes of native method instructions to FSharp.Core. These method totals exclude metadata, static data, alignment, native runtime code, and linker overhead; they are not whole-assembly size estimates.

## Changes

The publishing SDK project sets `OptimizationPreference=Size` for Native AOT. Native AOT already implies trimming, so adding more `PublishTrimmed` or `TrimMode` flags does not fix resources that the application deliberately enumerates and loads by name.

The publisher supplies explicit boolean `FScript.NativeAssets.<rid>` switches for all supported portable runtime identifiers. The renderer embeds its own `ILLink.Substitutions.xml` rules to remove resources whose switch is false. An unspecified switch removes nothing. This keeps ordinary provider builds portable, supports explicit target selection, and avoids teaching the compiler to delete arbitrary third-party resources. The GLFW license remains embedded. Resource names use forward slashes on every build host so linker rules also match Windows-built providers.

The provider fills one vertex buffer directly, avoiding temporary arrays per vertex and `Array.collect`. It uses a one-element callback array rather than a captured mutable F# variable, which compiles into an F# ref cell with structural equality methods. Those methods retained generic equality and hashing code that the graphics callback did not need.

One attempted replacement used `Array.zeroCreate`. Dependency graph analysis showed its checked negative-length error path reaching FSharp.Core's resource manager, then reflective resource construction, `Activator`, and the default reflection binder. That attempt increased the binary to 2,364,664 bytes even with the aggressive switches. The final code uses `GC.AllocateUninitializedArray` with checked length arithmetic and explicitly writes every vertex component before exposing the buffer. A regression test checks the complete packed buffer.

The optional `--size-optimized` profile sets `InvariantGlobalization=true`, `UseSystemResourceKeys=true`, `StackTraceSupport=false`, and `EventSourceSupport=false`. It keeps provider and FScript error text but changes framework error text and diagnostics. It can also break providers that require named cultures. Ordinary AOT keeps those capabilities.

## Analysis and validation

For size attribution, publish the generated assembly through an isolated SDK project whose post-`CoreCompile` target copies the generated PE over its placeholder assembly, matching `DotNetPackaging`. Set `IlcGenerateMapFile=true`, `IlcGenerateMstatFile=true`, and, when investigating a dependency, `IlcGenerateDgmlFile=true`. The SDK writes maps, statistics, and dependency graphs under `obj/Release/net10.0/<rid>/native/`. Compare `ResourceData` and native `MethodCode` nodes in the XML map, then follow dependency graph edges to find the root of unexpected code. FScript's regular publisher deletes this temporary project after compilation.

The sample integration tests publish both profiles, inspect the executable for the exact target GLFW payload and absence of every other GLFW payload, move the executable away from scripts and provider DLLs, run headless frames without a .NET installation, and check invalid frame limits. macOS ARM64 size budgets leave SDK headroom while catching the original all-platform resource regression. A packed-buffer test checks all uploaded position and normal components. Real OpenGL validation uses `--frames 3` and checks the renderer's count of teapot-colored framebuffer pixels.

## Limits and rejected options

The minimized result is about 1.80 MiB. It still exceeds a 1,474,560-byte floppy by 408,216 bytes. The executable includes a garbage collector, exception handling, native interop, console and filesystem support, the renderer, GLFW, and the script. A minimal `Console.WriteLine` application built on the same machine with the same minimized runtime switches is 970,328 bytes. A floppy-sized teapot has not been demonstrated here; that comparison leaves about 496 KB for the graphics application and GLFW if it is to fit.

`IlcScanReflection=false` produced no further reduction in the tested minimized sample. It is not enabled. Reflection-free and custom-runtime examples on the web give up runtime capabilities that arbitrary FFI providers may need. Disabling reflection globally would require a separate contract and broader compatibility coverage. Old blog properties such as `IlcOptimizationPreference` and `IlcDisableReflection` are not a substitute for checking the current SDK's actual compiler arguments. Unsupported properties can be silently ignored.

`EnableCompressionInSingleFile` compresses managed single-file bundles, not Native AOT executables. Executable packers and a replacement runtime are outside this implementation. Platform-specific binary surgery would need independent loader, signing, and portability validation.

## References

- [Microsoft's Native AOT optimization guidance](https://learn.microsoft.com/en-us/dotnet/core/deploying/native-aot/optimizing) documents the supported native size preference.
- [Framework trimming options](https://learn.microsoft.com/en-us/dotnet/core/deploying/trimming/trimming-options) describe the culture and diagnostic feature switches and their tradeoffs.
- [Runtime linker data formats](https://github.com/dotnet/runtime/blob/main/docs/tools/illink/data-formats.md) document conditional embedded-resource removal through substitutions.
- [The runtime team's Hello World size investigation](https://github.com/dotnet/runtime/issues/80165) traces unnecessary reflection dependencies and measures globalization/resource-key switches.
- [TinyWordle's experiments](https://github.com/nikouu/TinyWordle) demonstrate measurement-driven changes, interop costs, and SDK-dependent results. Its Windows-specific and reflection-free choices do not establish a macOS graphics size floor.
- [Reflection-scanning size discussion](https://github.com/dotnet/runtime/issues/113919) describes a switch worth measuring, rather than assuming it helps every app.
- [Runtime discussion of Native AOT compression](https://github.com/dotnet/runtime/discussions/126970) explains why single-file compression does not compress Native AOT output.
