namespace FScript.Runtime

open System
open System.IO
open System.Reflection
open System.Runtime.Loader

/// A process-lifetime context for one resolved graph. Only SDK contract types cross into the host.
type internal ExtensionLoadContext(assemblyPaths: string list) =
    inherit AssemblyLoadContext()
    let paths = Collections.Generic.Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
    do
        for path in assemblyPaths do
            let identity = AssemblyName.GetAssemblyName(path)
            let name = nonNull identity.Name
            let sdk = typeof<FScript.Extensibility.ExtensionRegistration>.Assembly.GetName()
            if name = "FScript.Extensibility" && identity.Version <> sdk.Version then
                raise (FileLoadException($"NuGet environment requires FScript.Extensibility {identity.Version}, but this host provides {sdk.Version}."))
            match paths.TryGetValue name with
            | true, existing when File.ReadAllBytes(existing) <> File.ReadAllBytes(path) ->
                raise (FileLoadException($"NuGet packages contain conflicting runtime assemblies named '{name}'."))
            | true, _ -> ()
            | _ -> paths.Add(name, path)
    let shared =
        [ typeof<FScript.Extensibility.ExtensionRegistration>.Assembly
          typeof<Microsoft.FSharp.Core.Unit>.Assembly ]
        |> List.map (fun assembly -> nonNull (assembly.GetName().Name), assembly) |> Map.ofList

    override this.Load(name: AssemblyName) =
        let simpleName = nonNull name.Name
        match shared.TryFind simpleName with
        | Some assembly ->
            let available = assembly.GetName()
            // The SDK ABI version is deliberately stable within a compatible major.
            if simpleName = "FScript.Extensibility" && name.Version <> available.Version then
                raise (FileLoadException($"Extension requires FScript.Extensibility {name.Version}, but this host provides {available.Version}. Rebuild against a compatible SDK."))
            if simpleName = "FSharp.Core" && not (isNull name.Version) && not (isNull available.Version) && name.Version > available.Version then
                raise (FileLoadException($"Extension requires FSharp.Core {name.Version}, but this host provides {available.Version}."))
            assembly
        | None ->
            match paths.TryGetValue simpleName with
            | true, path -> this.LoadFromAssemblyPath path
            | _ -> null

    override this.LoadUnmanagedDll(name: string) =
        let directory = assemblyPaths |> List.tryHead |> Option.map Path.GetDirectoryName
        match directory with
        | None -> IntPtr.Zero
        | Some directory ->
            let path =
                [ name; name + ".dll"; "lib" + name + ".so"; "lib" + name + ".dylib" ]
                |> List.map (fun candidate -> Path.Combine(directory, candidate))
                |> List.tryFind File.Exists
            match path with
            | Some path -> this.LoadUnmanagedDllFromPath path
            | None -> IntPtr.Zero
