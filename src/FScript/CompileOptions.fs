module CompileOptions

open System
open System.IO
open FScript.Runtime

type Settings =
    { Backend: string
      AheadOfTime: bool
      MinimizeSize: bool
      SingleFile: bool
      RuntimeIdentifier: string option
      OutputDirectory: string }

let resolve (arguments: string array) (scriptPath: string) (defaults: CompileConfiguration) =
    try
        let has name = Array.contains name arguments
        let option name =
            match Array.tryFindIndex ((=) name) arguments with
            | None -> None
            | Some index when index + 1 < arguments.Length && not (arguments[index+1].StartsWith("--", StringComparison.Ordinal)) && not (String.IsNullOrWhiteSpace arguments[index+1]) -> Some arguments[index+1]
            | _ -> invalidArg name $"{name} requires a value."
        let flag (name: string) fallback =
            let negative = "--no-" + name.Substring(2)
            if has name && has negative then invalidArg name $"{name} and {negative} cannot be used together."
            if has name then true elif has negative then false else fallback
        // Validate path-valued flags before provider discovery/loading.
        for name in ["--target"; "--output"; "--runtime"; "--config"; "--reference"; "--extern-assembly"] do option name |> ignore
        let backend = option "--target" |> Option.orElse defaults.Backend |> Option.defaultValue "dotnet" |> fun value -> value.ToLowerInvariant()
        let native = backend = "dotnet"
        let aot = flag "--aot" (native && not (has "--single-file") && Option.defaultValue false defaults.AheadOfTime)
        let single = flag "--single-file" (native && not (has "--aot") && Option.defaultValue false defaults.SingleFile)
        let size = flag "--size-optimized" (native && not (has "--no-aot" || has "--single-file") && Option.defaultValue false defaults.MinimizeSize)
        let runtime =
            option "--runtime"
            |> Option.orElse (if native && (aot || single) then defaults.RuntimeIdentifier else None)
            |> Option.orElse (if aot || single then Some Runtime.InteropServices.RuntimeInformation.RuntimeIdentifier else None)
        let output =
            option "--output" |> Option.map Path.GetFullPath
            |> Option.orElse defaults.OutputDirectory
            |> Option.defaultWith (fun () ->
                let root = Path.Combine(Path.GetDirectoryName(scriptPath) |> nonNull, "bin", Path.GetFileNameWithoutExtension(scriptPath) |> nonNull, backend)
                if native && (aot || single) then Path.Combine(root, runtime.Value, if aot then "aot" else "single-file") else root)
        Ok { Backend=backend; AheadOfTime=aot; SingleFile=single; MinimizeSize=size; RuntimeIdentifier=runtime; OutputDirectory=output }
    with :? ArgumentException as error -> Error error.Message
