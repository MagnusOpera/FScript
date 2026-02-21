namespace FScript.Runtime

open System
open System.IO
open System.Text.Json
open System.Text.RegularExpressions
open FScript.Language

module internal HostCommon =
    let unknownSpan = Span.mk (Span.pos 0 0) (Span.pos 0 0)

    let evalError message =
        EvalException { Message = message; Span = unknownSpan }

    let some v = VOption (Some v)
    let none = VOption None

    let normalizeRoot (ctx: HostContext) =
        Path.GetFullPath(ctx.RootDirectory)

    let isWithinRoot (root: string) (fullPath: string) =
        fullPath = root || fullPath.StartsWith(root + string Path.DirectorySeparatorChar, StringComparison.Ordinal)

    let tryResolvePath (ctx: HostContext) (candidate: string) =
        try
            let root = normalizeRoot ctx
            let full =
                if Path.IsPathRooted(candidate) then Path.GetFullPath(candidate)
                else Path.GetFullPath(Path.Combine(root, candidate))
            if isWithinRoot root full then
                Some full
            else None
        with _ -> None

    let private normalizeExcludedPaths (ctx: HostContext) =
        let root = normalizeRoot ctx
        ctx.ExcludedPaths
        |> List.choose (fun path ->
            try
                let full = Path.GetFullPath(path)
                if isWithinRoot root full then
                    Some full
                else
                    None
            with _ ->
                None)

    let private isExcludedDirectoryCandidate (fullExcludedPath: string) =
        Directory.Exists(fullExcludedPath)
        || fullExcludedPath.EndsWith(string Path.DirectorySeparatorChar, StringComparison.Ordinal)
        || fullExcludedPath.EndsWith(string Path.AltDirectorySeparatorChar, StringComparison.Ordinal)

    let isExcludedPath (ctx: HostContext) (fullPath: string) =
        normalizeExcludedPaths ctx
        |> List.exists (fun excludedPath ->
            if isExcludedDirectoryCandidate excludedPath then
                isWithinRoot excludedPath fullPath
            else
                String.Equals(excludedPath, fullPath, StringComparison.Ordinal))

    let globToRegex (glob: string) =
        let escaped = Regex.Escape(glob.Replace("\\", "/"))
        let pattern =
            escaped
                .Replace("\\*\\*", "___DOUBLESTAR___")
                .Replace("\\*", "[^/]*")
                .Replace("___DOUBLESTAR___", ".*")
                .Replace("\\?", ".")
        "^" + pattern + "$"

    let jsonInt (el: JsonElement) =
        match el.TryGetInt64() with
        | true, v -> Some v
        | _ -> None
