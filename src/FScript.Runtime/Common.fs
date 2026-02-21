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

    let toRelativeNormalizedPath (ctx: HostContext) (fullPath: string) =
        let root = normalizeRoot ctx
        if isWithinRoot root fullPath then
            Path.GetRelativePath(root, fullPath).Replace("\\", "/")
        else
            fullPath.Replace("\\", "/")

    let globToRegex (glob: string) =
        let escaped = Regex.Escape(glob.Replace("\\", "/"))
        let pattern =
            escaped
                .Replace("\\*\\*/", "___DOUBLESTAR_SLASH___")
                .Replace("\\*\\*", "___DOUBLESTAR___")
                .Replace("\\*", "[^/]*")
                .Replace("___DOUBLESTAR_SLASH___", "(?:.*/)?")
                .Replace("___DOUBLESTAR___", ".*")
                .Replace("\\?", ".")
        "^" + pattern + "$"

    let private normalizeDeniedGlob (glob: string) =
        let value = glob.Trim().Replace("\\", "/")
        if String.IsNullOrWhiteSpace(value) then None else Some value

    let private globRegexes (ctx: HostContext) =
        ctx.DeniedPathGlobs
        |> List.choose normalizeDeniedGlob
        |> List.map (fun glob -> Regex(globToRegex glob, RegexOptions.Compiled))

    let private ancestorsWithSelf (relativePath: string) =
        let rec loop (current: string) (acc: string list) =
            if String.IsNullOrWhiteSpace(current) || current = "." then
                "." :: acc
            else
                let parent = Path.GetDirectoryName(current.Replace("/", string Path.DirectorySeparatorChar))
                let normalizedParent =
                    match parent with
                    | null
                    | "" -> "."
                    | value -> value.Replace("\\", "/")
                loop normalizedParent (current :: acc)
        loop relativePath []

    let isDeniedPath (ctx: HostContext) (fullPath: string) =
        let relativePath = toRelativeNormalizedPath ctx fullPath
        let candidates = ancestorsWithSelf relativePath
        let regexes = globRegexes ctx
        regexes
        |> List.exists (fun regex ->
            candidates |> List.exists regex.IsMatch)

    let jsonInt (el: JsonElement) =
        match el.TryGetInt64() with
        | true, v -> Some v
        | _ -> None
