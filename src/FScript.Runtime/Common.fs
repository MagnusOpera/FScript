namespace FScript.Runtime

open System
open System.IO
open System.Runtime.InteropServices
open System.Text.Json
open System.Text.RegularExpressions
open FScript.Language

module internal HostCommon =
    let unknownSpan = Span.mk (Span.pos 0 0) (Span.pos 0 0)

    let evalError message =
        EvalException { Message = message; Span = unknownSpan }

    let some v = VOption (Some v)
    let none = VOption None

    let pathComparison =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
            StringComparison.OrdinalIgnoreCase
        else
            StringComparison.Ordinal

    let private isDirectorySeparator c =
        c = Path.DirectorySeparatorChar || c = Path.AltDirectorySeparatorChar

    let private trimTrailingDirectorySeparators (path: string) =
        if String.IsNullOrEmpty(path) then
            path
        else
            let root = Path.GetPathRoot(path)
            let minLength = if isNull root then 0 else root.Length
            let mutable endIndex = path.Length
            while endIndex > minLength && isDirectorySeparator path.[endIndex - 1] do
                endIndex <- endIndex - 1
            if endIndex = path.Length then path else path.Substring(0, endIndex)

    let normalizeFullPath (path: string) =
        Path.GetFullPath(path) |> trimTrailingDirectorySeparators

    let normalizeRoot (ctx: HostContext) =
        normalizeFullPath ctx.RootDirectory

    let isWithinRoot (root: string) (fullPath: string) =
        let normalizedRoot = trimTrailingDirectorySeparators root
        let normalizedFullPath = trimTrailingDirectorySeparators fullPath
        let rootPath = Path.GetPathRoot(normalizedRoot)
        let rootIsFilesystemRoot =
            not (isNull rootPath)
            && String.Equals(normalizedRoot, trimTrailingDirectorySeparators rootPath, pathComparison)

        if rootIsFilesystemRoot then
            let fullPathRoot = Path.GetPathRoot(normalizedFullPath)
            not (isNull fullPathRoot)
            && String.Equals(trimTrailingDirectorySeparators fullPathRoot, normalizedRoot, pathComparison)
        else
            String.Equals(normalizedFullPath, normalizedRoot, pathComparison)
            || normalizedFullPath.StartsWith(normalizedRoot + string Path.DirectorySeparatorChar, pathComparison)

    let private hasReparsePoint (path: string) =
        try
            let attrs = File.GetAttributes(path)
            (attrs &&& FileAttributes.ReparsePoint) = FileAttributes.ReparsePoint
        with
        | :? FileNotFoundException
        | :? DirectoryNotFoundException -> false
        | _ -> true

    let private pathContainsReparsePoint (root: string) (fullPath: string) =
        let rec loop current =
            if not (isWithinRoot root current) then
                false
            elif String.Equals(current, root, pathComparison) then
                false
            elif hasReparsePoint current then
                true
            else
                match Path.GetDirectoryName(current) with
                | null
                | "" -> false
                | parent ->
                    let normalizedParent = trimTrailingDirectorySeparators parent
                    if String.Equals(normalizedParent, current, pathComparison) then
                        false
                    else
                        loop normalizedParent

        loop (trimTrailingDirectorySeparators fullPath)

    let isSafePath (ctx: HostContext) (fullPath: string) =
        try
            let root = normalizeRoot ctx
            let full = normalizeFullPath fullPath
            isWithinRoot root full && not (pathContainsReparsePoint root full)
        with _ ->
            false

    let tryResolvePath (ctx: HostContext) (candidate: string) =
        try
            let root = normalizeRoot ctx
            let full =
                if Path.IsPathRooted(candidate) then normalizeFullPath candidate
                else normalizeFullPath (Path.Combine(root, candidate))
            if isWithinRoot root full && not (pathContainsReparsePoint root full) then
                Some full
            else None
        with _ -> None

    let toRelativeNormalizedPath (ctx: HostContext) (fullPath: string) =
        let root = normalizeRoot ctx
        let full = normalizeFullPath fullPath
        if isWithinRoot root full then
            Path.GetRelativePath(root, full).Replace("\\", "/")
        else
            full.Replace("\\", "/")

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
