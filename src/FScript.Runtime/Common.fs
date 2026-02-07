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

    let tryResolvePath (ctx: HostContext) (candidate: string) =
        try
            let root = normalizeRoot ctx
            let full =
                if Path.IsPathRooted(candidate) then Path.GetFullPath(candidate)
                else Path.GetFullPath(Path.Combine(root, candidate))
            if full = root || full.StartsWith(root + string Path.DirectorySeparatorChar, StringComparison.Ordinal) then
                Some full
            else None
        with _ -> None

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
