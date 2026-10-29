namespace FScript.Language

open System

/// Additional pure library contracts shared by inference, runtimes, and editor metadata.
module SupportLibrary =
    let private fn arguments result = List.foldBack (fun argument result -> TFun(argument,result)) arguments result
    let private a,b,k,s = TVar 0,TVar 1,TVar 2,TVar 3
    let versionFields = ["Major";"Minor";"Patch";"PreRelease";"Build"]
    let versionType = TRecord(versionFields |> List.map (fun name -> name,TString) |> Map.ofList)
    let signatures : (string * Scheme * string list) list =
        [
          "String.compare", Forall([], fn [TString; TString] TInt), ["left"; "right"]
          "Int.abs", Forall([], TFun(TInt, TInt)), ["value"]
          "Int.sign", Forall([], TFun(TInt, TInt)), ["value"]
          "Int.min", Forall([], fn [TInt; TInt] TInt), ["left";"right"]
          "Int.max", Forall([], fn [TInt; TInt] TInt), ["left";"right"]
          "Int.clamp", Forall([], fn [TInt; TInt; TInt] TInt), ["minimum";"maximum";"value"]
          "Float.abs", Forall([], TFun(TFloat, TFloat)), ["value"]
          "Float.sign", Forall([], TFun(TFloat, TInt)), ["value"]
          "Float.min", Forall([], fn [TFloat; TFloat] TFloat), ["left";"right"]
          "Float.max", Forall([], fn [TFloat; TFloat] TFloat), ["left";"right"]
          "Float.clamp", Forall([], fn [TFloat; TFloat; TFloat] TFloat), ["minimum";"maximum";"value"]
          "Float.floor", Forall([], TFun(TFloat, TFloat)), ["value"]
          "Float.ceil", Forall([], TFun(TFloat, TFloat)), ["value"]
          "Float.round", Forall([], TFun(TFloat, TFloat)), ["value"]
          "Float.truncate", Forall([], TFun(TFloat, TFloat)), ["value"]
          "Float.sqrt", Forall([], TFun(TFloat, TFloat)), ["value"]
          "Float.log", Forall([], TFun(TFloat, TFloat)), ["value"]
          "Float.log10", Forall([], TFun(TFloat, TFloat)), ["value"]
          "Float.exp", Forall([], TFun(TFloat, TFloat)), ["value"]
          "Float.sin", Forall([], TFun(TFloat, TFloat)), ["value"]
          "Float.cos", Forall([], TFun(TFloat, TFloat)), ["value"]
          "Float.tan", Forall([], TFun(TFloat, TFloat)), ["value"]
          "Float.asin", Forall([], TFun(TFloat, TFloat)), ["value"]
          "Float.acos", Forall([], TFun(TFloat, TFloat)), ["value"]
          "Float.atan", Forall([], TFun(TFloat, TFloat)), ["value"]
          "Float.pow", Forall([], fn [TFloat; TFloat] TFloat), ["left";"right"]
          "Float.atan2", Forall([], fn [TFloat; TFloat] TFloat), ["left";"right"]
          "Float.isNaN", Forall([], TFun(TFloat, TBool)), ["value"]
          "Float.isInfinity", Forall([], TFun(TFloat, TBool)), ["value"]
          "Float.isFinite", Forall([], TFun(TFloat, TBool)), ["value"]
          "Float.pi", Forall([], TFloat), []
          "Int.toFloat", Forall([], TFun(TInt, TFloat)), ["value"]
          "Float.tryToInt", Forall([], TFun(TFloat, TOption TInt)), ["value"]
          "String.length", Forall([], TFun(TString, TInt)), ["source"]
          "String.isEmpty", Forall([], TFun(TString, TBool)), ["source"]
          "String.isWhiteSpace", Forall([], TFun(TString, TBool)), ["source"]
          "String.trim", Forall([], TFun(TString, TString)), ["source"]
          "String.trimStart", Forall([], TFun(TString, TString)), ["source"]
          "String.trimEnd", Forall([], TFun(TString, TString)), ["source"]
          "String.splitLines", Forall([], TFun(TString, TList TString)), ["source"]
          "String.startsWith", Forall([], fn [TString; TString] TBool), ["value";"source"]
          "String.contains", Forall([], fn [TString; TString] TBool), ["value";"source"]
          "String.padLeft", Forall([], fn [TInt; TString] TString), ["count";"source"]
          "String.padRight", Forall([], fn [TInt; TString] TString), ["count";"source"]
          "String.repeat", Forall([], fn [TInt; TString] TString), ["count";"source"]
          "List.isEmpty", Forall([0], fn [TList a] TBool), ["values"]
          "List.forall", Forall([0], fn [fn [a] TBool; TList a] TBool), ["predicate";"values"]
          "List.mapi", Forall([0;1], fn [fn [TInt; a] b; TList a] (TList b)), ["mapper";"values"]
          "List.indexed", Forall([0], fn [TList a] (TList(TTuple [TInt; a]))), ["values"]
          "List.init", Forall([0], fn [TInt; fn [TInt] a] (TList a)), ["count";"initializer"]
          "List.take", Forall([0], fn [TInt; TList a] (TList a)), ["count";"values"]
          "List.skip", Forall([0], fn [TInt; TList a] (TList a)), ["count";"values"]
          "List.takeWhile", Forall([0], fn [fn [a] TBool; TList a] (TList a)), ["predicate";"values"]
          "List.skipWhile", Forall([0], fn [fn [a] TBool; TList a] (TList a)), ["predicate";"values"]
          "List.partition", Forall([0], fn [fn [a] TBool; TList a] (TTuple [TList a; TList a])), ["predicate";"values"]
          "List.zip", Forall([0;1], fn [TList a; TList b] (TList(TTuple [a;b]))), ["left";"right"]
          "List.unzip", Forall([0;1], fn [TList(TTuple [a;b])] (TTuple [TList a;TList b])), ["pairs"]
          "List.chunkBySize", Forall([0], fn [TInt; TList a] (TList(TList a))), ["size";"values"]
          "List.scan", Forall([0;3], fn [fn [s;a] s; s; TList a] (TList s)), ["folder";"state";"values"]
          "List.sortWith", Forall([0], fn [fn [a;a] TInt; TList a] (TList a)), ["comparer";"values"]
          "List.sortBy", Forall([0;2], fn [fn [a] k; TList a] (TList a)), ["projection";"values"]
          "List.distinctBy", Forall([0;2], fn [fn [a] k; TList a] (TList a)), ["projection";"values"]
          "List.groupBy", Forall([0;2], fn [fn [a] k; TList a] (TList(TTuple [k;TList a]))), ["projection";"values"]
          "List.countBy", Forall([0;2], fn [fn [a] k; TList a] (TList(TTuple [k;TInt]))), ["projection";"values"]
          "Map.keys", Forall([0;2], fn [TMap(k,a)] (TList k)), ["values"]
          "Map.values", Forall([0;2], fn [TMap(k,a)] (TList a)), ["values"]
          "Map.toList", Forall([0;2], fn [TMap(k,a)] (TList(TTuple [k;a]))), ["values"]
          "Map.exists", Forall([0;2], fn [fn [k;a] TBool; TMap(k,a)] TBool), ["predicate";"values"]
          "Map.forall", Forall([0;2], fn [fn [k;a] TBool; TMap(k,a)] TBool), ["predicate";"values"]
          "Map.update", Forall([0;2], fn [k; fn [TOption a] (TOption a); TMap(k,a)] (TMap(k,a))), ["key";"updater";"values"]
          "Map.mergeWith", Forall([0;2], fn [fn [k;a;a] a; TMap(k,a); TMap(k,a)] (TMap(k,a))), ["resolver";"left";"right"]
          "Option.bind", Forall([0;1], fn [fn [a] (TOption b); TOption a] (TOption b)), ["binder";"value"]
          "Option.flatten", Forall([0], fn [TOption(TOption a)] (TOption a)), ["value"]
          "Option.filter", Forall([0], fn [fn [a] TBool; TOption a] (TOption a)), ["predicate";"value"]
          "Option.orElse", Forall([0], fn [TOption a;TOption a] (TOption a)), ["fallback";"value"]
          "Option.orElseWith", Forall([0], fn [fn [TUnit] (TOption a);TOption a] (TOption a)), ["fallback";"value"]
          "Option.map2", Forall([0;1;3], fn [fn [a;b] s;TOption a;TOption b] (TOption s)), ["mapper";"left";"right"]
          "Option.toList", Forall([0], fn [TOption a] (TList a)), ["value"]
          "Path.normalize", Forall([], fn [TString] TString), ["path"]
          "Path.isAbsolute", Forall([], fn [TString] TBool), ["path"]
          "Path.combine", Forall([], fn [TString;TString] TString), ["left";"right"]
          "Path.fileName", Forall([], fn [TString] TString), ["path"]
          "Path.extension", Forall([], fn [TString] (TOption TString)), ["path"]
          "Path.fileNameWithoutExtension", Forall([], fn [TString] TString), ["path"]
          "Path.parent", Forall([], fn [TString] (TOption TString)), ["path"]
          "Path.relative", Forall([], fn [TString;TString] (TOption TString)), ["basePath";"targetPath"]
          "Glob.isMatch", Forall([], fn [TString;TString] TBool), ["pattern";"path"]
          "Glob.filter", Forall([], fn [TString;TList TString] (TList TString)), ["pattern";"paths"]
          "SemVer.tryParse", Forall([], fn [TString] (TOption versionType)), ["version"]
          "SemVer.format", Forall([], fn [versionType] TString), ["version"]
          "SemVer.compare", Forall([], fn [TString;TString] (TOption TInt)), ["left";"right"]
        ]

    let names = signatures |> List.map (fun (name,_,_) -> name) |> Set.ofList
    let totalFunctions = set ["String.compare";"Int.min";"Int.max";"Float.abs";"Float.min";"Float.max";"Float.floor";"Float.ceil";"Float.round";"Float.truncate";"Float.sqrt";"Float.log";"Float.log10";"Float.exp";"Float.sin";"Float.cos";"Float.tan";"Float.asin";"Float.acos";"Float.atan";"Float.pow";"Float.atan2";"Float.isNaN";"Float.isInfinity";"Float.isFinite";"Float.pi";"Int.toFloat";"Float.tryToInt";"String.length";"String.isEmpty";"String.isWhiteSpace";"String.trim";"String.trimStart";"String.trimEnd";"String.splitLines";"String.startsWith";"String.contains"]

    let normalizePath (path: string) =
        let absolute = path.StartsWith("/", StringComparison.Ordinal)
        let parts = path.Split('/') |> Array.fold (fun acc part ->
            match part,acc with
            | "",_ | ".",_ -> acc
            | "..",head::tail when head <> ".." -> tail
            | "..",_ when absolute -> acc
            | _ -> part::acc) [] |> List.rev
        let text = String.concat "/" parts
        if absolute then "/" + text elif text = "" then "." else text

    let fileName (path: string) =
        let path = path.TrimEnd('/')
        path.Substring(path.LastIndexOf('/') + 1)

    let relativePath basis target =
        let basis,target = normalizePath basis,normalizePath target
        if basis.StartsWith("/", StringComparison.Ordinal) <> target.StartsWith("/", StringComparison.Ordinal) then None
        else
            let parts (path: string) = path.Split('/') |> Array.filter (fun part -> part <> "" && part <> ".") |> Array.toList
            let rec removeCommon left right =
                match left,right with
                | a::xs,b::ys when a=b -> removeCommon xs ys
                | _ -> left,right
            let left,right = removeCommon (parts basis) (parts target)
            // Unresolved parents in the base do not identify a lexical ancestor.
            if List.contains ".." left then None
            else Some(normalizePath(String.concat "/" ((left |> List.map (fun _ -> "..")) @ right)))

    let private segmentMatch (pattern: string) (text: string) =
        let mutable previous = Array.create (text.Length + 1) false
        previous[0] <- true
        for tokenIndex in 0 .. pattern.Length - 1 do
            let token = pattern[tokenIndex]
            let current = Array.create (text.Length + 1) false
            if token = '*' then current[0] <- previous[0]
            for index in 1 .. text.Length do
                current[index] <-
                    if token = '*' then previous[index] || current[index-1]
                    else previous[index-1] && (token = '?' || token = text[index-1])
            previous <- current
        previous[text.Length]

    let globMatch (pattern: string) (path: string) =
        let tokens,parts = pattern.Split('/'),path.Split('/')
        if tokens |> Array.exists (fun token -> token.Contains("**") && token <> "**") then
            invalidArg "pattern" "Glob ** must occupy an entire path segment."
        let mutable previous = Array.create (parts.Length + 1) false
        previous[0] <- true
        for token in tokens do
            let current = Array.create (parts.Length + 1) false
            if token = "**" then current[0] <- previous[0]
            for index in 1 .. parts.Length do
                current[index] <-
                    if token = "**" then previous[index] || (current[index-1] && parts[index-1] <> "")
                    else previous[index-1] && segmentMatch token parts[index-1]
            previous <- current
        previous[parts.Length]

    let private digits (text: string) = text.Length > 0 && text |> Seq.forall (fun c -> c >= '0' && c <= '9')
    let private numericIdentifier text = digits text && (text = "0" || not (text.StartsWith("0", StringComparison.Ordinal)))
    let private validIdentifiers allowLeadingZeros (text: string) =
        text.Split('.') |> Array.forall (fun part ->
            part.Length > 0
            && part |> Seq.forall (fun c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '-')
            && (allowLeadingZeros || not (digits part) || numericIdentifier part))

    /// Decimal components remain strings so SemVer precedence has no integer size limit.
    let parseVersion (text: string) =
        let splitOnce separator (value: string) =
            match value.IndexOf(separator: char) with
            | -1 -> value,""
            | index -> value.Substring(0,index),value.Substring(index+1)
        let withoutBuild,build = splitOnce '+' text
        let core,prerelease = splitOnce '-' withoutBuild
        let numbers = core.Split('.')
        if numbers.Length <> 3 || not (Array.forall numericIdentifier numbers)
           || (text.Contains("+") && not (validIdentifiers true build))
           || (withoutBuild.Contains("-") && not (validIdentifiers false prerelease)) then None
        else Some [|numbers[0];numbers[1];numbers[2];prerelease;build|]

    let formatVersion (parts: string array) =
        let text = String.concat "." parts[0..2] + (if parts[3] = "" then "" else "-" + parts[3]) + (if parts[4] = "" then "" else "+" + parts[4])
        if parseVersion text |> Option.isNone then invalidArg "version" "Invalid semantic version components."
        text

    let compareVersions (left: string array) (right: string array) =
        let ordinal a b = Math.Sign(String.CompareOrdinal(a,b))
        let number (a: string) (b: string) = if a.Length = b.Length then ordinal a b else compare a.Length b.Length
        let rec compareParts comparePart left right =
            match left,right with
            | [],[] -> 0 | [],_ -> -1 | _,[] -> 1
            | a::xs,b::ys -> let order = comparePart a b in if order=0 then compareParts comparePart xs ys else order
        let core = compareParts number (Array.toList left[0..2]) (Array.toList right[0..2])
        if core <> 0 then core
        elif left[3] = right[3] then 0
        elif left[3] = "" then 1
        elif right[3] = "" then -1
        else
            compareParts (fun a b ->
                match digits a,digits b with
                | true,true -> number a b | true,false -> -1 | false,true -> 1 | _ -> ordinal a b)
                (left[3].Split('.') |> Array.toList) (right[3].Split('.') |> Array.toList)

    let private versionValue (parts: string array) =
        List.zip versionFields (parts |> Array.toList |> List.map VString) |> Map.ofList |> VRecord

    // Unicode White_Space, pinned explicitly for identical .NET/JavaScript behavior.
    let private whitespace c =
        (c >= '\u0009' && c <= '\u000D') || c='\u0020' || c='\u0085' || c='\u00A0'
        || c='\u1680' || (c >= '\u2000' && c <= '\u200A') || c='\u2028' || c='\u2029'
        || c='\u202F' || c='\u205F' || c='\u3000'

    let private trimText left right (text: string) =
        let mutable start = 0
        let mutable finish = text.Length
        if left then
            while start < finish && whitespace text[start] do start <- start + 1
        if right then
            while finish > start && whitespace text[finish-1] do finish <- finish - 1
        text.Substring(start,finish-start)

    let invokeScalar name arguments =
        match name,arguments with
        | "String.compare", [VString left;VString right] -> VInt(int64 (System.Math.Sign(System.String.CompareOrdinal(left,right))))
        | "Int.abs", [ VInt x ] ->
            if x = System.Int64.MinValue then invalidArg "value" "Integer overflow."
            VInt(abs x)
        | "Int.sign", [ VInt x ] -> VInt(int64 (sign x))
        | "Int.min", [ VInt x; VInt y ] -> VInt(min x y)
        | "Int.max", [ VInt x; VInt y ] -> VInt(max x y)
        | "Int.clamp", [ VInt low; VInt high; VInt x ] when low <= high -> VInt(max low (min high x))
        | "Float.abs", [ VFloat x ] -> VFloat(abs x)
        | "Float.sign", [ VFloat x ] when not (System.Double.IsNaN x) -> VInt(int64 (sign x))
        | "Float.min", [ VFloat x; VFloat y ] -> VFloat(System.Math.Min(x,y))
        | "Float.max", [ VFloat x; VFloat y ] -> VFloat(System.Math.Max(x,y))
        | "Float.clamp", [ VFloat low; VFloat high; VFloat x ] when low <= high -> VFloat(System.Math.Max(low,System.Math.Min(high,x)))
        | "Float.floor", [ VFloat x ] -> VFloat(floor x)
        | "Float.ceil", [ VFloat x ] -> VFloat(ceil x)
        | "Float.round", [ VFloat x ] -> VFloat(System.Math.Round x)
        | "Float.truncate", [ VFloat x ] -> VFloat(System.Math.Truncate x)
        | "Float.sqrt", [ VFloat x ] -> VFloat(sqrt x)
        | "Float.log", [ VFloat x ] -> VFloat(log x)
        | "Float.log10", [ VFloat x ] -> VFloat(log10 x)
        | "Float.exp", [ VFloat x ] -> VFloat(exp x)
        | "Float.sin", [ VFloat x ] -> VFloat(sin x)
        | "Float.cos", [ VFloat x ] -> VFloat(cos x)
        | "Float.tan", [ VFloat x ] -> VFloat(tan x)
        | "Float.asin", [ VFloat x ] -> VFloat(asin x)
        | "Float.acos", [ VFloat x ] -> VFloat(acos x)
        | "Float.atan", [ VFloat x ] -> VFloat(atan x)
        | "Float.pow", [ VFloat x; VFloat y ] -> VFloat(x ** y)
        | "Float.atan2", [ VFloat x; VFloat y ] -> VFloat(atan2 x y)
        | "Float.isNaN", [ VFloat x ] -> VBool(System.Double.IsNaN x)
        | "Float.isInfinity", [ VFloat x ] -> VBool(System.Double.IsInfinity x)
        | "Float.isFinite", [ VFloat x ] -> VBool(not (System.Double.IsNaN x || System.Double.IsInfinity x))
        | "Float.pi", [] -> VFloat System.Math.PI
        | "Int.toFloat", [ VInt x ] -> VFloat(float x)
        | "Float.tryToInt", [ VFloat x ] -> VOption(if System.Double.IsNaN x || x < -9223372036854775808.0 || x >= 9223372036854775808.0 then None else Some(VInt(int64 x)))
        | "String.length", [ VString x ] -> VInt(int64 x.Length)
        | "String.isEmpty", [ VString x ] -> VBool(x.Length = 0)
        | "String.isWhiteSpace", [ VString x ] -> VBool(x |> Seq.forall whitespace)
        | "String.trim", [ VString x ] -> VString(trimText true true x)
        | "String.trimStart", [ VString x ] -> VString(trimText true false x)
        | "String.trimEnd", [ VString x ] -> VString(trimText false true x)
        | "String.splitLines", [ VString x ] -> x.Replace("\r\n", "\n").Replace("\r", "\n").Split('\n') |> Array.toList |> List.map VString |> VList
        | "String.startsWith", [ VString needle; VString x ] -> VBool(x.StartsWith(needle, System.StringComparison.Ordinal))
        | "String.contains", [ VString needle; VString x ] -> VBool(x.IndexOf(needle, System.StringComparison.Ordinal) >= 0)
        | "String.padLeft", [ VInt n; VString x ] when n >= 0L && n <= int64 System.Int32.MaxValue -> VString(x.PadLeft(int n))
        | "String.padRight", [ VInt n; VString x ] when n >= 0L && n <= int64 System.Int32.MaxValue -> VString(x.PadRight(int n))
        | "String.repeat", [ VInt n; VString x ] when n >= 0L && n <= int64 System.Int32.MaxValue -> VString(String.replicate (int n) x)
        | "Path.normalize", [VString path] -> VString(normalizePath path)
        | "Path.isAbsolute", [VString path] -> VBool(path.StartsWith("/", System.StringComparison.Ordinal))
        | "Path.combine", [VString left;VString right] -> VString(normalizePath (if right.StartsWith("/", System.StringComparison.Ordinal) then right elif left = "" then right else left + "/" + right))
        | "Path.fileName", [VString path] -> VString(fileName path)
        | "Path.extension", [VString path] -> let name = fileName path in let dot = name.LastIndexOf('.') in VOption(if dot <= 0 || dot = name.Length - 1 then None else Some(VString(name.Substring dot)))
        | "Path.fileNameWithoutExtension", [VString path] -> let name = fileName path in let dot = name.LastIndexOf('.') in VString(if dot <= 0 then name else name.Substring(0,dot))
        | "Path.parent", [VString path] -> let normalized = normalizePath path in let slash = normalized.LastIndexOf('/') in VOption(if normalized = "/" || normalized = "." then None elif slash < 0 then Some(VString ".") elif slash = 0 then Some(VString "/") else Some(VString(normalized.Substring(0,slash))))
        | "Path.relative", [VString basis;VString target] -> relativePath basis target |> Option.map VString |> VOption
        | "Glob.isMatch", [VString pattern;VString path] -> VBool(globMatch pattern path)
        | "Glob.filter", [VString pattern;VList paths] -> globMatch pattern "" |> ignore; paths |> List.filter (function VString path -> globMatch pattern path | _ -> false) |> VList
        | "SemVer.tryParse", [VString text] -> parseVersion text |> Option.map versionValue |> VOption
        | "SemVer.format", [VRecord fields] -> VString(formatVersion [| for name in versionFields -> match fields[name] with VString x -> x | _ -> invalidArg "version" "Invalid semantic version" |])
        | "SemVer.compare", [VString left;VString right] -> match parseVersion left,parseVersion right with Some a,Some b -> VOption(Some(VInt(int64 (compareVersions a b)))) | _ -> VOption None
        | _ -> invalidArg "arguments" $"Invalid arguments to {name}."

    /// Pure .NET runtime capabilities, registered independently from core language builtins.
    let runtimeSignatures : (string * Scheme * string list) list =
        [ "Encoding.utf8Encode", Forall([],fn [TString] (TOption(TList TInt))), ["text"]
          "Encoding.utf8Decode", Forall([],fn [TList TInt] (TOption TString)), ["bytes"]
          "Encoding.base64Encode", Forall([],fn [TList TInt] (TOption TString)), ["bytes"]
          "Encoding.base64Decode", Forall([],fn [TString] (TOption(TList TInt))), ["text"]
          "Encoding.hexEncode", Forall([],fn [TList TInt] (TOption TString)), ["bytes"]
          "Encoding.hexDecode", Forall([],fn [TString] (TOption(TList TInt))), ["text"]
          "Encoding.uriEncode", Forall([],fn [TString] (TOption TString)), ["text"]
          "Encoding.uriDecode", Forall([],fn [TString] (TOption TString)), ["text"]
          "Hash.sha256", Forall([],fn [TString] (TOption TString)), ["text"]
          "Hash.sha256Bytes", Forall([],fn [TList TInt] (TOption TString)), ["bytes"] ]
