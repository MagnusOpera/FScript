namespace FScript.Language

open System
open System.Collections.Generic
open System.IO

module IncludeResolver =
    let private normalizeDirectoryPath (path: string) =
        let full = Path.GetFullPath(path)
        if full.EndsWith(Path.DirectorySeparatorChar.ToString(), StringComparison.Ordinal) then
            full
        else
            full + string Path.DirectorySeparatorChar

    let private ensureFssPath (path: string) (span: Span) =
        if not (path.EndsWith(".fss", StringComparison.OrdinalIgnoreCase)) then
            raise (ParseException { Message = "Only '.fss' files can be used with 'import'"; Span = span })

    let private ensureWithinRoot (rootDirectoryWithSeparator: string) (path: string) (span: Span) =
        let fullPath = Path.GetFullPath(path)
        let fullRoot = rootDirectoryWithSeparator.TrimEnd(Path.DirectorySeparatorChar)
        let isRootItself = String.Equals(fullPath, fullRoot, StringComparison.OrdinalIgnoreCase)
        let isUnderRoot = fullPath.StartsWith(rootDirectoryWithSeparator, StringComparison.OrdinalIgnoreCase)
        if not (isRootItself || isUnderRoot) then
            raise (ParseException { Message = $"Imported file '{fullPath}' is outside of sandbox root"; Span = span })
        fullPath

    let private resolveImportPath (currentFile: string) (importPath: string) (rootDirectoryWithSeparator: string) (span: Span) =
        if String.IsNullOrWhiteSpace(importPath) then
            raise (ParseException { Message = "Import path cannot be empty"; Span = span })

        ensureFssPath importPath span

        let currentDirectory = Path.GetDirectoryName(currentFile)
        let candidate =
            if Path.IsPathRooted(importPath) then importPath
            elif String.IsNullOrEmpty(currentDirectory) then importPath
            else Path.Combine(currentDirectory, importPath)

        ensureWithinRoot rootDirectoryWithSeparator candidate span

    let private isValidAliasName (name: string) =
        let startsValid c = Char.IsLetter(c) || c = '_'
        let partValid c = Char.IsLetterOrDigit(c) || c = '_'
        not (String.IsNullOrWhiteSpace(name))
        && startsValid name[0]
        && (name |> Seq.forall partValid)

    let private tryGetSourceModulePrefix (sourceName: string) =
        let stem =
            try
                Path.GetFileNameWithoutExtension(sourceName)
            with
            | _ -> sourceName

        if isValidAliasName stem then Some stem else None

    let private collectPatternBindings (pattern: Pattern) : Set<string> =
        let rec loop acc pat =
            match pat with
            | PWildcard _
            | PLiteral _
            | PNil _
            | PNone _ -> acc
            | PVar (name, _) -> acc |> Set.add name
            | PCons (head, tail, _) -> loop (loop acc head) tail
            | PTuple (patterns, _) -> patterns |> List.fold loop acc
            | PRecord (fields, _) -> fields |> List.fold (fun s (_, p) -> loop s p) acc
            | PMap (clauses, tailPattern, _) ->
                let withClauses =
                    clauses
                    |> List.fold (fun s (keyPattern, valuePattern) -> loop (loop s keyPattern) valuePattern) acc
                match tailPattern with
                | Some tail -> loop withClauses tail
                | None -> withClauses
            | PSome (inner, _) -> loop acc inner
            | PUnionCase (_, _, payload, _) ->
                match payload with
                | Some inner -> loop acc inner
                | None -> acc
            | PTypeRef _ -> acc
        loop Set.empty pattern

    let private parseDotted (name: string) = name.Split('.') |> Array.toList
    let private joinDotted (parts: string list) = String.concat "." parts

    let private rewriteNameWithAliases (aliases: Map<string, string>) (name: string) =
        match parseDotted name with
        | first :: rest when aliases.ContainsKey(first) && not rest.IsEmpty ->
            let prefix = aliases[first]
            prefix + "." + joinDotted rest
        | _ -> name

    let private rewriteNameWithSelfPrefix (selfPrefix: string option) (selfNames: Set<string>) (boundNames: Set<string>) (name: string) =
        match selfPrefix with
        | Some prefix when selfNames.Contains(name) && not (boundNames.Contains(name)) ->
            $"{prefix}.{name}"
        | _ -> name

    let private rewriteTypeName (aliases: Map<string, string>) (selfPrefix: string option) (selfTypeNames: Set<string>) (name: string) =
        let aliasRewritten = rewriteNameWithAliases aliases name
        if aliasRewritten <> name then
            aliasRewritten
        else
            match selfPrefix with
            | Some prefix when selfTypeNames.Contains(name) -> $"{prefix}.{name}"
            | _ -> name

    let private rewriteTypeQualifier (aliases: Map<string, string>) (selfPrefix: string option) (selfTypeNames: Set<string>) (qualifier: string option) =
        match qualifier with
        | None -> None
        | Some q ->
            let rewritten = rewriteTypeName aliases selfPrefix selfTypeNames q
            Some rewritten

    let rec private rewriteTypeRef (aliases: Map<string, string>) (selfPrefix: string option) (selfTypeNames: Set<string>) (tref: TypeRef) : TypeRef =
        match tref with
        | TRName name ->
            TRName (rewriteTypeName aliases selfPrefix selfTypeNames name)
        | TRTuple ts ->
            TRTuple (ts |> List.map (rewriteTypeRef aliases selfPrefix selfTypeNames))
        | TRFun (a, b) ->
            TRFun(rewriteTypeRef aliases selfPrefix selfTypeNames a, rewriteTypeRef aliases selfPrefix selfTypeNames b)
        | TRPostfix (inner, suffix) ->
            TRPostfix(rewriteTypeRef aliases selfPrefix selfTypeNames inner, suffix)
        | TRRecord fields ->
            TRRecord (fields |> List.map (fun (name, t) -> name, rewriteTypeRef aliases selfPrefix selfTypeNames t))
        | TRStructuralRecord fields ->
            TRStructuralRecord (fields |> List.map (fun (name, t) -> name, rewriteTypeRef aliases selfPrefix selfTypeNames t))

    let rec private rewritePattern (aliases: Map<string, string>) (selfPrefix: string option) (selfTypeNames: Set<string>) (pat: Pattern) : Pattern =
        match pat with
        | PWildcard _
        | PLiteral _
        | PNil _
        | PNone _
        | PVar _ -> pat
        | PCons (head, tail, span) ->
            PCons(rewritePattern aliases selfPrefix selfTypeNames head, rewritePattern aliases selfPrefix selfTypeNames tail, span)
        | PTuple (patterns, span) ->
            PTuple(patterns |> List.map (rewritePattern aliases selfPrefix selfTypeNames), span)
        | PRecord (fields, span) ->
            PRecord(fields |> List.map (fun (name, p) -> name, rewritePattern aliases selfPrefix selfTypeNames p), span)
        | PMap (clauses, tailPattern, span) ->
            PMap(
                clauses
                |> List.map (fun (k, v) -> rewritePattern aliases selfPrefix selfTypeNames k, rewritePattern aliases selfPrefix selfTypeNames v),
                tailPattern |> Option.map (rewritePattern aliases selfPrefix selfTypeNames),
                span)
        | PSome (inner, span) ->
            PSome(rewritePattern aliases selfPrefix selfTypeNames inner, span)
        | PUnionCase (qualifier, caseName, payload, span) ->
            PUnionCase(
                rewriteTypeQualifier aliases selfPrefix selfTypeNames qualifier,
                caseName,
                payload |> Option.map (rewritePattern aliases selfPrefix selfTypeNames),
                span)
        | PTypeRef (tref, span) ->
            PTypeRef(rewriteTypeRef aliases selfPrefix selfTypeNames tref, span)

    let private tryFlattenExprPath (expr: Expr) : string list option =
        let rec loop acc current =
            match current with
            | EVar (name, _) ->
                let parts = parseDotted name
                Some (parts @ acc)
            | EFieldGet (target, fieldName, _) ->
                loop (fieldName :: acc) target
            | _ -> None
        loop [] expr

    let private rewriteModuleScopedStatements (selfPrefix: string option) (aliases: Map<string, string>) (statements: Stmt list) : Stmt list =
        let topLevelValueNames =
            statements
            |> List.collect (function
                | SLet(name, _, _, _, _, _) -> [ name ]
                | SLetRecGroup(bindings, _, _) -> bindings |> List.map (fun (name, _, _, _) -> name)
                | _ -> [])
            |> Set.ofList

        let topLevelTypeNames =
            statements
            |> List.choose (function | SType def -> Some def.Name | _ -> None)
            |> Set.ofList

        let rewriteValueName boundNames name =
            let aliasRewritten = rewriteNameWithAliases aliases name
            if aliasRewritten <> name then
                aliasRewritten
            else
                rewriteNameWithSelfPrefix selfPrefix topLevelValueNames boundNames name

        let rec rewriteExpr (boundNames: Set<string>) (expr: Expr) : Expr =
            match expr with
            | EUnit _
            | ELiteral _
            | ENone _ -> expr
            | EVar (name, span) ->
                EVar(rewriteValueName boundNames name, span)
            | ENameOf (name, span) ->
                ENameOf(rewriteValueName boundNames name, span)
            | ETypeOf (name, span) ->
                ETypeOf(rewriteTypeName aliases selfPrefix topLevelTypeNames name, span)
            | EParen (inner, span) ->
                EParen(rewriteExpr boundNames inner, span)
            | ELambda (param, body, span) ->
                let rewrittenParam =
                    { param with Annotation = param.Annotation |> Option.map (rewriteTypeRef aliases selfPrefix topLevelTypeNames) }
                ELambda(rewrittenParam, rewriteExpr (Set.add param.Name boundNames) body, span)
            | EApply (fn, arg, span) ->
                EApply(rewriteExpr boundNames fn, rewriteExpr boundNames arg, span)
            | EIf (cond, thenExpr, elseExpr, span) ->
                EIf(rewriteExpr boundNames cond, rewriteExpr boundNames thenExpr, rewriteExpr boundNames elseExpr, span)
            | ERaise (valueExpr, span) ->
                ERaise(rewriteExpr boundNames valueExpr, span)
            | EFor (name, source, body, span) ->
                EFor(name, rewriteExpr boundNames source, rewriteExpr (Set.add name boundNames) body, span)
            | EMatch (scrutinee, cases, span) ->
                let rewrittenCases =
                    cases
                    |> List.map (fun (pattern, guard, body, caseSpan) ->
                        let rewrittenPattern = rewritePattern aliases selfPrefix topLevelTypeNames pattern
                        let boundWithPattern = Set.union boundNames (collectPatternBindings rewrittenPattern)
                        rewrittenPattern, guard |> Option.map (rewriteExpr boundWithPattern), rewriteExpr boundWithPattern body, caseSpan)
                EMatch(rewriteExpr boundNames scrutinee, rewrittenCases, span)
            | ELet (name, valueExpr, bodyExpr, isRec, span) ->
                if isRec then
                    let boundWithName = Set.add name boundNames
                    ELet(name, rewriteExpr boundWithName valueExpr, rewriteExpr boundWithName bodyExpr, true, span)
                else
                    ELet(name, rewriteExpr boundNames valueExpr, rewriteExpr (Set.add name boundNames) bodyExpr, false, span)
            | ELetRecGroup (bindings, body, span) ->
                let names = bindings |> List.map (fun (name, _, _, _) -> name) |> Set.ofList
                let recursiveBound = Set.union boundNames names
                let rewrittenBindings =
                    bindings
                    |> List.map (fun (name, args, valueExpr, bindingSpan) ->
                        let rewrittenArgs =
                            args
                            |> List.map (fun p ->
                                { p with Annotation = p.Annotation |> Option.map (rewriteTypeRef aliases selfPrefix topLevelTypeNames) })
                        let boundWithArgs = rewrittenArgs |> List.fold (fun s p -> Set.add p.Name s) recursiveBound
                        name, rewrittenArgs, rewriteExpr boundWithArgs valueExpr, bindingSpan)
                ELetRecGroup(rewrittenBindings, rewriteExpr recursiveBound body, span)
            | EList (items, span) ->
                EList(items |> List.map (rewriteExpr boundNames), span)
            | ERange (a, b, span) ->
                ERange(rewriteExpr boundNames a, rewriteExpr boundNames b, span)
            | ETuple (items, span) ->
                ETuple(items |> List.map (rewriteExpr boundNames), span)
            | ERecord (fields, span) ->
                ERecord(fields |> List.map (fun (name, valueExpr) -> name, rewriteExpr boundNames valueExpr), span)
            | EStructuralRecord (fields, span) ->
                EStructuralRecord(fields |> List.map (fun (name, valueExpr) -> name, rewriteExpr boundNames valueExpr), span)
            | EMap (entries, span) ->
                let rewritten =
                    entries
                    |> List.map (function
                        | MEKeyValue (k, v) -> MEKeyValue (rewriteExpr boundNames k, rewriteExpr boundNames v)
                        | MESpread e -> MESpread (rewriteExpr boundNames e))
                EMap(rewritten, span)
            | ERecordUpdate (target, updates, span) ->
                ERecordUpdate(rewriteExpr boundNames target, updates |> List.map (fun (name, valueExpr) -> name, rewriteExpr boundNames valueExpr), span)
            | EStructuralRecordUpdate (target, updates, span) ->
                EStructuralRecordUpdate(rewriteExpr boundNames target, updates |> List.map (fun (name, valueExpr) -> name, rewriteExpr boundNames valueExpr), span)
            | EFieldGet (target, fieldName, span) ->
                match tryFlattenExprPath target with
                | Some (root :: rest) when aliases.ContainsKey root ->
                    let rewritten = aliases[root] + "." + joinDotted (rest @ [ fieldName ])
                    EVar(rewritten, span)
                | _ ->
                    EFieldGet(rewriteExpr boundNames target, fieldName, span)
            | EIndexGet (target, keyExpr, span) ->
                EIndexGet(rewriteExpr boundNames target, rewriteExpr boundNames keyExpr, span)
            | ECons (head, tail, span) ->
                ECons(rewriteExpr boundNames head, rewriteExpr boundNames tail, span)
            | EAppend (left, right, span) ->
                EAppend(rewriteExpr boundNames left, rewriteExpr boundNames right, span)
            | EBinOp (op, left, right, span) ->
                EBinOp(op, rewriteExpr boundNames left, rewriteExpr boundNames right, span)
            | ESome (valueExpr, span) ->
                ESome(rewriteExpr boundNames valueExpr, span)
            | EInterpolatedString (parts, span) ->
                let rewrittenParts =
                    parts
                    |> List.map (function
                        | IPText text -> IPText text
                        | IPExpr valueExpr -> IPExpr (rewriteExpr boundNames valueExpr))
                EInterpolatedString(rewrittenParts, span)

        let qualifySelfName name =
            match selfPrefix with
            | Some prefix -> $"{prefix}.{name}"
            | None -> name

        let rewriteParam (param: Param) =
            { param with Annotation = param.Annotation |> Option.map (rewriteTypeRef aliases selfPrefix topLevelTypeNames) }

        let rewriteTypeDef (def: TypeDef) =
            { def with
                Name = qualifySelfName def.Name
                Fields =
                    def.Fields
                    |> List.map (fun (name, t) -> name, rewriteTypeRef aliases selfPrefix topLevelTypeNames t)
                Cases =
                    def.Cases
                    |> List.map (fun (name, payload) -> name, payload |> Option.map (rewriteTypeRef aliases selfPrefix topLevelTypeNames)) }

        statements
        |> List.map (function
            | SType def ->
                SType (rewriteTypeDef def)
            | SLet(name, args, valueExpr, isRec, isExported, span) ->
                let rewrittenName = qualifySelfName name
                let rewrittenArgs = args |> List.map rewriteParam
                let bound = rewrittenArgs |> List.fold (fun s p -> Set.add p.Name s) Set.empty
                let bodyBound = if isRec then Set.add rewrittenName bound else bound
                SLet(rewrittenName, rewrittenArgs, rewriteExpr bodyBound valueExpr, isRec, isExported, span)
            | SLetRecGroup(bindings, isExported, span) ->
                let names = bindings |> List.map (fun (name, _, _, _) -> qualifySelfName name) |> Set.ofList
                let rewrittenBindings =
                    bindings
                    |> List.map (fun (name, args, valueExpr, bindingSpan) ->
                        let rewrittenName = qualifySelfName name
                        let rewrittenArgs = args |> List.map rewriteParam
                        let bound = rewrittenArgs |> List.fold (fun s p -> Set.add p.Name s) names
                        rewrittenName, rewrittenArgs, rewriteExpr bound valueExpr, bindingSpan)
                SLetRecGroup(rewrittenBindings, isExported, span)
            | SExpr expr ->
                SExpr (rewriteExpr Set.empty expr)
            | SImport _ -> failwith "Unexpected unresolved import directive")

    let private expandProgram
        (rootDirectoryWithSeparator: string)
        (fileSpan: string -> Span)
        (getOrCreatePrefix: string -> string)
        (loadFileRef: (string list -> bool -> string -> string option -> Program) ref)
        (stack: string list)
        (isMainFile: bool)
        (currentFile: string)
        (currentPrefix: string option)
        (program: Program)
        : Program =
        let mutable seenCode = false
        let imports = ResizeArray<string * string * Span>()
        let localCode = ResizeArray<Stmt>()

        for stmt in program do
            match stmt with
            | SImport(alias, importPath, span) ->
                if seenCode then
                    raise (ParseException { Message = "'import' directives must appear before code"; Span = span })
                imports.Add(alias, importPath, span)
            | _ ->
                seenCode <- true
                localCode.Add(stmt)

        let aliasMappings =
            imports
            |> Seq.fold (fun (acc: Map<string, string>) (alias, importPath, span) ->
                if not (isValidAliasName alias) then
                    raise (ParseException { Message = $"Invalid import alias '{alias}'"; Span = span })
                if acc.ContainsKey(alias) then
                    raise (ParseException { Message = $"Duplicate import alias '{alias}'"; Span = span })
                let resolvedPath = resolveImportPath currentFile importPath rootDirectoryWithSeparator span
                let fullPrefix = getOrCreatePrefix resolvedPath
                acc |> Map.add alias fullPrefix) Map.empty

        let importedStatements =
            imports
            |> Seq.toList
            |> List.collect (fun (alias, importPath, span) ->
                let resolvedPath = resolveImportPath currentFile importPath rootDirectoryWithSeparator span
                let childPrefix = aliasMappings[alias]
                (!loadFileRef) stack false resolvedPath (Some childPrefix))

        let localStatements = localCode |> Seq.toList
        let rewrittenLocalStatements =
            rewriteModuleScopedStatements (if isMainFile then None else currentPrefix) aliasMappings localStatements

        importedStatements @ rewrittenLocalStatements

    let parseIncludedSource (sourceName: string) (source: string) : Program =
        let program = Parser.parseProgramWithSourceName (Some sourceName) source
        let dummyRoot = normalizeDirectoryPath "."
        let fileSpan path =
            let p = Span.posInFile path 1 1
            Span.mk p p
        let mutable counter = 0
        let nextPrefixSegment () =
            counter <- counter + 1
            $"__imp{counter}"
        let pathPrefixes = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        let getOrCreatePrefix (path: string) =
            match pathPrefixes.TryGetValue(path) with
            | true, prefix -> prefix
            | false, _ ->
                let prefix = nextPrefixSegment ()
                pathPrefixes[path] <- prefix
                prefix
        let loadRef = ref (fun (_: string list) (_: bool) (_: string) (_: string option) -> ([]: Program))
        if program |> List.exists (function | SImport _ -> true | _ -> false) then
            let importSpan =
                program
                |> List.choose (function | SImport(_, _, span) -> Some span | _ -> None)
                |> List.head
            raise (ParseException { Message = "Embedded stdlib source does not support 'import'"; Span = importSpan })
        let prefix = tryGetSourceModulePrefix sourceName
        expandProgram dummyRoot fileSpan getOrCreatePrefix loadRef [] false sourceName prefix program

    let parseProgramFromFile (rootDirectory: string) (entryFile: string) : Program =
        let rootDirectoryWithSeparator = normalizeDirectoryPath rootDirectory
        let fileSpan path =
            let p = Span.posInFile path 1 1
            Span.mk p p
        let mutable counter = 0
        let nextPrefixSegment () =
            counter <- counter + 1
            $"__imp{counter}"
        let pathPrefixes = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        let getOrCreatePrefix (path: string) =
            match pathPrefixes.TryGetValue(path) with
            | true, prefix -> prefix
            | false, _ ->
                let prefix = nextPrefixSegment ()
                pathPrefixes[path] <- prefix
                prefix
        let emittedFiles = HashSet<string>(StringComparer.OrdinalIgnoreCase)

        let rec loadFile (stack: string list) (isMainFile: bool) (filePath: string) (prefix: string option) : Program =
            let fullFilePath = Path.GetFullPath(filePath)
            let initialSpan = fileSpan fullFilePath
            ensureFssPath fullFilePath initialSpan
            let sandboxedPath = ensureWithinRoot rootDirectoryWithSeparator fullFilePath initialSpan

            if stack |> List.exists (fun p -> String.Equals(p, sandboxedPath, StringComparison.OrdinalIgnoreCase)) then
                let cycleChain = (sandboxedPath :: stack |> List.rev) @ [ sandboxedPath ]
                let message = sprintf "Import cycle detected: %s" (String.concat " -> " cycleChain)
                raise (ParseException { Message = message; Span = fileSpan sandboxedPath })
            elif not isMainFile && emittedFiles.Contains(sandboxedPath) then
                []
            else
                if not isMainFile then
                    emittedFiles.Add(sandboxedPath) |> ignore

                let source = File.ReadAllText(sandboxedPath)
                let program = Parser.parseProgramWithSourceName (Some sandboxedPath) source
                let loadRef = ref loadFile
                expandProgram rootDirectoryWithSeparator fileSpan getOrCreatePrefix loadRef (sandboxedPath :: stack) isMainFile sandboxedPath prefix program

        loadFile [] true entryFile None

    let private parseProgramFromSourceWithIncludesCore
        (rootDirectory: string)
        (entryFile: string)
        (entrySource: string)
        (resolveImportedSource: (string -> string option) option)
        : Program =
        let rootDirectoryWithSeparator = normalizeDirectoryPath rootDirectory
        let fileSpan path =
            let p = Span.posInFile path 1 1
            Span.mk p p

        let mutable counter = 0
        let nextPrefixSegment () =
            counter <- counter + 1
            $"__imp{counter}"
        let pathPrefixes = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        let getOrCreatePrefix (path: string) =
            match pathPrefixes.TryGetValue(path) with
            | true, prefix -> prefix
            | false, _ ->
                let prefix = nextPrefixSegment ()
                pathPrefixes[path] <- prefix
                prefix
        let emittedFiles = HashSet<string>(StringComparer.OrdinalIgnoreCase)

        let entryFullPath = Path.GetFullPath(entryFile)
        let entrySpan = fileSpan entryFullPath
        ensureFssPath entryFullPath entrySpan
        let entrySandboxedPath = ensureWithinRoot rootDirectoryWithSeparator entryFullPath entrySpan

        let rec loadFile (stack: string list) (isMainFile: bool) (filePath: string) (prefix: string option) : Program =
            let fullFilePath = Path.GetFullPath(filePath)
            let initialSpan = fileSpan fullFilePath
            ensureFssPath fullFilePath initialSpan
            let sandboxedPath = ensureWithinRoot rootDirectoryWithSeparator fullFilePath initialSpan

            if stack |> List.exists (fun p -> String.Equals(p, sandboxedPath, StringComparison.OrdinalIgnoreCase)) then
                let cycleChain = (sandboxedPath :: stack |> List.rev) @ [ sandboxedPath ]
                let message = sprintf "Import cycle detected: %s" (String.concat " -> " cycleChain)
                raise (ParseException { Message = message; Span = fileSpan sandboxedPath })
            elif not isMainFile && emittedFiles.Contains(sandboxedPath) then
                []
            else
                if not isMainFile then
                    emittedFiles.Add(sandboxedPath) |> ignore

                let source =
                    if String.Equals(sandboxedPath, entrySandboxedPath, StringComparison.OrdinalIgnoreCase) then
                        entrySource
                    else
                        match resolveImportedSource with
                        | Some resolver ->
                            match resolver sandboxedPath with
                            | Some source -> source
                            | None ->
                                raise (ParseException { Message = $"Imported file '{sandboxedPath}' could not be resolved"; Span = fileSpan sandboxedPath })
                        | None ->
                            File.ReadAllText(sandboxedPath)
                let program = Parser.parseProgramWithSourceName (Some sandboxedPath) source
                let loadRef = ref loadFile
                expandProgram rootDirectoryWithSeparator fileSpan getOrCreatePrefix loadRef (sandboxedPath :: stack) isMainFile sandboxedPath prefix program

        loadFile [] true entrySandboxedPath None

    let parseProgramFromSourceWithIncludes (rootDirectory: string) (entryFile: string) (entrySource: string) : Program =
        parseProgramFromSourceWithIncludesCore rootDirectory entryFile entrySource None

    let parseProgramFromSourceWithIncludesResolver
        (rootDirectory: string)
        (entryFile: string)
        (entrySource: string)
        (resolveImportedSource: string -> string option)
        : Program =
        parseProgramFromSourceWithIncludesCore rootDirectory entryFile entrySource (Some resolveImportedSource)
