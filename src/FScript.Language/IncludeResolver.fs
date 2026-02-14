namespace FScript.Language

open System
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

    let private qualifyName (moduleName: string) (name: string) = $"{moduleName}.{name}"

    let private isValidModuleName (name: string) =
        let startsValid c = Char.IsLetter(c) || c = '_'
        let partValid c = Char.IsLetterOrDigit(c) || c = '_'
        not (String.IsNullOrWhiteSpace(name))
        && startsValid name[0]
        && (name |> Seq.forall partValid)

    let private deriveModuleNameFromFilePath (filePath: string) (span: Span) =
        let stem = Path.GetFileNameWithoutExtension(filePath)
        if isValidModuleName stem then
            stem
        else
            raise (ParseException {
                Message = $"Imported filename stem '{stem}' is not a valid module name. Rename the file to a valid identifier."
                Span = span
            })

    let private rewriteModuleScopedStatements (moduleName: string) (statements: Stmt list) : Stmt list =
        let topLevelNames =
            statements
            |> List.collect (function
                | SLet(name, _, _, _, _, _) -> [ name ]
                | SLetRecGroup(bindings, _, _) -> bindings |> List.map (fun (name, _, _, _) -> name)
                | _ -> [])
            |> Set.ofList

        let maybeQualify (boundNames: Set<string>) (name: string) =
            if Set.contains name topLevelNames && not (Set.contains name boundNames) then
                qualifyName moduleName name
            else
                name

        let rec rewriteExpr (boundNames: Set<string>) (expr: Expr) : Expr =
            match expr with
            | EUnit _
            | ELiteral _
            | ENone _
            | ETypeOf _ -> expr
            | EVar (name, span) -> EVar(maybeQualify boundNames name, span)
            | ENameOf (name, span) -> ENameOf(maybeQualify boundNames name, span)
            | EParen (inner, span) -> EParen(rewriteExpr boundNames inner, span)
            | ELambda (param, body, span) ->
                ELambda(param, rewriteExpr (Set.add param.Name boundNames) body, span)
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
                        let boundWithPattern = Set.union boundNames (collectPatternBindings pattern)
                        pattern, guard |> Option.map (rewriteExpr boundWithPattern), rewriteExpr boundWithPattern body, caseSpan)
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
                        let boundWithArgs = args |> List.fold (fun s p -> Set.add p.Name s) recursiveBound
                        name, args, rewriteExpr boundWithArgs valueExpr, bindingSpan)
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

        statements
        |> List.map (function
            | SLet(name, args, valueExpr, isRec, isExported, span) ->
                let qualifiedName = qualifyName moduleName name
                let bound = args |> List.fold (fun s p -> Set.add p.Name s) Set.empty
                let bodyBound = if isRec then Set.add qualifiedName bound else bound
                SLet(qualifiedName, args, rewriteExpr bodyBound valueExpr, isRec, isExported, span)
            | SLetRecGroup(bindings, isExported, span) ->
                let names = bindings |> List.map (fun (name, _, _, _) -> qualifyName moduleName name) |> Set.ofList
                let rewrittenBindings =
                    bindings
                    |> List.map (fun (name, args, valueExpr, bindingSpan) ->
                        let qualifiedName = qualifyName moduleName name
                        let bound = args |> List.fold (fun s p -> Set.add p.Name s) names
                        qualifiedName, args, rewriteExpr bound valueExpr, bindingSpan)
                SLetRecGroup(rewrittenBindings, isExported, span)
            | SExpr expr -> SExpr (rewriteExpr Set.empty expr)
            | stmt -> stmt)

    let private expandProgram
        (rootDirectoryWithSeparator: string)
        (fileSpan: string -> Span)
        (moduleToFile: System.Collections.Generic.Dictionary<string, string>)
        (loadFileRef: (string list -> bool -> string -> Program) ref)
        (stack: string list)
        (isMainFile: bool)
        (currentFile: string)
        (program: Program)
        : Program =
        let mutable seenCode = false
        let imports = ResizeArray<string * Span>()
        let localCode = ResizeArray<Stmt>()

        for stmt in program do
            match stmt with
            | SImport(importPath, span) ->
                if seenCode then
                    raise (ParseException { Message = "'import' directives must appear before code"; Span = span })
                imports.Add(importPath, span)
            | _ ->
                seenCode <- true
                localCode.Add(stmt)

        let importedStatements =
            imports
            |> Seq.toList
            |> List.collect (fun (importPath, span) ->
                let resolvedPath = resolveImportPath currentFile importPath rootDirectoryWithSeparator span
                (!loadFileRef) stack false resolvedPath)

        let localStatements = localCode |> Seq.toList
        let rewrittenLocalStatements =
            if isMainFile then
                localStatements
            else
                let span = fileSpan currentFile
                let moduleName = deriveModuleNameFromFilePath currentFile span
                match moduleToFile.TryGetValue(moduleName) with
                | true, existingPath when not (String.Equals(existingPath, currentFile, StringComparison.OrdinalIgnoreCase)) ->
                    raise (ParseException {
                        Message = $"Module name collision: '{moduleName}' is derived from both '{existingPath}' and '{currentFile}'"
                        Span = span
                    })
                | _ ->
                    moduleToFile[moduleName] <- currentFile
                    rewriteModuleScopedStatements moduleName localStatements

        importedStatements @ rewrittenLocalStatements

    let parseIncludedSource (sourceName: string) (source: string) : Program =
        let program = Parser.parseProgramWithSourceName (Some sourceName) source
        let dummyRoot = normalizeDirectoryPath "."
        let fileSpan path =
            let p = Span.posInFile path 1 1
            Span.mk p p
        let moduleToFile = System.Collections.Generic.Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        let loadRef = ref (fun (_: string list) (_: bool) (_: string) -> ([]: Program))
        if program |> List.exists (function | SImport _ -> true | _ -> false) then
            let importSpan =
                program
                |> List.choose (function | SImport(_, span) -> Some span | _ -> None)
                |> List.head
            raise (ParseException { Message = "Embedded stdlib source does not support 'import'"; Span = importSpan })
        expandProgram dummyRoot fileSpan moduleToFile loadRef [] false sourceName program

    let parseProgramFromFile (rootDirectory: string) (entryFile: string) : Program =
        let rootDirectoryWithSeparator = normalizeDirectoryPath rootDirectory
        let visited = System.Collections.Generic.HashSet<string>(StringComparer.OrdinalIgnoreCase)
        let moduleToFile = System.Collections.Generic.Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        let fileSpan path =
            let p = Span.posInFile path 1 1
            Span.mk p p

        let rec loadFile (stack: string list) (isMainFile: bool) (filePath: string) : Program =
            let fullFilePath = Path.GetFullPath(filePath)
            let initialSpan = fileSpan fullFilePath
            ensureFssPath fullFilePath initialSpan
            let sandboxedPath = ensureWithinRoot rootDirectoryWithSeparator fullFilePath initialSpan

            if stack |> List.exists (fun p -> String.Equals(p, sandboxedPath, StringComparison.OrdinalIgnoreCase)) then
                let cycleChain = (sandboxedPath :: stack |> List.rev) @ [ sandboxedPath ]
                let message = sprintf "Import cycle detected: %s" (String.concat " -> " cycleChain)
                raise (ParseException { Message = message; Span = fileSpan sandboxedPath })

            if visited.Contains(sandboxedPath) then
                []
            else
                visited.Add(sandboxedPath) |> ignore
                let source = File.ReadAllText(sandboxedPath)
                let program = Parser.parseProgramWithSourceName (Some sandboxedPath) source
                let loadRef = ref loadFile
                expandProgram rootDirectoryWithSeparator fileSpan moduleToFile loadRef (sandboxedPath :: stack) isMainFile sandboxedPath program

        loadFile [] true entryFile

    let parseProgramFromSourceWithIncludes (rootDirectory: string) (entryFile: string) (entrySource: string) : Program =
        let rootDirectoryWithSeparator = normalizeDirectoryPath rootDirectory
        let visited = System.Collections.Generic.HashSet<string>(StringComparer.OrdinalIgnoreCase)
        let moduleToFile = System.Collections.Generic.Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        let fileSpan path =
            let p = Span.posInFile path 1 1
            Span.mk p p

        let entryFullPath = Path.GetFullPath(entryFile)
        let entrySpan = fileSpan entryFullPath
        ensureFssPath entryFullPath entrySpan
        let entrySandboxedPath = ensureWithinRoot rootDirectoryWithSeparator entryFullPath entrySpan

        let rec loadFile (stack: string list) (isMainFile: bool) (filePath: string) : Program =
            let fullFilePath = Path.GetFullPath(filePath)
            let initialSpan = fileSpan fullFilePath
            ensureFssPath fullFilePath initialSpan
            let sandboxedPath = ensureWithinRoot rootDirectoryWithSeparator fullFilePath initialSpan

            if stack |> List.exists (fun p -> String.Equals(p, sandboxedPath, StringComparison.OrdinalIgnoreCase)) then
                let cycleChain = (sandboxedPath :: stack |> List.rev) @ [ sandboxedPath ]
                let message = sprintf "Import cycle detected: %s" (String.concat " -> " cycleChain)
                raise (ParseException { Message = message; Span = fileSpan sandboxedPath })

            if visited.Contains(sandboxedPath) then
                []
            else
                visited.Add(sandboxedPath) |> ignore
                let source =
                    if String.Equals(sandboxedPath, entrySandboxedPath, StringComparison.OrdinalIgnoreCase) then
                        entrySource
                    else
                        File.ReadAllText(sandboxedPath)
                let program = Parser.parseProgramWithSourceName (Some sandboxedPath) source
                let loadRef = ref loadFile
                expandProgram rootDirectoryWithSeparator fileSpan moduleToFile loadRef (sandboxedPath :: stack) isMainFile sandboxedPath program

        loadFile [] true entrySandboxedPath
