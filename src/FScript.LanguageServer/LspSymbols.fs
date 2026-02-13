namespace FScript.LanguageServer

open System
open System.Collections.Generic
open System.Text.Json.Nodes
open FScript.Language

module LspSymbols =
    open LspModel

    let rec private typeRefToString (typeRef: TypeRef) =
        match typeRef with
        | TRName name -> name
        | TRTuple parts -> parts |> List.map typeRefToString |> String.concat " * " |> sprintf "(%s)"
        | TRFun (a, b) -> sprintf "%s -> %s" (typeRefToString a) (typeRefToString b)
        | TRPostfix (inner, suffix) -> sprintf "%s %s" (typeRefToString inner) suffix
        | TRRecord fields ->
            fields
            |> List.map (fun (name, t) -> sprintf "%s: %s" name (typeRefToString t))
            |> String.concat "; "
            |> sprintf "{ %s }"
        | TRStructuralRecord fields ->
            fields
            |> List.map (fun (name, t) -> sprintf "%s: %s" name (typeRefToString t))
            |> String.concat "; "
            |> sprintf "{| %s |}"

    let private canonicalRecordSignatureFromFields (fields: (string * string) list) =
        fields
        |> List.sortBy fst
        |> List.map (fun (name, t) -> $"{name}:{t}")
        |> String.concat ";"

    let buildSymbolsFromProgram (program: Program) (typed: TypeInfer.TypedProgram option) : TopLevelSymbol list =
        let typedByName = Dictionary<string, TypeInfer.TypedStmt>(StringComparer.Ordinal)
        let recordTypeDefsBySignature = Dictionary<string, ResizeArray<string>>(StringComparer.Ordinal)

        let canonicalRecordSignatureFromType (t: Type) =
            match t with
            | TRecord fields ->
                fields
                |> Map.toList
                |> List.map (fun (name, ty) -> name, Types.typeToString ty)
                |> canonicalRecordSignatureFromFields
                |> Some
            | _ -> None

        match typed with
        | Some typedProgram ->
            for stmt in typedProgram do
                match stmt with
                | TypeInfer.TSLet(name, _, _, _, _, _) ->
                    typedByName[name] <- stmt
                | TypeInfer.TSLetRecGroup(bindings, _, _) ->
                    for (name, _, _, _) in bindings do
                        typedByName[name] <- stmt
                | _ -> ()
        | None -> ()

        for stmt in program do
            match stmt with
            | SType typeDef when typeDef.Cases.IsEmpty ->
                let signature =
                    typeDef.Fields
                    |> List.map (fun (fieldName, t) -> fieldName, typeRefToString t)
                    |> canonicalRecordSignatureFromFields

                if not (recordTypeDefsBySignature.ContainsKey(signature)) then
                    recordTypeDefsBySignature[signature] <- ResizeArray<string>()
                recordTypeDefsBySignature[signature].Add(typeDef.Name)
            | _ -> ()

        let mkFromTyped (name: string) (span: Span) (fallbackKind: int) =
            let tryResolveRecordTarget (t: Type) =
                match canonicalRecordSignatureFromType t with
                | Some signature when recordTypeDefsBySignature.ContainsKey(signature) ->
                    let candidates = recordTypeDefsBySignature[signature] |> Seq.distinct |> Seq.toList
                    match candidates with
                    | [ one ] -> Some one
                    | _ -> None
                | _ -> None

            let typeTargetFromType (t: Type) =
                match t with
                | TNamed name -> Some name
                | TUnion (name, _) -> Some name
                | TRecord _ -> tryResolveRecordTarget t
                | _ -> None

            match typedByName.TryGetValue(name) with
            | true, TypeInfer.TSLet(_, _, t, _, _, _) ->
                { Name = name
                  Kind = symbolKindForType t
                  TypeText = Some (Types.typeToString t)
                  TypeTargetName = typeTargetFromType t
                  Span = span }
            | true, TypeInfer.TSLetRecGroup(bindings, _, _) ->
                match bindings |> List.tryFind (fun (n, _, _, _) -> n = name) with
                | Some (_, _, t, _) ->
                    { Name = name
                      Kind = symbolKindForType t
                      TypeText = Some (Types.typeToString t)
                      TypeTargetName = typeTargetFromType t
                      Span = span }
                | None ->
                    { Name = name
                      Kind = fallbackKind
                      TypeText = None
                      TypeTargetName = None
                      Span = span }
            | _ ->
                { Name = name
                  Kind = fallbackKind
                  TypeText = None
                  TypeTargetName = None
                  Span = span }

        program
        |> List.collect (fun stmt ->
            match stmt with
            | SType typeDef ->
                let typeText =
                    if typeDef.Cases.IsEmpty then
                        let fields =
                            typeDef.Fields
                            |> List.map (fun (name, t) -> $"{name}: {typeRefToString t}")
                            |> String.concat "; "
                        Some $"{{ {fields} }}"
                    else
                        Some typeDef.Name

                let typeSymbol =
                    { Name = typeDef.Name
                      Kind = 5
                      TypeText = typeText
                      TypeTargetName = None
                      Span = typeDef.Span }

                let caseSymbols =
                    typeDef.Cases
                    |> List.collect (fun (caseName, payload) ->
                        let caseType =
                            match payload with
                            | Some payloadType -> Some (sprintf "%s -> %s" (typeRefToString payloadType) typeDef.Name)
                            | None -> Some typeDef.Name

                        [ { Name = caseName
                            Kind = 22
                            TypeText = caseType
                            TypeTargetName = Some typeDef.Name
                            Span = typeDef.Span }
                          { Name = $"{typeDef.Name}.{caseName}"
                            Kind = 22
                            TypeText = caseType
                            TypeTargetName = Some typeDef.Name
                            Span = typeDef.Span } ])

                typeSymbol :: caseSymbols
            | SLet(name, args, _, _, _, span) ->
                [ mkFromTyped name span (declarationKindFromArgs args) ]
            | SLetRecGroup(bindings, _, _) ->
                bindings
                |> List.map (fun (name, args, _, span) -> mkFromTyped name span (declarationKindFromArgs args))
            | _ -> [])

    let collectVariableOccurrences (program: Program) : Map<string, Span list> =
        let addOccurrence name span (acc: Map<string, Span list>) =
            let existing = acc |> Map.tryFind name |> Option.defaultValue []
            acc |> Map.add name (span :: existing)

        let rec collectExpr (acc: Map<string, Span list>) (expr: Expr) =
            match expr with
            | EVar (name, span) -> addOccurrence name span acc
            | EParen (inner, _) -> collectExpr acc inner
            | ELambda (_, body, _) -> collectExpr acc body
            | EApply (fn, arg, _) ->
                let withFn = collectExpr acc fn
                collectExpr withFn arg
            | EIf (c, t, f, _) ->
                let withCond = collectExpr acc c
                let withThen = collectExpr withCond t
                collectExpr withThen f
            | ERaise (inner, _) -> collectExpr acc inner
            | EFor (_, source, body, _) ->
                let withSource = collectExpr acc source
                collectExpr withSource body
            | EMatch (scrutinee, cases, _) ->
                let withScrutinee = collectExpr acc scrutinee
                cases
                |> List.fold (fun state (_, guard, body, _) ->
                    let withGuard =
                        match guard with
                        | Some g -> collectExpr state g
                        | None -> state
                    collectExpr withGuard body) withScrutinee
            | ELet (_, value, body, _, _) ->
                let withValue = collectExpr acc value
                collectExpr withValue body
            | ELetRecGroup (bindings, body, _) ->
                let withBindings =
                    bindings
                    |> List.fold (fun state (_, _, value, _) -> collectExpr state value) acc
                collectExpr withBindings body
            | EList (items, _) ->
                items |> List.fold collectExpr acc
            | ERange (startExpr, endExpr, _) ->
                let withStart = collectExpr acc startExpr
                collectExpr withStart endExpr
            | ETuple (items, _) ->
                items |> List.fold collectExpr acc
            | ERecord (fields, _) ->
                fields |> List.fold (fun state (_, value) -> collectExpr state value) acc
            | EStructuralRecord (fields, _) ->
                fields |> List.fold (fun state (_, value) -> collectExpr state value) acc
            | EMap (entries, _) ->
                entries
                |> List.fold (fun state entry ->
                    match entry with
                    | MEKeyValue (keyExpr, valueExpr) ->
                        let withKey = collectExpr state keyExpr
                        collectExpr withKey valueExpr
                    | MESpread spreadExpr ->
                        collectExpr state spreadExpr) acc
            | ERecordUpdate (baseExpr, fields, _) ->
                let withBase = collectExpr acc baseExpr
                fields |> List.fold (fun state (_, value) -> collectExpr state value) withBase
            | EStructuralRecordUpdate (baseExpr, fields, _) ->
                let withBase = collectExpr acc baseExpr
                fields |> List.fold (fun state (_, value) -> collectExpr state value) withBase
            | EFieldGet (target, _, _) -> collectExpr acc target
            | EIndexGet (target, key, _) ->
                let withTarget = collectExpr acc target
                collectExpr withTarget key
            | ECons (head, tail, _) ->
                let withHead = collectExpr acc head
                collectExpr withHead tail
            | EAppend (left, right, _) ->
                let withLeft = collectExpr acc left
                collectExpr withLeft right
            | EBinOp (_, left, right, _) ->
                let withLeft = collectExpr acc left
                collectExpr withLeft right
            | ESome (inner, _) -> collectExpr acc inner
            | EInterpolatedString (parts, _) ->
                parts
                |> List.fold (fun state part ->
                    match part with
                    | IPText _ -> state
                    | IPExpr embedded -> collectExpr state embedded) acc
            | EUnit _
            | ELiteral _
            | ENone _
            | ETypeOf _
            | ENameOf _ -> acc

        let withDeclsAndExprs =
            program
            |> List.fold (fun state stmt ->
                match stmt with
                | SLet (name, _, expr, _, _, span) ->
                    let withDecl = addOccurrence name span state
                    collectExpr withDecl expr
                | SLetRecGroup (bindings, _, _) ->
                    bindings
                    |> List.fold (fun inner (name, _, expr, span) ->
                        let withDecl = addOccurrence name span inner
                        collectExpr withDecl expr) state
                | SExpr expr ->
                    collectExpr state expr
                | _ -> state) Map.empty

        withDeclsAndExprs
        |> Map.map (fun _ spans -> spans |> List.rev)

    let private isIncludeProgram (program: Program) =
        program
        |> List.exists (function
            | SInclude _ -> true
            | _ -> false)

    let private buildRecordParameterFields (program: Program) =
        let typeRecordFields =
            program
            |> List.choose (function
                | SType typeDef when typeDef.Cases.IsEmpty ->
                    let fields =
                        typeDef.Fields
                        |> List.map (fun (fieldName, t) -> fieldName, typeRefToString t)
                    Some (typeDef.Name, fields)
                | _ -> None)
            |> Map.ofList

        let fieldsFromAnnotation (annotation: TypeRef option) =
            match annotation with
            | Some (TRRecord fields) ->
                Some (fields |> List.map (fun (fieldName, t) -> fieldName, typeRefToString t))
            | Some (TRName typeName) ->
                typeRecordFields |> Map.tryFind typeName
            | _ ->
                None

        let collectFromArgs (acc: Map<string, (string * string) list>) (args: Param list) =
            args
            |> List.fold (fun state arg ->
                match fieldsFromAnnotation arg.Annotation with
                | Some fields -> state |> Map.add arg.Name fields
                | None -> state) acc

        program
        |> List.fold (fun state stmt ->
            match stmt with
            | SLet (_, args, _, _, _, _) ->
                collectFromArgs state args
            | SLetRecGroup (bindings, _, _) ->
                bindings
                |> List.fold (fun inner (_, args, _, _) -> collectFromArgs inner args) state
            | _ ->
                state) Map.empty

    let private buildParameterTypeTargets (program: Program) =
        let namedRecordTypeBySignature =
            program
            |> List.choose (function
                | SType typeDef when typeDef.Cases.IsEmpty ->
                    let fields =
                        typeDef.Fields
                        |> List.map (fun (fieldName, t) -> fieldName, typeRefToString t)
                    Some (canonicalRecordSignatureFromFields fields, typeDef.Name)
                | _ -> None)
            |> List.groupBy fst
            |> List.choose (fun (sigText, entries) ->
                let names = entries |> List.map snd |> List.distinct
                match names with
                | [ one ] -> Some (sigText, one)
                | _ -> None)
            |> Map.ofList

        let resolveAnnotationTarget (annotation: TypeRef option) =
            match annotation with
            | Some (TRName typeName) -> Some typeName
            | Some (TRRecord fields) ->
                fields
                |> List.map (fun (fieldName, t) -> fieldName, typeRefToString t)
                |> canonicalRecordSignatureFromFields
                |> fun sigText -> namedRecordTypeBySignature |> Map.tryFind sigText
            | _ -> None

        let collectFromArgs (acc: Map<string, string>) (args: Param list) =
            args
            |> List.fold (fun state arg ->
                match resolveAnnotationTarget arg.Annotation with
                | Some typeName -> state |> Map.add arg.Name typeName
                | None -> state) acc

        program
        |> List.fold (fun state stmt ->
            match stmt with
            | SLet (_, args, _, _, _, _) ->
                collectFromArgs state args
            | SLetRecGroup (bindings, _, _) ->
                bindings
                |> List.fold (fun inner (_, args, _, _) -> collectFromArgs inner args) state
            | _ ->
                state) Map.empty

    let private buildFunctionParameters (program: Program) =
        let addBinding name (args: Param list) (acc: Map<string, string list>) =
            let paramNames =
                args
                |> List.map (fun p -> p.Name)
                |> List.filter (fun n -> not (String.IsNullOrWhiteSpace(n)))
            if paramNames.IsEmpty then acc else acc |> Map.add name paramNames

        program
        |> List.fold (fun state stmt ->
            match stmt with
            | SLet (name, args, _, _, _, _) ->
                addBinding name args state
            | SLetRecGroup (bindings, _, _) ->
                bindings
                |> List.fold (fun inner (name, args, _, _) -> addBinding name args inner) state
            | _ ->
                state) Map.empty

    let private buildParameterTypeHints (program: Program) (typed: TypeInfer.TypedProgram option) =
        let typedByName = Dictionary<string, Type>(StringComparer.Ordinal)

        match typed with
        | Some typedProgram ->
            for stmt in typedProgram do
                match stmt with
                | TypeInfer.TSLet(name, _, t, _, _, _) ->
                    typedByName[name] <- t
                | TypeInfer.TSLetRecGroup(bindings, _, _) ->
                    for (name, _, t, _) in bindings do
                        typedByName[name] <- t
                | _ -> ()
        | None -> ()

        let rec collectLambdaParams (expr: Expr) =
            match expr with
            | ELambda (param, body, _) ->
                param :: collectLambdaParams body
            | EParen (inner, _) ->
                collectLambdaParams inner
            | _ ->
                []

        let rec takeParamTypes t count =
            if count <= 0 then []
            else
                match t with
                | TFun (arg, rest) ->
                    arg :: takeParamTypes rest (count - 1)
                | _ ->
                    []

        let emitHints (name: string) (parameters: Param list) =
            match typedByName.TryGetValue(name) with
            | true, t when not parameters.IsEmpty ->
                let argTypes = takeParamTypes t parameters.Length
                (parameters, argTypes)
                ||> List.zip
                |> List.choose (fun (param, argType) ->
                    if param.Annotation.IsSome then
                        None
                    else
                        Some (param.Span, $": {Types.typeToString argType}"))
            | _ ->
                []

        program
        |> List.collect (fun stmt ->
            match stmt with
            | SLet (name, args, expr, _, _, _) ->
                let allParams =
                    if args.IsEmpty then collectLambdaParams expr else args
                emitHints name allParams
            | SLetRecGroup (bindings, _, _) ->
                bindings
                |> List.collect (fun (name, args, expr, _) ->
                    let allParams =
                        if args.IsEmpty then collectLambdaParams expr else args
                    emitHints name allParams)
            | _ ->
                [])

    let analyzeDocument (uri: string) (text: string) =
        let sourceName =
            if uri.StartsWith("file://", StringComparison.OrdinalIgnoreCase) then
                Uri(uri).LocalPath
            else
                uri

        let diagnostics = ResizeArray<JsonNode>()
        let mutable symbols : TopLevelSymbol list = []
        let mutable occurrences : Map<string, Span list> = Map.empty
        let mutable recordParamFields : Map<string, (string * string) list> = Map.empty
        let mutable parameterTypeTargets : Map<string, string> = Map.empty
        let mutable functionParameters : Map<string, string list> = Map.empty
        let mutable parameterTypeHints : (Span * string) list = []

        let mutable parsedProgram : Program option = None

        try
            let program = FScript.parseWithSourceName (Some sourceName) text
            parsedProgram <- Some program
            occurrences <- collectVariableOccurrences program
            recordParamFields <- buildRecordParameterFields program
            parameterTypeTargets <- buildParameterTypeTargets program
            functionParameters <- buildFunctionParameters program
            if isIncludeProgram program then
                symbols <- buildSymbolsFromProgram program None
                parameterTypeHints <- buildParameterTypeHints program None
            else
                try
                    let typed = TypeInfer.inferProgram program
                    symbols <- buildSymbolsFromProgram program (Some typed)
                    parameterTypeHints <- buildParameterTypeHints program (Some typed)
                with
                | TypeException err ->
                    diagnostics.Add(diagnostic 1 "type" err.Span err.Message)
                    symbols <- buildSymbolsFromProgram program None
                    parameterTypeHints <- buildParameterTypeHints program None

        with
        | ParseException err ->
            diagnostics.Add(diagnostic 1 "parse" err.Span err.Message)

        match parsedProgram with
        | Some program ->
            let topLevelBindings =
                program
                |> List.collect (function
                    | SLet(name, _, _, _, isExported, span) -> [ name, span, isExported ]
                    | SLetRecGroup(bindings, isExported, _) ->
                        bindings |> List.map (fun (name, _, _, span) -> name, span, isExported)
                    | _ -> [])

            for (name, span, isExported) in topLevelBindings do
                if not isExported && not (name.StartsWith("_", StringComparison.Ordinal)) then
                    let refs = occurrences |> Map.tryFind name |> Option.defaultValue []
                    if refs.Length <= 1 then
                        diagnostics.Add(diagnostic 2 "unused" span $"Unused top-level binding '{name}'")
        | None -> ()

        documents[uri] <- { Text = text; Symbols = symbols; RecordParameterFields = recordParamFields; ParameterTypeTargets = parameterTypeTargets; FunctionParameters = functionParameters; ParameterTypeHints = parameterTypeHints; VariableOccurrences = occurrences }
        publishDiagnostics uri (diagnostics |> Seq.toList)

    let tryResolveSymbol (doc: DocumentState) (line: int) (character: int) : TopLevelSymbol option =
        match tryGetWordAtPosition doc.Text line character with
        | None -> None
        | Some word ->
            let candidates =
                if word.Contains('.') then
                    let segments = word.Split('.') |> Array.toList
                    word :: segments
                else
                    [ word ]
                |> List.distinct

            candidates
            |> List.tryPick (fun candidate ->
                doc.Symbols |> List.tryFind (fun s -> s.Name = candidate))

    let private splitTopLevelSemicolons (text: string) =
        let parts = ResizeArray<string>()
        let mutable depthParen = 0
        let mutable depthBrace = 0
        let mutable depthBracket = 0
        let mutable start = 0

        for i = 0 to text.Length - 1 do
            match text[i] with
            | '(' -> depthParen <- depthParen + 1
            | ')' -> if depthParen > 0 then depthParen <- depthParen - 1
            | '{' -> depthBrace <- depthBrace + 1
            | '}' -> if depthBrace > 0 then depthBrace <- depthBrace - 1
            | '[' -> depthBracket <- depthBracket + 1
            | ']' -> if depthBracket > 0 then depthBracket <- depthBracket - 1
            | ';' when depthParen = 0 && depthBrace = 0 && depthBracket = 0 ->
                let chunk = text.Substring(start, i - start).Trim()
                if chunk <> "" then
                    parts.Add(chunk)
                start <- i + 1
            | _ -> ()

        if start <= text.Length then
            let tail = text.Substring(start).Trim()
            if tail <> "" then
                parts.Add(tail)

        parts |> Seq.toList

    let private tryParseRecordFields (typeText: string) =
        let trimmed = typeText.Trim()
        if trimmed.StartsWith("{", StringComparison.Ordinal) && trimmed.EndsWith("}", StringComparison.Ordinal) then
            let inner = trimmed.Substring(1, trimmed.Length - 2).Trim()
            if inner = "" then
                Some []
            else
                let fields =
                    splitTopLevelSemicolons inner
                    |> List.choose (fun part ->
                        let idx = part.IndexOf(':')
                        if idx <= 0 then None
                        else
                            let name = part.Substring(0, idx).Trim()
                            let fieldType = part.Substring(idx + 1).Trim()
                            if name = "" || fieldType = "" then None else Some (name, fieldType))

                if fields.IsEmpty then None else Some fields
        else
            None

    let private tryRecordFieldsForQualifier (doc: DocumentState) (qualifier: string) =
        let normalized =
            if qualifier.Contains('.') then qualifier.Split('.') |> Array.last
            else qualifier

        let sym =
            doc.Symbols
            |> List.tryFind (fun s -> s.Name = qualifier || s.Name = normalized)

        let tryFromTypeName (typeName: string) =
            doc.Symbols
            |> List.tryFind (fun s -> s.Kind = 5 && s.Name = typeName)
            |> Option.bind (fun s -> s.TypeText)
            |> Option.bind tryParseRecordFields

        match sym with
        | None -> None
        | Some s ->
            match s.TypeText |> Option.bind tryParseRecordFields with
            | Some fields -> Some fields
            | None ->
                match s.TypeTargetName with
                | Some typeName ->
                    tryFromTypeName typeName
                | None ->
                    s.TypeText |> Option.bind tryFromTypeName
        |> function
            | Some fields -> Some fields
            | None -> doc.RecordParameterFields |> Map.tryFind normalized

    let private tryResolveNamedRecordTypeByFields (doc: DocumentState) (fields: (string * string) list) =
        let wanted = canonicalRecordSignatureFromFields fields
        let candidates =
            doc.Symbols
            |> List.choose (fun s ->
                if s.Kind = 5 then
                    s.TypeText
                    |> Option.bind tryParseRecordFields
                    |> Option.map (fun typeFields -> s.Name, canonicalRecordSignatureFromFields typeFields)
                else
                    None)
            |> List.choose (fun (name, signature) -> if signature = wanted then Some name else None)
            |> List.distinct
        match candidates with
        | [ one ] -> Some one
        | _ -> None

    let private tryResolveTypeNameForQualifier (doc: DocumentState) (qualifier: string) =
        let normalized =
            if qualifier.Contains('.') then qualifier.Split('.') |> Array.last
            else qualifier

        let fromSymbol =
            doc.Symbols
            |> List.tryFind (fun s -> s.Name = qualifier || s.Name = normalized)
            |> Option.bind (fun s ->
                match s.TypeTargetName with
                | Some t -> Some t
                | None ->
                    s.TypeText
                    |> Option.bind tryParseRecordFields
                    |> Option.bind (tryResolveNamedRecordTypeByFields doc))

        match fromSymbol with
        | Some typeName -> Some typeName
        | None -> doc.ParameterTypeTargets |> Map.tryFind normalized

    let private tryFindSymbolByName (doc: DocumentState) (name: string) =
        let normalized =
            if name.Contains('.') then name.Split('.') |> Array.last
            else name
        doc.Symbols
        |> List.tryFind (fun s -> s.Name = name || s.Name = normalized)

    let private tryResolveTypeNameFromSymbol (doc: DocumentState) (symbol: TopLevelSymbol) =
        match symbol.TypeTargetName with
        | Some typeName -> Some typeName
        | None ->
            symbol.TypeText
            |> Option.bind tryParseRecordFields
            |> Option.bind (tryResolveNamedRecordTypeByFields doc)

    let private trimWrappingParens (input: string) =
        let mutable value = input.Trim()
        let mutable changed = true
        while changed && value.Length >= 2 && value[0] = '(' && value[value.Length - 1] = ')' do
            changed <- false
            let mutable depth = 0
            let mutable wraps = true
            for i = 0 to value.Length - 1 do
                match value[i] with
                | '(' -> depth <- depth + 1
                | ')' ->
                    depth <- depth - 1
                    if depth = 0 && i < value.Length - 1 then
                        wraps <- false
                | _ -> ()
            if wraps then
                value <- value.Substring(1, value.Length - 2).Trim()
                changed <- true
        value

    let private tryFirstArgumentTypeNameFromFunctionSymbol (doc: DocumentState) (symbol: TopLevelSymbol) =
        let tryResolveTypeName (t: string) =
            let trimmed = trimWrappingParens t
            match doc.Symbols |> List.tryFind (fun s -> s.Kind = 5 && s.Name = trimmed) with
            | Some _ -> Some trimmed
            | None ->
                tryParseRecordFields trimmed
                |> Option.bind (tryResolveNamedRecordTypeByFields doc)

        symbol.TypeText
        |> Option.bind (fun t ->
            let normalizedTypeText = trimWrappingParens t
            let arrowIndex = normalizedTypeText.IndexOf("->", StringComparison.Ordinal)
            if arrowIndex <= 0 then None
            else
                let firstArg = normalizedTypeText.Substring(0, arrowIndex).Trim()
                tryResolveTypeName firstArg)

    let private tryResolveRecordLiteralCallArgTypeTarget (doc: DocumentState) (line: int) (character: int) =
        match getLineText doc.Text line with
        | Some lineText ->
            let pos = max 0 (min character lineText.Length)
            let leftBrace = lineText.LastIndexOf('{', max 0 (pos - 1))
            let rightBrace = if pos < lineText.Length then lineText.IndexOf('}', pos) else -1
            if leftBrace < 0 || rightBrace <= leftBrace then
                None
            else
                let segment = lineText.Substring(leftBrace, rightBrace - leftBrace + 1)
                if not (segment.Contains('='))
                   || segment.Contains(':') then
                    None
                else
                    let callPrefix = lineText.Substring(0, leftBrace).TrimEnd()
                    if String.IsNullOrWhiteSpace(callPrefix) then
                        None
                    else
                        let mutable finish = callPrefix.Length - 1
                        while finish >= 0 && Char.IsWhiteSpace(callPrefix[finish]) do
                            finish <- finish - 1
                        let mutable start = finish
                        while start >= 0 && isWordChar callPrefix[start] do
                            start <- start - 1
                        let tokenStart = start + 1
                        if tokenStart > finish then
                            None
                        else
                            let callTarget = callPrefix.Substring(tokenStart, finish - tokenStart + 1)
                            tryFindSymbolByName doc callTarget
                            |> Option.bind (tryFirstArgumentTypeNameFromFunctionSymbol doc)
        | None -> None

    let private tryResolveRecordLiteralBindingTypeTarget (doc: DocumentState) (line: int) (character: int) =
        match getLineText doc.Text line with
        | Some lineText ->
            let pos = max 0 (min character lineText.Length)
            let leftBrace = lineText.LastIndexOf('{', max 0 (pos - 1))
            let rightBrace = if pos < lineText.Length then lineText.IndexOf('}', pos) else -1
            if leftBrace < 0 || rightBrace <= leftBrace then
                None
            else
                let segment = lineText.Substring(leftBrace, rightBrace - leftBrace + 1)
                if not (segment.Contains('='))
                   || segment.Contains(':') then
                    None
                else
                    let prefix = lineText.Substring(0, leftBrace).TrimEnd()
                    let eqIndex = prefix.LastIndexOf('=')
                    if eqIndex <= 0 then
                        None
                    else
                        let lhs = prefix.Substring(0, eqIndex).Trim()
                        if not (lhs.StartsWith("let ", StringComparison.Ordinal)) then
                            None
                        else
                            let afterLet = lhs.Substring(4).Trim()
                            if String.IsNullOrWhiteSpace(afterLet) then
                                None
                            else
                                let mutable idx = 0
                                while idx < afterLet.Length && isWordChar afterLet[idx] do
                                    idx <- idx + 1
                                if idx = 0 then
                                    None
                                else
                                    let bindingName = afterLet.Substring(0, idx)
                                    tryFindSymbolByName doc bindingName
                                    |> Option.bind (tryResolveTypeNameFromSymbol doc)
        | None -> None

    let private tryExtractInlineRecordAnnotationAtPosition (doc: DocumentState) (line: int) (character: int) =
        match getLineText doc.Text line with
        | None -> None
        | Some lineText ->
            let pos = max 0 (min character lineText.Length)
            let left = lineText.LastIndexOf('{', max 0 (pos - 1))
            let right = if pos < lineText.Length then lineText.IndexOf('}', pos) else -1
            if left < 0 || right <= left then
                None
            else
                let segment = lineText.Substring(left, right - left + 1)
                if segment.Contains(':', StringComparison.Ordinal) && not (segment.Contains('=', StringComparison.Ordinal)) then
                    tryParseRecordFields segment
                else
                    None

    let tryResolveTypeTargetAtPosition (doc: DocumentState) (line: int) (character: int) : string option =
        let fromWord =
            match tryGetWordAtPosition doc.Text line character with
            | Some word when word.Contains('.') ->
                let idx = word.LastIndexOf('.')
                if idx > 0 then
                    let qualifier = word.Substring(0, idx)
                    tryResolveTypeNameForQualifier doc qualifier
                else
                    None
            | Some word ->
                tryResolveTypeNameForQualifier doc word
            | None -> None

        match fromWord with
        | Some typeName -> Some typeName
        | None ->
            match tryExtractInlineRecordAnnotationAtPosition doc line character with
            | Some fields ->
                tryResolveNamedRecordTypeByFields doc fields
            | None ->
                match tryResolveRecordLiteralCallArgTypeTarget doc line character with
                | Some t -> Some t
                | None -> tryResolveRecordLiteralBindingTypeTarget doc line character

    let tryGetRecordFieldHoverInfo (doc: DocumentState) (line: int) (character: int) : (string * string) option =
        match tryGetWordAtPosition doc.Text line character with
        | Some word when word.Contains('.') ->
            let idx = word.LastIndexOf('.')
            if idx <= 0 || idx + 1 >= word.Length then
                None
            else
                let qualifier = word.Substring(0, idx)
                let fieldName = word.Substring(idx + 1)
                tryRecordFieldsForQualifier doc qualifier
                |> Option.bind (fun fields ->
                    fields
                    |> List.tryFind (fun (name, _) -> name = fieldName))
        | _ ->
            None

    let private tryMemberCompletionItems (doc: DocumentState) (prefix: string option) =
        match prefix with
        | Some p when p.Contains('.') ->
            let idx = p.LastIndexOf('.')
            if idx <= 0 then None
            else
                let qualifier = p.Substring(0, idx)
                let memberPrefix =
                    if idx + 1 < p.Length then p.Substring(idx + 1) else ""

                match tryRecordFieldsForQualifier doc qualifier with
                | Some fields ->
                    let filtered =
                        fields
                        |> List.filter (fun (name, _) ->
                            memberPrefix = "" || name.StartsWith(memberPrefix, StringComparison.Ordinal))

                    let items =
                        filtered
                        |> List.map (fun (name, fieldType) ->
                            let item = JsonObject()
                            item["label"] <- JsonValue.Create(name)
                            item["kind"] <- JsonValue.Create(10)
                            item["detail"] <- JsonValue.Create(fieldType)
                            item["filterText"] <- JsonValue.Create(name)
                            item["sortText"] <- JsonValue.Create($"0_{name}")
                            if memberPrefix = name then
                                item["preselect"] <- JsonValue.Create(true)
                            item :> JsonNode)
                        |> List.toArray

                    Some (JsonArray(items))
                | None ->
                    None
        | _ -> None

    let makeCompletionItems (doc: DocumentState) (prefix: string option) =
        match tryMemberCompletionItems doc prefix with
        | Some memberItems -> memberItems
        | None ->
            let symbols = doc.Symbols
            let keywords =
                [ "let"; "rec"; "and"; "if"; "then"; "elif"; "else"; "match"; "with"; "when"
                  "for"; "in"; "do"; "type"; "module"; "true"; "false"; "None"; "Some" ]

            let keywordItems =
                keywords
                |> List.map (fun kw ->
                    let item = JsonObject()
                    item["label"] <- JsonValue.Create(kw)
                    item["kind"] <- JsonValue.Create(14)
                    item["filterText"] <- JsonValue.Create(kw)
                    item["sortText"] <- JsonValue.Create($"9_{kw}")
                    match prefix with
                    | Some p when p = kw -> item["preselect"] <- JsonValue.Create(true)
                    | _ -> ()
                    item)

            let namePool =
                [ for s in symbols -> s.Name
                  yield! stdlibNames
                  yield! builtinNames ]
                |> List.distinct

            let filteredNames =
                namePool
                |> List.filter (fun name ->
                    match prefix with
                    | Some p when p <> "" ->
                        if p.Contains('.') then
                            // When user types a dotted qualifier, avoid flooding with unrelated names.
                            name.StartsWith(p, StringComparison.Ordinal)
                        else
                            name.StartsWith(p, StringComparison.Ordinal)
                    | _ -> true)
                |> List.sortBy (fun name ->
                    let localPriority =
                        if name.Contains('.') then 1 else 0
                    (localPriority, name.Length, name))

            let symbolItems =
                filteredNames
                |> List.map (fun name ->
                    let symbolType = symbols |> List.tryFind (fun s -> s.Name = name) |> Option.bind (fun s -> s.TypeText)
                    let kind =
                        match symbols |> List.tryFind (fun s -> s.Name = name) with
                        | Some s -> s.Kind
                        | None -> 3
                    let item = JsonObject()
                    item["label"] <- JsonValue.Create(name)
                    item["kind"] <- JsonValue.Create(kind)
                    item["filterText"] <- JsonValue.Create(name)
                    let sortPrefix = if name.Contains('.') then "1" else "0"
                    item["sortText"] <- JsonValue.Create($"{sortPrefix}_{name}")
                    match prefix with
                    | Some p when p = name -> item["preselect"] <- JsonValue.Create(true)
                    | _ -> ()
                    match symbolType with
                    | Some t -> item["detail"] <- JsonValue.Create(t)
                    | None -> ()
                    item)

            let rankedItems =
                match prefix with
                | Some p when p <> "" ->
                    let keywordMatches =
                        keywordItems
                        |> List.filter (fun item ->
                            match item["label"] with
                            | :? JsonValue as label ->
                                try
                                    let kw = label.GetValue<string>()
                                    if p.Contains('.') then false
                                    else kw.StartsWith(p, StringComparison.Ordinal)
                                with _ -> false
                            | _ -> false)
                    symbolItems @ keywordMatches
                | _ ->
                    keywordItems @ symbolItems

            let nodes = rankedItems |> List.map (fun n -> n :> JsonNode)
            JsonArray(nodes |> List.toArray)

    let tryGetCallTargetPrefixAtPosition (text: string) (line: int) (character: int) : string option =
        match getLineText text line with
        | None -> None
        | Some lineText ->
            if lineText.Length = 0 then None
            else
                let pos = max 0 (min character lineText.Length)
                let mutable idx = pos - 1

                while idx >= 0 && Char.IsWhiteSpace(lineText[idx]) do
                    idx <- idx - 1

                if idx < 0 then None
                else
                    let mutable finish = idx + 1
                    let mutable start = idx

                    while start >= 0 && isWordChar lineText[start] do
                        start <- start - 1

                    let tokenStart = start + 1
                    if tokenStart < finish then Some(lineText.Substring(tokenStart, finish - tokenStart)) else None

    let findSymbolRangesInText (text: string) (candidateNames: string list) =
        let names = candidateNames |> List.distinct |> Set.ofList
        let lines = text.Split('\n')

        lines
        |> Array.mapi (fun lineIndex rawLine ->
            let line = rawLine.TrimEnd('\r')
            let mutable i = 0
            let found = ResizeArray<Span>()

            while i < line.Length do
                if isWordChar line[i] then
                    let start = i
                    let mutable j = i + 1
                    while j < line.Length && isWordChar line[j] do
                        j <- j + 1

                    let token = line.Substring(start, j - start)
                    if names.Contains token then
                        let span =
                            Span.mk
                                (Span.pos (lineIndex + 1) (start + 1))
                                (Span.pos (lineIndex + 1) (j + 1))
                        found.Add(span)

                    i <- j
                else
                    i <- i + 1

            found |> Seq.toList)
        |> Array.toList
        |> List.concat
