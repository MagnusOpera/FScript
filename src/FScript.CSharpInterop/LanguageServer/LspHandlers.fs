namespace FScript.LanguageServer

open System
open System.IO
open System.Text.Json.Nodes
open FScript.Language
open FScript.CSharpInterop

module LspHandlers =
    open LspModel
    open LspSymbols

    let handleInitialize (idNode: JsonNode) (paramsObj: JsonObject option) =
        match paramsObj with
        | Some p ->
            match tryGetObject p "initializationOptions" with
            | Some init ->
                match init["inlayHintsEnabled"] with
                | :? JsonValue as v ->
                    try inlayHintsEnabled <- v.GetValue<bool>() with _ -> ()
                | _ -> ()
            | None -> ()
        | None -> ()

        let sync = JsonObject()
        sync["openClose"] <- JsonValue.Create(true)
        sync["change"] <- JsonValue.Create(1)

        let completionProvider = JsonObject()
        completionProvider["resolveProvider"] <- JsonValue.Create(false)
        let triggerChars = JsonArray()
        triggerChars.Add(JsonValue.Create("."))
        triggerChars.Add(JsonValue.Create("["))
        completionProvider["triggerCharacters"] <- triggerChars

        let serverInfo = JsonObject()
        serverInfo["name"] <- JsonValue.Create("FScript Language Server")

        let capabilities = JsonObject()
        capabilities["textDocumentSync"] <- sync
        capabilities["completionProvider"] <- completionProvider
        capabilities["hoverProvider"] <- JsonValue.Create(true)
        capabilities["definitionProvider"] <- JsonValue.Create(true)
        capabilities["typeDefinitionProvider"] <- JsonValue.Create(true)
        capabilities["referencesProvider"] <- JsonValue.Create(true)
        capabilities["documentHighlightProvider"] <- JsonValue.Create(true)
        let renameProvider = JsonObject()
        renameProvider["prepareProvider"] <- JsonValue.Create(true)
        capabilities["renameProvider"] <- renameProvider
        let signatureHelpProvider = JsonObject()
        let signatureTriggers = JsonArray()
        signatureTriggers.Add(JsonValue.Create("("))
        signatureTriggers.Add(JsonValue.Create(","))
        signatureHelpProvider["triggerCharacters"] <- signatureTriggers
        capabilities["signatureHelpProvider"] <- signatureHelpProvider
        capabilities["documentSymbolProvider"] <- JsonValue.Create(true)
        capabilities["workspaceSymbolProvider"] <- JsonValue.Create(true)
        capabilities["codeActionProvider"] <- JsonValue.Create(true)
        capabilities["inlayHintProvider"] <- JsonValue.Create(true)
        let semanticLegend = JsonObject()
        let tokenTypeNodes = JsonArray()
        [| "keyword"; "string"; "number"; "function"; "type"; "variable" |]
        |> Array.iter (fun s -> tokenTypeNodes.Add(JsonValue.Create(s)))
        semanticLegend["tokenTypes"] <- tokenTypeNodes
        semanticLegend["tokenModifiers"] <- JsonArray()
        let semanticProvider = JsonObject()
        semanticProvider["legend"] <- semanticLegend
        semanticProvider["full"] <- JsonValue.Create(true)
        capabilities["semanticTokensProvider"] <- semanticProvider

        let result = JsonObject()
        result["capabilities"] <- capabilities
        result["serverInfo"] <- serverInfo

        LspProtocol.sendResponse idNode (Some result)

    let private keywordSet =
        [ "let"; "rec"; "and"; "if"; "then"; "elif"; "else"; "match"; "with"; "when"
          "for"; "in"; "do"; "type"; "module"; "true"; "false"; "None"; "Some"
          "fun"; "raise"; "import"; "export"; "qualified" ]
        |> Set.ofList

    let private classifyToken (line: string) (startIndex: int) (token: string) =
        let isFunctionCallToken () =
            let mutable i = startIndex + token.Length
            while i < line.Length && Char.IsWhiteSpace(line[i]) do
                i <- i + 1
            i < line.Length && line[i] = '('

        if keywordSet.Contains(token) then 0
        elif token.Length > 1 && token.StartsWith("\"") && token.EndsWith("\"") then 1
        elif token |> Seq.forall Char.IsDigit then 2
        elif isFunctionCallToken () then 3
        elif token.Contains('.') then
            let tail = token.Split('.') |> Array.last
            if tail.Length > 0 && Char.IsLower(tail[0]) then 3 else 4
        elif token.Length > 0 && Char.IsUpper(token[0]) then 4
        else 5

    let private scanSemanticTokens (text: string) =
        let lines = text.Split('\n')
        let mutable previousLine = 0
        let mutable previousStart = 0
        let data = ResizeArray<int>()

        for lineIndex = 0 to lines.Length - 1 do
            let line = lines[lineIndex].TrimEnd('\r')
            let mutable i = 0
            while i < line.Length do
                let c = line[i]
                if Char.IsWhiteSpace(c) then
                    i <- i + 1
                elif c = '/' && i + 1 < line.Length && line[i + 1] = '/' then
                    i <- line.Length
                elif c = '"' then
                    let start = i
                    i <- i + 1
                    while i < line.Length && line[i] <> '"' do
                        i <- i + 1
                    if i < line.Length then i <- i + 1
                    let length = i - start
                    let deltaLine = lineIndex - previousLine
                    let deltaStart = if deltaLine = 0 then start - previousStart else start
                    data.Add(deltaLine)
                    data.Add(deltaStart)
                    data.Add(length)
                    data.Add(1)
                    data.Add(0)
                    previousLine <- lineIndex
                    previousStart <- start
                elif Char.IsLetter(c) || c = '_' then
                    let start = i
                    i <- i + 1
                    while i < line.Length && (Char.IsLetterOrDigit(line[i]) || line[i] = '_' || line[i] = '.') do
                        i <- i + 1
                    let token = line.Substring(start, i - start)
                    let tokenType = classifyToken line start token
                    let deltaLine = lineIndex - previousLine
                    let deltaStart = if deltaLine = 0 then start - previousStart else start
                    data.Add(deltaLine)
                    data.Add(deltaStart)
                    data.Add(token.Length)
                    data.Add(tokenType)
                    data.Add(0)
                    previousLine <- lineIndex
                    previousStart <- start
                elif Char.IsDigit(c) then
                    let start = i
                    i <- i + 1
                    while i < line.Length && (Char.IsDigit(line[i]) || line[i] = '.') do
                        i <- i + 1
                    let length = i - start
                    let deltaLine = lineIndex - previousLine
                    let deltaStart = if deltaLine = 0 then start - previousStart else start
                    data.Add(deltaLine)
                    data.Add(deltaStart)
                    data.Add(length)
                    data.Add(2)
                    data.Add(0)
                    previousLine <- lineIndex
                    previousStart <- start
                else
                    i <- i + 1

        data

    let handleSemanticTokens (idNode: JsonNode) (paramsObj: JsonObject) =
        match tryGetUriFromTextDocument paramsObj with
        | Some uri when documents.ContainsKey(uri) ->
            let doc = documents[uri]
            let data: JsonNode array =
                scanSemanticTokens doc.Text
                |> Seq.map (fun n -> JsonValue.Create(n) :> JsonNode)
                |> Seq.toArray
            let result = JsonObject()
            result["data"] <- JsonArray(data)
            LspProtocol.sendResponse idNode (Some result)
        | _ ->
            let result = JsonObject()
            result["data"] <- JsonArray()
            LspProtocol.sendResponse idNode (Some result)

    let private positionInRange (line: int) (character: int) (sl: int, sc: int, el: int, ec: int) =
        let afterStart = line > sl || (line = sl && character >= sc)
        let beforeEnd = line < el || (line = el && character <= ec)
        afterStart && beforeEnd

    let private trimOuterParens (text: string) =
        let rec trim (value: string) =
            let trimmed = value.Trim()
            if trimmed.Length >= 2 && trimmed[0] = '(' && trimmed[trimmed.Length - 1] = ')' then
                let mutable depth = 0
                let mutable enclosesAll = true
                let mutable i = 0
                while i < trimmed.Length && enclosesAll do
                    let c = trimmed[i]
                    if c = '(' then depth <- depth + 1
                    elif c = ')' then
                        depth <- depth - 1
                        if depth = 0 && i < trimmed.Length - 1 then
                            enclosesAll <- false
                    i <- i + 1
                if enclosesAll then trim (trimmed.Substring(1, trimmed.Length - 2))
                else trimmed
            else
                trimmed
        trim text

    let private splitTopLevelArrows (typeText: string) =
        let typeText = trimOuterParens typeText
        let parts = ResizeArray<string>()
        let mutable depthParen = 0
        let mutable depthBrace = 0
        let mutable depthBracket = 0
        let mutable start = 0
        let mutable i = 0
        while i < typeText.Length do
            let c = typeText[i]
            match c with
            | '(' -> depthParen <- depthParen + 1
            | ')' when depthParen > 0 -> depthParen <- depthParen - 1
            | '{' -> depthBrace <- depthBrace + 1
            | '}' when depthBrace > 0 -> depthBrace <- depthBrace - 1
            | '[' -> depthBracket <- depthBracket + 1
            | ']' when depthBracket > 0 -> depthBracket <- depthBracket - 1
            | '-' when i + 1 < typeText.Length && typeText[i + 1] = '>' && depthParen = 0 && depthBrace = 0 && depthBracket = 0 ->
                let chunk = typeText.Substring(start, i - start).Trim()
                if chunk <> "" then
                    parts.Add(chunk)
                i <- i + 1
                start <- i + 1
            | _ -> ()
            i <- i + 1

        if start <= typeText.Length then
            let tail = typeText.Substring(start).Trim()
            if tail <> "" then
                parts.Add(tail)

        parts |> Seq.toList

    let private flattenArrowParts (typeText: string) =
        let rec flatten (text: string) =
            let parts = splitTopLevelArrows text
            match parts with
            | [] -> []
            | [ single ] ->
                let trimmed = trimOuterParens single
                if String.Equals(trimmed, single, StringComparison.Ordinal) then
                    [ trimmed ]
                else
                    flatten trimmed
            | first :: rest when rest.Length = 1 ->
                first :: flatten rest[0]
            | _ ->
                parts
        flatten typeText

    let private formatNamedArrowSignature (names: string list) (typeText: string) =
        let parts = flattenArrowParts typeText
        if parts.Length = (names.Length + 1) then
            let args =
                [ 0 .. names.Length - 1 ]
                |> List.map (fun i -> $"({names[i]}: {parts[i]})")
            String.concat " -> " (args @ [ parts[parts.Length - 1] ])
        else
            typeText

    let private displayTypeName (doc: DocumentState) (name: string) =
        if String.IsNullOrWhiteSpace(name) then name
        else
            let parts = name.Split('.') |> Array.toList
            match parts with
            | head :: tail when doc.ImportInternalToAlias.ContainsKey(head) ->
                let alias = doc.ImportInternalToAlias[head]
                match tail with
                | [] -> alias
                | _ -> alias + "." + (String.concat "." tail)
            | _ -> name

    let private formatFunctionSignature (doc: DocumentState) (sym: TopLevelSymbol) =
        let paramNames = doc.FunctionParameters |> Map.tryFind sym.Name |> Option.defaultValue []

        match sym.TypeText with
        | Some typeText when sym.Kind = 12 && not paramNames.IsEmpty ->
            let parts = flattenArrowParts typeText
            if parts.Length = (paramNames.Length + 1) then
                let effectiveParts =
                    match sym.TypeTargetName with
                    | Some returnName when parts.Length > 0 ->
                        (parts |> List.take (parts.Length - 1)) @ [ displayTypeName doc returnName ]
                    | _ -> parts
                let arrowText =
                    if effectiveParts.Length = (paramNames.Length + 1) then
                        let args =
                            [ 0 .. paramNames.Length - 1 ]
                            |> List.map (fun i -> $"({paramNames[i]}: {effectiveParts[i]})")
                        String.concat " -> " (args @ [ effectiveParts[effectiveParts.Length - 1] ])
                    else
                        String.concat " -> " effectiveParts
                $"{sym.Name}: {arrowText}"
            else
                $"{sym.Name} : {typeText}"
        | Some typeText ->
            $"{sym.Name} : {typeText}"
        | None when sym.Kind = 12 && not paramNames.IsEmpty ->
            match doc.FunctionAnnotationTypes |> Map.tryFind sym.Name with
            | Some annotated when annotated.Length = paramNames.Length ->
                let returnType =
                    doc.FunctionDeclaredReturnTargets
                    |> Map.tryFind sym.Name
                    |> Option.defaultValue "unknown"
                    |> displayTypeName doc
                let parts = annotated @ [ returnType ]
                let arrowText =
                    if parts.Length = (paramNames.Length + 1) then
                        let args =
                            [ 0 .. paramNames.Length - 1 ]
                            |> List.map (fun i -> $"({paramNames[i]}: {parts[i]})")
                        String.concat " -> " (args @ [ parts[parts.Length - 1] ])
                    else
                        String.concat " -> " parts
                $"{sym.Name}: {arrowText}"
            | _ ->
                let args = paramNames |> List.map (fun name -> $"({name})") |> String.concat " "
                $"{sym.Name} {args}"
        | None ->
            sym.Name

    let private formatInjectedFunctionSignature (doc: DocumentState) (name: string) (typeText: string) =
        match doc.InjectedFunctionParameterNames |> Map.tryFind name with
        | Some parameterNames when not parameterNames.IsEmpty ->
            let namedSignature = formatNamedArrowSignature parameterNames typeText
            $"{name}: {namedSignature}"
        | _ ->
            $"{name} : {typeText}"

    let handleInlayHints (idNode: JsonNode) (paramsObj: JsonObject) =
        if not inlayHintsEnabled then
            LspProtocol.sendResponse idNode (Some (JsonArray()))
        else
            match tryGetUriFromTextDocument paramsObj with
            | Some uri when documents.ContainsKey(uri) ->
                let doc = documents[uri]
                let (startLine, startChar, endLine, endChar) =
                    tryGetRange paramsObj |> Option.defaultValue (0, 0, Int32.MaxValue, Int32.MaxValue)
                let requireExplicitFileMatch = Path.IsPathRooted(doc.SourcePath)
                let spanInCurrentFile (span: Span) =
                    match span.Start.File with
                    | Some file -> String.Equals(file, doc.SourcePath, StringComparison.OrdinalIgnoreCase)
                    | None -> not requireExplicitFileMatch

                let hints = ResizeArray<JsonNode>()

                // Type hints for value bindings based on inferred top-level symbol types.
                doc.Symbols
                |> List.iter (fun sym ->
                    if sym.Kind = 13 && not (sym.Name.Contains('.')) then
                        match sym.TypeText with
                        | Some typeText ->
                            let hintLine = max 0 (sym.Span.End.Line - 1)
                            let hintChar = max 0 (sym.Span.End.Column - 1)
                            if spanInCurrentFile sym.Span
                               && positionInRange hintLine hintChar (startLine, startChar, endLine, endChar) then
                                let hint = JsonObject()
                                let pos = JsonObject()
                                pos["line"] <- JsonValue.Create(hintLine)
                                pos["character"] <- JsonValue.Create(hintChar)
                                hint["position"] <- pos
                                hint["label"] <- JsonValue.Create($": {typeText}")
                                hint["kind"] <- JsonValue.Create(1)
                                hint["paddingLeft"] <- JsonValue.Create(true)
                                hints.Add(hint :> JsonNode)
                        | None -> ())

                // Type hints for function/lambda parameters inferred by the typechecker.
                doc.ParameterTypeHints
                |> List.iter (fun (span, label) ->
                    let hintLine = max 0 (span.End.Line - 1)
                    let hintChar = max 0 (span.End.Column - 1)
                    if spanInCurrentFile span
                       && positionInRange hintLine hintChar (startLine, startChar, endLine, endChar) then
                        let hint = JsonObject()
                        let pos = JsonObject()
                        pos["line"] <- JsonValue.Create(hintLine)
                        pos["character"] <- JsonValue.Create(hintChar)
                        hint["position"] <- pos
                        hint["label"] <- JsonValue.Create(label)
                        hint["kind"] <- JsonValue.Create(1)
                        hint["paddingLeft"] <- JsonValue.Create(true)
                        hints.Add(hint :> JsonNode))

                // Return type hints for function declarations.
                doc.FunctionReturnTypeHints
                |> List.iter (fun (span, label) ->
                    let hintLine = max 0 (span.End.Line - 1)
                    let hintChar = max 0 (span.End.Column - 1)
                    if spanInCurrentFile span
                       && positionInRange hintLine hintChar (startLine, startChar, endLine, endChar) then
                        let hint = JsonObject()
                        let pos = JsonObject()
                        pos["line"] <- JsonValue.Create(hintLine)
                        pos["character"] <- JsonValue.Create(hintChar)
                        hint["position"] <- pos
                        hint["label"] <- JsonValue.Create(label)
                        hint["kind"] <- JsonValue.Create(1)
                        hint["paddingLeft"] <- JsonValue.Create(true)
                        hints.Add(hint :> JsonNode))

                // Type hints for pattern-bound variables (for example: `Some x`).
                doc.PatternTypeHints
                |> List.iter (fun (span, label) ->
                    let hintLine = max 0 (span.End.Line - 1)
                    let hintChar = max 0 (span.End.Column - 1)
                    if spanInCurrentFile span
                       && positionInRange hintLine hintChar (startLine, startChar, endLine, endChar) then
                        let hint = JsonObject()
                        let pos = JsonObject()
                        pos["line"] <- JsonValue.Create(hintLine)
                        pos["character"] <- JsonValue.Create(hintChar)
                        hint["position"] <- pos
                        hint["label"] <- JsonValue.Create(label)
                        hint["kind"] <- JsonValue.Create(1)
                        hint["paddingLeft"] <- JsonValue.Create(true)
                        hints.Add(hint :> JsonNode))

                doc.CallArgumentHints
                |> List.iter (fun (span, label) ->
                    let hintLine = max 0 (span.Start.Line - 1)
                    let hintChar = max 0 (span.Start.Column - 1)
                    if spanInCurrentFile span
                       && positionInRange hintLine hintChar (startLine, startChar, endLine, endChar) then
                        let hint = JsonObject()
                        let pos = JsonObject()
                        pos["line"] <- JsonValue.Create(hintLine)
                        pos["character"] <- JsonValue.Create(hintChar)
                        hint["position"] <- pos
                        hint["label"] <- JsonValue.Create(label)
                        hint["kind"] <- JsonValue.Create(2)
                        hint["paddingRight"] <- JsonValue.Create(true)
                        hints.Add(hint :> JsonNode))

                LspProtocol.sendResponse idNode (Some (JsonArray(hints.ToArray())))
            | _ ->
                LspProtocol.sendResponse idNode (Some (JsonArray()))

    let handleDidOpen (paramsObj: JsonObject) =
        match tryGetObject paramsObj "textDocument" with
        | Some textDocument ->
            match tryGetString textDocument "uri", tryGetString textDocument "text" with
            | Some uri, Some text -> analyzeDocument uri text
            | _ -> ()
        | None -> ()

    let handleDidChange (paramsObj: JsonObject) =
        match tryGetUriFromTextDocument paramsObj with
        | None -> ()
        | Some uri ->
            match paramsObj["contentChanges"] with
            | :? JsonArray as changes ->
                let mutable latest: string option = None
                for change in changes do
                    match change with
                    | :? JsonObject as changeObj ->
                        match tryGetString changeObj "text" with
                        | Some text -> latest <- Some text
                        | None -> ()
                    | _ -> ()

                match latest with
                | Some text -> analyzeDocument uri text
                | None -> ()
            | _ -> ()

    let handleDidClose (paramsObj: JsonObject) =
        match tryGetUriFromTextDocument paramsObj with
        | Some uri ->
            documents.Remove(uri) |> ignore
            publishDiagnostics uri []
        | None -> ()

    let private tryGetCommandUri (paramsObj: JsonObject) =
        match tryGetUriFromTextDocument paramsObj with
        | Some uri -> Some uri
        | None -> tryGetString paramsObj "uri"

    let private sendCommandError (idNode: JsonNode) (kind: string) (message: string) =
        let errorObj = JsonObject()
        errorObj["message"] <- JsonValue.Create(message)
        errorObj["kind"] <- JsonValue.Create(kind)
        let response = JsonObject()
        response["ok"] <- JsonValue.Create(false)
        response["error"] <- errorObj
        LspProtocol.sendResponse idNode (Some response)

    let private tryLoadSourceForUri (uri: string) =
        if documents.ContainsKey(uri) then
            Some documents[uri].Text
        else
            try
                let filePath = Uri(uri).LocalPath
                if File.Exists(filePath) then
                    Some (File.ReadAllText(filePath))
                else
                    None
            with _ ->
                None

    let handleViewAst (idNode: JsonNode) (paramsObj: JsonObject) =
        match tryGetCommandUri paramsObj with
        | None ->
            sendCommandError idNode "internal" "Missing document URI."
        | Some uri ->
            try
                let uriObj = Uri(uri)
                if not (String.Equals(uriObj.Scheme, "file", StringComparison.OrdinalIgnoreCase)) then
                    sendCommandError idNode "internal" "AST commands support file-based scripts only."
                else
                    let sourcePath = uriObj.LocalPath
                    match tryLoadSourceForUri uri with
                    | None ->
                        sendCommandError idNode "internal" $"Unable to read source file '{sourcePath}'."
                    | Some sourceText ->
                        let program = InteropServices.parseProgramFromSourceWithIncludes sourcePath sourceText
                        let response = JsonObject()
                        response["ok"] <- JsonValue.Create(true)
                        response["data"] <- AstJson.programToJson sourcePath program
                        LspProtocol.sendResponse idNode (Some response)
            with
            | :? ParseException as ex ->
                sendCommandError idNode "parse" ex.Message
            | ex ->
                sendCommandError idNode "internal" ex.Message

    let handleViewInferredAst (idNode: JsonNode) (paramsObj: JsonObject) =
        match tryGetCommandUri paramsObj with
        | None ->
            sendCommandError idNode "internal" "Missing document URI."
        | Some uri ->
            try
                let uriObj = Uri(uri)
                if not (String.Equals(uriObj.Scheme, "file", StringComparison.OrdinalIgnoreCase)) then
                    sendCommandError idNode "internal" "AST commands support file-based scripts only."
                else
                    let sourcePath = uriObj.LocalPath
                    match tryLoadSourceForUri uri with
                    | None ->
                        sendCommandError idNode "internal" $"Unable to read source file '{sourcePath}'."
                    | Some sourceText ->
                        let program = InteropServices.parseProgramFromSourceWithIncludes sourcePath sourceText
                        let runtimeExterns = LspRuntimeExterns.forSourcePath sourcePath
                        let typedProgram = InteropServices.inferProgramWithExterns runtimeExterns program
                        let response = JsonObject()
                        response["ok"] <- JsonValue.Create(true)
                        response["data"] <- AstJson.typedProgramToJson sourcePath typedProgram
                        LspProtocol.sendResponse idNode (Some response)
            with
            | :? ParseException as ex ->
                sendCommandError idNode "parse" ex.Message
            | :? TypeException as ex ->
                sendCommandError idNode "type" ex.Message
            | ex ->
                sendCommandError idNode "internal" ex.Message

    let handleHover (idNode: JsonNode) (paramsObj: JsonObject) =
        match tryGetUriFromTextDocument paramsObj, tryGetPosition paramsObj with
        | Some uri, Some (line, character) when documents.ContainsKey(uri) ->
            let doc = documents[uri]
            match tryGetRecordFieldHoverInfo doc line character with
            | Some (fieldName, fieldType) ->
                let contents = JsonObject()
                contents["kind"] <- JsonValue.Create("markdown")
                contents["value"] <- JsonValue.Create($"```fscript\n{fieldName} : {fieldType}\n```\nrecord-field")
                let result = JsonObject()
                result["contents"] <- contents
                LspProtocol.sendResponse idNode (Some result)
            | None ->
                match tryGetLocalVariableHoverInfo doc line character with
                | Some (name, typeText) ->
                    let contents = JsonObject()
                    contents["kind"] <- JsonValue.Create("markdown")
                    contents["value"] <- JsonValue.Create($"```fscript\n{name} : {typeText}\n```\nlocal-variable")
                    let result = JsonObject()
                    result["contents"] <- contents
                    LspProtocol.sendResponse idNode (Some result)
                | None ->
                    match tryResolveSymbol doc line character with
                    | Some sym ->
                        let signature = formatFunctionSignature doc sym

                        let contents = JsonObject()
                        contents["kind"] <- JsonValue.Create("markdown")
                        let kindLine = symbolKindLabel sym.Kind
                        let locationLine = $"defined at L{sym.Span.Start.Line}:C{sym.Span.Start.Column}"
                        contents["value"] <- JsonValue.Create($"```fscript\n{signature}\n```\n{kindLine}\n\n{locationLine}")

                        let result = JsonObject()
                        result["contents"] <- contents
                        LspProtocol.sendResponse idNode (Some result)
                    | None ->
                        match tryGetWordAtPosition doc.Text line character with
                        | Some word ->
                            let candidates =
                                if word.Contains('.') then
                                    [ word
                                      word.Split('.') |> Array.last ]
                                else
                                    [ word ]
                            let injectedMatch =
                                candidates
                                |> List.tryPick (fun candidate ->
                                    doc.InjectedFunctionSignatures
                                    |> Map.tryFind candidate
                                    |> Option.map (fun t -> candidate, t))
                            match injectedMatch with
                            | Some (name, typeText) ->
                                let contents = JsonObject()
                                contents["kind"] <- JsonValue.Create("markdown")
                                let signature = formatInjectedFunctionSignature doc name typeText
                                contents["value"] <- JsonValue.Create($"```fscript\n{signature}\n```\ninjected-function")
                                let result = JsonObject()
                                result["contents"] <- contents
                                LspProtocol.sendResponse idNode (Some result)
                            | None ->
                                LspProtocol.sendResponse idNode None
                        | None ->
                            LspProtocol.sendResponse idNode None
        | _ -> LspProtocol.sendResponse idNode None


    let private tryResolveIncludeLocation (sourceUri: string) (doc: DocumentState) (line: int) (character: int) : JsonObject option =
        match getLineText doc.Text line with
        | None -> None
        | Some lineText ->
            let trimmed = lineText.TrimStart()
            if not (trimmed.StartsWith("import", StringComparison.Ordinal)) then
                None
            else
                let firstQuote = lineText.IndexOf('"')
                if firstQuote < 0 then None
                else
                    let secondQuote = lineText.IndexOf('"', firstQuote + 1)
                    if secondQuote <= firstQuote then None
                    else
                        let insideLiteral = character >= (firstQuote + 1) && character <= secondQuote
                        if not insideLiteral then
                            None
                        else
                            let includePath = lineText.Substring(firstQuote + 1, secondQuote - firstQuote - 1)
                            if String.IsNullOrWhiteSpace(includePath) then
                                None
                            else
                                try
                                    let fullPath =
                                        if Path.IsPathRooted(includePath) then
                                            Path.GetFullPath(includePath)
                                        else
                                            if sourceUri.StartsWith("file://", StringComparison.OrdinalIgnoreCase) then
                                                let sourcePath = Uri(sourceUri).LocalPath
                                                match Path.GetDirectoryName(sourcePath) with
                                                | null -> Path.GetFullPath(includePath)
                                                | baseDir when String.IsNullOrWhiteSpace(baseDir) -> Path.GetFullPath(includePath)
                                                | baseDir -> Path.GetFullPath(Path.Combine(baseDir, includePath))
                                            else
                                                includePath

                                    if File.Exists(fullPath) then
                                        let loc = JsonObject()
                                        loc["uri"] <- JsonValue.Create(Uri(fullPath).AbsoluteUri)
                                        let startPos = Span.pos 1 1
                                        loc["range"] <- toLspRange (Span.mk startPos startPos)
                                        Some loc
                                    else
                                        None
                                with _ ->
                                    None

    let private tryUriFromSpanFile (fallbackUri: string) (span: Span) =
        match span.Start.File with
        | Some filePath when not (String.IsNullOrWhiteSpace(filePath)) ->
            try Some (Uri(filePath).AbsoluteUri) with _ -> Some fallbackUri
        | _ ->
            Some fallbackUri

    let handleDefinition (idNode: JsonNode) (paramsObj: JsonObject) =
        match tryGetUriFromTextDocument paramsObj, tryGetPosition paramsObj with
        | Some uri, Some (line, character) when documents.ContainsKey(uri) ->
            let doc = documents[uri]
            lastDefinitionRequest <- Some (uri, line, character, DateTime.UtcNow)
            match tryResolveIncludeLocation uri doc line character with
            | Some includeLoc ->
                LspProtocol.sendResponse idNode (Some includeLoc)
            | None ->
                match tryResolveLocalDefinitionAtPosition doc line character with
                | Some declSpan ->
                    let loc = JsonObject()
                    let targetUri =
                        tryUriFromSpanFile uri declSpan
                        |> Option.defaultValue uri
                    loc["uri"] <- JsonValue.Create(targetUri)
                    loc["range"] <- toLspRange declSpan
                    LspProtocol.sendResponse idNode (Some loc)
                | None ->
                    match tryResolveLocalBindingAtPosition doc line character with
                    | Some localBinding ->
                        let loc = JsonObject()
                        let targetUri =
                            tryUriFromSpanFile uri localBinding.DeclSpan
                            |> Option.defaultValue uri
                        loc["uri"] <- JsonValue.Create(targetUri)
                        loc["range"] <- toLspRange localBinding.DeclSpan
                        LspProtocol.sendResponse idNode (Some loc)
                    | None ->
                        match tryResolveLocalBindingFromRecordFieldAssignmentAtPosition doc line character with
                        | Some localBinding ->
                            let loc = JsonObject()
                            let targetUri =
                                tryUriFromSpanFile uri localBinding.DeclSpan
                                |> Option.defaultValue uri
                            loc["uri"] <- JsonValue.Create(targetUri)
                            loc["range"] <- toLspRange localBinding.DeclSpan
                            LspProtocol.sendResponse idNode (Some loc)
                        | None ->
                            let localSymbol = tryResolveSymbol doc line character
                            let wordAtCursor = tryGetWordAtOrAdjacentPosition doc.Text line character

                            let symbolAndUri =
                                match localSymbol with
                                | Some sym ->
                                    match tryUriFromSpanFile uri sym.Span with
                                    | Some targetUri -> Some (targetUri, sym)
                                    | None -> Some (uri, sym)
                                | None ->
                                    match wordAtCursor with
                                    | Some word ->
                                        documents
                                        |> Seq.tryPick (fun kv ->
                                            kv.Value.Symbols
                                            |> List.tryFind (fun s -> s.Name = word)
                                            |> Option.map (fun s -> kv.Key, s))
                                    | None -> None

                            match symbolAndUri with
                            | Some (targetUri, sym) ->
                                let loc = JsonObject()
                                loc["uri"] <- JsonValue.Create(targetUri)
                                loc["range"] <- toLspRange sym.Span
                                LspProtocol.sendResponse idNode (Some loc)
                            | None ->
                                let injectedDefinition =
                                    match wordAtCursor with
                                    | Some word ->
                                        let candidates =
                                            if word.Contains('.') then
                                                [ word; word.Split('.') |> Array.last ]
                                            else
                                                [ word ]

                                        candidates
                                        |> List.tryPick (fun candidate ->
                                            doc.InjectedFunctionDefinitions
                                            |> Map.tryFind candidate
                                            |> Option.map (fun target -> candidate, target))
                                    | None ->
                                        None

                                match injectedDefinition with
                                | Some (_, (targetUri, targetSpan)) ->
                                    let loc = JsonObject()
                                    loc["uri"] <- JsonValue.Create(targetUri)
                                    loc["range"] <- toLspRange targetSpan
                                    LspProtocol.sendResponse idNode (Some loc)
                                | None ->
                                    match tryResolveTypeTargetAtPosition doc line character with
                                    | Some typeName ->
                                        match doc.Symbols |> List.tryFind (fun s -> s.Kind = 5 && s.Name = typeName) with
                                        | Some typeSym ->
                                            let loc = JsonObject()
                                            let targetUri =
                                                tryUriFromSpanFile uri typeSym.Span
                                                |> Option.defaultValue uri
                                            loc["uri"] <- JsonValue.Create(targetUri)
                                            loc["range"] <- toLspRange typeSym.Span
                                            LspProtocol.sendResponse idNode (Some loc)
                                        | None ->
                                            match tryFindInjectedTypeDefinition typeName with
                                            | Some (targetUri, targetSpan) ->
                                                let loc = JsonObject()
                                                loc["uri"] <- JsonValue.Create(targetUri)
                                                loc["range"] <- toLspRange targetSpan
                                                LspProtocol.sendResponse idNode (Some loc)
                                            | None ->
                                                LspProtocol.sendResponse idNode None
                                    | None ->
                                        LspProtocol.sendResponse idNode None
        | _ ->
            LspProtocol.sendResponse idNode None

    let handleTypeDefinition (idNode: JsonNode) (paramsObj: JsonObject) =
        match tryGetUriFromTextDocument paramsObj, tryGetPosition paramsObj with
        | Some uri, Some (line, character) when documents.ContainsKey(uri) ->
            let doc = documents[uri]
            let wordAtCursor = tryGetWordAtPosition doc.Text line character
            let targetTypeName =
                let fromSymbol =
                    match tryResolveSymbol doc line character with
                    | Some sym ->
                        match sym.TypeTargetName with
                        | Some name -> Some name
                        | None ->
                            sym.TypeText
                            |> Option.bind (fun t ->
                                doc.Symbols
                                |> List.tryFind (fun s -> s.Kind = 5 && s.Name = t)
                                |> Option.map (fun s -> s.Name))
                    | None ->
                        None

                match fromSymbol with
                | Some _ -> fromSymbol
                | None -> tryResolveTypeTargetAtPosition doc line character

            match targetTypeName with
            | Some typeName ->
                match doc.Symbols |> List.tryFind (fun s -> s.Kind = 5 && s.Name = typeName) with
                | Some typeSym ->
                    let loc = JsonObject()
                    let targetUri =
                        tryUriFromSpanFile uri typeSym.Span
                        |> Option.defaultValue uri
                    loc["uri"] <- JsonValue.Create(targetUri)
                    loc["range"] <- toLspRange typeSym.Span
                    LspProtocol.sendResponse idNode (Some loc)
                | None ->
                    match tryFindInjectedTypeDefinition typeName with
                    | Some (targetUri, targetSpan) ->
                        let loc = JsonObject()
                        loc["uri"] <- JsonValue.Create(targetUri)
                        loc["range"] <- toLspRange targetSpan
                        LspProtocol.sendResponse idNode (Some loc)
                    | None ->
                        let injectedTypeDefinition =
                            match wordAtCursor with
                            | Some word ->
                                let candidates =
                                    if word.Contains('.') then
                                        [ word; word.Split('.') |> Array.last ]
                                    else
                                        [ word ]
                                candidates
                                |> List.tryPick tryFindInjectedTypeDefinition
                            | None ->
                                None

                        match injectedTypeDefinition with
                        | Some (targetUri, targetSpan) ->
                            let loc = JsonObject()
                            loc["uri"] <- JsonValue.Create(targetUri)
                            loc["range"] <- toLspRange targetSpan
                            LspProtocol.sendResponse idNode (Some loc)
                        | None ->
                            LspProtocol.sendResponse idNode None
            | None ->
                let injectedTypeDefinition =
                    match wordAtCursor with
                    | Some word ->
                        let candidates =
                            if word.Contains('.') then
                                [ word; word.Split('.') |> Array.last ]
                            else
                                [ word ]
                        candidates
                        |> List.tryPick tryFindInjectedTypeDefinition
                    | None ->
                        None

                match injectedTypeDefinition with
                | Some (targetUri, targetSpan) ->
                    let loc = JsonObject()
                    loc["uri"] <- JsonValue.Create(targetUri)
                    loc["range"] <- toLspRange targetSpan
                    LspProtocol.sendResponse idNode (Some loc)
                | None ->
                    LspProtocol.sendResponse idNode None
        | _ ->
            LspProtocol.sendResponse idNode None

    let handleCompletion (idNode: JsonNode) (paramsObj: JsonObject) =
        match tryGetUriFromTextDocument paramsObj with
        | Some uri when documents.ContainsKey(uri) ->
            let doc = documents[uri]
            let prefix =
                match tryGetPosition paramsObj with
                | Some (line, character) -> tryGetWordPrefixAtPosition doc.Text line character
                | None -> None

            let items = makeCompletionItems doc prefix
            let result = JsonObject()
            result["isIncomplete"] <- JsonValue.Create(false)
            result["items"] <- items
            LspProtocol.sendResponse idNode (Some result)
        | _ ->
            let result = JsonObject()
            result["isIncomplete"] <- JsonValue.Create(false)
            result["items"] <- JsonArray()
            LspProtocol.sendResponse idNode (Some result)

    let handleDocumentSymbol (idNode: JsonNode) (paramsObj: JsonObject) =
        match tryGetUriFromTextDocument paramsObj with
        | Some uri when documents.ContainsKey(uri) ->
            let symbols =
                documents[uri].Symbols
                |> List.map (fun s ->
                    let d = JsonObject()
                    d["name"] <- JsonValue.Create(s.Name)
                    d["kind"] <- JsonValue.Create(s.Kind)
                    d["range"] <- toLspRange s.Span
                    d["selectionRange"] <- toLspRange s.Span
                    d)

            let nodes = symbols |> List.map (fun s -> s :> JsonNode) |> List.toArray
            LspProtocol.sendResponse idNode (Some (JsonArray(nodes)))
        | _ -> LspProtocol.sendResponse idNode (Some (JsonArray()))

    let private resolveTargetNames (doc: DocumentState) line character =
        match tryResolveSymbol doc line character with
        | Some sym ->
            let normalized =
                if sym.Name.Contains('.') then sym.Name.Split('.') |> Array.last
                else sym.Name
            [ sym.Name; normalized ]
        | None ->
            match tryGetWordAtPosition doc.Text line character with
            | Some word ->
                let normalized =
                    if word.Contains('.') then word.Split('.') |> Array.last
                    else word
                [ word; normalized ]
            | None -> []

    let handleReferences (idNode: JsonNode) (paramsObj: JsonObject) =
        match tryGetUriFromTextDocument paramsObj, tryGetPosition paramsObj with
        | Some uri, Some (line, character) when documents.ContainsKey(uri) ->
            let doc = documents[uri]
            let shouldSuppressAsFollowup =
                match lastDefinitionRequest with
                | Some (lastUri, lastLine, lastChar, ts) ->
                    String.Equals(uri, lastUri, StringComparison.Ordinal)
                    && line = lastLine
                    && character = lastChar
                    && (DateTime.UtcNow - ts).TotalMilliseconds <= 1200.0
                | None ->
                    false

            if shouldSuppressAsFollowup then
                let declSpan =
                    match tryResolveLocalDefinitionAtPosition doc line character with
                    | Some span -> Some span
                    | None ->
                        match tryResolveLocalBindingAtPosition doc line character with
                        | Some binding -> Some binding.DeclSpan
                        | None ->
                            match tryResolveSymbol doc line character with
                            | Some sym -> Some sym.Span
                            | None ->
                                match tryGetWordAtOrAdjacentPosition doc.Text line character with
                                | Some word ->
                                    doc.Symbols
                                    |> List.tryFind (fun s -> s.Name = word)
                                    |> Option.map (fun s -> s.Span)
                                | None -> None

                match declSpan with
                | Some declSpan ->
                    let targetUri =
                        tryUriFromSpanFile uri declSpan
                        |> Option.defaultValue uri
                    let loc = JsonObject()
                    loc["uri"] <- JsonValue.Create(targetUri)
                    loc["range"] <- toLspRange declSpan
                    LspProtocol.sendResponse idNode (Some (JsonArray([| loc :> JsonNode |])))
                | None ->
                    LspProtocol.sendResponse idNode (Some (JsonArray()))
            else
                let targetNames = resolveTargetNames doc line character
                let includeDeclaration =
                    match tryGetObject paramsObj "context" with
                    | Some contextObj ->
                        match contextObj["includeDeclaration"] with
                        | :? JsonValue as v ->
                            try v.GetValue<bool>() with _ -> true
                        | _ -> true
                    | None -> true

                match targetNames with
                | head :: _ ->
                    let normalized =
                        if head.Contains('.') then head.Split('.') |> Array.last
                        else head

                    let declarationSpansByUri =
                        documents
                        |> Seq.map (fun kv ->
                            let docUri = kv.Key
                            let spans =
                                kv.Value.Symbols
                                |> List.choose (fun s ->
                                    let symbolNormalized =
                                        if s.Name.Contains('.') then s.Name.Split('.') |> Array.last
                                        else s.Name
                                    if s.Name = head || s.Name = normalized || symbolNormalized = normalized then
                                        Some s.Span
                                    else
                                        None)
                                |> Set.ofList
                            docUri, spans)
                        |> Map.ofSeq

                    let locations =
                        documents
                        |> Seq.collect (fun kv ->
                            let docUri = kv.Key
                            let candidateDoc = kv.Value
                            let fromOccurrences =
                                [ head; normalized ]
                                |> List.distinct
                                |> List.collect (fun n -> candidateDoc.VariableOccurrences |> Map.tryFind n |> Option.defaultValue [])
                                |> List.distinct

                            let spans =
                                if fromOccurrences.IsEmpty then
                                    findSymbolRangesInText candidateDoc.Text (targetNames @ [ normalized ])
                                else
                                    fromOccurrences

                            let filteredSpans =
                                if includeDeclaration then
                                    spans
                                else
                                    let decls = declarationSpansByUri |> Map.tryFind docUri |> Option.defaultValue Set.empty
                                    spans |> List.filter (fun span -> not (decls.Contains span))

                            filteredSpans
                            |> List.map (fun span ->
                                let loc = JsonObject()
                                loc["uri"] <- JsonValue.Create(docUri)
                                loc["range"] <- toLspRange span
                                loc :> JsonNode))
                        |> Seq.toArray

                    LspProtocol.sendResponse idNode (Some (JsonArray(locations)))
                | [] ->
                    LspProtocol.sendResponse idNode (Some (JsonArray()))
        | _ ->
            LspProtocol.sendResponse idNode (Some (JsonArray()))

    let handleDocumentHighlight (idNode: JsonNode) (paramsObj: JsonObject) =
        match tryGetUriFromTextDocument paramsObj, tryGetPosition paramsObj with
        | Some uri, Some (line, character) when documents.ContainsKey(uri) ->
            let doc = documents[uri]
            let targetNames = resolveTargetNames doc line character

            match targetNames with
            | head :: _ ->
                let normalized =
                    if head.Contains('.') then head.Split('.') |> Array.last
                    else head

                let fromOccurrences =
                    [ head; normalized ]
                    |> List.distinct
                    |> List.collect (fun n -> doc.VariableOccurrences |> Map.tryFind n |> Option.defaultValue [])
                    |> List.distinct

                let spans =
                    if fromOccurrences.IsEmpty then
                        findSymbolRangesInText doc.Text (targetNames @ [ normalized ])
                    else
                        fromOccurrences

                let highlights =
                    spans
                    |> List.map (fun span ->
                        let highlight = JsonObject()
                        highlight["range"] <- toLspRange span
                        highlight["kind"] <- JsonValue.Create(1)
                        highlight :> JsonNode)
                    |> List.toArray

                LspProtocol.sendResponse idNode (Some (JsonArray(highlights)))
            | [] ->
                LspProtocol.sendResponse idNode (Some (JsonArray()))
        | _ ->
            LspProtocol.sendResponse idNode (Some (JsonArray()))

    let handleSignatureHelp (idNode: JsonNode) (paramsObj: JsonObject) =
        match tryGetUriFromTextDocument paramsObj, tryGetPosition paramsObj with
        | Some uri, Some (line, character) when documents.ContainsKey(uri) ->
            let doc = documents[uri]
            let tryResolveCallTargetFromInvocation () =
                match getLineText doc.Text line with
                | None -> None
                | Some lineText ->
                    let pos = max 0 (min character lineText.Length)
                    let mutable idx = pos - 1
                    let mutable closeDepth = 0
                    let mutable openIdx = -1

                    while idx >= 0 && openIdx < 0 do
                        match lineText[idx] with
                        | ')' -> closeDepth <- closeDepth + 1
                        | '(' ->
                            if closeDepth = 0 then
                                openIdx <- idx
                            else
                                closeDepth <- closeDepth - 1
                        | _ -> ()
                        idx <- idx - 1

                    if openIdx <= 0 then None
                    else
                        let mutable finish = openIdx
                        while finish > 0 && Char.IsWhiteSpace(lineText[finish - 1]) do
                            finish <- finish - 1
                        let mutable start = finish - 1
                        while start >= 0 && isWordChar lineText[start] do
                            start <- start - 1
                        let tokenStart = start + 1
                        if tokenStart < finish then
                            Some (lineText.Substring(tokenStart, finish - tokenStart))
                        else
                            None

            let computeActiveParameter () =
                match getLineText doc.Text line with
                | None -> 0
                | Some lineText ->
                    let pos = max 0 (min character lineText.Length)
                    let mutable idx = pos - 1
                    let mutable closeDepth = 0
                    let mutable openIdx = -1

                    while idx >= 0 && openIdx < 0 do
                        match lineText[idx] with
                        | ')' -> closeDepth <- closeDepth + 1
                        | '(' ->
                            if closeDepth = 0 then
                                openIdx <- idx
                            else
                                closeDepth <- closeDepth - 1
                        | _ -> ()
                        idx <- idx - 1

                    if openIdx < 0 then 0
                    else
                        let mutable depthParen = 0
                        let mutable depthBracket = 0
                        let mutable depthBrace = 0
                        let mutable commas = 0
                        let mutable i = openIdx + 1

                        while i < pos do
                            match lineText[i] with
                            | '(' -> depthParen <- depthParen + 1
                            | ')' -> if depthParen > 0 then depthParen <- depthParen - 1
                            | '[' -> depthBracket <- depthBracket + 1
                            | ']' -> if depthBracket > 0 then depthBracket <- depthBracket - 1
                            | '{' -> depthBrace <- depthBrace + 1
                            | '}' -> if depthBrace > 0 then depthBrace <- depthBrace - 1
                            | ',' when depthParen = 0 && depthBracket = 0 && depthBrace = 0 ->
                                commas <- commas + 1
                            | _ -> ()
                            i <- i + 1

                        commas

            let callTarget =
                match tryGetContextTriggerCharacter paramsObj with
                | Some "(" -> tryGetCallTargetPrefixAtPosition doc.Text line (character - 1)
                | Some "," -> tryGetCallTargetPrefixAtPosition doc.Text line (character - 1)
                | _ -> tryGetCallTargetPrefixAtPosition doc.Text line character

            let resolvedCallTarget =
                match callTarget with
                | Some c -> Some c
                | None ->
                    match tryGetWordAtPosition doc.Text line (max 0 (character - 1)) with
                    | Some w -> Some w
                    | None -> tryResolveCallTargetFromInvocation ()

            match resolvedCallTarget with
            | Some target ->
                let normalize (name: string) =
                    if name.Contains('.') then name.Split('.') |> Array.last
                    else name

                let targetNormalized = normalize target
                let matched =
                    doc.Symbols
                    |> List.tryFind (fun s -> s.Name = target || normalize s.Name = targetNormalized)

                match matched with
                | Some sym ->
                    let signatureLabel =
                        match sym.TypeText with
                        | Some t -> $"{sym.Name} : {t}"
                        | None -> sym.Name

                    let sigInfo = JsonObject()
                    sigInfo["label"] <- JsonValue.Create(signatureLabel)

                    let signatureHelp = JsonObject()
                    signatureHelp["signatures"] <- JsonArray([| sigInfo :> JsonNode |])
                    signatureHelp["activeSignature"] <- JsonValue.Create(0)
                    signatureHelp["activeParameter"] <- JsonValue.Create(computeActiveParameter ())
                    LspProtocol.sendResponse idNode (Some signatureHelp)
                | None ->
                    let injectedSignature =
                        [ target; targetNormalized ]
                        |> List.tryPick (fun candidate ->
                            doc.InjectedFunctionSignatures |> Map.tryFind candidate)

                    match injectedSignature with
                    | Some typeText ->
                        let signature =
                            let normalizedTarget =
                                if target.Contains('.') then target.Split('.') |> Array.last
                                else target

                            if doc.InjectedFunctionSignatures.ContainsKey(target) then
                                formatInjectedFunctionSignature doc target typeText
                            elif doc.InjectedFunctionSignatures.ContainsKey(normalizedTarget) then
                                formatInjectedFunctionSignature doc normalizedTarget typeText
                            else
                                $"{target} : {typeText}"

                        let sigInfo = JsonObject()
                        sigInfo["label"] <- JsonValue.Create(signature)

                        let signatureHelp = JsonObject()
                        signatureHelp["signatures"] <- JsonArray([| sigInfo :> JsonNode |])
                        signatureHelp["activeSignature"] <- JsonValue.Create(0)
                        signatureHelp["activeParameter"] <- JsonValue.Create(computeActiveParameter ())
                        LspProtocol.sendResponse idNode (Some signatureHelp)
                    | None ->
                        LspProtocol.sendResponse idNode None
            | None ->
                LspProtocol.sendResponse idNode None
        | _ ->
            LspProtocol.sendResponse idNode None

    let handleStdlibSource (idNode: JsonNode) (paramsObj: JsonObject) =
        match tryGetString paramsObj "uri" with
        | None ->
            sendCommandError idNode "internal" "Missing stdlib URI."
        | Some uri ->
            match InteropServices.tryLoadStdlibSourceText uri with
            | Some sourceText ->
                let response = JsonObject()
                response["ok"] <- JsonValue.Create(true)
                let data = JsonObject()
                data["uri"] <- JsonValue.Create(uri)
                data["text"] <- JsonValue.Create(sourceText)
                data["languageId"] <- JsonValue.Create("fscript")
                response["data"] <- data
                LspProtocol.sendResponse idNode (Some response)
            | None ->
                sendCommandError idNode "internal" $"Unable to load stdlib source for '{uri}'."

    let handleRename (idNode: JsonNode) (paramsObj: JsonObject) =
        match tryGetUriFromTextDocument paramsObj, tryGetPosition paramsObj, tryGetString paramsObj "newName" with
        | Some _, Some _, Some newName when not (isValidIdentifierName newName) ->
            LspProtocol.sendError idNode -32602 $"Invalid rename target '{newName}'"
        | Some uri, Some (line, character), Some newName when documents.ContainsKey(uri) ->
            let doc = documents[uri]

            let targetNames = resolveTargetNames doc line character

            match targetNames with
            | [] -> LspProtocol.sendResponse idNode None
            | _ ->
                let normalizedNames =
                    targetNames
                    |> List.collect (fun n ->
                        if n.Contains('.') then [ n; n.Split('.') |> Array.last ] else [ n ])
                    |> List.distinct

                let changes = JsonObject()

                documents
                |> Seq.iter (fun kv ->
                    let docUri = kv.Key
                    let candidateDoc = kv.Value
                    let fromOccurrences =
                        normalizedNames
                        |> List.collect (fun n -> candidateDoc.VariableOccurrences |> Map.tryFind n |> Option.defaultValue [])
                        |> List.distinct

                    let spans =
                        if fromOccurrences.IsEmpty then
                            findSymbolRangesInText candidateDoc.Text targetNames
                        else
                            fromOccurrences

                    if not spans.IsEmpty then
                        let edits =
                            spans
                            |> List.map (fun span ->
                                let edit = JsonObject()
                                edit["range"] <- toLspRange span
                                edit["newText"] <- JsonValue.Create(newName)
                                edit :> JsonNode)
                            |> List.toArray
                        changes[docUri] <- JsonArray(edits))

                let workspaceEdit = JsonObject()
                workspaceEdit["changes"] <- changes
                LspProtocol.sendResponse idNode (Some workspaceEdit)
        | _ ->
            LspProtocol.sendResponse idNode None

    let handlePrepareRename (idNode: JsonNode) (paramsObj: JsonObject) =
        match tryGetUriFromTextDocument paramsObj, tryGetPosition paramsObj with
        | Some uri, Some (line, character) when documents.ContainsKey(uri) ->
            let doc = documents[uri]
            match tryGetWordAtPosition doc.Text line character with
            | Some word when isValidIdentifierName word ->
                let lineText = getLineText doc.Text line |> Option.defaultValue ""
                let pos = max 0 (min character lineText.Length)
                let mutable start = pos
                while start > 0 && isWordChar lineText[start - 1] do
                    start <- start - 1
                let mutable finish = pos
                while finish < lineText.Length && isWordChar lineText[finish] do
                    finish <- finish + 1

                let span =
                    Span.mk
                        (Span.pos (line + 1) (start + 1))
                        (Span.pos (line + 1) (finish + 1))

                let result = JsonObject()
                result["range"] <- toLspRange span
                result["placeholder"] <- JsonValue.Create(word)
                LspProtocol.sendResponse idNode (Some result)
            | _ ->
                LspProtocol.sendError idNode -32602 "Rename is not valid at this position"
        | _ ->
            LspProtocol.sendError idNode -32602 "Rename is not valid at this position"

    let handleWorkspaceSymbol (idNode: JsonNode) (paramsObj: JsonObject) =
        let query =
            tryGetString paramsObj "query"
            |> Option.defaultValue ""
            |> fun q -> q.Trim()

        let hasQuery = not (String.IsNullOrWhiteSpace(query))
        let queryLower = query.ToLowerInvariant()

        let symbols =
            documents
            |> Seq.collect (fun kv ->
                let uri = kv.Key
                kv.Value.Symbols
                |> Seq.filter (fun s ->
                    if not hasQuery then true
                    else
                        s.Name.ToLowerInvariant().Contains(queryLower)
                        || (s.TypeText |> Option.exists (fun t -> t.ToLowerInvariant().Contains(queryLower))))
                |> Seq.map (fun s ->
                    let item = JsonObject()
                    item["name"] <- JsonValue.Create(s.Name)
                    item["kind"] <- JsonValue.Create(s.Kind)
                    let location = JsonObject()
                    location["uri"] <- JsonValue.Create(uri)
                    location["range"] <- toLspRange s.Span
                    item["location"] <- location
                    item :> JsonNode))
            |> Seq.toArray

        LspProtocol.sendResponse idNode (Some (JsonArray(symbols)))

    let private levenshteinDistance (a: string) (b: string) =
        let m = a.Length
        let n = b.Length
        let d = Array2D.zeroCreate<int> (m + 1) (n + 1)

        for i = 0 to m do d[i, 0] <- i
        for j = 0 to n do d[0, j] <- j

        for i = 1 to m do
            for j = 1 to n do
                let cost = if a[i - 1] = b[j - 1] then 0 else 1
                d[i, j] <- List.min [ d[i - 1, j] + 1; d[i, j - 1] + 1; d[i - 1, j - 1] + cost ]

        d[m, n]

    let private tryExtractUnboundName (message: string) =
        let prefix = "Unbound variable '"
        if message.StartsWith(prefix, StringComparison.Ordinal) && message.EndsWith("'", StringComparison.Ordinal) then
            let inner = message.Substring(prefix.Length, message.Length - prefix.Length - 1)
            if String.IsNullOrWhiteSpace(inner) then None else Some inner
        else
            None

    let handleCodeAction (idNode: JsonNode) (paramsObj: JsonObject) =
        match tryGetUriFromTextDocument paramsObj with
        | Some uri when documents.ContainsKey(uri) ->
            let doc = documents[uri]
            let candidatePool =
                [ for s in doc.Symbols -> s.Name
                  yield! stdlibNames
                  yield! builtinNames ]
                |> List.distinct

            let actions = ResizeArray<JsonNode>()

            match tryGetObject paramsObj "context" with
            | Some contextObj ->
                match contextObj["diagnostics"] with
                | :? JsonArray as diagnostics ->
                    for diag in diagnostics do
                        match diag with
                        | :? JsonObject as diagObj ->
                            match diagObj["message"] with
                            | :? JsonValue as mv ->
                                let message =
                                    try mv.GetValue<string>() with _ -> ""
                                match tryExtractUnboundName message with
                                | Some missingName ->
                                    let suggestion =
                                        candidatePool
                                        |> List.map (fun c -> c, levenshteinDistance missingName c)
                                        |> List.sortBy snd
                                        |> List.tryHead
                                        |> Option.bind (fun (name, dist) ->
                                            if dist <= 3 then Some name else None)

                                    match suggestion, diagObj["range"] with
                                    | Some replacement, (:? JsonObject as rangeObjUntyped) ->
                                        let rangeObj: JsonObject = rangeObjUntyped
                                        let edit = JsonObject()
                                        let copiedRange =
                                            let mkPos (obj: JsonObject) =
                                                let p = JsonObject()
                                                p["line"] <- JsonValue.Create(tryGetInt obj "line" |> Option.defaultValue 0)
                                                p["character"] <- JsonValue.Create(tryGetInt obj "character" |> Option.defaultValue 0)
                                                p

                                            let r = JsonObject()
                                            match rangeObj["start"], rangeObj["end"] with
                                            | (:? JsonObject as s), (:? JsonObject as e) ->
                                                r["start"] <- mkPos s
                                                r["end"] <- mkPos e
                                            | _ ->
                                                r["start"] <- JsonObject()
                                                r["end"] <- JsonObject()
                                            r

                                        edit["range"] <- copiedRange
                                        edit["newText"] <- JsonValue.Create(replacement)

                                        let changes = JsonObject()
                                        changes[uri] <- JsonArray([| edit :> JsonNode |])

                                        let workspaceEdit = JsonObject()
                                        workspaceEdit["changes"] <- changes

                                        let action = JsonObject()
                                        action["title"] <- JsonValue.Create($"Replace with '{replacement}'")
                                        action["kind"] <- JsonValue.Create("quickfix")
                                        action["edit"] <- workspaceEdit
                                        action["isPreferred"] <- JsonValue.Create(true)
                                        actions.Add(action)
                                    | _ -> ()
                                | None -> ()
                            | _ -> ()
                        | _ -> ()
                | _ -> ()
            | None -> ()

            LspProtocol.sendResponse idNode (Some (JsonArray(actions.ToArray())))
        | _ ->
            LspProtocol.sendResponse idNode (Some (JsonArray()))
