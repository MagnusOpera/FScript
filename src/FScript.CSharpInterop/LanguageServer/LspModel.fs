namespace FScript.LanguageServer

open System
open System.Text.Json.Nodes
open System.Collections.Generic
open FScript.Language

module LspModel =
    let mutable inlayHintsEnabled = true
    let mutable lastDefinitionRequest: (string * int * int * DateTime) option = None

    let stdlibNames = Stdlib.reservedNames() |> Set.toList
    let builtinNames = [ "ignore"; "nameof"; "typeof" ]

    let reservedKeywords =
        [ "let"; "rec"; "and"; "if"; "then"; "elif"; "else"; "match"; "with"; "when"
          "for"; "in"; "do"; "type"; "module"; "true"; "false"; "None"; "Some" ]
        |> Set.ofList

    let asObject (node: JsonNode) : JsonObject option =
        match node with
        | :? JsonObject as o -> Some o
        | _ -> None

    let tryGetObject (obj: JsonObject) (name: string) : JsonObject option =
        match obj[name] with
        | null -> None
        | node -> asObject node

    let tryGetNode (obj: JsonObject) (name: string) : JsonNode option =
        match obj[name] with
        | null -> None
        | node -> Some node

    let tryGetString (obj: JsonObject) (name: string) : string option =
        match obj[name] with
        | :? JsonValue as v ->
            try Some (v.GetValue<string>()) with _ -> None
        | _ -> None

    let tryGetInt (obj: JsonObject) (name: string) : int option =
        match obj[name] with
        | :? JsonValue as v ->
            try Some (v.GetValue<int>()) with _ -> None
        | _ -> None

    type TopLevelSymbol =
        { Name: string
          Kind: int
          TypeText: string option
          TypeTargetName: string option
          Span: Span }

    type LocalBindingKind =
        | Parameter
        | LetBound
        | PatternBound
        | LoopBound

    type LocalBindingInfo =
        { Name: string
          DeclSpan: Span
          ScopeSpan: Span
          BindingKind: LocalBindingKind
          AnnotationType: string option }

    type DocumentState =
        { SourcePath: string
          Text: string
          Symbols: TopLevelSymbol list
          RecordParameterFields: Map<string, (string * string) list>
          ParameterTypeTargets: Map<string, string>
          FunctionParameters: Map<string, string list>
          FunctionAnnotationTypes: Map<string, string list>
          FunctionDeclaredReturnTargets: Map<string, string>
          CallArgumentHints: (Span * string) list
          FunctionReturnTypeHints: (Span * string) list
          ParameterTypeHints: (Span * string) list
          PatternTypeHints: (Span * string) list
          LocalVariableTypeHints: (Span * string * string) list
          LocalBindings: LocalBindingInfo list
          InjectedFunctionSignatures: Map<string, string>
          InjectedFunctionParameterNames: Map<string, string list>
          InjectedFunctionDefinitions: Map<string, (string * Span)>
          ImportAliasToInternal: Map<string, string>
          ImportInternalToAlias: Map<string, string>
          // Variable occurrences keyed by identifier, sourced from AST spans.
          // This avoids text-based false positives (for example record field labels).
          VariableOccurrences: Map<string, Span list>
          // Lexically resolved local-definition targets keyed by variable-usage spans.
          ResolvedLocalDefinitions: (Span * Span) list }

    let documents = Dictionary<string, DocumentState>(StringComparer.Ordinal)

    let toLspRange (span: Span) =
        let startLine = max 0 (span.Start.Line - 1)
        let startChar = max 0 (span.Start.Column - 1)
        let endLine = max 0 (span.End.Line - 1)
        let endChar = max 0 (span.End.Column - 1)

        let startObj = JsonObject()
        startObj["line"] <- JsonValue.Create(startLine)
        startObj["character"] <- JsonValue.Create(startChar)

        let endObj = JsonObject()
        endObj["line"] <- JsonValue.Create(endLine)
        endObj["character"] <- JsonValue.Create(endChar)

        let rangeObj = JsonObject()
        rangeObj["start"] <- startObj
        rangeObj["end"] <- endObj
        rangeObj

    let symbolKindLabel (kind: int) =
        match kind with
        | 5 -> "type"
        | 12 -> "function"
        | 13 -> "value"
        | 22 -> "union-case"
        | _ -> "symbol"

    let diagnostic (severity: int) (code: string) (span: Span) (message: string) =
        let d = JsonObject()
        d["range"] <- toLspRange span
        d["severity"] <- JsonValue.Create(severity)
        d["code"] <- JsonValue.Create(code)
        d["source"] <- JsonValue.Create("fscript-lsp")
        d["message"] <- JsonValue.Create(message)
        d

    let publishDiagnostics (uri: string) (diags: JsonNode list) =
        let p = JsonObject()
        p["uri"] <- JsonValue.Create(uri)
        p["diagnostics"] <- JsonArray(diags |> Seq.toArray)
        LspProtocol.sendNotification "textDocument/publishDiagnostics" (Some p)

    let symbolKindForType (t: Type) =
        match t with
        | TFun _ -> 12
        | _ -> 13

    let declarationKindFromArgs (args: Param list) =
        if args.IsEmpty then 13 else 12

    let getLineText (text: string) (line: int) : string option =
        if line < 0 then None
        else
            let lines = text.Split('\n')
            if line >= lines.Length then None
            else Some (lines[line].TrimEnd('\r'))

    let isWordChar (c: char) =
        Char.IsLetterOrDigit(c) || c = '_' || c = '.'

    let isValidIdentifierName (name: string) =
        let startsValid c = Char.IsLetter(c) || c = '_'
        let partValid c = Char.IsLetterOrDigit(c) || c = '_'
        not (String.IsNullOrWhiteSpace(name))
        && startsValid name[0]
        && (name |> Seq.forall partValid)
        && not (reservedKeywords.Contains name)

    let tryGetWordAtPosition (text: string) (line: int) (character: int) : string option =
        match getLineText text line with
        | None -> None
        | Some lineText ->
            if lineText.Length = 0 then None
            else
                let pos =
                    if character < 0 then 0
                    elif character > lineText.Length then lineText.Length
                    else character

                let mutable start = pos
                while start > 0 && isWordChar lineText[start - 1] do
                    start <- start - 1

                let mutable finish = pos
                while finish < lineText.Length && isWordChar lineText[finish] do
                    finish <- finish + 1

                if finish > start then Some (lineText.Substring(start, finish - start)) else None

    let tryGetWordAtOrAdjacentPosition (text: string) (line: int) (character: int) : string option =
        [ character; character - 1; character + 1; character - 2; character + 2 ]
        |> List.distinct
        |> List.tryPick (fun candidate ->
            tryGetWordAtPosition text line candidate)

    let tryGetWordPrefixAtPosition (text: string) (line: int) (character: int) : string option =
        match getLineText text line with
        | None -> None
        | Some lineText ->
            if lineText.Length = 0 then None
            else
                let pos =
                    if character < 0 then 0
                    elif character > lineText.Length then lineText.Length
                    else character

                let mutable start = pos
                while start > 0 && isWordChar lineText[start - 1] do
                    start <- start - 1

                if pos > start then Some (lineText.Substring(start, pos - start)) else None

    let tryGetPosition (paramsObj: JsonObject) : (int * int) option =
        match tryGetObject paramsObj "position" with
        | None -> None
        | Some posObj ->
            match tryGetInt posObj "line", tryGetInt posObj "character" with
            | Some line, Some character -> Some (line, character)
            | _ -> None

    let tryGetRange (paramsObj: JsonObject) : (int * int * int * int) option =
        match tryGetObject paramsObj "range" with
        | Some rangeObj ->
            match tryGetObject rangeObj "start", tryGetObject rangeObj "end" with
            | Some startObj, Some endObj ->
                match tryGetInt startObj "line", tryGetInt startObj "character", tryGetInt endObj "line", tryGetInt endObj "character" with
                | Some sl, Some sc, Some el, Some ec -> Some (sl, sc, el, ec)
                | _ -> None
            | _ -> None
        | None -> None

    let tryGetUriFromTextDocument (paramsObj: JsonObject) : string option =
        match tryGetObject paramsObj "textDocument" with
        | None -> None
        | Some td -> tryGetString td "uri"

    let tryGetContextTriggerCharacter (paramsObj: JsonObject) : string option =
        match tryGetObject paramsObj "context" with
        | None -> None
        | Some ctx -> tryGetString ctx "triggerCharacter"
