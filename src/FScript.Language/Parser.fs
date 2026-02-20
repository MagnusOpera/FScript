namespace FScript.Language

module Parser =
    type private TokenStream(tokens: Token list) =
        let mutable index = 0
        member _.Peek() = tokens.[index]
        member _.PeekAt(offset: int) = tokens.[index + offset]
        member _.TokenAt(i: int) = tokens.[i]
        member _.Index = index
        member _.Mark() = index
        member _.Restore(mark: int) = index <- mark
        member _.Next() =
            let t = tokens.[index]
            index <- index + 1
            t
        member _.Match(kind) =
            if tokens.[index].Kind = kind then
                index <- index + 1
                true
            else
                false
        member _.Expect(kind, message) =
            let t = tokens.[index]
            if t.Kind = kind then
                index <- index + 1
                t
            else
                raise (ParseException { Message = message; Span = t.Span })
        member _.ExpectIdent(message) =
            let t = tokens.[index]
            match t.Kind with
            | Ident _ ->
                index <- index + 1
                t
            | _ -> raise (ParseException { Message = message; Span = t.Span })
        member _.AtEnd = tokens.[index].Kind = EOF
        member _.SkipNewlines() =
            while tokens.[index].Kind = Newline do index <- index + 1

    let private mkSpanFrom a b = Span.merge a b

    let private isStartAtom (k: TokenKind) =
        match k with
        | Ident _ | IntLit _ | FloatLit _ | StringLit _ | InterpString _ | BoolLit _ | LParen | LBracket | LBrace | Let | Fun | If | Raise | For | Match | Typeof | Nameof -> true
        | _ -> false

    let private isUpperIdent (name: string) =
        not (System.String.IsNullOrEmpty name) && System.Char.IsUpper(name.[0])

    let private isStartPatternAtom (k: TokenKind) =
        match k with
        | Ident _ | IntLit _ | FloatLit _ | StringLit _ | BoolLit _ | LBracket | LParen | LBrace -> true
        | _ -> false

    let private parseLiteral (t: Token) =
        match t.Kind with
        | IntLit v -> LInt v
        | FloatLit v -> LFloat v
        | StringLit v -> LString v
        | BoolLit v -> LBool v
        | _ -> raise (ParseException { Message = "Expected literal"; Span = t.Span })

    let rec parseProgramWithSourceName (sourceName: string option) (src: string) : Program =
        let tokens = Lexer.tokenizeWithSourceName sourceName src
        let stream = TokenStream(tokens)

        let parseSingleExpression (exprText: string) =
            let parsed = parseProgramWithSourceName None exprText
            match parsed with
            | [ SExpr expr ] -> expr
            | _ ->
                raise (ParseException { Message = "Interpolation placeholders must contain a single expression"; Span = stream.Peek().Span })

        let parseInterpolatedString (raw: string) (span: Span) : Expr =
            let parts = ResizeArray<InterpolatedPart>()
            let text = System.Text.StringBuilder()

            let flushText () =
                if text.Length > 0 then
                    parts.Add(IPText (text.ToString()))
                    text.Clear() |> ignore

            let rec readPlaceholder (s: string) (start: int) =
                let mutable i = start
                let mutable depth = 1
                let mutable inString = false
                let mutable escaped = false

                while i < s.Length && depth > 0 do
                    let ch = s.[i]
                    if inString then
                        if escaped then
                            escaped <- false
                            i <- i + 1
                        else
                            match ch with
                            | '\\' -> escaped <- true; i <- i + 1
                            | '"' -> inString <- false; i <- i + 1
                            | _ -> i <- i + 1
                    else
                        match ch with
                        | '"' -> inString <- true; i <- i + 1
                        | '{' -> depth <- depth + 1; i <- i + 1
                        | '}' -> depth <- depth - 1; i <- i + 1
                        | _ -> i <- i + 1

                if depth <> 0 then
                    raise (ParseException { Message = "Unterminated interpolation placeholder"; Span = span })

                let exprText = s.Substring(start, i - start - 1)
                if System.String.IsNullOrWhiteSpace(exprText) then
                    raise (ParseException { Message = "Interpolation placeholder cannot be empty"; Span = span })
                let expr = parseSingleExpression exprText
                expr, i

            let mutable i = 0
            while i < raw.Length do
                match raw.[i] with
                | '{' when i + 1 < raw.Length && raw.[i + 1] = '{' ->
                    text.Append('{') |> ignore
                    i <- i + 2
                | '}' when i + 1 < raw.Length && raw.[i + 1] = '}' ->
                    text.Append('}') |> ignore
                    i <- i + 2
                | '{' ->
                    flushText ()
                    let expr, nextIdx = readPlaceholder raw (i + 1)
                    parts.Add(IPExpr expr)
                    i <- nextIdx
                | '}' ->
                    raise (ParseException { Message = "Unescaped '}' in interpolated string"; Span = span })
                | '\\' when i + 1 < raw.Length ->
                    let next = raw.[i + 1]
                    match next with
                    | 'n' -> text.Append('\n') |> ignore
                    | 't' -> text.Append('\t') |> ignore
                    | '"' -> text.Append('"') |> ignore
                    | '\\' -> text.Append('\\') |> ignore
                    | _ -> text.Append(next) |> ignore
                    i <- i + 2
                | ch ->
                    text.Append(ch) |> ignore
                    i <- i + 1

            flushText ()
            let finalParts =
                if parts.Count = 0 then [ IPText "" ] else parts |> Seq.toList
            EInterpolatedString(finalParts, span)

        let consumeLayoutSeparators () =
            let mutable consumed = false
            let mutable progress = true
            while progress do
                progress <- false
                while stream.Match(Newline) do
                    consumed <- true
                    progress <- true
                while stream.Match(Indent) do
                    consumed <- true
                    progress <- true
                while stream.Match(Dedent) do
                    consumed <- true
                    progress <- true
            consumed

        let hasNonLayoutTokenBeforeOnSameLine (tokenIndex: int) (line: int) =
            let mutable i = tokenIndex - 1
            let mutable keepSearching = true
            let mutable foundSameLine = false
            while keepSearching && i >= 0 do
                let tok = stream.TokenAt(i)
                match tok.Kind with
                | Newline ->
                    keepSearching <- false
                | Indent
                | Dedent ->
                    i <- i - 1
                | _ ->
                    foundSameLine <- tok.Span.Start.Line = line
                    keepSearching <- false
            foundSameLine

        let mutable allowIndentedApplication = true
        let mutable allowBinaryNewlineSkipping = true

        let parseQualifiedTypeName () : string =
            let first = stream.ExpectIdent("Expected type name")
            let firstName =
                match first.Kind with
                | Ident n -> n
                | _ -> ""
            let parts = ResizeArray<string>()
            parts.Add(firstName)
            let mutable keepGoing = true
            while keepGoing && stream.Match(Dot) do
                let next = stream.ExpectIdent("Expected identifier after '.' in qualified type name")
                match next.Kind with
                | Ident n -> parts.Add(n)
                | _ -> ()
                keepGoing <- true
            String.concat "." parts

        let rec parseTypeRefAtom () : TypeRef =
            stream.SkipNewlines()
            match stream.Peek().Kind with
            | Ident _ ->
                let qualified = parseQualifiedTypeName()
                TRName qualified
            | LBrace ->
                stream.Next() |> ignore
                let isStructural = stream.Match(Bar)
                let fields = ResizeArray<string * TypeRef>()
                let seen = System.Collections.Generic.HashSet<string>()
                let parseField () =
                    let nameTok = stream.ExpectIdent("Expected field name in inline record type")
                    let fieldName =
                        match nameTok.Kind with
                        | Ident n -> n
                        | _ -> ""
                    if not (seen.Add fieldName) then
                        raise (ParseException { Message = $"Duplicate field '{fieldName}' in inline record type"; Span = nameTok.Span })
                    stream.Expect(Colon, "Expected ':' after inline record type field name") |> ignore
                    let fieldType = parseTypeRef()
                    fields.Add(fieldName, fieldType)
                if (isStructural && stream.Peek().Kind = Bar)
                   || (not isStructural && stream.Peek().Kind = RBrace) then
                    raise (ParseException { Message = "Inline record type must define at least one field"; Span = stream.Peek().Span })
                parseField()
                while stream.Match(Semicolon) do
                    if (isStructural && stream.Peek().Kind <> Bar)
                       || ((not isStructural) && stream.Peek().Kind <> RBrace) then
                        parseField()
                if isStructural then
                    stream.Expect(Bar, "Expected '|' in structural record type") |> ignore
                stream.Expect(RBrace, "Expected '}' in inline record type") |> ignore
                if isStructural then
                    TRStructuralRecord (fields |> Seq.toList)
                else
                    TRRecord (fields |> Seq.toList)
            | LParen ->
                stream.Next() |> ignore
                let first = parseTypeRef()
                if stream.Match(Star) then
                    let items = ResizeArray<TypeRef>()
                    items.Add(first)
                    items.Add(parseTypeRef())
                    while stream.Match(Star) do
                        items.Add(parseTypeRef())
                    stream.Expect(RParen, "Expected ')' in tuple type") |> ignore
                    TRTuple (items |> Seq.toList)
                else
                    stream.Expect(RParen, "Expected ')' in type expression") |> ignore
                    first
            | _ -> raise (ParseException { Message = "Expected type expression"; Span = stream.Peek().Span })

        and parseTypeRefPostfix () : TypeRef =
            let mutable t = parseTypeRefAtom()
            let mutable loop = true
            while loop do
                match stream.Peek().Kind with
                | Ident "list" ->
                    stream.Next() |> ignore
                    t <- TRPostfix(t, "list")
                | Ident "option" ->
                    stream.Next() |> ignore
                    t <- TRPostfix(t, "option")
                | Ident "map" ->
                    stream.Next() |> ignore
                    t <- TRPostfix(t, "map")
                | _ -> loop <- false
            t

        and parseTypeRef () : TypeRef =
            let left = parseTypeRefPostfix()
            if stream.Match(Arrow) then
                let right = parseTypeRef()
                TRFun(left, right)
            else
                left

        and parseParam () : Param =
            stream.SkipNewlines()
            match stream.Peek().Kind with
            | Ident n ->
                let tok = stream.Next()
                { Name = n; Annotation = None; Span = tok.Span }
            | LParen ->
                let lp = stream.Next()
                let nameTok = stream.ExpectIdent("Expected parameter name in annotated parameter")
                let name =
                    match nameTok.Kind with
                    | Ident n -> n
                    | _ -> ""
                stream.SkipNewlines()
                stream.Expect(Colon, "Expected ':' in annotated parameter") |> ignore
                let anno = parseTypeRef()
                let rp = stream.Expect(RParen, "Expected ')' after annotated parameter")
                { Name = name; Annotation = Some anno; Span = mkSpanFrom lp.Span rp.Span }
            | _ -> raise (ParseException { Message = "Expected parameter"; Span = stream.Peek().Span })

        and parseParamsAligned () : ResizeArray<Param> =
            let args = ResizeArray<Param>()
            let mutable argsDone = false
            let mutable firstParamColumn: int option = None
            while not argsDone do
                let mutable sawNewline = false
                while stream.Match(Newline) do
                    sawNewline <- true
                while stream.Match(Indent) do ()
                while stream.Match(Dedent) do ()
                match stream.Peek().Kind with
                | Ident _
                | LParen ->
                    let param = parseParam()
                    match firstParamColumn with
                    | None -> firstParamColumn <- Some param.Span.Start.Column
                    | Some firstCol ->
                        if sawNewline && param.Span.Start.Column <> firstCol then
                            raise (ParseException { Message = "Multiline let parameter columns must align"; Span = param.Span })
                    args.Add(param)
                | _ ->
                    argsDone <- true
            args

        and parseTypeDecl () : Stmt =
            let typeTok = stream.Expect(Type, "Expected 'type'")
            let isRec = stream.Match(Rec)
            let nameTok = stream.ExpectIdent("Expected identifier after 'type'")
            let name =
                match nameTok.Kind with
                | Ident n -> n
                | _ -> ""
            stream.SkipNewlines()
            stream.Expect(Equals, "Expected '=' in type declaration") |> ignore
            let mutable hasOuterIndent = false
            if stream.Match(Newline) then
                while stream.Match(Newline) do ()
                if stream.Match(Indent) then
                    hasOuterIndent <- true

            let parseRecordDecl () =
                stream.Expect(LBrace, "Expected '{' in record type declaration") |> ignore
                let fields = ResizeArray<string * TypeRef>()
                let parseField () =
                    stream.SkipNewlines()
                    let fieldTok = stream.ExpectIdent("Expected field name in type declaration")
                    let fieldName =
                        match fieldTok.Kind with
                        | Ident n -> n
                        | _ -> ""
                    stream.SkipNewlines()
                    stream.Expect(Colon, "Expected ':' after field name") |> ignore
                    let fieldType = parseTypeRef()
                    fields.Add(fieldName, fieldType)
                    fieldTok.Span.Start.Column
                let firstFieldColumn = parseField()
                let mutable doneFields = false
                while not doneFields do
                    if stream.Match(Semicolon) then
                        if stream.Peek().Kind <> RBrace then
                            parseField() |> ignore
                        else
                            doneFields <- true
                    else
                        let mutable sawNewline = false
                        while stream.Match(Newline) do
                            sawNewline <- true
                        while stream.Match(Indent) do ()
                        while stream.Match(Dedent) do ()
                        if sawNewline then
                            match stream.Peek().Kind with
                            | RBrace -> doneFields <- true
                            | Ident _ ->
                                let fieldColumn = parseField()
                                if fieldColumn <> firstFieldColumn then
                                    raise (ParseException { Message = "Type declaration fields must align"; Span = stream.Peek().Span })
                            | _ ->
                                raise (ParseException { Message = "Expected field or '}' in record type declaration"; Span = stream.Peek().Span })
                        else
                            doneFields <- true
                let rb = stream.Expect(RBrace, "Expected '}' in record type declaration")
                fields |> Seq.toList, [], rb.Span

            let parseUnionDecl () =
                let cases = ResizeArray<string * TypeRef option>()
                let parseCase () =
                    stream.SkipNewlines()
                    stream.Match(Bar) |> ignore
                    let caseTok = stream.ExpectIdent("Expected union case name")
                    let caseName =
                        match caseTok.Kind with
                        | Ident n -> n
                        | _ -> ""
                    if not (isUpperIdent caseName) then
                        raise (ParseException { Message = "Union case name must start with uppercase letter"; Span = caseTok.Span })
                    stream.SkipNewlines()
                    let payload =
                        if stream.Match(Of) then
                            Some (parseTypeRef())
                        else
                            None
                    cases.Add(caseName, payload)
                    caseTok.Span
                let mutable lastSpan = (parseCase()).End
                let mutable doneCases = false
                while not doneCases do
                    let mutable sawNewline = false
                    while stream.Match(Newline) do
                        sawNewline <- true
                    while stream.Match(Indent) do ()
                    if stream.Peek().Kind = Bar then
                        let s = parseCase()
                        lastSpan <- s.End
                    elif sawNewline then
                        doneCases <- true
                    else
                        doneCases <- true
                [], (cases |> Seq.toList), { Start = lastSpan; End = lastSpan }

            let fields, cases, endSpan =
                match stream.Peek().Kind with
                | LBrace -> parseRecordDecl()
                | Bar | Ident _ -> parseUnionDecl()
                | _ -> raise (ParseException { Message = "Expected record or union type declaration body"; Span = stream.Peek().Span })

            if hasOuterIndent then
                stream.SkipNewlines()
                if not (stream.Match(Dedent)) then
                    raise (ParseException { Message = "Expected dedent after type declaration"; Span = stream.Peek().Span })
                while stream.Match(Dedent) do ()
            SType { Name = name; IsRecursive = isRec; Fields = fields; Cases = cases; Span = mkSpanFrom typeTok.Span endSpan }

        and parsePattern () : Pattern =
            stream.SkipNewlines()
            let t = stream.Peek()
            match t.Kind with
            | Ident name when name = "_" ->
                let t = stream.Next()
                PWildcard t.Span
            | Ident name ->
                let t = stream.Next()
                if isUpperIdent name && stream.Match(Dot) then
                    let caseTok = stream.ExpectIdent("Expected union case name after '.'")
                    let caseName =
                        match caseTok.Kind with
                        | Ident n -> n
                        | _ -> ""
                    if not (isUpperIdent caseName) then
                        raise (ParseException { Message = "Union case name must start with uppercase letter"; Span = caseTok.Span })
                    if isStartPatternAtom (stream.Peek().Kind) then
                        let p = parsePattern()
                        PUnionCase(Some name, caseName, Some p, mkSpanFrom t.Span (Ast.spanOfPattern p))
                    else
                        PUnionCase(Some name, caseName, None, mkSpanFrom t.Span caseTok.Span)
                else
                    match name with
                    | "None" -> PNone t.Span
                    | "Some" ->
                        let p = parsePattern()
                        PSome(p, mkSpanFrom t.Span (Ast.spanOfPattern p))
                    | _ when isUpperIdent name ->
                        if isStartPatternAtom (stream.Peek().Kind) then
                            let p = parsePattern()
                            PUnionCase(None, name, Some p, mkSpanFrom t.Span (Ast.spanOfPattern p))
                        else
                            PUnionCase(None, name, None, t.Span)
                    | _ -> PVar(name, t.Span)
            | IntLit _ | FloatLit _ | StringLit _ | BoolLit _ ->
                let t = stream.Next()
                PLiteral(parseLiteral t, t.Span)
            | LBracket ->
                let lb = stream.Next()
                if stream.Match(RBracket) then
                    PNil (mkSpanFrom lb.Span lb.Span)
                else
                    let first = parsePatternCons()
                    let elements = ResizeArray<Pattern>()
                    elements.Add(first)
                    while stream.Match(Semicolon) do
                        if stream.Peek().Kind <> RBracket then
                            elements.Add(parsePatternCons())
                    let rb = stream.Expect(RBracket, "Expected ']' in list pattern")
                    let listPattern =
                        (elements |> Seq.toList, PNil (mkSpanFrom rb.Span rb.Span))
                        ||> List.foldBack (fun head tail -> PCons(head, tail, mkSpanFrom (Ast.spanOfPattern head) (Ast.spanOfPattern tail)))
                    listPattern
            | LParen ->
                let lp = stream.Next()
                let first = parsePatternCons()
                if stream.Match(Comma) then
                    let elements = ResizeArray<Pattern>()
                    elements.Add(first)
                    elements.Add(parsePatternCons())
                    while stream.Match(Comma) do
                        elements.Add(parsePatternCons())
                    let rp = stream.Expect(RParen, "Expected ')' after tuple pattern")
                    PTuple(elements |> Seq.toList, mkSpanFrom lp.Span rp.Span)
                else
                    stream.Expect(RParen, "Expected ')' after parenthesized pattern") |> ignore
                    first
            | LBrace ->
                let lb = stream.Next()
                stream.SkipNewlines()
                if stream.Match(RBrace) then
                    PMap([], None, mkSpanFrom lb.Span lb.Span)
                elif stream.Peek().Kind = Bar then
                    // Type-ref pattern: {| Field: type; ... |}
                    stream.Next() |> ignore
                    let fields = ResizeArray<string * TypeRef>()
                    let seen = System.Collections.Generic.HashSet<string>()
                    let parseTypeField () =
                        let nameTok = stream.ExpectIdent("Expected field name in structural record type pattern")
                        let fieldName =
                            match nameTok.Kind with
                            | Ident n -> n
                            | _ -> ""
                        if not (seen.Add fieldName) then
                            raise (ParseException { Message = $"Duplicate field '{fieldName}' in structural record type pattern"; Span = nameTok.Span })
                        stream.Expect(Colon, "Expected ':' in structural record type pattern") |> ignore
                        let fieldType = parseTypeRef()
                        fields.Add(fieldName, fieldType)
                    parseTypeField()
                    while stream.Match(Semicolon) do
                        if stream.Peek().Kind <> Bar then
                            parseTypeField()
                    stream.Expect(Bar, "Expected '|' in structural record type pattern") |> ignore
                    let rb = stream.Expect(RBrace, "Expected '}' in structural record type pattern")
                    PTypeRef(TRStructuralRecord(fields |> Seq.toList), mkSpanFrom lb.Span rb.Span)
                elif stream.Peek().Kind = LBracket then
                    let clauses = ResizeArray<Pattern * Pattern>()
                    let parseClause () =
                        stream.Expect(LBracket, "Expected '[' in map pattern key") |> ignore
                        let keyPattern = parsePatternCons()
                        stream.Expect(RBracket, "Expected ']' in map pattern key") |> ignore
                        stream.SkipNewlines()
                        stream.Expect(Equals, "Expected '=' after map key pattern") |> ignore
                        let valuePattern = parsePatternCons()
                        clauses.Add(keyPattern, valuePattern)

                    parseClause()
                    stream.SkipNewlines()

                    let mutable tailPattern : Pattern option = None
                    let mutable donePattern = false

                    while not donePattern do
                        if stream.Match(Semicolon) then
                            stream.SkipNewlines()
                            if stream.Peek().Kind = RangeDots then
                                stream.Next() |> ignore
                                tailPattern <- Some (parsePatternCons())
                                stream.SkipNewlines()
                                donePattern <- true
                            elif stream.Peek().Kind = LBracket then
                                parseClause()
                                stream.SkipNewlines()
                            elif stream.Peek().Kind = RBrace then
                                donePattern <- true
                            else
                                raise (ParseException { Message = "Expected map pattern clause or '..tail'"; Span = stream.Peek().Span })
                        elif stream.Peek().Kind = RangeDots then
                            stream.Next() |> ignore
                            tailPattern <- Some (parsePatternCons())
                            stream.SkipNewlines()
                            donePattern <- true
                        elif stream.Peek().Kind = LBracket then
                            parseClause()
                            stream.SkipNewlines()
                        else
                            donePattern <- true

                    let rb = stream.Expect(RBrace, "Expected '}' in map pattern")
                    PMap(clauses |> Seq.toList, tailPattern, mkSpanFrom lb.Span rb.Span)
                else
                    let mark = stream.Mark()
                    let isTypeRefPattern =
                        try
                            let _ = stream.ExpectIdent("Expected field name in record type pattern")
                            stream.SkipNewlines()
                            stream.Match(Colon)
                        with _ ->
                            false
                    stream.Restore(mark)

                    if isTypeRefPattern then
                        let fields = ResizeArray<string * TypeRef>()
                        let seen = System.Collections.Generic.HashSet<string>()
                        let parseTypeField () =
                            stream.SkipNewlines()
                            let nameTok = stream.ExpectIdent("Expected field name in record type pattern")
                            let name =
                                match nameTok.Kind with
                                | Ident n -> n
                                | _ -> ""
                            if not (seen.Add name) then
                                raise (ParseException { Message = $"Duplicate field '{name}' in record type pattern"; Span = nameTok.Span })
                            stream.SkipNewlines()
                            stream.Expect(Colon, "Expected ':' in record type pattern field") |> ignore
                            let t = parseTypeRef()
                            fields.Add(name, t)
                        parseTypeField()
                        while stream.Match(Semicolon) do
                            if stream.Peek().Kind <> RBrace then
                                parseTypeField()
                        let rb = stream.Expect(RBrace, "Expected '}' in record type pattern")
                        PTypeRef(TRRecord(fields |> Seq.toList), mkSpanFrom lb.Span rb.Span)
                    else
                        let fields = ResizeArray<string * Pattern>()
                        let seen = System.Collections.Generic.HashSet<string>()
                        let parseField () =
                            stream.SkipNewlines()
                            let nameTok = stream.ExpectIdent("Expected field name in record pattern")
                            let name =
                                match nameTok.Kind with
                                | Ident n -> n
                                | _ -> ""
                            if not (seen.Add name) then
                                raise (ParseException { Message = $"Duplicate field '{name}' in record pattern"; Span = nameTok.Span })
                            stream.SkipNewlines()
                            stream.Expect(Equals, "Expected '=' in record pattern field") |> ignore
                            let p = parsePatternCons()
                            fields.Add(name, p)
                        parseField()
                        while stream.Match(Semicolon) do
                            if stream.Peek().Kind <> RBrace then
                                parseField()
                        let rb = stream.Expect(RBrace, "Expected '}' in record pattern")
                        PRecord(fields |> Seq.toList, mkSpanFrom lb.Span rb.Span)
            | _ -> raise (ParseException { Message = "Unexpected token in pattern"; Span = t.Span })

        and parsePatternCons () : Pattern =
            let left = parsePattern()
            if stream.Match(Cons) then
                let right = parsePatternCons()
                PCons(left, right, mkSpanFrom (Ast.spanOfPattern left) (Ast.spanOfPattern right))
            else
                left

        and parseEntryExpr () : Expr =
            let previousIndentedApplication = allowIndentedApplication
            let previousBinaryNewlineSkipping = allowBinaryNewlineSkipping
            allowIndentedApplication <- false
            allowBinaryNewlineSkipping <- false
            try
                parseExpr()
            finally
                allowIndentedApplication <- previousIndentedApplication
                allowBinaryNewlineSkipping <- previousBinaryNewlineSkipping

        and parsePrimary () : Expr =
            stream.SkipNewlines()
            let t = stream.Peek()
            match t.Kind with
            | IntLit _ | FloatLit _ | StringLit _ | BoolLit _ ->
                let t = stream.Next()
                ELiteral(parseLiteral t, t.Span)
            | InterpString raw ->
                let t = stream.Next()
                parseInterpolatedString raw t.Span
            | Ident name ->
                let t = stream.Next()
                match name with
                | "None" -> ENone t.Span
                | "Some" ->
                    let arg = parsePrimary()
                    ESome(arg, mkSpanFrom t.Span (Ast.spanOfExpr arg))
                | _ -> EVar(name, t.Span)
            | LParen ->
                let lp = stream.Next()
                if stream.Peek().Kind = RParen then
                    let rp = stream.Next()
                    EUnit (mkSpanFrom lp.Span rp.Span)
                else
                    let first = parseExpr()
                    if stream.Match(Comma) then
                        let elements = ResizeArray<Expr>()
                        elements.Add(first)
                        elements.Add(parseExpr())
                        while stream.Match(Comma) do
                            elements.Add(parseExpr())
                        let rp = stream.Expect(RParen, "Expected ')' after tuple expression")
                        ETuple(elements |> Seq.toList, mkSpanFrom lp.Span rp.Span)
                    else
                        let rp = stream.Expect(RParen, "Expected ')' after expression")
                        EParen(first, mkSpanFrom lp.Span rp.Span)
            | LBracket ->
                let lbIndex = stream.Index
                let lb = stream.Next()
                let immediateMultiline = stream.Peek().Kind = Newline
                let hasSameLinePrefix = hasNonLayoutTokenBeforeOnSameLine lbIndex lb.Span.Start.Line
                if immediateMultiline && hasSameLinePrefix then
                    raise (ParseException { Message = "For multiline list literals, '[' must be on its own line"; Span = lb.Span })
                consumeLayoutSeparators() |> ignore
                if stream.Match(RBracket) then
                    EList([], mkSpanFrom lb.Span lb.Span)
                else
                    let first = parseEntryExpr()
                    if stream.Match(RangeDots) then
                        let second = parseEntryExpr()
                        if stream.Match(Semicolon) || stream.Match(RangeDots) then
                            raise (ParseException { Message = "Invalid range syntax"; Span = stream.Peek().Span })
                        let rb = stream.Expect(RBracket, "Expected ']' in range expression")
                        ERange(first, second, mkSpanFrom lb.Span rb.Span)
                    else
                        let elements = ResizeArray<Expr>()
                        elements.Add(first)
                        let mutable keepParsing = true
                        while keepParsing do
                            let hasSeparator =
                                if stream.Match(Semicolon) then
                                    consumeLayoutSeparators() |> ignore
                                    true
                                else
                                    consumeLayoutSeparators()
                            if hasSeparator && stream.Peek().Kind <> RBracket then
                                elements.Add(parseEntryExpr())
                            else
                                keepParsing <- false
                        let rb = stream.Expect(RBracket, "Expected ']' in list literal")
                        EList(elements |> Seq.toList, mkSpanFrom lb.Span rb.Span)
            | LBrace ->
                let lbIndex = stream.Index
                let lb = stream.Next()
                let immediateMultiline = stream.Peek().Kind = Newline
                let hasSameLinePrefix = hasNonLayoutTokenBeforeOnSameLine lbIndex lb.Span.Start.Line
                if immediateMultiline && hasSameLinePrefix then
                    raise (ParseException { Message = "For multiline record/map literals, '{' must be on its own line"; Span = lb.Span })
                consumeLayoutSeparators() |> ignore
                if stream.Match(RBrace) then
                    EMap([], mkSpanFrom lb.Span lb.Span)
                else
                    if stream.Match(Bar) then
                        if stream.Peek().Kind = Bar then
                            raise (ParseException { Message = "Structural record literal must define at least one field"; Span = stream.Peek().Span })
                        let mark = stream.Mark()
                        let tryStructuralRecordUpdate () =
                            let baseExpr = parseExpr()
                            stream.SkipNewlines()
                            if not (stream.Match(With)) then
                                stream.Restore(mark)
                                None
                            else
                                let updates = ResizeArray<string * Expr>()
                                let parseUpdateField () =
                                    let nameTok = stream.ExpectIdent("Expected field name in structural record update")
                                    let name =
                                        match nameTok.Kind with
                                        | Ident n -> n
                                        | _ -> ""
                                    stream.SkipNewlines()
                                    stream.Expect(Equals, "Expected '=' in structural record update field") |> ignore
                                    let value = parseEntryExpr()
                                    updates.Add(name, value)
                                parseUpdateField()
                                let mutable keepParsing = true
                                while keepParsing do
                                    let hasSeparator =
                                        if stream.Match(Semicolon) then
                                            consumeLayoutSeparators() |> ignore
                                            true
                                        else
                                            consumeLayoutSeparators()
                                    if hasSeparator && stream.Peek().Kind <> Bar then
                                        parseUpdateField()
                                    else
                                        keepParsing <- false
                                stream.Expect(Bar, "Expected '|' in structural record update") |> ignore
                                let rb = stream.Expect(RBrace, "Expected '}' in structural record update")
                                Some (EStructuralRecordUpdate(baseExpr, updates |> Seq.toList, mkSpanFrom lb.Span rb.Span))
                        match tryStructuralRecordUpdate() with
                        | Some updateExpr -> updateExpr
                        | None ->
                            let fields = ResizeArray<string * Expr>()
                            let parseField () =
                                let nameTok = stream.ExpectIdent("Expected field name in structural record literal")
                                let name =
                                    match nameTok.Kind with
                                    | Ident n -> n
                                    | _ -> ""
                                stream.SkipNewlines()
                                stream.Expect(Equals, "Expected '=' in structural record field") |> ignore
                                let value = parseEntryExpr()
                                fields.Add(name, value)
                            parseField()
                            let mutable keepParsing = true
                            while keepParsing do
                                let hasSeparator =
                                    if stream.Match(Semicolon) then
                                        consumeLayoutSeparators() |> ignore
                                        true
                                    else
                                        consumeLayoutSeparators()
                                if hasSeparator && stream.Peek().Kind <> Bar then
                                    parseField()
                                else
                                    keepParsing <- false
                            stream.Expect(Bar, "Expected '|' in structural record literal") |> ignore
                            let rb = stream.Expect(RBrace, "Expected '}' in structural record literal")
                            EStructuralRecord(fields |> Seq.toList, mkSpanFrom lb.Span rb.Span)
                    else
                    if stream.Peek().Kind = LBracket || stream.Peek().Kind = RangeDots then
                        let parseMapEntry () =
                            if stream.Match(RangeDots) then
                                MESpread (parseEntryExpr())
                            else
                                stream.Expect(LBracket, "Expected '[' in map entry key") |> ignore
                                let keyExpr = parseExpr()
                                stream.Expect(RBracket, "Expected ']' after map entry key") |> ignore
                                stream.SkipNewlines()
                                stream.Expect(Equals, "Expected '=' in map entry") |> ignore
                                let value = parseEntryExpr()
                                MEKeyValue (keyExpr, value)

                        let entries = ResizeArray<MapEntry>()
                        entries.Add(parseMapEntry())

                        let mutable keepParsing = true
                        while keepParsing do
                            let hasSeparator =
                                if stream.Match(Semicolon) then
                                    consumeLayoutSeparators() |> ignore
                                    true
                                else
                                    consumeLayoutSeparators()
                            if hasSeparator then
                                if stream.Peek().Kind = RBrace then
                                    keepParsing <- false
                                else
                                    entries.Add(parseMapEntry())
                            else
                                if stream.Peek().Kind = RBrace then
                                    keepParsing <- false
                                else
                                    raise (ParseException { Message = "Expected ';', newline, or '}' in map literal"; Span = stream.Peek().Span })

                        let rb = stream.Expect(RBrace, "Expected '}' in map literal")
                        EMap(entries |> Seq.toList, mkSpanFrom lb.Span rb.Span)
                    else
                        let mark = stream.Mark()
                        let tryRecordUpdate () =
                            let baseExpr = parseExpr()
                            stream.SkipNewlines()
                            if not (stream.Match(With)) then
                                stream.Restore(mark)
                                None
                            else
                                let updates = ResizeArray<string * Expr>()
                                let parseUpdateField () =
                                    let nameTok = stream.ExpectIdent("Expected field name in record update")
                                    let name =
                                        match nameTok.Kind with
                                        | Ident n -> n
                                        | _ -> ""
                                    stream.SkipNewlines()
                                    stream.Expect(Equals, "Expected '=' in record update field") |> ignore
                                    let value = parseEntryExpr()
                                    updates.Add(name, value)
                                parseUpdateField()
                                let mutable keepParsing = true
                                while keepParsing do
                                    let hasSeparator =
                                        if stream.Match(Semicolon) then
                                            consumeLayoutSeparators() |> ignore
                                            true
                                        else
                                            consumeLayoutSeparators()
                                    if hasSeparator && stream.Peek().Kind <> RBrace then
                                        parseUpdateField()
                                    else
                                        keepParsing <- false
                                let rb = stream.Expect(RBrace, "Expected '}' in record update")
                                Some (ERecordUpdate(baseExpr, updates |> Seq.toList, mkSpanFrom lb.Span rb.Span))
                        match tryRecordUpdate() with
                        | Some updateExpr -> updateExpr
                        | None ->
                            let fields = ResizeArray<string * Expr>()
                            let parseField () =
                                let nameTok = stream.ExpectIdent("Expected field name in record literal")
                                let name =
                                    match nameTok.Kind with
                                    | Ident n -> n
                                    | _ -> ""
                                stream.SkipNewlines()
                                stream.Expect(Equals, "Expected '=' in record field") |> ignore
                                let value = parseEntryExpr()
                                fields.Add(name, value)
                            parseField()
                            let mutable keepParsing = true
                            while keepParsing do
                                let hasSeparator =
                                    if stream.Match(Semicolon) then
                                        consumeLayoutSeparators() |> ignore
                                        true
                                    else
                                        consumeLayoutSeparators()
                                if hasSeparator && stream.Peek().Kind <> RBrace then
                                    parseField()
                                else
                                    keepParsing <- false
                            let rb = stream.Expect(RBrace, "Expected '}' in record literal")
                            ERecord(fields |> Seq.toList, mkSpanFrom lb.Span rb.Span)
            | Let ->
                parseLetExpr()
            | Fun ->
                parseLambda()
            | If ->
                parseIf()
            | Raise ->
                let raiseTok = stream.Next()
                let value = parsePrimary()
                ERaise(value, mkSpanFrom raiseTok.Span (Ast.spanOfExpr value))
            | For ->
                parseFor()
            | Match ->
                parseMatch()
            | Typeof ->
                let t = stream.Next()
                let nameTok = stream.ExpectIdent("Expected type name after 'typeof'")
                let name =
                    match nameTok.Kind with
                    | Ident n -> n
                    | _ -> ""
                ETypeOf(name, mkSpanFrom t.Span nameTok.Span)
            | Nameof ->
                let t = stream.Next()
                let nameTok = stream.ExpectIdent("Expected identifier after 'nameof'")
                let name =
                    match nameTok.Kind with
                    | Ident n -> n
                    | _ -> ""
                ENameOf(name, mkSpanFrom t.Span nameTok.Span)
            | Indent ->
                raise (ParseException { Message = "Indentation error: unexpected indent in expression"; Span = t.Span })
            | Dedent ->
                raise (ParseException { Message = "Indentation error: unexpected dedent in expression"; Span = t.Span })
            | _ -> raise (ParseException { Message = "Unexpected token in expression"; Span = t.Span })

        and parsePostfix () : Expr =
            let mutable expr = parsePrimary()
            let mutable keepGoing = true
            while keepGoing do
                if stream.Match(Dot) then
                    let fieldTok = stream.ExpectIdent("Expected field name after '.'")
                    let fieldName =
                        match fieldTok.Kind with
                        | Ident n -> n
                        | _ -> ""
                    expr <- EFieldGet(expr, fieldName, mkSpanFrom (Ast.spanOfExpr expr) fieldTok.Span)
                elif stream.Peek().Kind = LBracket then
                    let lb = stream.Peek()
                    let targetSpan = Ast.spanOfExpr expr
                    let isAdjacent =
                        lb.Span.Start.Line = targetSpan.End.Line
                        && lb.Span.Start.Column = targetSpan.End.Column
                    let hasIndexerPayload = stream.PeekAt(1).Kind <> RBracket
                    if isAdjacent && hasIndexerPayload then
                        stream.Next() |> ignore
                        let keyExpr = parseExpr()
                        let rb = stream.Expect(RBracket, "Expected ']' after index expression")
                        expr <- EIndexGet(expr, keyExpr, mkSpanFrom targetSpan rb.Span)
                    else
                        keepGoing <- false
                else
                    keepGoing <- false
            expr

        and parseApplication () : Expr =
            let mutable expr = parsePostfix()
            let mutable keepGoing = true
            let mutable hasMultilineArgBlock = false
            while keepGoing do
                let next = stream.Peek()
                let sameLineAsExpr = next.Span.Start.Line = (Ast.spanOfExpr expr).End.Line
                if sameLineAsExpr && isStartAtom next.Kind then
                    let arg = parsePostfix()
                    expr <- EApply(expr, arg, mkSpanFrom (Ast.spanOfExpr expr) (Ast.spanOfExpr arg))
                else
                    let mark = stream.Mark()
                    let mutable sawNewline = false
                    while stream.Match(Newline) do
                        sawNewline <- true
                    if sawNewline && allowIndentedApplication then
                        let consumedIndent = stream.Match(Indent)
                        if consumedIndent then
                            hasMultilineArgBlock <- true
                        let candidate = stream.Peek()
                        let calleeColumn = (Ast.spanOfExpr expr).Start.Column
                        let isIndentedContinuation = candidate.Span.Start.Column > calleeColumn
                        if isIndentedContinuation && isStartAtom candidate.Kind then
                            let arg = parsePostfix()
                            expr <- EApply(expr, arg, mkSpanFrom (Ast.spanOfExpr expr) (Ast.spanOfExpr arg))
                        elif hasMultilineArgBlock && candidate.Kind = Dedent then
                            stream.Next() |> ignore
                            hasMultilineArgBlock <- false
                            keepGoing <- false
                        else
                            stream.Restore(mark)
                            keepGoing <- false
                    else
                        stream.Restore(mark)
                        keepGoing <- false
            expr

        and parseBinary (minPrec: int) : Expr =
            let mutable left = parseApplication()
            let precedence op =
                match op with
                | Star | Slash | Percent -> 7
                | Plus | Minus -> 6
                | Append -> 5
                | Cons -> 4
                | Equals | Less | Greater | LessEqual | GreaterEqual -> 3
                | AndAnd -> 2
                | OrOr -> 1
                | PipeForward -> 0
                | _ -> -1
            let opToString op =
                match op with
                | Plus -> "+"
                | Minus -> "-"
                | Star -> "*"
                | Slash -> "/"
                | Percent -> "%"
                | Equals -> "="
                | Less -> "<"
                | Greater -> ">"
                | LessEqual -> "<="
                | GreaterEqual -> ">="
                | AndAnd -> "&&"
                | OrOr -> "||"
                | PipeForward -> "|>"
                | Cons -> "::"
                | Append -> "@"
                | _ -> "?"

            let skipContinuationDedents () =
                if allowBinaryNewlineSkipping then
                    let mutable continueLoop = true
                    while continueLoop do
                        stream.SkipNewlines()
                        let mark = stream.Mark()
                        if stream.Match(Dedent) then
                            let nextPrec = precedence (stream.Peek().Kind)
                            if nextPrec >= minPrec then
                                ()
                            else
                                stream.Restore(mark)
                                continueLoop <- false
                        else
                            stream.Restore(mark)
                            continueLoop <- false

            let mutable looping = true
            while looping do
                skipContinuationDedents()
                let next = stream.Peek()
                let prec = precedence next.Kind
                if prec >= minPrec then
                    let opTok = stream.Next()
                    let right = parseBinary (prec + 1)
                    left <- EBinOp(opToString opTok.Kind, left, right, mkSpanFrom (Ast.spanOfExpr left) (Ast.spanOfExpr right))
                else
                    looping <- false
            left

        and parseIf () : Expr =
            let ifTok = stream.Expect(If, "Expected 'if'")
            parseIfAfterKeyword ifTok.Span

        and parseIfAfterKeyword (startSpan: Span) : Expr =
            let cond = parseExpr()
            stream.SkipNewlines()
            stream.Expect(Then, "Expected 'then'") |> ignore
            let thenExpr = parseExprOrBlock()
            stream.SkipNewlines()
            let elseExpr =
                match stream.Peek().Kind with
                | Else ->
                    stream.Next() |> ignore
                    parseExprOrBlock()
                | Elif ->
                    let elifTok = stream.Next()
                    parseIfAfterKeyword elifTok.Span
                | _ ->
                    raise (ParseException { Message = "Expected 'else' or 'elif'"; Span = stream.Peek().Span })
            EIf(cond, thenExpr, elseExpr, mkSpanFrom startSpan (Ast.spanOfExpr elseExpr))

        and parseFor () : Expr =
            let forTok = stream.Expect(For, "Expected 'for'")
            let nameTok = stream.ExpectIdent("Expected identifier after 'for'")
            let name =
                match nameTok.Kind with
                | Ident n -> n
                | _ -> ""
            stream.SkipNewlines()
            match stream.Peek().Kind with
            | Ident "in" ->
                stream.Next() |> ignore
            | _ ->
                raise (ParseException { Message = "Expected 'in' after for loop variable"; Span = stream.Peek().Span })
            let source = parsePostfix()
            stream.SkipNewlines()
            match stream.Peek().Kind with
            | Ident "do" ->
                stream.Next() |> ignore
            | _ ->
                raise (ParseException { Message = "Expected 'do' in for loop"; Span = stream.Peek().Span })
            let body = parseExprOrBlock()
            EFor(name, source, body, mkSpanFrom forTok.Span (Ast.spanOfExpr body))

        and parseLambda () : Expr =
            let funTok = stream.Expect(Fun, "Expected 'fun'")
            let parameters = ResizeArray<Param>()
            let mutable doneParams = false
            while not doneParams do
                stream.SkipNewlines()
                match stream.Peek().Kind with
                | Ident _ | LParen ->
                    parameters.Add(parseParam())
                | _ ->
                    doneParams <- true
            if parameters.Count = 0 then
                raise (ParseException { Message = "Expected at least one lambda parameter"; Span = stream.Peek().Span })
            stream.SkipNewlines()
            stream.Expect(Arrow, "Expected '->' in lambda") |> ignore
            let body = parseExprOrBlock()
            Seq.foldBack
                (fun param acc -> ELambda(param, acc, mkSpanFrom funTok.Span (Ast.spanOfExpr acc)))
                parameters
                body

        and parseMatch () : Expr =
            let matchTok = stream.Expect(Match, "Expected 'match'")
            let expr = parseExpr()
            stream.SkipNewlines()
            stream.Expect(With, "Expected 'with' in match") |> ignore
            let cases = ResizeArray<Pattern * Expr option * Expr * Span>()
            let rec parseCase () =
                stream.SkipNewlines()
                if stream.Match(Bar) then ()
                let pat = parsePatternCons()
                stream.SkipNewlines()
                let guard =
                    if stream.Match(When) then
                        let g = parseExpr()
                        stream.SkipNewlines()
                        Some g
                    else
                        None
                stream.Expect(Arrow, "Expected '->' in match case") |> ignore
                let body = parseExprOrBlock()
                let span = mkSpanFrom (Ast.spanOfPattern pat) (Ast.spanOfExpr body)
                cases.Add(pat, guard, body, span)
            if stream.Match(Newline) then
                while stream.Match(Newline) do ()
                let hasIndentedCaseBlock = stream.Match(Indent)
                let firstCaseToken = stream.Peek()
                if firstCaseToken.Kind = EOF || firstCaseToken.Kind = Dedent then
                    raise (ParseException { Message = "Expected match case"; Span = firstCaseToken.Span })
                let caseColumn = firstCaseToken.Span.Start.Column
                if caseColumn < matchTok.Span.Start.Column then
                    raise (ParseException { Message = "Match cases must start at match column or deeper"; Span = firstCaseToken.Span })
                let ensureAlignedCaseStart () =
                    let t = stream.Peek()
                    if t.Span.Start.Column <> caseColumn then
                        raise (ParseException { Message = "Match case columns must align"; Span = t.Span })
                ensureAlignedCaseStart()
                parseCase()
                let mutable doneCases = false
                while not doneCases do
                    stream.SkipNewlines()
                    match stream.Peek().Kind with
                    | Bar ->
                        ensureAlignedCaseStart()
                        parseCase()
                    | Dedent when hasIndentedCaseBlock ->
                        stream.Next() |> ignore
                        doneCases <- true
                    | Indent ->
                        raise (ParseException { Message = "Match case columns must align"; Span = stream.Peek().Span })
                    | _ ->
                        if hasIndentedCaseBlock then
                            raise (ParseException { Message = "Expected match case or dedent"; Span = stream.Peek().Span })
                        else
                            doneCases <- true
            else
                parseCase()
                let mutable doneCases = false
                while not doneCases do
                    stream.SkipNewlines()
                    match stream.Peek().Kind with
                    | Bar -> parseCase()
                    | Dedent | EOF -> doneCases <- true
                    | _ -> doneCases <- true
            EMatch(expr, cases |> Seq.toList, mkSpanFrom matchTok.Span (Ast.spanOfExpr expr))

        and parseLetExpr () : Expr =
            let letTok = stream.Expect(Let, "Expected 'let'")
            let isRec = stream.Match(Rec)
            let parseTupleLetPattern () =
                let pattern = parsePatternCons()
                match pattern with
                | PTuple _ -> pattern
                | _ ->
                    raise (ParseException { Message = "Only tuple patterns are supported in let bindings"; Span = Ast.spanOfPattern pattern })

            let bindingName, args, tuplePatternOpt, bindingSpan =
                if isRec then
                    if stream.Peek().Kind = LParen then
                        raise (ParseException { Message = "'let rec' does not support pattern bindings"; Span = stream.Peek().Span })
                    let nameTok = stream.ExpectIdent("Expected identifier after 'let'")
                    let name = match nameTok.Kind with Ident n -> n | _ -> ""
                    let args = parseParamsAligned()
                    if args.Count = 0 then
                        raise (ParseException { Message = "'let rec' requires at least one function argument"; Span = nameTok.Span })
                    name, args, None, nameTok.Span
                else
                    match stream.Peek().Kind with
                    | LParen ->
                        let pattern = parseTupleLetPattern ()
                        "", ResizeArray<Param>(), Some pattern, Ast.spanOfPattern pattern
                    | _ ->
                        let nameTok = stream.ExpectIdent("Expected identifier after 'let'")
                        let name = match nameTok.Kind with Ident n -> n | _ -> ""
                        name, parseParamsAligned(), None, nameTok.Span
            stream.SkipNewlines()
            let returnAnnotation =
                if stream.Match(Colon) then
                    if args.Count = 0 then
                        raise (ParseException { Message = "Return type annotations are only supported on function let bindings"; Span = stream.Peek().Span })
                    Some (parseTypeRef())
                else
                    None
            if stream.Peek().Kind = Colon then
                raise (ParseException { Message = "Annotated parameters must be parenthesized as (x: T)"; Span = stream.Peek().Span })
            stream.Expect(Equals, "Expected '=' in let binding") |> ignore
            let value = parseExprOrBlock()
            if isRec then
                let bindings = ResizeArray<string * Param list * TypeRef option * Expr * Span>()
                let firstSpan = mkSpanFrom bindingSpan (Ast.spanOfExpr value)
                bindings.Add(bindingName, args |> Seq.toList, returnAnnotation, value, firstSpan)
                let mutable doneBindings = false
                while not doneBindings do
                    stream.SkipNewlines()
                    if stream.Match(And) then
                        let nextNameTok = stream.ExpectIdent("Expected identifier after 'and'")
                        let nextName = match nextNameTok.Kind with Ident n -> n | _ -> ""
                        let nextArgs = parseParamsAligned()
                        if nextArgs.Count = 0 then
                            raise (ParseException { Message = "'let rec ... and ...' requires function arguments for each binding"; Span = nextNameTok.Span })
                        stream.SkipNewlines()
                        let nextReturnAnnotation =
                            if stream.Match(Colon) then
                                Some (parseTypeRef())
                            else
                                None
                        if stream.Peek().Kind = Colon then
                            raise (ParseException { Message = "Annotated parameters must be parenthesized as (x: T)"; Span = stream.Peek().Span })
                        stream.Expect(Equals, "Expected '=' in let binding") |> ignore
                        let nextValue = parseExprOrBlock()
                        let nextSpan = mkSpanFrom nextNameTok.Span (Ast.spanOfExpr nextValue)
                        bindings.Add(nextName, nextArgs |> Seq.toList, nextReturnAnnotation, nextValue, nextSpan)
                    else
                        doneBindings <- true
                match stream.Peek().Kind with
                | Ident "in" ->
                    raise (ParseException { Message = "'in' keyword is not supported"; Span = stream.Peek().Span })
                | _ ->
                    let body =
                        match stream.Peek().Kind with
                        | Indent ->
                            stream.Next() |> ignore
                            parseBlock()
                        | _ -> parseExprOrBlock()
                    if bindings.Count = 1 then
                        let funValue = Seq.foldBack (fun arg acc -> ELambda(arg, acc, Ast.spanOfExpr acc)) args value
                        ELet(bindingName, funValue, body, true, returnAnnotation, mkSpanFrom letTok.Span (Ast.spanOfExpr body))
                    else
                        ELetRecGroup(bindings |> Seq.toList, body, mkSpanFrom letTok.Span (Ast.spanOfExpr body))
            else
                match stream.Peek().Kind with
                | Ident "in" ->
                    raise (ParseException { Message = "'in' keyword is not supported"; Span = stream.Peek().Span })
                | _ ->
                    // parseExpr may already have consumed trailing newline before a block body.
                    let body =
                        match stream.Peek().Kind with
                        | Indent ->
                            stream.Next() |> ignore
                            parseBlock()
                        | _ -> parseExprOrBlock()
                    match tuplePatternOpt with
                    | Some pattern when args.Count > 0 ->
                        raise (ParseException { Message = "Tuple let binding cannot take function parameters"; Span = Ast.spanOfPattern pattern })
                    | Some pattern ->
                        ELetPattern(pattern, value, body, mkSpanFrom letTok.Span (Ast.spanOfExpr body))
                    | None ->
                        let funValue = Seq.foldBack (fun arg acc -> ELambda(arg, acc, Ast.spanOfExpr acc)) args value
                        ELet(bindingName, funValue, body, false, returnAnnotation, mkSpanFrom letTok.Span (Ast.spanOfExpr body))

        and parseExprOrBlock () : Expr =
            let mutable sawNewline = false
            while stream.Match(Newline) do
                sawNewline <- true
            if sawNewline && stream.Match(Indent) then
                parseBlock()
            else
                parseExpr()

        and parseExpr () : Expr =
            parseBinary 0

        and parseBlock () : Expr =
            let statements = ResizeArray<Stmt>()
            let mutable doneBlock = false
            let mutable blockStatementColumn: int option = None
            while not doneBlock do
                stream.SkipNewlines()
                match stream.Peek().Kind with
                | Dedent ->
                    stream.Next() |> ignore
                    stream.SkipNewlines()
                    let rec findNextNonLayout (offset: int) =
                        match stream.PeekAt(offset).Kind with
                        | Newline
                        | Indent
                        | Dedent -> findNextNonLayout (offset + 1)
                        | _ -> stream.PeekAt(offset)
                    let nextSignificant = findNextNonLayout 0
                    let next = stream.Peek()
                    let continuationDedent =
                        match blockStatementColumn with
                        | Some col ->
                            match nextSignificant.Kind with
                            | EOF
                            | Dedent
                            | RParen
                            | RBracket
                            | RBrace -> false
                            | _ -> nextSignificant.Span.Start.Column >= col
                        | None -> false
                    if not continuationDedent then
                        doneBlock <- true
                | RParen
                | RBracket
                | RBrace ->
                    doneBlock <- true
                | EOF -> doneBlock <- true
                | Let ->
                    if blockStatementColumn.IsNone then
                        blockStatementColumn <- Some ((stream.Peek()).Span.Start.Column)
                    statements.Add(parseLetStmt false)
                | Import ->
                    raise (ParseException { Message = "'import' is only supported at top level"; Span = stream.Peek().Span })
                | Hash when stream.PeekAt(1).Kind = Import || stream.PeekAt(1).Kind = Include ->
                    raise (ParseException { Message = "Use 'import \"file.fss\"' at top level"; Span = stream.Peek().Span })
                | Module ->
                    raise (ParseException { Message = "'module' declarations were removed; module names are derived from imported filenames"; Span = stream.Peek().Span })
                | LBracket when stream.PeekAt(1).Kind = Less ->
                    raise (ParseException { Message = "'[<export>]' is only supported at top level"; Span = stream.Peek().Span })
                | _ ->
                    if blockStatementColumn.IsNone then
                        blockStatementColumn <- Some ((stream.Peek()).Span.Start.Column)
                    let expr = parseExpr()
                    statements.Add(SExpr expr)
            if statements.Count = 0 then
                raise (ParseException { Message = "Empty block"; Span = stream.Peek().Span })
            let rec desugar (stmts: Stmt list) =
                match stmts with
                | [] -> EUnit (stream.Peek().Span)
                | [SExpr e] -> e
                | [SLet(_, _, _, _, _, _, span)] ->
                    raise (ParseException { Message = "Block cannot end with a let binding; add a final expression"; Span = span })
                | [SLetPattern(_, _, _, span)] ->
                    raise (ParseException { Message = "Block cannot end with a let binding; add a final expression"; Span = span })
                | [SLetRecGroup(_, _, span)] ->
                    raise (ParseException { Message = "Block cannot end with a let binding; add a final expression"; Span = span })
                | SLet(name, args, returnAnnotation, value, isRec, _, span) :: rest ->
                    let valExpr = Seq.foldBack (fun arg acc -> ELambda(arg, acc, span)) args value
                    let body = desugar rest
                    ELet(name, valExpr, body, isRec, returnAnnotation, mkSpanFrom span (Ast.spanOfExpr body))
                | SLetPattern(pattern, value, _, span) :: rest ->
                    let body = desugar rest
                    ELetPattern(pattern, value, body, mkSpanFrom span (Ast.spanOfExpr body))
                | SLetRecGroup(bindings, _, span) :: rest ->
                    let body = desugar rest
                    ELetRecGroup(bindings, body, mkSpanFrom span (Ast.spanOfExpr body))
                | SExpr e :: rest ->
                    let body = desugar rest
                    ELet("_", e, body, false, None, mkSpanFrom (Ast.spanOfExpr e) (Ast.spanOfExpr body))
                | SType def :: _ ->
                    raise (ParseException { Message = "Type declarations are only supported at top level"; Span = def.Span })
                | SImport (_, _, span) :: _ ->
                    raise (ParseException { Message = "'import' is only supported at top level"; Span = span })
            desugar (statements |> Seq.toList)

        and parseLetStmt (isExported: bool) : Stmt =
            let letTok = stream.Next()
            let isRec = stream.Match(Rec)
            let parseTupleLetPattern () =
                let pattern = parsePatternCons()
                match pattern with
                | PTuple _ -> pattern
                | _ ->
                    raise (ParseException { Message = "Only tuple patterns are supported in let bindings"; Span = Ast.spanOfPattern pattern })

            let bindingName, args, tuplePatternOpt, bindingSpan =
                if isRec then
                    if stream.Peek().Kind = LParen then
                        raise (ParseException { Message = "'let rec' does not support pattern bindings"; Span = stream.Peek().Span })
                    let nameTok = stream.ExpectIdent("Expected identifier after 'let'")
                    let name = match nameTok.Kind with Ident n -> n | _ -> ""
                    let args = parseParamsAligned()
                    if args.Count = 0 then
                        raise (ParseException { Message = "'let rec' requires at least one function argument"; Span = nameTok.Span })
                    name, args, None, nameTok.Span
                else
                    match stream.Peek().Kind with
                    | LParen ->
                        if isExported then
                            raise (ParseException { Message = "Exported let binding requires a single identifier, not a tuple pattern"; Span = stream.Peek().Span })
                        let pattern = parseTupleLetPattern ()
                        "", ResizeArray<Param>(), Some pattern, Ast.spanOfPattern pattern
                    | _ ->
                        let nameTok = stream.ExpectIdent("Expected identifier after 'let'")
                        let name = match nameTok.Kind with Ident n -> n | _ -> ""
                        name, parseParamsAligned(), None, nameTok.Span
            stream.SkipNewlines()
            let returnAnnotation =
                if stream.Match(Colon) then
                    if args.Count = 0 then
                        raise (ParseException { Message = "Return type annotations are only supported on function let bindings"; Span = stream.Peek().Span })
                    Some (parseTypeRef())
                else
                    None
            if stream.Peek().Kind = Colon then
                raise (ParseException { Message = "Annotated parameters must be parenthesized as (x: T)"; Span = stream.Peek().Span })
            stream.Expect(Equals, "Expected '=' in let binding") |> ignore
            let value = parseExprOrBlock()
            if isRec then
                let bindings = ResizeArray<string * Param list * TypeRef option * Expr * Span>()
                let firstSpan = mkSpanFrom bindingSpan (Ast.spanOfExpr value)
                bindings.Add(bindingName, args |> Seq.toList, returnAnnotation, value, firstSpan)
                let mutable doneBindings = false
                while not doneBindings do
                    stream.SkipNewlines()
                    if stream.Match(And) then
                        let nextNameTok = stream.ExpectIdent("Expected identifier after 'and'")
                        let nextName = match nextNameTok.Kind with Ident n -> n | _ -> ""
                        let nextArgs = parseParamsAligned()
                        if nextArgs.Count = 0 then
                            raise (ParseException { Message = "'let rec ... and ...' requires function arguments for each binding"; Span = nextNameTok.Span })
                        stream.SkipNewlines()
                        let nextReturnAnnotation =
                            if stream.Match(Colon) then
                                Some (parseTypeRef())
                            else
                                None
                        if stream.Peek().Kind = Colon then
                            raise (ParseException { Message = "Annotated parameters must be parenthesized as (x: T)"; Span = stream.Peek().Span })
                        stream.Expect(Equals, "Expected '=' in let binding") |> ignore
                        let nextValue = parseExprOrBlock()
                        let nextSpan = mkSpanFrom nextNameTok.Span (Ast.spanOfExpr nextValue)
                        bindings.Add(nextName, nextArgs |> Seq.toList, nextReturnAnnotation, nextValue, nextSpan)
                    else
                        doneBindings <- true
                if bindings.Count = 1 then
                    SLet(bindingName, args |> Seq.toList, returnAnnotation, value, true, isExported, mkSpanFrom letTok.Span (Ast.spanOfExpr value))
                else
                    let (_, _, _, _, lastSpan) = bindings.[bindings.Count - 1]
                    SLetRecGroup(bindings |> Seq.toList, isExported, mkSpanFrom letTok.Span lastSpan)
            else
                match tuplePatternOpt with
                | Some pattern when args.Count > 0 ->
                    raise (ParseException { Message = "Tuple let binding cannot take function parameters"; Span = Ast.spanOfPattern pattern })
                | Some pattern ->
                    SLetPattern(pattern, value, isExported, mkSpanFrom letTok.Span (Ast.spanOfExpr value))
                | None ->
                    SLet(bindingName, args |> Seq.toList, returnAnnotation, value, false, isExported, mkSpanFrom letTok.Span (Ast.spanOfExpr value))

        and parseAttributeName () =
            stream.Expect(LBracket, "Expected '[' in attribute") |> ignore
            stream.Expect(Less, "Expected '<' in attribute") |> ignore
            let nameTok = stream.ExpectIdent("Expected attribute name")
            let name =
                match nameTok.Kind with
                | Ident n -> n
                | _ -> ""
            stream.Expect(Greater, "Expected '>' in attribute") |> ignore
            stream.Expect(RBracket, "Expected ']' in attribute") |> ignore
            name, nameTok.Span

        and parseStmt () : Stmt =
            stream.SkipNewlines()

            if stream.Peek().Kind = Ident "export" && stream.PeekAt(1).Kind = Let then
                raise (ParseException { Message = "'export let' is no longer supported; use '[<export>] let ...'"; Span = stream.Peek().Span })

            let mutable isExported = false
            let mutable hasAttributes = false
            let mutable parsingAttributes = true
            while parsingAttributes do
                if stream.Peek().Kind = LBracket && stream.PeekAt(1).Kind = Less then
                    hasAttributes <- true
                    let name, span = parseAttributeName()
                    match name with
                    | "export" ->
                        if isExported then
                            raise (ParseException { Message = "Duplicate attribute 'export'"; Span = span })
                        isExported <- true
                    | _ ->
                        raise (ParseException { Message = $"Unknown attribute '{name}'"; Span = span })
                    stream.SkipNewlines()
                else
                    parsingAttributes <- false

            match stream.Peek().Kind with
            | Hash ->
                if hasAttributes || isExported then
                    raise (ParseException { Message = "Attributes are not supported on 'import' directives"; Span = stream.Peek().Span })
                let hashTok = stream.Next()
                match stream.Next().Kind with
                | Include ->
                    raise (ParseException { Message = "'#include' was removed in 0.32.0; use 'import \"file.fss\" as Alias'"; Span = hashTok.Span })
                | Import ->
                    raise (ParseException { Message = "'#import' was removed in 0.32.0; use 'import \"file.fss\" as Alias'"; Span = hashTok.Span })
                | _ ->
                    raise (ParseException { Message = "Unexpected '#'; use 'import \"file.fss\" as Alias'"; Span = hashTok.Span })
            | Import ->
                if hasAttributes || isExported then
                    raise (ParseException { Message = "Attributes are not supported on 'import' directives"; Span = stream.Peek().Span })
                let importTok = stream.Next()
                let pathTok = stream.Next()
                match pathTok.Kind with
                | StringLit path when not (System.String.IsNullOrWhiteSpace path) ->
                    stream.Expect(As, "Expected 'as' in import directive. Use: import \"file.fss\" as Alias") |> ignore
                    let aliasTok = stream.ExpectIdent("Expected module alias after 'as'")
                    let alias =
                        match aliasTok.Kind with
                        | Ident value -> value
                        | _ -> ""
                    SImport(alias, path, mkSpanFrom importTok.Span aliasTok.Span)
                | StringLit _ ->
                    raise (ParseException { Message = "Import path cannot be empty"; Span = pathTok.Span })
                | _ ->
                    raise (ParseException { Message = "Expected string literal import path after 'import'"; Span = pathTok.Span })
            | Module ->
                raise (ParseException { Message = "'module' declarations are not supported in scripts"; Span = stream.Peek().Span })
            | Type ->
                if isExported then
                    raise (ParseException { Message = "'[<export>]' is only valid for top-level let bindings"; Span = stream.Peek().Span })
                parseTypeDecl()
            | Let ->
                parseLetStmt isExported
            | _ when hasAttributes ->
                raise (ParseException { Message = "'[<export>]' must be followed by a top-level let binding"; Span = stream.Peek().Span })
            | _ ->
                let expr = parseExpr()
                SExpr expr

        let program = ResizeArray<Stmt>()
        stream.SkipNewlines()
        while not stream.AtEnd do
            match stream.Peek().Kind with
            | EOF -> stream.Next() |> ignore
            | Dedent -> stream.Next() |> ignore
            | _ ->
                let stmt = parseStmt()
                program.Add(stmt)
                stream.SkipNewlines()
        program |> Seq.toList

    let parseProgram (src: string) : Program =
        parseProgramWithSourceName None src
