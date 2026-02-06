namespace FScript.Core

module Parser =
    type private TokenStream(tokens: Token list) =
        let mutable index = 0
        member _.Peek() = tokens.[index]
        member _.PeekAt(offset: int) = tokens.[index + offset]
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
        | Ident _ | IntLit _ | FloatLit _ | StringLit _ | InterpString _ | BoolLit _ | LParen | LBracket | LBrace | Let | Fun | If | Raise | For | Match | Typeof -> true
        | _ -> false

    let private parseLiteral (t: Token) =
        match t.Kind with
        | IntLit v -> LInt v
        | FloatLit v -> LFloat v
        | StringLit v -> LString v
        | BoolLit v -> LBool v
        | _ -> raise (ParseException { Message = "Expected literal"; Span = t.Span })

    let rec parseProgram (src: string) : Program =
        let tokens = Lexer.tokenize src
        let stream = TokenStream(tokens)

        let parseSingleExpression (exprText: string) =
            let parsed = parseProgram exprText
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

        let rec parseTypeRefAtom () : TypeRef =
            stream.SkipNewlines()
            match stream.Peek().Kind with
            | Ident name ->
                stream.Next() |> ignore
                TRName name
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

        and parseTypeRef () : TypeRef =
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

        and parseTypeDecl () : Stmt =
            let typeTok = stream.Expect(Type, "Expected 'type'")
            let nameTok = stream.ExpectIdent("Expected identifier after 'type'")
            let name =
                match nameTok.Kind with
                | Ident n -> n
                | _ -> ""
            stream.SkipNewlines()
            stream.Expect(Equals, "Expected '=' in type declaration") |> ignore
            stream.SkipNewlines()
            stream.Expect(LBrace, "Expected '{' in record type declaration") |> ignore
            let fields = ResizeArray<string * TypeRef>()
            let parseField () =
                let fieldTok = stream.ExpectIdent("Expected field name in type declaration")
                let fieldName =
                    match fieldTok.Kind with
                    | Ident n -> n
                    | _ -> ""
                stream.SkipNewlines()
                stream.Expect(Colon, "Expected ':' after field name") |> ignore
                let fieldType = parseTypeRef()
                fields.Add(fieldName, fieldType)
            parseField()
            while stream.Match(Semicolon) do
                parseField()
            let rb = stream.Expect(RBrace, "Expected '}' in record type declaration")
            SType { Name = name; Fields = fields |> Seq.toList; Span = mkSpanFrom typeTok.Span rb.Span }

        and parsePattern () : Pattern =
            stream.SkipNewlines()
            let t = stream.Peek()
            match t.Kind with
            | Ident name when name = "_" ->
                let t = stream.Next()
                PWildcard t.Span
            | Ident name ->
                let t = stream.Next()
                match name with
                | "None" -> PNone t.Span
                | "Some" ->
                    let p = parsePattern()
                    PSome(p, mkSpanFrom t.Span (Ast.spanOfPattern p))
                | _ -> PVar(name, t.Span)
            | IntLit _ | FloatLit _ | StringLit _ | BoolLit _ ->
                let t = stream.Next()
                PLiteral(parseLiteral t, t.Span)
            | LBracket ->
                let lb = stream.Next()
                if stream.Match(RBracket) then
                    PNil (mkSpanFrom lb.Span lb.Span)
                else
                    raise (ParseException { Message = "Only empty list pattern [] supported"; Span = lb.Span })
            | _ -> raise (ParseException { Message = "Unexpected token in pattern"; Span = t.Span })

        and parsePatternCons () : Pattern =
            let left = parsePattern()
            if stream.Match(Cons) then
                let right = parsePatternCons()
                PCons(left, right, mkSpanFrom (Ast.spanOfPattern left) (Ast.spanOfPattern right))
            else
                left

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
                    stream.Expect(RParen, "Expected ')' after expression") |> ignore
                    first
            | LBracket ->
                let lb = stream.Next()
                if stream.Match(RBracket) then
                    EList([], mkSpanFrom lb.Span lb.Span)
                else
                    let first = parseExpr()
                    if stream.Match(RangeDots) then
                        let second = parseExpr()
                        if stream.Match(Semicolon) || stream.Match(RangeDots) then
                            raise (ParseException { Message = "Invalid range syntax"; Span = stream.Peek().Span })
                        let rb = stream.Expect(RBracket, "Expected ']' in range expression")
                        ERange(first, second, mkSpanFrom lb.Span rb.Span)
                    else
                        let elements = ResizeArray<Expr>()
                        elements.Add(first)
                        while stream.Match(Semicolon) do
                            elements.Add(parseExpr())
                        let rb = stream.Expect(RBracket, "Expected ']' in list literal")
                        EList(elements |> Seq.toList, mkSpanFrom lb.Span rb.Span)
            | LBrace ->
                let lb = stream.Next()
                if stream.Match(RBrace) then
                    ERecord([], mkSpanFrom lb.Span lb.Span)
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
                                let value = parseExpr()
                                updates.Add(name, value)
                            parseUpdateField()
                            while stream.Match(Semicolon) do
                                parseUpdateField()
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
                            let value = parseExpr()
                            fields.Add(name, value)
                        parseField()
                        while stream.Match(Semicolon) do
                            parseField()
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
                else
                    keepGoing <- false
            expr

        and parseApplication () : Expr =
            let mutable expr = parsePostfix()
            let mutable keepGoing = true
            while keepGoing do
                let next = stream.Peek()
                if isStartAtom next.Kind then
                    let arg = parsePostfix()
                    expr <- EApply(expr, arg, mkSpanFrom (Ast.spanOfExpr expr) (Ast.spanOfExpr arg))
                else
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

            let mutable looping = true
            while looping do
                stream.SkipNewlines()
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
            let nameTok = stream.ExpectIdent("Expected identifier after 'fun'")
            let argName = match nameTok.Kind with Ident n -> n | _ -> ""
            stream.SkipNewlines()
            stream.Expect(Arrow, "Expected '->' in lambda") |> ignore
            let body = parseExprOrBlock()
            ELambda(argName, body, mkSpanFrom funTok.Span (Ast.spanOfExpr body))

        and parseMatch () : Expr =
            let matchTok = stream.Expect(Match, "Expected 'match'")
            let expr = parseExpr()
            stream.SkipNewlines()
            stream.Expect(With, "Expected 'with' in match") |> ignore
            let cases = ResizeArray<Pattern * Expr * Span>()
            let rec parseCase () =
                stream.SkipNewlines()
                if stream.Match(Bar) then ()
                let pat = parsePatternCons()
                stream.SkipNewlines()
                stream.Expect(Arrow, "Expected '->' in match case") |> ignore
                let body = parseExprOrBlock()
                let span = mkSpanFrom (Ast.spanOfPattern pat) (Ast.spanOfExpr body)
                cases.Add(pat, body, span)
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
            let nameTok = stream.ExpectIdent("Expected identifier after 'let'")
            let name = match nameTok.Kind with Ident n -> n | _ -> ""
            let args = ResizeArray<string>()
            let mutable argsDone = false
            while not argsDone do
                stream.SkipNewlines()
                match stream.Peek().Kind with
                | Ident n ->
                    args.Add(n)
                    stream.Next() |> ignore
                | _ -> argsDone <- true
            if isRec && args.Count = 0 then
                raise (ParseException { Message = "'let rec' requires at least one function argument"; Span = nameTok.Span })
            stream.SkipNewlines()
            stream.Expect(Equals, "Expected '=' in let binding") |> ignore
            let value = parseExprOrBlock()
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
                let funValue = Seq.foldBack (fun arg acc -> ELambda(arg, acc, Ast.spanOfExpr acc)) args value
                ELet(name, funValue, body, isRec, mkSpanFrom letTok.Span (Ast.spanOfExpr body))

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
            while not doneBlock do
                stream.SkipNewlines()
                match stream.Peek().Kind with
                | Dedent ->
                    stream.Next() |> ignore
                    doneBlock <- true
                | EOF -> doneBlock <- true
                | Let ->
                    statements.Add(parseStmt())
                | _ ->
                    let expr = parseExpr()
                    statements.Add(SExpr expr)
            if statements.Count = 0 then
                raise (ParseException { Message = "Empty block"; Span = stream.Peek().Span })
            let rec desugar (stmts: Stmt list) =
                match stmts with
                | [] -> ELiteral(LBool true, stream.Peek().Span)
                | [SExpr e] -> e
                | SLet(name, args, value, isRec, span) :: rest ->
                    let valExpr = Seq.foldBack (fun arg acc -> ELambda(arg, acc, span)) args value
                    let body = desugar rest
                    ELet(name, valExpr, body, isRec, mkSpanFrom span (Ast.spanOfExpr body))
                | SExpr e :: rest ->
                    let body = desugar rest
                    ELet("_", e, body, false, mkSpanFrom (Ast.spanOfExpr e) (Ast.spanOfExpr body))
                | SType def :: _ ->
                    raise (ParseException { Message = "Type declarations are only supported at top level"; Span = def.Span })
            desugar (statements |> Seq.toList)

        and parseStmt () : Stmt =
            stream.SkipNewlines()
            match stream.Peek().Kind with
            | Type ->
                parseTypeDecl()
            | Let ->
                let letTok = stream.Next()
                let isRec = stream.Match(Rec)
                let nameTok = stream.ExpectIdent("Expected identifier after 'let'")
                let name = match nameTok.Kind with Ident n -> n | _ -> ""
                let args = ResizeArray<string>()
                let mutable argsDone = false
                while not argsDone do
                    stream.SkipNewlines()
                    match stream.Peek().Kind with
                    | Ident n ->
                        args.Add(n)
                        stream.Next() |> ignore
                    | _ -> argsDone <- true
                if isRec && args.Count = 0 then
                    raise (ParseException { Message = "'let rec' requires at least one function argument"; Span = nameTok.Span })
                stream.SkipNewlines()
                stream.Expect(Equals, "Expected '=' in let binding") |> ignore
                let value = parseExprOrBlock()
                SLet(name, args |> Seq.toList, value, isRec, mkSpanFrom letTok.Span (Ast.spanOfExpr value))
            | _ ->
                let expr = parseExpr()
                SExpr expr

        let program = ResizeArray<Stmt>()
        stream.SkipNewlines()
        while not stream.AtEnd do
            match stream.Peek().Kind with
            | EOF -> stream.Next() |> ignore
            | Dedent -> raise (ParseException { Message = "Unexpected dedent at top level"; Span = stream.Peek().Span })
            | _ ->
                let stmt = parseStmt()
                program.Add(stmt)
                stream.SkipNewlines()
        program |> Seq.toList
