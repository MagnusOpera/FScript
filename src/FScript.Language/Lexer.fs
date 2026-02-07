namespace FScript.Language

open System

module Lexer =
    let private isIdentStart c = Char.IsLetter c || c = '_'
    let private isIdentPart c = Char.IsLetterOrDigit c || c = '_' || c = '\''

    let private mkSpan line col length =
        let startPos = { Line = line; Column = col }
        let endPos = { Line = line; Column = col + length }
        { Start = startPos; End = endPos }

    let private keywordToken ident =
        match ident with
        | "let" -> Some Let
        | "rec" -> Some Rec
        | "and" -> Some And
        | "type" -> Some TokenKind.Type
        | "typeof" -> Some TokenKind.Typeof
        | "of" -> Some TokenKind.Of
        | "fun" -> Some Fun
        | "match" -> Some Match
        | "with" -> Some With
        | "if" -> Some If
        | "then" -> Some Then
        | "else" -> Some Else
        | "elif" -> Some Elif
        | "raise" -> Some Raise
        | "for" -> Some For
        // 'in' is intentionally not a keyword in FScript layout-style let expressions.
        | "true" -> Some (BoolLit true)
        | "false" -> Some (BoolLit false)
        | _ -> None

    let private addToken kind span (tokens: ResizeArray<Token>) =
        tokens.Add({ Kind = kind; Span = span })

    let private readString (src: string) (i: int) (line: int) (col: int) =
        let sb = System.Text.StringBuilder()
        let mutable idx = i + 1
        let mutable cLine = line
        let mutable cCol = col + 1
        let mutable doneFlag = false
        while idx < src.Length && not doneFlag do
            let ch = src.[idx]
            if ch = '"' then
                doneFlag <- true
                idx <- idx + 1
                cCol <- cCol + 1
            else
                if ch = '\\' && idx + 1 < src.Length then
                    let next = src.[idx + 1]
                    match next with
                    | 'n' -> sb.Append('\n') |> ignore
                    | 't' -> sb.Append('\t') |> ignore
                    | '"' -> sb.Append('"') |> ignore
                    | '\\' -> sb.Append('\\') |> ignore
                    | _ -> sb.Append(next) |> ignore
                    idx <- idx + 2
                    cCol <- cCol + 2
                else
                    sb.Append(ch) |> ignore
                    if ch = '\n' then
                        cLine <- cLine + 1
                        cCol <- 1
                    else
                        cCol <- cCol + 1
                    idx <- idx + 1
        if not doneFlag then
            raise (ParseException { Message = "Unterminated string literal"; Span = mkSpan line col 1 })
        sb.ToString(), idx, cLine, cCol

    let private readInterpolatedRaw (src: string) (i: int) (line: int) (col: int) =
        let mutable idx = i + 2 // skip $"
        let mutable cLine = line
        let mutable cCol = col + 2
        let mutable doneFlag = false
        let mutable escaped = false

        while idx < src.Length && not doneFlag do
            let ch = src.[idx]
            if escaped then
                escaped <- false
                if ch = '\n' then
                    cLine <- cLine + 1
                    cCol <- 1
                else
                    cCol <- cCol + 1
                idx <- idx + 1
            else
                match ch with
                | '\\' ->
                    escaped <- true
                    idx <- idx + 1
                    cCol <- cCol + 1
                | '"' ->
                    doneFlag <- true
                    idx <- idx + 1
                    cCol <- cCol + 1
                | '\n' ->
                    idx <- idx + 1
                    cLine <- cLine + 1
                    cCol <- 1
                | _ ->
                    idx <- idx + 1
                    cCol <- cCol + 1

        if not doneFlag then
            raise (ParseException { Message = "Unterminated interpolated string literal"; Span = mkSpan line col 2 })

        let start = i + 2
        let contentLen = (idx - 1) - start
        let raw = if contentLen <= 0 then "" else src.Substring(start, contentLen)
        raw, idx, cLine, cCol

    let tokenize (src: string) : Token list =
        let tokens = ResizeArray<Token>()
        let indentStack = ResizeArray<int>()
        indentStack.Add(0)

        let mutable i = 0
        let mutable line = 1
        let mutable col = 1
        let mutable atLineStart = true

        let emitIndentChanges newIndent span =
            let current = indentStack.[indentStack.Count - 1]
            if newIndent > current then
                indentStack.Add(newIndent)
                addToken Indent span tokens
            elif newIndent < current then
                let mutable j = indentStack.Count - 1
                while j > 0 && indentStack.[j] > newIndent do
                    indentStack.RemoveAt(j)
                    addToken Dedent span tokens
                    j <- j - 1
                if indentStack.[indentStack.Count - 1] <> newIndent then
                    raise (ParseException { Message = "Inconsistent indentation"; Span = span })
            else
                ()

        while i < src.Length do
            let ch = src.[i]
            if atLineStart then
                let mutable idx = i
                let mutable indent = 0
                let mutable cCol = col
                let mutable onlyWhitespace = true
                while idx < src.Length && (src.[idx] = ' ' || src.[idx] = '\t') do
                    indent <- indent + (if src.[idx] = '\t' then 4 else 1)
                    idx <- idx + 1
                    cCol <- cCol + 1
                if idx < src.Length && src.[idx] = '\n' then
                    addToken Newline (mkSpan line col 1) tokens
                    i <- idx + 1
                    line <- line + 1
                    col <- 1
                    atLineStart <- true
                elif idx < src.Length && src.[idx] = '/' && idx + 1 < src.Length && src.[idx + 1] = '/' then
                    while i < src.Length && src.[i] <> '\n' do i <- i + 1
                    addToken Newline (mkSpan line col 1) tokens
                    if i < src.Length && src.[i] = '\n' then
                        i <- i + 1
                        line <- line + 1
                        col <- 1
                    atLineStart <- true
                else
                    let span = mkSpan line col 1
                    emitIndentChanges indent span
                    i <- idx
                    col <- cCol
                    atLineStart <- false
                onlyWhitespace |> ignore
            else
                match ch with
                | ' ' | '\t' ->
                    i <- i + 1
                    col <- col + 1
                | '\r' ->
                    i <- i + 1
                | '\n' ->
                    addToken Newline (mkSpan line col 1) tokens
                    i <- i + 1
                    line <- line + 1
                    col <- 1
                    atLineStart <- true
                | '/' when i + 1 < src.Length && src.[i + 1] = '/' ->
                    while i < src.Length && src.[i] <> '\n' do i <- i + 1
                | '(' when i + 1 < src.Length && src.[i + 1] = '*' ->
                    raise (ParseException { Message = "Block comments are not supported"; Span = mkSpan line col 2 })
                | '"' ->
                    let str, idx, nline, ncol = readString src i line col
                    let span = mkSpan line col (idx - i)
                    addToken (StringLit str) span tokens
                    i <- idx
                    line <- nline
                    col <- ncol
                | '$' when i + 1 < src.Length && src.[i + 1] = '"' ->
                    let raw, idx, nline, ncol = readInterpolatedRaw src i line col
                    let span = mkSpan line col (idx - i)
                    addToken (InterpString raw) span tokens
                    i <- idx
                    line <- nline
                    col <- ncol
                | c when Char.IsDigit c ->
                    let start = i
                    let startCol = col
                    let mutable hasDot = false
                    let mutable idx = i
                    while
                        idx < src.Length
                        && (Char.IsDigit src.[idx]
                            || (src.[idx] = '.'
                                && not hasDot
                                && idx + 1 < src.Length
                                && Char.IsDigit src.[idx + 1])) do
                        if src.[idx] = '.' then
                            hasDot <- true
                            idx <- idx + 1
                        else
                            idx <- idx + 1
                    let text = src.Substring(start, idx - start)
                    let span = mkSpan line startCol (idx - start)
                    if hasDot then
                        addToken (FloatLit (Double.Parse(text, Globalization.CultureInfo.InvariantCulture))) span tokens
                    else
                        addToken (IntLit (Int64.Parse(text))) span tokens
                    i <- idx
                    col <- startCol + (idx - start)
                | c when isIdentStart c ->
                    let start = i
                    let startCol = col
                    let mutable idx = i + 1
                    while idx < src.Length && isIdentPart src.[idx] do idx <- idx + 1
                    let ident = src.Substring(start, idx - start)
                    let span = mkSpan line startCol (idx - start)
                    match keywordToken ident with
                    | Some k -> addToken k span tokens
                    | None -> addToken (Ident ident) span tokens
                    i <- idx
                    col <- startCol + (idx - start)
                | '-' when i + 1 < src.Length && src.[i + 1] = '>' ->
                    addToken Arrow (mkSpan line col 2) tokens
                    i <- i + 2
                    col <- col + 2
                | ':' when i + 1 < src.Length && src.[i + 1] = ':' ->
                    addToken Cons (mkSpan line col 2) tokens
                    i <- i + 2
                    col <- col + 2
                | '<' when i + 1 < src.Length && src.[i + 1] = '=' ->
                    addToken LessEqual (mkSpan line col 2) tokens
                    i <- i + 2
                    col <- col + 2
                | '>' when i + 1 < src.Length && src.[i + 1] = '=' ->
                    addToken GreaterEqual (mkSpan line col 2) tokens
                    i <- i + 2
                    col <- col + 2
                | '&' when i + 1 < src.Length && src.[i + 1] = '&' ->
                    addToken AndAnd (mkSpan line col 2) tokens
                    i <- i + 2
                    col <- col + 2
                | '|' when i + 1 < src.Length && src.[i + 1] = ']' ->
                    addToken BarRBracket (mkSpan line col 2) tokens
                    i <- i + 2
                    col <- col + 2
                | '|' when i + 1 < src.Length && src.[i + 1] = '|' ->
                    addToken OrOr (mkSpan line col 2) tokens
                    i <- i + 2
                    col <- col + 2
                | '|' when i + 1 < src.Length && src.[i + 1] = '>' ->
                    addToken PipeForward (mkSpan line col 2) tokens
                    i <- i + 2
                    col <- col + 2
                | '+' -> addToken Plus (mkSpan line col 1) tokens; i <- i + 1; col <- col + 1
                | '-' -> addToken Minus (mkSpan line col 1) tokens; i <- i + 1; col <- col + 1
                | '*' -> addToken Star (mkSpan line col 1) tokens; i <- i + 1; col <- col + 1
                | '/' -> addToken Slash (mkSpan line col 1) tokens; i <- i + 1; col <- col + 1
                | '%' -> addToken Percent (mkSpan line col 1) tokens; i <- i + 1; col <- col + 1
                | '=' -> addToken Equals (mkSpan line col 1) tokens; i <- i + 1; col <- col + 1
                | '<' -> addToken Less (mkSpan line col 1) tokens; i <- i + 1; col <- col + 1
                | '>' -> addToken Greater (mkSpan line col 1) tokens; i <- i + 1; col <- col + 1
                | '|' -> addToken Bar (mkSpan line col 1) tokens; i <- i + 1; col <- col + 1
                | '@' -> addToken Append (mkSpan line col 1) tokens; i <- i + 1; col <- col + 1
                | '(' -> addToken LParen (mkSpan line col 1) tokens; i <- i + 1; col <- col + 1
                | ')' -> addToken RParen (mkSpan line col 1) tokens; i <- i + 1; col <- col + 1
                | '[' when i + 1 < src.Length && src.[i + 1] = '|' ->
                    addToken LBracketBar (mkSpan line col 2) tokens
                    i <- i + 2
                    col <- col + 2
                | '[' -> addToken LBracket (mkSpan line col 1) tokens; i <- i + 1; col <- col + 1
                | ']' -> addToken RBracket (mkSpan line col 1) tokens; i <- i + 1; col <- col + 1
                | ';' -> addToken Semicolon (mkSpan line col 1) tokens; i <- i + 1; col <- col + 1
                | ',' -> addToken Comma (mkSpan line col 1) tokens; i <- i + 1; col <- col + 1
                | ':' -> addToken Colon (mkSpan line col 1) tokens; i <- i + 1; col <- col + 1
                | '.' when i + 1 < src.Length && src.[i + 1] = '.' ->
                    addToken RangeDots (mkSpan line col 2) tokens
                    i <- i + 2
                    col <- col + 2
                | '.' -> addToken Dot (mkSpan line col 1) tokens; i <- i + 1; col <- col + 1
                | '{' -> addToken LBrace (mkSpan line col 1) tokens; i <- i + 1; col <- col + 1
                | '}' -> addToken RBrace (mkSpan line col 1) tokens; i <- i + 1; col <- col + 1
                | _ ->
                    raise (ParseException { Message = sprintf "Unexpected character '%c'" ch; Span = mkSpan line col 1 })

        addToken Newline (mkSpan line col 1) tokens
        while indentStack.Count > 1 do
            indentStack.RemoveAt(indentStack.Count - 1)
            addToken Dedent (mkSpan line col 1) tokens
        addToken EOF (mkSpan line col 1) tokens
        tokens |> Seq.toList
