namespace FScript.Language

type TokenKind =
    | EOF
    | Newline
    | Indent
    | Dedent
    | Ident of string
    | IntLit of int64
    | FloatLit of float
    | StringLit of string
    | InterpString of string
    | BoolLit of bool
    | Let
    | Rec
    | And
    | Fun
    | Match
    | With
    | If
    | Then
    | Else
    | Elif
    | Raise
    | For
    | In
    | Arrow
    | Equals
    | Bar
    | LParen
    | RParen
    | LBracket
    | RBracket
    | LBracketBar
    | BarRBracket
    | Semicolon
    | Comma
    | Colon
    | Dot
    | RangeDots
    | Hash
    | Include
    | LBrace
    | RBrace
    | Type
    | Typeof
    | Nameof
    | Of
    | Plus
    | Minus
    | Star
    | Slash
    | Percent
    | Less
    | Greater
    | LessEqual
    | GreaterEqual
    | AndAnd
    | OrOr
    | PipeForward
    | Cons
    | Append

[<StructuralEquality; StructuralComparison>]
type Token = { Kind: TokenKind; Span: Span }
