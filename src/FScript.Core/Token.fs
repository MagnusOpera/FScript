namespace FScript.Core

type TokenKind =
    | EOF
    | Newline
    | Indent
    | Dedent
    | Ident of string
    | IntLit of int64
    | FloatLit of float
    | StringLit of string
    | BoolLit of bool
    | Let
    | Rec
    | Fun
    | Match
    | With
    | If
    | Then
    | Else
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
    | LBrace
    | RBrace
    | Type
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
