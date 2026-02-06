namespace FScript.Core

type TypeDef =
    { Name: string
      Fields: (string * TypeRef) list
      Span: Span }

and TypeRef =
    | TRName of string
    | TRTuple of TypeRef list
    | TRPostfix of TypeRef * string

type Literal =
    | LInt of int64
    | LFloat of float
    | LBool of bool
    | LString of string

and Pattern =
    | PWildcard of Span
    | PVar of string * Span
    | PLiteral of Literal * Span
    | PNil of Span
    | PCons of Pattern * Pattern * Span
    | PSome of Pattern * Span
    | PNone of Span

and Expr =
    | ELiteral of Literal * Span
    | EVar of string * Span
    | ELambda of string * Expr * Span
    | EApply of Expr * Expr * Span
    | EIf of Expr * Expr * Expr * Span
    | ERaise of Expr * Span
    | EFor of string * Expr * Expr * Span
    | EMatch of Expr * (Pattern * Expr * Span) list * Span
    | ELet of string * Expr * Expr * bool * Span
    | EList of Expr list * Span
    | ERange of Expr * Expr * Span
    | ETuple of Expr list * Span
    | ERecord of (string * Expr) list * Span
    | ERecordUpdate of Expr * (string * Expr) list * Span
    | EFieldGet of Expr * string * Span
    | ECons of Expr * Expr * Span
    | EAppend of Expr * Expr * Span
    | EBinOp of string * Expr * Expr * Span
    | ESome of Expr * Span
    | ENone of Span
    | ETypeOf of string * Span
    | EInterpolatedString of InterpolatedPart list * Span

and InterpolatedPart =
    | IPText of string
    | IPExpr of Expr

and Stmt =
    | SType of TypeDef
    | SLet of string * string list * Expr * bool * Span
    | SExpr of Expr

type Program = Stmt list

module Ast =
    let spanOfPattern pat =
        match pat with
        | PWildcard s -> s
        | PVar (_, s) -> s
        | PLiteral (_, s) -> s
        | PNil s -> s
        | PCons (_, _, s) -> s
        | PSome (_, s) -> s
        | PNone s -> s

    let spanOfExpr expr =
        match expr with
        | ELiteral (_, s) -> s
        | EVar (_, s) -> s
        | ELambda (_, _, s) -> s
        | EApply (_, _, s) -> s
        | EIf (_, _, _, s) -> s
        | ERaise (_, s) -> s
        | EFor (_, _, _, s) -> s
        | EMatch (_, _, s) -> s
        | ELet (_, _, _, _, s) -> s
        | EList (_, s) -> s
        | ERange (_, _, s) -> s
        | ETuple (_, s) -> s
        | ERecord (_, s) -> s
        | ERecordUpdate (_, _, s) -> s
        | EFieldGet (_, _, s) -> s
        | ECons (_, _, s) -> s
        | EAppend (_, _, s) -> s
        | EBinOp (_, _, _, s) -> s
        | ESome (_, s) -> s
        | ENone s -> s
        | ETypeOf (_, s) -> s
        | EInterpolatedString (_, s) -> s
