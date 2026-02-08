namespace FScript.Language

type TypeDef =
    { Name: string
      IsRecursive: bool
      Fields: (string * TypeRef) list
      Cases: (string * TypeRef option) list
      Span: Span }

and TypeRef =
    | TRName of string
    | TRTuple of TypeRef list
    | TRFun of TypeRef * TypeRef
    | TRPostfix of TypeRef * string
    | TRRecord of (string * TypeRef) list

type Param =
    { Name: string
      Annotation: TypeRef option
      Span: Span }

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
    | PTuple of Pattern list * Span
    | PRecord of (string * Pattern) list * Span
    | PSome of Pattern * Span
    | PNone of Span
    | PUnionCase of string * Pattern option * Span

and Expr =
    | EUnit of Span
    | ELiteral of Literal * Span
    | EVar of string * Span
    | ELambda of Param * Expr * Span
    | EApply of Expr * Expr * Span
    | EIf of Expr * Expr * Expr * Span
    | ERaise of Expr * Span
    | EFor of string * Expr * Expr * Span
    | EMatch of Expr * (Pattern * Expr * Span) list * Span
    | ELet of string * Expr * Expr * bool * Span
    | ELetRecGroup of (string * Param list * Expr * Span) list * Expr * Span
    | EList of Expr list * Span
    | ERange of Expr * Expr * Span
    | ETuple of Expr list * Span
    | ERecord of (string * Expr) list * Span
    | EMap of (Expr * Expr) list * Span
    | ERecordUpdate of Expr * (string * Expr) list * Span
    | EFieldGet of Expr * string * Span
    | ECons of Expr * Expr * Span
    | EAppend of Expr * Expr * Span
    | EBinOp of string * Expr * Expr * Span
    | ESome of Expr * Span
    | ENone of Span
    | ETypeOf of string * Span
    | ENameOf of string * Span
    | EInterpolatedString of InterpolatedPart list * Span

and InterpolatedPart =
    | IPText of string
    | IPExpr of Expr

and Stmt =
    | SType of TypeDef
    | SLet of string * Param list * Expr * bool * bool * Span
    | SLetRecGroup of (string * Param list * Expr * Span) list * bool * Span
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
        | PTuple (_, s) -> s
        | PRecord (_, s) -> s
        | PSome (_, s) -> s
        | PNone s -> s
        | PUnionCase (_, _, s) -> s

    let spanOfExpr expr =
        match expr with
        | EUnit s -> s
        | ELiteral (_, s) -> s
        | EVar (_, s) -> s
        | ELambda (_, _, s) -> s
        | EApply (_, _, s) -> s
        | EIf (_, _, _, s) -> s
        | ERaise (_, s) -> s
        | EFor (_, _, _, s) -> s
        | EMatch (_, _, s) -> s
        | ELet (_, _, _, _, s) -> s
        | ELetRecGroup (_, _, s) -> s
        | EList (_, s) -> s
        | ERange (_, _, s) -> s
        | ETuple (_, s) -> s
        | ERecord (_, s) -> s
        | EMap (_, s) -> s
        | ERecordUpdate (_, _, s) -> s
        | EFieldGet (_, _, s) -> s
        | ECons (_, _, s) -> s
        | EAppend (_, _, s) -> s
        | EBinOp (_, _, _, s) -> s
        | ESome (_, s) -> s
        | ENone s -> s
        | ETypeOf (_, s) -> s
        | ENameOf (_, s) -> s
        | EInterpolatedString (_, s) -> s
