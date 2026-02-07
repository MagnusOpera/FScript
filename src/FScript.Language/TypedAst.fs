namespace FScript.Language

type TypedExpr =
    | TELiteral of Literal * Type * Span
    | TEVar of string * Type * Span
    | TELambda of string * TypedExpr * Type * Span
    | TEApply of TypedExpr * TypedExpr * Type * Span
    | TEIf of TypedExpr * TypedExpr * TypedExpr * Type * Span
    | TEMatch of TypedExpr * (Pattern * TypedExpr * Span) list * Type * Span
    | TELet of string * TypedExpr * TypedExpr * bool * Type * Span
    | TEList of TypedExpr list * Type * Span
    | TETuple of TypedExpr list * Type * Span
    | TERecord of (string * TypedExpr) list * Type * Span
    | TEFieldGet of TypedExpr * string * Type * Span
    | TEConstructor of string * string * Type option * Type * Span
    | TECons of TypedExpr * TypedExpr * Type * Span
    | TEAppend of TypedExpr * TypedExpr * Type * Span
    | TEBinOp of string * TypedExpr * TypedExpr * Type * Span
    | TESome of TypedExpr * Type * Span
    | TENone of Type * Span

and TypedStmt =
    | TSLet of string * string list * TypedExpr * bool * Span
    | TSType of TypeDef
    | TSExpr of TypedExpr

type TypedProgram = TypedStmt list

module TypedAst =
    let spanOfExpr expr =
        match expr with
        | TELiteral (_, _, s) -> s
        | TEVar (_, _, s) -> s
        | TELambda (_, _, _, s) -> s
        | TEApply (_, _, _, s) -> s
        | TEIf (_, _, _, _, s) -> s
        | TEMatch (_, _, _, s) -> s
        | TELet (_, _, _, _, _, s) -> s
        | TEList (_, _, s) -> s
        | TETuple (_, _, s) -> s
        | TERecord (_, _, s) -> s
        | TEFieldGet (_, _, _, s) -> s
        | TEConstructor (_, _, _, _, s) -> s
        | TECons (_, _, _, s) -> s
        | TEAppend (_, _, _, s) -> s
        | TEBinOp (_, _, _, _, s) -> s
        | TESome (_, _, s) -> s
        | TENone (_, s) -> s

    let typeOf expr =
        match expr with
        | TELiteral (_, t, _) -> t
        | TEVar (_, t, _) -> t
        | TELambda (_, _, t, _) -> t
        | TEApply (_, _, t, _) -> t
        | TEIf (_, _, _, t, _) -> t
        | TEMatch (_, _, t, _) -> t
        | TELet (_, _, _, _, t, _) -> t
        | TEList (_, t, _) -> t
        | TETuple (_, t, _) -> t
        | TERecord (_, t, _) -> t
        | TEFieldGet (_, _, t, _) -> t
        | TEConstructor (_, _, _, t, _) -> t
        | TECons (_, _, t, _) -> t
        | TEAppend (_, _, t, _) -> t
        | TEBinOp (_, _, _, t, _) -> t
        | TESome (_, t, _) -> t
        | TENone (t, _) -> t
