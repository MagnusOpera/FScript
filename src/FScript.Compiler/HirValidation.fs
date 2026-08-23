namespace FScript.Compiler

module HirValidation =
    let validate (program: Hir.Program) =
        let declarationIds = program.Declarations |> List.map (fun value -> value.Id)
        let duplicateIds = declarationIds |> List.countBy id |> List.filter (fun (_, count) -> count > 1)
        if not duplicateIds.IsEmpty then
            Error [ Diagnostic.hir "FSH1101" "HIR declaration IDs must be unique." None ]
        else
            let rec references (expression: Hir.Expression) =
                match expression.Node with
                | Hir.Constant _ -> Set.empty
                | Hir.Reference id -> Set.singleton id
                | Hir.Lambda(parameter, _, _, body) -> references body |> Set.remove parameter
                | Hir.Apply(fn, argument) -> Set.union (references fn) (references argument)
                | Hir.If(condition, whenTrue, whenFalse) ->
                    Set.unionMany [ references condition; references whenTrue; references whenFalse ]
                | Hir.Primitive(_, arguments) -> arguments |> List.map references |> Set.unionMany
                | Hir.Tuple items | Hir.List items -> items |> List.map references |> Set.unionMany
                | Hir.Record fields -> fields |> List.map (snd >> references) |> Set.unionMany
                | Hir.FieldGet(target, _) | Hir.SomeValue target -> references target
                | Hir.NoneValue -> Set.empty
                | Hir.Cons(head, tail) | Hir.Append(head, tail) -> Set.union (references head) (references tail)
            let declared = declarationIds |> Set.ofList
            let unresolved =
                [ yield! program.Declarations |> List.collect (fun value -> references value.Value |> Set.toList)
                  match program.Entry with
                  | Some value -> yield! references value |> Set.toList
                  | None -> () ]
                |> Set.ofList
                |> Set.filter (fun id -> not (Set.contains id declared))
            // IDs below the first declaration are reserved deterministic builtin symbols.
            let firstDeclaration =
                match declarationIds |> List.map Hir.symbolNumber with
                | [] -> 0
                | values -> List.min values
            let invalid = unresolved |> Set.filter (fun id -> Hir.symbolNumber id >= firstDeclaration)
            if invalid.IsEmpty then Ok program
            else Error [ Diagnostic.hir "FSH1102" "HIR contains unresolved symbol references." None ]
