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
                | Hir.Let(id, _, recursive, binding, body) ->
                    let bindingReferences =
                        let value = references binding
                        if recursive then Set.remove id value else value
                    Set.union bindingReferences (references body |> Set.remove id)
                | Hir.LetRecursiveGroup(bindings, body) ->
                    let ids = bindings |> List.map (fun (id, _, _, _) -> id) |> Set.ofList
                    let allReferences =
                        references body :: (bindings |> List.map (fun (_, _, _, value) -> references value))
                        |> Set.unionMany
                    Set.difference allReferences ids
                | Hir.Fail value -> references value
                | Hir.Match(target, cases) ->
                    let rec bindings (pattern: Hir.Pattern) =
                        match pattern.Node with
                        | Hir.Binding(id, _) -> Set.singleton id
                        | Hir.ConsPattern(head, tail) -> Set.union (bindings head) (bindings tail)
                        | Hir.TuplePattern items -> items |> List.map bindings |> Set.unionMany
                        | Hir.RecordPattern fields -> fields |> List.map (snd >> bindings) |> Set.unionMany
                        | Hir.SomePattern item -> bindings item
                        | Hir.UnionPattern(_, _, payload) -> payload |> Option.map bindings |> Option.defaultValue Set.empty
                        | _ -> Set.empty
                    let caseReferences (case: Hir.MatchCase) =
                        let bound = bindings case.Pattern
                        let values = Set.union (case.Guard |> Option.map references |> Option.defaultValue Set.empty) (references case.Body)
                        Set.difference values bound
                    Set.union (references target) (cases |> List.map caseReferences |> Set.unionMany)
                | Hir.Range(startValue, endValue) | Hir.IndexGet(startValue, endValue) -> Set.union (references startValue) (references endValue)
                | Hir.RecordUpdate(target, fields) -> Set.union (references target) (fields |> List.map (snd >> references) |> Set.unionMany)
                | Hir.Interpolate parts -> parts |> List.choose (function Hir.TextPart _ -> None | Hir.ValuePart item -> Some(references item)) |> Set.unionMany
                | Hir.UnionCase(_, _, payload) -> payload |> Option.map references |> Option.defaultValue Set.empty
                | Hir.Map entries ->
                    entries |> List.map (function Hir.KeyValue(key, value) -> Set.union (references key) (references value) | Hir.Spread value -> references value) |> Set.unionMany
                | Hir.Intrinsic _ -> Set.empty
                | Hir.HostExternal _ -> Set.empty
                | Hir.ForEach(id, _, _, source, body) -> Set.union (references source) (references body |> Set.remove id)
                | Hir.TypeToken _ -> Set.empty
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
