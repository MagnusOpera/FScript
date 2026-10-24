namespace FScript.Compiler

open FScript.Language

module HirValidation =
    let rec private compatible left right =
        match left, right with
        | TRecordShape _, TNamed _ | TNamed _, TRecordShape _ -> true
        | TRecordShape(_, left, _), TRecord right | TRecord right, TRecordShape(_, left, _) ->
            left |> Map.forall (fun name item -> right |> Map.tryFind name |> Option.exists (compatible item))
        | TVar _, _ | _, TVar _ -> true
        | TFun(la, lr), TFun(ra, rr) -> compatible la ra && compatible lr rr
        | TTuple left, TTuple right -> left.Length = right.Length && List.forall2 compatible left right
        | TList left, TList right | TOption left, TOption right | TTask left, TTask right -> compatible left right
        | TMap(lk, lv), TMap(rk, rv) -> compatible lk rk && compatible lv rv
        | TRecord left, TRecord right -> left.Count = right.Count && (left |> Map.forall (fun name item -> right |> Map.tryFind name |> Option.exists (compatible item)))
        | TUnion(ln, _), TUnion(rn, _) -> ln = rn
        | TNamed name, TUnion(actual, _) | TUnion(actual, _), TNamed name -> name = actual
        | TNamed _, TRecord _ | TRecord _, TNamed _ -> true
        | _ -> left = right

    let private validSpan (span: Span) =
        span.Start.Line >= 0 && span.Start.Column >= 0 && span.End.Line >= 0 && span.End.Column >= 0
        && (span.Start.File <> span.End.File || span.End.Line > span.Start.Line || (span.End.Line = span.Start.Line && span.End.Column >= span.Start.Column))

    let validate (program: Hir.Program) =
        let declarationIds = program.Declarations |> List.map (fun value -> value.Id)
        let typeIds = program.Types |> List.map _.Id
        let allIds = typeIds @ declarationIds
        let duplicateIds = allIds |> List.countBy id |> List.filter (fun (_, count) -> count > 1)
        let invalidTypes =
            (program.Types |> List.map _.Name |> List.distinct |> List.length) <> program.Types.Length
            || program.Types |> List.exists (fun declaration ->
                not (validSpan declaration.Span)
                || match declaration.Definition with
                   | TRecord _ | THostManaged _ -> false
                   | TUnion(name, _) -> name <> declaration.Name
                   | _ -> true
                || declaration.Import |> Option.exists (fun imported -> imported.Name <> declaration.Name))
        let invalidAttributes =
            [ yield! program.Types |> List.collect _.Attributes
              yield! program.Declarations |> List.collect _.Attributes ]
            |> List.exists (fun attribute -> System.String.IsNullOrWhiteSpace attribute.Name || not (validSpan attribute.Span))
            || program.Declarations |> List.exists (fun declaration ->
                declaration.IsExported <> (declaration.Attributes |> List.exists (fun attribute -> attribute.Name = "Export")))
        let invalidImports =
            program.ImportedRecords |> List.exists (fun record -> System.String.IsNullOrWhiteSpace(record.ModulePath) || System.String.IsNullOrWhiteSpace(record.MemberName))
            || (program.ImportedRecords |> List.map _.Name |> List.distinct |> List.length) <> program.ImportedRecords.Length
        let knownTypes = program.Types |> List.map _.Name |> Set.ofList
        let rec validType (quantified: Set<int>) = function
            | TVar variable -> variable >= 0 && (quantified.IsEmpty || Set.contains variable quantified)
            | TFun(argument, result) -> validType quantified argument && validType quantified result
            | TTuple items -> items |> List.forall (validType quantified)
            | TList item | TOption item | TTask item -> validType quantified item
            | TMap(key, value) -> validType quantified key && validType quantified value
            | TRecord fields -> fields |> Map.forall (fun name fieldType -> not (System.String.IsNullOrWhiteSpace name) && validType quantified fieldType)
            | TUnion(name, cases) ->
                Set.contains name knownTypes
                && cases |> Map.forall (fun caseName payload -> not (System.String.IsNullOrWhiteSpace caseName) && payload |> Option.forall (validType quantified))
            | TNamed name -> Set.contains name knownTypes
            | _ -> true
        let validClosedType = validType Set.empty
        let validScheme (Forall(variables, valueType)) =
            variables = [ 0 .. variables.Length - 1 ] && validType (Set.ofList variables) valueType
        let rec validPattern (pattern: Hir.Pattern) =
            validSpan pattern.Span
            && validClosedType pattern.Type
            && match pattern.Node with
               | Hir.ConsPattern(head, tail) -> validPattern head && validPattern tail
               | Hir.TuplePattern items -> items |> List.forall validPattern
               | Hir.RecordPattern fields -> fields |> List.forall (fun (name, value) -> not (System.String.IsNullOrWhiteSpace name) && validPattern value)
               | Hir.MapPattern(clauses, tail) -> clauses |> List.forall (fun (key, value) -> validPattern key && validPattern value) && tail |> Option.forall validPattern
               | Hir.SomePattern item -> validPattern item
               | Hir.UnionPattern(typeName, caseName, payload) -> Set.contains typeName knownTypes && not (System.String.IsNullOrWhiteSpace caseName) && payload |> Option.forall validPattern
               | Hir.TypePattern patternType -> validClosedType patternType
               | _ -> true
        let rec validExpression (expression: Hir.Expression) =
            let valid = validExpression
            validSpan expression.Span
            && validClosedType expression.Type
            && match expression.Node with
               | Hir.Lambda(_, name, parameterType, body) -> not (System.String.IsNullOrWhiteSpace name) && validClosedType parameterType && valid body
               | Hir.Apply(fn, argument) | Hir.Pipe(argument, fn) | Hir.Cons(fn, argument) | Hir.Append(fn, argument)
               | Hir.Range(fn, argument) | Hir.IndexGet(fn, argument) -> valid fn && valid argument
               | Hir.If(condition, whenTrue, whenFalse) -> valid condition && valid whenTrue && valid whenFalse
               | Hir.Primitive(_, arguments) | Hir.Tuple arguments | Hir.List arguments -> arguments |> List.forall valid
               | Hir.Record fields -> fields |> List.forall (fun (name, value) -> not (System.String.IsNullOrWhiteSpace name) && valid value)
               | Hir.FieldGet(target, name) -> valid target && not (System.String.IsNullOrWhiteSpace name)
               | Hir.SomeValue target | Hir.Fail target -> valid target
               | Hir.Let(_, name, _, binding, body) -> not (System.String.IsNullOrWhiteSpace name) && valid binding && valid body
               | Hir.LetRecursiveGroup(bindings, body) ->
                   bindings |> List.forall (fun (_, name, bindingType, value) -> not (System.String.IsNullOrWhiteSpace name) && validClosedType bindingType && valid value)
                   && valid body
               | Hir.Match(target, cases) ->
                   valid target
                   && cases |> List.forall (fun case -> validSpan case.Span && validPattern case.Pattern && case.Guard |> Option.forall valid && valid case.Body)
               | Hir.RecordUpdate(target, fields) -> valid target && fields |> List.forall (fun (name, value) -> not (System.String.IsNullOrWhiteSpace name) && valid value)
               | Hir.RecordConvert(target, targetType) -> valid target && validClosedType targetType
               | Hir.Interpolate parts -> parts |> List.forall (function Hir.TextPart _ -> true | Hir.ValuePart value -> valid value)
               | Hir.UnionCase(typeName, caseName, payload) -> Set.contains typeName knownTypes && not (System.String.IsNullOrWhiteSpace caseName) && payload |> Option.forall valid
               | Hir.Map entries -> entries |> List.forall (function Hir.KeyValue(key, value) -> valid key && valid value | Hir.Spread value -> valid value)
               | Hir.ForEach(_, name, itemType, source, body) -> not (System.String.IsNullOrWhiteSpace name) && validClosedType itemType && valid source && valid body
               | Hir.TypeToken tokenType -> validClosedType tokenType
               | _ -> true
        let invalidTypeReferences =
            program.Types |> List.exists (fun declaration -> not (validClosedType declaration.Definition))
            || program.ImportedRecords |> List.exists (fun record -> record.Fields |> List.exists (snd >> validClosedType >> not))
            || program.Declarations |> List.exists (fun declaration -> not (validScheme declaration.Scheme) || not (validExpression declaration.Value))
            || program.Entry |> Option.exists (validExpression >> not)
        if invalidAttributes then
            Error [ Diagnostic.hir "FSH1108" "HIR structured attribute metadata is invalid or inconsistent." None ]
        elif invalidTypes then
            Error [ Diagnostic.hir "FSH1107" "HIR type declarations must be unique, well-formed, and consistently linked." None ]
        elif invalidImports then
            Error [ Diagnostic.hir "FSH1105" "HIR imported record linkage must be complete and uniquely named." None ]
        elif invalidTypeReferences then
            Error [ Diagnostic.hir "FSH1109" "HIR contains an unresolved or invalid semantic type, nested span, or field name." None ]
        elif not duplicateIds.IsEmpty then
            Error [ Diagnostic.hir "FSH1101" "HIR declaration IDs must be unique." None ]
        elif program.Declarations |> List.exists (fun declaration -> declaration.IsExported <> declaration.Export.IsSome || (declaration.Export |> Option.exists (fun export -> export.Name <> declaration.Name || not (validSpan export.Span)))) then
            Error [ Diagnostic.hir "FSH1106" "HIR export metadata is inconsistent." None ]
        elif program.Declarations |> List.exists (fun declaration -> not (validSpan declaration.Span && validSpan declaration.Value.Span)) then
            Error [ Diagnostic.hir "FSH1103" "HIR contains an invalid source span." None ]
        elif program.Declarations |> List.exists (fun declaration -> let (Forall(_, declaredType)) = declaration.Scheme in not (compatible declaredType declaration.Value.Type)) then
            Error [ Diagnostic.hir "FSH1104" "HIR declaration scheme does not match its value type." None ]
        else
            let rec references (expression: Hir.Expression) =
                match expression.Node with
                | Hir.Constant _ -> Set.empty
                | Hir.Reference id -> Set.singleton id
                | Hir.Lambda(parameter, _, _, body) -> references body |> Set.remove parameter
                | Hir.Apply(fn, argument) | Hir.Pipe(argument, fn) -> Set.union (references fn) (references argument)
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
                        | Hir.MapPattern(clauses, tail) ->
                            let clauseBindings = clauses |> List.collect (fun (key, value) -> [ bindings key; bindings value ])
                            tail |> Option.map bindings |> Option.toList |> (@) clauseBindings |> Set.unionMany
                        | Hir.SomePattern item -> bindings item
                        | Hir.UnionPattern(_, _, payload) -> payload |> Option.map bindings |> Option.defaultValue Set.empty
                        | Hir.TypePattern _ -> Set.empty
                        | _ -> Set.empty
                    let caseReferences (case: Hir.MatchCase) =
                        let bound = bindings case.Pattern
                        let values = Set.union (case.Guard |> Option.map references |> Option.defaultValue Set.empty) (references case.Body)
                        Set.difference values bound
                    Set.union (references target) (cases |> List.map caseReferences |> Set.unionMany)
                | Hir.Range(startValue, endValue) | Hir.IndexGet(startValue, endValue) -> Set.union (references startValue) (references endValue)
                | Hir.RecordUpdate(target, fields) -> Set.union (references target) (fields |> List.map (snd >> references) |> Set.unionMany)
                | Hir.RecordConvert(target, _) -> references target
                | Hir.Interpolate parts -> parts |> List.choose (function Hir.TextPart _ -> None | Hir.ValuePart item -> Some(references item)) |> Set.unionMany
                | Hir.UnionCase(_, _, payload) -> payload |> Option.map references |> Option.defaultValue Set.empty
                | Hir.Map entries ->
                    entries |> List.map (function Hir.KeyValue(key, value) -> Set.union (references key) (references value) | Hir.Spread value -> references value) |> Set.unionMany
                | Hir.Intrinsic _ -> Set.empty
                | Hir.HostExternal _ -> Set.empty
                | Hir.RuntimeContext -> Set.empty
                | Hir.TargetImport _ -> Set.empty
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
            if unresolved.IsEmpty then Ok program
            else
                let identifiers =
                    unresolved
                    |> Seq.map (Hir.symbolNumber >> string)
                    |> String.concat ", "
                Error [ Diagnostic.hir "FSH1102" $"HIR contains unresolved symbol references: {identifiers}." None ]
