namespace FScript.Compiler

open FScript.Language

module Lir =
    type SymbolId = SymbolId of int
    type FunctionId = FunctionId of int

    type Constant =
        | Unit
        | Int of int64
        | Float of float
        | Bool of bool
        | String of string

    type Operation =
        | IntAdd | FloatAdd | IntSubtract | FloatSubtract
        | IntMultiply | FloatMultiply | IntDivide | FloatDivide | IntRemainder
        | StructuralEqual
        | NumericLessThan | NumericGreaterThan | NumericLessThanOrEqual | NumericGreaterThanOrEqual
        | BooleanAnd | BooleanOr

    type Expression =
        { Type: Type
          Span: Span
          Node: ExpressionNode }
    and ExpressionNode =
        | Constant of Constant
        | Load of SymbolId
        | MakeClosure of FunctionId * SymbolId list
        | CallClosure of Expression * Expression
        | PipeClosure of Expression * Expression
        | Branch of Expression * Expression * Expression
        | Operation of Operation * Expression list
        | Tuple of Expression list
        | List of Expression list
        | Record of (string * Expression) list
        | FieldGet of Expression * string
        | SomeValue of Expression
        | NoneValue
        | Cons of Expression * Expression
        | Append of Expression * Expression
        | Let of SymbolId * string * bool * Expression * Expression
        | LetRecursiveGroup of (SymbolId * string * Type * Expression) list * Expression
        | Fail of Expression
        | DecisionTree of Expression * DecisionCase list
        | Range of Expression * Expression
        | RecordUpdate of Expression * (string * Expression) list
        | RecordConvert of Expression * Type
        | IndexGet of Expression * Expression
        | Interpolate of InterpolationPart list
        | UnionCase of string * string * Expression option
        | Map of MapEntry list
        | Intrinsic of string
        | HostExternal of string * int
        | RuntimeContext
        | TargetImport of string * string
        | ForEach of SymbolId * string * Type * Expression * Expression
        | TypeToken of Type

    and MapEntry =
        | KeyValue of Expression * Expression
        | Spread of Expression

    and InterpolationPart =
        | TextPart of string
        | ValuePart of Expression

    and ProjectionStep =
        | ListHead
        | ListTail
        | TupleItem of int
        | RecordField of string
        | OptionValue
        | UnionPayload

    and PatternCheck =
        | LiteralCheck of ProjectionStep list * Constant
        | EmptyListCheck of ProjectionStep list
        | NonEmptyListCheck of ProjectionStep list
        | TupleArityCheck of ProjectionStep list * int
        | RecordFieldCheck of ProjectionStep list * string
        | SomeCheck of ProjectionStep list
        | NoneCheck of ProjectionStep list
        | UnionCaseCheck of ProjectionStep list * string * string * bool
        | TypeCheck of ProjectionStep list * Type

    and PatternBinding =
        { Id: SymbolId
          Name: string
          Type: Type
          Projection: ProjectionStep list }

    and PatternPlan =
        { Span: Span
          Checks: PatternCheck list
          Bindings: PatternBinding list
          Maps: MapPlan list }

    and MapPlan =
        { Projection: ProjectionStep list
          Clauses: MapClausePlan list
          Tail: PatternPlan option
          RequireEmptyRemainder: bool }

    and MapClausePlan =
        { Key: PatternPlan
          Value: PatternPlan }

    and DecisionCase =
        { Plan: PatternPlan
          Guard: Expression option
          Body: Expression
          Span: Span }

    type Declaration =
        { Id: SymbolId
          Name: string
          Type: Type
          IsRecursive: bool
          IsExported: bool
          Export: ExportMetadata option
          Span: Span
          Value: Expression }

    and ExportMetadata =
        { Name: string
          Span: Span }

    type Function =
        { Id: FunctionId
          Parameter: SymbolId
          Name: string
          ParameterType: Type
          Captures: SymbolId list
          ReturnType: Type
          Span: Span
          Body: Expression }

    type ImportedRecord =
        { Name: string
          ModulePath: string
          MemberName: string
          IsUnion: bool
          Fields: (string * Type) list
          Span: Span }

    type TypeDeclaration =
        { Id: SymbolId
          Name: string
          IsRecursive: bool
          Definition: Type
          Import: ImportedRecord option
          Span: Span }

    type Program =
        { Types: TypeDeclaration list
          ImportedRecords: ImportedRecord list
          Functions: Function list
          Declarations: Declaration list
          Entry: Expression option }

    let symbolNumber (SymbolId value) = value
    let functionNumber (FunctionId value) = value

    let format (program: Program) =
        let lines = ResizeArray<string>()
        let typeName = Types.typeToString
        let symbols values = values |> List.map (symbolNumber >> string) |> String.concat "," |> fun value -> "[" + value + "]"
        let rec expression indent (value: Expression) =
            let prefix = System.String(' ', indent)
            let add label = lines.Add($"{prefix}{label} : {typeName value.Type}")
            let children label values = add label; values |> List.iter (expression (indent + 2))
            match value.Node with
            | Constant constant -> add $"const {constant}"
            | Load id -> add $"load %%{symbolNumber id}"
            | MakeClosure(id, captures) -> add $"make-closure @{functionNumber id} captures={symbols captures}"
            | CallClosure(fn, argument) -> children "call-closure" [ fn; argument ]
            | PipeClosure(value, fn) -> children "pipe-closure" [ value; fn ]
            | Branch(condition, whenTrue, whenFalse) -> children "branch" [ condition; whenTrue; whenFalse ]
            | Operation(operation, arguments) -> children $"operation {operation}" arguments
            | Tuple items -> children "tuple" items
            | List items -> children "list" items
            | Record fields -> children ("record " + (fields |> List.map fst |> String.concat ",")) (List.map snd fields)
            | FieldGet(target, name) -> children $"field-get {name}" [ target ]
            | SomeValue item -> children "some" [ item ]
            | NoneValue -> add "none"
            | Cons(head, tail) -> children "cons" [ head; tail ]
            | Append(left, right) -> children "append" [ left; right ]
            | Let(id, name, recursive, binding, body) -> children $"let %%{symbolNumber id} {name} recursive={recursive}" [ binding; body ]
            | LetRecursiveGroup(bindings, body) -> children ("let-rec-group " + (bindings |> List.map (fun (id, name, _, _) -> $"%%{symbolNumber id}:{name}") |> String.concat ",")) ((bindings |> List.map (fun (_, _, _, item) -> item)) @ [ body ])
            | Fail item -> children "fail" [ item ]
            | DecisionTree(target, cases) ->
                add $"decision-tree cases={cases.Length}"; expression (indent + 2) target
                cases |> List.iter (fun case -> case.Guard |> Option.iter (expression (indent + 2)); expression (indent + 2) case.Body)
            | Range(first, last) -> children "range" [ first; last ]
            | RecordUpdate(target, fields) -> children ("record-update " + (fields |> List.map fst |> String.concat ",")) (target :: List.map snd fields)
            | RecordConvert(target, targetType) -> children $"record-convert {typeName targetType}" [ target ]
            | IndexGet(target, index) -> children "index-get" [ target; index ]
            | Interpolate parts -> children "interpolate" (parts |> List.choose (function TextPart _ -> None | ValuePart item -> Some item))
            | UnionCase(typeName, caseName, payload) -> children $"union {typeName}.{caseName}" (Option.toList payload)
            | Map entries -> children "map" (entries |> List.collect (function KeyValue(key, item) -> [ key; item ] | Spread item -> [ item ]))
            | Intrinsic name -> add $"intrinsic {name}"
            | HostExternal(name, arity) -> add $"host-external {name}/{arity}"
            | RuntimeContext -> add "runtime-context"
            | TargetImport(modulePath, memberName) -> add $"target-import {modulePath}.{memberName}"
            | ForEach(id, name, _, source, body) -> children $"for-each %%{symbolNumber id} {name}" [ source; body ]
            | TypeToken tokenType -> add $"type-token {typeName tokenType}"
        for typeDeclaration in program.Types do
            lines.Add($"type %%{symbolNumber typeDeclaration.Id} {typeDeclaration.Name}: {typeName typeDeclaration.Definition}")
        for fn in program.Functions do
            lines.Add($"function @{functionNumber fn.Id} %%{symbolNumber fn.Parameter} {fn.Name} captures={symbols fn.Captures}: {typeName fn.ParameterType} -> {typeName fn.ReturnType}")
            expression 2 fn.Body
        for declaration in program.Declarations do
            let export = declaration.Export |> Option.map (fun metadata -> $" export={metadata.Name}") |> Option.defaultValue ""
            lines.Add($"decl %%{symbolNumber declaration.Id} {declaration.Name}{export}: {typeName declaration.Type}")
            expression 2 declaration.Value
        program.Entry |> Option.iter (fun entry -> lines.Add("entry"); expression 2 entry)
        System.String.Join("\n", lines) + "\n"

module LirLowering =
    type private State =
        { mutable NextFunction: int
          Functions: ResizeArray<Lir.Function> }

    let private newFunction state =
        let id = Lir.FunctionId state.NextFunction
        state.NextFunction <- state.NextFunction + 1
        id

    let private symbol (Hir.SymbolId value) = Lir.SymbolId value

    let private operation = function
        | Hir.IntAdd -> Lir.IntAdd | Hir.FloatAdd -> Lir.FloatAdd
        | Hir.IntSubtract -> Lir.IntSubtract | Hir.FloatSubtract -> Lir.FloatSubtract
        | Hir.IntMultiply -> Lir.IntMultiply | Hir.FloatMultiply -> Lir.FloatMultiply
        | Hir.IntDivide -> Lir.IntDivide | Hir.FloatDivide -> Lir.FloatDivide
        | Hir.IntRemainder -> Lir.IntRemainder | Hir.StructuralEqual -> Lir.StructuralEqual
        | Hir.NumericLessThan -> Lir.NumericLessThan | Hir.NumericGreaterThan -> Lir.NumericGreaterThan
        | Hir.NumericLessThanOrEqual -> Lir.NumericLessThanOrEqual
        | Hir.NumericGreaterThanOrEqual -> Lir.NumericGreaterThanOrEqual
        | Hir.BooleanAnd -> Lir.BooleanAnd | Hir.BooleanOr -> Lir.BooleanOr

    let private constant = function
        | Hir.Unit -> Lir.Unit | Hir.Int value -> Lir.Int value | Hir.Float value -> Lir.Float value
        | Hir.Bool value -> Lir.Bool value | Hir.String value -> Lir.String value

    let private appendPlan (left: Lir.PatternPlan) (right: Lir.PatternPlan) : Lir.PatternPlan =
        { Span = left.Span
          Checks = left.Checks @ right.Checks
          Bindings = left.Bindings @ right.Bindings
          Maps = left.Maps @ right.Maps }

    let rec private patternPlan projection (value: Hir.Pattern) : Lir.PatternPlan =
        let empty () : Lir.PatternPlan = { Span = value.Span; Checks = []; Bindings = []; Maps = [] }
        let combine plans = plans |> List.fold appendPlan (empty ())
        match value.Node with
        | Hir.Wildcard -> empty ()
        | Hir.Binding(id, name) ->
            { empty () with
                Bindings = [ { Id = symbol id; Name = name; Type = value.Type; Projection = projection } ] }
        | Hir.LiteralPattern item -> { empty () with Checks = [ Lir.LiteralCheck(projection, constant item) ] }
        | Hir.EmptyList -> { empty () with Checks = [ Lir.EmptyListCheck projection ] }
        | Hir.ConsPattern(head, tail) ->
            combine
                [ { empty () with Checks = [ Lir.NonEmptyListCheck projection ] }
                  patternPlan (projection @ [ Lir.ListHead ]) head
                  patternPlan (projection @ [ Lir.ListTail ]) tail ]
        | Hir.TuplePattern items ->
            { empty () with Checks = [ Lir.TupleArityCheck(projection, items.Length) ] }
            :: (items |> List.mapi (fun index item -> patternPlan (projection @ [ Lir.TupleItem index ]) item))
            |> combine
        | Hir.RecordPattern fields ->
            fields
            |> List.collect (fun (name, item) ->
                [ { empty () with Checks = [ Lir.RecordFieldCheck(projection, name) ] }
                  patternPlan (projection @ [ Lir.RecordField name ]) item ])
            |> combine
        | Hir.MapPattern(clauses, tail) ->
            let mapPlan : Lir.MapPlan =
                { Projection = projection
                  Clauses =
                    clauses
                    |> List.map (fun (key, item) ->
                        { Key = patternPlan [] key
                          Value = patternPlan [] item })
                  Tail = Option.map (patternPlan []) tail
                  RequireEmptyRemainder = clauses.IsEmpty && tail.IsNone }
            { empty () with Maps = [ mapPlan ] }
        | Hir.SomePattern item ->
            combine
                [ { empty () with Checks = [ Lir.SomeCheck projection ] }
                  patternPlan (projection @ [ Lir.OptionValue ]) item ]
        | Hir.NonePattern -> { empty () with Checks = [ Lir.NoneCheck projection ] }
        | Hir.UnionPattern(typeName, caseName, payload) ->
            combine
                [ { empty () with Checks = [ Lir.UnionCaseCheck(projection, typeName, caseName, payload.IsSome) ] }
                  yield! payload |> Option.map (patternPlan (projection @ [ Lir.UnionPayload ])) |> Option.toList ]
        | Hir.TypePattern patternType -> { empty () with Checks = [ Lir.TypeCheck(projection, patternType) ] }

    let rec private patternBindings (plan: Lir.PatternPlan) =
        [ plan.Bindings |> List.map _.Id |> Set.ofList
          yield!
            plan.Maps
            |> List.collect (fun mapPlan ->
                [ yield! mapPlan.Clauses |> List.collect (fun clause -> [ patternBindings clause.Key; patternBindings clause.Value ])
                  yield! mapPlan.Tail |> Option.map patternBindings |> Option.toList ]) ]
        |> Set.unionMany

    let rec capturedReferences bound (value: Lir.Expression) =
        let free = capturedReferences bound
        match value.Node with
        | Lir.Load id -> if Set.contains id bound then Set.empty else Set.singleton id
        | Lir.MakeClosure(_, captures) -> Set.difference (Set.ofList captures) bound
        | Lir.CallClosure(fn, argument) | Lir.PipeClosure(argument, fn) | Lir.Append(fn, argument) | Lir.Cons(fn, argument)
        | Lir.Range(fn, argument) | Lir.IndexGet(fn, argument) -> Set.union (free fn) (free argument)
        | Lir.Branch(condition, whenTrue, whenFalse) -> [ free condition; free whenTrue; free whenFalse ] |> Set.unionMany
        | Lir.Operation(_, arguments) | Lir.Tuple arguments | Lir.List arguments -> arguments |> List.map free |> Set.unionMany
        | Lir.Record fields -> fields |> List.map (snd >> free) |> Set.unionMany
        | Lir.FieldGet(target, _) | Lir.SomeValue target | Lir.Fail target | Lir.RecordConvert(target, _) -> free target
        | Lir.Let(id, _, recursive, binding, body) ->
            let scope = Set.add id bound
            Set.union (capturedReferences (if recursive then scope else bound) binding) (capturedReferences scope body)
        | Lir.LetRecursiveGroup(bindings, body) ->
            let scope = bindings |> List.fold (fun state (id, _, _, _) -> Set.add id state) bound
            (capturedReferences scope body :: (bindings |> List.map (fun (_, _, _, binding) -> capturedReferences scope binding))) |> Set.unionMany
        | Lir.DecisionTree(target, cases) ->
            free target
            :: (cases |> List.collect (fun case ->
                let scope = Set.union bound (patternBindings case.Plan)
                [ yield! case.Guard |> Option.map (capturedReferences scope) |> Option.toList
                  capturedReferences scope case.Body ]))
            |> Set.unionMany
        | Lir.RecordUpdate(target, fields) -> free target :: (fields |> List.map (snd >> free)) |> Set.unionMany
        | Lir.Interpolate parts -> parts |> List.choose (function Lir.TextPart _ -> None | Lir.ValuePart item -> Some(free item)) |> Set.unionMany
        | Lir.UnionCase(_, _, payload) -> payload |> Option.map free |> Option.defaultValue Set.empty
        | Lir.Map entries -> entries |> List.map (function Lir.KeyValue(key, item) -> Set.union (free key) (free item) | Lir.Spread item -> free item) |> Set.unionMany
        | Lir.ForEach(id, _, _, source, body) -> Set.union (free source) (capturedReferences (Set.add id bound) body)
        | Lir.Constant _ | Lir.NoneValue | Lir.Intrinsic _ | Lir.HostExternal _ | Lir.RuntimeContext
        | Lir.TargetImport _ | Lir.TypeToken _ -> Set.empty

    let rec private expression state (value: Hir.Expression) : Lir.Expression =
        let node =
            match value.Node with
            | Hir.Constant value -> Lir.Constant(constant value)
            | Hir.Reference id -> Lir.Load(symbol id)
            | Hir.Lambda(id, name, parameterType, body) ->
                let parameter = symbol id
                let functionId = newFunction state
                let loweredBody = expression state body
                let captures = capturedReferences (Set.singleton parameter) loweredBody |> Set.toList |> List.sortBy Lir.symbolNumber
                state.Functions.Add
                    { Id = functionId; Parameter = parameter; Name = name; ParameterType = parameterType
                      Captures = captures; ReturnType = loweredBody.Type; Span = value.Span; Body = loweredBody }
                Lir.MakeClosure(functionId, captures)
            | Hir.Apply(fn, argument) -> Lir.CallClosure(expression state fn, expression state argument)
            | Hir.Pipe(argument, fn) -> Lir.PipeClosure(expression state argument, expression state fn)
            | Hir.If(condition, whenTrue, whenFalse) -> Lir.Branch(expression state condition, expression state whenTrue, expression state whenFalse)
            | Hir.Primitive(op, arguments) -> Lir.Operation(operation op, arguments |> List.map (expression state))
            | Hir.Tuple items -> Lir.Tuple(items |> List.map (expression state))
            | Hir.List items -> Lir.List(items |> List.map (expression state))
            | Hir.Record fields -> Lir.Record(fields |> List.map (fun (name, item) -> name, expression state item))
            | Hir.FieldGet(target, name) -> Lir.FieldGet(expression state target, name)
            | Hir.SomeValue item -> Lir.SomeValue(expression state item)
            | Hir.NoneValue -> Lir.NoneValue
            | Hir.Cons(head, tail) -> Lir.Cons(expression state head, expression state tail)
            | Hir.Append(left, right) -> Lir.Append(expression state left, expression state right)
            | Hir.Let(id, name, recursive, binding, body) -> Lir.Let(symbol id, name, recursive, expression state binding, expression state body)
            | Hir.LetRecursiveGroup(bindings, body) ->
                Lir.LetRecursiveGroup(bindings |> List.map (fun (id, name, bindingType, value) -> symbol id, name, bindingType, expression state value), expression state body)
            | Hir.Fail value -> Lir.Fail(expression state value)
            | Hir.Match(target, cases) ->
                Lir.DecisionTree(expression state target, cases |> List.map (fun case ->
                    ({ Plan = patternPlan [] case.Pattern
                       Guard = Option.map (expression state) case.Guard
                       Body = expression state case.Body
                       Span = case.Span }: Lir.DecisionCase)))
            | Hir.Range(startValue, endValue) -> Lir.Range(expression state startValue, expression state endValue)
            | Hir.RecordUpdate(target, fields) -> Lir.RecordUpdate(expression state target, fields |> List.map (fun (name, item) -> name, expression state item))
            | Hir.RecordConvert(target, targetType) -> Lir.RecordConvert(expression state target, targetType)
            | Hir.IndexGet(target, index) -> Lir.IndexGet(expression state target, expression state index)
            | Hir.Interpolate parts -> Lir.Interpolate(parts |> List.map (function Hir.TextPart text -> Lir.TextPart text | Hir.ValuePart item -> Lir.ValuePart(expression state item)))
            | Hir.UnionCase(typeName, caseName, payload) -> Lir.UnionCase(typeName, caseName, Option.map (expression state) payload)
            | Hir.Map entries -> Lir.Map(entries |> List.map (function Hir.KeyValue(key, item) -> Lir.KeyValue(expression state key, expression state item) | Hir.Spread item -> Lir.Spread(expression state item)))
            | Hir.Intrinsic name -> Lir.Intrinsic name
            | Hir.HostExternal(name, arity) -> Lir.HostExternal(name, arity)
            | Hir.RuntimeContext -> Lir.RuntimeContext
            | Hir.TargetImport(modulePath, memberName) -> Lir.TargetImport(modulePath, memberName)
            | Hir.ForEach(id, name, itemType, source, body) -> Lir.ForEach(symbol id, name, itemType, expression state source, expression state body)
            | Hir.TypeToken tokenType -> Lir.TypeToken tokenType
        { Type = value.Type; Span = value.Span; Node = node }

    let lower (program: Hir.Program) : Lir.Program =
        let state = { NextFunction = 0; Functions = ResizeArray() }
        let declarations =
            program.Declarations
            |> List.map (fun value ->
                ({ Id = symbol value.Id; Name = value.Name; Type = value.Value.Type
                   IsRecursive = value.IsRecursive; IsExported = value.IsExported
                   Export = value.Export |> Option.map (fun export -> { Name = export.Name; Span = export.Span })
                   Span = value.Span; Value = expression state value.Value }: Lir.Declaration))
        let entry = program.Entry |> Option.map (expression state)
        { ImportedRecords =
            program.ImportedRecords
            |> List.map (fun value ->
                ({ Name = value.Name; ModulePath = value.ModulePath; MemberName = value.MemberName; IsUnion = value.IsUnion
                   Fields = value.Fields; Span = value.Span }: Lir.ImportedRecord))
          Types =
            program.Types
            |> List.map (fun value ->
                ({ Id = symbol value.Id; Name = value.Name; IsRecursive = value.IsRecursive; Definition = value.Definition
                   Import = value.Import |> Option.map (fun imported ->
                       { Name = imported.Name; ModulePath = imported.ModulePath; MemberName = imported.MemberName
                         IsUnion = imported.IsUnion; Fields = imported.Fields; Span = imported.Span })
                   Span = value.Span }: Lir.TypeDeclaration))
          Functions = state.Functions |> Seq.sortBy (_.Id >> Lir.functionNumber) |> Seq.toList
          Declarations = declarations
          Entry = entry }

module LirValidation =
    let rec private compatible left right =
        match left, right with
        | TVar _, _ | _, TVar _ -> true
        | TFun(la, lr), TFun(ra, rr) -> compatible la ra && compatible lr rr
        | TTuple left, TTuple right -> left.Length = right.Length && List.forall2 compatible left right
        | TList left, TList right | TOption left, TOption right | TTask left, TTask right -> compatible left right
        | TMap(lk, lv), TMap(rk, rv) -> compatible lk rk && compatible lv rv
        | TRecord left, TRecord right ->
            left.Count = right.Count
            && (left |> Map.forall (fun name item -> right |> Map.tryFind name |> Option.exists (compatible item)))
        | TUnion(ln, _), TUnion(rn, _) -> ln = rn
        | _ -> left = right

    let private literalType = function
        | Lir.Unit -> TUnit | Lir.Int _ -> TInt | Lir.Float _ -> TFloat
        | Lir.Bool _ -> TBool | Lir.String _ -> TString

    let validate (program: Lir.Program) =
        let ids = program.Declarations |> List.map _.Id
        let typeIds = program.Types |> List.map _.Id
        let typeDefinitions = program.Types |> List.map (fun declaration -> declaration.Name, declaration.Definition) |> Map.ofList
        let knownTypes = typeDefinitions |> Map.keys |> Set.ofSeq
        let resolve = function
            | TNamed name as named -> typeDefinitions |> Map.tryFind name |> Option.defaultValue named
            | value -> value
        let compatible left right =
            let rec loop visited left right =
                let resolvedLeft, resolvedRight = resolve left, resolve right
                let pair = resolvedLeft, resolvedRight
                if Set.contains pair visited then true
                else
                    let visited = Set.add pair visited
                    match resolvedLeft, resolvedRight with
                    | TVar _, _ | _, TVar _ -> true
                    | TFun(la, lr), TFun(ra, rr) -> loop visited la ra && loop visited lr rr
                    | TTuple left, TTuple right -> left.Length = right.Length && List.forall2 (loop visited) left right
                    | TList left, TList right | TOption left, TOption right | TTask left, TTask right -> loop visited left right
                    | TMap(lk, lv), TMap(rk, rv) -> loop visited lk rk && loop visited lv rv
                    | TRecord left, TRecord right ->
                        left.Count = right.Count
                        && (left |> Map.forall (fun name item -> right |> Map.tryFind name |> Option.exists (loop visited item)))
                    | TUnion(ln, _), TUnion(rn, _) -> ln = rn
                    | left, right -> left = right
            loop Set.empty left right
        let validSpan (span: Span) =
            span.Start.Line >= 0 && span.Start.Column >= 0 && span.End.Line >= 0 && span.End.Column >= 0
            && (span.Start.File <> span.End.File
                || span.End.Line > span.Start.Line
                || (span.End.Line = span.Start.Line && span.End.Column >= span.Start.Column))
        let rec validType = function
            | TVar variable -> variable >= 0
            | TFun(argument, result) -> validType argument && validType result
            | TTuple items -> items |> List.forall validType
            | TList item | TOption item | TTask item -> validType item
            | TMap(key, value) -> validType key && validType value
            | TRecord fields -> fields |> Map.forall (fun name fieldType -> not (System.String.IsNullOrWhiteSpace name) && validType fieldType)
            | TUnion(name, cases) ->
                Set.contains name knownTypes
                && cases |> Map.forall (fun caseName payload -> not (System.String.IsNullOrWhiteSpace caseName) && payload |> Option.forall validType)
            | TNamed name -> Set.contains name knownTypes
            | _ -> true
        let functionIds = program.Functions |> List.map _.Id
        let functions = program.Functions |> List.map (fun fn -> fn.Id, fn) |> Map.ofList
        let invalidImports =
            program.ImportedRecords |> List.exists (fun record ->
                System.String.IsNullOrWhiteSpace(record.ModulePath) || System.String.IsNullOrWhiteSpace(record.MemberName)
                || not (validSpan record.Span)
                || record.Fields |> List.exists (fun (name, fieldType) -> System.String.IsNullOrWhiteSpace name || not (validType fieldType)))
            || (program.ImportedRecords |> List.map _.Name |> List.distinct |> List.length) <> program.ImportedRecords.Length
        let invalidTypes =
            (program.Types |> List.map _.Name |> List.distinct |> List.length) <> program.Types.Length
            || (typeIds @ ids |> List.distinct |> List.length) <> typeIds.Length + ids.Length
            || program.Types |> List.exists (fun declaration ->
                not (validSpan declaration.Span) || not (validType declaration.Definition)
                || match declaration.Definition with
                   | TRecord _ -> false
                   | TUnion(name, _) -> name <> declaration.Name
                   | _ -> true)
        if invalidTypes then
            Error [ { Code = "FSL2016"; Stage = Lowering; Message = "LIR type declarations must have unique IDs and names with valid definitions."; Span = None } ]
        elif invalidImports then
            Error [ { Code = "FSL2012"; Stage = Lowering; Message = "LIR imported record linkage must be complete and uniquely named."; Span = None } ]
        elif ids.Length <> (ids |> Set.ofList |> Set.count) then
            Error [ { Code = "FSL2001"; Stage = Lowering; Message = "LIR declaration IDs must be unique."; Span = None } ]
        elif functionIds <> ([ 0 .. functionIds.Length - 1 ] |> List.map Lir.FunctionId) then
            Error [ { Code = "FSL2014"; Stage = Lowering; Message = "LIR function IDs must be unique, contiguous, and ordered."; Span = None } ]
        else
            let diagnostics = ResizeArray<CompilerDiagnostic>()
            let report code message span =
                diagnostics.Add({ Code = code; Stage = Lowering; Message = message; Span = Some span })
            let checkSpan span = if not (validSpan span) then report "FSL2002" "LIR source span is invalid." span
            let checkType span valueType = if not (validType valueType) then report "FSL2017" "LIR contains an unresolved or invalid semantic type." span
            program.Declarations |> List.iter (fun declaration ->
                if declaration.IsExported <> declaration.Export.IsSome || (declaration.Export |> Option.exists (fun export -> export.Name <> declaration.Name || not (validSpan export.Span))) then
                    report "FSL2015" "LIR export metadata is inconsistent." declaration.Span)
            let rec projectionType checks traversed current = function
                | [] -> Some current
                | step :: rest ->
                    let next =
                        match step, resolve current with
                        | Lir.ListHead, TList item -> Some item
                        | Lir.ListTail, TList _ -> Some current
                        | Lir.TupleItem index, TTuple items when index >= 0 && index < items.Length -> Some items.[index]
                        | Lir.RecordField name, TRecord fields -> Map.tryFind name fields
                        | Lir.OptionValue, TOption item -> Some item
                        | Lir.UnionPayload, TUnion(_, cases) ->
                            checks
                            |> List.tryPick (function
                                | Lir.UnionCaseCheck(projection, _, caseName, true) when projection = traversed ->
                                    Map.tryFind caseName cases |> Option.flatten
                                | _ -> None)
                        | _ -> None
                    next |> Option.bind (fun item -> projectionType checks (traversed @ [ step ]) item rest)
            let rec checkPlan expected (plan: Lir.PatternPlan) =
                checkSpan plan.Span
                checkType plan.Span expected
                let at projection = projectionType plan.Checks [] expected projection
                let require projection predicate message =
                    match at projection with Some value when predicate (resolve value) -> () | _ -> report "FSL2003" message plan.Span
                for check in plan.Checks do
                    match check with
                    | Lir.LiteralCheck(projection, value) -> require projection (compatible (literalType value)) "LIR literal plan has an inconsistent projection type."
                    | Lir.EmptyListCheck projection | Lir.NonEmptyListCheck projection -> require projection (function TList _ -> true | _ -> false) "LIR list plan requires a list projection."
                    | Lir.TupleArityCheck(projection, arity) -> require projection (function TTuple items -> items.Length = arity | _ -> false) "LIR tuple plan has an inconsistent arity."
                    | Lir.RecordFieldCheck(projection, name) -> require projection (function TRecord fields -> Map.containsKey name fields | _ -> false) $"LIR record plan references unknown field '{name}'."
                    | Lir.SomeCheck projection | Lir.NoneCheck projection -> require projection (function TOption _ -> true | _ -> false) "LIR option plan requires an option projection."
                    | Lir.UnionCaseCheck(projection, typeName, caseName, hasPayload) ->
                        require projection (function TUnion(actual, cases) when actual = typeName -> Map.tryFind caseName cases |> Option.exists (fun payload -> payload.IsSome = hasPayload) | _ -> false) "LIR union plan has inconsistent case metadata."
                    | Lir.TypeCheck(projection, patternType) -> checkType plan.Span patternType; require projection (fun _ -> true) "LIR type-test plan has an invalid projection."
                for binding in plan.Bindings do
                    checkType plan.Span binding.Type
                    require binding.Projection (compatible binding.Type) "LIR pattern binding has an inconsistent projection type."
                for mapPlan in plan.Maps do
                    match at mapPlan.Projection |> Option.map resolve with
                    | Some(TMap(keyType, valueType) as mapType) ->
                        mapPlan.Clauses |> List.iter (fun clause -> checkPlan keyType clause.Key; checkPlan valueType clause.Value)
                        mapPlan.Tail |> Option.iter (checkPlan mapType)
                    | _ -> report "FSL2003" "LIR map plan requires a map projection." plan.Span
            let rec patternBindings (plan: Lir.PatternPlan) =
                [ plan.Bindings |> List.map _.Id |> Set.ofList
                  yield!
                    plan.Maps
                    |> List.collect (fun mapPlan ->
                        [ yield! mapPlan.Clauses |> List.collect (fun clause -> [ patternBindings clause.Key; patternBindings clause.Value ])
                          yield! mapPlan.Tail |> Option.map patternBindings |> Option.toList ]) ]
                |> Set.unionMany
            let declared = ids |> Set.ofList
            let rec checkExpression locals (expression: Lir.Expression) =
                checkSpan expression.Span
                checkType expression.Span expression.Type
                let check = checkExpression locals
                match expression.Node with
                | Lir.Constant value when not (compatible expression.Type (literalType value)) -> report "FSL2004" "LIR constant has an inconsistent type." expression.Span
                | Lir.Load id when not (Set.contains id locals || Set.contains id declared) -> report "FSL2005" (sprintf "LIR reference %%%d is unresolved." (Lir.symbolNumber id)) expression.Span
                | Lir.MakeClosure(functionId, captures) ->
                    if captures |> List.exists (fun id -> not (Set.contains id locals || Set.contains id declared)) then
                        report "FSL2005" "LIR closure contains an unresolved capture." expression.Span
                    match Map.tryFind functionId functions with
                    | None -> report "FSL2014" "LIR closure references an unknown lifted function." expression.Span
                    | Some fn ->
                        if captures <> fn.Captures then report "FSL2013" "LIR closure captures do not match its lifted function." expression.Span
                        match expression.Type with
                        | TFun(argument, result) when compatible argument fn.ParameterType && compatible result fn.ReturnType -> ()
                        | _ -> report "FSL2006" "LIR closure signature is inconsistent with its lifted function." expression.Span
                | Lir.CallClosure(fn, argument) ->
                    check fn; check argument
                    match fn.Type with TFun(parameter, result) when compatible parameter argument.Type && compatible result expression.Type -> () | _ -> report "FSL2006" "LIR closure call signature is inconsistent." expression.Span
                | Lir.PipeClosure(argument, fn) ->
                    check argument; check fn
                    match fn.Type with TFun(parameter, result) when compatible parameter argument.Type && compatible result expression.Type -> () | _ -> report "FSL2006" "LIR pipeline call signature is inconsistent." expression.Span
                | Lir.Branch(condition, whenTrue, whenFalse) ->
                    check condition; check whenTrue; check whenFalse
                    if not (compatible condition.Type TBool && compatible expression.Type whenTrue.Type && compatible expression.Type whenFalse.Type) then report "FSL2007" "LIR branch types are inconsistent." expression.Span
                | Lir.Operation(operation, arguments) ->
                    arguments |> List.iter check
                    let expected =
                        match operation with
                        | Lir.IntAdd | Lir.IntSubtract | Lir.IntMultiply | Lir.IntDivide | Lir.IntRemainder -> Some(TInt, TInt)
                        | Lir.FloatAdd | Lir.FloatSubtract | Lir.FloatMultiply | Lir.FloatDivide -> Some(TFloat, TFloat)
                        | Lir.BooleanAnd | Lir.BooleanOr -> Some(TBool, TBool)
                        | Lir.StructuralEqual -> None
                        | _ -> Some((arguments |> List.tryHead |> Option.map _.Type |> Option.defaultValue TInt), TBool)
                    if arguments.Length <> 2 then report "FSL2008" "LIR primitive operation requires two operands." expression.Span
                    match expected with Some(operand, result) when not (arguments |> List.forall (fun item -> compatible operand item.Type) && compatible result expression.Type) -> report "FSL2008" "LIR primitive operand or result type is inconsistent." expression.Span | _ -> ()
                | Lir.Tuple items -> items |> List.iter check; match expression.Type with TTuple types when types.Length = items.Length && List.forall2 (fun expected (item: Lir.Expression) -> compatible expected item.Type) types items -> () | _ -> report "FSL2004" "LIR tuple representation is inconsistent." expression.Span
                | Lir.List items -> items |> List.iter check; match expression.Type with TList itemType when items |> List.forall (fun item -> compatible itemType item.Type) -> () | _ -> report "FSL2004" "LIR list representation is inconsistent." expression.Span
                | Lir.Record fields -> fields |> List.iter (snd >> check); match resolve expression.Type with TRecord types when fields.Length = types.Count && fields |> List.forall (fun (name, item) -> Map.tryFind name types |> Option.exists (fun expected -> compatible expected item.Type)) -> () | _ -> report "FSL2004" "LIR record representation is inconsistent." expression.Span
                | Lir.FieldGet(target, name) ->
                    check target
                    match resolve target.Type, name with
                    | TRecord fields, _ when Map.tryFind name fields |> Option.exists (compatible expression.Type) -> ()
                    | TNamed "Environment", "ScriptName" when compatible expression.Type (TOption TString) -> ()
                    | TNamed "Environment", "Arguments" when compatible expression.Type (TList TString) -> ()
                    | _ -> report "FSL2004" $"LIR field projection '{name}' is inconsistent." expression.Span
                | Lir.SomeValue item -> check item; match expression.Type with TOption itemType when compatible itemType item.Type -> () | _ -> report "FSL2004" "LIR option representation is inconsistent." expression.Span
                | Lir.NoneValue -> match expression.Type with TOption _ -> () | _ -> report "FSL2004" "LIR none value requires an option type." expression.Span
                | Lir.Cons(head, tail) -> check head; check tail; match expression.Type, tail.Type with TList item, TList tailItem when compatible item head.Type && compatible item tailItem -> () | _ -> report "FSL2004" "LIR cons representation is inconsistent." expression.Span
                | Lir.Append(left, right) -> check left; check right; if not (compatible expression.Type left.Type && compatible expression.Type right.Type) then report "FSL2004" "LIR append representation is inconsistent." expression.Span
                | Lir.Let(id, _, recursive, binding, body) -> let scope = Set.add id locals in checkExpression (if recursive then scope else locals) binding; checkExpression scope body; if not (compatible expression.Type body.Type) then report "FSL2004" "LIR let result type is inconsistent." expression.Span
                | Lir.LetRecursiveGroup(bindings, body) -> let scope = bindings |> List.fold (fun value (id, _, _, _) -> Set.add id value) locals in bindings |> List.iter (fun (_, _, bindingType, value) -> checkType value.Span bindingType; checkExpression scope value; if not (compatible bindingType value.Type) then report "FSL2006" "LIR recursive binding signature is inconsistent." value.Span); checkExpression scope body; if not (compatible expression.Type body.Type) then report "FSL2004" "LIR recursive group result is inconsistent." expression.Span
                | Lir.Fail value -> check value
                | Lir.DecisionTree(target, cases) -> check target; cases |> List.iter (fun case -> let caseLocals = Set.union locals (patternBindings case.Plan) in checkPlan target.Type case.Plan; case.Guard |> Option.iter (fun guard -> checkExpression caseLocals guard; if not (compatible guard.Type TBool) then report "FSL2007" "LIR decision guard must be Boolean." guard.Span); checkExpression caseLocals case.Body; if not (compatible expression.Type case.Body.Type) then report "FSL2007" "LIR decision body type is inconsistent." case.Body.Span)
                | Lir.Range(first, last) -> check first; check last; if not (compatible first.Type TInt && compatible last.Type TInt && compatible expression.Type (TList TInt)) then report "FSL2004" "LIR range representation is inconsistent." expression.Span
                | Lir.RecordUpdate(target, fields) ->
                    check target
                    fields |> List.iter (snd >> check)
                    match resolve target.Type, resolve expression.Type with
                    | TRecord targetFields, TRecord resultFields ->
                        let preservesTarget =
                            targetFields
                            |> Map.forall (fun name fieldType ->
                                resultFields |> Map.tryFind name |> Option.exists (compatible fieldType))
                        let validUpdates =
                            fields
                            |> List.forall (fun (name, value) ->
                                resultFields |> Map.tryFind name |> Option.exists (compatible value.Type))
                        if not (preservesTarget && validUpdates) then
                            report "FSL2004" "LIR record update representation is inconsistent." expression.Span
                    | _ -> report "FSL2004" "LIR record update representation is inconsistent." expression.Span
                | Lir.RecordConvert(target, targetType) -> check target; checkType expression.Span targetType; match resolve target.Type, resolve targetType with TRecord _, TRecord _ when compatible expression.Type targetType -> () | _ -> report "FSL2004" "LIR record conversion evidence is inconsistent." expression.Span
                | Lir.IndexGet(target, index) -> check target; check index; match target.Type, expression.Type with TList item, TOption result when compatible index.Type TInt && compatible item result -> () | TMap(key, value), TOption result when compatible key index.Type && compatible value result -> () | _ -> report "FSL2004" "LIR index representation is inconsistent." expression.Span
                | Lir.Interpolate parts -> parts |> List.iter (function Lir.TextPart _ -> () | Lir.ValuePart value -> check value); if not (compatible expression.Type TString) then report "FSL2004" "LIR interpolation must produce string." expression.Span
                | Lir.UnionCase(typeName, caseName, payload) -> payload |> Option.iter check; match resolve expression.Type with TUnion(actual, cases) when actual = typeName && Map.containsKey caseName cases -> () | _ -> report "FSL2004" "LIR union representation is inconsistent." expression.Span
                | Lir.Map entries -> entries |> List.iter (function Lir.KeyValue(key, value) -> check key; check value | Lir.Spread value -> check value); match expression.Type with TMap _ -> () | _ -> report "FSL2004" "LIR map node requires a map type." expression.Span
                | Lir.Intrinsic name when not (Map.containsKey name BuiltinSignatures.builtinSchemes || Map.containsKey name RuntimeIntrinsics.schemes) -> report "FSL2009" $"Unknown LIR intrinsic '{name}'." expression.Span
                | Lir.HostExternal(_, arity) when arity < 0 -> report "FSL2010" "LIR host external arity cannot be negative." expression.Span
                | Lir.TargetImport(modulePath, memberName) when System.String.IsNullOrWhiteSpace(modulePath) || System.String.IsNullOrWhiteSpace(memberName) -> report "FSL2011" "LIR target import linkage is incomplete." expression.Span
                | Lir.ForEach(id, _, itemType, source, body) -> checkType expression.Span itemType; check source; checkExpression (Set.add id locals) body; match source.Type with TList actual when compatible actual itemType && compatible expression.Type TUnit -> () | _ -> report "FSL2004" "LIR for-loop representation is inconsistent." expression.Span
                | Lir.TypeToken tokenType -> checkType expression.Span tokenType; if not (compatible expression.Type TTypeToken) then report "FSL2004" "LIR type token has an inconsistent type." expression.Span
                | _ -> ()
            program.Functions |> List.iter (fun fn ->
                checkSpan fn.Span
                checkType fn.Span fn.ParameterType
                checkType fn.Span fn.ReturnType
                let expected = LirLowering.capturedReferences (Set.singleton fn.Parameter) fn.Body |> Set.toList |> List.sortBy Lir.symbolNumber
                if fn.Captures <> expected then report "FSL2013" "Lifted LIR function captures do not match its ordered free-symbol set." fn.Span
                checkExpression (Set.add fn.Parameter (Set.ofList fn.Captures)) fn.Body
                if not (compatible fn.ReturnType fn.Body.Type) then report "FSL2006" "Lifted LIR function return type is inconsistent." fn.Span)
            program.Declarations |> List.iter (fun declaration -> checkSpan declaration.Span; checkType declaration.Span declaration.Type; checkExpression Set.empty declaration.Value; if not (compatible declaration.Type declaration.Value.Type) then report "FSL2004" $"LIR declaration '{declaration.Name}' type is inconsistent." declaration.Span)
            program.Entry |> Option.iter (checkExpression Set.empty)
            if diagnostics.Count = 0 then Ok program else Error(List.ofSeq diagnostics)

module LirOptimization =
    type Effect =
        | Pure
        | Observable

    let private combine effects =
        if effects |> List.exists ((=) Observable) then Observable else Pure

    let rec effect (expression: Lir.Expression) =
        let child = effect
        match expression.Node with
        | Lir.Constant _ | Lir.Load _ | Lir.MakeClosure _ | Lir.NoneValue | Lir.Intrinsic _
        | Lir.HostExternal _ | Lir.RuntimeContext | Lir.TargetImport _ | Lir.TypeToken _ -> Pure
        | Lir.CallClosure _ | Lir.PipeClosure _ | Lir.Fail _ | Lir.ForEach _ -> Observable
        | Lir.Operation((Lir.IntDivide | Lir.IntRemainder), _) -> Observable
        | Lir.Operation(_, arguments) | Lir.Tuple arguments | Lir.List arguments -> arguments |> List.map child |> combine
        | Lir.Branch(condition, whenTrue, whenFalse) -> [ child condition; child whenTrue; child whenFalse ] |> combine
        | Lir.Record fields -> fields |> List.map (snd >> child) |> combine
        | Lir.FieldGet(target, _) | Lir.SomeValue target | Lir.RecordConvert(target, _) -> child target
        | Lir.Cons(head, tail) | Lir.Append(head, tail) | Lir.Range(head, tail) | Lir.IndexGet(head, tail) -> [ child head; child tail ] |> combine
        | Lir.Let(_, _, _, binding, body) -> [ child binding; child body ] |> combine
        | Lir.LetRecursiveGroup(bindings, body) -> child body :: (bindings |> List.map (fun (_, _, _, value) -> child value)) |> combine
        | Lir.DecisionTree(target, cases) ->
            child target
            :: (cases |> List.collect (fun case -> [ yield! case.Guard |> Option.map child |> Option.toList; child case.Body ]))
            |> combine
        | Lir.RecordUpdate(target, fields) -> child target :: (fields |> List.map (snd >> child)) |> combine
        | Lir.Interpolate parts -> parts |> List.choose (function Lir.TextPart _ -> None | Lir.ValuePart value -> Some(child value)) |> combine
        | Lir.UnionCase(_, _, payload) -> payload |> Option.map child |> Option.defaultValue Pure
        | Lir.Map entries ->
            entries
            |> List.collect (function Lir.KeyValue(key, value) -> [ child key; child value ] | Lir.Spread value -> [ child value ])
            |> combine

    let rec loadedSymbols (expression: Lir.Expression) =
        let child = loadedSymbols
        match expression.Node with
        | Lir.Load id -> Set.singleton id
        | Lir.CallClosure(fn, argument) | Lir.PipeClosure(argument, fn) | Lir.Cons(fn, argument)
        | Lir.Append(fn, argument) | Lir.Range(fn, argument) | Lir.IndexGet(fn, argument) -> Set.union (child fn) (child argument)
        | Lir.Branch(condition, whenTrue, whenFalse) -> [ child condition; child whenTrue; child whenFalse ] |> Set.unionMany
        | Lir.Operation(_, arguments) | Lir.Tuple arguments | Lir.List arguments -> arguments |> List.map child |> Set.unionMany
        | Lir.Record fields -> fields |> List.map (snd >> child) |> Set.unionMany
        | Lir.FieldGet(target, _) | Lir.SomeValue target | Lir.Fail target | Lir.RecordConvert(target, _) -> child target
        | Lir.Let(_, _, _, binding, body) -> Set.union (child binding) (child body)
        | Lir.LetRecursiveGroup(bindings, body) -> child body :: (bindings |> List.map (fun (_, _, _, value) -> child value)) |> Set.unionMany
        | Lir.DecisionTree(target, cases) ->
            child target
            :: (cases |> List.collect (fun case -> [ yield! case.Guard |> Option.map child |> Option.toList; child case.Body ]))
            |> Set.unionMany
        | Lir.RecordUpdate(target, fields) -> child target :: (fields |> List.map (snd >> child)) |> Set.unionMany
        | Lir.Interpolate parts -> parts |> List.choose (function Lir.TextPart _ -> None | Lir.ValuePart value -> Some(child value)) |> Set.unionMany
        | Lir.UnionCase(_, _, payload) -> payload |> Option.map child |> Option.defaultValue Set.empty
        | Lir.Map entries -> entries |> List.map (function Lir.KeyValue(key, value) -> Set.union (child key) (child value) | Lir.Spread value -> child value) |> Set.unionMany
        | Lir.ForEach(_, _, _, source, body) -> Set.union (child source) (child body)
        | Lir.Constant _ | Lir.MakeClosure _ | Lir.NoneValue | Lir.Intrinsic _ | Lir.HostExternal _
        | Lir.RuntimeContext | Lir.TargetImport _ | Lir.TypeToken _ -> Set.empty

    let rec private simplify (expression: Lir.Expression) =
        let recurse (value: Lir.Expression) : Lir.Expression = simplify value
        let withNode node = { expression with Node = node }
        match expression.Node with
        | Lir.Branch(condition, whenTrue, whenFalse) ->
            let condition = recurse condition
            match condition.Node with
            | Lir.Constant(Lir.Bool true) -> recurse whenTrue
            | Lir.Constant(Lir.Bool false) -> recurse whenFalse
            | _ -> withNode (Lir.Branch(condition, recurse whenTrue, recurse whenFalse))
        | Lir.Operation(Lir.BooleanAnd, [ left; right ]) ->
            let left = recurse left
            match left.Node with
            | Lir.Constant(Lir.Bool false) -> left
            | Lir.Constant(Lir.Bool true) -> recurse right
            | _ -> withNode (Lir.Operation(Lir.BooleanAnd, [ left; recurse right ]))
        | Lir.Operation(Lir.BooleanOr, [ left; right ]) ->
            let left = recurse left
            match left.Node with
            | Lir.Constant(Lir.Bool true) -> left
            | Lir.Constant(Lir.Bool false) -> recurse right
            | _ -> withNode (Lir.Operation(Lir.BooleanOr, [ left; recurse right ]))
        | Lir.Let(id, name, recursive, binding, body) ->
            let binding, body = recurse binding, recurse body
            match body.Node with
            | Lir.Load loaded when loaded = id && not recursive -> binding
            | _ when not (Set.contains id (loadedSymbols body)) && effect binding = Pure -> body
            | _ -> withNode (Lir.Let(id, name, recursive, binding, body))
        | Lir.LetRecursiveGroup(bindings, body) ->
            let bindings = bindings |> List.map (fun (id, name, valueType, value) -> id, name, valueType, recurse value)
            let body = recurse body
            let ids = bindings |> List.map (fun (id, _, _, _) -> id) |> Set.ofList
            let used = Set.intersect ids (loadedSymbols body)
            if used.IsEmpty && bindings |> List.forall (fun (_, _, _, value) -> effect value = Pure) then body
            else withNode (Lir.LetRecursiveGroup(bindings, body))
        | Lir.CallClosure(fn, argument) -> withNode (Lir.CallClosure(recurse fn, recurse argument))
        | Lir.PipeClosure(argument, fn) -> withNode (Lir.PipeClosure(recurse argument, recurse fn))
        | Lir.Operation(operation, arguments) -> withNode (Lir.Operation(operation, List.map recurse arguments))
        | Lir.Tuple items -> withNode (Lir.Tuple(List.map recurse items))
        | Lir.List items -> withNode (Lir.List(List.map recurse items))
        | Lir.Record fields -> withNode (Lir.Record(fields |> List.map (fun (name, value) -> name, recurse value)))
        | Lir.FieldGet(target, name) -> withNode (Lir.FieldGet(recurse target, name))
        | Lir.SomeValue value -> withNode (Lir.SomeValue(recurse value))
        | Lir.Cons(head, tail) -> withNode (Lir.Cons(recurse head, recurse tail))
        | Lir.Append(left, right) -> withNode (Lir.Append(recurse left, recurse right))
        | Lir.Fail value -> withNode (Lir.Fail(recurse value))
        | Lir.DecisionTree(target, cases) ->
            withNode (Lir.DecisionTree(recurse target, cases |> List.map (fun case -> { case with Guard = Option.map recurse case.Guard; Body = recurse case.Body })))
        | Lir.Range(first, last) -> withNode (Lir.Range(recurse first, recurse last))
        | Lir.RecordUpdate(target, fields) -> withNode (Lir.RecordUpdate(recurse target, fields |> List.map (fun (name, value) -> name, recurse value)))
        | Lir.RecordConvert(target, targetType) -> withNode (Lir.RecordConvert(recurse target, targetType))
        | Lir.IndexGet(target, index) -> withNode (Lir.IndexGet(recurse target, recurse index))
        | Lir.Interpolate parts -> withNode (Lir.Interpolate(parts |> List.map (function Lir.TextPart text -> Lir.TextPart text | Lir.ValuePart value -> Lir.ValuePart(recurse value))))
        | Lir.UnionCase(typeName, caseName, payload) -> withNode (Lir.UnionCase(typeName, caseName, Option.map recurse payload))
        | Lir.Map entries -> withNode (Lir.Map(entries |> List.map (function Lir.KeyValue(key, value) -> Lir.KeyValue(recurse key, recurse value) | Lir.Spread value -> Lir.Spread(recurse value))))
        | Lir.ForEach(id, name, itemType, source, body) -> withNode (Lir.ForEach(id, name, itemType, recurse source, recurse body))
        | _ -> expression

    let rec private rewriteClosures functionMap captureMap (expression: Lir.Expression) =
        let recurse = rewriteClosures functionMap captureMap
        let withNode node = { expression with Node = node }
        match expression.Node with
        | Lir.MakeClosure(id, captures) ->
            let mapped = Map.tryFind id functionMap |> Option.defaultValue id
            withNode (Lir.MakeClosure(mapped, Map.tryFind id captureMap |> Option.defaultValue captures))
        | Lir.CallClosure(fn, argument) -> withNode (Lir.CallClosure(recurse fn, recurse argument))
        | Lir.PipeClosure(argument, fn) -> withNode (Lir.PipeClosure(recurse argument, recurse fn))
        | Lir.Branch(condition, whenTrue, whenFalse) -> withNode (Lir.Branch(recurse condition, recurse whenTrue, recurse whenFalse))
        | Lir.Operation(operation, arguments) -> withNode (Lir.Operation(operation, List.map recurse arguments))
        | Lir.Tuple items -> withNode (Lir.Tuple(List.map recurse items))
        | Lir.List items -> withNode (Lir.List(List.map recurse items))
        | Lir.Record fields -> withNode (Lir.Record(fields |> List.map (fun (name, value) -> name, recurse value)))
        | Lir.FieldGet(target, name) -> withNode (Lir.FieldGet(recurse target, name))
        | Lir.SomeValue value -> withNode (Lir.SomeValue(recurse value))
        | Lir.Cons(head, tail) -> withNode (Lir.Cons(recurse head, recurse tail))
        | Lir.Append(left, right) -> withNode (Lir.Append(recurse left, recurse right))
        | Lir.Let(id, name, recursive, binding, body) -> withNode (Lir.Let(id, name, recursive, recurse binding, recurse body))
        | Lir.LetRecursiveGroup(bindings, body) -> withNode (Lir.LetRecursiveGroup(bindings |> List.map (fun (id, name, valueType, value) -> id, name, valueType, recurse value), recurse body))
        | Lir.Fail value -> withNode (Lir.Fail(recurse value))
        | Lir.DecisionTree(target, cases) -> withNode (Lir.DecisionTree(recurse target, cases |> List.map (fun case -> { case with Guard = Option.map recurse case.Guard; Body = recurse case.Body })))
        | Lir.Range(first, last) -> withNode (Lir.Range(recurse first, recurse last))
        | Lir.RecordUpdate(target, fields) -> withNode (Lir.RecordUpdate(recurse target, fields |> List.map (fun (name, value) -> name, recurse value)))
        | Lir.RecordConvert(target, targetType) -> withNode (Lir.RecordConvert(recurse target, targetType))
        | Lir.IndexGet(target, index) -> withNode (Lir.IndexGet(recurse target, recurse index))
        | Lir.Interpolate parts -> withNode (Lir.Interpolate(parts |> List.map (function Lir.TextPart text -> Lir.TextPart text | Lir.ValuePart value -> Lir.ValuePart(recurse value))))
        | Lir.UnionCase(typeName, caseName, payload) -> withNode (Lir.UnionCase(typeName, caseName, Option.map recurse payload))
        | Lir.Map entries -> withNode (Lir.Map(entries |> List.map (function Lir.KeyValue(key, value) -> Lir.KeyValue(recurse key, recurse value) | Lir.Spread value -> Lir.Spread(recurse value))))
        | Lir.ForEach(id, name, itemType, source, body) -> withNode (Lir.ForEach(id, name, itemType, recurse source, recurse body))
        | _ -> expression

    let optimize (program: Lir.Program) =
        let simplifiedFunctions = program.Functions |> List.map (fun fn -> { fn with Body = simplify fn.Body })
        let captureMap =
            simplifiedFunctions
            |> List.map (fun fn -> fn.Id, (LirLowering.capturedReferences (Set.singleton fn.Parameter) fn.Body |> Set.toList |> List.sortBy Lir.symbolNumber))
            |> Map.ofList
        let identityFunctionMap = simplifiedFunctions |> List.map (fun fn -> fn.Id, fn.Id) |> Map.ofList
        let functions = simplifiedFunctions |> List.map (fun fn -> { fn with Captures = captureMap.[fn.Id]; Body = rewriteClosures identityFunctionMap captureMap fn.Body })
        let declarations = program.Declarations |> List.map (fun declaration -> { declaration with Value = declaration.Value |> simplify |> rewriteClosures identityFunctionMap captureMap })
        let entry = program.Entry |> Option.map (simplify >> rewriteClosures identityFunctionMap captureMap)
        let declarationMap = declarations |> List.map (fun declaration -> declaration.Id, declaration) |> Map.ofList
        let functionById = functions |> List.map (fun fn -> fn.Id, fn) |> Map.ofList
        let reachableDeclarations = System.Collections.Generic.HashSet<Lir.SymbolId>()
        let reachableFunctions = System.Collections.Generic.HashSet<Lir.FunctionId>()
        let rec visitExpression (expression: Lir.Expression) =
            for id in loadedSymbols expression do
                match Map.tryFind id declarationMap with
                | Some declaration when reachableDeclarations.Add id -> visitExpression declaration.Value
                | _ -> ()
            let rec closures (value: Lir.Expression) =
                match value.Node with
                | Lir.MakeClosure(id, _) when reachableFunctions.Add id ->
                    match Map.tryFind id functionById with Some fn -> visitExpression fn.Body | None -> ()
                | _ ->
                    let collect child = closures child
                    match value.Node with
                    | Lir.CallClosure(fn, argument) | Lir.PipeClosure(argument, fn) | Lir.Cons(fn, argument)
                    | Lir.Append(fn, argument) | Lir.Range(fn, argument) | Lir.IndexGet(fn, argument) -> collect fn; collect argument
                    | Lir.Branch(condition, whenTrue, whenFalse) -> collect condition; collect whenTrue; collect whenFalse
                    | Lir.Operation(_, arguments) | Lir.Tuple arguments | Lir.List arguments -> List.iter collect arguments
                    | Lir.Record fields -> fields |> List.iter (snd >> collect)
                    | Lir.FieldGet(target, _) | Lir.SomeValue target | Lir.Fail target | Lir.RecordConvert(target, _) -> collect target
                    | Lir.Let(_, _, _, binding, body) -> collect binding; collect body
                    | Lir.LetRecursiveGroup(bindings, body) -> bindings |> List.iter (fun (_, _, _, binding) -> collect binding); collect body
                    | Lir.DecisionTree(target, cases) -> collect target; cases |> List.iter (fun case -> Option.iter collect case.Guard; collect case.Body)
                    | Lir.RecordUpdate(target, fields) -> collect target; fields |> List.iter (snd >> collect)
                    | Lir.Interpolate parts -> parts |> List.iter (function Lir.TextPart _ -> () | Lir.ValuePart item -> collect item)
                    | Lir.UnionCase(_, _, payload) -> Option.iter collect payload
                    | Lir.Map entries -> entries |> List.iter (function Lir.KeyValue(key, item) -> collect key; collect item | Lir.Spread item -> collect item)
                    | Lir.ForEach(_, _, _, source, body) -> collect source; collect body
                    | _ -> ()
            closures expression
        declarations
        |> List.iter (fun declaration ->
            if declaration.IsExported || effect declaration.Value = Observable then
                if reachableDeclarations.Add declaration.Id then visitExpression declaration.Value)
        Option.iter visitExpression entry
        let retainedFunctions = functions |> List.filter (fun fn -> reachableFunctions.Contains fn.Id)
        let functionMap = retainedFunctions |> List.mapi (fun index fn -> fn.Id, Lir.FunctionId index) |> Map.ofList
        let finalFunctions =
            retainedFunctions
            |> List.map (fun fn -> { fn with Id = functionMap.[fn.Id]; Body = rewriteClosures functionMap captureMap fn.Body })
        let rewrite = rewriteClosures functionMap captureMap
        { program with
            Functions = finalFunctions
            Declarations = declarations |> List.filter (fun declaration -> reachableDeclarations.Contains declaration.Id) |> List.map (fun declaration -> { declaration with Value = rewrite declaration.Value })
            Entry = Option.map rewrite entry }
