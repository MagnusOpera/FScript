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

    and Pattern =
        { Type: Type
          Span: Span
          Node: PatternNode }

    and PatternNode =
        | Wildcard
        | Binding of SymbolId * string
        | LiteralPattern of Constant
        | EmptyList
        | ConsPattern of Pattern * Pattern
        | TuplePattern of Pattern list
        | RecordPattern of (string * Pattern) list
        | MapPattern of (Pattern * Pattern) list * Pattern option
        | SomePattern of Pattern
        | NonePattern
        | UnionPattern of string * string * Pattern option
        | TypePattern of Type

    and DecisionCase =
        { Pattern: Pattern
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

    type Program =
        { ImportedRecords: ImportedRecord list
          Functions: Function list
          Declarations: Declaration list
          Entry: Expression option }

    let symbolNumber (SymbolId value) = value
    let functionNumber (FunctionId value) = value

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

    let rec private pattern (value: Hir.Pattern) : Lir.Pattern =
        let node =
            match value.Node with
            | Hir.Wildcard -> Lir.Wildcard
            | Hir.Binding(id, name) -> Lir.Binding(symbol id, name)
            | Hir.LiteralPattern value -> Lir.LiteralPattern(constant value)
            | Hir.EmptyList -> Lir.EmptyList
            | Hir.ConsPattern(head, tail) -> Lir.ConsPattern(pattern head, pattern tail)
            | Hir.TuplePattern items -> Lir.TuplePattern(List.map pattern items)
            | Hir.RecordPattern fields -> Lir.RecordPattern(fields |> List.map (fun (name, item) -> name, pattern item))
            | Hir.MapPattern(clauses, tail) -> Lir.MapPattern(clauses |> List.map (fun (key, value) -> pattern key, pattern value), Option.map pattern tail)
            | Hir.SomePattern item -> Lir.SomePattern(pattern item)
            | Hir.NonePattern -> Lir.NonePattern
            | Hir.UnionPattern(typeName, caseName, payload) -> Lir.UnionPattern(typeName, caseName, Option.map pattern payload)
            | Hir.TypePattern patternType -> Lir.TypePattern patternType
        { Type = value.Type; Span = value.Span; Node = node }

    let rec private patternBindings (value: Lir.Pattern) =
        match value.Node with
        | Lir.Binding(id, _) -> Set.singleton id
        | Lir.ConsPattern(head, tail) -> Set.union (patternBindings head) (patternBindings tail)
        | Lir.TuplePattern items -> items |> List.map patternBindings |> Set.unionMany
        | Lir.RecordPattern fields -> fields |> List.map (snd >> patternBindings) |> Set.unionMany
        | Lir.MapPattern(clauses, tail) ->
            [ yield! clauses |> List.collect (fun (key, value) -> [ patternBindings key; patternBindings value ])
              tail |> Option.map patternBindings |> Option.defaultValue Set.empty ]
            |> Set.unionMany
        | Lir.SomePattern item -> patternBindings item
        | Lir.UnionPattern(_, _, payload) -> payload |> Option.map patternBindings |> Option.defaultValue Set.empty
        | _ -> Set.empty

    let rec capturedReferences bound (value: Lir.Expression) =
        let free = capturedReferences bound
        match value.Node with
        | Lir.Load id -> if Set.contains id bound then Set.empty else Set.singleton id
        | Lir.MakeClosure(_, captures) -> Set.difference (Set.ofList captures) bound
        | Lir.CallClosure(fn, argument) | Lir.Append(fn, argument) | Lir.Cons(fn, argument)
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
                let scope = Set.union bound (patternBindings case.Pattern)
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
                    ({ Pattern = pattern case.Pattern
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
        let functionIds = program.Functions |> List.map _.Id
        let functions = program.Functions |> List.map (fun fn -> fn.Id, fn) |> Map.ofList
        let invalidImports =
            program.ImportedRecords |> List.exists (fun record -> System.String.IsNullOrWhiteSpace(record.ModulePath) || System.String.IsNullOrWhiteSpace(record.MemberName))
            || (program.ImportedRecords |> List.map _.Name |> List.distinct |> List.length) <> program.ImportedRecords.Length
        if invalidImports then
            Error [ { Code = "FSL2012"; Stage = Lowering; Message = "LIR imported record linkage must be complete and uniquely named."; Span = None } ]
        elif ids.Length <> (ids |> Set.ofList |> Set.count) then
            Error [ { Code = "FSL2001"; Stage = Lowering; Message = "LIR declaration IDs must be unique."; Span = None } ]
        elif functionIds <> ([ 0 .. functionIds.Length - 1 ] |> List.map Lir.FunctionId) then
            Error [ { Code = "FSL2014"; Stage = Lowering; Message = "LIR function IDs must be unique, contiguous, and ordered."; Span = None } ]
        else
            let diagnostics = ResizeArray<CompilerDiagnostic>()
            let report code message span =
                diagnostics.Add({ Code = code; Stage = Lowering; Message = message; Span = Some span })
            let validSpan (span: Span) =
                span.Start.Line >= 0 && span.Start.Column >= 0 && span.End.Line >= 0 && span.End.Column >= 0
                && (span.Start.File <> span.End.File
                    || span.End.Line > span.Start.Line
                    || (span.End.Line = span.Start.Line && span.End.Column >= span.Start.Column))
            let checkSpan span = if not (validSpan span) then report "FSL2002" "LIR source span is invalid." span
            program.Declarations |> List.iter (fun declaration ->
                if declaration.IsExported <> declaration.Export.IsSome || (declaration.Export |> Option.exists (fun export -> export.Name <> declaration.Name || not (validSpan export.Span))) then
                    report "FSL2015" "LIR export metadata is inconsistent." declaration.Span)
            let rec checkPattern expected (pattern: Lir.Pattern) =
                checkSpan pattern.Span
                if not (compatible expected pattern.Type) then report "FSL2003" "LIR pattern type does not match its scrutinee." pattern.Span
                match pattern.Node with
                | Lir.LiteralPattern value when not (compatible pattern.Type (literalType value)) -> report "FSL2003" "LIR literal pattern has an inconsistent type." pattern.Span
                | Lir.ConsPattern(head, tail) ->
                    match pattern.Type with
                    | TList item -> checkPattern item head; checkPattern pattern.Type tail
                    | _ -> report "FSL2003" "LIR cons pattern requires a list." pattern.Span
                | Lir.TuplePattern items ->
                    match pattern.Type with
                    | TTuple types when types.Length = items.Length -> List.iter2 checkPattern types items
                    | _ -> report "FSL2003" "LIR tuple pattern shape is inconsistent." pattern.Span
                | Lir.RecordPattern fields ->
                    match pattern.Type with
                    | TRecord types -> fields |> List.iter (fun (name, item) -> match Map.tryFind name types with Some itemType -> checkPattern itemType item | None -> report "FSL2003" $"Unknown LIR record-pattern field '{name}'." item.Span)
                    | _ -> report "FSL2003" "LIR record pattern requires a record." pattern.Span
                | Lir.SomePattern item -> match pattern.Type with TOption itemType -> checkPattern itemType item | _ -> report "FSL2003" "LIR some pattern requires an option." pattern.Span
                | Lir.EmptyList -> match pattern.Type with TList _ -> () | _ -> report "FSL2003" "LIR empty-list pattern requires a list." pattern.Span
                | Lir.NonePattern -> match pattern.Type with TOption _ -> () | _ -> report "FSL2003" "LIR none pattern requires an option." pattern.Span
                | Lir.MapPattern(clauses, tail) ->
                    match pattern.Type with
                    | TMap(keyType, valueType) -> clauses |> List.iter (fun (key, value) -> checkPattern keyType key; checkPattern valueType value); tail |> Option.iter (checkPattern pattern.Type)
                    | _ -> report "FSL2003" "LIR map pattern requires a map." pattern.Span
                | Lir.UnionPattern(typeName, caseName, payload) ->
                    match pattern.Type with
                    | TUnion(actual, cases) when actual = typeName ->
                        match Map.tryFind caseName cases, payload with
                        | Some(Some payloadType), Some value -> checkPattern payloadType value
                        | Some None, None -> ()
                        | _ -> report "FSL2003" "LIR union-pattern payload is inconsistent." pattern.Span
                    | _ -> report "FSL2003" "LIR union pattern has an inconsistent declared type." pattern.Span
                | _ -> ()
            let rec patternBindings (pattern: Lir.Pattern) =
                match pattern.Node with
                | Lir.Binding(id, _) -> Set.singleton id
                | Lir.ConsPattern(head, tail) -> Set.union (patternBindings head) (patternBindings tail)
                | Lir.TuplePattern items -> items |> List.map patternBindings |> Set.unionMany
                | Lir.RecordPattern fields -> fields |> List.map (snd >> patternBindings) |> Set.unionMany
                | Lir.MapPattern(clauses, tail) ->
                    [ yield! clauses |> List.collect (fun (key, value) -> [ patternBindings key; patternBindings value ])
                      match tail with Some value -> patternBindings value | None -> Set.empty ] |> Set.unionMany
                | Lir.SomePattern value -> patternBindings value
                | Lir.UnionPattern(_, _, payload) -> payload |> Option.map patternBindings |> Option.defaultValue Set.empty
                | _ -> Set.empty
            let declared = ids |> Set.ofList
            let rec checkExpression locals (expression: Lir.Expression) =
                checkSpan expression.Span
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
                | Lir.Record fields -> fields |> List.iter (snd >> check); match expression.Type with TRecord types when fields.Length = types.Count && fields |> List.forall (fun (name, item) -> Map.tryFind name types |> Option.exists (fun expected -> compatible expected item.Type)) -> () | _ -> report "FSL2004" "LIR record representation is inconsistent." expression.Span
                | Lir.FieldGet(target, name) ->
                    check target
                    match target.Type, name with
                    | TRecord fields, _ when Map.tryFind name fields |> Option.exists (compatible expression.Type) -> ()
                    | TNamed "Environment", "ScriptName" when compatible expression.Type (TOption TString) -> ()
                    | TNamed "Environment", "Arguments" when compatible expression.Type (TList TString) -> ()
                    | _ -> report "FSL2004" $"LIR field projection '{name}' is inconsistent." expression.Span
                | Lir.SomeValue item -> check item; match expression.Type with TOption itemType when compatible itemType item.Type -> () | _ -> report "FSL2004" "LIR option representation is inconsistent." expression.Span
                | Lir.NoneValue -> match expression.Type with TOption _ -> () | _ -> report "FSL2004" "LIR none value requires an option type." expression.Span
                | Lir.Cons(head, tail) -> check head; check tail; match expression.Type, tail.Type with TList item, TList tailItem when compatible item head.Type && compatible item tailItem -> () | _ -> report "FSL2004" "LIR cons representation is inconsistent." expression.Span
                | Lir.Append(left, right) -> check left; check right; if not (compatible expression.Type left.Type && compatible expression.Type right.Type) then report "FSL2004" "LIR append representation is inconsistent." expression.Span
                | Lir.Let(id, _, recursive, binding, body) -> let scope = Set.add id locals in checkExpression (if recursive then scope else locals) binding; checkExpression scope body; if not (compatible expression.Type body.Type) then report "FSL2004" "LIR let result type is inconsistent." expression.Span
                | Lir.LetRecursiveGroup(bindings, body) -> let scope = bindings |> List.fold (fun value (id, _, _, _) -> Set.add id value) locals in bindings |> List.iter (fun (_, _, bindingType, value) -> checkExpression scope value; if not (compatible bindingType value.Type) then report "FSL2006" "LIR recursive binding signature is inconsistent." value.Span); checkExpression scope body; if not (compatible expression.Type body.Type) then report "FSL2004" "LIR recursive group result is inconsistent." expression.Span
                | Lir.Fail value -> check value
                | Lir.DecisionTree(target, cases) -> check target; cases |> List.iter (fun case -> let caseLocals = Set.union locals (patternBindings case.Pattern) in checkPattern target.Type case.Pattern; case.Guard |> Option.iter (fun guard -> checkExpression caseLocals guard; if not (compatible guard.Type TBool) then report "FSL2007" "LIR decision guard must be Boolean." guard.Span); checkExpression caseLocals case.Body; if not (compatible expression.Type case.Body.Type) then report "FSL2007" "LIR decision body type is inconsistent." case.Body.Span)
                | Lir.Range(first, last) -> check first; check last; if not (compatible first.Type TInt && compatible last.Type TInt && compatible expression.Type (TList TInt)) then report "FSL2004" "LIR range representation is inconsistent." expression.Span
                | Lir.RecordUpdate(target, fields) -> check target; fields |> List.iter (snd >> check); if not (compatible expression.Type target.Type) then report "FSL2004" "LIR record update representation is inconsistent." expression.Span
                | Lir.RecordConvert(target, targetType) -> check target; match target.Type, targetType with TRecord _, (TNamed _ | TRecord _) when compatible expression.Type targetType -> () | TNamed _, TRecord _ when compatible expression.Type targetType -> () | _ -> report "FSL2004" "LIR record conversion evidence is inconsistent." expression.Span
                | Lir.IndexGet(target, index) -> check target; check index; match target.Type, expression.Type with TList item, TOption result when compatible index.Type TInt && compatible item result -> () | TMap(key, value), TOption result when compatible key index.Type && compatible value result -> () | _ -> report "FSL2004" "LIR index representation is inconsistent." expression.Span
                | Lir.Interpolate parts -> parts |> List.iter (function Lir.TextPart _ -> () | Lir.ValuePart value -> check value); if not (compatible expression.Type TString) then report "FSL2004" "LIR interpolation must produce string." expression.Span
                | Lir.UnionCase(typeName, caseName, payload) -> payload |> Option.iter check; match expression.Type with TUnion(actual, cases) when actual = typeName && Map.containsKey caseName cases -> () | _ -> report "FSL2004" "LIR union representation is inconsistent." expression.Span
                | Lir.Map entries -> entries |> List.iter (function Lir.KeyValue(key, value) -> check key; check value | Lir.Spread value -> check value); match expression.Type with TMap _ -> () | _ -> report "FSL2004" "LIR map node requires a map type." expression.Span
                | Lir.Intrinsic name when not (Map.containsKey name BuiltinSignatures.builtinSchemes || Map.containsKey name RuntimeIntrinsics.schemes) -> report "FSL2009" $"Unknown LIR intrinsic '{name}'." expression.Span
                | Lir.HostExternal(_, arity) when arity < 0 -> report "FSL2010" "LIR host external arity cannot be negative." expression.Span
                | Lir.TargetImport(modulePath, memberName) when System.String.IsNullOrWhiteSpace(modulePath) || System.String.IsNullOrWhiteSpace(memberName) -> report "FSL2011" "LIR target import linkage is incomplete." expression.Span
                | Lir.ForEach(id, _, itemType, source, body) -> check source; checkExpression (Set.add id locals) body; match source.Type with TList actual when compatible actual itemType && compatible expression.Type TUnit -> () | _ -> report "FSL2004" "LIR for-loop representation is inconsistent." expression.Span
                | Lir.TypeToken _ when not (compatible expression.Type TTypeToken) -> report "FSL2004" "LIR type token has an inconsistent type." expression.Span
                | _ -> ()
            program.Functions |> List.iter (fun fn ->
                checkSpan fn.Span
                let expected = LirLowering.capturedReferences (Set.singleton fn.Parameter) fn.Body |> Set.toList |> List.sortBy Lir.symbolNumber
                if fn.Captures <> expected then report "FSL2013" "Lifted LIR function captures do not match its ordered free-symbol set." fn.Span
                checkExpression (Set.add fn.Parameter (Set.ofList fn.Captures)) fn.Body
                if not (compatible fn.ReturnType fn.Body.Type) then report "FSL2006" "Lifted LIR function return type is inconsistent." fn.Span)
            program.Declarations |> List.iter (fun declaration -> checkSpan declaration.Span; checkExpression Set.empty declaration.Value; if not (compatible declaration.Type declaration.Value.Type) then report "FSL2004" $"LIR declaration '{declaration.Name}' type is inconsistent." declaration.Span)
            program.Entry |> Option.iter (checkExpression Set.empty)
            if diagnostics.Count = 0 then Ok program else Error(List.ofSeq diagnostics)

module LirOptimization =
    /// The initial pipeline deliberately performs no optimization. Keeping this
    /// pass explicit makes future transformations independently testable.
    let optimize (program: Lir.Program) = program
