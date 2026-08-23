namespace FScript.Compiler

open System
open System.IO
open System.Text
open System.Threading
open FScript.Language

type GoOptions = { ModuleName: string }
type GeneratedFile = { RelativePath: string; Content: string }

module GoBackend =
    let private currentImportedRecords = AsyncLocal<Map<string, Lir.ImportedRecord> option>()
    let private currentFunctions = AsyncLocal<Map<Lir.FunctionId, Lir.Function>>()
    let private tryImportedRecord name =
        currentImportedRecords.Value |> Option.bind (Map.tryFind name)
    let private indent (value: string) = value.Replace("\n", "\n\t")

    let private runtimeTemplate fileName =
        let assembly = typeof<GoOptions>.Assembly
        let suffix = $".Runtime.{fileName}"
        let resourceName = assembly.GetManifestResourceNames() |> Array.find (fun name -> name.EndsWith(suffix, StringComparison.Ordinal))
        use stream =
            match assembly.GetManifestResourceStream(resourceName) with
            | null -> failwith $"Embedded Go runtime template '{fileName}' was not found."
            | value -> value
        use reader = new StreamReader(stream)
        reader.ReadToEnd()

    let private importAlias (modulePath: string) =
        let hash = Security.Cryptography.SHA256.HashData(Encoding.UTF8.GetBytes(modulePath)) |> Convert.ToHexString
        "goimport" + hash.Substring(0, 8).ToLowerInvariant()

    let rec private goType = function
        | TUnit -> "struct{}"
        | TInt -> "int64"
        | TFloat -> "float64"
        | TBool -> "bool"
        | TString -> "string"
        | TTypeToken -> "string"
        | TVar _ -> "any"
        | TFun _ -> "fsruntime.Closure"
        | TTuple _ -> "fsruntime.Tuple"
        | TList _ -> "[]any"
        | TOption _ -> "fsruntime.Option"
        | TRecord _ -> "map[string]any"
        | TNamed "Environment" -> "map[string]any"
        | TNamed name when tryImportedRecord name |> Option.isSome -> "map[string]any"
        | TUnion _ | TNamed _ -> "fsruntime.Union"
        | TMap _ -> "fsruntime.OrderedMap"
        | value -> invalidArg "value" $"Unsupported Go representation for {Types.typeToString value}."

    let private identifier prefix id = $"{prefix}{Lir.symbolNumber id}"

    let private constant = function
        | Lir.Unit -> "struct{}{}"
        | Lir.Int value -> $"int64({value})"
        | Lir.Float value -> value.ToString("R", Globalization.CultureInfo.InvariantCulture)
        | Lir.Bool true -> "true"
        | Lir.Bool false -> "false"
        | Lir.String value -> System.Text.Json.JsonSerializer.Serialize(value)

    let private operation = function
        | Lir.IntAdd | Lir.FloatAdd -> Some "+"
        | Lir.IntSubtract | Lir.FloatSubtract -> Some "-"
        | Lir.IntMultiply | Lir.FloatMultiply -> Some "*"
        | Lir.IntDivide | Lir.FloatDivide -> Some "/"
        | Lir.IntRemainder -> Some "%"
        | Lir.StructuralEqual -> None
        | Lir.NumericLessThan -> Some "<"
        | Lir.NumericGreaterThan -> Some ">"
        | Lir.NumericLessThanOrEqual -> Some "<="
        | Lir.NumericGreaterThanOrEqual -> Some ">="
        | Lir.BooleanAnd -> Some "&&"
        | Lir.BooleanOr -> Some "||"

    let private cast valueType code =
        match valueType with
        | TUnit -> $"fsruntime.Unit({code})"
        | TInt -> $"fsruntime.Int({code})"
        | TFloat -> $"fsruntime.Float({code})"
        | TBool -> $"fsruntime.Bool({code})"
        | TString -> $"fsruntime.String({code})"
        | TList _ -> $"fsruntime.List({code})"
        | TTuple _ -> $"fsruntime.AsTuple({code})"
        | TRecord _ -> $"fsruntime.Record({code})"
        | TOption _ -> $"fsruntime.AsOption({code})"
        | TNamed "Environment" -> $"fsruntime.Record({code})"
        | TNamed name when tryImportedRecord name |> Option.isSome -> $"fsruntime.Record({code})"
        | TUnion _ | TNamed _ -> $"fsruntime.AsUnion({code})"
        | TMap _ -> $"fsruntime.AsMap({code})"
        | TFun _ -> $"fsruntime.AsClosure({code})"
        | _ -> code

    let rec private patternBindings (pattern: Lir.Pattern) =
        match pattern.Node with
        | Lir.Binding(id, _) -> [ id ]
        | Lir.ConsPattern(head, tail) -> patternBindings head @ patternBindings tail
        | Lir.TuplePattern items -> List.collect patternBindings items
        | Lir.RecordPattern fields -> fields |> List.collect (snd >> patternBindings)
        | Lir.MapPattern(clauses, tail) ->
            clauses |> List.collect (fun (key, value) -> patternBindings key @ patternBindings value)
            |> (@) (tail |> Option.map patternBindings |> Option.defaultValue [])
        | Lir.SomePattern item -> patternBindings item
        | Lir.UnionPattern(_, _, payload) -> payload |> Option.map patternBindings |> Option.defaultValue []
        | _ -> []

    let rec private patternBindingDetails (pattern: Lir.Pattern) =
        match pattern.Node with
        | Lir.Binding(id, _) -> [ id, pattern.Type ]
        | Lir.ConsPattern(head, tail) -> patternBindingDetails head @ patternBindingDetails tail
        | Lir.TuplePattern items -> List.collect patternBindingDetails items
        | Lir.RecordPattern fields -> fields |> List.collect (snd >> patternBindingDetails)
        | Lir.MapPattern(clauses, tail) ->
            clauses |> List.collect (fun (key, value) -> patternBindingDetails key @ patternBindingDetails value)
            |> (@) (tail |> Option.map patternBindingDetails |> Option.defaultValue [])
        | Lir.SomePattern item -> patternBindingDetails item
        | Lir.UnionPattern(_, _, payload) -> payload |> Option.map patternBindingDetails |> Option.defaultValue []
        | _ -> []

    let rec private emitPatternDescriptor (pattern: Lir.Pattern) =
        let quoted value = System.Text.Json.JsonSerializer.Serialize(value)
        match pattern.Node with
        | Lir.Wildcard -> "fsruntime.Pattern{Kind: \"wildcard\"}"
        | Lir.Binding(id, _) -> $"fsruntime.Pattern{{Kind: \"binding\", Symbol: {Lir.symbolNumber id}}}"
        | Lir.LiteralPattern value -> $"fsruntime.Pattern{{Kind: \"literal\", Literal: {constant value}}}"
        | Lir.EmptyList -> "fsruntime.Pattern{Kind: \"empty-list\"}"
        | Lir.NonePattern -> "fsruntime.Pattern{Kind: \"none\"}"
        | Lir.SomePattern item -> $"fsruntime.Pattern{{Kind: \"some\", Items: []fsruntime.Pattern{{{emitPatternDescriptor item}}}}}"
        | Lir.ConsPattern(head, tail) -> $"fsruntime.Pattern{{Kind: \"cons\", Items: []fsruntime.Pattern{{{emitPatternDescriptor head}, {emitPatternDescriptor tail}}}}}"
        | Lir.TuplePattern items ->
            let values = items |> List.map emitPatternDescriptor |> String.concat ", "
            $"fsruntime.Pattern{{Kind: \"tuple\", Items: []fsruntime.Pattern{{{values}}}}}"
        | Lir.RecordPattern fields ->
            let values = fields |> List.map (fun (name, item) -> $"fsruntime.FieldPattern{{Name: {quoted name}, Pattern: {emitPatternDescriptor item}}}") |> String.concat ", "
            $"fsruntime.Pattern{{Kind: \"record\", Fields: []fsruntime.FieldPattern{{{values}}}}}"
        | Lir.MapPattern(clauses, tail) ->
            let values = clauses |> List.map (fun (key, value) -> $"fsruntime.MapPatternClause{{Key: {emitPatternDescriptor key}, Value: {emitPatternDescriptor value}}}") |> String.concat ", "
            let tailValue = tail |> Option.map (fun item -> $"fsruntime.PatternPtr({emitPatternDescriptor item})") |> Option.defaultValue "nil"
            $"fsruntime.Pattern{{Kind: \"map\", Clauses: []fsruntime.MapPatternClause{{{values}}}, Tail: {tailValue}}}"
        | Lir.UnionPattern(typeName, caseName, payload) ->
            let payloadValue = payload |> Option.map (fun item -> $"[]fsruntime.Pattern{{{emitPatternDescriptor item}}}") |> Option.defaultValue "nil"
            $"fsruntime.Pattern{{Kind: \"union\", TypeName: {quoted typeName}, CaseName: {quoted caseName}, Items: {payloadValue}}}"
        | Lir.TypePattern patternType -> $"fsruntime.Pattern{{Kind: \"type\", TypeName: {quoted (Types.typeToString patternType)}}}"

    let rec private emitPattern valueCode (pattern: Lir.Pattern) =
        let combine parts =
            let tests, assignments = List.unzip parts
            let test = tests |> List.filter ((<>) "true") |> function [] -> "true" | values -> String.concat " && " values
            test, List.concat assignments
        match pattern.Node with
        | Lir.Wildcard -> "true", []
        | Lir.Binding(id, _) ->
            let name = identifier "match" id
            "true", [ $"{name} := {cast pattern.Type valueCode}; _ = {name}" ]
        | Lir.LiteralPattern value -> $"fsruntime.Equal({valueCode}, {constant value})", []
        | Lir.EmptyList -> $"len(fsruntime.List({valueCode})) == 0", []
        | Lir.NonePattern -> $"!fsruntime.IsSome({valueCode})", []
        | Lir.SomePattern item ->
            let test, assignments = emitPattern $"fsruntime.OptionValue({valueCode})" item
            $"fsruntime.IsSome({valueCode}) && ({test})", assignments
        | Lir.ConsPattern(head, tail) ->
            let headPart = emitPattern $"fsruntime.List({valueCode})[0]" head
            let tailPart = emitPattern $"fsruntime.Tail({valueCode})" tail
            let test, assignments = combine [ headPart; tailPart ]
            $"len(fsruntime.List({valueCode})) > 0 && ({test})", assignments
        | Lir.TuplePattern items ->
            items |> List.mapi (fun index item -> emitPattern $"fsruntime.AsTuple({valueCode})[{index}]" item)
            |> combine |> fun (test, assignments) -> $"len(fsruntime.AsTuple({valueCode})) == {items.Length} && ({test})", assignments
        | Lir.RecordPattern fields ->
            fields |> List.map (fun (name, item) ->
                let quoted = System.Text.Json.JsonSerializer.Serialize(name)
                let test, assignments = emitPattern $"fsruntime.Record({valueCode})[{quoted}]" item
                $"fsruntime.HasField({valueCode}, {quoted}) && ({test})", assignments)
            |> combine
        | Lir.MapPattern _ ->
            let descriptor = emitPatternDescriptor pattern
            let assignments =
                patternBindingDetails pattern |> List.distinctBy fst |> List.map (fun (id, bindingType) ->
                    let name = identifier "match" id
                    let bindingValue = $"fsruntime.PatternBinding({valueCode}, {descriptor}, {Lir.symbolNumber id})"
                    $"{name} := {cast bindingType bindingValue}; _ = {name}")
            $"fsruntime.PatternMatches({valueCode}, {descriptor})", assignments
        | Lir.UnionPattern(typeName, caseName, payload) ->
            let quotedType = System.Text.Json.JsonSerializer.Serialize(typeName)
            let quotedCase = System.Text.Json.JsonSerializer.Serialize(caseName)
            match payload with
            | None -> $"fsruntime.IsUnion({valueCode}, {quotedType}, {quotedCase}) && !fsruntime.UnionHasValue({valueCode})", []
            | Some item ->
                let test, assignments = emitPattern $"fsruntime.UnionValue({valueCode})" item
                $"fsruntime.IsUnion({valueCode}, {quotedType}, {quotedCase}) && fsruntime.UnionHasValue({valueCode}) && ({test})", assignments
        | Lir.TypePattern patternType -> $"fsruntime.TypeMatches({valueCode}, {System.Text.Json.JsonSerializer.Serialize(Types.typeToString patternType)})", []

    let private emitIntrinsic name intrinsicType =
        let quoted = System.Text.Json.JsonSerializer.Serialize(name)
        let rec arity = function TFun(_, result) -> 1 + arity result | _ -> 0
        let count = arity intrinsicType
        if count = 0 then cast intrinsicType $"fsruntime.DispatchIntrinsic({quoted}, nil)"
        else $"fsruntime.IntrinsicClosure({quoted}, {count})"

    let private emitHostExternal name externalType =
        let quoted = System.Text.Json.JsonSerializer.Serialize(name)
        let rec arity = function TFun(_, result) -> 1 + arity result | _ -> 0
        $"fsruntime.HostClosure({quoted}, {arity externalType})"

    let private emitTargetImport modulePath memberName importType =
        let alias = importAlias modulePath
        let importedArgument raw _ (record: Lir.ImportedRecord) =
            let fields =
                record.Fields
                |> List.map (fun (fieldName, fieldType) ->
                    let fieldAccess = $"fsruntime.Record({raw})[{System.Text.Json.JsonSerializer.Serialize(fieldName)}]"
                    let fieldValue = cast fieldType fieldAccess
                    $"{fieldName}: {fieldValue}")
                |> String.concat ", "
            $"{importAlias record.ModulePath}.{record.MemberName}{{{fields}}}"
        let importedResult code (record: Lir.ImportedRecord) =
            let fields =
                record.Fields
                |> List.map (fun (fieldName, _) -> $"{System.Text.Json.JsonSerializer.Serialize(fieldName)}: importedResult.{fieldName}")
                |> String.concat ", "
            $"func() map[string]any {{\n\timportedResult := {code}\n\treturn map[string]any{{{fields}}}\n}}()"
        let rec build index arguments = function
            | TFun(argumentType, resultType) ->
                let raw = $"targetArg{index}"
                let argument = $"targetValue{index}"
                let body = build (index + 1) (arguments @ [ argument ]) resultType
                let argumentValue =
                    match argumentType with
                    | TNamed name -> tryImportedRecord name |> Option.map (importedArgument raw name) |> Option.defaultValue (cast argumentType raw)
                    | _ -> cast argumentType raw
                $"fsruntime.Closure{{Call: func({raw} any) any {{\n\t{argument} := {argumentValue}\n\treturn {indent body}\n}}}}"
            | resultType ->
                let joined = String.concat ", " arguments
                let call = $"{alias}.{memberName}({joined})"
                match resultType with
                | TUnit -> $"func() any {{ {alias}.{memberName}({joined}); return struct{{}}{{}} }}()"
                | TNamed name -> tryImportedRecord name |> Option.map (importedResult call) |> Option.defaultValue call
                | _ -> call
        build 0 [] importType

    let rec private emitExpression environment (expression: Lir.Expression) =
        match expression.Node with
        | Lir.Constant value -> Ok(constant value)
        | Lir.Load id ->
            environment |> Map.tryFind id
            |> Option.map Ok
            |> Option.defaultValue (Error(sprintf "Unknown symbol %%%d" (Lir.symbolNumber id)))
        | Lir.MakeClosure(functionId, _) ->
            match currentFunctions.Value |> Map.tryFind functionId with
            | None -> Error $"Unknown lifted function {Lir.functionNumber functionId}"
            | Some fn ->
                let name = identifier "arg" fn.Parameter
                let parameterValue = cast fn.ParameterType "rawValue"
                emitExpression (environment |> Map.add fn.Parameter name) fn.Body
                |> Result.map (fun bodyCode ->
                    let indentedBody = bodyCode.Replace("\n", "\n\t")
                    $"fsruntime.Closure{{Call: func(rawValue any) any {{\n\t{name} := {parameterValue}\n\t_ = {name}\n\treturn {indentedBody}\n}}}}")
        | Lir.CallClosure(fn, argument) ->
            match emitExpression environment fn, emitExpression environment argument with
            | Ok fnCode, Ok argumentCode -> Ok(cast expression.Type $"fsruntime.Call({fnCode}, {argumentCode})")
            | Error error, _ | _, Error error -> Error error
        | Lir.Branch(condition, whenTrue, whenFalse) ->
            match emitExpression environment condition, emitExpression environment whenTrue, emitExpression environment whenFalse with
            | Ok c, Ok t, Ok f -> Ok $"func() {goType expression.Type} {{\n\tif {c} {{\n\t\treturn {indent (indent t)}\n\t}}\n\treturn {indent f}\n}}()"
            | Error error, _, _ | _, Error error, _ | _, _, Error error -> Error error
        | Lir.Operation(op, [ left; right ]) ->
            match emitExpression environment left, emitExpression environment right with
            | Ok l, Ok r when op = Lir.StructuralEqual -> Ok $"fsruntime.Equal({l}, {r})"
            | Ok l, Ok r ->
                match operation op with
                | Some token ->
                    let l, r =
                        match op with
                        | Lir.IntAdd | Lir.IntSubtract | Lir.IntMultiply | Lir.IntDivide | Lir.IntRemainder ->
                            $"fsruntime.Int({l})", $"fsruntime.Int({r})"
                        | _ -> l, r
                    match op with
                    | Lir.NumericLessThan | Lir.NumericGreaterThan | Lir.NumericLessThanOrEqual | Lir.NumericGreaterThanOrEqual | Lir.BooleanAnd | Lir.BooleanOr -> Ok $"{l} {token} {r}"
                    | _ -> Ok $"({l} {token} {r})"
                | None -> Error $"Unsupported Go operation {op}"
            | Error error, _ | _, Error error -> Error error
        | Lir.Tuple items ->
            items |> List.map (emitExpression environment) |> collect
            |> Result.map (fun values ->
                let joined = String.concat ", " values
                $"fsruntime.Tuple{{{joined}}}")
        | Lir.List items ->
            items |> List.map (emitExpression environment) |> collect
            |> Result.map (fun values ->
                let joined = String.concat ", " values
                $"[]any{{{joined}}}")
        | Lir.Record fields ->
            fields |> List.map (fun (name, value) -> emitExpression environment value |> Result.map (fun code -> $"{System.Text.Json.JsonSerializer.Serialize(name)}: {code}"))
            |> collect |> Result.map (fun values ->
                let joined = String.concat ", " values
                $"map[string]any{{{joined}}}")
        | Lir.FieldGet(target, name) ->
            emitExpression environment target |> Result.map (fun code -> $"({code})[{System.Text.Json.JsonSerializer.Serialize(name)}]")
        | Lir.SomeValue item -> emitExpression environment item |> Result.map (fun code -> $"fsruntime.Some({code})")
        | Lir.NoneValue -> Ok "fsruntime.None()"
        | Lir.Cons(head, tail) ->
            match emitExpression environment head, emitExpression environment tail with
            | Ok h, Ok t -> Ok $"fsruntime.Cons({h}, {t})"
            | Error error, _ | _, Error error -> Error error
        | Lir.Append(left, right) ->
            match emitExpression environment left, emitExpression environment right with
            | Ok l, Ok r -> Ok $"fsruntime.Append({l}, {r})"
            | Error error, _ | _, Error error -> Error error
        | Lir.Let(id, _, recursive, binding, body) ->
            let name = identifier "local" id
            let scoped = environment |> Map.add id name
            match emitExpression scoped binding, emitExpression scoped body with
            | Ok bindingCode, Ok bodyCode ->
                let declaration =
                    if recursive then $"var {name} {goType binding.Type}\n\t{name} = {indent bindingCode}"
                    else $"{name} := {indent bindingCode}"
                Ok $"func() {goType expression.Type} {{\n\t{declaration}\n\treturn {indent bodyCode}\n}}()"
            | Error error, _ | _, Error error -> Error error
        | Lir.LetRecursiveGroup(bindings, body) ->
            let scoped = bindings |> List.fold (fun env (id, _, _, _) -> Map.add id (identifier "localRec" id) env) environment
            let loweredBindings =
                bindings |> List.map (fun (id, _, bindingType, value) ->
                    emitExpression scoped value |> Result.map (fun code -> identifier "localRec" id, bindingType, code))
                |> collect
            match loweredBindings, emitExpression scoped body with
            | Ok values, Ok bodyCode ->
                let declarations = values |> List.map (fun (name, bindingType, _) -> $"var {name} {goType bindingType}") |> String.concat "\n\t"
                let assignments = values |> List.map (fun (name, _, code) -> $"{name} = {indent code}") |> String.concat "\n\t"
                Ok $"func() {goType expression.Type} {{\n\t{declarations}\n\t{assignments}\n\treturn {indent bodyCode}\n}}()"
            | Error error, _ | _, Error error -> Error error
        | Lir.Fail value ->
            emitExpression environment value
            |> Result.map (fun code -> $"func() {goType expression.Type} {{\n\tfsruntime.Fail({indent code})\n\tvar zero {goType expression.Type}\n\treturn zero\n}}()")
        | Lir.DecisionTree(target, cases) ->
            emitExpression environment target |> Result.bind (fun targetCode ->
                let mutable failure = None
                let clauses = StringBuilder()
                for case in cases do
                    let bindings = patternBindings case.Pattern
                    let caseEnvironment = bindings |> List.fold (fun env id -> Map.add id (identifier "match" id) env) environment
                    match case.Guard |> Option.map (emitExpression caseEnvironment) |> Option.defaultValue (Ok "true"), emitExpression caseEnvironment case.Body with
                    | Ok guardCode, Ok bodyCode ->
                        let test, assignments = emitPattern "matchValue" case.Pattern
                        clauses.Append($"\tif {test} {{\n") |> ignore
                        assignments |> List.iter (fun assignment -> clauses.Append("\t\t").Append(assignment.Replace("; ", "\n\t\t")).Append("\n") |> ignore)
                        clauses.Append($"\t\tif {guardCode} {{\n\t\t\treturn {indent (indent (indent bodyCode))}\n\t\t}}\n\t}}\n") |> ignore
                    | Error error, _ | _, Error error -> failure <- Some error
                match failure with
                | Some error -> Error error
                | None -> Ok $"func() {goType expression.Type} {{\n\tmatchValue := {indent targetCode}\n{clauses}\tfsruntime.Fail(\"Match failure.\")\n\tvar zero {goType expression.Type}\n\treturn zero\n}}()")
        | Lir.Range(startValue, endValue) ->
            match emitExpression environment startValue, emitExpression environment endValue with
            | Ok first, Ok last -> Ok $"fsruntime.Range({first}, {last})"
            | Error error, _ | _, Error error -> Error error
        | Lir.RecordUpdate(target, fields) ->
            match emitExpression environment target,
                  (fields |> List.map (fun (name, item) -> emitExpression environment item |> Result.map (fun code -> System.Text.Json.JsonSerializer.Serialize(name) + ": " + code)) |> collect) with
            | Ok targetCode, Ok values ->
                let joined = String.concat ", " values
                Ok $"fsruntime.UpdateRecord({targetCode}, map[string]any{{{joined}}})"
            | Error error, _ | _, Error error -> Error error
        | Lir.RecordConvert(target, _) -> emitExpression environment target
        | Lir.IndexGet(target, index) ->
            match emitExpression environment target, emitExpression environment index with
            | Ok targetCode, Ok indexCode -> Ok $"fsruntime.Index({targetCode}, {indexCode})"
            | Error error, _ | _, Error error -> Error error
        | Lir.Interpolate parts ->
            parts |> List.map (function
                | Lir.TextPart text -> Ok(System.Text.Json.JsonSerializer.Serialize(text))
                | Lir.ValuePart item -> emitExpression environment item)
            |> collect |> Result.map (fun values ->
                let joined = String.concat ", " values
                $"fsruntime.Interpolate([]any{{{joined}}})")
        | Lir.UnionCase(typeName, caseName, payload) ->
            let quotedType = System.Text.Json.JsonSerializer.Serialize(typeName)
            let quotedCase = System.Text.Json.JsonSerializer.Serialize(caseName)
            match payload with
            | None -> Ok $"fsruntime.MakeUnion({quotedType}, {quotedCase})"
            | Some item -> emitExpression environment item |> Result.map (fun code -> $"fsruntime.MakeUnionValue({quotedType}, {quotedCase}, {code})")
        | Lir.Map entries ->
            entries |> List.map (function
                | Lir.KeyValue(key, value) ->
                    match emitExpression environment key, emitExpression environment value with
                    | Ok keyCode, Ok valueCode -> Ok $"fsruntime.Pair({keyCode}, {valueCode})"
                    | Error error, _ | _, Error error -> Error error
                | Lir.Spread value -> emitExpression environment value |> Result.map (fun code -> $"fsruntime.SpreadMap({code})"))
            |> collect |> Result.map (fun parts ->
                let joined = String.concat ", " parts
                $"fsruntime.BuildMap([]fsruntime.MapPart{{{joined}}})")
        | Lir.Intrinsic name -> Ok(emitIntrinsic name expression.Type)
        | Lir.HostExternal(name, _) -> Ok(emitHostExternal name expression.Type)
        | Lir.RuntimeContext -> Ok "fsruntime.Environment()"
        | Lir.TargetImport(modulePath, memberName) -> Ok(emitTargetImport modulePath memberName expression.Type)
        | Lir.ForEach(id, _, itemType, source, body) ->
            let name = identifier "forValue" id
            let scoped = environment |> Map.add id name
            match emitExpression environment source, emitExpression scoped body with
            | Ok sourceCode, Ok bodyCode ->
                let itemCode = cast itemType "rawValue"
                Ok $"func() struct{{}} {{\n\tfor _, rawValue := range fsruntime.List({sourceCode}) {{\n\t\t{name} := {itemCode}\n\t\t_ = {indent (indent bodyCode)}\n\t}}\n\treturn struct{{}}{{}}\n}}()"
            | Error error, _ | _, Error error -> Error error
        | Lir.TypeToken tokenType -> Ok(System.Text.Json.JsonSerializer.Serialize(Types.typeToString tokenType))
        | Lir.Operation _ -> Error "Invalid operation arity"

    and private collect<'a> (results: Result<'a, string> list) : Result<'a list, string> =
        results |> List.fold (fun state result ->
            match state, result with Ok values, Ok value -> Ok(values @ [ value ]) | Error error, _ | _, Error error -> Error error) (Ok [])

    let private closureRuntime = """package fsruntime

type Closure struct { Call func(any) any }

func AsClosure(value any) Closure { return value.(Closure) }
func Call(function any, argument any) any { return AsClosure(function).Call(argument) }

func DispatchIntrinsic(name string, args []any) any {
	switch name {
	case "List.map": callback, values := AsClosure(args[0]), List(args[1]); result := make([]any, len(values)); for index, value := range values { result[index] = callback.Call(value) }; return result
	case "List.iter": callback := AsClosure(args[0]); for _, value := range List(args[1]) { callback.Call(value) }; return struct{}{}
	case "List.choose": callback, result := AsClosure(args[0]), []any{}; for _, value := range List(args[1]) { chosen := AsOption(callback.Call(value)); if chosen.HasValue { result = append(result, chosen.Value) } }; return result
	case "List.collect": callback, result := AsClosure(args[0]), []any{}; for _, value := range List(args[1]) { result = append(result, List(callback.Call(value))...) }; return result
	case "List.exists": callback := AsClosure(args[0]); for _, value := range List(args[1]) { if Bool(callback.Call(value)) { return true } }; return false
	case "List.distinct": result := []any{}; for _, value := range List(args[0]) { found := false; for _, existing := range result { if Equal(existing, value) { found = true; break } }; if !found { result = append(result, value) } }; return result
	case "List.fold": callback, state := AsClosure(args[0]), args[1]; for _, value := range List(args[2]) { state = Call(callback.Call(state), value) }; return state
	case "List.filter": callback, result := AsClosure(args[0]), []any{}; for _, value := range List(args[1]) { if Bool(callback.Call(value)) { result = append(result, value) } }; return result
	case "List.tryFind": callback := AsClosure(args[0]); for _, value := range List(args[1]) { if Bool(callback.Call(value)) { return Some(value) } }; return None()
	case "List.tryGet": callback := AsClosure(args[0]); for index, value := range List(args[1]) { if Bool(callback.Call(value)) { return Some(int64(index)) } }; return None()
	case "List.tryItem": index, values := Int(args[0]), List(args[1]); if index < 0 || index >= int64(len(values)) { return None() }; return Some(values[index])
	case "Option.defaultWith": option := AsOption(args[1]); if option.HasValue { return option.Value }; return Call(args[0], struct{}{})
	case "Option.map": option := AsOption(args[1]); if !option.HasValue { return None() }; return Some(Call(args[0], option.Value))
	case "Map.add": values := append([]MapEntry{}, AsMap(args[2]).Entries...); return OrderedMap{Entries: mapSet(values, args[0], args[1], true)}
	case "Map.ofList": values := []MapEntry{}; for _, pair := range List(args[0]) { tuple := List(pair); values = mapSet(values, tuple[0], tuple[1], true) }; return OrderedMap{Entries: values}
	case "Map.fold": callback, state := AsClosure(args[0]), args[1]; for _, entry := range AsMap(args[2]).Entries { state = Call(Call(callback.Call(state), entry.Key), entry.Value) }; return state
	case "Map.filter": callback, result := AsClosure(args[0]), []MapEntry{}; for _, entry := range AsMap(args[1]).Entries { if Bool(Call(callback.Call(entry.Key), entry.Value)) { result = append(result, entry) } }; return OrderedMap{Entries: result}
	case "Map.choose": callback, result := AsClosure(args[0]), []MapEntry{}; for _, entry := range AsMap(args[1]).Entries { chosen := AsOption(Call(callback.Call(entry.Key), entry.Value)); if chosen.HasValue { result = append(result, MapEntry{Key: entry.Key, Value: chosen.Value}) } }; return OrderedMap{Entries: result}
	case "Map.map": callback, result := AsClosure(args[0]), []MapEntry{}; for _, entry := range AsMap(args[1]).Entries { result = append(result, MapEntry{Key: entry.Key, Value: callback.Call(entry.Value)}) }; return OrderedMap{Entries: result}
	case "Map.iter": callback := AsClosure(args[0]); for _, entry := range AsMap(args[1]).Entries { Call(callback.Call(entry.Key), entry.Value) }; return struct{}{}
	case "Map.remove": result := []MapEntry{}; for _, entry := range AsMap(args[1]).Entries { if !Equal(entry.Key, args[0]) { result = append(result, entry) } }; return OrderedMap{Entries: result}
	default: return Intrinsic(name, args)
	}
}
"""

    let private patternRuntime = """package fsruntime

import "strings"

type Pattern struct { Kind string; Symbol int; Literal any; Items []Pattern; Fields []FieldPattern; Clauses []MapPatternClause; Tail *Pattern; TypeName string; CaseName string }
type FieldPattern struct { Name string; Pattern Pattern }
type MapPatternClause struct { Key Pattern; Value Pattern }

func PatternPtr(value Pattern) *Pattern { return &value }

func mergePatternBindings(left map[int]any, right map[int]any) (map[int]any, bool) {
	result := map[int]any{}; for key, value := range left { result[key] = value }; for key, value := range right { if existing, found := result[key]; found && !Equal(existing, value) { return nil, false }; result[key] = value }; return result, true
}

func matchRuntimePattern(value any, pattern Pattern) (map[int]any, bool) {
	switch pattern.Kind {
	case "wildcard": return map[int]any{}, true
	case "binding": return map[int]any{pattern.Symbol: value}, true
	case "literal": return map[int]any{}, Equal(value, pattern.Literal)
	case "empty-list": values, ok := value.([]any); return map[int]any{}, ok && len(values) == 0
	case "none": option, ok := value.(Option); return map[int]any{}, ok && !option.HasValue
	case "some": option, ok := value.(Option); if !ok || !option.HasValue { return nil, false }; return matchRuntimePattern(option.Value, pattern.Items[0])
	case "cons": values, ok := value.([]any); if !ok || len(values) == 0 { return nil, false }; left, matched := matchRuntimePattern(values[0], pattern.Items[0]); if !matched { return nil, false }; right, matched := matchRuntimePattern(values[1:], pattern.Items[1]); if !matched { return nil, false }; return mergePatternBindings(left, right)
	case "tuple": values, ok := value.([]any); if !ok || len(values) != len(pattern.Items) { return nil, false }; bindings := map[int]any{}; for index, item := range pattern.Items { next, matched := matchRuntimePattern(values[index], item); if !matched { return nil, false }; bindings, matched = mergePatternBindings(bindings, next); if !matched { return nil, false } }; return bindings, true
	case "record": values, ok := value.(map[string]any); if !ok { return nil, false }; bindings := map[int]any{}; for _, field := range pattern.Fields { item, found := values[field.Name]; if !found { return nil, false }; next, matched := matchRuntimePattern(item, field.Pattern); if !matched { return nil, false }; bindings, matched = mergePatternBindings(bindings, next); if !matched { return nil, false } }; return bindings, true
	case "union": item, ok := value.(Union); if !ok || item.TypeName != pattern.TypeName || item.CaseName != pattern.CaseName || (len(pattern.Items) == 0) != !item.HasValue { return nil, false }; if len(pattern.Items) == 0 { return map[int]any{}, true }; return matchRuntimePattern(item.Value, pattern.Items[0])
	case "type": return map[int]any{}, TypeMatches(value, pattern.TypeName)
	case "map":
		item, ok := value.(OrderedMap); if !ok { return nil, false }; remaining := append([]MapEntry{}, item.Entries...); bindings := map[int]any{}
		for _, clause := range pattern.Clauses { found := false; for index, entry := range remaining { keyBindings, keyMatched := matchRuntimePattern(entry.Key, clause.Key); if !keyMatched { continue }; withKey, merged := mergePatternBindings(bindings, keyBindings); if !merged { continue }; valueBindings, valueMatched := matchRuntimePattern(entry.Value, clause.Value); if !valueMatched { continue }; withValue, merged := mergePatternBindings(withKey, valueBindings); if !merged { continue }; bindings = withValue; remaining = append(remaining[:index], remaining[index+1:]...); found = true; break }; if !found { return nil, false } }
		if pattern.Tail != nil { tailBindings, matched := matchRuntimePattern(OrderedMap{Entries: remaining}, *pattern.Tail); if !matched { return nil, false }; return mergePatternBindings(bindings, tailBindings) }; if len(pattern.Clauses) > 0 || len(remaining) == 0 { return bindings, true }; return nil, false
	default: return nil, false
	}
}

func PatternMatches(value any, pattern Pattern) bool { _, matched := matchRuntimePattern(value, pattern); return matched }
func PatternBinding(value any, pattern Pattern, symbol int) any { bindings, matched := matchRuntimePattern(value, pattern); if !matched { panic("Pattern did not match") }; return bindings[symbol] }
func TypeMatches(value any, typeName string) bool { switch typeName { case "unit": _, ok := value.(struct{}); return ok; case "int": _, ok := value.(int64); return ok; case "float": _, ok := value.(float64); return ok; case "bool": _, ok := value.(bool); return ok; case "string": _, ok := value.(string); return ok }; if strings.HasSuffix(typeName, " list") { _, ok := value.([]any); return ok }; if strings.HasSuffix(typeName, " option") { _, ok := value.(Option); return ok }; if strings.HasPrefix(typeName, "{") { _, ok := value.(map[string]any); return ok }; if item, ok := value.(Union); ok { return item.TypeName == typeName }; _, record := value.(map[string]any); return record }
"""

    let private environmentRuntime = """package fsruntime

import (
	"os"
	"path/filepath"
)

func Environment() map[string]any {
	arguments := make([]any, len(os.Args)-1)
	for index, argument := range os.Args[1:] { arguments[index] = argument }
	return map[string]any{"ScriptName": Some(filepath.Base(os.Args[0])), "Arguments": arguments}
}
"""

    let private generateCore options (program: Lir.Program) : Result<GeneratedFile list, CompilerDiagnostic list> =
        try
            currentImportedRecords.Value <- Some(program.ImportedRecords |> List.map (fun record -> record.Name, record) |> Map.ofList)
            currentFunctions.Value <- program.Functions |> List.map (fun fn -> fn.Id, fn) |> Map.ofList
            let targetImports =
                program.Declarations
                |> List.choose (fun declaration -> match declaration.Value.Node with Lir.TargetImport(modulePath, memberName) -> Some(modulePath, memberName, declaration.Type, declaration.Span) | _ -> None)
                |> List.sortBy (fun (modulePath, memberName, _, _) -> modulePath, memberName)
            let rec directlyRepresentable = function
                | TUnit | TInt | TFloat | TBool | TString -> true
                | TFun(argument, result) -> directlyRepresentable argument && directlyRepresentable result
                | TNamed name ->
                    tryImportedRecord name
                    |> Option.exists (fun record -> not record.IsUnion && record.Fields |> List.forall (snd >> directlyRepresentable))
                | _ -> false
            let unsupportedImport = targetImports |> List.tryFind (fun (_, _, importType, _) -> not (directlyRepresentable importType))
            let names =
                program.Declarations
                |> List.fold (fun env declaration -> env |> Map.add declaration.Id (identifier "value" declaration.Id)) Map.empty
            let declarations = StringBuilder()
            let initializers = StringBuilder()
            let mutable error = None
            for declaration in program.Declarations do
                declarations.AppendLine($"var {names.[declaration.Id]} {goType declaration.Type}") |> ignore
                match emitExpression names declaration.Value with
                | Ok code ->
                    let indentedCode = code.Replace("\n", "\n\t")
                    initializers.AppendLine($"\t{names.[declaration.Id]} = {indentedCode}") |> ignore
                | Error message -> error <- Some(message, declaration.Span)
            let entry =
                match program.Entry with
                | None -> Ok "struct{}{}"
                | Some value -> emitExpression names value
            match unsupportedImport, error, entry with
            | Some(modulePath, memberName, _, span), _, _ ->
                Error [ { Code = "FSB4003"; Stage = GoGeneration; Message = $"Go import '{modulePath}.{memberName}' uses a type that is not directly representable."; Span = Some span } ]
            | None, Some(message, span), _ ->
                Error [ { Code = "FSB4001"; Stage = GoGeneration; Message = message; Span = Some span } ]
            | None, None, Error message ->
                Error [ { Code = "FSB4001"; Stage = GoGeneration; Message = message; Span = None } ]
            | None, None, Ok entryCode ->
                let moduleName = if String.IsNullOrWhiteSpace options.ModuleName then "fscript.generated/program" else options.ModuleName
                let importLines =
                    ($"\t\"{moduleName}/internal/fsruntime\"") ::
                    ([ yield! targetImports |> List.map (fun (modulePath, _, _, _) -> modulePath)
                       yield! program.ImportedRecords |> List.map _.ModulePath ] |> List.distinct |> List.sort |> List.map (fun path -> $"\t{importAlias path} {System.Text.Json.JsonSerializer.Serialize(path)}"))
                    |> String.concat "\n"
                let programSource = $"package fscriptprogram\n\nimport (\n{importLines}\n)\n\n{declarations}\nfunc init() {{\n{initializers}}}\n\nfunc Run() any {{ return {entryCode} }}\n"
                let printExpression =
                    match program.Entry with
                    | Some { Type = TUnit } -> "_ = fsruntime.Format(fscriptprogram.Run()); fmt.Println(\"()\")"
                    | _ -> "fmt.Println(fsruntime.Format(fscriptprogram.Run()))"
                Ok
                    [ { RelativePath = "go.mod"; Content = $"module {moduleName}\n\ngo 1.25\n" }
                      { RelativePath = "main.go"; Content = $"package main\n\nimport (\n\t\"fmt\"\n\t\"{moduleName}/internal/fscriptprogram\"\n\t\"{moduleName}/internal/fsruntime\"\n)\n\nfunc main() {{ {printExpression} }}\n" }
                      { RelativePath = "internal/fscriptprogram/program.go"; Content = programSource }
                      { RelativePath = "internal/fsruntime/closures.go"; Content = runtimeTemplate "closures.go" }
                      { RelativePath = "internal/fsruntime/patterns.go"; Content = runtimeTemplate "patterns.go" }
                      { RelativePath = "internal/fsruntime/environment.go"; Content = runtimeTemplate "environment.go" }
                      { RelativePath = "internal/fsruntime/runtime.go"; Content = runtimeTemplate "runtime.go" } ]
        with ex ->
            Error [ { Code = "FSB4002"; Stage = GoGeneration; Message = ex.Message; Span = None } ]

    let generate options (program: Lir.Program) : Result<GeneratedFile list, CompilerDiagnostic list> =
        currentImportedRecords.Value <- Some(program.ImportedRecords |> List.map (fun record -> record.Name, record) |> Map.ofList)
        currentFunctions.Value <- program.Functions |> List.map (fun fn -> fn.Id, fn) |> Map.ofList
        let rec containsTask = function
            | TTask _ -> true
            | TFun(argument, result) | TMap(argument, result) -> containsTask argument || containsTask result
            | TTuple items -> List.exists containsTask items
            | TList item | TOption item -> containsTask item
            | TRecord fields -> fields |> Map.exists (fun _ -> containsTask)
            | TUnion(_, cases) -> cases |> Map.exists (fun _ -> Option.exists containsTask)
            | _ -> false
        let rec references (expression: Lir.Expression) =
            let many values = values |> List.map references |> Set.unionMany
            match expression.Node with
            | Lir.Load id -> Set.singleton id
            | Lir.MakeClosure(_, captures) -> Set.ofList captures
            | Lir.Fail body | Lir.FieldGet(body, _) | Lir.SomeValue body -> references body
            | Lir.CallClosure(left, right) | Lir.Append(left, right) | Lir.Cons(left, right) | Lir.Range(left, right) | Lir.IndexGet(left, right) -> Set.union (references left) (references right)
            | Lir.Branch(condition, whenTrue, whenFalse) -> many [ condition; whenTrue; whenFalse ]
            | Lir.Operation(_, values) | Lir.Tuple values | Lir.List values -> many values
            | Lir.Record fields -> fields |> List.map (snd >> references) |> Set.unionMany
            | Lir.Let(_, _, _, binding, body) -> Set.union (references binding) (references body)
            | Lir.LetRecursiveGroup(bindings, body) -> references body :: (bindings |> List.map (fun (_, _, _, value) -> references value)) |> Set.unionMany
            | Lir.DecisionTree(target, cases) -> references target :: (cases |> List.collect (fun case -> [ yield! case.Guard |> Option.map references |> Option.toList; references case.Body ])) |> Set.unionMany
            | Lir.RecordUpdate(target, fields) -> references target :: (fields |> List.map (snd >> references)) |> Set.unionMany
            | Lir.RecordConvert(target, _) -> references target
            | Lir.Interpolate parts -> parts |> List.choose (function Lir.TextPart _ -> None | Lir.ValuePart value -> Some(references value)) |> Set.unionMany
            | Lir.UnionCase(_, _, payload) -> payload |> Option.map references |> Option.defaultValue Set.empty
            | Lir.Map entries -> entries |> List.map (function Lir.KeyValue(key, value) -> Set.union (references key) (references value) | Lir.Spread value -> references value) |> Set.unionMany
            | Lir.ForEach(_, _, _, source, body) -> Set.union (references source) (references body)
            | _ -> Set.empty
        let declarations = program.Declarations |> List.map (fun declaration -> declaration.Id, declaration) |> Map.ofList
        let rec closeReachable visited pending =
            match pending with
            | [] -> visited
            | id :: rest when Set.contains id visited -> closeReachable visited rest
            | id :: rest ->
                let dependencies = declarations |> Map.tryFind id |> Option.map (_.Value >> references >> Set.toList) |> Option.defaultValue []
                closeReachable (Set.add id visited) (dependencies @ rest)
        let reachable = program.Entry |> Option.map (references >> Set.toList >> closeReachable Set.empty) |> Option.defaultValue Set.empty
        let taskSpan =
            program.Declarations
            |> List.tryPick (fun declaration -> if Set.contains declaration.Id reachable && containsTask declaration.Type then Some declaration.Span else None)
            |> Option.orElseWith (fun () -> program.Entry |> Option.bind (fun entry -> if containsTask entry.Type then Some entry.Span else None))
        let invalidImportedRecord =
            program.ImportedRecords
            |> List.tryFind (fun record ->
                record.IsUnion
                || record.Fields.IsEmpty
                || (record.Fields |> List.exists (fun (name, fieldType) ->
                    let unsupportedType = match fieldType with TUnit | TInt | TFloat | TBool | TString -> false | _ -> true
                    System.String.IsNullOrEmpty(name) || not (System.Char.IsUpper(name.[0])) || containsTask fieldType || unsupportedType)))
        match invalidImportedRecord, taskSpan with
        | Some record, _ -> Error [ { Code = "FSB4005"; Stage = GoGeneration; Message = $"Imported Go record '{record.Name}' must have exported, exactly named primitive fields and cannot be a union."; Span = Some record.Span } ]
        | None, Some span -> Error [ { Code = "FSB4004"; Stage = GoGeneration; Message = "Task values are not supported by the Go backend."; Span = Some span } ]
        | None, None -> generateCore options program
