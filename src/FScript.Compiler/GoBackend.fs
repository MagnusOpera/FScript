namespace FScript.Compiler

open System
open System.Text
open FScript.Language

type GoOptions = { ModuleName: string }
type GeneratedFile = { RelativePath: string; Content: string }

module GoBackend =
    let rec private goType = function
        | TUnit -> "struct{}"
        | TInt -> "int64"
        | TFloat -> "float64"
        | TBool -> "bool"
        | TString -> "string"
        | TVar _ -> "any"
        | TFun(argument, result) -> $"func({goType argument}) {goType result}"
        | TTuple _ -> "[]any"
        | TList _ -> "[]any"
        | TOption _ -> "fsruntime.Option"
        | TRecord _ -> "map[string]any"
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
        | TList _ | TTuple _ -> $"fsruntime.List({code})"
        | TRecord _ -> $"fsruntime.Record({code})"
        | TOption _ -> $"fsruntime.AsOption({code})"
        | TUnion _ | TNamed _ -> $"fsruntime.AsUnion({code})"
        | TMap _ -> $"fsruntime.AsMap({code})"
        | _ -> code

    let rec private patternBindings (pattern: Lir.Pattern) =
        match pattern.Node with
        | Lir.Binding(id, _) -> [ id ]
        | Lir.ConsPattern(head, tail) -> patternBindings head @ patternBindings tail
        | Lir.TuplePattern items -> List.collect patternBindings items
        | Lir.RecordPattern fields -> fields |> List.collect (snd >> patternBindings)
        | Lir.SomePattern item -> patternBindings item
        | Lir.UnionPattern(_, _, payload) -> payload |> Option.map patternBindings |> Option.defaultValue []
        | _ -> []

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
            items |> List.mapi (fun index item -> emitPattern $"fsruntime.List({valueCode})[{index}]" item)
            |> combine |> fun (test, assignments) -> $"len(fsruntime.List({valueCode})) == {items.Length} && ({test})", assignments
        | Lir.RecordPattern fields ->
            fields |> List.map (fun (name, item) ->
                let quoted = System.Text.Json.JsonSerializer.Serialize(name)
                let test, assignments = emitPattern $"fsruntime.Record({valueCode})[{quoted}]" item
                $"fsruntime.HasField({valueCode}, {quoted}) && ({test})", assignments)
            |> combine
        | Lir.UnionPattern(typeName, caseName, payload) ->
            let quotedType = System.Text.Json.JsonSerializer.Serialize(typeName)
            let quotedCase = System.Text.Json.JsonSerializer.Serialize(caseName)
            match payload with
            | None -> $"fsruntime.IsUnion({valueCode}, {quotedType}, {quotedCase}) && !fsruntime.UnionHasValue({valueCode})", []
            | Some item ->
                let test, assignments = emitPattern $"fsruntime.UnionValue({valueCode})" item
                $"fsruntime.IsUnion({valueCode}, {quotedType}, {quotedCase}) && fsruntime.UnionHasValue({valueCode}) && ({test})", assignments

    let private emitIntrinsic name intrinsicType =
        let quoted = System.Text.Json.JsonSerializer.Serialize(name)
        let rec build index arguments = function
            | TFun(argumentType, resultType) ->
                let argument = $"intrinsicArg{index}"
                let body = build (index + 1) (arguments @ [ argument ]) resultType
                $"func({argument} {goType argumentType}) {goType resultType} {{ return {body} }}"
            | resultType ->
                let values = String.concat ", " arguments
                cast resultType $"fsruntime.Intrinsic({quoted}, []any{{{values}}})"
        build 0 [] intrinsicType

    let rec private emitExpression environment (expression: Lir.Expression) =
        match expression.Node with
        | Lir.Constant value -> Ok(constant value)
        | Lir.Load id ->
            environment |> Map.tryFind id
            |> Option.map Ok
            |> Option.defaultValue (Error(sprintf "Unknown symbol %%%d" (Lir.symbolNumber id)))
        | Lir.MakeClosure(parameter, _, parameterType, body) ->
            let name = identifier "arg" parameter
            emitExpression (environment |> Map.add parameter name) body
            |> Result.map (fun bodyCode -> $"func({name} {goType parameterType}) {goType body.Type} {{ return {bodyCode} }}")
        | Lir.CallClosure(fn, argument) ->
            match emitExpression environment fn, emitExpression environment argument with
            | Ok fnCode, Ok argumentCode -> Ok $"({fnCode})({argumentCode})"
            | Error error, _ | _, Error error -> Error error
        | Lir.Branch(condition, whenTrue, whenFalse) ->
            match emitExpression environment condition, emitExpression environment whenTrue, emitExpression environment whenFalse with
            | Ok c, Ok t, Ok f -> Ok $"func() {goType expression.Type} {{ if {c} {{ return {t} }}; return {f} }}()"
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
                    Ok $"({l} {token} {r})"
                | None -> Error $"Unsupported Go operation {op}"
            | Error error, _ | _, Error error -> Error error
        | Lir.Tuple items | Lir.List items ->
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
                    if recursive then $"var {name} {goType binding.Type}; {name} = {bindingCode}"
                    else $"{name} := {bindingCode}"
                Ok $"func() {goType expression.Type} {{ {declaration}; return {bodyCode} }}()"
            | Error error, _ | _, Error error -> Error error
        | Lir.Fail value ->
            emitExpression environment value
            |> Result.map (fun code -> $"func() {goType expression.Type} {{ fsruntime.Fail({code}); var zero {goType expression.Type}; return zero }}()")
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
                        clauses.Append($"if {test} {{ ") |> ignore
                        assignments |> List.iter (fun assignment -> clauses.Append(assignment).Append("; ") |> ignore)
                        clauses.Append($"if {guardCode} {{ return {bodyCode} }} }}; ") |> ignore
                    | Error error, _ | _, Error error -> failure <- Some error
                match failure with
                | Some error -> Error error
                | None -> Ok $"func() {goType expression.Type} {{ matchValue := {targetCode}; {clauses}fsruntime.Fail(\"Match failure.\"); var zero {goType expression.Type}; return zero }}()")
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
        | Lir.Operation _ -> Error "Invalid operation arity"

    and private collect<'a> (results: Result<'a, string> list) : Result<'a list, string> =
        results |> List.fold (fun state result ->
            match state, result with Ok values, Ok value -> Ok(values @ [ value ]) | Error error, _ | _, Error error -> Error error) (Ok [])

    let generate options (program: Lir.Program) : Result<GeneratedFile list, CompilerDiagnostic list> =
        try
            let names =
                program.Declarations
                |> List.fold (fun env declaration -> env |> Map.add declaration.Id (identifier "value" declaration.Id)) Map.empty
            let declarations = StringBuilder()
            let initializers = StringBuilder()
            let mutable error = None
            for declaration in program.Declarations do
                declarations.AppendLine($"var {names.[declaration.Id]} {goType declaration.Type}") |> ignore
                match emitExpression names declaration.Value with
                | Ok code -> initializers.AppendLine($"\t{names.[declaration.Id]} = {code}") |> ignore
                | Error message -> error <- Some(message, declaration.Span)
            let entry =
                match program.Entry with
                | None -> Ok "struct{}{}"
                | Some value -> emitExpression names value
            match error, entry with
            | Some(message, span), _ ->
                Error [ { Code = "FSB4001"; Stage = GoGeneration; Message = message; Span = Some span } ]
            | None, Error message ->
                Error [ { Code = "FSB4001"; Stage = GoGeneration; Message = message; Span = None } ]
            | None, Ok entryCode ->
                let moduleName = if String.IsNullOrWhiteSpace options.ModuleName then "fscript.generated/program" else options.ModuleName
                let programSource = $"package fscriptprogram\n\nimport \"{moduleName}/internal/fsruntime\"\n\n{declarations}\nfunc init() {{\n{initializers}}}\n\nfunc Run() any {{ return {entryCode} }}\n"
                Ok
                    [ { RelativePath = "go.mod"; Content = $"module {moduleName}\n\ngo 1.25\n" }
                      { RelativePath = "main.go"; Content = $"package main\n\nimport (\n\t\"fmt\"\n\t\"{moduleName}/internal/fscriptprogram\"\n\t\"{moduleName}/internal/fsruntime\"\n)\n\nfunc main() {{ fmt.Println(fsruntime.Format(fscriptprogram.Run())) }}\n" }
                      { RelativePath = "internal/fscriptprogram/program.go"; Content = programSource }
                      { RelativePath = "internal/fsruntime/runtime.go"; Content = "package fsruntime\n\nimport (\n\t\"fmt\"\n\t\"reflect\"\n\t\"sort\"\n\t\"strconv\"\n\t\"strings\"\n)\n\ntype Option struct { HasValue bool; Value any }\ntype Union struct { TypeName string; CaseName string; HasValue bool; Value any }\ntype MapEntry struct { Key any; Value any }\ntype OrderedMap struct { Entries []MapEntry }\ntype MapPart struct { IsSpread bool; Key any; Value any; Map OrderedMap }\n\nfunc Unit(value any) struct{} { return struct{}{} }\nfunc Int(value any) int64 { return value.(int64) }\nfunc Float(value any) float64 { return value.(float64) }\nfunc Bool(value any) bool { return value.(bool) }\nfunc String(value any) string { return value.(string) }\nfunc List(value any) []any { return value.([]any) }\nfunc Record(value any) map[string]any { return value.(map[string]any) }\nfunc AsOption(value any) Option { return value.(Option) }\nfunc AsUnion(value any) Union { return value.(Union) }\nfunc AsMap(value any) OrderedMap { return value.(OrderedMap) }\nfunc Some(value any) Option { return Option{HasValue: true, Value: value} }\nfunc None() Option { return Option{} }\nfunc IsSome(value any) bool { return AsOption(value).HasValue }\nfunc OptionValue(value any) any { return AsOption(value).Value }\nfunc MakeUnion(typeName string, caseName string) Union { return Union{TypeName: typeName, CaseName: caseName} }\nfunc MakeUnionValue(typeName string, caseName string, value any) Union { return Union{TypeName: typeName, CaseName: caseName, HasValue: true, Value: value} }\nfunc IsUnion(value any, typeName string, caseName string) bool { item := AsUnion(value); return item.TypeName == typeName && item.CaseName == caseName }\nfunc UnionHasValue(value any) bool { return AsUnion(value).HasValue }\nfunc UnionValue(value any) any { return AsUnion(value).Value }\nfunc HasField(value any, name string) bool { _, ok := Record(value)[name]; return ok }\nfunc Equal(left any, right any) bool { return reflect.DeepEqual(left, right) }\nfunc Cons(head any, tail []any) []any { return append([]any{head}, tail...) }\nfunc Append(left []any, right []any) []any { return append(append([]any{}, left...), right...) }\nfunc Tail(value any) []any { return append([]any{}, List(value)[1:]...) }\nfunc Range(first int64, last int64) []any { step := int64(1); if first > last { step = -1 }; values := []any{}; for current := first; ; current += step { values = append(values, current); if current == last { break } }; return values }\nfunc UpdateRecord(value any, updates map[string]any) map[string]any { result := map[string]any{}; for key, item := range Record(value) { result[key] = item }; for key, item := range updates { result[key] = item }; return result }\nfunc Pair(key any, value any) MapPart { return MapPart{Key: key, Value: value} }\nfunc SpreadMap(value any) MapPart { return MapPart{IsSpread: true, Map: AsMap(value)} }\nfunc mapSet(entries []MapEntry, key any, value any, replace bool) []MapEntry { for index, entry := range entries { if Equal(entry.Key, key) { if replace { entries[index].Value = value }; return entries } }; return append(entries, MapEntry{Key: key, Value: value}) }\nfunc BuildMap(parts []MapPart) OrderedMap { entries := []MapEntry{}; for _, part := range parts { if part.IsSpread { for _, entry := range part.Map.Entries { entries = mapSet(entries, entry.Key, entry.Value, false) } } else { entries = mapSet(entries, part.Key, part.Value, true) } }; sort.SliceStable(entries, func(left int, right int) bool { return Format(entries[left].Key) < Format(entries[right].Key) }); return OrderedMap{Entries: entries} }\nfunc Index(value any, index any) Option { switch target := value.(type) { case []any: position := Int(index); if position < 0 || position >= int64(len(target)) { return None() }; return Some(target[position]); case OrderedMap: for _, entry := range target.Entries { if Equal(entry.Key, index) { return Some(entry.Value) } }; return None(); default: panic(\"Index access requires a list or map\") } }\nfunc Intrinsic(name string, args []any) any { switch name { case \"ignore\": return struct{}{}; case \"List.empty\": return []any{}; case \"Map.empty\": return OrderedMap{}; case \"Int.tryParse\": value, error := strconv.ParseInt(String(args[0]), 10, 64); if error != nil { return None() }; return Some(value); case \"Float.tryParse\": value, error := strconv.ParseFloat(String(args[0]), 64); if error != nil { return None() }; return Some(value); case \"Bool.tryParse\": value, error := strconv.ParseBool(String(args[0])); if error != nil { return None() }; return Some(value); case \"Int.toString\", \"Float.toString\", \"Bool.toString\": return Format(args[0]); case \"String.replace\": return strings.ReplaceAll(String(args[2]), String(args[0]), String(args[1])); case \"String.indexOf\": index := strings.Index(String(args[1]), String(args[0])); if index < 0 { return None() }; return Some(int64(index)); case \"String.toLower\": return strings.ToLower(String(args[0])); case \"String.toUpper\": return strings.ToUpper(String(args[0])); case \"String.substring\": start, length, value := Int(args[0]), Int(args[1]), String(args[2]); if start < 0 || length < 0 || start + length > int64(len(value)) { return None() }; return Some(value[start:start+length]); case \"String.concat\": values := List(args[1]); parts := make([]string, len(values)); for index, value := range values { parts[index] = String(value) }; return strings.Join(parts, String(args[0])); case \"String.split\": values := strings.Split(String(args[1]), String(args[0])); result := make([]any, len(values)); for index, value := range values { result[index] = value }; return result; case \"String.endsWith\": return strings.HasSuffix(String(args[1]), String(args[0])); case \"List.length\": return int64(len(List(args[0]))); case \"List.rev\": values := List(args[0]); result := make([]any, len(values)); for index, value := range values { result[len(values)-index-1] = value }; return result; case \"List.tryHead\": values := List(args[0]); if len(values) == 0 { return None() }; return Some(values[0]); case \"List.tail\": return Tail(args[0]); case \"List.append\": return Append(List(args[0]), List(args[1])); case \"List.contains\": for _, value := range List(args[1]) { if Equal(args[0], value) { return true } }; return false; case \"Option.isNone\": return !IsSome(args[0]); case \"Option.isSome\": return IsSome(args[0]); case \"Option.defaultValue\": option := AsOption(args[1]); if option.HasValue { return option.Value }; return args[0]; case \"Map.count\": return int64(len(AsMap(args[0]).Entries)); case \"Map.containsKey\": return IsSome(Index(args[1], args[0])); case \"Map.tryGet\": return Index(args[1], args[0]); default: panic(\"Unsupported FScript intrinsic: \" + name) } }\nfunc Format(value any) string { switch item := value.(type) { case bool: if item { return \"true\" }; return \"false\"; case Option: if !item.HasValue { return \"None\" }; return \"Some \" + Format(item.Value); case Union: if !item.HasValue { return item.CaseName }; return item.CaseName + \" \" + Format(item.Value); case OrderedMap: parts := make([]string, len(item.Entries)); for index, entry := range item.Entries { parts[index] = \"[\" + Format(entry.Key) + \"] = \" + Format(entry.Value) }; return \"{ \" + strings.Join(parts, \"; \") + \" }\"; case []any: parts := make([]string, len(item)); for index, value := range item { parts[index] = Format(value) }; return \"[\" + strings.Join(parts, \";\") + \"]\"; default: return fmt.Sprint(value) } }\nfunc Interpolate(values []any) string { var result strings.Builder; for _, value := range values { result.WriteString(Format(value)) }; return result.String() }\nfunc Fail(value any) { panic(fmt.Sprint(value)) }\n" } ]
        with ex ->
            Error [ { Code = "FSB4002"; Stage = GoGeneration; Message = ex.Message; Span = None } ]
