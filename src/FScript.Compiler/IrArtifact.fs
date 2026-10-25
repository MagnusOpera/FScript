namespace FScript.Compiler

open System
open System.IO
open System.Text
open System.Text.Json
open FScript.Language

module IrArtifact =
    [<Literal>]
    let Format = "fscript-lir"
    [<Literal>]
    let Version = 1

    let private assemblyVersion (assembly: Reflection.Assembly) =
        match assembly.GetName().Version with null -> "0.0.0" | value -> value.ToString()

    let private normalizePath (path: string) = path.Replace('\\', '/')

    let private sourceMetadata (program: Lir.Program) =
        let paths =
            [ yield! program.Types |> List.choose (fun declaration -> declaration.Span.Start.File)
              yield! program.ImportedRecords |> List.choose (fun declaration -> declaration.Span.Start.File)
              yield! program.Functions |> List.choose (fun fn -> fn.Span.Start.File)
              yield! program.Declarations |> List.choose (fun declaration -> declaration.Span.Start.File)
              match program.Entry with Some entry -> yield! entry.Span.Start.File |> Option.toList | None -> () ]
            |> List.distinct
            |> List.sort
        paths |> List.choose (fun path ->
            if File.Exists path then
                let hash =
                    File.ReadAllBytes path
                    |> Security.Cryptography.SHA256.HashData
                    |> Convert.ToHexString
                    |> _.ToLowerInvariant()
                Some(normalizePath path, hash)
            else None)

    let private dependencies (program: Lir.Program) =
        [ yield! program.ImportedRecords |> List.map (fun record -> $"target:{record.ModulePath}:{record.MemberName}")
          yield! program.Declarations |> List.choose (fun declaration ->
              match declaration.Value.Node with
              | Lir.HostExternal(name, _) -> Some $"host:{name}"
              | Lir.TargetImport(modulePath, memberName) -> Some $"target:{modulePath}:{memberName}"
              | _ -> None) ]
        |> List.distinct
        |> List.sort

    let private writeOptionString (writer: Utf8JsonWriter) (name: string) (value: string option) =
        match value with
        | Some text -> writer.WriteString(name, text)
        | None -> writer.WriteNull(name)

    let rec private writeType (writer: Utf8JsonWriter) value =
        writer.WriteStartObject()
        let kind, arguments, name =
            match value with
            | TUnit -> "unit", [], None | TInt -> "int", [], None | TFloat -> "float", [], None
            | TBool -> "bool", [], None | TString -> "string", [], None | TTypeToken -> "type-token", [], None
            | TList item -> "list", [ item ], None | TTask item -> "task", [ item ], None
            | TOption item -> "option", [ item ], None | TTuple items -> "tuple", items, None
            | TMap(key, item) -> "map", [ key; item ], None | TFun(argument, result) -> "function", [ argument; result ], None
            | TNamed typeName -> "named", [], Some typeName
            | TVar id -> "variable", [], Some(string id)
            | TNumeric id -> "numeric", [], Some(string id)
            | TRecordShape(id, fields, constructible) ->
                writer.WriteString("kind", "record-shape")
                writer.WriteNumber("id", id); writer.WriteBoolean("constructible", constructible)
                writer.WriteStartArray("fields")
                for fieldName, fieldType in fields |> Map.toList do
                    writer.WriteStartObject(); writer.WriteString("name", fieldName); writer.WritePropertyName("type")
                    writeType writer fieldType; writer.WriteEndObject()
                writer.WriteEndArray(); writer.WriteEndObject()
                "", [], None
            | TRecord fields | THostManaged(_, fields) ->
                match value with
                | THostManaged(name, _) -> writer.WriteString("kind", "host-managed"); writer.WriteString("name", name)
                | _ -> writer.WriteString("kind", "record")
                writer.WriteStartArray("fields")
                for fieldName, fieldType in fields |> Map.toList do
                    writer.WriteStartObject(); writer.WriteString("name", fieldName); writer.WritePropertyName("type")
                    writeType writer fieldType; writer.WriteEndObject()
                writer.WriteEndArray(); writer.WriteEndObject()
                "", [], None
            | TUnion(typeName, cases) ->
                writer.WriteString("kind", "union"); writer.WriteString("name", typeName)
                writer.WriteStartArray("cases")
                for caseName, payload in cases |> Map.toList do
                    writer.WriteStartObject(); writer.WriteString("name", caseName)
                    match payload with
                    | Some payloadType -> writer.WritePropertyName("payload"); writeType writer payloadType
                    | None -> writer.WriteNull("payload")
                    writer.WriteEndObject()
                writer.WriteEndArray(); writer.WriteEndObject()
                "", [], None
        if kind <> "" then
            writer.WriteString("kind", kind)
            match name with Some value -> writer.WriteString("name", value) | None -> ()
            if not arguments.IsEmpty then
                writer.WriteStartArray("arguments"); arguments |> List.iter (writeType writer); writer.WriteEndArray()
            writer.WriteEndObject()

    let private writePosition (writer: Utf8JsonWriter) value =
        writer.WriteStartObject(); writeOptionString writer "file" value.File
        writer.WriteNumber("line", value.Line); writer.WriteNumber("column", value.Column); writer.WriteEndObject()

    let private writeSpan (writer: Utf8JsonWriter) value =
        writer.WriteStartObject(); writer.WritePropertyName("start"); writePosition writer value.Start
        writer.WritePropertyName("end"); writePosition writer value.End; writer.WriteEndObject()

    let private constantName = function
        | Lir.Unit -> "unit" | Lir.Int _ -> "int" | Lir.Float _ -> "float" | Lir.Bool _ -> "bool" | Lir.String _ -> "string"

    let private writeConstantValue (writer: Utf8JsonWriter) constant =
        writer.WriteString("constantKind", constantName constant)
        match constant with
        | Lir.Unit -> () | Lir.Int value -> writer.WriteNumber("intValue", value) | Lir.Float value -> writer.WriteNumber("floatValue", value)
        | Lir.Bool value -> writer.WriteBoolean("boolValue", value) | Lir.String value -> writer.WriteString("stringValue", value)

    let private writeProjection (writer: Utf8JsonWriter) (name: string) (steps: Lir.ProjectionStep list) =
        writer.WriteStartArray(name)
        for step in steps do
            writer.WriteStartObject()
            match step with
            | Lir.ListHead -> writer.WriteString("kind", "list-head")
            | Lir.ListTail -> writer.WriteString("kind", "list-tail")
            | Lir.TupleItem index -> writer.WriteString("kind", "tuple-item"); writer.WriteNumber("index", index)
            | Lir.RecordField name -> writer.WriteString("kind", "record-field"); writer.WriteString("name", name)
            | Lir.OptionValue -> writer.WriteString("kind", "option-value")
            | Lir.UnionPayload -> writer.WriteString("kind", "union-payload")
            writer.WriteEndObject()
        writer.WriteEndArray()

    let private writeCheck (writer: Utf8JsonWriter) check =
        writer.WriteStartObject()
        match check with
        | Lir.LiteralCheck(projection, value) -> writer.WriteString("kind", "literal"); writeProjection writer "projection" projection; writeConstantValue writer value
        | Lir.EmptyListCheck projection -> writer.WriteString("kind", "empty-list"); writeProjection writer "projection" projection
        | Lir.NonEmptyListCheck projection -> writer.WriteString("kind", "non-empty-list"); writeProjection writer "projection" projection
        | Lir.TupleArityCheck(projection, arity) -> writer.WriteString("kind", "tuple-arity"); writeProjection writer "projection" projection; writer.WriteNumber("arity", arity)
        | Lir.RecordFieldCheck(projection, name) -> writer.WriteString("kind", "record-field"); writeProjection writer "projection" projection; writer.WriteString("name", name)
        | Lir.SomeCheck projection -> writer.WriteString("kind", "some"); writeProjection writer "projection" projection
        | Lir.NoneCheck projection -> writer.WriteString("kind", "none"); writeProjection writer "projection" projection
        | Lir.UnionCaseCheck(projection, typeName, caseName, hasPayload) ->
            writer.WriteString("kind", "union-case"); writeProjection writer "projection" projection
            writer.WriteString("typeName", typeName); writer.WriteString("caseName", caseName); writer.WriteBoolean("hasPayload", hasPayload)
        | Lir.TypeCheck(projection, patternType) ->
            writer.WriteString("kind", "type"); writeProjection writer "projection" projection
            writer.WritePropertyName("patternType"); writeType writer patternType
        writer.WriteEndObject()

    let rec private writePlan (writer: Utf8JsonWriter) (plan: Lir.PatternPlan) =
        writer.WriteStartObject(); writer.WritePropertyName("span"); writeSpan writer plan.Span
        writer.WriteStartArray("checks"); plan.Checks |> List.iter (writeCheck writer); writer.WriteEndArray()
        writer.WriteStartArray("bindings")
        for binding in plan.Bindings do
            writer.WriteStartObject(); writer.WriteNumber("symbol", Lir.symbolNumber binding.Id); writer.WriteString("name", binding.Name)
            writer.WritePropertyName("type"); writeType writer binding.Type; writeProjection writer "projection" binding.Projection
            writer.WriteEndObject()
        writer.WriteEndArray(); writer.WriteStartArray("maps")
        for mapPlan in plan.Maps do
            writer.WriteStartObject(); writeProjection writer "projection" mapPlan.Projection
            writer.WriteBoolean("requireEmptyRemainder", mapPlan.RequireEmptyRemainder); writer.WriteStartArray("clauses")
            for clause in mapPlan.Clauses do
                writer.WriteStartObject(); writer.WritePropertyName("key"); writePlan writer clause.Key
                writer.WritePropertyName("value"); writePlan writer clause.Value; writer.WriteEndObject()
            writer.WriteEndArray()
            match mapPlan.Tail with Some tail -> writer.WritePropertyName("tail"); writePlan writer tail | None -> writer.WriteNull("tail")
            writer.WriteEndObject()
        writer.WriteEndArray(); writer.WriteEndObject()

    let rec private writeExpression (writer: Utf8JsonWriter) (value: Lir.Expression) =
        writer.WriteStartObject(); writer.WritePropertyName("type"); writeType writer value.Type
        writer.WritePropertyName("span"); writeSpan writer value.Span
        match value.Node with
        | Lir.Constant constant ->
            writer.WriteString("kind", "constant"); writeConstantValue writer constant
        | Lir.Load id -> writer.WriteString("kind", "load"); writer.WriteNumber("symbol", Lir.symbolNumber id)
        | Lir.MakeClosure(id, captures) ->
            writer.WriteString("kind", "closure"); writer.WriteNumber("function", Lir.functionNumber id)
            writer.WriteStartArray("captures"); captures |> List.iter (Lir.symbolNumber >> writer.WriteNumberValue); writer.WriteEndArray()
        | Lir.CallClosure(fn, argument) ->
            writer.WriteString("kind", "call"); writer.WritePropertyName("function"); writeExpression writer fn
            writer.WritePropertyName("argument"); writeExpression writer argument
        | Lir.PipeClosure(argument, fn) ->
            writer.WriteString("kind", "pipe-call"); writer.WritePropertyName("argument"); writeExpression writer argument
            writer.WritePropertyName("function"); writeExpression writer fn
        | Lir.TailCallClosure(fn, argument) ->
            writer.WriteString("kind", "tail-call"); writer.WritePropertyName("function"); writeExpression writer fn
            writer.WritePropertyName("argument"); writeExpression writer argument
        | Lir.TailPipeClosure(argument, fn) ->
            writer.WriteString("kind", "tail-pipe-call"); writer.WritePropertyName("argument"); writeExpression writer argument
            writer.WritePropertyName("function"); writeExpression writer fn
        | Lir.Branch(condition, whenTrue, whenFalse) ->
            writer.WriteString("kind", "branch"); writer.WritePropertyName("condition"); writeExpression writer condition
            writer.WritePropertyName("whenTrue"); writeExpression writer whenTrue; writer.WritePropertyName("whenFalse"); writeExpression writer whenFalse
        | Lir.Operation(operation, arguments) ->
            writer.WriteString("kind", "operation"); writer.WriteString("operation", sprintf "%A" operation)
            writer.WriteStartArray("arguments"); arguments |> List.iter (writeExpression writer); writer.WriteEndArray()
        | Lir.Tuple items | Lir.List items ->
            writer.WriteString("kind", match value.Node with Lir.Tuple _ -> "tuple" | _ -> "list")
            writer.WriteStartArray("items"); items |> List.iter (writeExpression writer); writer.WriteEndArray()
        | Lir.Record fields ->
            writer.WriteString("kind", "record"); writer.WriteStartArray("fields")
            for name, field in fields do
                writer.WriteStartObject(); writer.WriteString("name", name); writer.WritePropertyName("value")
                writeExpression writer field; writer.WriteEndObject()
            writer.WriteEndArray()
        | Lir.FieldGet(target, name) ->
            writer.WriteString("kind", "field-get"); writer.WriteString("name", name)
            writer.WritePropertyName("target"); writeExpression writer target
        | Lir.SomeValue item ->
            writer.WriteString("kind", "some"); writer.WritePropertyName("value"); writeExpression writer item
        | Lir.NoneValue -> writer.WriteString("kind", "none")
        | Lir.Cons(head, tail) ->
            writer.WriteString("kind", "cons"); writer.WritePropertyName("head"); writeExpression writer head
            writer.WritePropertyName("tail"); writeExpression writer tail
        | Lir.Append(left, right) ->
            writer.WriteString("kind", "append"); writer.WritePropertyName("left"); writeExpression writer left
            writer.WritePropertyName("right"); writeExpression writer right
        | Lir.Let(id, name, recursive, binding, body) ->
            writer.WriteString("kind", "let"); writer.WriteNumber("symbol", Lir.symbolNumber id); writer.WriteString("name", name)
            writer.WriteBoolean("recursive", recursive); writer.WritePropertyName("binding"); writeExpression writer binding
            writer.WritePropertyName("body"); writeExpression writer body
        | Lir.LetRecursiveGroup(bindings, body) ->
            writer.WriteString("kind", "let-rec-group"); writer.WriteStartArray("bindings")
            for id, name, bindingType, value in bindings do
                writer.WriteStartObject(); writer.WriteNumber("symbol", Lir.symbolNumber id); writer.WriteString("name", name)
                writer.WritePropertyName("bindingType"); writeType writer bindingType
                writer.WritePropertyName("value"); writeExpression writer value; writer.WriteEndObject()
            writer.WriteEndArray(); writer.WritePropertyName("body"); writeExpression writer body
        | Lir.Fail value -> writer.WriteString("kind", "fail"); writer.WritePropertyName("value"); writeExpression writer value
        | Lir.DecisionTree(target, cases) ->
            writer.WriteString("kind", "decision-tree"); writer.WritePropertyName("target"); writeExpression writer target
            writer.WriteStartArray("cases")
            for case in cases do
                writer.WriteStartObject(); writer.WritePropertyName("plan"); writePlan writer case.Plan
                match case.Guard with Some guard -> writer.WritePropertyName("guard"); writeExpression writer guard | None -> writer.WriteNull("guard")
                writer.WritePropertyName("body"); writeExpression writer case.Body; writer.WritePropertyName("span"); writeSpan writer case.Span
                writer.WriteEndObject()
            writer.WriteEndArray()
        | Lir.Range(startValue, endValue) ->
            writer.WriteString("kind", "range"); writer.WritePropertyName("start"); writeExpression writer startValue
            writer.WritePropertyName("end"); writeExpression writer endValue
        | Lir.RecordUpdate(target, fields) ->
            writer.WriteString("kind", "record-update"); writer.WritePropertyName("target"); writeExpression writer target
            writer.WriteStartArray("fields")
            for name, field in fields do
                writer.WriteStartObject(); writer.WriteString("name", name); writer.WritePropertyName("value"); writeExpression writer field; writer.WriteEndObject()
            writer.WriteEndArray()
        | Lir.RecordConvert(target, targetType) ->
            writer.WriteString("kind", "record-convert"); writer.WritePropertyName("target"); writeExpression writer target
            writer.WritePropertyName("targetType"); writeType writer targetType
        | Lir.IndexGet(target, index) ->
            writer.WriteString("kind", "index-get"); writer.WritePropertyName("target"); writeExpression writer target
            writer.WritePropertyName("index"); writeExpression writer index
        | Lir.Interpolate parts ->
            writer.WriteString("kind", "interpolate"); writer.WriteStartArray("parts")
            for part in parts do
                writer.WriteStartObject()
                match part with
                | Lir.TextPart text -> writer.WriteString("kind", "text"); writer.WriteString("text", text)
                | Lir.ValuePart item -> writer.WriteString("kind", "value"); writer.WritePropertyName("value"); writeExpression writer item
                writer.WriteEndObject()
            writer.WriteEndArray()
        | Lir.UnionCase(typeName, caseName, payload) ->
            writer.WriteString("kind", "union-case"); writer.WriteString("typeName", typeName); writer.WriteString("caseName", caseName)
            match payload with Some item -> writer.WritePropertyName("payload"); writeExpression writer item | None -> writer.WriteNull("payload")
        | Lir.Map entries ->
            writer.WriteString("kind", "map"); writer.WriteStartArray("entries")
            for entry in entries do
                writer.WriteStartObject()
                match entry with
                | Lir.KeyValue(key, value) -> writer.WriteString("kind", "key-value"); writer.WritePropertyName("key"); writeExpression writer key; writer.WritePropertyName("value"); writeExpression writer value
                | Lir.Spread value -> writer.WriteString("kind", "spread"); writer.WritePropertyName("value"); writeExpression writer value
                writer.WriteEndObject()
            writer.WriteEndArray()
        | Lir.Intrinsic name -> writer.WriteString("kind", "intrinsic"); writer.WriteString("name", name)
        | Lir.HostExternal(name, arity) -> writer.WriteString("kind", "host-external"); writer.WriteString("name", name); writer.WriteNumber("arity", arity)
        | Lir.RuntimeContext -> writer.WriteString("kind", "runtime-context")
        | Lir.TargetImport(modulePath, memberName) -> writer.WriteString("kind", "target-import"); writer.WriteString("modulePath", modulePath); writer.WriteString("memberName", memberName)
        | Lir.ForEach(id, name, itemType, source, body) ->
            writer.WriteString("kind", "for-each"); writer.WriteNumber("symbol", Lir.symbolNumber id); writer.WriteString("name", name)
            writer.WritePropertyName("itemType"); writeType writer itemType; writer.WritePropertyName("source"); writeExpression writer source
            writer.WritePropertyName("body"); writeExpression writer body
        | Lir.TypeToken tokenType -> writer.WriteString("kind", "type-token"); writer.WritePropertyName("tokenType"); writeType writer tokenType
        writer.WriteEndObject()

    let serialize (program: Lir.Program) =
        use stream = new MemoryStream()
        use writer = new Utf8JsonWriter(stream, JsonWriterOptions(Indented = true))
        writer.WriteStartObject(); writer.WriteString("format", Format); writer.WriteNumber("irVersion", Version)
        writer.WriteString("compilerVersion", assemblyVersion typeof<Lir.Program>.Assembly)
        writer.WriteString("languageVersion", assemblyVersion typeof<Type>.Assembly)
        writer.WriteStartArray("sources")
        for path, hash in sourceMetadata program do
            writer.WriteStartObject(); writer.WriteString("path", path); writer.WriteString("sha256", hash); writer.WriteEndObject()
        writer.WriteEndArray(); writer.WriteStartArray("dependencies")
        dependencies program |> List.iter writer.WriteStringValue
        writer.WriteEndArray()
        writer.WriteStartObject("program"); writer.WriteStartArray("types")
        for declaration in program.Types do
            writer.WriteStartObject(); writer.WriteNumber("id", Lir.symbolNumber declaration.Id); writer.WriteString("name", declaration.Name)
            writer.WriteBoolean("recursive", declaration.IsRecursive); writer.WriteBoolean("pure", declaration.IsPure); writer.WritePropertyName("definition"); writeType writer declaration.Definition
            match declaration.Import with
            | Some imported ->
                writer.WriteStartObject("import"); writer.WriteString("name", imported.Name); writer.WriteString("modulePath", imported.ModulePath)
                writer.WriteString("memberName", imported.MemberName); writer.WriteBoolean("union", imported.IsUnion); writer.WriteStartArray("fields")
                for name, fieldType in imported.Fields do
                    writer.WriteStartObject(); writer.WriteString("name", name); writer.WritePropertyName("type"); writeType writer fieldType; writer.WriteEndObject()
                writer.WriteEndArray(); writer.WritePropertyName("span"); writeSpan writer imported.Span; writer.WriteEndObject()
            | None -> writer.WriteNull("import")
            writer.WritePropertyName("span"); writeSpan writer declaration.Span; writer.WriteEndObject()
        writer.WriteEndArray(); writer.WriteStartArray("importedRecords")
        for record in program.ImportedRecords do
            writer.WriteStartObject(); writer.WriteString("name", record.Name); writer.WriteString("modulePath", record.ModulePath); writer.WriteString("memberName", record.MemberName); writer.WriteBoolean("union", record.IsUnion)
            writer.WriteStartArray("fields")
            for name, fieldType in record.Fields do
                writer.WriteStartObject(); writer.WriteString("name", name); writer.WritePropertyName("type"); writeType writer fieldType; writer.WriteEndObject()
            writer.WriteEndArray(); writer.WritePropertyName("span"); writeSpan writer record.Span; writer.WriteEndObject()
        writer.WriteEndArray(); writer.WriteStartArray("functions")
        for fn in program.Functions do
            writer.WriteStartObject(); writer.WriteNumber("id", Lir.functionNumber fn.Id)
            writer.WriteNumber("parameter", Lir.symbolNumber fn.Parameter); writer.WriteString("name", fn.Name)
            writer.WritePropertyName("parameterType"); writeType writer fn.ParameterType
            writer.WriteStartArray("captures"); fn.Captures |> List.iter (Lir.symbolNumber >> writer.WriteNumberValue); writer.WriteEndArray()
            writer.WritePropertyName("returnType"); writeType writer fn.ReturnType
            writer.WritePropertyName("span"); writeSpan writer fn.Span
            writer.WritePropertyName("body"); writeExpression writer fn.Body; writer.WriteEndObject()
        writer.WriteEndArray(); writer.WriteStartArray("declarations")
        for declaration in program.Declarations do
            writer.WriteStartObject(); writer.WriteNumber("id", Lir.symbolNumber declaration.Id); writer.WriteString("name", declaration.Name)
            writer.WritePropertyName("type"); writeType writer declaration.Type; writer.WriteBoolean("recursive", declaration.IsRecursive)
            writer.WriteBoolean("exported", declaration.IsExported); writer.WriteBoolean("pure", declaration.IsPure)
            match declaration.ExternalArity with Some arity -> writer.WriteNumber("externalArity",arity) | None -> writer.WriteNull("externalArity")
            match declaration.Export with
            | Some export -> writer.WriteStartObject("export"); writer.WriteString("name", export.Name); writer.WritePropertyName("span"); writeSpan writer export.Span; writer.WriteEndObject()
            | None -> writer.WriteNull("export")
            writer.WritePropertyName("span"); writeSpan writer declaration.Span
            writer.WritePropertyName("value"); writeExpression writer declaration.Value; writer.WriteEndObject()
        writer.WriteEndArray()
        match program.Entry with Some entry -> writer.WritePropertyName("entry"); writeExpression writer entry | None -> writer.WriteNull("entry")
        writer.WriteEndObject(); writer.WriteEndObject(); writer.Flush(); stream.ToArray()

    let private property (name: string) (element: JsonElement) = element.GetProperty(name)
    let private hasProperty (name: string) (element: JsonElement) =
        let mutable value = Unchecked.defaultof<JsonElement>
        element.TryGetProperty(name, &value)
    let private stringProperty (name: string) (element: JsonElement) =
        match (property name element).GetString() with null -> "" | value -> value
    let private readPosition (element: JsonElement) =
        let fileElement = property "file" element
        { File = if fileElement.ValueKind = JsonValueKind.Null then None else fileElement.GetString() |> Option.ofObj
          Line = (property "line" element).GetInt32(); Column = (property "column" element).GetInt32() }
    let private readSpan (element: JsonElement) = { Start = readPosition (property "start" element); End = readPosition (property "end" element) }

    let rec private readType (element: JsonElement) =
        let args () = (property "arguments" element).EnumerateArray() |> Seq.map readType |> Seq.toList
        match stringProperty "kind" element with
        | "unit" -> TUnit | "int" -> TInt | "float" -> TFloat | "bool" -> TBool | "string" -> TString | "type-token" -> TTypeToken
        | "list" -> TList(args().Head) | "task" -> TTask(args().Head) | "option" -> TOption(args().Head) | "tuple" -> TTuple(args())
        | "map" -> let values = args() in TMap(values.[0], values.[1])
        | "function" -> let values = args() in TFun(values.[0], values.[1])
        | "named" -> TNamed(stringProperty "name" element) | "variable" -> TVar(Int32.Parse(stringProperty "name" element))
        | "numeric" -> TNumeric(Int32.Parse(stringProperty "name" element))
        | "host-managed" ->
            THostManaged(stringProperty "name" element, property "fields" element |> _.EnumerateArray()
                |> Seq.map (fun field -> stringProperty "name" field, readType (property "type" field)) |> Map.ofSeq)
        | "record-shape" ->
            TRecordShape((property "id" element).GetInt32(), property "fields" element |> _.EnumerateArray()
                |> Seq.map (fun field -> stringProperty "name" field, readType (property "type" field)) |> Map.ofSeq,
                (property "constructible" element).GetBoolean())
        | "record" ->
            (property "fields" element).EnumerateArray()
            |> Seq.map (fun field -> stringProperty "name" field, readType (property "type" field)) |> Map.ofSeq |> TRecord
        | "union" ->
            let cases = (property "cases" element).EnumerateArray() |> Seq.map (fun item ->
                let payload = property "payload" item
                stringProperty "name" item, if payload.ValueKind = JsonValueKind.Null then None else Some(readType payload)) |> Map.ofSeq
            TUnion(stringProperty "name" element, cases)
        | kind -> invalidOp $"Unknown IR type kind '{kind}'."

    let private readOperation = function
        | "NumericAdd" -> Lir.NumericAdd | "NumericSubtract" -> Lir.NumericSubtract
        | "NumericMultiply" -> Lir.NumericMultiply | "NumericDivide" -> Lir.NumericDivide
        | "IntAdd" -> Lir.IntAdd | "FloatAdd" -> Lir.FloatAdd | "IntSubtract" -> Lir.IntSubtract | "FloatSubtract" -> Lir.FloatSubtract
        | "IntMultiply" -> Lir.IntMultiply | "FloatMultiply" -> Lir.FloatMultiply | "IntDivide" -> Lir.IntDivide | "FloatDivide" -> Lir.FloatDivide
        | "IntRemainder" -> Lir.IntRemainder | "StructuralEqual" -> Lir.StructuralEqual | "NumericLessThan" -> Lir.NumericLessThan
        | "NumericGreaterThan" -> Lir.NumericGreaterThan | "NumericLessThanOrEqual" -> Lir.NumericLessThanOrEqual
        | "NumericGreaterThanOrEqual" -> Lir.NumericGreaterThanOrEqual | "BooleanAnd" -> Lir.BooleanAnd | "BooleanOr" -> Lir.BooleanOr
        | value -> invalidOp $"Unknown LIR operation '{value}'."

    let private readConstant (element: JsonElement) =
        match stringProperty "constantKind" element with
        | "unit" -> Lir.Unit | "int" -> Lir.Int((property "intValue" element).GetInt64())
        | "float" -> Lir.Float((property "floatValue" element).GetDouble())
        | "bool" -> Lir.Bool((property "boolValue" element).GetBoolean())
        | "string" -> Lir.String(stringProperty "stringValue" element)
        | value -> invalidOp $"Unknown constant '{value}'."

    let private readProjection (element: JsonElement) name =
        (property name element).EnumerateArray()
        |> Seq.map (fun step ->
            match stringProperty "kind" step with
            | "list-head" -> Lir.ListHead
            | "list-tail" -> Lir.ListTail
            | "tuple-item" -> Lir.TupleItem((property "index" step).GetInt32())
            | "record-field" -> Lir.RecordField(stringProperty "name" step)
            | "option-value" -> Lir.OptionValue
            | "union-payload" -> Lir.UnionPayload
            | kind -> invalidOp $"Unknown LIR projection step '{kind}'.")
        |> Seq.toList

    let private readCheck (element: JsonElement) =
        let projection = readProjection element "projection"
        match stringProperty "kind" element with
        | "literal" -> Lir.LiteralCheck(projection, readConstant element)
        | "empty-list" -> Lir.EmptyListCheck projection
        | "non-empty-list" -> Lir.NonEmptyListCheck projection
        | "tuple-arity" -> Lir.TupleArityCheck(projection, (property "arity" element).GetInt32())
        | "record-field" -> Lir.RecordFieldCheck(projection, stringProperty "name" element)
        | "some" -> Lir.SomeCheck projection
        | "none" -> Lir.NoneCheck projection
        | "union-case" -> Lir.UnionCaseCheck(projection, stringProperty "typeName" element, stringProperty "caseName" element, (property "hasPayload" element).GetBoolean())
        | "type" -> Lir.TypeCheck(projection, readType (property "patternType" element))
        | kind -> invalidOp $"Unknown LIR pattern-plan check '{kind}'."

    let rec private readPlan (element: JsonElement) : Lir.PatternPlan =
        let bindings =
            (property "bindings" element).EnumerateArray()
            |> Seq.map (fun binding ->
                ({ Id = Lir.SymbolId((property "symbol" binding).GetInt32())
                   Name = stringProperty "name" binding
                   Type = readType (property "type" binding)
                   Projection = readProjection binding "projection" }: Lir.PatternBinding))
            |> Seq.toList
        let maps =
            (property "maps" element).EnumerateArray()
            |> Seq.map (fun mapPlan ->
                let tail = property "tail" mapPlan
                ({ Projection = readProjection mapPlan "projection"
                   Clauses =
                    (property "clauses" mapPlan).EnumerateArray()
                    |> Seq.map (fun clause -> ({ Key = readPlan (property "key" clause); Value = readPlan (property "value" clause) }: Lir.MapClausePlan))
                    |> Seq.toList
                   Tail = if tail.ValueKind = JsonValueKind.Null then None else Some(readPlan tail)
                   RequireEmptyRemainder = (property "requireEmptyRemainder" mapPlan).GetBoolean() }: Lir.MapPlan))
            |> Seq.toList
        { Span = readSpan (property "span" element)
          Checks = (property "checks" element).EnumerateArray() |> Seq.map readCheck |> Seq.toList
          Bindings = bindings
          Maps = maps }

    let rec private readExpression (element: JsonElement) : Lir.Expression =
        let expressionType = readType (property "type" element)
        let span = readSpan (property "span" element)
        let node =
            match stringProperty "kind" element with
            | "constant" -> Lir.Constant(readConstant element)
            | "load" -> Lir.Load(Lir.SymbolId((property "symbol" element).GetInt32()))
            | "closure" ->
                Lir.MakeClosure(
                    Lir.FunctionId((property "function" element).GetInt32()),
                    (property "captures" element).EnumerateArray() |> Seq.map (fun item -> Lir.SymbolId(item.GetInt32())) |> Seq.toList)
            | "call" -> Lir.CallClosure(readExpression (property "function" element), readExpression (property "argument" element))
            | "pipe-call" -> Lir.PipeClosure(readExpression (property "argument" element), readExpression (property "function" element))
            | "tail-call" -> Lir.TailCallClosure(readExpression (property "function" element), readExpression (property "argument" element))
            | "tail-pipe-call" -> Lir.TailPipeClosure(readExpression (property "argument" element), readExpression (property "function" element))
            | "branch" -> Lir.Branch(readExpression (property "condition" element), readExpression (property "whenTrue" element), readExpression (property "whenFalse" element))
            | "operation" -> Lir.Operation(readOperation (stringProperty "operation" element), (property "arguments" element).EnumerateArray() |> Seq.map readExpression |> Seq.toList)
            | "tuple" -> Lir.Tuple((property "items" element).EnumerateArray() |> Seq.map readExpression |> Seq.toList)
            | "list" -> Lir.List((property "items" element).EnumerateArray() |> Seq.map readExpression |> Seq.toList)
            | "record" ->
                Lir.Record((property "fields" element).EnumerateArray() |> Seq.map (fun field -> stringProperty "name" field, readExpression (property "value" field)) |> Seq.toList)
            | "field-get" -> Lir.FieldGet(readExpression (property "target" element), stringProperty "name" element)
            | "some" -> Lir.SomeValue(readExpression (property "value" element))
            | "none" -> Lir.NoneValue
            | "cons" -> Lir.Cons(readExpression (property "head" element), readExpression (property "tail" element))
            | "append" -> Lir.Append(readExpression (property "left" element), readExpression (property "right" element))
            | "let" -> Lir.Let(Lir.SymbolId((property "symbol" element).GetInt32()), stringProperty "name" element,
                                (property "recursive" element).GetBoolean(), readExpression (property "binding" element), readExpression (property "body" element))
            | "let-rec-group" ->
                let bindings = (property "bindings" element).EnumerateArray() |> Seq.map (fun item ->
                    Lir.SymbolId((property "symbol" item).GetInt32()), stringProperty "name" item,
                    readType (property "bindingType" item), readExpression (property "value" item)) |> Seq.toList
                Lir.LetRecursiveGroup(bindings, readExpression (property "body" element))
            | "fail" -> Lir.Fail(readExpression (property "value" element))
            | "decision-tree" ->
                let cases = (property "cases" element).EnumerateArray() |> Seq.map (fun item ->
                    let guard = property "guard" item
                    ({ Plan = readPlan (property "plan" item)
                       Guard = if guard.ValueKind = JsonValueKind.Null then None else Some(readExpression guard)
                       Body = readExpression (property "body" item)
                       Span = readSpan (property "span" item) }: Lir.DecisionCase)) |> Seq.toList
                Lir.DecisionTree(readExpression (property "target" element), cases)
            | "range" -> Lir.Range(readExpression (property "start" element), readExpression (property "end" element))
            | "record-update" ->
                let fields = (property "fields" element).EnumerateArray() |> Seq.map (fun item -> stringProperty "name" item, readExpression (property "value" item)) |> Seq.toList
                Lir.RecordUpdate(readExpression (property "target" element), fields)
            | "record-convert" -> Lir.RecordConvert(readExpression (property "target" element), readType (property "targetType" element))
            | "index-get" -> Lir.IndexGet(readExpression (property "target" element), readExpression (property "index" element))
            | "interpolate" ->
                Lir.Interpolate((property "parts" element).EnumerateArray() |> Seq.map (fun item ->
                    match stringProperty "kind" item with
                    | "text" -> Lir.TextPart(stringProperty "text" item)
                    | "value" -> Lir.ValuePart(readExpression (property "value" item))
                    | kind -> invalidOp $"Unknown interpolation part '{kind}'.") |> Seq.toList)
            | "union-case" ->
                let payload = property "payload" element
                Lir.UnionCase(stringProperty "typeName" element, stringProperty "caseName" element, if payload.ValueKind = JsonValueKind.Null then None else Some(readExpression payload))
            | "map" ->
                Lir.Map((property "entries" element).EnumerateArray() |> Seq.map (fun item ->
                    match stringProperty "kind" item with
                    | "key-value" -> Lir.KeyValue(readExpression (property "key" item), readExpression (property "value" item))
                    | "spread" -> Lir.Spread(readExpression (property "value" item))
                    | kind -> invalidOp $"Unknown map entry '{kind}'.") |> Seq.toList)
            | "intrinsic" -> Lir.Intrinsic(stringProperty "name" element)
            | "host-external" -> Lir.HostExternal(stringProperty "name" element, (property "arity" element).GetInt32())
            | "runtime-context" -> Lir.RuntimeContext
            | "target-import" -> Lir.TargetImport(stringProperty "modulePath" element, stringProperty "memberName" element)
            | "for-each" -> Lir.ForEach(Lir.SymbolId((property "symbol" element).GetInt32()), stringProperty "name" element,
                                         readType (property "itemType" element), readExpression (property "source" element), readExpression (property "body" element))
            | "type-token" -> Lir.TypeToken(readType (property "tokenType" element))
            | kind -> invalidOp $"Unknown LIR expression kind '{kind}'."
        { Type = expressionType; Span = span; Node = node }

    let deserialize (bytes: byte array) =
        try
            use document = JsonDocument.Parse(bytes)
            let root = document.RootElement
            if stringProperty "format" root <> Format then Error [ { Code = "FSA5001"; Stage = Artifact; Message = "Unsupported IR artifact format."; Span = None } ]
            elif (property "irVersion" root).GetInt32() <> Version then Error [ { Code = "FSA5002"; Stage = Artifact; Message = "Incompatible IR artifact version."; Span = None } ]
            elif [ "compilerVersion"; "languageVersion"; "sources"; "dependencies"; "program" ] |> List.exists (fun name -> not (hasProperty name root)) then
                Error [ { Code = "FSA5004"; Stage = Artifact; Message = "IR artifact is missing required metadata."; Span = None } ]
            else
                let sourceElements =
                    if (property "sources" root).ValueKind = JsonValueKind.Array then
                        (property "sources" root).EnumerateArray() |> Seq.toList
                    else []
                let sourceValues = sourceElements |> List.map (fun source -> stringProperty "path" source, stringProperty "sha256" source)
                let validHash (hash: string) =
                    hash.Length = 64
                    && hash |> Seq.forall (fun character -> System.Char.IsDigit character || (character >= 'a' && character <= 'f'))
                let sourcesValid =
                    (property "sources" root).ValueKind = JsonValueKind.Array
                    && sourceElements |> List.forall (fun source -> hasProperty "path" source && hasProperty "sha256" source)
                    && sourceValues |> List.forall (fun (path, hash) -> not (String.IsNullOrWhiteSpace path) && path = normalizePath path && validHash hash)
                    && sourceValues = (sourceValues |> List.distinct |> List.sortBy fst)
                let dependencyElements =
                    if (property "dependencies" root).ValueKind = JsonValueKind.Array then
                        (property "dependencies" root).EnumerateArray() |> Seq.toList
                    else []
                let dependencyValues = dependencyElements |> List.map _.GetString() |> List.map (Option.ofObj >> Option.defaultValue "")
                let versionsValid =
                    not (String.IsNullOrWhiteSpace(stringProperty "compilerVersion" root))
                    && not (String.IsNullOrWhiteSpace(stringProperty "languageVersion" root))
                if not versionsValid || not sourcesValid || dependencyValues |> List.exists String.IsNullOrWhiteSpace || dependencyValues <> (dependencyValues |> List.distinct |> List.sort) then
                    Error [ { Code = "FSA5005"; Stage = Artifact; Message = "IR artifact metadata is invalid or non-canonical."; Span = None } ]
                else
                    let programElement = property "program" root
                    let readImportedRecord (item: JsonElement) =
                        ({ Name = stringProperty "name" item; ModulePath = stringProperty "modulePath" item; MemberName = stringProperty "memberName" item; IsUnion = (property "union" item).GetBoolean()
                           Fields = (property "fields" item).EnumerateArray() |> Seq.map (fun field -> stringProperty "name" field, readType (property "type" field)) |> Seq.toList
                           Span = readSpan (property "span" item) }: Lir.ImportedRecord)
                    let importedRecords = (property "importedRecords" programElement).EnumerateArray() |> Seq.map (fun item ->
                        readImportedRecord item) |> Seq.toList
                    let types = (property "types" programElement).EnumerateArray() |> Seq.map (fun item ->
                        let importElement = property "import" item
                        ({ Id = Lir.SymbolId((property "id" item).GetInt32()); Name = stringProperty "name" item
                           IsRecursive = (property "recursive" item).GetBoolean(); IsPure = (match item.TryGetProperty("pure") with true,value -> value.GetBoolean() | _ -> false); Definition = readType (property "definition" item)
                           Import = if importElement.ValueKind = JsonValueKind.Null then None else Some(readImportedRecord importElement)
                           Span = readSpan (property "span" item) }: Lir.TypeDeclaration)) |> Seq.toList
                    let declarations = (property "declarations" programElement).EnumerateArray() |> Seq.map (fun (item: JsonElement) ->
                        let exportElement = property "export" item
                        let export =
                            if exportElement.ValueKind = JsonValueKind.Null then None
                            else Some({ Name = stringProperty "name" exportElement; Span = readSpan (property "span" exportElement) }: Lir.ExportMetadata)
                        ({ Id = Lir.SymbolId((property "id" item).GetInt32()); Name = stringProperty "name" item; Type = readType (property "type" item)
                           IsRecursive = (property "recursive" item).GetBoolean(); IsExported = (property "exported" item).GetBoolean(); IsPure = (match item.TryGetProperty("pure") with true,value -> value.GetBoolean() | _ -> false)
                           ExternalArity = (match item.TryGetProperty("externalArity") with true,value when value.ValueKind<>JsonValueKind.Null -> Some(value.GetInt32()) | _ -> None)
                           Export = export
                           Span = readSpan (property "span" item); Value = readExpression (property "value" item) }: Lir.Declaration)) |> Seq.toList
                    let functions = (property "functions" programElement).EnumerateArray() |> Seq.map (fun (item: JsonElement) ->
                        ({ Id = Lir.FunctionId((property "id" item).GetInt32())
                           Parameter = Lir.SymbolId((property "parameter" item).GetInt32())
                           Name = stringProperty "name" item
                           ParameterType = readType (property "parameterType" item)
                           Captures = (property "captures" item).EnumerateArray() |> Seq.map (fun capture -> Lir.SymbolId(capture.GetInt32())) |> Seq.toList
                           ReturnType = readType (property "returnType" item)
                           Span = readSpan (property "span" item)
                           Body = readExpression (property "body" item) }: Lir.Function)) |> Seq.toList
                    let entryElement = property "entry" programElement
                    let program : Lir.Program =
                        { Types = types
                          ImportedRecords = importedRecords
                          Functions = functions
                          Declarations = declarations
                          Entry = if entryElement.ValueKind = JsonValueKind.Null then None else Some(readExpression entryElement) }
                    LirValidation.validate program
        with ex -> Error [ { Code = "FSA5003"; Stage = Artifact; Message = ex.Message; Span = None } ]
