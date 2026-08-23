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
            | TRecord fields ->
                writer.WriteString("kind", "record")
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

    let rec private writePattern (writer: Utf8JsonWriter) (value: Lir.Pattern) =
        writer.WriteStartObject(); writer.WritePropertyName("type"); writeType writer value.Type
        writer.WritePropertyName("span"); writeSpan writer value.Span
        match value.Node with
        | Lir.Wildcard -> writer.WriteString("kind", "wildcard")
        | Lir.Binding(id, name) -> writer.WriteString("kind", "binding"); writer.WriteNumber("symbol", Lir.symbolNumber id); writer.WriteString("name", name)
        | Lir.LiteralPattern constant -> writer.WriteString("kind", "literal"); writeConstantValue writer constant
        | Lir.EmptyList -> writer.WriteString("kind", "empty-list")
        | Lir.NonePattern -> writer.WriteString("kind", "none")
        | Lir.SomePattern item -> writer.WriteString("kind", "some"); writer.WritePropertyName("item"); writePattern writer item
        | Lir.ConsPattern(head, tail) ->
            writer.WriteString("kind", "cons"); writer.WritePropertyName("head"); writePattern writer head
            writer.WritePropertyName("tail"); writePattern writer tail
        | Lir.TuplePattern items ->
            writer.WriteString("kind", "tuple"); writer.WriteStartArray("items"); items |> List.iter (writePattern writer); writer.WriteEndArray()
        | Lir.RecordPattern fields ->
            writer.WriteString("kind", "record"); writer.WriteStartArray("fields")
            for name, item in fields do
                writer.WriteStartObject(); writer.WriteString("name", name); writer.WritePropertyName("pattern"); writePattern writer item; writer.WriteEndObject()
            writer.WriteEndArray()
        | Lir.UnionPattern(typeName, caseName, payload) ->
            writer.WriteString("kind", "union"); writer.WriteString("typeName", typeName); writer.WriteString("caseName", caseName)
            match payload with Some item -> writer.WritePropertyName("payload"); writePattern writer item | None -> writer.WriteNull("payload")
        writer.WriteEndObject()

    let rec private writeExpression (writer: Utf8JsonWriter) (value: Lir.Expression) =
        writer.WriteStartObject(); writer.WritePropertyName("type"); writeType writer value.Type
        writer.WritePropertyName("span"); writeSpan writer value.Span
        match value.Node with
        | Lir.Constant constant ->
            writer.WriteString("kind", "constant"); writeConstantValue writer constant
        | Lir.Load id -> writer.WriteString("kind", "load"); writer.WriteNumber("symbol", Lir.symbolNumber id)
        | Lir.MakeClosure(id, name, parameterType, body) ->
            writer.WriteString("kind", "closure"); writer.WriteNumber("symbol", Lir.symbolNumber id); writer.WriteString("name", name)
            writer.WritePropertyName("parameterType"); writeType writer parameterType
            writer.WritePropertyName("body"); writeExpression writer body
        | Lir.CallClosure(fn, argument) ->
            writer.WriteString("kind", "call"); writer.WritePropertyName("function"); writeExpression writer fn
            writer.WritePropertyName("argument"); writeExpression writer argument
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
        | Lir.Fail value -> writer.WriteString("kind", "fail"); writer.WritePropertyName("value"); writeExpression writer value
        | Lir.DecisionTree(target, cases) ->
            writer.WriteString("kind", "decision-tree"); writer.WritePropertyName("target"); writeExpression writer target
            writer.WriteStartArray("cases")
            for case in cases do
                writer.WriteStartObject(); writer.WritePropertyName("pattern"); writePattern writer case.Pattern
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
        writer.WriteString("compilerVersion", "0.0.0"); writer.WriteString("languageVersion", "0.0.0")
        writer.WriteStartArray("sources"); writer.WriteEndArray(); writer.WriteStartArray("dependencies"); writer.WriteEndArray()
        writer.WriteStartObject("program"); writer.WriteStartArray("declarations")
        for declaration in program.Declarations do
            writer.WriteStartObject(); writer.WriteNumber("id", Lir.symbolNumber declaration.Id); writer.WriteString("name", declaration.Name)
            writer.WritePropertyName("type"); writeType writer declaration.Type; writer.WriteBoolean("recursive", declaration.IsRecursive)
            writer.WriteBoolean("exported", declaration.IsExported); writer.WritePropertyName("span"); writeSpan writer declaration.Span
            writer.WritePropertyName("value"); writeExpression writer declaration.Value; writer.WriteEndObject()
        writer.WriteEndArray()
        match program.Entry with Some entry -> writer.WritePropertyName("entry"); writeExpression writer entry | None -> writer.WriteNull("entry")
        writer.WriteEndObject(); writer.WriteEndObject(); writer.Flush(); stream.ToArray()

    let private property (name: string) (element: JsonElement) = element.GetProperty(name)
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

    let rec private readPattern (element: JsonElement) : Lir.Pattern =
        let node =
            match stringProperty "kind" element with
            | "wildcard" -> Lir.Wildcard
            | "binding" -> Lir.Binding(Lir.SymbolId((property "symbol" element).GetInt32()), stringProperty "name" element)
            | "literal" -> Lir.LiteralPattern(readConstant element)
            | "empty-list" -> Lir.EmptyList
            | "none" -> Lir.NonePattern
            | "some" -> Lir.SomePattern(readPattern (property "item" element))
            | "cons" -> Lir.ConsPattern(readPattern (property "head" element), readPattern (property "tail" element))
            | "tuple" -> Lir.TuplePattern((property "items" element).EnumerateArray() |> Seq.map readPattern |> Seq.toList)
            | "record" -> Lir.RecordPattern((property "fields" element).EnumerateArray() |> Seq.map (fun field -> stringProperty "name" field, readPattern (property "pattern" field)) |> Seq.toList)
            | "union" ->
                let payload = property "payload" element
                Lir.UnionPattern(stringProperty "typeName" element, stringProperty "caseName" element, if payload.ValueKind = JsonValueKind.Null then None else Some(readPattern payload))
            | kind -> invalidOp $"Unknown LIR pattern kind '{kind}'."
        { Type = readType (property "type" element); Span = readSpan (property "span" element); Node = node }

    let rec private readExpression (element: JsonElement) : Lir.Expression =
        let expressionType = readType (property "type" element)
        let span = readSpan (property "span" element)
        let node =
            match stringProperty "kind" element with
            | "constant" -> Lir.Constant(readConstant element)
            | "load" -> Lir.Load(Lir.SymbolId((property "symbol" element).GetInt32()))
            | "closure" -> Lir.MakeClosure(Lir.SymbolId((property "symbol" element).GetInt32()), stringProperty "name" element,
                                                           readType (property "parameterType" element), readExpression (property "body" element))
            | "call" -> Lir.CallClosure(readExpression (property "function" element), readExpression (property "argument" element))
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
            | "fail" -> Lir.Fail(readExpression (property "value" element))
            | "decision-tree" ->
                let cases = (property "cases" element).EnumerateArray() |> Seq.map (fun item ->
                    let guard = property "guard" item
                    ({ Pattern = readPattern (property "pattern" item)
                       Guard = if guard.ValueKind = JsonValueKind.Null then None else Some(readExpression guard)
                       Body = readExpression (property "body" item)
                       Span = readSpan (property "span" item) }: Lir.DecisionCase)) |> Seq.toList
                Lir.DecisionTree(readExpression (property "target" element), cases)
            | "range" -> Lir.Range(readExpression (property "start" element), readExpression (property "end" element))
            | "record-update" ->
                let fields = (property "fields" element).EnumerateArray() |> Seq.map (fun item -> stringProperty "name" item, readExpression (property "value" item)) |> Seq.toList
                Lir.RecordUpdate(readExpression (property "target" element), fields)
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
            else
                let programElement = property "program" root
                let declarations = (property "declarations" programElement).EnumerateArray() |> Seq.map (fun (item: JsonElement) ->
                    ({ Id = Lir.SymbolId((property "id" item).GetInt32()); Name = stringProperty "name" item; Type = readType (property "type" item)
                       IsRecursive = (property "recursive" item).GetBoolean(); IsExported = (property "exported" item).GetBoolean()
                       Span = readSpan (property "span" item); Value = readExpression (property "value" item) }: Lir.Declaration)) |> Seq.toList
                let entryElement = property "entry" programElement
                let program : Lir.Program =
                    { Declarations = declarations
                      Entry = if entryElement.ValueKind = JsonValueKind.Null then None else Some(readExpression entryElement) }
                LirValidation.validate program
        with ex -> Error [ { Code = "FSA5003"; Stage = Artifact; Message = ex.Message; Span = None } ]
