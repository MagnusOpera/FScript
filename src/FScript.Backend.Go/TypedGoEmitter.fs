namespace FScript.Compiler

open System
open System.Collections.Generic
open System.Security.Cryptography
open System.Text
open FScript.Language

module TypedGoEmitter =
    type private NamedRepresentation =
        | LocalNamed of recursive: bool
        | ImportedNamed of alias: string * memberName: string

    let private namedRepresentations = Threading.AsyncLocal<Map<string, NamedRepresentation>>()
    let private tryNamedRepresentation name =
        let current = namedRepresentations.Value
        if isNull (box current) then None else Map.tryFind name current
    let private symbolName prefix id = $"{prefix}{Lir.symbolNumber id}"
    let private indent (value: string) = value.Replace("\n", "\n\t")
    let private key value = Types.typeToString value
    let private suffix value =
        SHA256.HashData(Encoding.UTF8.GetBytes(key value)) |> Convert.ToHexString |> fun hash -> hash.Substring(0, 10).ToLowerInvariant()

    let private identifier (value: string) =
        let rendered =
            value
            |> Seq.map (fun character -> if Char.IsLetterOrDigit character || character = '_' then character else '_')
            |> Seq.toArray
            |> String
        if String.IsNullOrEmpty rendered then "value"
        elif Char.IsDigit rendered.[0] then "_" + rendered
        else rendered

    let private fieldName value = "Field_" + identifier value
    let private caseField value = "Case_" + identifier value
    let private goCondition (value: string) =
        if value.Length >= 2 && value.[0] = '(' && value.[value.Length - 1] = ')' then value.Substring(1, value.Length - 2) else value

    let rec private goType = function
        | TUnit -> "struct{}"
        | TInt -> "int64"
        | TFloat -> "float64"
        | TBool -> "bool"
        | TString -> "string"
        | TTypeToken -> "typeToken"
        | TFun(argument, result) -> $"func({goType argument}) {goType result}"
        | TList item -> $"[]{goType item}"
        | TOption _ as value -> $"option_{suffix value}"
        | TTuple _ as value -> $"tuple_{suffix value}"
        | TRecord _ as value -> $"record_{suffix value}"
        | TMap _ as value -> $"map_{suffix value}"
        | TUnion _ as value -> $"union_{suffix value}"
        | TNamed "Environment" -> "environment"
        | TNamed name ->
            match tryNamedRepresentation name with
            | Some(LocalNamed true) -> $"*named_{identifier name}"
            | Some(ImportedNamed(alias, memberName)) -> $"{alias}.{memberName}"
            | _ -> $"named_{identifier name}"
        | value -> invalidArg "value" $"Typed Go vertical slice cannot represent {Types.typeToString value}."

    let private constant = function
        | Lir.Unit -> "struct{}{}"
        | Lir.Int value -> $"int64({value})"
        | Lir.Float value ->
            let rendered = value.ToString("R", Globalization.CultureInfo.InvariantCulture)
            if rendered.Contains(".") || rendered.Contains("e", StringComparison.OrdinalIgnoreCase) then rendered else rendered + ".0"
        | Lir.Bool true -> "true"
        | Lir.Bool false -> "false"
        | Lir.String value -> Text.Json.JsonSerializer.Serialize(value)

    let rec private supportedType = function
        | TUnit | TInt | TFloat | TBool | TString | TTypeToken | TNamed _ -> true
        | TFun(argument, result) -> supportedType argument && supportedType result
        | TList item | TOption item -> supportedType item
        | TTuple items -> List.forall supportedType items
        | TRecord fields -> fields |> Map.forall (fun _ value -> supportedType value)
        | TMap(keyType, valueType) -> supportedType keyType && supportedType valueType
        | TUnion(_, cases) -> cases |> Map.forall (fun _ value -> Option.forall supportedType value)
        | TTask _ | TVar _ -> false

    let private typedIntrinsics =
        set [ "ignore"; "Int.tryParse"; "Float.tryParse"; "Bool.tryParse"; "Int.toString"; "Float.toString"; "Bool.toString"
              "String.replace"; "String.indexOf"; "String.toLower"; "String.toUpper"; "String.substring"; "String.concat"; "String.split"; "String.endsWith"
              "List.empty"; "List.map"; "List.iter"; "List.choose"; "List.collect"; "List.exists"; "List.contains"; "List.rev"; "List.distinct"
              "List.fold"; "List.filter"; "List.length"; "List.tryFind"; "List.tryGet"; "List.tryItem"; "List.tryHead"; "List.tail"; "List.append"
              "Option.defaultValue"; "Option.defaultWith"; "Option.isNone"; "Option.isSome"; "Option.map"
              "Map.empty"; "Map.tryGet"; "Map.containsKey"; "Map.add"; "Map.ofList"; "Map.fold"; "Map.count"; "Map.filter"; "Map.choose"; "Map.map"; "Map.iter"; "Map.remove"
              "Console.readLine"; "Console.writeLine"; "Fs.readText" ]

    let private typedHostExternals = set [ "Console.readLine"; "Console.writeLine"; "Fs.readText" ]

    let rec private supportedExpression (functions: Map<Lir.FunctionId, Lir.Function>) (expression: Lir.Expression) =
        let supported = supportedExpression functions
        supportedType expression.Type
        && match expression.Node with
           | Lir.Constant _ | Lir.Load _ | Lir.NoneValue | Lir.RuntimeContext | Lir.TypeToken _ -> true
           | Lir.MakeClosure(id, _) -> functions |> Map.tryFind id |> Option.exists (fun fn -> supported fn.Body)
           | Lir.CallClosure(left, right) | Lir.PipeClosure(left, right) | Lir.Cons(left, right)
           | Lir.Append(left, right) | Lir.Range(left, right) | Lir.IndexGet(left, right) -> supported left && supported right
           | Lir.Branch(a, b, c) -> supported a && supported b && supported c
           | Lir.Operation(_, values) | Lir.Tuple values | Lir.List values -> List.forall supported values
           | Lir.Record fields -> fields |> List.forall (snd >> supported)
           | Lir.FieldGet(target, _) -> supported target
           | Lir.SomeValue value | Lir.Fail value -> supported value
           | Lir.Let(_, _, _, binding, body) -> supported binding && supported body
           | Lir.LetRecursiveGroup(bindings, body) -> bindings |> List.forall (fun (_, _, valueType, value) -> supportedType valueType && supported value) && supported body
           | Lir.RecordUpdate(target, fields) -> supported target && (fields |> List.forall (snd >> supported))
           | Lir.RecordConvert(target, targetType) -> supportedType targetType && supported target
           | Lir.UnionCase(_, _, payload) -> payload |> Option.forall supported
           | Lir.Map entries -> entries |> List.forall (function Lir.KeyValue(k, v) -> supported k && supported v | Lir.Spread value -> supported value)
           | Lir.DecisionTree(target, cases) ->
               supported target
               && (cases |> List.forall (fun case -> case.Guard |> Option.forall supported && supported case.Body))
           | Lir.Interpolate parts -> parts |> List.forall (function Lir.TextPart _ -> true | Lir.ValuePart value -> supported value)
           | Lir.ForEach(_, _, itemType, source, body) -> supportedType itemType && supported source && supported body
           | Lir.Intrinsic name -> Set.contains name typedIntrinsics
           | Lir.HostExternal(name, _) -> Set.contains name typedHostExternals
           | Lir.TargetImport _ -> true

    let canEmit (program: Lir.Program) =
        let functions = program.Functions |> List.map (fun item -> item.Id, item) |> Map.ofList
        let validImportedRecord (record: Lir.ImportedRecord) =
            not record.IsUnion
            && not record.Fields.IsEmpty
            && (record.Fields |> List.forall (fun (name, fieldType) ->
                not (String.IsNullOrEmpty name)
                && Char.IsUpper name.[0]
                && match fieldType with TUnit | TInt | TFloat | TBool | TString -> true | _ -> false))
        (program.ImportedRecords |> List.forall validImportedRecord)
        && (program.Declarations |> List.forall (fun item -> supportedType item.Type && supportedExpression functions item.Value))
        && program.Entry |> Option.forall (supportedExpression functions)

    let generate moduleName (program: Lir.Program) =
        let rec targetModules (expression: Lir.Expression) =
            [ match expression.Node with
              | Lir.TargetImport(modulePath, _) -> yield modulePath
              | Lir.CallClosure(left, right) | Lir.PipeClosure(left, right) | Lir.Cons(left, right) | Lir.Append(left, right) | Lir.Range(left, right) | Lir.IndexGet(left, right) -> yield! targetModules left; yield! targetModules right
              | Lir.Branch(a, b, c) -> yield! targetModules a; yield! targetModules b; yield! targetModules c
              | Lir.Operation(_, values) | Lir.Tuple values | Lir.List values -> for value in values do yield! targetModules value
              | Lir.Record fields | Lir.RecordUpdate(_, fields) -> for _, value in fields do yield! targetModules value
              | Lir.FieldGet(value, _) | Lir.SomeValue value | Lir.Fail value | Lir.RecordConvert(value, _) -> yield! targetModules value
              | Lir.Let(_, _, _, binding, body) -> yield! targetModules binding; yield! targetModules body
              | Lir.LetRecursiveGroup(bindings, body) -> for _, _, _, value in bindings do yield! targetModules value; yield! targetModules body
              | Lir.DecisionTree(target, cases) -> yield! targetModules target; for case in cases do yield! case.Guard |> Option.map targetModules |> Option.defaultValue []; yield! targetModules case.Body
              | Lir.Interpolate parts -> for part in parts do match part with Lir.ValuePart value -> yield! targetModules value | _ -> ()
              | Lir.UnionCase(_, _, payload) -> yield! payload |> Option.map targetModules |> Option.defaultValue []
              | Lir.Map entries -> for entry in entries do match entry with Lir.KeyValue(key, value) -> yield! targetModules key; yield! targetModules value | Lir.Spread value -> yield! targetModules value
              | Lir.ForEach(_, _, _, source, body) -> yield! targetModules source; yield! targetModules body
              | _ -> () ]
        let importModules =
            [ yield! program.ImportedRecords |> List.map _.ModulePath
              for declaration in program.Declarations do yield! targetModules declaration.Value
              yield! program.Entry |> Option.map targetModules |> Option.defaultValue [] ]
            |> List.distinct
            |> List.sort
            |> List.mapi (fun index path -> path, $"goimport{index}")
            |> Map.ofList
        namedRepresentations.Value <-
            program.Types
            |> List.map (fun declaration ->
                let representation =
                    match declaration.Import with
                    | Some imported -> ImportedNamed(importModules.[imported.ModulePath], imported.MemberName)
                    | None -> LocalNamed declaration.IsRecursive
                declaration.Name, representation)
            |> Map.ofList
        let functions = program.Functions |> List.map (fun item -> item.Id, item) |> Map.ofList
        let namedDefinitions = program.Types |> List.map (fun item -> item.Name, item.Definition) |> Map.ofList
        let namedRepresentation name = tryNamedRepresentation name
        let emittedField ownerType name =
            match ownerType with
            | TNamed typeName -> match namedRepresentation typeName with Some(ImportedNamed _) -> identifier name | _ -> fieldName name
            | _ -> fieldName name
        let types = HashSet<Type>()
        types.Add(TOption TString) |> ignore
        let rec collectType value =
            if types.Add value then
                match value with
                | TFun(argument, result) -> collectType argument; collectType result
                | TList item | TOption item -> collectType item
                | TTuple items -> List.iter collectType items
                | TRecord fields -> fields |> Map.iter (fun _ value -> collectType value)
                | TMap(keyType, valueType) -> collectType keyType; collectType valueType
                | TUnion(_, cases) -> cases |> Map.iter (fun _ value -> Option.iter collectType value)
                | TNamed name ->
                    program.Types
                    |> List.tryFind (fun declaration -> declaration.Name = name)
                    |> Option.iter (fun declaration -> collectType declaration.Definition)
                | _ -> ()
        let rec collectExpression (expression: Lir.Expression) =
            collectType expression.Type
            match expression.Node with
            | Lir.MakeClosure(id, _) -> let fn = functions.[id] in collectType fn.ParameterType; collectType fn.ReturnType; collectExpression fn.Body
            | Lir.CallClosure(left, right) | Lir.PipeClosure(left, right) | Lir.Cons(left, right)
            | Lir.Append(left, right) | Lir.Range(left, right) | Lir.IndexGet(left, right) -> collectExpression left; collectExpression right
            | Lir.Branch(a, b, c) -> collectExpression a; collectExpression b; collectExpression c
            | Lir.Operation(_, values) | Lir.Tuple values | Lir.List values -> List.iter collectExpression values
            | Lir.Record fields -> fields |> List.iter (snd >> collectExpression)
            | Lir.FieldGet(target, _) -> collectExpression target
            | Lir.SomeValue value | Lir.Fail value -> collectExpression value
            | Lir.Let(_, _, _, binding, body) -> collectExpression binding; collectExpression body
            | Lir.LetRecursiveGroup(bindings, body) -> bindings |> List.iter (fun (_, _, valueType, value) -> collectType valueType; collectExpression value); collectExpression body
            | Lir.DecisionTree(target, cases) -> collectExpression target; cases |> List.iter (fun case -> case.Guard |> Option.iter collectExpression; collectExpression case.Body)
            | Lir.RecordUpdate(target, fields) -> collectExpression target; fields |> List.iter (snd >> collectExpression)
            | Lir.RecordConvert(target, targetType) -> collectExpression target; collectType targetType
            | Lir.UnionCase(_, _, payload) -> payload |> Option.iter collectExpression
            | Lir.Map entries -> entries |> List.iter (function Lir.KeyValue(k, v) -> collectExpression k; collectExpression v | Lir.Spread value -> collectExpression value)
            | Lir.Interpolate parts -> parts |> List.iter (function Lir.TextPart _ -> () | Lir.ValuePart value -> collectExpression value)
            | Lir.ForEach(_, _, itemType, source, body) -> collectType itemType; collectExpression source; collectExpression body
            | Lir.TypeToken valueType -> collectType valueType
            | _ -> ()
        program.Declarations |> List.iter (fun item -> collectType item.Type; collectExpression item.Value)
        program.Entry |> Option.iter collectExpression

        let typeDeclarations = StringBuilder()
        typeDeclarations.AppendLine("type typeToken struct{ Value string }") |> ignore
        typeDeclarations.AppendLine("type environment struct {") |> ignore
        typeDeclarations.AppendLine($"\tScriptName {goType (TOption TString)}") |> ignore
        typeDeclarations.AppendLine("\tArguments  []string") |> ignore
        typeDeclarations.AppendLine("}") |> ignore
        let appendStructFields fields =
            let width = fields |> List.map (fst >> String.length) |> List.fold max 0
            fields
            |> List.iter (fun (name, fieldType) ->
                let padding = String(' ', width - name.Length + 1)
                typeDeclarations.AppendLine($"\t{name}{padding}{goType fieldType}") |> ignore)
        let appendFields fields = fields |> Map.toList |> List.map (fun (name, fieldType) -> fieldName name, fieldType) |> appendStructFields
        let appendUnionCases cases =
            ("Tag", TString) :: (cases |> Map.toList |> List.choose (fun (caseName, payload) -> payload |> Option.map (fun payloadType -> caseField caseName, payloadType)))
            |> appendStructFields
        program.Types
        |> List.filter (fun declaration -> declaration.Name <> "Environment" && declaration.Import.IsNone && types.Contains(TNamed declaration.Name))
        |> List.sortBy _.Name
        |> List.iter (fun declaration ->
            match declaration.Definition with
            | TRecord fields ->
                typeDeclarations.AppendLine($"type named_{identifier declaration.Name} struct {{") |> ignore
                appendFields fields
                typeDeclarations.AppendLine("}") |> ignore
            | TUnion(_, cases) ->
                typeDeclarations.AppendLine($"type named_{identifier declaration.Name} struct {{") |> ignore
                appendUnionCases cases
                typeDeclarations.AppendLine("}") |> ignore
            | _ -> ())
        for value in types |> Seq.sortBy key do
            match value with
            | TOption item ->
                typeDeclarations.AppendLine($"type {goType value} struct {{") |> ignore
                typeDeclarations.AppendLine("\tHasValue bool") |> ignore
                typeDeclarations.AppendLine($"\tValue    {goType item}") |> ignore
                typeDeclarations.AppendLine("}") |> ignore
            | TTuple items ->
                typeDeclarations.AppendLine($"type {goType value} struct {{") |> ignore
                items |> List.mapi (fun index item -> $"Item{index}", item) |> appendStructFields
                typeDeclarations.AppendLine("}") |> ignore
            | TRecord fields ->
                typeDeclarations.AppendLine($"type {goType value} struct {{") |> ignore
                appendFields fields
                typeDeclarations.AppendLine("}") |> ignore
            | TMap(keyType, valueType) ->
                typeDeclarations.AppendLine($"type entry_{suffix value} struct {{") |> ignore
                appendStructFields [ "Key", keyType; "Value", valueType ]
                typeDeclarations.AppendLine("}") |> ignore
                typeDeclarations.AppendLine($"type {goType value} struct {{") |> ignore
                typeDeclarations.AppendLine($"\tEntries []entry_{suffix value}") |> ignore
                typeDeclarations.AppendLine("}") |> ignore
            | TUnion(_, cases) ->
                typeDeclarations.AppendLine($"type {goType value} struct {{") |> ignore
                appendUnionCases cases
                typeDeclarations.AppendLine("}") |> ignore
            | _ -> ()

        let formatterNames = Dictionary<Type, string>()
        let formatterBodies = ResizeArray<string>()
        let rec formatter value =
            match formatterNames.TryGetValue value with
            | true, name -> name
            | _ ->
                let name = $"format_{suffix value}"
                formatterNames.Add(value, name)
                let body =
                    match value with
                    | TUnit -> "return \"()\""
                    | TInt -> "return strconv.FormatInt(value, 10)"
                    | TFloat -> "if math.IsInf(value, 1) {\n\t\treturn \"Infinity\"\n\t}\n\tif math.IsInf(value, -1) {\n\t\treturn \"-Infinity\"\n\t}\n\treturn strconv.FormatFloat(value, 'g', -1, 64)"
                    | TBool -> "return strconv.FormatBool(value)"
                    | TString -> "return strconv.Quote(value)"
                    | TTypeToken -> "return \"<type \" + value.Value + \">\""
                    | TFun _ -> "return \"<function>\""
                    | TList item ->
                        let itemFormatter = formatter item
                        $"result := \"[\"\n\tfor index, item := range value {{\n\t\tif index > 0 {{\n\t\t\tresult += \";\"\n\t\t}}\n\t\tresult += {itemFormatter}(item)\n\t}}\n\treturn result + \"]\""
                    | TOption item ->
                        let itemFormatter = formatter item
                        $"if !value.HasValue {{\n\t\treturn \"None\"\n\t}}\n\treturn \"Some \" + {itemFormatter}(value.Value)"
                    | TTuple items ->
                        let parts = items |> List.mapi (fun index item -> $"{formatter item}(value.Item{index})")
                        match parts with
                        | [] -> "return \"()\""
                        | _ -> "return \"(\" + " + (parts |> List.reduce (fun left right -> left + " + \", \" + " + right)) + " + \")\""
                    | TRecord fields ->
                        let parts = fields |> Map.toList |> List.map (fun (field, fieldType) -> Text.Json.JsonSerializer.Serialize(field + " = ") + " + " + $"{formatter fieldType}(value.{fieldName field})")
                        match parts with
                        | [] -> "return \"{}\""
                        | _ -> "return \"{ \" + " + (parts |> List.reduce (fun left right -> left + " + \"; \" + " + right)) + " + \" }\""
                    | TMap(keyType, valueType) ->
                        let keyFormatter, valueFormatter = formatter keyType, formatter valueType
                        $"result := \"map {{ \"\n\tfor index, entry := range value.Entries {{\n\t\tif index > 0 {{\n\t\t\tresult += \"; \"\n\t\t}}\n\t\tresult += {keyFormatter}(entry.Key) + \" => \" + {valueFormatter}(entry.Value)\n\t}}\n\treturn result + \" }}\""
                    | TUnion(_, cases) ->
                        let clauses =
                            cases |> Map.toList |> List.map (fun (caseName, payload) ->
                                match payload with
                                | None -> $"case {Text.Json.JsonSerializer.Serialize(caseName)}:\n\t\treturn {Text.Json.JsonSerializer.Serialize(caseName)}"
                                | Some payloadType ->
                                    let label = Text.Json.JsonSerializer.Serialize(caseName + " ")
                                    $"case {Text.Json.JsonSerializer.Serialize(caseName)}:\n\t\treturn {label} + {formatter payloadType}(value.{caseField caseName})")
                        let renderedClauses = String.concat "\n\t" clauses
                        $"switch value.Tag {{\n\t{renderedClauses}\n\t}}\n\treturn \"<invalid union>\""
                    | TNamed "Environment" -> "return \"<environment>\""
                    | TNamed name ->
                        match Map.tryFind name namedDefinitions with
                        | Some(TRecord fields) ->
                            let parts = fields |> Map.toList |> List.map (fun (field, fieldType) -> Text.Json.JsonSerializer.Serialize(field + " = ") + " + " + $"{formatter fieldType}(value.{emittedField (TNamed name) field})")
                            let rendered = match parts with [] -> "\"{}\"" | _ -> "\"{ \" + " + (parts |> List.reduce (fun left right -> left + " + \"; \" + " + right)) + " + \" }\""
                            match namedRepresentation name with
                            | Some(LocalNamed true) -> $"if value == nil {{\n\t\treturn \"<nil>\"\n\t}}\n\treturn {rendered}"
                            | _ -> $"return {rendered}"
                        | Some(TUnion(_, cases)) ->
                            let clauses = cases |> Map.toList |> List.map (fun (caseName, payload) ->
                                match payload with
                                | None -> $"case {Text.Json.JsonSerializer.Serialize(caseName)}:\n\t\treturn {Text.Json.JsonSerializer.Serialize(caseName)}"
                                | Some payloadType ->
                                    let label = Text.Json.JsonSerializer.Serialize(caseName + " ")
                                    $"case {Text.Json.JsonSerializer.Serialize(caseName)}:\n\t\treturn {label} + {formatter payloadType}(value.{caseField caseName})")
                            let renderedClauses = String.concat "\n\t" clauses
                            let switchBody = $"switch value.Tag {{\n\t{renderedClauses}\n\t}}\n\treturn \"<invalid union>\""
                            match namedRepresentation name with
                            | Some(LocalNamed true) -> $"if value == nil {{\n\t\treturn \"<nil>\"\n\t}}\n\t{switchBody}"
                            | _ -> switchBody
                        | _ -> "return \"<value>\""
                    | other -> invalidArg "value" $"No typed formatter for {Types.typeToString other}."
                formatterBodies.Add($"func {name}(value {goType value}) string {{\n\t{body}\n}}")
                name

        let equalityNames = Dictionary<Type, string>()
        let equalityBodies = ResizeArray<string>()
        let rec equality value =
            match equalityNames.TryGetValue value with
            | true, name -> name
            | _ ->
                let name = $"equal_{suffix value}"
                equalityNames.Add(value, name)
                let body =
                    match value with
                    | TUnit -> "return true"
                    | TInt | TFloat | TBool | TString -> "return left == right"
                    | TTypeToken -> "return left.Value == right.Value"
                    | TFun _ -> "return false"
                    | TList item ->
                        let itemEquality = equality item
                        $"if len(left) != len(right) {{\n\t\treturn false\n\t}}\n\tfor index := range left {{\n\t\tif !{itemEquality}(left[index], right[index]) {{\n\t\t\treturn false\n\t\t}}\n\t}}\n\treturn true"
                    | TOption item ->
                        let itemEquality = equality item
                        $"if left.HasValue != right.HasValue {{\n\t\treturn false\n\t}}\n\treturn !left.HasValue || {itemEquality}(left.Value, right.Value)"
                    | TTuple items ->
                        match items |> List.mapi (fun index item -> $"{equality item}(left.Item{index}, right.Item{index})") with
                        | [] -> "return true"
                        | checks -> "return " + String.concat " && " checks
                    | TRecord fields ->
                        match fields |> Map.toList |> List.map (fun (field, fieldType) -> $"{equality fieldType}(left.{fieldName field}, right.{fieldName field})") with
                        | [] -> "return true"
                        | checks -> "return " + String.concat " && " checks
                    | TMap(keyType, valueType) ->
                        let keyEquality, valueEquality = equality keyType, equality valueType
                        $"if len(left.Entries) != len(right.Entries) {{\n\t\treturn false\n\t}}\n\tfor index := range left.Entries {{\n\t\tif !{keyEquality}(left.Entries[index].Key, right.Entries[index].Key) || !{valueEquality}(left.Entries[index].Value, right.Entries[index].Value) {{\n\t\t\treturn false\n\t\t}}\n\t}}\n\treturn true"
                    | TUnion(_, cases) ->
                        let clauses = cases |> Map.toList |> List.choose (fun (caseName, payload) -> payload |> Option.map (fun payloadType -> $"case {Text.Json.JsonSerializer.Serialize(caseName)}:\n\t\treturn {equality payloadType}(left.{caseField caseName}, right.{caseField caseName})"))
                        let renderedClauses = String.concat "\n\t" clauses
                        $"if left.Tag != right.Tag {{\n\t\treturn false\n\t}}\n\tswitch left.Tag {{\n\t{renderedClauses}\n\t}}\n\treturn true"
                    | TNamed "Environment" -> "return left.ScriptName.HasValue == right.ScriptName.HasValue && left.ScriptName.Value == right.ScriptName.Value"
                    | TNamed name ->
                        match Map.tryFind name namedDefinitions with
                        | Some(TRecord fields) ->
                            let checks = fields |> Map.toList |> List.map (fun (field, fieldType) -> let emitted = emittedField (TNamed name) field in $"{equality fieldType}(left.{emitted}, right.{emitted})")
                            let result = if checks.IsEmpty then "true" else String.concat " && " checks
                            match namedRepresentation name with
                            | Some(LocalNamed true) -> $"if left == nil || right == nil {{\n\t\treturn left == right\n\t}}\n\treturn {result}"
                            | _ -> $"return {result}"
                        | Some(TUnion(_, cases)) ->
                            let clauses = cases |> Map.toList |> List.choose (fun (caseName, payload) -> payload |> Option.map (fun payloadType -> $"case {Text.Json.JsonSerializer.Serialize(caseName)}:\n\t\treturn {equality payloadType}(left.{caseField caseName}, right.{caseField caseName})"))
                            let renderedClauses = String.concat "\n\t" clauses
                            let comparison = $"if left.Tag != right.Tag {{\n\t\treturn false\n\t}}\n\tswitch left.Tag {{\n\t{renderedClauses}\n\t}}\n\treturn true"
                            match namedRepresentation name with
                            | Some(LocalNamed true) -> $"if left == nil || right == nil {{\n\t\treturn left == right\n\t}}\n\t{comparison}"
                            | _ -> comparison
                        | _ -> "return left == right"
                    | other -> invalidArg "value" $"No typed equality for {Types.typeToString other}."
                equalityBodies.Add($"func {name}(left {goType value}, right {goType value}) bool {{\n\t{body}\n}}")
                name

        let names = program.Declarations |> List.map (fun item -> item.Id, symbolName "value" item.Id) |> Map.ofList
        let recordFields = function
            | TRecord fields -> fields
            | TNamed name ->
                match Map.tryFind name namedDefinitions with
                | Some(TRecord fields) -> fields
                | _ -> invalidOp $"Named type '{name}' is not a record."
            | value -> invalidOp $"Type '{Types.typeToString value}' is not a record."
        let unionCases = function
            | TUnion(_, cases) -> cases
            | TNamed name ->
                match Map.tryFind name namedDefinitions with
                | Some(TUnion(_, cases)) -> cases
                | _ -> invalidOp $"Named type '{name}' is not a union."
            | value -> invalidOp $"Type '{Types.typeToString value}' is not a union."
        let intrinsic (name: string) (intrinsicType: Type) =
            let rec curry index currentType arguments body =
                match currentType with
                | TFun(argumentType, resultType) ->
                    let argument = $"intrinsicArg{index}"
                    let inner = curry (index + 1) resultType (arguments @ [ argument ]) body
                    $"func({argument} {goType argumentType}) {goType resultType} {{\n\treturn {indent inner}\n}}"
                | resultType -> body arguments resultType
            let build body = curry 0 intrinsicType [] body
            let iife resultType body = $"func() {goType resultType} {{\n\t{indent body}\n}}()"
            let sortedMap resultType setup =
                iife resultType $"{setup}\nfor outer := 1; outer < len(result.Entries); outer++ {{\n\tfor inner := outer; inner > 0 && result.Entries[inner].Key < result.Entries[inner-1].Key; inner-- {{\n\t\tresult.Entries[inner], result.Entries[inner-1] = result.Entries[inner-1], result.Entries[inner]\n\t}}\n}}\nreturn result"
            match name, intrinsicType with
            | "List.empty", TList _ -> $"{goType intrinsicType}{{}}"
            | "Map.empty", TMap _ -> $"{goType intrinsicType}{{}}"
            | "ignore", _ -> build (fun _ _ -> "struct{}{}")
            | "Int.toString", _ -> build (fun args _ -> $"strconv.FormatInt({args.[0]}, 10)")
            | "Float.toString", _ -> build (fun args _ -> $"strconv.FormatFloat({args.[0]}, 'g', -1, 64)")
            | "Bool.toString", _ -> build (fun args _ -> $"strconv.FormatBool({args.[0]})")
            | "Int.tryParse", TFun(_, TOption TInt) -> build (fun args resultType -> iife resultType $"value, err := strconv.ParseInt({args.[0]}, 10, 64)\nif err != nil {{\n\treturn {goType resultType}{{}}\n}}\nreturn {goType resultType}{{HasValue: true, Value: value}}")
            | "Float.tryParse", TFun(_, TOption TFloat) -> build (fun args resultType -> iife resultType $"value, err := strconv.ParseFloat({args.[0]}, 64)\nif err != nil {{\n\treturn {goType resultType}{{}}\n}}\nreturn {goType resultType}{{HasValue: true, Value: value}}")
            | "Bool.tryParse", TFun(_, TOption TBool) -> build (fun args resultType -> iife resultType $"if strings.EqualFold({args.[0]}, \"true\") {{\n\treturn {goType resultType}{{HasValue: true, Value: true}}\n}}\nif strings.EqualFold({args.[0]}, \"false\") {{\n\treturn {goType resultType}{{HasValue: true, Value: false}}\n}}\nreturn {goType resultType}{{}}")
            | "String.toLower", _ -> build (fun args _ -> $"strings.ToLower({args.[0]})")
            | "String.toUpper", _ -> build (fun args _ -> $"strings.ToUpper({args.[0]})")
            | "String.replace", _ -> build (fun args _ -> $"strings.ReplaceAll({args.[2]}, {args.[0]}, {args.[1]})")
            | "String.endsWith", _ -> build (fun args _ -> $"strings.HasSuffix({args.[1]}, {args.[0]})")
            | "String.concat", _ -> build (fun args _ -> $"strings.Join({args.[1]}, {args.[0]})")
            | "String.split", _ -> build (fun args _ -> $"strings.Split({args.[1]}, {args.[0]})")
            | "String.indexOf", TFun(_, TFun(_, TOption TInt)) ->
                build (fun args resultType -> iife resultType $"index := strings.Index({args.[1]}, {args.[0]})\nif index < 0 {{\n\treturn {goType resultType}{{}}\n}}\nreturn {goType resultType}{{HasValue: true, Value: int64(utf8.RuneCountInString({args.[1]}[:index]))}}")
            | "String.substring", TFun(_, TFun(_, TFun(_, TOption TString))) ->
                build (fun args resultType -> iife resultType $"start, length := {args.[0]}, {args.[1]}\nvalue := []rune({args.[2]})\nif start < 0 || length < 0 || start+length > int64(len(value)) {{\n\treturn {goType resultType}{{}}\n}}\nreturn {goType resultType}{{HasValue: true, Value: string(value[start : start+length])}}")
            | "List.length", TFun(TList _, _) -> build (fun args _ -> $"int64(len({args.[0]}))")
            | "List.rev", TFun(TList itemType, resultType) -> build (fun args _ -> iife resultType $"values := {args.[0]}\nresult := make([]{goType itemType}, len(values))\nfor index := range values {{\n\tresult[len(values)-index-1] = values[index]\n}}\nreturn result")
            | "List.append", TFun(TList itemType, _) -> build (fun args _ -> $"append(append([]{goType itemType}{{}}, {args.[0]}...), {args.[1]}...)")
            | "List.tryHead", TFun(TList itemType, resultType) -> build (fun args _ -> iife resultType $"values := {args.[0]}\nif len(values) == 0 {{\n\treturn {goType resultType}{{}}\n}}\nreturn {goType resultType}{{HasValue: true, Value: values[0]}}")
            | "List.tail", TFun(TList itemType, resultType) -> build (fun args _ -> iife resultType $"values := {args.[0]}\nif len(values) == 0 {{\n\tpanic(\"List.tail requires a non-empty list.\")\n}}\nreturn append([]{goType itemType}{{}}, values[1:]...)")
            | "List.tryItem", TFun(_, TFun(TList itemType, resultType)) -> build (fun args _ -> iife resultType $"index, values := {args.[0]}, {args.[1]}\nif index < 0 || index >= int64(len(values)) {{\n\treturn {goType resultType}{{}}\n}}\nreturn {goType resultType}{{HasValue: true, Value: values[index]}}")
            | "List.map", TFun(TFun(_, mappedType), TFun(_, resultType)) -> build (fun args _ -> iife resultType $"result := make([]{goType mappedType}, 0, len({args.[1]}))\nfor _, item := range {args.[1]} {{\n\tresult = append(result, {args.[0]}(item))\n}}\nreturn result")
            | "List.iter", TFun(TFun(_, _), TFun(_, resultType)) -> build (fun args _ -> iife resultType $"for _, item := range {args.[1]} {{\n\t_ = {args.[0]}(item)\n}}\nreturn struct{{}}{{}}")
            | "List.choose", TFun(TFun(_, TOption chosenType), TFun(_, resultType)) -> build (fun args _ -> iife resultType $"result := []{goType chosenType}{{}}\nfor _, item := range {args.[1]} {{\n\tchosen := {args.[0]}(item)\n\tif chosen.HasValue {{\n\t\tresult = append(result, chosen.Value)\n\t}}\n}}\nreturn result")
            | "List.collect", TFun(TFun(_, TList collectedType), TFun(_, resultType)) -> build (fun args _ -> iife resultType $"result := []{goType collectedType}{{}}\nfor _, item := range {args.[1]} {{\n\tresult = append(result, {args.[0]}(item)...)\n}}\nreturn result")
            | "List.exists", TFun(TFun(_, TBool), TFun(_, resultType)) -> build (fun args _ -> iife resultType $"for _, item := range {args.[1]} {{\n\tif {args.[0]}(item) {{\n\t\treturn true\n\t}}\n}}\nreturn false")
            | "List.contains", TFun(itemType, TFun(_, resultType)) -> let equal = equality itemType in build (fun args _ -> iife resultType $"for _, item := range {args.[1]} {{\n\tif {equal}({args.[0]}, item) {{\n\t\treturn true\n\t}}\n}}\nreturn false")
            | "List.distinct", TFun(TList itemType, resultType) -> let equal = equality itemType in build (fun args _ -> iife resultType $"result := []{goType itemType}{{}}\nfor _, item := range {args.[0]} {{\n\tfound := false\n\tfor _, existing := range result {{\n\t\tif {equal}(item, existing) {{\n\t\t\tfound = true\n\t\t\tbreak\n\t\t}}\n\t}}\n\tif !found {{\n\t\tresult = append(result, item)\n\t}}\n}}\nreturn result")
            | "List.filter", TFun(TFun(_, TBool), TFun(TList itemType, resultType)) -> build (fun args _ -> iife resultType $"result := []{goType itemType}{{}}\nfor _, item := range {args.[1]} {{\n\tif {args.[0]}(item) {{\n\t\tresult = append(result, item)\n\t}}\n}}\nreturn result")
            | "List.tryFind", TFun(TFun(itemType, TBool), TFun(_, resultType)) -> build (fun args _ -> iife resultType $"for _, item := range {args.[1]} {{\n\tif {args.[0]}(item) {{\n\t\treturn {goType resultType}{{HasValue: true, Value: item}}\n\t}}\n}}\nreturn {goType resultType}{{}}")
            | "List.tryGet", TFun(TFun(_, TBool), TFun(_, resultType)) -> build (fun args _ -> iife resultType $"for index, item := range {args.[1]} {{\n\tif {args.[0]}(item) {{\n\t\treturn {goType resultType}{{HasValue: true, Value: int64(index)}}\n\t}}\n}}\nreturn {goType resultType}{{}}")
            | "List.fold", TFun(_, TFun(stateType, TFun(_, resultType))) -> build (fun args _ -> iife resultType $"state := {args.[1]}\nfor _, item := range {args.[2]} {{\n\tstate = {args.[0]}(state)(item)\n}}\nreturn state")
            | "Option.defaultValue", TFun(itemType, _) -> build (fun args resultType -> iife resultType $"if {args.[1]}.HasValue {{\n\treturn {args.[1]}.Value\n}}\nreturn {args.[0]}")
            | "Option.defaultWith", TFun(TFun(_, itemType), _) -> build (fun args resultType -> iife resultType $"if {args.[1]}.HasValue {{\n\treturn {args.[1]}.Value\n}}\nreturn {args.[0]}(struct{{}}{{}})")
            | "Option.isNone", _ -> build (fun args _ -> $"!{args.[0]}.HasValue")
            | "Option.isSome", _ -> build (fun args _ -> $"{args.[0]}.HasValue")
            | "Option.map", TFun(TFun(_, mappedType), TFun(_, resultType)) -> build (fun args _ -> iife resultType $"if !{args.[1]}.HasValue {{\n\treturn {goType resultType}{{}}\n}}\nreturn {goType resultType}{{HasValue: true, Value: {args.[0]}({args.[1]}.Value)}}")
            | "Map.tryGet", TFun(_, TFun(TMap(_, valueType), resultType)) ->
                build (fun args _ -> iife resultType $"for _, entry := range {args.[1]}.Entries {{\n\tif entry.Key == {args.[0]} {{\n\t\treturn {goType resultType}{{HasValue: true, Value: entry.Value}}\n\t}}\n}}\nreturn {goType resultType}{{}}")
            | "Map.containsKey", _ ->
                build (fun args resultType -> iife resultType $"for _, entry := range {args.[1]}.Entries {{\n\tif entry.Key == {args.[0]} {{\n\t\treturn true\n\t}}\n}}\nreturn false")
            | "Map.count", _ -> build (fun args _ -> $"int64(len({args.[0]}.Entries))")
            | "Map.add", TFun(_, TFun(_, TFun(sourceType, resultType))) ->
                let entryType = $"entry_{suffix resultType}"
                build (fun args _ -> sortedMap resultType $"result := {goType resultType}{{Entries: append([]{entryType}{{}}, {args.[2]}.Entries...)}}\nfound := false\nfor index := range result.Entries {{\n\tif result.Entries[index].Key == {args.[0]} {{\n\t\tresult.Entries[index].Value = {args.[1]}\n\t\tfound = true\n\t\tbreak\n\t}}\n}}\nif !found {{\n\tresult.Entries = append(result.Entries, {entryType}{{Key: {args.[0]}, Value: {args.[1]}}})\n}}")
            | "Map.ofList", TFun(TList(TTuple [ TString; valueType ]), resultType) ->
                let entryType = $"entry_{suffix resultType}"
                build (fun args _ -> sortedMap resultType $"result := {goType resultType}{{}}\nset := func(key string, value {goType valueType}) {{\n\tfor index := range result.Entries {{\n\t\tif result.Entries[index].Key == key {{\n\t\t\tresult.Entries[index].Value = value\n\t\t\treturn\n\t\t}}\n\t}}\n\tresult.Entries = append(result.Entries, {entryType}{{Key: key, Value: value}})\n}}\nfor _, pair := range {args.[0]} {{\n\tset(pair.Item0, pair.Item1)\n}}")
            | "Map.fold", TFun(_, TFun(stateType, TFun(_, resultType))) ->
                build (fun args _ -> iife resultType $"state := {args.[1]}\nfor _, entry := range {args.[2]}.Entries {{\n\tstate = {args.[0]}(state)(entry.Key)(entry.Value)\n}}\nreturn state")
            | "Map.filter", TFun(_, TFun(TMap(_, valueType), resultType)) ->
                let entryType = $"entry_{suffix resultType}"
                build (fun args _ -> iife resultType $"result := {goType resultType}{{Entries: make([]{entryType}, 0, len({args.[1]}.Entries))}}\nfor _, entry := range {args.[1]}.Entries {{\n\tif {args.[0]}(entry.Key)(entry.Value) {{\n\t\tresult.Entries = append(result.Entries, entry)\n\t}}\n}}\nreturn result")
            | "Map.choose", TFun(_, TFun(_, resultType)) ->
                let entryType = $"entry_{suffix resultType}"
                build (fun args _ -> iife resultType $"result := {goType resultType}{{}}\nfor _, entry := range {args.[1]}.Entries {{\n\tchosen := {args.[0]}(entry.Key)(entry.Value)\n\tif chosen.HasValue {{\n\t\tresult.Entries = append(result.Entries, {entryType}{{Key: entry.Key, Value: chosen.Value}})\n\t}}\n}}\nreturn result")
            | "Map.map", TFun(_, TFun(_, resultType)) ->
                let entryType = $"entry_{suffix resultType}"
                build (fun args _ -> iife resultType $"result := {goType resultType}{{}}\nfor _, entry := range {args.[1]}.Entries {{\n\tresult.Entries = append(result.Entries, {entryType}{{Key: entry.Key, Value: {args.[0]}(entry.Value)}})\n}}\nreturn result")
            | "Map.iter", _ ->
                build (fun args resultType -> iife resultType $"for _, entry := range {args.[1]}.Entries {{\n\t_ = {args.[0]}(entry.Key)(entry.Value)\n}}\nreturn struct{{}}{{}}")
            | "Map.remove", TFun(_, TFun(_, resultType)) ->
                let entryType = $"entry_{suffix resultType}"
                build (fun args _ -> iife resultType $"result := {goType resultType}{{Entries: make([]{entryType}, 0, len({args.[1]}.Entries))}}\nfor _, entry := range {args.[1]}.Entries {{\n\tif entry.Key != {args.[0]} {{\n\t\tresult.Entries = append(result.Entries, entry)\n\t}}\n}}\nreturn result")
            | "Console.writeLine", TFun(TString, TUnit) ->
                "func(value string) struct{} {\n\t_, _ = os.Stdout.WriteString(value + \"\\n\")\n\treturn struct{}{}\n}"
            | "Console.readLine", TFun(TUnit, TOption TString) ->
                let resultType = goType (TOption TString)
                $"func(_ struct{{}}) {resultType} {{\n\tif consoleScanner.Scan() {{\n\t\treturn {resultType}{{HasValue: true, Value: consoleScanner.Text()}}\n\t}}\n\treturn {resultType}{{}}\n}}"
            | "Fs.readText", TFun(TString, TOption TString) ->
                let resultType = goType (TOption TString)
                $"func(path string) {resultType} {{\n\tcontent, err := os.ReadFile(path)\n\tif err != nil {{\n\t\treturn {resultType}{{}}\n\t}}\n\treturn {resultType}{{HasValue: true, Value: string(content)}}\n}}"
            | _ -> invalidOp $"Unsupported typed intrinsic '{name}' at {Types.typeToString intrinsicType}."
        let hostExternal (name: string) (externalType: Type) =
            match name, externalType with
            | "Console.writeLine", TFun(TString, TUnit) ->
                "func(value string) struct{} {\n\t_, _ = os.Stdout.WriteString(value + \"\\n\")\n\treturn struct{}{}\n}"
            | "Console.readLine", TFun(TUnit, TOption TString) ->
                let resultType = goType (TOption TString)
                $"func(_ struct{{}}) {resultType} {{\n\tif consoleScanner.Scan() {{\n\t\treturn {resultType}{{HasValue: true, Value: consoleScanner.Text()}}\n\t}}\n\treturn {resultType}{{}}\n}}"
            | "Fs.readText", TFun(TString, TOption TString) ->
                let resultType = goType (TOption TString)
                $"func(path string) {resultType} {{\n\tcontent, err := os.ReadFile(path)\n\tif err != nil {{\n\t\treturn {resultType}{{}}\n\t}}\n\treturn {resultType}{{HasValue: true, Value: string(content)}}\n}}"
            | _ -> invalidOp $"Unsupported typed host external '{name}' at {Types.typeToString externalType}."
        let targetImport modulePath memberName importType =
            let alias = importModules.[modulePath]
            let rec curry index currentType arguments =
                match currentType with
                | TFun(argumentType, resultType) ->
                    let argument = $"importArg{index}"
                    let inner = curry (index + 1) resultType (arguments @ [ argument ])
                    $"func({argument} {goType argumentType}) {goType resultType} {{\n\treturn {indent inner}\n}}"
                | _ ->
                    let renderedArguments = String.concat ", " arguments
                    $"{alias}.{memberName}({renderedArguments})"
            curry 0 importType []
        let rec emit environment (expression: Lir.Expression) =
            let emitChild = emit environment
            match expression.Node with
            | Lir.Constant value -> constant value
            | Lir.Load id -> environment |> Map.tryFind id |> Option.orElseWith (fun () -> Map.tryFind id names) |> Option.defaultWith (fun () -> invalidOp $"Unknown specialized symbol {Lir.symbolNumber id}.")
            | Lir.MakeClosure(id, _) ->
                let fn = functions.[id]
                let parameter = symbolName "arg" fn.Parameter
                let body = emit (Map.add fn.Parameter parameter environment) fn.Body
                $"func({parameter} {goType fn.ParameterType}) {goType fn.ReturnType} {{\n\treturn {indent body}\n}}"
            | Lir.CallClosure(fn, argument) -> $"({emitChild fn})({emitChild argument})"
            | Lir.PipeClosure(argument, fn) -> $"func() {goType expression.Type} {{\n\tpipelineArgument := {indent (emitChild argument)}\n\tpipelineFunction := {indent (emitChild fn)}\n\treturn pipelineFunction(pipelineArgument)\n}}()"
            | Lir.Branch(condition, whenTrue, whenFalse) -> $"func() {goType expression.Type} {{\n\tif {goCondition (emitChild condition)} {{\n\t\treturn {indent (indent (emitChild whenTrue))}\n\t}}\n\treturn {indent (emitChild whenFalse)}\n}}()"
            | Lir.Operation(operation, [ left; right ]) ->
                let leftCode, rightCode = emitChild left, emitChild right
                match operation with
                | Lir.StructuralEqual when left.Type <> right.Type ->
                    match left.Type, right.Type with
                    | (TNamed _ | TRecord _), (TNamed _ | TRecord _) ->
                        let leftFields, rightFields = recordFields left.Type, recordFields right.Type
                        let shared = leftFields |> Map.toList |> List.filter (fun (name, fieldType) -> Map.tryFind name rightFields = Some fieldType)
                        let checks = shared |> List.map (fun (name, fieldType) -> $"{equality fieldType}(leftValue.{emittedField left.Type name}, rightValue.{emittedField right.Type name})")
                        let result = if checks.IsEmpty then "true" else String.concat " && " checks
                        $"func() bool {{\n\tleftValue := {leftCode}\n\trightValue := {rightCode}\n\treturn {result}\n}}()"
                    | _ -> $"{equality left.Type}({leftCode}, {rightCode})"
                | Lir.StructuralEqual -> $"{equality left.Type}({leftCode}, {rightCode})"
                | Lir.IntAdd | Lir.FloatAdd -> $"({leftCode} + {rightCode})"
                | Lir.IntSubtract | Lir.FloatSubtract -> $"({leftCode} - {rightCode})"
                | Lir.IntMultiply | Lir.FloatMultiply -> $"({leftCode} * {rightCode})"
                | Lir.IntDivide | Lir.FloatDivide ->
                    $"func() {goType expression.Type} {{ left, right := {leftCode}, {rightCode}; return left / right }}()"
                | Lir.IntRemainder ->
                    $"func() int64 {{ left, right := {leftCode}, {rightCode}; return left %% right }}()"
                | Lir.NumericLessThan -> $"({leftCode} < {rightCode})"
                | Lir.NumericGreaterThan -> $"({leftCode} > {rightCode})"
                | Lir.NumericLessThanOrEqual -> $"({leftCode} <= {rightCode})"
                | Lir.NumericGreaterThanOrEqual -> $"({leftCode} >= {rightCode})"
                | Lir.BooleanAnd -> $"({leftCode} && {rightCode})"
                | Lir.BooleanOr -> $"({leftCode} || {rightCode})"
            | Lir.Tuple items ->
                let fields = items |> List.mapi (fun index item -> $"Item{index}: {emitChild item}") |> String.concat ", "
                $"{goType expression.Type}{{{fields}}}"
            | Lir.List items ->
                let values = items |> List.map emitChild |> String.concat ", "
                $"{goType expression.Type}{{{values}}}"
            | Lir.Record fields ->
                let values = fields |> List.map (fun (name, value) -> $"{emittedField expression.Type name}: {emitChild value}") |> String.concat ", "
                match expression.Type with
                | TNamed name ->
                    match namedRepresentation name with
                    | Some(LocalNamed true) -> $"&named_{identifier name}{{{values}}}"
                    | _ -> $"{goType expression.Type}{{{values}}}"
                | _ -> $"{goType expression.Type}{{{values}}}"
            | Lir.FieldGet(target, name) ->
                let field = if target.Type = TNamed "Environment" then identifier name else emittedField target.Type name
                $"({emitChild target}).{field}"
            | Lir.SomeValue item -> $"{goType expression.Type}{{HasValue: true, Value: {emitChild item}}}"
            | Lir.NoneValue -> $"{goType expression.Type}{{}}"
            | Lir.Cons(head, tail) -> $"append({goType expression.Type}{{{emitChild head}}}, {emitChild tail}...)"
            | Lir.Append(left, right) -> $"append(append({goType expression.Type}{{}}, {emitChild left}...), {emitChild right}...)"
            | Lir.Let(id, _, recursive, binding, body) ->
                let variable = symbolName "local" id
                let scoped = Map.add id variable environment
                let bindingCode = emit (if recursive then scoped else environment) binding
                let bodyCode = emit scoped body
                let declaration = if recursive then $"var {variable} {goType binding.Type}\n\t{variable} = {indent bindingCode}" else $"{variable} := {indent bindingCode}"
                $"func() {goType expression.Type} {{\n\t{declaration}\n\t_ = {variable}\n\treturn {indent bodyCode}\n}}()"
            | Lir.LetRecursiveGroup(bindings, body) ->
                let scoped = bindings |> List.fold (fun current (id, _, _, _) -> Map.add id (symbolName "localRec" id) current) environment
                let declarations = bindings |> List.map (fun (id, _, valueType, _) -> let name = symbolName "localRec" id in $"var {name} {goType valueType}") |> String.concat "\n\t"
                let assignments = bindings |> List.map (fun (id, _, _, value) -> let name = symbolName "localRec" id in $"{name} = {indent (emit scoped value)}") |> String.concat "\n\t"
                $"func() {goType expression.Type} {{\n\t{declarations}\n\t{assignments}\n\treturn {indent (emit scoped body)}\n}}()"
            | Lir.Fail message -> $"func() {goType expression.Type} {{ panic({emitChild message}) }}()"
            | Lir.DecisionTree(target, cases) ->
                let targetName = "matchValue"
                let rec project (plan: Lir.PatternPlan) code valueType prefix steps =
                    match steps with
                    | [] -> code, valueType
                    | step :: rest ->
                        let nextCode, nextType =
                            match step, valueType with
                            | Lir.ListHead, TList item -> $"({code})[0]", item
                            | Lir.ListTail, TList item -> $"({code})[1:]", TList item
                            | Lir.TupleItem index, TTuple items -> $"({code}).Item{index}", items.[index]
                            | Lir.RecordField name, recordType ->
                                let field = if recordType = TNamed "Environment" then identifier name else emittedField recordType name
                                $"({code}).{field}", (recordFields recordType).[name]
                            | Lir.OptionValue, TOption item -> $"({code}).Value", item
                            | Lir.UnionPayload, unionType ->
                                let caseName =
                                    plan.Checks
                                    |> List.tryPick (function Lir.UnionCaseCheck(projection, _, caseName, true) when projection = prefix -> Some caseName | _ -> None)
                                    |> Option.defaultWith (fun () -> invalidOp "Union payload projection has no corresponding case check.")
                                let payloadType = (unionCases unionType).[caseName] |> Option.defaultWith (fun () -> invalidOp "Union case has no payload.")
                                $"({code}).{caseField caseName}", payloadType
                            | _ -> invalidOp $"Invalid typed pattern projection {step} from {Types.typeToString valueType}."
                        project plan nextCode nextType (prefix @ [ step ]) rest
                let emitPlanCheck plan root rootType = function
                    | Lir.LiteralCheck(projection, literal) ->
                        let code, valueType = project plan root rootType [] projection
                        $"{equality valueType}({code}, {constant literal})"
                    | Lir.EmptyListCheck projection -> let code, _ = project plan root rootType [] projection in $"len({code}) == 0"
                    | Lir.NonEmptyListCheck projection -> let code, _ = project plan root rootType [] projection in $"len({code}) > 0"
                    | Lir.TupleArityCheck _ | Lir.RecordFieldCheck _ | Lir.TypeCheck _ -> "true"
                    | Lir.SomeCheck projection -> let code, _ = project plan root rootType [] projection in $"({code}).HasValue"
                    | Lir.NoneCheck projection -> let code, _ = project plan root rootType [] projection in $"!({code}).HasValue"
                    | Lir.UnionCaseCheck(projection, _, caseName, _) ->
                        let code, _ = project plan root rootType [] projection
                        let tag = Text.Json.JsonSerializer.Serialize(caseName)
                        $"({code}).Tag == {tag}"
                let emitCase (case: Lir.DecisionCase) =
                    let rec allBindings (plan: Lir.PatternPlan) =
                        [ yield! plan.Bindings
                          for mapPlan in plan.Maps do
                              for clause in mapPlan.Clauses do
                                  yield! allBindings clause.Key
                                  yield! allBindings clause.Value
                              yield! mapPlan.Tail |> Option.map allBindings |> Option.defaultValue [] ]
                    let groupedBindings = allBindings case.Plan |> List.groupBy _.Id
                    let scoped, declarations =
                        groupedBindings
                        |> List.fold (fun (currentEnvironment, lines) (id, bindings) ->
                            let binding = List.head bindings
                            let variable = symbolName "pattern" id
                            Map.add id variable currentEnvironment, lines @ [ $"var {variable} {goType binding.Type}"; $"_ = {variable}" ]) (environment, [])
                    let simplePlan (plan: Lir.PatternPlan) root (rootType: Type) =
                        let checks = plan.Checks |> List.map (emitPlanCheck plan root rootType)
                        let grouped = plan.Bindings |> List.groupBy _.Id
                        let repeated =
                            grouped |> List.collect (fun (_, bindings) ->
                                match bindings with
                                | [] | [ _ ] -> []
                                | first :: rest ->
                                    let firstCode, _ = project plan root rootType [] first.Projection
                                    rest |> List.map (fun binding -> let code, _ = project plan root rootType [] binding.Projection in $"{equality binding.Type}({firstCode}, {code})"))
                        let assignments =
                            grouped |> List.map (fun (id, bindings) ->
                                let binding = List.head bindings
                                let code, _ = project plan root rootType [] binding.Projection
                                let variable = symbolName "pattern" id
                                $"{variable} = {code}")
                        let meaningfulChecks = checks @ repeated |> List.filter ((<>) "true")
                        (match meaningfulChecks with [] -> "true" | values -> String.concat " && " values), assignments
                    let emitMapPlan _ (mapPlan: Lir.MapPlan) =
                        let mapCode, mapType = project case.Plan targetName target.Type [] mapPlan.Projection
                        let keyType, valueType = match mapType with TMap(keyType, valueType) -> keyType, valueType | _ -> invalidOp "Map pattern projection is not a map."
                        let entryType = $"entry_{suffix mapType}"
                        let clauseCases =
                            mapPlan.Clauses
                            |> List.mapi (fun clauseIndex clause ->
                                let keyCondition, keyAssignments = simplePlan clause.Key "entry.Key" keyType
                                let valueCondition, valueAssignments = simplePlan clause.Value "entry.Value" valueType
                                let clauseCondition =
                                    [ keyCondition; valueCondition ]
                                    |> List.filter ((<>) "true")
                                    |> function [] -> "true" | values -> String.concat " && " values
                                let assignments = String.concat "\n\t\t\t\t" (keyAssignments @ valueAssignments)
                                let assignmentSource = if String.IsNullOrEmpty assignments then "" else assignments + "\n\t\t\t\t"
                                $"case {clauseIndex}:\n\t\tfor entryIndex, entry := range remaining.Entries {{\n\t\t\tif {clauseCondition} {{\n\t\t\t\t{assignmentSource}nextEntries := make([]{entryType}, 0, len(remaining.Entries)-1)\n\t\t\t\tnextEntries = append(nextEntries, remaining.Entries[:entryIndex]...)\n\t\t\t\tnextEntries = append(nextEntries, remaining.Entries[entryIndex+1:]...)\n\t\t\t\tif matchClause(clauseIndex+1, {goType mapType}{{Entries: nextEntries}}) {{\n\t\t\t\t\treturn true\n\t\t\t\t}}\n\t\t\t}}\n\t\t}}\n\t\treturn false")
                            |> String.concat "\n\t"
                        let terminal =
                            match mapPlan.Tail with
                            | None -> if mapPlan.RequireEmptyRemainder then "return len(remaining.Entries) == 0" else "return true"
                            | Some tail ->
                                let condition, assignments = simplePlan tail "remaining" mapType
                                let assignmentSource = if assignments.IsEmpty then "" else String.concat "\n\t\t" assignments + "\n\t\t"
                                $"if {condition} {{\n\t\t{assignmentSource}return true\n\t}}\n\treturn false"
                        let switchSource = if mapPlan.Clauses.IsEmpty then "" else $"\n\t\tswitch clauseIndex {{\n\t\t{indent clauseCases}\n\t\t}}"
                        $"func() bool {{\n\tvar matchClause func(int, {goType mapType}) bool\n\tmatchClause = func(clauseIndex int, remaining {goType mapType}) bool {{\n\t\tif clauseIndex == {mapPlan.Clauses.Length} {{\n\t\t\t{indent (indent terminal)}\n\t\t}}{switchSource}\n\t\treturn false\n\t}}\n\treturn matchClause(0, {mapCode})\n}}()"
                    let condition, topAssignments = simplePlan case.Plan targetName target.Type
                    let mapChecks = case.Plan.Maps |> List.mapi emitMapPlan
                    let bodyCode = emit scoped case.Body
                    let guardedBody =
                        match case.Guard with
                        | None -> $"return {indent (indent bodyCode)}"
                        | Some guard -> $"if {goCondition (emit scoped guard)} {{\n\t\t\treturn {indent (indent bodyCode)}\n\t\t}}"
                    let declarationCode = if declarations.IsEmpty then "" else String.concat "\n\t\t" declarations + "\n\t\t"
                    let assignmentCode = if topAssignments.IsEmpty then "" else String.concat "\n\t\t" topAssignments + "\n\t\t"
                    let mapEvaluation =
                        mapChecks
                        |> List.map (fun check -> $"if patternsMatch {{\n\t\t\tpatternsMatch = {indent (indent (indent check))}\n\t\t}}")
                        |> String.concat "\n\t\t"
                    let mapEvaluationSource = if mapChecks.IsEmpty then "" else $"patternsMatch := true\n\t\t{mapEvaluation}\n\t\t"
                    let mapCondition = if mapChecks.IsEmpty then "true" else "patternsMatch"
                    $"if {condition} {{\n\t\t{declarationCode}{assignmentCode}{mapEvaluationSource}if {mapCondition} {{\n\t\t\t{indent guardedBody}\n\t\t}}\n\t}}"
                let caseCode = cases |> List.map emitCase |> String.concat "\n\t"
                $"func() {goType expression.Type} {{\n\t{targetName} := {indent (emitChild target)}\n\t_ = {targetName}\n\t{caseCode}\n\tpanic(\"Match failure.\")\n}}()"
            | Lir.RecordUpdate(target, fields) ->
                let targetCode = emitChild target
                let assignments = fields |> List.map (fun (name, value) -> $"updated.{emittedField expression.Type name} = {emitChild value}") |> String.concat "\n\t"
                match expression.Type with
                | TNamed name when namedRepresentation name = Some(LocalNamed true) -> $"func() {goType expression.Type} {{\n\toriginal := {targetCode}\n\tupdated := *original\n\t{assignments}\n\treturn &updated\n}}()"
                | _ -> $"func() {goType expression.Type} {{\n\tupdated := {targetCode}\n\t{assignments}\n\treturn updated\n}}()"
            | Lir.RecordConvert(target, targetType) ->
                let targetCode = emitChild target
                let fields = recordFields targetType
                let assignments = fields |> Map.toList |> List.map (fun (name, _) -> $"{emittedField targetType name}: converted.{emittedField target.Type name}") |> String.concat ", "
                match targetType with
                | TNamed name when namedRepresentation name = Some(LocalNamed true) -> $"func() {goType targetType} {{\n\tconverted := {targetCode}\n\treturn &named_{identifier name}{{{assignments}}}\n}}()"
                | _ -> $"func() {goType targetType} {{\n\tconverted := {targetCode}\n\treturn {goType targetType}{{{assignments}}}\n}}()"
            | Lir.Range(first, last) -> $"func() []int64 {{\n\tfirst, last := {emitChild first}, {emitChild last}\n\tresult := []int64{{}}\n\tif first <= last {{\n\t\tfor value := first; value <= last; value++ {{\n\t\t\tresult = append(result, value)\n\t\t}}\n\t}} else {{\n\t\tfor value := first; value >= last; value-- {{\n\t\t\tresult = append(result, value)\n\t\t}}\n\t}}\n\treturn result\n}}()"
            | Lir.IndexGet(target, index) ->
                match target.Type with
                | TList _ -> $"func() {goType expression.Type} {{\n\tvalues, index := {emitChild target}, {emitChild index}\n\tif index < 0 || index >= int64(len(values)) {{\n\t\treturn {goType expression.Type}{{}}\n\t}}\n\treturn {goType expression.Type}{{HasValue: true, Value: values[index]}}\n}}()"
                | TMap(keyType, _) ->
                    let keyEquality = equality keyType
                    $"func() {goType expression.Type} {{\n\tvalues, key := {emitChild target}, {emitChild index}\n\tfor _, entry := range values.Entries {{\n\t\tif {keyEquality}(entry.Key, key) {{\n\t\t\treturn {goType expression.Type}{{HasValue: true, Value: entry.Value}}\n\t\t}}\n\t}}\n\treturn {goType expression.Type}{{}}\n}}()"
                | _ -> invalidOp "Index target is neither list nor map."
            | Lir.Interpolate parts -> parts |> List.map (function Lir.TextPart text -> Text.Json.JsonSerializer.Serialize(text) | Lir.ValuePart value -> $"{formatter value.Type}({emitChild value})") |> String.concat " + "
            | Lir.Intrinsic name -> intrinsic name expression.Type
            | Lir.HostExternal(name, _) -> hostExternal name expression.Type
            | Lir.TargetImport(modulePath, memberName) -> targetImport modulePath memberName expression.Type
            | Lir.RuntimeContext -> $"environment{{ScriptName: {goType (TOption TString)}{{HasValue: true, Value: filepath.Base(os.Args[0])}}, Arguments: append([]string{{}}, os.Args[1:]...)}}"
            | Lir.ForEach(id, _, _, source, body) ->
                let variable = symbolName "item" id
                let bodyCode = emit (Map.add id variable environment) body
                $"func() struct{{}} {{\n\tfor _, {variable} := range {emitChild source} {{\n\t\t_ = {indent (indent bodyCode)}\n\t}}\n\treturn struct{{}}{{}}\n}}()"
            | Lir.TypeToken valueType -> $"typeToken{{Value: {Text.Json.JsonSerializer.Serialize(Types.typeToString valueType)}}}"
            | Lir.UnionCase(_, caseName, payload) ->
                let payloadField = payload |> Option.map (fun value -> $", {caseField caseName}: {emitChild value}") |> Option.defaultValue ""
                let tag = Text.Json.JsonSerializer.Serialize(caseName)
                match expression.Type with
                | TNamed name when namedRepresentation name = Some(LocalNamed true) -> $"&named_{identifier name}{{Tag: {tag}{payloadField}}}"
                | _ -> $"{goType expression.Type}{{Tag: {tag}{payloadField}}}"
            | Lir.Map entries ->
                let keyType, valueType = match expression.Type with TMap(k, v) -> k, v | _ -> invalidOp "Map expression has non-map type."
                let resultType, entryType = goType expression.Type, $"entry_{suffix expression.Type}"
                let keyEqual = equality keyType
                let entryCode =
                    entries
                    |> List.map (function
                        | Lir.Spread value -> $"for _, entry := range ({emitChild value}).Entries {{\n\t\tset(entry.Key, entry.Value)\n\t}}"
                        | Lir.KeyValue(key, value) -> $"set({emitChild key}, {emitChild value})")
                    |> String.concat "\n\t"
                let ordering =
                    match keyType with
                    | TString | TInt -> "result.Entries[inner].Key < result.Entries[inner-1].Key"
                    | _ -> "false"
                $"func() {resultType} {{\n\tresult := {resultType}{{}}\n\tset := func(key {goType keyType}, value {goType valueType}) {{\n\t\tfor index := range result.Entries {{\n\t\t\tif {keyEqual}(result.Entries[index].Key, key) {{\n\t\t\t\tresult.Entries[index].Value = value\n\t\t\t\treturn\n\t\t\t}}\n\t\t}}\n\t\tresult.Entries = append(result.Entries, {entryType}{{Key: key, Value: value}})\n\t}}\n\t{entryCode}\n\tfor outer := 1; outer < len(result.Entries); outer++ {{\n\t\tfor inner := outer; inner > 0 && {ordering}; inner-- {{\n\t\t\tresult.Entries[inner], result.Entries[inner-1] = result.Entries[inner-1], result.Entries[inner]\n\t\t}}\n\t}}\n\treturn result\n}}()"
            | node -> invalidOp $"Unsupported typed Go node {node}."

        let declarations = StringBuilder()
        for declaration in program.Declarations do declarations.AppendLine($"var {names.[declaration.Id]} {goType declaration.Type}") |> ignore
        declarations.AppendLine() |> ignore
        declarations.AppendLine("func init() {") |> ignore
        for declaration in program.Declarations do declarations.AppendLine($"\t{names.[declaration.Id]} = {indent (emit names declaration.Value)}") |> ignore
        declarations.AppendLine("}") |> ignore
        let entry = program.Entry |> Option.defaultValue { Type = TUnit; Span = { Start = { Line = 0; Column = 0; File = None }; End = { Line = 0; Column = 0; File = None } }; Node = Lir.Constant Lir.Unit }
        let entryCode = emit names entry
        let resultFormatter = formatter entry.Type
        let helperSource = String.concat "\n\n" (Seq.append equalityBodies formatterBodies)
        let emittedValues = declarations.ToString() + entryCode
        let consoleSource = if emittedValues.Contains("consoleScanner", StringComparison.Ordinal) then "var consoleScanner = bufio.NewScanner(os.Stdin)\n\n" else ""
        let generatedBody = helperSource + consoleSource + emittedValues
        let imports =
            [ if generatedBody.Contains("bufio.", StringComparison.Ordinal) then
                  "\t\"bufio\""
              if generatedBody.Contains("os.", StringComparison.Ordinal) then
                  "\t\"os\""
              if generatedBody.Contains("filepath.", StringComparison.Ordinal) then
                  "\t\"path/filepath\""
              if generatedBody.Contains("math.", StringComparison.Ordinal) then
                  "\t\"math\""
              if generatedBody.Contains("strconv.", StringComparison.Ordinal) then
                  "\t\"strconv\""
              if generatedBody.Contains("strings.", StringComparison.Ordinal) then
                  "\t\"strings\""
              if generatedBody.Contains("utf8.", StringComparison.Ordinal) then
                  "\t\"unicode/utf8\"" ]
        let targetStandardImports, targetExternalImports =
            importModules
            |> Map.toList
            |> List.map (fun (path, alias) -> path, $"\t{alias} {Text.Json.JsonSerializer.Serialize(path)}")
            |> List.partition (fun (path, _) -> not (path.Contains(".", StringComparison.Ordinal)))
            |> fun (standard, external) -> List.map snd standard, List.map snd external
        let importSource =
            let standard = imports @ targetStandardImports |> List.sort
            let groups = [ if not standard.IsEmpty then String.concat "\n" standard; if not targetExternalImports.IsEmpty then String.concat "\n" targetExternalImports ]
            match groups with
            | [] -> ""
            | values ->
                let renderedGroups = String.concat "\n\n" values
                $"import (\n{renderedGroups}\n)\n\n"
        let programSource = $"package fscriptprogram\n\n{importSource}{typeDeclarations}\n{helperSource}\n\n{consoleSource}{declarations}\nfunc Run() {goType entry.Type} {{\n\treturn {entryCode}\n}}\n\nfunc FormatResult(value {goType entry.Type}) string {{\n\treturn {resultFormatter}(value)\n}}\n"
        let mainSource = $"package main\n\nimport (\n\t\"{moduleName}/internal/fscriptprogram\"\n\t\"os\"\n)\n\nfunc main() {{\n\tvalue := fscriptprogram.Run()\n\t_, _ = os.Stdout.WriteString(fscriptprogram.FormatResult(value) + \"\\n\")\n}}\n"
        [ "go.mod", $"module {moduleName}\n\ngo 1.25\n"
          "main.go", mainSource
          "internal/fscriptprogram/program.go", programSource ]
