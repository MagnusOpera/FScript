namespace FScript.Compiler

open System
open System.Collections.Generic
open System.IO
open System.Security.Cryptography
open System.Text
open FScript.Language

module TypedGoEmitter =
    type private NamedRepresentation =
        | LocalNamed of recursive: bool
        | LocalUnion of hasPayload: bool
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
    let private mapKeyLess keyType left right =
        match keyType with
        | TBool -> $"!{left} && {right}"
        | _ -> $"{left} < {right}"
    let private tryStaticRangeDirection (first: Lir.Expression) (last: Lir.Expression) =
        match first.Node, last.Node with
        | Lir.Constant(Lir.Int firstValue), Lir.Constant(Lir.Int lastValue) -> Some(firstValue <= lastValue)
        | _ -> None

    let private goCondition (value: string) =
        if value.Length < 2 || value.[0] <> '(' || value.[value.Length - 1] <> ')' then value
        else
            let mutable depth = 0
            let mutable quoted = false
            let mutable escaped = false
            let mutable closesAtEnd = true
            for index = 0 to value.Length - 1 do
                let character = value.[index]
                if escaped then escaped <- false
                elif quoted && character = '\\' then escaped <- true
                elif character = '"' then quoted <- not quoted
                elif not quoted then
                    if character = '(' then depth <- depth + 1
                    elif character = ')' then
                        depth <- depth - 1
                        if depth = 0 && index <> value.Length - 1 then closesAtEnd <- false
            if closesAtEnd && depth = 0 then value.Substring(1, value.Length - 2) else value

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
        | TTask _ as value -> $"task_{suffix value}"
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
        | TList item | TOption item | TTask item -> supportedType item
        | TTuple items -> List.forall supportedType items
        | TRecord fields -> fields |> Map.forall (fun _ value -> supportedType value)
        | TMap(keyType, valueType) -> supportedType keyType && supportedType valueType
        | TUnion(_, cases) -> cases |> Map.forall (fun _ value -> Option.forall supportedType value)
        | TVar _ | TNumeric _ | TRecordShape _ | THostManaged _ -> false

    let private typedIntrinsics =
        set [ "ignore"; "Int.tryParse"; "Float.tryParse"; "Bool.tryParse"; "Int.toString"; "Float.toString"; "Bool.toString"
              "String.replace"; "String.indexOf"; "String.toLower"; "String.toUpper"; "String.substring"; "String.concat"; "String.split"; "String.endsWith"
              "List.empty"; "List.map"; "List.iter"; "List.choose"; "List.collect"; "List.exists"; "List.contains"; "List.rev"; "List.distinct"
              "List.fold"; "List.filter"; "List.length"; "List.tryFind"; "List.tryGet"; "List.tryItem"; "List.tryHead"; "List.tail"; "List.append"
              "Option.defaultValue"; "Option.defaultWith"; "Option.isNone"; "Option.isSome"; "Option.map"
              "Map.empty"; "Map.tryGet"; "Map.containsKey"; "Map.add"; "Map.ofList"; "Map.fold"; "Map.count"; "Map.filter"; "Map.choose"; "Map.map"; "Map.iter"; "Map.remove"
              "Console.readLine"; "Console.writeLine"; "Fs.readText" ]

    let private typedHostExternals =
        set [ "Console.readLine"; "Console.writeLine"; "Regex.matchGroups"
              "Fs.readText"; "Fs.exists"; "Fs.kind"; "Fs.createDirectory"; "Fs.writeText"; "Fs.combinePath"
              "Fs.parentDirectory"; "Fs.extension"; "Fs.fileNameWithoutExtension"; "Fs.glob"; "Fs.enumerateFiles"
              "Task.spawn"; "Task.await" ]

    let isTypedIntrinsic name = Set.contains name typedIntrinsics
    let isTypedHostExternal name = Set.contains name typedHostExternals

    let rec private supportedExpression (functions: Map<Lir.FunctionId, Lir.Function>) (expression: Lir.Expression) =
        let supported = supportedExpression functions
        supportedType expression.Type
        && match expression.Node with
           | Lir.Constant _ | Lir.Load _ | Lir.NoneValue | Lir.RuntimeContext | Lir.TypeToken _ -> true
           | Lir.MakeClosure(id, _) -> functions |> Map.tryFind id |> Option.exists (fun fn -> supported fn.Body)
           | Lir.CallClosure(left, right) | Lir.PipeClosure(left, right)
           | Lir.TailCallClosure(left, right) | Lir.TailPipeClosure(left, right)
           | Lir.Cons(left, right)
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
           | Lir.Intrinsic name -> isTypedIntrinsic name
           | Lir.HostExternal(name, _) -> isTypedHostExternal name
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
              | Lir.CallClosure(left, right) | Lir.PipeClosure(left, right)
              | Lir.TailCallClosure(left, right) | Lir.TailPipeClosure(left, right)
              | Lir.Cons(left, right) | Lir.Append(left, right) | Lir.Range(left, right) | Lir.IndexGet(left, right) -> yield! targetModules left; yield! targetModules right
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
                    | None ->
                        match declaration.Definition with
                        | TUnion(_, cases) -> LocalUnion(cases |> Map.exists (fun _ payload -> payload.IsSome))
                        | _ -> LocalNamed declaration.IsRecursive
                declaration.Name, representation)
            |> Map.ofList
        let functions = program.Functions |> List.map (fun item -> item.Id, item) |> Map.ofList
        let namedDefinitions = program.Types |> List.map (fun item -> item.Name, item.Definition) |> Map.ofList
        let namedRepresentation name = tryNamedRepresentation name
        let unionCases = function
            | TUnion(_, cases) -> cases
            | TNamed name ->
                match Map.tryFind name namedDefinitions with
                | Some(TUnion(_, cases)) -> cases
                | _ -> invalidOp $"Named type '{name}' is not a union."
            | value -> invalidOp $"Type '{Types.typeToString value}' is not a union."
        let unionHasPayload unionType = unionCases unionType |> Map.exists (fun _ payload -> payload.IsSome)
        let unionCaseType unionType caseName = $"case_{identifier (goType unionType)}_{identifier caseName}"
        let unionMarker unionType = $"is_{identifier (goType unionType)}"
        let unionConstant unionType caseName = $"{identifier (goType unionType)}_{identifier caseName}"
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
                | TList item | TOption item | TTask item -> collectType item
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
            | Lir.CallClosure(left, right) | Lir.PipeClosure(left, right)
            | Lir.TailCallClosure(left, right) | Lir.TailPipeClosure(left, right)
            | Lir.Cons(left, right)
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
        let appendUnionDeclaration unionType cases =
            let typeName = goType unionType
            if unionHasPayload unionType then
                typeDeclarations.AppendLine($"type {typeName} interface {{") |> ignore
                typeDeclarations.AppendLine($"\t{unionMarker unionType}()") |> ignore
                typeDeclarations.AppendLine("}") |> ignore
                typeDeclarations.AppendLine() |> ignore
                cases
                |> Map.iter (fun caseName payload ->
                    let caseType = unionCaseType unionType caseName
                    match payload with
                    | None ->
                        typeDeclarations.AppendLine($"type {caseType} struct{{}}") |> ignore
                        typeDeclarations.AppendLine() |> ignore
                    | Some payloadType ->
                        typeDeclarations.AppendLine($"type {caseType} struct {{") |> ignore
                        appendStructFields [ "Value", payloadType ]
                        typeDeclarations.AppendLine("}") |> ignore
                        typeDeclarations.AppendLine() |> ignore
                    typeDeclarations.AppendLine($"func ({caseType}) {unionMarker unionType}() {{}}") |> ignore
                    typeDeclarations.AppendLine() |> ignore)
            else
                typeDeclarations.AppendLine($"type {typeName} uint8") |> ignore
                typeDeclarations.AppendLine() |> ignore
                typeDeclarations.AppendLine("const (") |> ignore
                cases
                |> Map.toList
                |> List.iteri (fun index (caseName, _) ->
                    let declaration =
                        if index = 0 then $"{unionConstant unionType caseName} {typeName} = iota"
                        else unionConstant unionType caseName
                    typeDeclarations.AppendLine($"\t{declaration}") |> ignore)
                typeDeclarations.AppendLine(")") |> ignore
                typeDeclarations.AppendLine() |> ignore
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
                appendUnionDeclaration (TNamed declaration.Name) cases
            | _ -> ())
        for value in types |> Seq.sortBy key do
            match value with
            | TOption item ->
                typeDeclarations.AppendLine($"type {goType value} struct {{") |> ignore
                typeDeclarations.AppendLine("\tHasValue bool") |> ignore
                typeDeclarations.AppendLine($"\tValue    {goType item}") |> ignore
                typeDeclarations.AppendLine("}") |> ignore
            | TTask item ->
                typeDeclarations.AppendLine($"type {goType value} struct {{") |> ignore
                typeDeclarations.AppendLine("\tDone   <-chan struct{}") |> ignore
                typeDeclarations.AppendLine($"\tResult *{goType item}") |> ignore
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
                appendUnionDeclaration value cases
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
                    | TTask _ -> "return \"<task>\""
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
                                if unionHasPayload value then
                                    match payload with
                                    | None -> $"case {unionCaseType value caseName}:\n\t\treturn {Text.Json.JsonSerializer.Serialize(caseName)}"
                                    | Some payloadType ->
                                        let label = Text.Json.JsonSerializer.Serialize(caseName + " ")
                                        $"case {unionCaseType value caseName}:\n\t\treturn {label} + {formatter payloadType}(caseValue.Value)"
                                else
                                    $"case {unionConstant value caseName}:\n\t\treturn {Text.Json.JsonSerializer.Serialize(caseName)}")
                        let renderedClauses = String.concat "\n\t" clauses
                        let target = if unionHasPayload value then "caseValue := value.(type)" else "value"
                        $"switch {target} {{\n\t{renderedClauses}\n\t}}\n\treturn \"<invalid union>\""
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
                                if unionHasPayload value then
                                    match payload with
                                    | None -> $"case {unionCaseType value caseName}:\n\t\treturn {Text.Json.JsonSerializer.Serialize(caseName)}"
                                    | Some payloadType ->
                                        let label = Text.Json.JsonSerializer.Serialize(caseName + " ")
                                        $"case {unionCaseType value caseName}:\n\t\treturn {label} + {formatter payloadType}(caseValue.Value)"
                                else
                                    $"case {unionConstant value caseName}:\n\t\treturn {Text.Json.JsonSerializer.Serialize(caseName)}")
                            let renderedClauses = String.concat "\n\t" clauses
                            let target = if unionHasPayload value then "caseValue := value.(type)" else "value"
                            $"switch {target} {{\n\t{renderedClauses}\n\t}}\n\treturn \"<invalid union>\""
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
                    | TTask _ -> "return false"
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
                        if not (unionHasPayload value) then
                            "return left == right"
                        else
                            let clauses =
                                cases |> Map.toList |> List.map (fun (caseName, payload) ->
                                    let caseType = unionCaseType value caseName
                                    match payload with
                                    | None -> $"case {caseType}:\n\t\t_, ok := right.({caseType})\n\t\treturn ok"
                                    | Some payloadType -> $"case {caseType}:\n\t\trightValue, ok := right.({caseType})\n\t\treturn ok && {equality payloadType}(leftValue.Value, rightValue.Value)")
                            let renderedClauses = String.concat "\n\t" clauses
                            $"switch leftValue := left.(type) {{\n\t{renderedClauses}\n\t}}\n\treturn false"
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
                            if not (unionHasPayload value) then
                                "return left == right"
                            else
                                let clauses =
                                    cases |> Map.toList |> List.map (fun (caseName, payload) ->
                                        let caseType = unionCaseType value caseName
                                        match payload with
                                        | None -> $"case {caseType}:\n\t\t_, ok := right.({caseType})\n\t\treturn ok"
                                        | Some payloadType -> $"case {caseType}:\n\t\trightValue, ok := right.({caseType})\n\t\treturn ok && {equality payloadType}(leftValue.Value, rightValue.Value)")
                                let renderedClauses = String.concat "\n\t" clauses
                                $"switch leftValue := left.(type) {{\n\t{renderedClauses}\n\t}}\n\treturn false"
                        | _ -> "return left == right"
                    | other -> invalidArg "value" $"No typed equality for {Types.typeToString other}."
                equalityBodies.Add($"func {name}(left {goType value}, right {goType value}) bool {{\n\t{body}\n}}")
                name

        let names = program.Declarations |> List.map (fun item -> item.Id, symbolName "value" item.Id) |> Map.ofList
        let declarationTypes = program.Declarations |> List.map (fun item -> item.Id, item.Type) |> Map.ofList
        let declarationNames = program.Declarations |> List.map (fun item -> item.Id, item.Name) |> Map.ofList
        let declarationValues = program.Declarations |> List.map (fun item -> item.Id, item.Value) |> Map.ofList
        let declarationDefinitions = program.Declarations |> List.map (fun item -> item.Id, item) |> Map.ofList
        let recordFields = function
            | TRecord fields -> fields
            | TNamed name ->
                match Map.tryFind name namedDefinitions with
                | Some(TRecord fields) -> fields
                | _ -> invalidOp $"Named type '{name}' is not a record."
            | value -> invalidOp $"Type '{Types.typeToString value}' is not a record."
        let intrinsicSource appliedArguments (name: string) (intrinsicType: Type) =
            let rec curry index currentType arguments body =
                match currentType with
                | TFun(argumentType, resultType) ->
                    let argument = $"intrinsicArg{index}"
                    let inner = curry (index + 1) resultType (arguments @ [ argument ]) body
                    $"func({argument} {goType argumentType}) {goType resultType} {{\n\treturn {indent inner}\n}}"
                | resultType -> body arguments resultType
            let rec signature currentType argumentTypes =
                match currentType with
                | TFun(argumentType, resultType) -> signature resultType (argumentTypes @ [ argumentType ])
                | resultType -> argumentTypes, resultType
            let build body =
                match appliedArguments with
                | None -> curry 0 intrinsicType [] body
                | Some arguments ->
                    let argumentTypes, resultType = signature intrinsicType []
                    let names = argumentTypes |> List.mapi (fun index _ -> $"intrinsicArg{index}")
                    let parameters = List.map2 (fun name argumentType -> $"{name} {goType argumentType}") names argumentTypes |> String.concat ", "
                    let values = String.concat ", " arguments
                    $"(func({parameters}) {goType resultType} {{\n\treturn {indent (body names resultType)}\n}})({values})"
            let iife resultType body = $"func() {goType resultType} {{\n\t{indent body}\n}}()"
            let sortedMap resultType setup =
                let keyType = match resultType with TMap(keyType, _) -> keyType | _ -> invalidOp "Expected map result."
                let ordering = mapKeyLess keyType "result.Entries[inner].Key" "result.Entries[inner-1].Key"
                iife resultType $"{setup}\nfor outer := 1; outer < len(result.Entries); outer++ {{\n\tfor inner := outer; inner > 0 && ({ordering}); inner-- {{\n\t\tresult.Entries[inner], result.Entries[inner-1] = result.Entries[inner-1], result.Entries[inner]\n\t}}\n}}\nreturn result"
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
            | "Map.ofList", TFun(TList(TTuple [ keyType; valueType ]), resultType) ->
                let entryType = $"entry_{suffix resultType}"
                build (fun args _ -> sortedMap resultType $"result := {goType resultType}{{}}\nset := func(key {goType keyType}, value {goType valueType}) {{\n\tfor index := range result.Entries {{\n\t\tif result.Entries[index].Key == key {{\n\t\t\tresult.Entries[index].Value = value\n\t\t\treturn\n\t\t}}\n\t}}\n\tresult.Entries = append(result.Entries, {entryType}{{Key: key, Value: value}})\n}}\nfor _, pair := range {args.[0]} {{\n\tset(pair.Item0, pair.Item1)\n}}")
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
                build (fun args resultType -> iife resultType $"_, _ = os.Stdout.WriteString({args.[0]} + \"\\n\")\nreturn struct{{}}{{}}")
            | "Console.readLine", TFun(TUnit, TOption TString) ->
                build (fun _ resultType -> iife resultType $"if consoleScanner.Scan() {{\n\treturn {goType resultType}{{HasValue: true, Value: consoleScanner.Text()}}\n}}\nreturn {goType resultType}{{}}")
            | "Fs.readText", TFun(TString, TOption TString) ->
                build (fun args resultType -> iife resultType $"full, ok := fsResolve({args.[0]})\nif !ok {{\n\treturn {goType resultType}{{}}\n}}\ncontent, err := os.ReadFile(full)\nif err != nil {{\n\treturn {goType resultType}{{}}\n}}\nreturn {goType resultType}{{HasValue: true, Value: string(content)}}")
            | _ -> invalidOp $"Unsupported typed intrinsic '{name}' at {Types.typeToString intrinsicType}."
        let intrinsic name intrinsicType = intrinsicSource None name intrinsicType
        let appliedIntrinsic name intrinsicType arguments = intrinsicSource (Some arguments) name intrinsicType
        let hostExternal (name: string) (externalType: Type) =
            match name, externalType with
            | "Console.writeLine", TFun(TString, TUnit) ->
                "func(value string) struct{} {\n\t_, _ = os.Stdout.WriteString(value + \"\\n\")\n\treturn struct{}{}\n}"
            | "Console.readLine", TFun(TUnit, TOption TString) ->
                let resultType = goType (TOption TString)
                $"func(_ struct{{}}) {resultType} {{\n\tif consoleScanner.Scan() {{\n\t\treturn {resultType}{{HasValue: true, Value: consoleScanner.Text()}}\n\t}}\n\treturn {resultType}{{}}\n}}"
            | "Fs.readText", TFun(TString, TOption TString) ->
                let resultType = goType (TOption TString)
                $"func(path string) {resultType} {{\n\tfull, ok := fsResolve(path)\n\tif !ok {{\n\t\treturn {resultType}{{}}\n\t}}\n\tcontent, err := os.ReadFile(full)\n\tif err != nil {{\n\t\treturn {resultType}{{}}\n\t}}\n\treturn {resultType}{{HasValue: true, Value: string(content)}}\n}}"
            | "Regex.matchGroups", TFun(TString, TFun(TString, TOption(TList TString))) ->
                let resultType = goType (TOption(TList TString))
                $"func(pattern string) func(string) {resultType} {{\n\treturn func(input string) {resultType} {{\n\t\tgroups, ok := fsRegexMatchGroups(pattern, input)\n\t\tif !ok {{\n\t\t\treturn {resultType}{{}}\n\t\t}}\n\t\treturn {resultType}{{HasValue: true, Value: groups}}\n\t}}\n}}"
            | "Fs.exists", TFun(TString, TBool) ->
                "func(path string) bool {\n\tfull, ok := fsResolve(path)\n\tif !ok {\n\t\treturn false\n\t}\n\t_, err := os.Stat(full)\n\treturn err == nil\n}"
            | "Fs.kind", TFun(TString, resultType) ->
                let result = goType resultType
                let missing = unionCaseType resultType "Missing"
                let directory = unionCaseType resultType "Directory"
                let file = unionCaseType resultType "File"
                $"func(path string) {result} {{\n\tfull, ok := fsResolve(path)\n\tif !ok {{\n\t\treturn {missing}{{}}\n\t}}\n\tinfo, err := os.Stat(full)\n\tif err != nil {{\n\t\treturn {missing}{{}}\n\t}}\n\trelative := fsRelative(full)\n\tif info.IsDir() {{\n\t\treturn {directory}{{Value: relative}}\n\t}}\n\treturn {file}{{Value: relative}}\n}}"
            | "Fs.createDirectory", TFun(TString, TBool) ->
                "func(path string) bool {\n\tfull, ok := fsResolve(path)\n\tif !ok {\n\t\treturn false\n\t}\n\tif os.MkdirAll(full, 0755) != nil {\n\t\treturn false\n\t}\n\tinfo, err := os.Stat(full)\n\treturn err == nil && info.IsDir()\n}"
            | "Fs.writeText", TFun(TString, TFun(TString, TBool)) ->
                "func(path string) func(string) bool {\n\treturn func(content string) bool {\n\t\tfull, ok := fsResolve(path)\n\t\tif !ok {\n\t\t\treturn false\n\t\t}\n\t\tparent := filepath.Dir(full)\n\t\tif os.MkdirAll(parent, 0755) != nil {\n\t\t\treturn false\n\t\t}\n\t\treturn os.WriteFile(full, []byte(content), 0644) == nil\n\t}\n}"
            | "Fs.combinePath", TFun(TString, TFun(TString, TString)) ->
                "func(left string) func(string) string {\n\treturn func(right string) string { return filepath.ToSlash(filepath.Join(left, right)) }\n}"
            | "Fs.parentDirectory", TFun(TString, TOption TString) ->
                let resultType = goType (TOption TString)
                $"func(path string) {resultType} {{\n\tcleaned := filepath.Clean(path)\n\tparent := filepath.Dir(cleaned)\n\tif parent == \".\" || parent == cleaned {{\n\t\treturn {resultType}{{}}\n\t}}\n\treturn {resultType}{{HasValue: true, Value: filepath.ToSlash(parent)}}\n}}"
            | "Fs.extension", TFun(TString, TOption TString) ->
                let resultType = goType (TOption TString)
                $"func(path string) {resultType} {{\n\textension := filepath.Ext(path)\n\tif extension == \"\" {{\n\t\treturn {resultType}{{}}\n\t}}\n\treturn {resultType}{{HasValue: true, Value: extension}}\n}}"
            | "Fs.fileNameWithoutExtension", TFun(TString, TString) ->
                "func(path string) string {\n\tbase := filepath.Base(path)\n\treturn strings.TrimSuffix(base, filepath.Ext(base))\n}"
            | "Fs.glob", TFun(TString, TOption(TList TString)) ->
                let resultType = goType (TOption(TList TString))
                $"func(pattern string) {resultType} {{\n\tvalues, ok := fsGlob(\".\", pattern)\n\tif !ok {{\n\t\treturn {resultType}{{}}\n\t}}\n\treturn {resultType}{{HasValue: true, Value: values}}\n}}"
            | "Fs.enumerateFiles", TFun(TString, TFun(TString, TOption(TList TString))) ->
                let resultType = goType (TOption(TList TString))
                $"func(directory string) func(string) {resultType} {{\n\treturn func(pattern string) {resultType} {{\n\t\tvalues, ok := fsGlob(directory, pattern)\n\t\tif !ok {{\n\t\t\treturn {resultType}{{}}\n\t\t}}\n\t\treturn {resultType}{{HasValue: true, Value: values}}\n\t}}\n}}"
            | "Task.spawn", TFun(TFun(TUnit, resultType), TTask taskResult) when resultType = taskResult ->
                let taskType = goType (TTask taskResult)
                $"func(thunk func(struct{{}}) {goType taskResult}) {taskType} {{\n\tdone := make(chan struct{{}})\n\tresult := new({goType taskResult})\n\tgo func() {{\n\t\tdefer close(done)\n\t\t*result = thunk(struct{{}}{{}})\n\t}}()\n\treturn {taskType}{{Done: done, Result: result}}\n}}"
            | "Task.await", TFun(TTask taskResult, resultType) when resultType = taskResult ->
                $"func(task {goType (TTask taskResult)}) {goType resultType} {{\n\t<-task.Done\n\treturn *task.Result\n}}"
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
        let rec coerce sourceType targetType code =
            if sourceType = targetType then code
            else
                match sourceType, targetType with
                | TNamed _, TRecord _
                | TRecord _, TNamed _ ->
                    let sourceFields = recordFields sourceType
                    let targetFields = recordFields targetType
                    if sourceFields.Count <> targetFields.Count || (sourceFields |> Map.exists (fun name _ -> not (Map.containsKey name targetFields))) then code
                    else
                        let assignments =
                            targetFields
                            |> Map.toList
                            |> List.map (fun (name, targetFieldType) ->
                                let sourceFieldType = sourceFields.[name]
                                let value = coerce sourceFieldType targetFieldType $"converted.{emittedField sourceType name}"
                                $"{emittedField targetType name}: {value}")
                            |> String.concat ", "
                        $"func() {goType targetType} {{\n\tconverted := {indent code}\n\treturn {goType targetType}{{{assignments}}}\n}}()"
                | TList sourceItem, TList targetItem when sourceItem <> targetItem ->
                    let convertedItem = coerce sourceItem targetItem "item"
                    $"func() {goType targetType} {{\n\tsource := {indent code}\n\tresult := make({goType targetType}, 0, len(source))\n\tfor _, item := range source {{\n\t\tresult = append(result, {indent (indent convertedItem)})\n\t}}\n\treturn result\n}}()"
                | TOption sourceItem, TOption targetItem when sourceItem <> targetItem ->
                    let convertedItem = coerce sourceItem targetItem "source.Value"
                    $"func() {goType targetType} {{\n\tsource := {indent code}\n\tif !source.HasValue {{\n\t\treturn {goType targetType}{{}}\n\t}}\n\treturn {goType targetType}{{HasValue: true, Value: {indent convertedItem}}}\n}}()"
                | TFun(sourceArgument, sourceResult), TFun(targetArgument, targetResult) ->
                    let argument = coerce targetArgument sourceArgument "argument"
                    let result = coerce sourceResult targetResult $"source({argument})"
                    $"func() {goType targetType} {{\n\tsource := {indent code}\n\treturn func(argument {goType targetArgument}) {goType targetResult} {{\n\t\treturn {indent (indent result)}\n\t}}\n}}()"
                | _ -> code
        let rec emittedType (expression: Lir.Expression) =
            match expression.Node with
            | Lir.Load id -> Map.tryFind id declarationTypes |> Option.defaultValue expression.Type
            | Lir.MakeClosure(id, _) ->
                let fn = functions.[id]
                TFun(fn.ParameterType, fn.ReturnType)
            | Lir.CallClosure(fn, _) | Lir.PipeClosure(_, fn)
            | Lir.TailCallClosure(fn, _) | Lir.TailPipeClosure(_, fn) ->
                match emittedType fn with
                | TFun(_, resultType) -> resultType
                | _ -> expression.Type
            | _ -> expression.Type
        let declarationFunctionTargets =
            program.Declarations
            |> List.choose (fun declaration ->
                match declaration.Value.Node with
                | Lir.MakeClosure(functionId, _) -> Some(declaration.Id, functionId)
                | _ -> None)
            |> Map.ofList
        let functionChain root =
            let rec collect visited functionId =
                if Set.contains functionId visited then []
                else
                    let fn = functions.[functionId]
                    let rest =
                        match fn.Body.Node with
                        | Lir.MakeClosure(next, _) -> collect (Set.add functionId visited) next
                        | _ -> []
                    fn :: rest
            collect Set.empty root
        let raisedDeclarations =
            declarationFunctionTargets
            |> Map.map (fun symbolId functionId ->
                let chain = functionChain functionId
                $"raised{Lir.symbolNumber symbolId}", chain)
        let representationHelperBodies = Dictionary<string, string>()
        let unwindApplications expression =
            let rec unwind (value: Lir.Expression) =
                match value.Node with
                | Lir.CallClosure(fn, argument) ->
                    let root, arguments = unwind fn
                    root, arguments @ [ argument ]
                | _ -> value, []
            unwind expression
        let rec functionArity = function TFun(_, resultType) -> 1 + functionArity resultType | _ -> 0
        let tryAppliedIntrinsic (expression: Lir.Expression) =
            let candidate =
                match expression.Node with
                | Lir.CallClosure _ -> Some(unwindApplications expression)
                | Lir.TailCallClosure(fn, argument) ->
                    let root, preceding = unwindApplications fn
                    Some(root, preceding @ [ argument ])
                | Lir.PipeClosure(argument, fn) | Lir.TailPipeClosure(argument, fn) ->
                    let root, preceding = unwindApplications fn
                    Some(root, preceding @ [ argument ])
                | _ -> None
            candidate
            |> Option.bind (fun (root, arguments) ->
                match root.Node with
                | Lir.Load id when arguments.Length = (Map.tryFind id declarationTypes |> Option.map functionArity |> Option.defaultValue -1) ->
                    match Map.tryFind id declarationValues |> Option.map _.Node with
                    | Some(Lir.Intrinsic name) -> Some(name, declarationTypes.[id], arguments)
                    | _ -> None
                | _ -> None)
        let tryConsoleWrite (expression: Lir.Expression) =
            let candidate =
                match expression.Node with
                | Lir.CallClosure _ -> Some(unwindApplications expression)
                | Lir.TailCallClosure(fn, argument) ->
                    let root, preceding = unwindApplications fn
                    Some(root, preceding @ [ argument ])
                | _ -> None
            candidate
            |> Option.bind (fun (root, arguments) ->
                match root.Node, arguments with
                | Lir.Load id, [ argument ] when Map.tryFind id declarationNames = Some "Console.writeLine" -> Some argument
                | _ -> None)
        let parameterIsProjectionOnly parameterId expression =
            let rec visit visitedFunctions (value: Lir.Expression) =
                let recurse = visit visitedFunctions
                let all values = List.forall recurse values
                match value.Node with
                | Lir.Load id -> id <> parameterId
                | Lir.FieldGet({ Node = Lir.Load id }, _) when id = parameterId -> true
                | Lir.MakeClosure(functionId, _) ->
                    Set.contains functionId visitedFunctions
                    || visit (Set.add functionId visitedFunctions) functions.[functionId].Body
                | Lir.CallClosure(left, right) | Lir.PipeClosure(left, right)
                | Lir.TailCallClosure(left, right) | Lir.TailPipeClosure(left, right)
                | Lir.Cons(left, right) | Lir.Append(left, right) | Lir.Range(left, right) | Lir.IndexGet(left, right) -> all [ left; right ]
                | Lir.Branch(condition, whenTrue, whenFalse) -> all [ condition; whenTrue; whenFalse ]
                | Lir.Operation(_, values) | Lir.Tuple values | Lir.List values -> all values
                | Lir.Record fields -> fields |> List.map snd |> all
                | Lir.FieldGet(target, _) | Lir.SomeValue target | Lir.Fail target | Lir.RecordConvert(target, _) -> recurse target
                | Lir.Let(_, _, _, binding, body) -> all [ binding; body ]
                | Lir.LetRecursiveGroup(bindings, body) -> all (body :: (bindings |> List.map (fun (_, _, _, item) -> item)))
                | Lir.DecisionTree(target, cases) ->
                    let targetIsCompatibleRecordPlan =
                        match target.Node with
                        | Lir.Load id when id = parameterId ->
                            cases
                            |> List.forall (fun case ->
                                case.Plan.Maps.IsEmpty
                                && case.Plan.Checks
                                   |> List.forall (function Lir.RecordFieldCheck _ | Lir.TypeCheck _ -> true | _ -> false))
                        | _ -> recurse target
                    targetIsCompatibleRecordPlan
                    && (cases |> List.forall (fun case -> Option.forall recurse case.Guard && recurse case.Body))
                | Lir.RecordUpdate(target, fields) -> recurse target && (fields |> List.map snd |> all)
                | Lir.Interpolate parts -> parts |> List.forall (function Lir.TextPart _ -> true | Lir.ValuePart item -> recurse item)
                | Lir.UnionCase(_, _, payload) -> Option.forall recurse payload
                | Lir.Map entries -> entries |> List.forall (function Lir.KeyValue(key, item) -> all [ key; item ] | Lir.Spread item -> recurse item)
                | Lir.ForEach(_, _, _, source, body) -> all [ source; body ]
                | _ -> true
            visit Set.empty expression
        let rec pureTotalWith candidates (expression: Lir.Expression) =
            let isPure = pureTotalWith candidates
            let all = List.forall isPure
            match expression.Node with
            | Lir.Constant _ | Lir.Load _ | Lir.Intrinsic _ | Lir.TypeToken _ | Lir.RuntimeContext -> true
            | Lir.HostExternal _ | Lir.TargetImport _ | Lir.Fail _ -> false
            | Lir.MakeClosure _ -> true
            | Lir.CallClosure _ | Lir.TailCallClosure _ ->
                let root, arguments = unwindApplications expression
                match root.Node with
                | Lir.Load id -> Set.contains id candidates && all arguments
                | Lir.MakeClosure(functionId, _) -> all arguments && isPure functions.[functionId].Body
                | _ -> false
            | Lir.PipeClosure(argument, fn) | Lir.TailPipeClosure(argument, fn) ->
                let root, preceding = unwindApplications fn
                let arguments = preceding @ [ argument ]
                match root.Node with
                | Lir.Load id -> Set.contains id candidates && all arguments
                | Lir.MakeClosure(functionId, _) -> all arguments && isPure functions.[functionId].Body
                | _ -> false
            | Lir.Branch(condition, whenTrue, whenFalse) -> all [ condition; whenTrue; whenFalse ]
            | Lir.Operation((Lir.IntDivide | Lir.IntRemainder), _) -> false
            | Lir.Operation(_, values) | Lir.Tuple values | Lir.List values -> all values
            | Lir.Record fields -> fields |> List.map snd |> all
            | Lir.FieldGet(target, _) | Lir.SomeValue target | Lir.RecordConvert(target, _) -> isPure target
            | Lir.NoneValue -> true
            | Lir.Cons(left, right) | Lir.Append(left, right) | Lir.Range(left, right) | Lir.IndexGet(left, right) -> all [ left; right ]
            | Lir.Let(_, _, _, binding, body) -> all [ binding; body ]
            | Lir.LetRecursiveGroup(bindings, body) -> all (body :: (bindings |> List.map (fun (_, _, _, value) -> value)))
            | Lir.DecisionTree(target, cases) ->
                let exhaustive =
                    cases
                    |> List.tryLast
                    |> Option.exists (fun case -> case.Guard.IsNone && case.Plan.Checks.IsEmpty && case.Plan.Maps.IsEmpty)
                exhaustive && isPure target && (cases |> List.forall (fun case -> Option.forall isPure case.Guard && isPure case.Body))
            | Lir.RecordUpdate(target, fields) -> isPure target && (fields |> List.map snd |> all)
            | Lir.Interpolate parts -> parts |> List.forall (function Lir.TextPart _ -> true | Lir.ValuePart value -> isPure value)
            | Lir.UnionCase(_, _, payload) -> Option.forall isPure payload
            | Lir.Map entries -> entries |> List.forall (function Lir.KeyValue(key, value) -> all [ key; value ] | Lir.Spread value -> isPure value)
            | Lir.ForEach(_, _, _, source, body) -> all [ source; body ]
        let pureTotalDeclarations =
            let isPureIntrinsic (name: string) =
                not (name.StartsWith("Console.", StringComparison.Ordinal)
                     || name.StartsWith("Fs.", StringComparison.Ordinal)
                     || name.StartsWith("Task.", StringComparison.Ordinal))
            let initial =
                program.Declarations
                |> List.choose (fun declaration ->
                    match declaration.Value.Node with
                    | Lir.Intrinsic name when isPureIntrinsic name -> Some declaration.Id
                    | Lir.MakeClosure _ -> Some declaration.Id
                    | _ -> None)
                |> Set.ofList
            let rec prune candidates =
                let retained =
                    candidates
                    |> Set.filter (fun id ->
                        match (program.Declarations |> List.find (fun declaration -> declaration.Id = id)).Value.Node with
                        | Lir.Intrinsic name -> isPureIntrinsic name
                        | Lir.MakeClosure(functionId, _) -> pureTotalWith candidates functions.[functionId].Body
                        | _ -> false)
                if retained = candidates then retained else prune retained
            prune initial
        let callbackIsPureTotal (expression: Lir.Expression) =
            match expression.Node with
            | Lir.MakeClosure(functionId, _) -> pureTotalWith pureTotalDeclarations functions.[functionId].Body
            | Lir.Load id -> Set.contains id pureTotalDeclarations
            | _ -> false
        let tryMapIterFusion (expression: Lir.Expression) =
            let tryPartial expectedName (value: Lir.Expression) =
                match value.Node with
                | Lir.CallClosure({ Node = Lir.Load id }, callback)
                | Lir.TailCallClosure({ Node = Lir.Load id }, callback)
                    when Map.tryFind id declarationNames = Some expectedName -> Some callback
                | _ -> None
            match expression.Node with
            | Lir.PipeClosure(mapped, iterFunction) | Lir.TailPipeClosure(mapped, iterFunction) ->
                match tryPartial "List.iter" iterFunction, mapped.Node with
                | Some consumer, (Lir.PipeClosure(source, mapFunction) | Lir.TailPipeClosure(source, mapFunction)) ->
                    match tryPartial "List.map" mapFunction with
                    | Some mapper when callbackIsPureTotal mapper -> Some(source, mapper, consumer)
                    | _ -> None
                | _ -> None
            | _ -> None
        let tryTailCall functionBindings (expression: Lir.Expression) =
            let candidate =
                match expression.Node with
                | Lir.TailCallClosure(fn, argument) ->
                    let root, arguments = unwindApplications fn
                    Some(root, arguments @ [ argument ], [ 0 .. arguments.Length ])
                | Lir.TailPipeClosure(argument, fn) ->
                    let root, arguments = unwindApplications fn
                    let allArguments = arguments @ [ argument ]
                    Some(root, allArguments, arguments.Length :: [ 0 .. arguments.Length - 1 ])
                | _ -> None
            candidate
            |> Option.bind (fun (root, arguments, evaluationOrder) ->
                match root.Node with
                | Lir.Load symbolId ->
                    functionBindings
                    |> Map.tryFind symbolId
                    |> Option.bind (fun rootFunction ->
                        let chain = functionChain rootFunction
                        match List.tryLast chain with
                        | Some deepest when chain.Length = arguments.Length ->
                            Some(deepest.Id, chain |> List.map (fun fn -> fn.Parameter, fn.ParameterType), arguments, evaluationOrder)
                        | _ -> None)
                | _ -> None)
        let trySelfTail functionBindings currentFunction expression =
            tryTailCall functionBindings expression
            |> Option.bind (fun (targetFunction, parameters, arguments, evaluationOrder) ->
                if targetFunction = currentFunction then Some(parameters, arguments, evaluationOrder) else None)
        let tailTargets functionBindings (expression: Lir.Expression) =
            let rec collect expression =
                match tryTailCall functionBindings expression with
                | Some(target, _, _, _) -> Set.singleton target
                | None ->
                    match expression.Node with
                    | Lir.Branch(_, whenTrue, whenFalse) -> Set.union (collect whenTrue) (collect whenFalse)
                    | Lir.Let(_, _, false, _, body) -> collect body
                    | Lir.DecisionTree(_, cases) when cases |> List.forall (fun case -> case.Plan.Maps.IsEmpty) ->
                        cases |> List.map (fun case -> collect case.Body) |> Set.unionMany
                    | _ -> Set.empty
            collect expression
        let raisedFunctions =
            raisedDeclarations
            |> Map.toList
            |> List.choose (fun (symbolId, (raisedName, chain)) ->
                List.tryLast chain |> Option.map (fun deepest -> deepest.Id, symbolId, raisedName, chain, deepest))
            |> List.sortBy (fun (_, symbolId, _, _, _) -> Lir.symbolNumber symbolId)
        let raisedFunctionIds = raisedFunctions |> List.map (fun (id, _, _, _, _) -> id) |> Set.ofList
        let tailGraph =
            raisedFunctions
            |> List.map (fun (id, _, _, _, deepest) ->
                id, Set.intersect raisedFunctionIds (tailTargets declarationFunctionTargets deepest.Body))
            |> Map.ofList
        let reachableFrom start =
            let rec visit pending visited =
                match pending with
                | [] -> visited
                | current :: rest when Set.contains current visited -> visit rest visited
                | current :: rest ->
                    let next = Map.tryFind current tailGraph |> Option.defaultValue Set.empty |> Set.toList
                    visit (next @ rest) (Set.add current visited)
            visit [ start ] Set.empty
        let mutualTailGroups =
            let mutable assigned = Set.empty
            let mutable nextGroup = 0
            [ for (functionId, _, _, chain, deepest) in raisedFunctions do
                if not (Set.contains functionId assigned) then
                    let reachable = reachableFrom functionId
                    let members =
                        raisedFunctions
                        |> List.filter (fun (candidateId, _, _, _, _) ->
                            candidateId <> functionId
                            && Set.contains candidateId reachable
                            && Set.contains functionId (reachableFrom candidateId))
                    let current = raisedFunctions |> List.find (fun (id, _, _, _, _) -> id = functionId)
                    let stronglyConnected = current :: members
                    let parameterTypes = chain |> List.map _.ParameterType
                    let compatible =
                        stronglyConnected.Length > 1
                        && stronglyConnected
                           |> List.forall (fun (_, _, _, candidateChain, candidate) ->
                               candidateChain.Length = chain.Length
                               && (candidateChain |> List.map _.ParameterType) = parameterTypes
                               && candidate.ReturnType = deepest.ReturnType)
                    if compatible then
                        let group = nextGroup
                        nextGroup <- nextGroup + 1
                        assigned <- Set.union assigned (stronglyConnected |> List.map (fun (id, _, _, _, _) -> id) |> Set.ofList)
                        yield group, stronglyConnected ]
        let mutualTailMembership =
            mutualTailGroups
            |> List.collect (fun (group, members) ->
                members |> List.mapi (fun state (functionId, _, _, _, _) -> functionId, (group, state)))
            |> Map.ofList
        let rec hasSelfTail functionBindings currentFunction (expression: Lir.Expression) =
            if trySelfTail functionBindings currentFunction expression |> Option.isSome then true
            else
                match expression.Node with
                | Lir.Branch(_, whenTrue, whenFalse) -> hasSelfTail functionBindings currentFunction whenTrue || hasSelfTail functionBindings currentFunction whenFalse
                | Lir.Let(_, _, false, _, body) -> hasSelfTail functionBindings currentFunction body
                | Lir.DecisionTree(_, cases) when cases |> List.forall (fun case -> case.Plan.Maps.IsEmpty) ->
                    cases |> List.exists (fun case -> hasSelfTail functionBindings currentFunction case.Body)
                | _ -> false
        let mutable statementTemporary = 0
        let freshStatementName prefix =
            statementTemporary <- statementTemporary + 1
            $"{prefix}{statementTemporary}"

        let rec emit environment functionBindings (expression: Lir.Expression) =
            let emitChild = emit environment functionBindings
            let emitListBuilder (listExpression: Lir.Expression) =
                let itemType = match listExpression.Type with TList item -> item | _ -> invalidOp "List builder received a non-list expression."
                let rec collect (value: Lir.Expression) =
                    match value.Node with
                    | Lir.Append(left, right) -> collect left @ collect right
                    | Lir.Cons(head, tail) -> Choice1Of2 head :: collect tail
                    | Lir.List items -> items |> List.map Choice1Of2
                    | _ -> [ Choice2Of2 value ]
                let pieces = collect listExpression
                let setup, capacity, appends =
                    pieces
                    |> List.mapi (fun index piece ->
                        match piece with
                        | Choice1Of2 item ->
                            let name = freshStatementName "listItem"
                            $"{name} := {indent (coerce (emittedType item) itemType (emitChild item))}", "1", $"result = append(result, {name})"
                        | Choice2Of2 segment ->
                            let name = freshStatementName "listSegment"
                            $"{name} := {indent (coerce (emittedType segment) listExpression.Type (emitChild segment))}", $"len({name})", $"result = append(result, {name}...)" )
                    |> List.fold (fun (setups, capacities, actions) (setup, itemCapacity, action) -> setups @ [ setup ], capacities @ [ itemCapacity ], actions @ [ action ]) ([], [], [])
                let capacityCode = if capacity.IsEmpty then "0" else String.concat "+" capacity
                let setupCode = if setup.IsEmpty then "" else String.concat "\n\t" setup + "\n\t"
                let appendCode = if appends.IsEmpty then "" else String.concat "\n\t" appends + "\n\t"
                $"func() {goType listExpression.Type} {{\n\t{setupCode}result := make({goType listExpression.Type}, 0, {capacityCode})\n\t{appendCode}return result\n}}()"
            match expression.Node with
            | _ when tryAppliedIntrinsic expression |> Option.isSome ->
                let name, intrinsicType, arguments = tryAppliedIntrinsic expression |> Option.get
                let values = arguments |> List.map emitChild
                appliedIntrinsic name intrinsicType values
            | Lir.Constant value -> constant value
            | Lir.Load id -> environment |> Map.tryFind id |> Option.orElseWith (fun () -> Map.tryFind id names) |> Option.defaultWith (fun () -> invalidOp $"Unknown specialized symbol {Lir.symbolNumber id}.")
            | Lir.MakeClosure(id, _) ->
                let fn = functions.[id]
                let parameter = symbolName "arg" fn.Parameter
                let scoped = Map.add fn.Parameter parameter environment
                let body = emitTail scoped functionBindings id fn.ReturnType fn.Body
                if hasSelfTail functionBindings id fn.Body then
                    $"func({parameter} {goType fn.ParameterType}) {goType fn.ReturnType} {{\n\tfor {{\n\t\t{indent (indent body)}\n\t}}\n}}"
                else
                    $"func({parameter} {goType fn.ParameterType}) {goType fn.ReturnType} {{\n\t{indent body}\n}}"
            | Lir.CallClosure(fn, argument) | Lir.TailCallClosure(fn, argument) ->
                let root, preceding = unwindApplications fn
                let arguments = preceding @ [ argument ]
                match root.Node with
                | Lir.Load id when Map.containsKey id raisedDeclarations && (snd raisedDeclarations.[id]).Length = arguments.Length ->
                    let raisedName, chain = raisedDeclarations.[id]
                    let deepest = List.last chain
                    let canSpecializeRepresentation =
                        declarationDefinitions
                        |> Map.tryFind id
                        |> Option.exists (fun declaration ->
                            not declaration.IsRecursive
                            && deepest.Captures |> List.forall (fun capture -> Map.containsKey capture names))
                    let representedArguments =
                        List.map2 (fun (parameter: Lir.Function) (value: Lir.Expression) ->
                            match value.Node with
                            | Lir.RecordConvert(source, targetType)
                                when canSpecializeRepresentation
                                     && targetType = parameter.ParameterType
                                     && goType source.Type <> goType parameter.ParameterType
                                     && parameterIsProjectionOnly parameter.Parameter deepest.Body ->
                                source, source.Type, true
                            | _ -> value, parameter.ParameterType, false) chain arguments
                    let hasRepresentationSpecialization = representedArguments |> List.exists (fun (_, _, changed) -> changed)
                    let callName =
                        if not hasRepresentationSpecialization then raisedName
                        else
                            let representationTypes = representedArguments |> List.map (fun (_, valueType, _) -> valueType)
                            let specializedName = $"{raisedName}_repr_{suffix (TTuple representationTypes)}"
                            if not (representationHelperBodies.ContainsKey specializedName) then
                                let parameters = List.map2 (fun (fn: Lir.Function) valueType -> symbolName "arg" fn.Parameter, valueType) chain representationTypes
                                let scoped = parameters |> List.fold2 (fun current (fn: Lir.Function) (name, _) -> Map.add fn.Parameter name current) names chain
                                let signature = parameters |> List.map (fun (name, valueType) -> $"{name} {goType valueType}") |> String.concat ", "
                                let body = emitTail scoped functionBindings deepest.Id deepest.ReturnType deepest.Body
                                representationHelperBodies.Add(specializedName, $"func {specializedName}({signature}) {goType deepest.ReturnType} {{\n\t{indent body}\n}}")
                            specializedName
                    let values =
                        representedArguments
                        |> List.map (fun (value, valueType, _) -> coerce (emittedType value) valueType (emitChild value))
                        |> String.concat ", "
                    let call = $"{callName}({values})"
                    coerce (List.last chain).ReturnType expression.Type call
                | _ ->
                    let functionType = match fn.Node with Lir.Load id -> Map.tryFind id declarationTypes |> Option.defaultValue fn.Type | _ -> fn.Type
                    let parameterType = match functionType with TFun(parameterType, _) -> parameterType | _ -> argument.Type
                    $"({emitChild fn})({coerce (emittedType argument) parameterType (emitChild argument)})"
            | Lir.PipeClosure(argument, fn) | Lir.TailPipeClosure(argument, fn) ->
                let functionType = match fn.Node with Lir.Load id -> Map.tryFind id declarationTypes |> Option.defaultValue fn.Type | _ -> fn.Type
                let parameterType = match functionType with TFun(parameterType, _) -> parameterType | _ -> argument.Type
                let root, preceding = unwindApplications fn
                match root.Node with
                | Lir.Load id when Map.containsKey id raisedDeclarations && (snd raisedDeclarations.[id]).Length = preceding.Length + 1 ->
                    let raisedName, chain = raisedDeclarations.[id]
                    let precedingSetup, precedingNames =
                        preceding
                        |> List.mapi (fun index value ->
                            let parameter = chain.[index]
                            let name = freshStatementName "pipelineCurriedArgument"
                            $"{name} := {coerce (emittedType value) parameter.ParameterType (emitChild value)}", name)
                        |> List.unzip
                    let values = precedingNames @ [ "pipelineArgument" ] |> String.concat ", "
                    let call = coerce (List.last chain).ReturnType expression.Type $"{raisedName}({values})"
                    let setup = if precedingSetup.IsEmpty then "" else "\n\t" + (precedingSetup |> String.concat "\n\t" |> indent)
                    $"func() {goType expression.Type} {{\n\tpipelineArgument := {indent (coerce (emittedType argument) parameterType (emitChild argument))}{setup}\n\treturn {call}\n}}()"
                | _ ->
                    $"func() {goType expression.Type} {{\n\tpipelineArgument := {indent (coerce (emittedType argument) parameterType (emitChild argument))}\n\tpipelineFunction := {indent (emitChild fn)}\n\treturn pipelineFunction(pipelineArgument)\n}}()"
            | Lir.Branch(condition, whenTrue, whenFalse) -> $"func() {goType expression.Type} {{\n\tif {goCondition (emitChild condition)} {{\n\t\treturn {indent (indent (coerce whenTrue.Type expression.Type (emitChild whenTrue)))}\n\t}}\n\treturn {indent (coerce whenFalse.Type expression.Type (emitChild whenFalse))}\n}}()"
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
                | Lir.IntAdd | Lir.FloatAdd | Lir.NumericAdd -> $"({leftCode} + {rightCode})"
                | Lir.IntSubtract | Lir.FloatSubtract | Lir.NumericSubtract -> $"({leftCode} - {rightCode})"
                | Lir.IntMultiply | Lir.FloatMultiply | Lir.NumericMultiply -> $"({leftCode} * {rightCode})"
                | Lir.IntDivide | Lir.FloatDivide | Lir.NumericDivide ->
                    $"func() {goType expression.Type} {{\n\tleft, right := {indent leftCode}, {indent rightCode}\n\treturn left / right\n}}()"
                | Lir.IntRemainder ->
                    $"func() int64 {{\n\tleft, right := {indent leftCode}, {indent rightCode}\n\treturn left %% right\n}}()"
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
                let destinationFields = recordFields expression.Type
                let values =
                    fields
                    |> List.map (fun (name, value) ->
                        let converted = coerce (emittedType value) destinationFields.[name] (emitChild value)
                        $"{emittedField expression.Type name}: {converted}")
                    |> String.concat ", "
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
            | Lir.Cons _ | Lir.Append _ -> emitListBuilder expression
            | Lir.Let(id, _, recursive, binding, body) ->
                let variable = symbolName "local" id
                let scoped = Map.add id variable environment
                let scopedFunctions =
                    match binding.Node with
                    | Lir.MakeClosure(functionId, _) -> Map.add id functionId functionBindings
                    | _ -> functionBindings
                let bindingCode = emit (if recursive then scoped else environment) (if recursive then scopedFunctions else functionBindings) binding
                let bodyCode = emit scoped scopedFunctions body
                let coercedBinding = coerce (emittedType binding) binding.Type bindingCode
                let declaration = $"var {variable} {goType binding.Type}\n\t{variable} = {indent coercedBinding}"
                $"func() {goType expression.Type} {{\n\t{declaration}\n\t_ = {variable}\n\treturn {indent bodyCode}\n}}()"
            | Lir.LetRecursiveGroup(bindings, body) ->
                let scoped = bindings |> List.fold (fun current (id, _, _, _) -> Map.add id (symbolName "localRec" id) current) environment
                let scopedFunctions =
                    bindings
                    |> List.fold (fun current (id, _, _, value) ->
                        match value.Node with Lir.MakeClosure(functionId, _) -> Map.add id functionId current | _ -> current) functionBindings
                let declarations = bindings |> List.map (fun (id, _, valueType, _) -> let name = symbolName "localRec" id in $"var {name} {goType valueType}") |> String.concat "\n\t"
                let assignments = bindings |> List.map (fun (id, _, _, value) -> let name = symbolName "localRec" id in $"{name} = {indent (emit scoped scopedFunctions value)}") |> String.concat "\n\t"
                $"func() {goType expression.Type} {{\n\t{declarations}\n\t{assignments}\n\treturn {indent (emit scoped scopedFunctions body)}\n}}()"
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
                                if unionHasPayload unionType then
                                    $"({code}).({unionCaseType unionType caseName}).Value", payloadType
                                else
                                    invalidOp "Payload-free union has no payload projection."
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
                        let code, unionType = project plan root rootType [] projection
                        if unionHasPayload unionType then
                            $"func() bool {{ _, ok := ({code}).({unionCaseType unionType caseName}); return ok }}()"
                        else
                            $"({code}) == {unionConstant unionType caseName}"
                let emitCase (case: Lir.DecisionCase) =
                    let rec referencedSymbols (value: Lir.Expression) =
                        let recurse = referencedSymbols
                        let many values = values |> List.map recurse |> Set.unionMany
                        match value.Node with
                        | Lir.Load id -> Set.singleton id
                        | Lir.MakeClosure(_, captures) -> Set.ofList captures
                        | Lir.CallClosure(fn, argument) | Lir.PipeClosure(argument, fn)
                        | Lir.TailCallClosure(fn, argument) | Lir.TailPipeClosure(argument, fn) -> many [ fn; argument ]
                        | Lir.Branch(condition, whenTrue, whenFalse) -> many [ condition; whenTrue; whenFalse ]
                        | Lir.Operation(_, values) | Lir.Tuple values | Lir.List values -> many values
                        | Lir.Record fields -> fields |> List.map snd |> many
                        | Lir.FieldGet(target, _) | Lir.SomeValue target | Lir.Fail target | Lir.RecordConvert(target, _) -> recurse target
                        | Lir.Cons(left, right) | Lir.Append(left, right) | Lir.Range(left, right) | Lir.IndexGet(left, right) -> many [ left; right ]
                        | Lir.Let(_, _, _, binding, body) -> many [ binding; body ]
                        | Lir.LetRecursiveGroup(bindings, body) -> many (body :: (bindings |> List.map (fun (_, _, _, binding) -> binding)))
                        | Lir.DecisionTree(target, cases) ->
                            recurse target :: (cases |> List.collect (fun nested -> [ yield! Option.toList nested.Guard; nested.Body ] |> List.map recurse))
                            |> Set.unionMany
                        | Lir.RecordUpdate(target, fields) -> recurse target :: (fields |> List.map (snd >> recurse)) |> Set.unionMany
                        | Lir.Interpolate parts -> parts |> List.choose (function Lir.TextPart _ -> None | Lir.ValuePart expression -> Some(recurse expression)) |> Set.unionMany
                        | Lir.UnionCase(_, _, payload) -> payload |> Option.map recurse |> Option.defaultValue Set.empty
                        | Lir.Map entries -> entries |> List.map (function Lir.KeyValue(key, item) -> many [ key; item ] | Lir.Spread item -> recurse item) |> Set.unionMany
                        | Lir.ForEach(_, _, _, source, body) -> many [ source; body ]
                        | _ -> Set.empty
                    let usedPatternSymbols =
                        case.Body :: Option.toList case.Guard
                        |> List.map referencedSymbols
                        |> Set.unionMany
                    let rec allBindings (plan: Lir.PatternPlan) =
                        [ yield! plan.Bindings
                          for mapPlan in plan.Maps do
                              for clause in mapPlan.Clauses do
                                  yield! allBindings clause.Key
                                  yield! allBindings clause.Value
                              yield! mapPlan.Tail |> Option.map allBindings |> Option.defaultValue [] ]
                    let groupedBindings = allBindings case.Plan |> List.groupBy _.Id
                    let usedPatternSymbols = Set.union usedPatternSymbols (groupedBindings |> List.choose (fun (id,values) -> if values.Length>1 then Some id else None) |> Set.ofList)
                    let mapBindingSymbols =
                        case.Plan.Maps
                        |> List.collect (fun mapPlan ->
                            [ for clause in mapPlan.Clauses do
                                  yield! allBindings clause.Key
                                  yield! allBindings clause.Value
                              yield! mapPlan.Tail |> Option.map allBindings |> Option.defaultValue [] ])
                        |> List.map _.Id
                        |> Set.ofList
                    let usedBindings = groupedBindings |> List.filter (fun (id, _) -> Set.contains id usedPatternSymbols)
                    let scoped =
                        usedBindings
                        |> List.fold (fun current (id, _) -> Map.add id (symbolName "pattern" id) current) environment
                    let declarations =
                        usedBindings
                        |> List.choose (fun (id, bindings) ->
                            if not (Set.contains id mapBindingSymbols) then None
                            else
                                let binding = List.head bindings
                                let variable = symbolName "pattern" id
                                Some $"var {variable} {goType binding.Type}")
                    let simplePlanWithBound bound initializeLocals (plan: Lir.PatternPlan) root (rootType: Type) =
                        let checks = plan.Checks |> List.map (emitPlanCheck plan root rootType)
                        let grouped = plan.Bindings |> List.groupBy _.Id
                        let repeated =
                            grouped |> List.collect (fun (_, bindings) ->
                                match bindings with
                                | [] | [ _ ] -> []
                                | first :: rest ->
                                    let firstCode, _ = project plan root rootType [] first.Projection
                                    rest |> List.map (fun binding -> let code, _ = project plan root rootType [] binding.Projection in $"{equality binding.Type}({firstCode}, {code})"))
                        let existing = grouped |> List.choose (fun (id,bindings) ->
                            if Set.contains id bound then
                                let binding=List.head bindings
                                let code,_=project plan root rootType [] binding.Projection
                                let variable=symbolName "pattern" id
                                Some $"{equality binding.Type}({variable}, {code})"
                            else None)
                        let assignments =
                            grouped
                            |> List.choose (fun (id, bindings) ->
                                if Set.contains id bound || not (Set.contains id usedPatternSymbols) then None
                                else
                                    let binding = List.head bindings
                                    let code, _ = project plan root rootType [] binding.Projection
                                    let variable = symbolName "pattern" id
                                    let assignment = if initializeLocals && not (Set.contains id mapBindingSymbols) then ":=" else "="
                                    Some $"{variable} {assignment} {code}")
                        let meaningfulChecks = checks @ repeated @ existing |> List.filter ((<>) "true")
                        (match meaningfulChecks with [] -> "true" | values -> String.concat " && " values), assignments
                    let simplePlan = simplePlanWithBound Set.empty
                    let emitMapPlan mapIndex (mapPlan: Lir.MapPlan) =
                        let incoming =
                            case.Plan.Maps |> List.take mapIndex |> List.collect (fun map ->
                                [yield! map.Clauses |> List.collect (fun clause -> [Lir.planBindingIds clause.Key; Lir.planBindingIds clause.Value]); yield! map.Tail |> Option.map Lir.planBindingIds |> Option.toList])
                            |> fun values -> Set.unionMany ((case.Plan.Bindings |> List.map _.Id |> Set.ofList) :: values)
                        let beforeClause index =
                            mapPlan.Clauses |> List.take index |> List.collect (fun clause -> [Lir.planBindingIds clause.Key; Lir.planBindingIds clause.Value]) |> fun values -> Set.unionMany (incoming::values)
                        let mapCode, mapType = project case.Plan targetName target.Type [] mapPlan.Projection
                        let keyType, valueType = match mapType with TMap(keyType, valueType) -> keyType, valueType | _ -> invalidOp "Map pattern projection is not a map."
                        let entryType = $"entry_{suffix mapType}"
                        let clauseAttempt bound locallyBound (clause: Lir.MapClausePlan) nextBody =
                            let keyCondition,keyAssignments=simplePlanWithBound bound false clause.Key "entry.Key" keyType
                            let valueCondition,valueAssignments=simplePlanWithBound (Set.union bound (Lir.planBindingIds clause.Key)) false clause.Value "entry.Value" valueType
                            let nextBody=String.concat "\n" (valueAssignments @ [nextBody])
                            match Lir.simpleKeyBinding clause.Key with
                            | Some id ->
                                let valueAttempt=if valueCondition="true" then nextBody else $"if {valueCondition} {{\n\t{indent nextBody}\n}}\nreturn false"
                                let body=String.concat "\n" (keyAssignments @ [valueAttempt])
                                let attempt=if keyCondition="true" then body else $"if {keyCondition} {{\n\t{indent body}\n}}"
                                if not (Set.contains id locallyBound) && keyCondition<>"true" then attempt+"\nreturn false" else attempt
                            | None ->
                                let conditions=[keyCondition;valueCondition] |> List.filter ((<>) "true")
                                let body=String.concat "\n" (keyAssignments @ [nextBody])
                                match conditions with [] -> body | values -> let condition=String.concat " && " values in $"if {condition} {{\n\t{indent body}\n}}"
                        let directPlan =
                            match mapPlan.Clauses with
                            | [] ->
                                match mapPlan.Tail with
                                | None when mapPlan.RequireEmptyRemainder -> Some $"len(({mapCode}).Entries) == 0"
                                | None -> Some "true"
                                | Some tail ->
                                    let condition, assignments = simplePlanWithBound incoming false tail "remaining" mapType
                                    let success =
                                        if condition = "true" then "return true"
                                        else $"return {condition}"
                                    let body = [ yield $"remaining := {mapCode}"; yield! assignments; yield success ] |> String.concat "\n"
                                    Some $"func() bool {{\n\t{indent body}\n}}()"
                            | [ clause ] ->
                                let tailBody =
                                    match mapPlan.Tail with
                                    | None -> [ "return true" ]
                                    | Some tail ->
                                        let tailCondition, tailAssignments = simplePlanWithBound (beforeClause 1) false tail "remaining" mapType
                                        if tailAssignments.IsEmpty && tailCondition = "true" then [ "return true" ]
                                        else
                                            [ yield $"nextEntries := make([]{entryType}, 0, len(values.Entries)-1)"
                                              yield "nextEntries = append(nextEntries, values.Entries[:entryIndex]...)"
                                              yield "nextEntries = append(nextEntries, values.Entries[entryIndex+1:]...)"
                                              yield $"remaining := {goType mapType}{{Entries: nextEntries}}"
                                              yield! tailAssignments
                                              if tailCondition = "true" then yield "return true"
                                              else yield $"return {tailCondition}" ]
                                let attempt=clauseAttempt incoming Set.empty clause (String.concat "\n" tailBody)
                                let iteration =
                                    match attempt.Contains("entryIndex", StringComparison.Ordinal), attempt.Contains("entry.", StringComparison.Ordinal) with
                                    | true, true -> "entryIndex, entry := range values.Entries"
                                    | true, false -> "entryIndex := range values.Entries"
                                    | false, true -> "_, entry := range values.Entries"
                                    | false, false -> "range values.Entries"
                                Some $"func() bool {{\n\tvalues := {indent mapCode}\n\tfor {iteration} {{\n\t\t{indent (indent attempt)}\n\t}}\n\treturn false\n}}()"
                            | _ -> None
                        let clauseCases =
                            mapPlan.Clauses
                            |> List.mapi (fun clauseIndex clause ->
                                let bound=beforeClause clauseIndex
                                let attemptBody =
                                    [ yield $"nextEntries := make([]{entryType}, 0, len(remaining.Entries)-1)"
                                      yield "nextEntries = append(nextEntries, remaining.Entries[:entryIndex]...)"
                                      yield "nextEntries = append(nextEntries, remaining.Entries[entryIndex+1:]...)"
                                      yield $"return matchClause(clauseIndex+1, {goType mapType}{{Entries: nextEntries}})" ]
                                    |> String.concat "\n"
                                let locallyBound=mapPlan.Clauses |> List.take clauseIndex |> List.collect (fun previous -> [Lir.planBindingIds previous.Key;Lir.planBindingIds previous.Value]) |> Set.unionMany
                                let attempt=clauseAttempt bound locallyBound clause attemptBody
                                let iteration =
                                    if attempt.Contains("entry.", StringComparison.Ordinal) then "entryIndex, entry := range"
                                    else "entryIndex := range"
                                $"case {clauseIndex}:\n\t\tfor {iteration} remaining.Entries {{\n\t\t\t{indent (indent (indent attempt))}\n\t\t}}\n\t\treturn false")
                            |> String.concat "\n\t"
                        let terminal =
                            match mapPlan.Tail with
                            | None -> if mapPlan.RequireEmptyRemainder then "return len(remaining.Entries) == 0" else "return true"
                            | Some tail ->
                                let condition, assignments = simplePlanWithBound (beforeClause mapPlan.Clauses.Length) false tail "remaining" mapType
                                let success = [ yield! assignments; yield "return true" ] |> String.concat "\n"
                                if condition = "true" then success
                                else $"if {condition} {{\n\t{indent success}\n}}\nreturn false"
                        let switchSource = if mapPlan.Clauses.IsEmpty then "" else $"\n\t\tswitch clauseIndex {{\n\t\t{indent clauseCases}\n\t\t}}"
                        directPlan
                        |> Option.defaultValue $"func() bool {{\n\tvar matchClause func(int, {goType mapType}) bool\n\tmatchClause = func(clauseIndex int, remaining {goType mapType}) bool {{\n\t\tif clauseIndex == {mapPlan.Clauses.Length} {{\n\t\t\t{indent (indent (indent terminal))}\n\t\t}}{switchSource}\n\t\treturn false\n\t}}\n\treturn matchClause(0, {mapCode})\n}}()"
                    let condition, topAssignments = simplePlan true case.Plan targetName target.Type
                    let mapChecks = case.Plan.Maps |> List.mapi emitMapPlan
                    let bodyCode = coerce case.Body.Type expression.Type (emit scoped functionBindings case.Body)
                    let guardedBody =
                        match case.Guard with
                        | None -> $"return {bodyCode}"
                        | Some guard -> $"if {goCondition (emit scoped functionBindings guard)} {{\n\treturn {indent bodyCode}\n}}"
                    let selection =
                        match mapChecks with
                        | [] -> guardedBody
                        | [ check ] -> $"if {check} {{\n\t{indent guardedBody}\n}}"
                        | first :: remaining ->
                            let mapEvaluation =
                                remaining
                                |> List.map (fun check -> $"if patternsMatch {{\n\tpatternsMatch = {indent check}\n}}")
                            [ yield $"patternsMatch := {first}"
                              yield! mapEvaluation
                              yield $"if patternsMatch {{\n\t{indent guardedBody}\n}}" ]
                            |> String.concat "\n"
                    let contents =
                        [ yield! declarations
                          yield! topAssignments
                          yield selection ]
                        |> String.concat "\n"
                    let nestedContents = indent (indent contents)
                    let rendered = if condition = "true" then $"{{\n\t\t{nestedContents}\n\t}}" else $"if {condition} {{\n\t\t{nestedContents}\n\t}}"
                    rendered, condition = "true" && mapChecks.IsEmpty && case.Guard.IsNone
                let rec emitCases values =
                    match values with
                    | [] -> [], false
                    | case :: rest ->
                        let rendered, terminal = emitCase case
                        if terminal then [ rendered ], true
                        else
                            let remaining, exhaustive = emitCases rest
                            rendered :: remaining, exhaustive
                let emittedCases, exhaustive = emitCases cases
                let caseCode = emittedCases |> String.concat "\n\t"
                let failure = if exhaustive then "" else "\n\tpanic(\"Match failure.\")"
                let discard = if caseCode.Contains(targetName, StringComparison.Ordinal) then "" else $"\n\t_ = {targetName}"
                $"func() {goType expression.Type} {{\n\t{targetName} := {indent (emitChild target)}{discard}\n\t{caseCode}{failure}\n}}()"
            | Lir.RecordUpdate(target, fields) ->
                let targetCode = emitChild target
                let sourceFields = recordFields target.Type
                let destinationFields = recordFields expression.Type
                let assignments =
                    fields
                    |> List.map (fun (name, value) ->
                        $"updated.{emittedField expression.Type name} = {indent (coerce (emittedType value) destinationFields.[name] (emitChild value))}")
                    |> String.concat "\n\t"
                if goType target.Type = goType expression.Type then
                    match expression.Type with
                    | TNamed name when namedRepresentation name = Some(LocalNamed true) -> $"func() {goType expression.Type} {{\n\toriginal := {targetCode}\n\tupdated := *original\n\t{assignments}\n\treturn &updated\n}}()"
                    | _ -> $"func() {goType expression.Type} {{\n\tupdated := {targetCode}\n\t{assignments}\n\treturn updated\n}}()"
                else
                    let copiedFields =
                        destinationFields
                        |> Map.toList
                        |> List.choose (fun (name, destinationFieldType) ->
                            sourceFields
                            |> Map.tryFind name
                            |> Option.map (fun sourceFieldType ->
                                let copied = coerce sourceFieldType destinationFieldType $"original.{emittedField target.Type name}"
                                $"{emittedField expression.Type name}: {copied}"))
                        |> String.concat ", "
                    let initializer =
                        match expression.Type with
                        | TNamed name when namedRepresentation name = Some(LocalNamed true) -> $"&named_{identifier name}{{{copiedFields}}}"
                        | _ -> $"{goType expression.Type}{{{copiedFields}}}"
                    $"func() {goType expression.Type} {{\n\toriginal := {targetCode}\n\tupdated := {initializer}\n\t{assignments}\n\treturn updated\n}}()"
            | Lir.RecordConvert(target, targetType) ->
                let targetCode = emitChild target
                let fields = recordFields targetType
                let assignments = fields |> Map.toList |> List.map (fun (name, _) -> $"{emittedField targetType name}: converted.{emittedField target.Type name}") |> String.concat ", "
                match targetType with
                | TNamed name when namedRepresentation name = Some(LocalNamed true) -> $"func() {goType targetType} {{\n\tconverted := {indent targetCode}\n\treturn &named_{identifier name}{{{assignments}}}\n}}()"
                | _ -> $"func() {goType targetType} {{\n\tconverted := {indent targetCode}\n\treturn {goType targetType}{{{assignments}}}\n}}()"
            | Lir.Range(first, last) ->
                let prefix = $"func() []int64 {{\n\tfirst, last := {emitChild first}, {emitChild last}\n\tresult := []int64{{}}\n"
                let ascending = "\tfor value := first; value <= last; value++ {\n\t\tresult = append(result, value)\n\t}"
                let descending = "\tfor value := first; value >= last; value-- {\n\t\tresult = append(result, value)\n\t}"
                let loop =
                    match tryStaticRangeDirection first last with
                    | Some true -> ascending
                    | Some false -> descending
                    | None -> $"\tif first <= last {{\n\t{indent ascending}\n\t}} else {{\n\t{indent descending}\n\t}}"
                $"{prefix}{loop}\n\treturn result\n}}()"
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
                let bodyCode = emit (Map.add id variable environment) functionBindings body
                $"func() struct{{}} {{\n\tfor _, {variable} := range {emitChild source} {{\n\t\t_ = {indent (indent bodyCode)}\n\t}}\n\treturn struct{{}}{{}}\n}}()"
            | Lir.TypeToken valueType -> $"typeToken{{Value: {Text.Json.JsonSerializer.Serialize(Types.typeToString valueType)}}}"
            | Lir.UnionCase(_, caseName, payload) ->
                if unionHasPayload expression.Type then
                    let caseType = unionCaseType expression.Type caseName
                    match payload with
                    | None -> $"{caseType}{{}}"
                    | Some value -> $"{caseType}{{Value: {emitChild value}}}"
                else
                    unionConstant expression.Type caseName
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
                let ordering = mapKeyLess keyType "result.Entries[inner].Key" "result.Entries[inner-1].Key"
                $"func() {resultType} {{\n\tresult := {resultType}{{}}\n\tset := func(key {goType keyType}, value {goType valueType}) {{\n\t\tfor index := range result.Entries {{\n\t\t\tif {keyEqual}(result.Entries[index].Key, key) {{\n\t\t\t\tresult.Entries[index].Value = value\n\t\t\t\treturn\n\t\t\t}}\n\t\t}}\n\t\tresult.Entries = append(result.Entries, {entryType}{{Key: key, Value: value}})\n\t}}\n\t{entryCode}\n\tfor outer := 1; outer < len(result.Entries); outer++ {{\n\t\tfor inner := outer; inner > 0 && {ordering}; inner-- {{\n\t\t\tresult.Entries[inner], result.Entries[inner-1] = result.Entries[inner-1], result.Entries[inner]\n\t\t}}\n\t}}\n\treturn result\n}}()"
            | node -> invalidOp $"Unsupported typed Go node {node}."

        and emitTail environment functionBindings currentFunction resultType (expression: Lir.Expression) =
            let mutualTail =
                tryTailCall functionBindings expression
                |> Option.bind (fun (targetFunction, parameters, arguments, evaluationOrder) ->
                    match Map.tryFind currentFunction mutualTailMembership, Map.tryFind targetFunction mutualTailMembership with
                    | Some(currentGroup, _), Some(targetGroup, targetState) when currentGroup = targetGroup && targetFunction <> currentFunction ->
                        Some(currentGroup, targetState, parameters, arguments, evaluationOrder)
                    | _ -> None)
            match mutualTail, trySelfTail functionBindings currentFunction expression with
            | Some(group, targetState, parameters, arguments, evaluationOrder), _ ->
                let temporaries =
                    evaluationOrder
                    |> List.map (fun index ->
                        let _, parameterType = parameters.[index]
                        let argument = arguments.[index]
                        let temporary = freshStatementName "mutualTailNext"
                        index, temporary, $"{temporary} := {coerce (emittedType argument) parameterType (emit environment functionBindings argument)}")
                let values = temporaries |> List.sortBy (fun (index, _, _) -> index) |> List.map (fun (_, name, _) -> name)
                String.concat "\n" (temporaries |> List.map (fun (_, _, source) -> source))
                + "\n" + ([ for index in 0 .. parameters.Length - 1 -> $"mutualArg{group}_{index}" ] |> String.concat ", ")
                + " = " + String.concat ", " values
                + $"\nmutualState{group} = {targetState}\ncontinue"
            | None, Some(parameters, arguments, evaluationOrder) ->
                let temporaries =
                    evaluationOrder
                    |> List.map (fun index ->
                        let parameterId, parameterType = parameters.[index]
                        let argument = arguments.[index]
                        let temporary = $"tailNext{Lir.symbolNumber parameterId}"
                        temporary, $"{temporary} := {coerce (emittedType argument) parameterType (emit environment functionBindings argument)}")
                let assignments =
                    parameters
                    |> List.mapi (fun index (parameterId, _) ->
                        let variable = environment |> Map.tryFind parameterId |> Option.defaultWith (fun () -> invalidOp "Tail-call parameter is not available in the generated closure environment.")
                        let temporary = $"tailNext{Lir.symbolNumber parameterId}"
                        variable, temporary)
                let variables, values = assignments |> List.unzip
                String.concat "\n" (temporaries |> List.map snd)
                + "\n" + String.concat ", " variables + " = " + String.concat ", " values
                + "\ncontinue"
            | None, None ->
                match expression.Node with
                | _ when tryAppliedIntrinsic expression |> Option.isSome ->
                    $"return {coerce expression.Type resultType (emit environment functionBindings expression)}"
                | Lir.PipeClosure(argument, fn) | Lir.TailPipeClosure(argument, fn) ->
                    let functionType = match fn.Node with Lir.Load id -> Map.tryFind id declarationTypes |> Option.defaultValue fn.Type | _ -> fn.Type
                    let parameterType = match functionType with TFun(parameterType, _) -> parameterType | _ -> argument.Type
                    let argumentName = freshStatementName "pipelineArgument"
                    let root, preceding = unwindApplications fn
                    match fn.Node, root.Node with
                    | Lir.MakeClosure(functionId, _), _ ->
                        let applied = functions.[functionId]
                        let scoped = Map.add applied.Parameter argumentName environment
                        $"var {argumentName} {goType parameterType}\n{emitAssign environment functionBindings argumentName parameterType argument}\n{emitTail scoped functionBindings currentFunction resultType applied.Body}"
                    | _, Lir.Load id when Map.containsKey id raisedDeclarations && (snd raisedDeclarations.[id]).Length = preceding.Length + 1 ->
                        let raisedName, chain = raisedDeclarations.[id]
                        let precedingTemporaries =
                            List.map2 (fun (parameter: Lir.Function) value ->
                                let name = freshStatementName "pipelineCurriedArgument"
                                name, parameter.ParameterType, value) (chain |> List.take preceding.Length) preceding
                        let setup =
                            precedingTemporaries
                            |> List.map (fun (name, valueType, value) -> $"var {name} {goType valueType}\n{emitAssign environment functionBindings name valueType value}")
                            |> String.concat "\n"
                        let values = (precedingTemporaries |> List.map (fun (name, _, _) -> name)) @ [ argumentName ] |> String.concat ", "
                        let setupSource = if String.IsNullOrEmpty setup then "" else "\n" + setup
                        let call = $"{raisedName}({values})"
                        let typedCall = coerce (List.last chain).ReturnType expression.Type call
                        $"var {argumentName} {goType parameterType}\n{emitAssign environment functionBindings argumentName parameterType argument}{setupSource}\nreturn {coerce expression.Type resultType typedCall}"
                    | _, _ ->
                        let functionName = freshStatementName "pipelineFunction"
                        let call = $"{functionName}({argumentName})"
                        $"var {argumentName} {goType parameterType}\n{emitAssign environment functionBindings argumentName parameterType argument}\nvar {functionName} {goType fn.Type}\n{emitAssign environment functionBindings functionName fn.Type fn}\nreturn {coerce expression.Type resultType call}"
                | Lir.Branch(condition, whenTrue, whenFalse) ->
                    $"if {goCondition (emit environment functionBindings condition)} {{\n\t{indent (emitTail environment functionBindings currentFunction resultType whenTrue)}\n}}\n{emitTail environment functionBindings currentFunction resultType whenFalse}"
                | Lir.Let(id, _, false, binding, body) ->
                    let variable = symbolName "local" id
                    let scoped = Map.add id variable environment
                    let scopedFunctions =
                        match binding.Node with Lir.MakeClosure(functionId, _) -> Map.add id functionId functionBindings | _ -> functionBindings
                    let bodyCode = emitTail scoped scopedFunctions currentFunction resultType body
                    if bodyCode.Contains(variable, StringComparison.Ordinal) then
                        $"var {variable} {goType binding.Type}\n{emitAssign environment functionBindings variable binding.Type binding}\n{bodyCode}"
                    else
                        let bindingCode = emitAssign environment functionBindings "_" binding.Type binding
                        $"{bindingCode}\n{bodyCode}"
                | Lir.DecisionTree(target, cases) when cases |> List.forall (fun case -> case.Plan.Maps.IsEmpty) ->
                    let targetName = "tailMatchValue"
                    let rec project (plan: Lir.PatternPlan) code valueType prefix steps =
                        match steps with
                        | [] -> code, valueType
                        | step :: rest ->
                            let nextCode, nextType =
                                match step, valueType with
                                | Lir.ListHead, TList item -> $"({code})[0]", item
                                | Lir.ListTail, TList item -> $"({code})[1:]", TList item
                                | Lir.TupleItem index, TTuple items -> $"({code}).Item{index}", items.[index]
                                | Lir.RecordField name, recordType -> $"({code}).{emittedField recordType name}", (recordFields recordType).[name]
                                | Lir.OptionValue, TOption item -> $"({code}).Value", item
                                | Lir.UnionPayload, unionType ->
                                    let caseName =
                                        plan.Checks
                                        |> List.tryPick (function Lir.UnionCaseCheck(projection, _, caseName, true) when projection = prefix -> Some caseName | _ -> None)
                                        |> Option.defaultWith (fun () -> invalidOp "Union payload projection has no corresponding case check.")
                                    if unionHasPayload unionType then
                                        $"({code}).({unionCaseType unionType caseName}).Value", (unionCases unionType).[caseName].Value
                                    else
                                        invalidOp "Payload-free union has no payload projection."
                                | _ -> invalidOp $"Invalid typed tail-pattern projection {step} from {Types.typeToString valueType}."
                            project plan nextCode nextType (prefix @ [ step ]) rest
                    let emitCheck plan root rootType = function
                        | Lir.LiteralCheck(projection, literal) ->
                            let code, valueType = project plan root rootType [] projection
                            $"{equality valueType}({code}, {constant literal})"
                        | Lir.EmptyListCheck projection -> let code, _ = project plan root rootType [] projection in $"len({code}) == 0"
                        | Lir.NonEmptyListCheck projection -> let code, _ = project plan root rootType [] projection in $"len({code}) > 0"
                        | Lir.SomeCheck projection -> let code, _ = project plan root rootType [] projection in $"({code}).HasValue"
                        | Lir.NoneCheck projection -> let code, _ = project plan root rootType [] projection in $"!({code}).HasValue"
                        | Lir.UnionCaseCheck(projection, _, caseName, _) ->
                            let code, unionType = project plan root rootType [] projection
                            if unionHasPayload unionType then
                                $"func() bool {{ _, ok := ({code}).({unionCaseType unionType caseName}); return ok }}()"
                            else
                                $"({code}) == {unionConstant unionType caseName}"
                        | Lir.TupleArityCheck _ | Lir.RecordFieldCheck _ | Lir.TypeCheck _ -> "true"
                    let emitCase (case: Lir.DecisionCase) =
                        let grouped = case.Plan.Bindings |> List.groupBy _.Id
                        let scoped =
                            grouped
                            |> List.fold (fun current (id, _) -> Map.add id (symbolName "pattern" id) current) environment
                        let checks = case.Plan.Checks |> List.map (emitCheck case.Plan targetName target.Type)
                        let repeated =
                            grouped
                            |> List.collect (fun (_, bindings) ->
                                match bindings with
                                | [] | [ _ ] -> []
                                | first :: rest ->
                                    let firstCode, _ = project case.Plan targetName target.Type [] first.Projection
                                    rest |> List.map (fun binding -> let code, _ = project case.Plan targetName target.Type [] binding.Projection in $"{equality binding.Type}({firstCode}, {code})"))
                        let condition = checks @ repeated |> List.filter ((<>) "true") |> function [] -> "true" | values -> String.concat " && " values
                        let body = emitTail scoped functionBindings currentFunction resultType case.Body
                        let guardCode = case.Guard |> Option.map (emit scoped functionBindings)
                        let assignments =
                            grouped
                            |> List.choose (fun (id, bindings) ->
                                let variable = symbolName "pattern" id
                                let used = body.Contains(variable, StringComparison.Ordinal) || guardCode |> Option.exists (fun value -> value.Contains(variable, StringComparison.Ordinal))
                                if not used then None
                                else
                                    let binding = List.head bindings
                                    let code, _ = project case.Plan targetName target.Type [] binding.Projection
                                    Some $"{variable} := {code}")
                        let setup = assignments |> String.concat "\n"
                        let guarded =
                            match guardCode with
                            | None -> body
                            | Some guard -> $"if {goCondition guard} {{\n\t{indent body}\n}}"
                        let contents = if String.IsNullOrEmpty setup then guarded else setup + "\n" + guarded
                        let rendered = if condition = "true" then $"{{\n\t{indent contents}\n}}" else $"if {condition} {{\n\t{indent contents}\n}}"
                        rendered, condition = "true" && case.Guard.IsNone
                    let rec emitCases values =
                        match values with
                        | [] -> [], false
                        | case :: rest ->
                            let rendered, terminal = emitCase case
                            if terminal then [ rendered ], true
                            else
                                let remaining, exhaustive = emitCases rest
                                rendered :: remaining, exhaustive
                    let emittedCases, exhaustive = emitCases cases
                    let caseCode = emittedCases |> String.concat "\n"
                    let failure = if exhaustive then "" else "\npanic(\"Match failure.\")"
                    let discard = if caseCode.Contains(targetName, StringComparison.Ordinal) then "" else $"\n_ = {targetName}"
                    $"{targetName} := {emit environment functionBindings target}{discard}\n{caseCode}{failure}"
                | _ -> $"return {coerce expression.Type resultType (emit environment functionBindings expression)}"

        and emitAssign environment functionBindings target targetType (expression: Lir.Expression) =
            let assign value = $"{target} = {coerce expression.Type targetType value}"
            match expression.Node with
            | _ when tryConsoleWrite expression |> Option.isSome ->
                let argument = tryConsoleWrite expression |> Option.get
                let completed = if target = "_" then "" else $"\n{target} = struct{{}}{{}}"
                $"_, _ = os.Stdout.WriteString({emit environment functionBindings argument} + \"\\n\"){completed}"
            | _ when tryMapIterFusion expression |> Option.isSome ->
                let source, mapper, consumer = tryMapIterFusion expression |> Option.get
                let itemType, mappedType =
                    match source.Type, expression.Node with
                    | TList itemType, (Lir.PipeClosure({ Type = TList mappedType }, _) | Lir.TailPipeClosure({ Type = TList mappedType }, _)) -> itemType, mappedType
                    | _ -> invalidOp "List map/iter fusion has inconsistent specialized types."
                let itemName = freshStatementName "fusedItem"
                let mappedName = freshStatementName "fusedMapped"
                let rec inlineExpression inlineEnvironment (value: Lir.Expression) =
                    match value.Node with
                    | Lir.PipeClosure(argument, fn) | Lir.TailPipeClosure(argument, fn) ->
                        inlineExpression inlineEnvironment argument
                        |> Option.bind (fun argumentCode ->
                            let root, preceding = unwindApplications fn
                            match fn.Node, root.Node with
                            | Lir.MakeClosure(functionId, _), _ ->
                                let applied = functions.[functionId]
                                inlineExpression (Map.add applied.Parameter argumentCode inlineEnvironment) applied.Body
                            | _, Lir.Load id when Map.containsKey id raisedDeclarations && (snd raisedDeclarations.[id]).Length = preceding.Length + 1 ->
                                let raisedName, chain = raisedDeclarations.[id]
                                let values = preceding |> List.map (inlineExpression inlineEnvironment)
                                if values |> List.forall Option.isSome then
                                    values |> List.map Option.get |> fun values -> Some(
                                    let arguments = values @ [ argumentCode ] |> String.concat ", "
                                    let call = $"{raisedName}({arguments})"
                                    coerce (List.last chain).ReturnType value.Type call)
                                else None
                            | _ -> None)
                    | Lir.Constant _ | Lir.Load _ | Lir.CallClosure _ | Lir.TailCallClosure _
                    | Lir.Operation _ | Lir.Tuple _ | Lir.List _ | Lir.Record _ | Lir.FieldGet _
                    | Lir.SomeValue _ | Lir.NoneValue | Lir.Cons _ | Lir.Append _ | Lir.Interpolate _
                    | Lir.TypeToken _ | Lir.UnionCase _ -> Some(emit inlineEnvironment functionBindings value)
                    | _ -> None
                let callbackAssignment (callback: Lir.Expression) argumentName resultName resultType =
                    match callback.Node with
                    | Lir.MakeClosure(functionId, _) ->
                        let fn = functions.[functionId]
                        let scoped = Map.add fn.Parameter argumentName environment
                        match inlineExpression scoped fn.Body with
                        | Some code -> $"{resultName} = {coerce fn.Body.Type resultType code}"
                        | None -> emitAssign scoped functionBindings resultName resultType fn.Body
                    | Lir.Load id when resultType = TUnit ->
                        match Map.tryFind id declarationNames with
                        | Some "Console.writeLine" ->
                            let completed = if resultName = "_" then "" else $"\n{resultName} = struct{{}}{{}}"
                            $"_, _ = os.Stdout.WriteString({argumentName} + \"\\n\"){completed}"
                        | _ -> $"{resultName} = ({emit environment functionBindings callback})({argumentName})"
                    | _ -> $"{resultName} = ({emit environment functionBindings callback})({argumentName})"
                let body =
                    let mapped = callbackAssignment mapper itemName mappedName mappedType
                    let consumed = callbackAssignment consumer mappedName target TUnit
                    $"var {mappedName} {goType mappedType}\n{mapped}\n{consumed}"
                match source.Node with
                | Lir.Range(first, last) ->
                    let firstName = freshStatementName "fusedFirst"
                    let lastName = freshStatementName "fusedLast"
                    let completed = if target = "_" then "" else "\n" + assign "struct{}{}"
                    let ascending = $"for {itemName} := {firstName}; {itemName} <= {lastName}; {itemName}++ {{\n\t{indent body}\n}}"
                    let descending = $"for {itemName} := {firstName}; {itemName} >= {lastName}; {itemName}-- {{\n\t{indent body}\n}}"
                    let loop =
                        match tryStaticRangeDirection first last with
                        | Some true -> ascending
                        | Some false -> descending
                        | None -> $"if {firstName} <= {lastName} {{\n\t{indent ascending}\n}} else {{\n\t{indent descending}\n}}"
                    $"{firstName}, {lastName} := {emit environment functionBindings first}, {emit environment functionBindings last}\n{loop}{completed}"
                | _ ->
                    let sourceName = freshStatementName "fusedSource"
                    let completed = if target = "_" then "" else "\n" + assign "struct{}{}"
                    $"var {sourceName} {goType source.Type}\n{emitAssign environment functionBindings sourceName source.Type source}\nfor _, {itemName} := range {sourceName} {{\n\t{indent body}\n}}{completed}"
            | _ when tryAppliedIntrinsic expression |> Option.isSome ->
                assign (emit environment functionBindings expression)
            | Lir.PipeClosure(argument, fn) | Lir.TailPipeClosure(argument, fn) ->
                let functionType = match fn.Node with Lir.Load id -> Map.tryFind id declarationTypes |> Option.defaultValue fn.Type | _ -> fn.Type
                let parameterType = match functionType with TFun(parameterType, _) -> parameterType | _ -> argument.Type
                let argumentName = freshStatementName "pipelineArgument"
                let root, preceding = unwindApplications fn
                match fn.Node, root.Node with
                | Lir.MakeClosure(functionId, _), _ ->
                    let applied = functions.[functionId]
                    let scoped = Map.add applied.Parameter argumentName environment
                    $"var {argumentName} {goType parameterType}\n{emitAssign environment functionBindings argumentName parameterType argument}\n{emitAssign scoped functionBindings target targetType applied.Body}"
                | _, Lir.Load id when Map.containsKey id raisedDeclarations && (snd raisedDeclarations.[id]).Length = preceding.Length + 1 ->
                    let raisedName, chain = raisedDeclarations.[id]
                    let precedingTemporaries =
                        List.map2 (fun (parameter: Lir.Function) value ->
                            let name = freshStatementName "pipelineCurriedArgument"
                            name, parameter.ParameterType, value) (chain |> List.take preceding.Length) preceding
                    let setup =
                        precedingTemporaries
                        |> List.map (fun (name, valueType, value) -> $"var {name} {goType valueType}\n{emitAssign environment functionBindings name valueType value}")
                        |> String.concat "\n"
                    let values = (precedingTemporaries |> List.map (fun (name, _, _) -> name)) @ [ argumentName ] |> String.concat ", "
                    let setupSource = if String.IsNullOrEmpty setup then "" else "\n" + setup
                    let call = $"{raisedName}({values})"
                    let typedCall = coerce (List.last chain).ReturnType expression.Type call
                    $"var {argumentName} {goType parameterType}\n{emitAssign environment functionBindings argumentName parameterType argument}{setupSource}\n{target} = {coerce expression.Type targetType typedCall}"
                | _, _ ->
                    let functionName = freshStatementName "pipelineFunction"
                    let call = $"{functionName}({argumentName})"
                    $"var {argumentName} {goType parameterType}\n{emitAssign environment functionBindings argumentName parameterType argument}\nvar {functionName} {goType fn.Type}\n{emitAssign environment functionBindings functionName fn.Type fn}\n{target} = {coerce expression.Type targetType call}"
            | Lir.Branch(condition, whenTrue, whenFalse) ->
                $"if {goCondition (emit environment functionBindings condition)} {{\n\t{indent (emitAssign environment functionBindings target targetType whenTrue)}\n}} else {{\n\t{indent (emitAssign environment functionBindings target targetType whenFalse)}\n}}"
            | Lir.Let(id, _, recursive, binding, body) ->
                let variable = symbolName "local" id
                let scoped = Map.add id variable environment
                let scopedFunctions =
                    match binding.Node with
                    | Lir.MakeClosure(functionId, _) -> Map.add id functionId functionBindings
                    | _ -> functionBindings
                let bindingEnvironment = if recursive then scoped else environment
                let bindingFunctions = if recursive then scopedFunctions else functionBindings
                $"var {variable} {goType binding.Type}\n{emitAssign bindingEnvironment bindingFunctions variable binding.Type binding}\n_ = {variable}\n{emitAssign scoped scopedFunctions target targetType body}"
            | Lir.LetRecursiveGroup(bindings, body) ->
                let scoped = bindings |> List.fold (fun current (id, _, _, _) -> Map.add id (symbolName "localRec" id) current) environment
                let scopedFunctions =
                    bindings
                    |> List.fold (fun current (id, _, _, value) ->
                        match value.Node with Lir.MakeClosure(functionId, _) -> Map.add id functionId current | _ -> current) functionBindings
                let declarations = bindings |> List.map (fun (id, _, valueType, _) -> let name = symbolName "localRec" id in $"var {name} {goType valueType}")
                let assignments = bindings |> List.map (fun (id, _, valueType, value) -> emitAssign scoped scopedFunctions (symbolName "localRec" id) valueType value)
                String.concat "\n" (declarations @ assignments @ [ emitAssign scoped scopedFunctions target targetType body ])
            | Lir.Range(first, last) ->
                let firstName = freshStatementName "rangeFirst"
                let lastName = freshStatementName "rangeLast"
                let ascending = $"for value := {firstName}; value <= {lastName}; value++ {{\n\t{target} = append({target}, value)\n}}"
                let descending = $"for value := {firstName}; value >= {lastName}; value-- {{\n\t{target} = append({target}, value)\n}}"
                let loop =
                    match tryStaticRangeDirection first last with
                    | Some true -> ascending
                    | Some false -> descending
                    | None -> $"if {firstName} <= {lastName} {{\n\t{indent ascending}\n}} else {{\n\t{indent descending}\n}}"
                $"{firstName}, {lastName} := {emit environment functionBindings first}, {emit environment functionBindings last}\n{target} = []int64{{}}\n{loop}"
            | Lir.ForEach(id, _, _, source, body) ->
                let variable = symbolName "item" id
                let bodyCode = emit (Map.add id variable environment) functionBindings body
                let completed = assign "struct{}{}"
                $"for _, {variable} := range {emit environment functionBindings source} {{\n\t_ = {indent bodyCode}\n}}\n{completed}"
            | Lir.Fail message -> $"panic({emit environment functionBindings message})"
            | _ -> assign (emit environment functionBindings expression)

        let defaultEntry: Lir.Expression = { Type = TUnit; Span = { Start = { Line = 0; Column = 0; File = None }; End = { Line = 0; Column = 0; File = None } }; Node = Lir.Constant Lir.Unit }
        let declarationsById = program.Declarations |> List.map (fun declaration -> declaration.Id, declaration) |> Map.ofList
        let rec resolveEntryAlias omitted (expression: Lir.Expression) =
            match expression.Node with
            | Lir.Load id ->
                match Map.tryFind id declarationsById with
                | Some declaration when not declaration.IsExported && declaration.Name.StartsWith("$expression", StringComparison.Ordinal) ->
                    match declaration.Value.Node with
                    | Lir.Load _ -> resolveEntryAlias (Set.add id omitted) declaration.Value
                    | _ -> expression, omitted
                | _ -> expression, omitted
            | _ -> expression, omitted
        let entry, omittedAliases = resolveEntryAlias Set.empty (program.Entry |> Option.defaultValue defaultEntry)
        let rec emittedReferences visitedFunctions (expression: Lir.Expression) =
            let recurse = emittedReferences visitedFunctions
            let many values = values |> List.map recurse |> Set.unionMany
            match tryMapIterFusion expression with
            | Some(source, mapper, consumer) ->
                let consumerIsDirectConsole =
                    match consumer.Node with
                    | Lir.Load id -> Map.tryFind id declarationNames = Some "Console.writeLine"
                    | _ -> false
                many ([ source; mapper ] @ (if consumerIsDirectConsole then [] else [ consumer ]))
            | None ->
                match expression.Node with
                | _ when tryAppliedIntrinsic expression |> Option.isSome ->
                    let _, _, arguments = tryAppliedIntrinsic expression |> Option.get
                    many arguments
                | _ when tryConsoleWrite expression |> Option.isSome ->
                    tryConsoleWrite expression |> Option.map recurse |> Option.defaultValue Set.empty
                | Lir.Load id -> Set.singleton id
                | Lir.MakeClosure(functionId, _) ->
                    if Set.contains functionId visitedFunctions then Set.empty
                    else emittedReferences (Set.add functionId visitedFunctions) functions.[functionId].Body
                | Lir.CallClosure _ | Lir.TailCallClosure _ ->
                    let root, arguments =
                        match expression.Node with
                        | Lir.CallClosure _ -> unwindApplications expression
                        | Lir.TailCallClosure(fn, argument) ->
                            let root, preceding = unwindApplications fn
                            root, preceding @ [ argument ]
                        | _ -> invalidOp "Expected a closure call."
                    match root.Node with
                    | Lir.Load id when Map.containsKey id raisedDeclarations && (snd raisedDeclarations.[id]).Length = arguments.Length -> many arguments
                    | _ -> many (root :: arguments)
                | Lir.PipeClosure(argument, fn) | Lir.TailPipeClosure(argument, fn) ->
                    let root, preceding = unwindApplications fn
                    match root.Node with
                    | Lir.Load id when Map.containsKey id raisedDeclarations && (snd raisedDeclarations.[id]).Length = preceding.Length + 1 -> many (argument :: preceding)
                    | _ -> many [ argument; fn ]
                | Lir.Branch(condition, whenTrue, whenFalse) -> many [ condition; whenTrue; whenFalse ]
                | Lir.Operation(_, values) | Lir.Tuple values | Lir.List values -> many values
                | Lir.Record fields -> fields |> List.map snd |> many
                | Lir.FieldGet(target, _) | Lir.SomeValue target | Lir.Fail target | Lir.RecordConvert(target, _) -> recurse target
                | Lir.Cons(left, right) | Lir.Append(left, right) | Lir.Range(left, right) | Lir.IndexGet(left, right) -> many [ left; right ]
                | Lir.Let(_, _, _, binding, body) -> many [ binding; body ]
                | Lir.LetRecursiveGroup(bindings, body) -> many (body :: (bindings |> List.map (fun (_, _, _, value) -> value)))
                | Lir.DecisionTree(target, cases) -> recurse target :: (cases |> List.collect (fun case -> [ yield! Option.toList case.Guard; case.Body ] |> List.map recurse)) |> Set.unionMany
                | Lir.RecordUpdate(target, fields) -> recurse target :: (fields |> List.map (snd >> recurse)) |> Set.unionMany
                | Lir.Interpolate parts -> parts |> List.choose (function Lir.TextPart _ -> None | Lir.ValuePart value -> Some(recurse value)) |> Set.unionMany
                | Lir.UnionCase(_, _, payload) -> payload |> Option.map recurse |> Option.defaultValue Set.empty
                | Lir.Map entries -> entries |> List.map (function Lir.KeyValue(key, value) -> many [ key; value ] | Lir.Spread value -> recurse value) |> Set.unionMany
                | Lir.ForEach(_, _, _, source, body) -> many [ source; body ]
                | _ -> Set.empty
        let referencedSymbols =
            entry :: (program.Declarations |> List.filter (fun declaration -> not (Set.contains declaration.Id omittedAliases)) |> List.map _.Value)
            |> List.map (emittedReferences Set.empty)
            |> Set.unionMany
        let needsCarrier (declaration: Lir.Declaration) =
            declaration.IsExported || Set.contains declaration.Id referencedSymbols || not (Map.containsKey declaration.Id raisedDeclarations)
        let emittedDeclarations =
            program.Declarations
            |> List.filter (fun declaration ->
                not (Set.contains declaration.Id omittedAliases)
                && (match declaration.Value.Node with
                    | Lir.Intrinsic _ -> declaration.IsExported || Set.contains declaration.Id referencedSymbols
                    | Lir.MakeClosure _ -> needsCarrier declaration
                    | _ -> true))
        let unitEntryId =
            match entry.Node with
            | Lir.Load id ->
                match Map.tryFind id declarationsById with
                | Some declaration when declaration.Name = "main" && declaration.Type = TUnit && not declaration.IsExported -> Some id
                | _ -> None
            | _ -> None
        let runtimeEntry = if unitEntryId.IsSome then { entry with Node = Lir.Constant Lir.Unit } else entry

        let normalizedSourcePaths: string list =
            [ yield! program.Types |> List.choose (fun item -> item.Span.Start.File)
              yield! program.Functions |> List.choose (fun item -> item.Span.Start.File)
              yield! program.Declarations |> List.choose (fun item -> item.Span.Start.File) ]
            |> List.map (fun path -> Path.GetFullPath(path).Replace('\\', '/'))
            |> List.distinct
            |> List.sort
        let sourceRoot =
            match normalizedSourcePaths with
            | [] -> None
            | first :: rest ->
                let firstDirectory = Path.GetDirectoryName(first) |> Option.ofObj |> Option.defaultValue first
                let rec commonDirectory (candidate: string) =
                    if rest |> List.forall (fun path -> path.StartsWith(candidate.TrimEnd('/') + "/", StringComparison.Ordinal)) then candidate
                    else
                        match Path.GetDirectoryName(candidate) with
                        | null -> candidate
                        | parent when parent = candidate -> candidate
                        | parent -> commonDirectory ((parent: string).Replace('\\', '/'))
                Some(commonDirectory (firstDirectory.Replace('\\', '/')))
        let relativeSourcePath (path: string) =
            let normalized = Path.GetFullPath(path).Replace('\\', '/')
            match sourceRoot with
            | Some root -> Path.GetRelativePath(root, normalized).Replace('\\', '/')
            | None -> Path.GetFileName(normalized) |> Option.ofObj |> Option.defaultValue normalized
        let sourceFileNames =
            let candidates =
                normalizedSourcePaths
                |> List.map (fun path ->
                    let relative = relativeSourcePath path
                    let withoutExtension =
                        if relative.EndsWith(".fss", StringComparison.OrdinalIgnoreCase) then relative.Substring(0, relative.Length - 4)
                        else relative
                    let stem = identifier withoutExtension |> _.ToLowerInvariant()
                    path, relative, $"module_{stem}.go")
            let collisions = candidates |> List.countBy (fun (_, _, fileName) -> fileName) |> Map.ofList
            candidates
            |> List.map (fun (path, relative, fileName) ->
                let allocated =
                    if collisions.[fileName] = 1 then fileName
                    else
                        let hash = SHA256.HashData(Encoding.UTF8.GetBytes(relative)) |> Convert.ToHexString |> fun value -> value.Substring(0, 8).ToLowerInvariant()
                        fileName.Substring(0, fileName.Length - 3) + "_" + hash + ".go"
                path, allocated)
            |> Map.ofList
        let moduleBodies = Dictionary<string, StringBuilder>()
        let sourceBuilder span =
            span.Start.File
            |> Option.bind (fun path -> Map.tryFind (Path.GetFullPath(path).Replace('\\', '/')) sourceFileNames)
            |> Option.map (fun fileName ->
                match moduleBodies.TryGetValue fileName with
                | true, builder -> builder
                | false, _ ->
                    let builder = StringBuilder()
                    moduleBodies.Add(fileName, builder)
                    builder)
        let declarations = StringBuilder()
        for group, members in mutualTailGroups do
            let _, _, _, firstChain, firstDeepest = List.head members
            let target = sourceBuilder firstDeepest.Span |> Option.defaultValue declarations
            let arguments =
                firstChain
                |> List.mapi (fun index (fn: Lir.Function) -> $"mutualArg{group}_{index}", fn.ParameterType)
            let signature =
                ([$"mutualState{group} int"]
                 @ (arguments |> List.map (fun (name, valueType) -> $"{name} {goType valueType}")))
                |> String.concat ", "
            target.AppendLine($"func mutualTail{group}({signature}) {goType firstDeepest.ReturnType} {{") |> ignore
            target.AppendLine("\tfor {") |> ignore
            target.AppendLine($"\t\tswitch mutualState{group} {{") |> ignore
            for state, (functionId, _, _, chain, deepest) in members |> List.indexed do
                let scoped =
                    chain
                    |> List.mapi (fun index fn -> fn.Parameter, $"mutualArg{group}_{index}")
                    |> List.fold (fun current (parameter, name) -> Map.add parameter name current) names
                let body = emitTail scoped declarationFunctionTargets functionId deepest.ReturnType deepest.Body
                target.AppendLine($"\t\tcase {state}:") |> ignore
                target.AppendLine("\t\t\t" + indent (indent (indent body))) |> ignore
            target.AppendLine("\t\tdefault:") |> ignore
            target.AppendLine("\t\t\tpanic(\"Invalid mutual tail state.\")") |> ignore
            target.AppendLine("\t\t}") |> ignore
            target.AppendLine("\t}") |> ignore
            target.AppendLine("}") |> ignore
            target.AppendLine() |> ignore
            for state, (_, _, raisedName, chain, deepest) in members |> List.indexed do
                let parameters = chain |> List.map (fun (fn: Lir.Function) -> symbolName "arg" fn.Parameter, fn.ParameterType)
                let wrapperSignature = parameters |> List.map (fun (name, valueType) -> $"{name} {goType valueType}") |> String.concat ", "
                let values = parameters |> List.map fst |> String.concat ", "
                target.AppendLine($"func {raisedName}({wrapperSignature}) {goType deepest.ReturnType} {{") |> ignore
                target.AppendLine($"\treturn mutualTail{group}({state}, {values})") |> ignore
                target.AppendLine("}") |> ignore
                target.AppendLine() |> ignore
        for KeyValue(_, (raisedName, chain)) in raisedDeclarations do
            match List.tryLast chain with
            | None -> ()
            | Some deepest when Map.containsKey deepest.Id mutualTailMembership -> ()
            | Some deepest ->
                let target = sourceBuilder deepest.Span |> Option.defaultValue declarations
                let parameters = chain |> List.map (fun (fn: Lir.Function) -> symbolName "arg" fn.Parameter, fn.ParameterType)
                let scoped = parameters |> List.fold2 (fun current (fn: Lir.Function) (name, _) -> Map.add fn.Parameter name current) names chain
                let signature = parameters |> List.map (fun (name, valueType) -> $"{name} {goType valueType}") |> String.concat ", "
                let body = emitTail scoped declarationFunctionTargets deepest.Id deepest.ReturnType deepest.Body
                if hasSelfTail declarationFunctionTargets deepest.Id deepest.Body then
                    target.AppendLine($"func {raisedName}({signature}) {goType deepest.ReturnType} {{\n\tfor {{\n\t\t{indent (indent body)}\n\t}}\n}}") |> ignore
                else
                    target.AppendLine($"func {raisedName}({signature}) {goType deepest.ReturnType} {{\n\t{indent body}\n}}") |> ignore
                target.AppendLine() |> ignore
        for declaration in emittedDeclarations do
            if unitEntryId <> Some declaration.Id then
                let target = sourceBuilder declaration.Span |> Option.defaultValue declarations
                target.AppendLine($"var {names.[declaration.Id]} {goType declaration.Type}") |> ignore
        if declarations.Length > 0 then declarations.AppendLine() |> ignore
        declarations.AppendLine("func init() {") |> ignore
        for declaration in emittedDeclarations do
            if unitEntryId = Some declaration.Id then
                declarations.AppendLine("\t" + indent (emitAssign names declarationFunctionTargets "_" declaration.Type declaration.Value)) |> ignore
            else
                declarations.AppendLine("\t" + indent (emitAssign names declarationFunctionTargets names.[declaration.Id] declaration.Type declaration.Value)) |> ignore
        declarations.AppendLine("}") |> ignore
        let entryCode = emit names declarationFunctionTargets runtimeEntry
        let resultFormatter = formatter runtimeEntry.Type
        let representationHelpers =
            representationHelperBodies
            |> Seq.sortBy _.Key
            |> Seq.map _.Value
        let helperSource = String.concat "\n\n" (Seq.concat [ equalityBodies :> seq<string>; formatterBodies :> seq<string>; representationHelpers ])
        let moduleValueSource =
            moduleBodies
            |> Seq.sortBy _.Key
            |> Seq.map (fun entry -> entry.Value.ToString())
            |> String.concat "\n"
        let emittedValues = declarations.ToString() + moduleValueSource + entryCode
        let regexRuntimeSource =
            if emittedValues.Contains("fsRegexMatchGroups", StringComparison.Ordinal) then
                """func fsRegexMatchGroups(pattern string, input string) ([]string, bool) {
	translated, captures, ok := fsTranslateRegex(pattern)
	if !ok {
		return nil, false
	}
	expression, err := regexp.Compile(translated)
	if err != nil {
		return nil, false
	}
	match := expression.FindStringSubmatch(input)
	if match == nil {
		return nil, false
	}
	groups := make([]string, 0, len(captures))
	for index, include := range captures {
		if include {
			groups = append(groups, match[index+1])
		}
	}
	return groups, true
}

func fsTranslateRegex(pattern string) (string, []bool, bool) {
	var translated strings.Builder
	captures := []bool{}
	escaped := false
	inClass := false
	for index := 0; index < len(pattern); index++ {
		character := pattern[index]
		if escaped {
			translated.WriteByte(character)
			escaped = false
			continue
		}
		if character == '\\' {
			translated.WriteByte(character)
			escaped = true
			continue
		}
		if character == '[' {
			inClass = true
			translated.WriteByte(character)
			continue
		}
		if character == ']' && inClass {
			inClass = false
			translated.WriteByte(character)
			continue
		}
		if character == '(' && !inClass {
			if index+2 < len(pattern) && pattern[index+1] == '?' && pattern[index+2] == ':' {
				translated.WriteByte('(')
				captures = append(captures, false)
				index += 2
				continue
			}
			if index+1 < len(pattern) && pattern[index+1] == '?' {
				return "", nil, false
			}
			captures = append(captures, true)
		}
		translated.WriteByte(character)
	}
	if escaped || inClass {
		return "", nil, false
	}
	return translated.String(), captures, true
}
"""
            else ""
        let filesystemRuntimeSource =
            if emittedValues.Contains("fsResolve", StringComparison.Ordinal) || emittedValues.Contains("fsGlob(", StringComparison.Ordinal) then
                """func fsRoot() (string, bool) {
	root, err := os.Getwd()
	if err != nil {
		return "", false
	}
	root, err = filepath.Abs(root)
	return root, err == nil
}

func fsResolve(candidate string) (string, bool) {
	full, err := filepath.Abs(candidate)
	if err != nil {
		return "", false
	}
	return full, true
}

func fsRelative(full string) string {
	root, ok := fsRoot()
	if !ok {
		return filepath.ToSlash(full)
	}
	relative, err := filepath.Rel(root, full)
	if err != nil {
		return filepath.ToSlash(full)
	}
	return filepath.ToSlash(relative)
}

func fsGlobRegex(pattern string) (*regexp.Regexp, bool) {
	escaped := regexp.QuoteMeta(filepath.ToSlash(pattern))
	escaped = strings.ReplaceAll(escaped, `\*\*/`, "___DOUBLESTAR_SLASH___")
	escaped = strings.ReplaceAll(escaped, `\*\*`, "___DOUBLESTAR___")
	escaped = strings.ReplaceAll(escaped, `\*`, `[^/]*`)
	escaped = strings.ReplaceAll(escaped, "___DOUBLESTAR_SLASH___", `(.*/)?`)
	escaped = strings.ReplaceAll(escaped, "___DOUBLESTAR___", `.*`)
	escaped = strings.ReplaceAll(escaped, `\?`, `.`)
	expression, err := regexp.Compile("^" + escaped + "$")
	return expression, err == nil
}

func fsGlob(directory string, pattern string) ([]string, bool) {
	fullDirectory, ok := fsResolve(directory)
	if !ok {
		return nil, false
	}
	info, err := os.Stat(fullDirectory)
	if err != nil || !info.IsDir() {
		return nil, false
	}
	expression, ok := fsGlobRegex(pattern)
	if !ok {
		return nil, false
	}
	values := []string{}
	consider := func(full string) {
		relative, err := filepath.Rel(fullDirectory, full)
		if err == nil && expression.MatchString(filepath.ToSlash(relative)) {
			values = append(values, fsRelative(full))
		}
	}
	if strings.Contains(filepath.ToSlash(pattern), "/") {
		err = filepath.WalkDir(fullDirectory, func(path string, entry os.DirEntry, walkError error) error {
			if walkError != nil {
				if entry != nil && entry.IsDir() {
					return filepath.SkipDir
				}
				return nil
			}
			if !entry.IsDir() {
				consider(path)
			}
			return nil
		})
	} else {
		var entries []os.DirEntry
		entries, err = os.ReadDir(fullDirectory)
		if err == nil {
			for _, entry := range entries {
				if !entry.IsDir() {
					consider(filepath.Join(fullDirectory, entry.Name()))
				}
			}
		}
	}
	if err != nil {
		return nil, false
	}
	sort.Strings(values)
	return values, true
}
"""
            else ""
        let portableRuntimeSource =
            [ regexRuntimeSource; filesystemRuntimeSource ]
            |> List.filter (String.IsNullOrWhiteSpace >> not)
            |> String.concat "\n"
            |> fun source -> if String.IsNullOrEmpty source then source else source + "\n"
        let consoleSource = if emittedValues.Contains("consoleScanner", StringComparison.Ordinal) then "var consoleScanner = bufio.NewScanner(os.Stdin)\n\n" else ""
        let importsFor (body: string) =
            [ if body.Contains("bufio.", StringComparison.Ordinal) then
                  "\t\"bufio\""
              if body.Contains("os.", StringComparison.Ordinal) then
                  "\t\"os\""
              if body.Contains("filepath.", StringComparison.Ordinal) then
                  "\t\"path/filepath\""
              if body.Contains("math.", StringComparison.Ordinal) then
                  "\t\"math\""
              if body.Contains("regexp.", StringComparison.Ordinal) then
                  "\t\"regexp\""
              if body.Contains("sort.", StringComparison.Ordinal) then
                  "\t\"sort\""
              if body.Contains("strconv.", StringComparison.Ordinal) then
                  "\t\"strconv\""
              if body.Contains("strings.", StringComparison.Ordinal) then
                  "\t\"strings\""
              if body.Contains("utf8.", StringComparison.Ordinal) then
                  "\t\"unicode/utf8\"" ]
        let importSourceFor (body: string) =
            let targetStandardImports, targetExternalImports =
                importModules
                |> Map.toList
                |> List.filter (fun (_, alias) -> body.Contains(alias + ".", StringComparison.Ordinal))
                |> List.map (fun (path, alias) -> path, $"\t{alias} {Text.Json.JsonSerializer.Serialize(path)}")
                |> List.partition (fun (path, _) -> not (path.Contains(".", StringComparison.Ordinal)))
                |> fun (standard, external) -> List.map snd standard, List.map snd external
            let standard = importsFor body @ targetStandardImports |> List.sort
            let groups = [ if not standard.IsEmpty then String.concat "\n" standard; if not targetExternalImports.IsEmpty then String.concat "\n" targetExternalImports ]
            match groups with
            | [] -> ""
            | values ->
                let renderedGroups = String.concat "\n\n" values
                $"import (\n{renderedGroups}\n)\n\n"
        let programBody = $"{typeDeclarations}\n{helperSource}\n\n{portableRuntimeSource}{consoleSource}{declarations}\nfunc Run() {goType entry.Type} {{\n\treturn {entryCode}\n}}\n\nfunc FormatResult(value {goType entry.Type}) string {{\n\treturn {resultFormatter}(value)\n}}\n"
        let programSource = $"package fscriptprogram\n\n{importSourceFor programBody}{programBody}"
        let moduleFiles =
            moduleBodies
            |> Seq.sortBy _.Key
            |> Seq.map (fun entry ->
                let body = entry.Value.ToString().TrimEnd() + "\n"
                $"internal/fscriptprogram/{entry.Key}", $"package fscriptprogram\n\n{importSourceFor body}{body}")
            |> Seq.toList
        let mainSource = $"package main\n\nimport (\n\t\"{moduleName}/internal/fscriptprogram\"\n\t\"os\"\n)\n\nfunc main() {{\n\tvalue := fscriptprogram.Run()\n\t_, _ = os.Stdout.WriteString(fscriptprogram.FormatResult(value) + \"\\n\")\n}}\n"
        [ "go.mod", $"module {moduleName}\n\ngo 1.25\n"
          "main.go", mainSource
          "internal/fscriptprogram/program.go", programSource
          yield! moduleFiles ]
