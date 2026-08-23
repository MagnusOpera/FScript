namespace FScript.Compiler

open System
open System.Collections.Generic
open System.Security.Cryptography
open System.Text
open FScript.Language

module TypedGoEmitter =
    let private symbolName prefix id = $"{prefix}{Lir.symbolNumber id}"
    let private indent (value: string) = value.Replace("\n", "\n\t")
    let private key value = Types.typeToString value
    let private suffix value =
        SHA256.HashData(Encoding.UTF8.GetBytes(key value)) |> Convert.ToHexString |> fun hash -> hash.Substring(0, 10).ToLowerInvariant()

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
        | TNamed "Environment" -> "environment"
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
        | TUnit | TInt | TFloat | TBool | TString | TTypeToken | TNamed "Environment" -> true
        | TFun(argument, result) -> supportedType argument && supportedType result
        | TList item | TOption item -> supportedType item
        | TTuple items -> List.forall supportedType items
        | _ -> false

    let rec private supportedExpression (functions: Map<Lir.FunctionId, Lir.Function>) (expression: Lir.Expression) =
        let supported = supportedExpression functions
        supportedType expression.Type
        && match expression.Node with
           | Lir.Constant _ | Lir.Load _ | Lir.NoneValue | Lir.RuntimeContext | Lir.TypeToken _ -> true
           | Lir.MakeClosure(id, _) -> functions |> Map.tryFind id |> Option.exists (fun fn -> supported fn.Body)
           | Lir.CallClosure(left, right) | Lir.PipeClosure(left, right) | Lir.Cons(left, right)
           | Lir.Append(left, right) -> supported left && supported right
           | Lir.Branch(a, b, c) -> supported a && supported b && supported c
           | Lir.Operation(_, values) | Lir.Tuple values | Lir.List values -> List.forall supported values
           | Lir.SomeValue value | Lir.Fail value -> supported value
           | Lir.Let(_, _, false, binding, body) -> supported binding && supported body
           | Lir.Interpolate parts -> parts |> List.forall (function Lir.TextPart _ -> true | Lir.ValuePart value -> supported value)
           | Lir.Intrinsic "ignore" -> true
           | _ -> false

    let canEmit (program: Lir.Program) =
        let functions = program.Functions |> List.map (fun item -> item.Id, item) |> Map.ofList
        program.ImportedRecords.IsEmpty
        && (program.Declarations |> List.forall (fun item -> not item.IsRecursive && supportedType item.Type && supportedExpression functions item.Value))
        && program.Entry |> Option.forall (supportedExpression functions)

    let generate moduleName (program: Lir.Program) =
        let functions = program.Functions |> List.map (fun item -> item.Id, item) |> Map.ofList
        let types = HashSet<Type>()
        let rec collectType value =
            if types.Add value then
                match value with
                | TFun(argument, result) -> collectType argument; collectType result
                | TList item | TOption item -> collectType item
                | TTuple items -> List.iter collectType items
                | _ -> ()
        let rec collectExpression (expression: Lir.Expression) =
            collectType expression.Type
            match expression.Node with
            | Lir.MakeClosure(id, _) -> let fn = functions.[id] in collectType fn.ParameterType; collectType fn.ReturnType; collectExpression fn.Body
            | Lir.CallClosure(left, right) | Lir.PipeClosure(left, right) | Lir.Cons(left, right)
            | Lir.Append(left, right) | Lir.Range(left, right) | Lir.IndexGet(left, right) -> collectExpression left; collectExpression right
            | Lir.Branch(a, b, c) -> collectExpression a; collectExpression b; collectExpression c
            | Lir.Operation(_, values) | Lir.Tuple values | Lir.List values -> List.iter collectExpression values
            | Lir.SomeValue value | Lir.Fail value -> collectExpression value
            | Lir.Let(_, _, _, binding, body) -> collectExpression binding; collectExpression body
            | Lir.LetRecursiveGroup(bindings, body) -> bindings |> List.iter (fun (_, _, valueType, value) -> collectType valueType; collectExpression value); collectExpression body
            | Lir.Interpolate parts -> parts |> List.iter (function Lir.TextPart _ -> () | Lir.ValuePart value -> collectExpression value)
            | Lir.ForEach(_, _, itemType, source, body) -> collectType itemType; collectExpression source; collectExpression body
            | Lir.TypeToken valueType -> collectType valueType
            | _ -> ()
        program.Declarations |> List.iter (fun item -> collectType item.Type; collectExpression item.Value)
        program.Entry |> Option.iter collectExpression

        let typeDeclarations = StringBuilder()
        typeDeclarations.AppendLine("type typeToken struct{ Value string }") |> ignore
        typeDeclarations.AppendLine("type optionString struct {") |> ignore
        typeDeclarations.AppendLine("\tHasValue bool") |> ignore
        typeDeclarations.AppendLine("\tValue    string") |> ignore
        typeDeclarations.AppendLine("}") |> ignore
        typeDeclarations.AppendLine("type environment struct {") |> ignore
        typeDeclarations.AppendLine("\tScriptName optionString") |> ignore
        typeDeclarations.AppendLine("\tArguments  []string") |> ignore
        typeDeclarations.AppendLine("}") |> ignore
        for value in types |> Seq.sortBy key do
            match value with
            | TOption item ->
                typeDeclarations.AppendLine($"type {goType value} struct {{") |> ignore
                typeDeclarations.AppendLine("\tHasValue bool") |> ignore
                typeDeclarations.AppendLine($"\tValue    {goType item}") |> ignore
                typeDeclarations.AppendLine("}") |> ignore
            | TTuple items ->
                typeDeclarations.AppendLine($"type {goType value} struct {{") |> ignore
                items |> List.iteri (fun index item -> typeDeclarations.AppendLine($"\tItem{index} {goType item}") |> ignore)
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
                    | TNamed "Environment" -> "return \"<environment>\""
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
                    | other -> invalidArg "value" $"No typed equality for {Types.typeToString other}."
                equalityBodies.Add($"func {name}(left {goType value}, right {goType value}) bool {{\n\t{body}\n}}")
                name

        let names = program.Declarations |> List.map (fun item -> item.Id, symbolName "value" item.Id) |> Map.ofList
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
            | Lir.Branch(condition, whenTrue, whenFalse) -> $"func() {goType expression.Type} {{\n\tif {emitChild condition} {{ return {indent (emitChild whenTrue)} }}\n\treturn {indent (emitChild whenFalse)}\n}}()"
            | Lir.Operation(operation, [ left; right ]) ->
                let leftCode, rightCode = emitChild left, emitChild right
                match operation with
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
            | Lir.Range(first, last) -> $"func() []int64 {{\n\tfirst, last := {emitChild first}, {emitChild last}\n\tresult := []int64{{}}\n\tif first <= last {{ for value := first; value <= last; value++ {{ result = append(result, value) }} }} else {{ for value := first; value >= last; value-- {{ result = append(result, value) }} }}\n\treturn result\n}}()"
            | Lir.IndexGet(target, index) -> $"func() {goType expression.Type} {{\n\tvalues, index := {emitChild target}, {emitChild index}\n\tif index < 0 || index >= int64(len(values)) {{ return {goType expression.Type}{{}} }}\n\treturn {goType expression.Type}{{HasValue: true, Value: values[index]}}\n}}()"
            | Lir.Interpolate parts -> parts |> List.map (function Lir.TextPart text -> Text.Json.JsonSerializer.Serialize(text) | Lir.ValuePart value -> $"{formatter value.Type}({emitChild value})") |> String.concat " + "
            | Lir.Intrinsic "ignore" ->
                let argumentType = match expression.Type with TFun(argument, _) -> goType argument | _ -> "struct{}"
                $"func(value {argumentType}) struct{{}} {{ return struct{{}}{{}} }}"
            | Lir.RuntimeContext -> "environment{ScriptName: optionString{HasValue: true, Value: filepath.Base(os.Args[0])}, Arguments: append([]string{}, os.Args[1:]...)}"
            | Lir.ForEach(id, _, _, source, body) ->
                let variable = symbolName "item" id
                let bodyCode = emit (Map.add id variable environment) body
                $"func() struct{{}} {{\n\tfor _, {variable} := range {emitChild source} {{ _ = {indent bodyCode} }}\n\treturn struct{{}}{{}}\n}}()"
            | Lir.TypeToken valueType -> $"typeToken{{Value: {Text.Json.JsonSerializer.Serialize(Types.typeToString valueType)}}}"
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
        let imports =
            [ if types.Contains(TNamed "Environment") then
                  "\t\"os\""
                  "\t\"path/filepath\""
              if helperSource.Contains("math.", StringComparison.Ordinal) then
                  "\t\"math\""
              if helperSource.Contains("strconv.", StringComparison.Ordinal) then
                  "\t\"strconv\"" ]
        let importSource =
            match imports with
            | [] -> ""
            | values ->
                let renderedImports = String.concat "\n" values
                $"import (\n{renderedImports}\n)\n\n"
        let programSource = $"package fscriptprogram\n\n{importSource}{typeDeclarations}\n{helperSource}\n\n{declarations}\nfunc Run() {goType entry.Type} {{\n\treturn {entryCode}\n}}\n\nfunc FormatResult(value {goType entry.Type}) string {{\n\treturn {resultFormatter}(value)\n}}\n"
        let mainSource = $"package main\n\nimport (\n\t\"{moduleName}/internal/fscriptprogram\"\n\t\"os\"\n)\n\nfunc main() {{\n\tvalue := fscriptprogram.Run()\n\t_, _ = os.Stdout.WriteString(fscriptprogram.FormatResult(value) + \"\\n\")\n}}\n"
        [ "go.mod", $"module {moduleName}\n\ngo 1.25\n"
          "main.go", mainSource
          "internal/fscriptprogram/program.go", programSource ]
