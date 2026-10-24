namespace FScript.Compiler

open System
open System.IO
open System.Reflection
open System.Text
open System.Text.Json
open System.Security.Cryptography
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open FScript.Language

type CSharpOptions =
    { Namespace: string
      ClassName: string
      References: string list }

module CSharpBackend =
    let private comma = ", "
    let private space = " "
    let private itemCode = "item"
    let private aCode = "a"
    let private bCode = "b"
    let private valueCode = "value"
    let private stringCode = "string"
    let private valueMember code = code + ".Value"
    exception private Failure of CompilerDiagnostic
    let private fail code message span =
        raise (Failure { Code = code; Stage = CSharpGeneration; Message = message; Span = span })
    let private quote (value: string) = JsonSerializer.Serialize(value)
    let private ident (value: string) =
        "v_" + (value |> Seq.map (fun c -> if Char.IsLetterOrDigit c || c = '_' then c else '_') |> Seq.toArray |> String)
    let private hash value =
        SHA256.HashData(Encoding.UTF8.GetBytes(sprintf "%A" value)) |> Convert.ToHexString |> fun s -> s.Substring(0, 12).ToLowerInvariant()
    let private sym id = "v" + string (Lir.symbolNumber id)
    let private funName id = "Function" + string (Lir.functionNumber id)
    let private wrap resultType body = $"((Func<{resultType}>)(() => {{ {body} }}))()"
    let private typeName (value: System.Type) =
        let rec render (value: System.Type) =
            if value.IsArray then render ((value.GetElementType() |> nonNull)) + "[]"
            elif value.IsGenericType then
                let name = (value.GetGenericTypeDefinition().FullName |> nonNull)
                let name = name.Substring(0, name.IndexOf('`')).Replace('+', '.')
                "global::" + name + "<" + (value.GetGenericArguments() |> Array.map render |> String.concat comma) + ">"
            else "global::" + (value.FullName |> nonNull).Replace('+', '.')
        render value

    let private emit assemblyMode options (program: Lir.Program) =
        let hasSdk = program.Declarations |> List.exists (fun d -> match d.Value.Node with Lir.TargetImport(path,_) -> path.StartsWith("clr-sdk:",StringComparison.Ordinal) | _ -> false)
        let definitions = program.Types |> List.map (fun d -> d.Name, d.Definition) |> Map.ofList
        let imports = program.ImportedRecords |> List.map (fun d -> d.Name, d) |> Map.ofList
        let nativeTypes =
            program.ImportedRecords |> List.map (fun imported ->
                if not (imported.ModulePath.StartsWith("clr:", StringComparison.Ordinal)) then
                    fail "FSC6002" $"Target import '{imported.Name}' has no CLR binding." (Some imported.Span)
                let value = System.Type.GetType(imported.ModulePath.Substring(4), false)
                if isNull value then fail "FSC6003" $"Cannot resolve CLR type '{imported.ModulePath.Substring(4)}'. Supply its assembly in References." (Some imported.Span)
                imported.Name, nonNull value) |> Map.ofList
        let rec resolve = function TNamed name when definitions.ContainsKey name -> definitions.[name] | value -> value
        let rec csType = function
            | TUnit -> "Unit" | TInt -> "long" | TFloat -> "double" | TBool -> "bool" | TString -> "string"
            | TTypeToken -> "TypeToken"
            | TFun(argument, result) -> $"Func<{csType argument}, {csType result}>"
            | TList item -> csType item + "[]"
            | TOption item -> $"Option<{csType item}>"
            | TTask item -> $"Task<{csType item}>"
            | TMap(key, value) -> $"OrderedMap<{csType key}, {csType value}>"
            | TNamed name | THostManaged(name, _) ->
                match nativeTypes.TryFind name with
                | Some value -> typeName value
                | None when hasSdk && (match definitions.TryFind name with Some(THostManaged _) -> true | _ -> false) -> "global::FScript.Extensibility.HostManagedValue"
                | None when (match definitions.TryFind name with Some(THostManaged _) -> true | _ -> false) ->
                    fail "FSC6003" $"Host-managed type '{name}' requires ClrBindings.bindType." None
                | None -> ident name
            | (TRecord _ | TTuple _ | TUnion _) as value -> "Shape_" + hash value
            | value -> fail "FSC6004" $"Cannot represent open type '{Types.typeToString value}' in C#." None
        let fieldName target name =
            match target with
            | TNamed typeName | THostManaged(typeName, _) when nativeTypes.ContainsKey typeName -> "@" + name
            | _ -> "Field_" + ident name
        let fields value = match resolve value with TRecord fields | THostManaged(_, fields) -> fields | _ -> Map.empty
        let types = Collections.Generic.HashSet<Type>()
        let rec addType value =
            if types.Add value then
                match value with
                | TFun(a,b) | TMap(a,b) -> addType a; addType b
                | TList item | TOption item | TTask item -> addType item
                | TTuple items -> List.iter addType items
                | TRecord fields | THostManaged(_, fields) -> fields |> Map.iter (fun _ -> addType)
                | TUnion(_, cases) -> cases |> Map.iter (fun _ -> Option.iter addType)
                | TNamed name -> definitions.TryFind name |> Option.iter addType
                | _ -> ()
        let rec collect (expression: Lir.Expression) =
            addType expression.Type
            let visit = collect
            match expression.Node with
            | Lir.CallClosure(a,b) | Lir.PipeClosure(a,b) | Lir.TailCallClosure(a,b) | Lir.TailPipeClosure(a,b)
            | Lir.Cons(a,b) | Lir.Append(a,b) | Lir.Range(a,b) | Lir.IndexGet(a,b) -> visit a; visit b
            | Lir.Branch(a,b,c) -> visit a; visit b; visit c
            | Lir.Operation(_,items) | Lir.List items | Lir.Tuple items -> List.iter visit items
            | Lir.Record items -> items |> List.iter (snd >> visit)
            | Lir.FieldGet(item,_) | Lir.SomeValue item | Lir.Fail item -> visit item
            | Lir.RecordConvert(item,target) -> visit item; addType target
            | Lir.Let(_,_,_,a,b) -> visit a; visit b
            | Lir.LetRecursiveGroup(bindings,body) -> bindings |> List.iter (fun (_,_,t,v) -> addType t; visit v); visit body
            | Lir.DecisionTree(target,cases) ->
                visit target
                let rec plan (value: Lir.PatternPlan) =
                    value.Bindings |> List.iter (_.Type >> addType)
                    for map in value.Maps do
                        for clause in map.Clauses do plan clause.Key; plan clause.Value
                        Option.iter plan map.Tail
                for case in cases do plan case.Plan; Option.iter visit case.Guard; visit case.Body
            | Lir.RecordUpdate(target,items) -> visit target; items |> List.iter (snd >> visit)
            | Lir.Interpolate parts -> parts |> List.iter (function Lir.ValuePart item -> visit item | _ -> ())
            | Lir.UnionCase(_,_,payload) -> Option.iter visit payload
            | Lir.Map items -> items |> List.iter (function Lir.KeyValue(a,b) -> visit a; visit b | Lir.Spread item -> visit item)
            | Lir.ForEach(_,_,t,source,body) -> addType t; visit source; visit body
            | Lir.TypeToken t -> addType t
            | _ -> ()
        program.Declarations |> List.iter (fun d -> addType d.Type; collect d.Value)
        program.Functions |> List.iter (fun f -> addType f.ParameterType; addType f.ReturnType; collect f.Body)
        Option.iter collect program.Entry
        if assemblyMode then addType (program.Entry |> Option.map _.Type |> Option.defaultValue TUnit)
        let symbolTypes = Collections.Generic.Dictionary<Lir.SymbolId, Type>()
        let knownBindings = Collections.Generic.Dictionary<Lir.SymbolId, Lir.Expression>()
        let rec collectBindings (value: Lir.Expression) =
            match value.Node with
            | Lir.Load id -> symbolTypes.TryAdd(id,value.Type) |> ignore
            | _ -> ()
            // Captures also carry their use types in the lifted function body.
            let rec walk (v: Lir.Expression) =
                match v.Node with
                | Lir.Load id -> symbolTypes.TryAdd(id,v.Type) |> ignore
                | Lir.Let(id,_,_,binding,body) ->
                    symbolTypes.[id] <- binding.Type
                    knownBindings.[id] <- binding
                    walk binding
                    walk body
                | Lir.LetRecursiveGroup(bindings,body) ->
                    for id,_,t,item in bindings do
                        symbolTypes.[id] <- t
                        knownBindings.[id] <- item
                        walk item
                    walk body
                | Lir.ForEach(id,_,t,source,body) -> symbolTypes.[id] <- t; walk source; walk body
                | Lir.CallClosure(a,b) | Lir.PipeClosure(a,b) | Lir.TailCallClosure(a,b) | Lir.TailPipeClosure(a,b)
                | Lir.Cons(a,b) | Lir.Append(a,b) | Lir.Range(a,b) | Lir.IndexGet(a,b) -> walk a; walk b
                | Lir.Branch(a,b,c) -> walk a; walk b; walk c
                | Lir.Operation(_,items) | Lir.Tuple items | Lir.List items -> List.iter walk items
                | Lir.Record items -> List.iter (snd >> walk) items
                | Lir.FieldGet(item,_) | Lir.SomeValue item | Lir.Fail item | Lir.RecordConvert(item,_) -> walk item
                | Lir.DecisionTree(target,cases) -> walk target; for case in cases do Option.iter walk case.Guard; walk case.Body
                | Lir.RecordUpdate(target,items) -> walk target; List.iter (snd >> walk) items
                | Lir.Interpolate parts -> List.iter (function Lir.ValuePart item -> walk item | _ -> ()) parts
                | Lir.UnionCase(_,_,payload) -> Option.iter walk payload
                | Lir.Map entries -> List.iter (function Lir.KeyValue(a,b) -> walk a; walk b | Lir.Spread item -> walk item) entries
                | _ -> ()
            walk value
        for declaration in program.Declarations do symbolTypes.[declaration.Id] <- declaration.Type; collectBindings declaration.Value
        for fn in program.Functions do symbolTypes.[fn.Parameter] <- fn.ParameterType; collectBindings fn.Body
        let functions = program.Functions |> List.map (fun f -> f.Id,f) |> Map.ofList
        let globalIds = program.Declarations |> List.map _.Id |> Set.ofList
        let mutable temporary = 0
        let fresh prefix = temporary <- temporary + 1; prefix + string temporary
        let constant = function
            | Lir.Unit -> "default(Unit)"
            | Lir.Int value -> if value = Int64.MinValue then "long.MinValue" else string value + "L"
            | Lir.Float value -> if Double.IsNaN value then "double.NaN" elif Double.IsPositiveInfinity value then "double.PositiveInfinity" elif Double.IsNegativeInfinity value then "double.NegativeInfinity" else value.ToString("R", Globalization.CultureInfo.InvariantCulture) + "d"
            | Lir.Bool value -> if value then "true" else "false"
            | Lir.String value -> quote value
        let eqName t = "Equal_" + hash t
        let fmtName t = "Format_" + hash t
        let interpName t = "Interpolate_" + hash t
        let encName t = "Encode_" + hash t
        let decName t = "Decode_" + hash t
        let jsonEncodeName t = "JsonEncode_" + hash t
        let jsonDecodeName t = "JsonDecode_" + hash t
        let rec sdkType t =
            let st="global::FScript.Extensibility.ScriptType"
            let unary name item = $"{st}.New{name}({sdkType item})"
            match t with
            | TUnit -> st+".TUnit" | TInt -> st+".TInt" | TFloat -> st+".TFloat" | TBool -> st+".TBool" | TString -> st+".TString" | TTypeToken -> st+".TTypeToken"
            | TVar id -> $"{st}.NewTVar({id})" | TNumeric id -> $"{st}.NewTNumeric({id})"
            | TNamed name -> $"{st}.NewTNamed({quote name})"
            | TList item -> unary "TList" item | TOption item -> unary "TOption" item | TTask item -> unary "TTask" item
            | TFun(a,b) -> $"{st}.NewTFun({sdkType a},{sdkType b})" | TMap(a,b) -> $"{st}.NewTMap({sdkType a},{sdkType b})"
            | TTuple items ->
                let values=items |> List.map sdkType |> String.concat comma
                $"{st}.NewTTuple(global::Microsoft.FSharp.Collections.ListModule.OfArray(new {st}[] {{ {values} }}))"
            | TRecord fields | THostManaged(_,fields) ->
                let values=fields |> Map.toList |> List.map (fun (name,t) -> $"Tuple.Create({quote name}, {sdkType t})") |> String.concat comma
                let map= $"global::Microsoft.FSharp.Collections.MapModule.OfSeq(new Tuple<string,{st}>[] {{ {values} }})"
                match t with THostManaged(name,_) -> $"{st}.NewTHostManaged({quote name},{map})" | _ -> $"{st}.NewTRecord({map})"
            | TUnion(name,cases) ->
                let values=cases |> Map.toList |> List.map (fun (name,t) ->
                    let payload=t |> Option.map (fun t -> $"global::Microsoft.FSharp.Core.FSharpOption<{st}>.Some({sdkType t})") |> Option.defaultValue "null"
                    $"Tuple.Create<string,global::Microsoft.FSharp.Core.FSharpOption<{st}>?>({quote name},{payload})") |> String.concat comma
                $"{st}.NewTUnion({quote name},global::Microsoft.FSharp.Collections.MapModule.OfSeq(new Tuple<string,global::Microsoft.FSharp.Core.FSharpOption<{st}>?>[] {{ {values} }}))"
            | TRecordShape _ -> fail "FSC6008" "Unresolved structural witness in SDK signature." None
        let readField target name code =
            match resolve target with
            | THostManaged(typeName,required) when hasSdk && not (nativeTypes.ContainsKey typeName) ->
                $"{decName required.[name]}(({code}).Fields[{quote name}])"
            | _ -> $"({code}).{fieldName target name}"
        let rec coerce source target code =
            if csType source = csType target then code
            else
                match resolve source, resolve target with
                | (TRecord _ | THostManaged _), TRecord destination ->
                    let original = fresh "converted"
                    let args = destination |> Map.toList |> List.map (fun (name,t) -> coerce (fields source).[name] t (readField source name original)) |> String.concat comma
                    wrap (csType target) $"var {original} = {code}; return new {csType target}({args});"
                | TOption a,TOption b ->
                    let original = fresh "option"
                    wrap (csType target) $"var {original} = {code}; return {original}.HasValue ? {csType target}.Some({coerce a b (valueMember original)}) : default;"
                | TList a,TList b -> $"({code}).Select(item => {coerce a b itemCode}).ToArray()"
                | TTuple sourceItems,TTuple targetItems when sourceItems.Length=targetItems.Length ->
                    let original=fresh "tuple"
                    let args=List.zip sourceItems targetItems |> List.mapi (fun i (a,b) -> coerce a b $"{original}.Item{i}") |> String.concat comma
                    wrap (csType target) $"var {original}={code}; return new {csType target}({args});"
                | TFun(sa,sr),TFun(ta,tr) ->
                    let original=fresh "function"
                    let argument=fresh "argument"
                    let call= $"{original}.Invoke({coerce ta sa argument})"
                    wrap (csType target) $"var {original}={code}; return ({csType ta} {argument}) => {coerce sr tr call};"
                | _ -> code
        let rec curryApplied supplied current args build =
            match supplied,current with
            | argument::rest,TFun(_,result) -> curryApplied rest result (args @ [argument]) build
            | _,TFun(argument,result) ->
                let parameter = fresh "argument"
                $"(({csType current})(({csType argument} {parameter}) => {curryApplied [] result (args @ [parameter]) build}))"
            | _,result -> build args result
        let rec intrinsic name signature supplied =
            let build body = curryApplied supplied signature [] body
            let arrayType = function TList item -> item | _ -> invalidOp "Expected list."
            let rec argsOf = function TFun(a,b) -> a :: argsOf b | _ -> []
            let parameters = argsOf signature
            match name, signature with
            | "List.empty", TList t -> $"Array.Empty<{csType t}>()"
            | "Map.empty", TMap _ -> $"new {csType signature}()"
            | "ignore", _ -> build (fun _ _ -> "default(Unit)")
            | "Int.toString", _ -> build (fun a _ -> $"{a.[0]}.ToString(CultureInfo.InvariantCulture)")
            | "Float.toString", _ -> build (fun a _ -> $"FsRuntime.FloatText({a.[0]})")
            | "Bool.toString", _ -> build (fun a _ -> $"({a.[0]} ? \"true\" : \"false\")")
            | "Int.tryParse", _ -> build (fun a _ -> $"FsRuntime.ParseInt({a.[0]})")
            | "Float.tryParse", _ -> build (fun a _ -> $"FsRuntime.ParseFloat({a.[0]})")
            | "Bool.tryParse", _ -> build (fun a _ -> $"FsRuntime.ParseBool({a.[0]})")
            | "String.replace", _ -> build (fun a _ -> $"{a.[2]}.Replace({a.[0]}, {a.[1]}, StringComparison.Ordinal)")
            | "String.indexOf", _ -> build (fun a _ -> $"FsRuntime.IndexOf({a.[0]}, {a.[1]})")
            | "String.toLower", _ -> build (fun a _ -> $"{a.[0]}.ToLowerInvariant()")
            | "String.toUpper", _ -> build (fun a _ -> $"{a.[0]}.ToUpperInvariant()")
            | "String.substring", _ -> build (fun a _ -> $"FsRuntime.Substring({a.[0]}, {a.[1]}, {a.[2]})")
            | "String.concat", _ -> build (fun a _ -> $"string.Join({a.[0]}, {a.[1]})")
            | "String.split", _ -> build (fun a _ -> $"{a.[1]}.Split(new string[] {{ {a.[0]} }}, StringSplitOptions.None)")
            | "String.endsWith", _ -> build (fun a _ -> $"{a.[1]}.EndsWith({a.[0]}, StringComparison.Ordinal)")
            | "List.map", _ -> build (fun a _ -> $"{a.[1]}.Select(item => {a.[0]}(item)).ToArray()")
            | "List.iter", _ -> build (fun a _ -> $"FsRuntime.Iter({a.[0]}, {a.[1]})")
            | "List.choose", _ -> build (fun a _ -> $"{a.[1]}.Select(item => {a.[0]}(item)).Where(item => item.HasValue).Select(item => item.Value).ToArray()")
            | "List.collect", _ -> build (fun a _ -> $"{a.[1]}.SelectMany(item => {a.[0]}(item)).ToArray()")
            | "List.exists", _ -> build (fun a _ -> $"{a.[1]}.Any(item => {a.[0]}(item))")
            | "List.contains", _ -> build (fun a _ -> $"{a.[1]}.Any(item => {eqName parameters.[0]}({a.[0]}, item))")
            | "List.rev", _ -> build (fun a _ -> $"{a.[0]}.Reverse().ToArray()")
            | "List.distinct", _ -> build (fun a _ -> $"FsRuntime.Distinct({a.[0]}, {eqName (arrayType parameters.[0])})")
            | "List.fold", _ -> build (fun a _ -> $"FsRuntime.Fold({a.[0]}, {a.[1]}, {a.[2]})")
            | "List.filter", _ -> build (fun a _ -> $"{a.[1]}.Where(item => {a.[0]}(item)).ToArray()")
            | "List.length", _ -> build (fun a _ -> $"{a.[0]}.LongLength")
            | "List.tryFind", _ -> build (fun a _ -> $"FsRuntime.Find({a.[0]}, {a.[1]})")
            | "List.tryGet", _ -> build (fun a _ -> $"FsRuntime.FindIndex({a.[0]}, {a.[1]})")
            | "List.tryItem", _ -> build (fun a _ -> $"FsRuntime.At({a.[0]}, {a.[1]})")
            | "List.tryHead", _ -> build (fun a _ -> $"FsRuntime.At(0, {a.[0]})")
            | "List.tail", _ -> build (fun a _ -> $"{a.[0]}.Skip(1).ToArray()")
            | "List.append", _ -> build (fun a _ -> $"{a.[0]}.Concat({a.[1]}).ToArray()")
            | "Option.defaultValue", _ -> build (fun a _ -> $"({a.[1]}.HasValue ? {a.[1]}.Value : {a.[0]})")
            | "Option.defaultWith", _ -> build (fun a _ -> $"({a.[1]}.HasValue ? {a.[1]}.Value : {a.[0]}(default(Unit)))")
            | "Option.isNone", _ -> build (fun a _ -> $"!{a.[0]}.HasValue")
            | "Option.isSome", _ -> build (fun a _ -> $"{a.[0]}.HasValue")
            | "Option.map", _ -> build (fun a r -> $"({a.[1]}.HasValue ? {csType r}.Some({a.[0]}({a.[1]}.Value)) : default)")
            | "Map.tryGet", _ -> build (fun a _ -> $"{a.[1]}.Find({a.[0]})")
            | "Map.containsKey", _ -> build (fun a _ -> $"{a.[1]}.Find({a.[0]}).HasValue")
            | "Map.count", _ -> build (fun a _ -> $"{a.[0]}.Entries.LongLength")
            | "Map.add", _ -> build (fun a _ -> $"{a.[2]}.Add({a.[0]}, {a.[1]})")
            | "Map.remove", _ -> build (fun a _ -> $"{a.[1]}.Remove({a.[0]})")
            | "Map.ofList", _ -> build (fun a r ->
                let tuple = arrayType parameters.[0]
                let key,value = match tuple with TTuple [k;v] -> k,v | _ -> invalidOp "Expected pairs."
                $"FsRuntime.MapOfArray<{csType key},{csType value}>({a.[0]}.Select(pair => (pair.Item0,pair.Item1)).ToArray())")
            | "Map.fold", _ -> build (fun a _ -> $"FsRuntime.MapFold({a.[0]}, {a.[1]}, {a.[2]})")
            | "Map.iter", _ -> build (fun a _ -> $"FsRuntime.MapIter({a.[0]}, {a.[1]})")
            | "Map.filter", _ -> build (fun a r -> $"new {csType r}({a.[1]}.Entries.Where(e => {a.[0]}(e.Key)(e.Value)).ToArray())")
            | "Map.map", _ -> build (fun a r -> $"new {csType r}({a.[1]}.Entries.Select(e => new {csType r}.Entry(e.Key, {a.[0]}(e.Value))).ToArray())")
            | "Map.choose", _ -> build (fun a r -> $"new {csType r}({a.[1]}.Entries.Select(e => (e.Key, Value: {a.[0]}(e.Key)(e.Value))).Where(e => e.Value.HasValue).Select(e => new {csType r}.Entry(e.Key,e.Value.Value)).ToArray())")
            | "Console.writeLine", _ -> build (fun a _ -> $"FsRuntime.WriteLine({a.[0]})")
            | "Console.readLine", _ -> build (fun a _ -> $"FsRuntime.ReadLine({a.[0]})")
            | "Fs.readText", _ -> build (fun a _ -> $"FsRuntime.Try(() => File.ReadAllText({a.[0]}))")
            | "Fs.exists", _ -> build (fun a _ -> $"(File.Exists({a.[0]}) || Directory.Exists({a.[0]}))")
            | "Fs.kind", _ -> build (fun a r -> $"(File.Exists({a.[0]}) ? ({csType r})new {csType r}.v_File(Path.GetRelativePath(Directory.GetCurrentDirectory(),Path.GetFullPath({a.[0]}))) : Directory.Exists({a.[0]}) ? ({csType r})new {csType r}.v_Directory(Path.GetRelativePath(Directory.GetCurrentDirectory(),Path.GetFullPath({a.[0]}))) : new {csType r}.v_Missing())")
            | "Fs.createDirectory", _ -> build (fun a _ -> $"FsRuntime.TryEffect(() => Directory.CreateDirectory({a.[0]}))")
            | "Fs.writeText", _ -> build (fun a _ -> $"FsRuntime.TryEffect(() => File.WriteAllText({a.[0]}, {a.[1]}))")
            | "Fs.combinePath", _ -> build (fun a _ -> $"Path.Combine({a.[0]}, {a.[1]})")
            | "Fs.parentDirectory", _ -> build (fun a r -> wrap (csType r) $"var path=Path.GetDirectoryName({a.[0]}); return string.IsNullOrEmpty(path) ? default : {csType r}.Some(path);")
            | "Fs.extension", _ -> build (fun a r -> wrap (csType r) $"var extension=Path.GetExtension({a.[0]}); return string.IsNullOrEmpty(extension) ? default : {csType r}.Some(extension);")
            | "Fs.fileNameWithoutExtension", _ -> build (fun a _ -> $"Path.GetFileNameWithoutExtension({a.[0]})")
            | "Fs.glob", _ -> build (fun a _ -> $"FsRuntime.Files(Directory.GetCurrentDirectory(), {a.[0]}, true)")
            | "Fs.enumerateFiles", _ -> build (fun a _ -> $"FsRuntime.Files({a.[0]}, {a.[1]}, false)")
            | "Regex.matchGroups", _ -> build (fun a _ -> $"FsRuntime.MatchGroups({a.[0]}, {a.[1]})")
            | "Hash.md5", _ -> build (fun a _ -> $"FsRuntime.Try(() => Convert.ToHexString(System.Security.Cryptography.MD5.HashData(System.Text.Encoding.UTF8.GetBytes({a.[0]}))).ToLowerInvariant())")
            | "Guid.new", _ -> build (fun _ _ -> "Option<string>.Some(Guid.NewGuid().ToString())")
            | "Json.serialize", _ -> build (fun a _ -> $"FsRuntime.Try(() => {jsonEncodeName parameters.[0]}({a.[0]}))")
            | "Json.deserialize", _ -> build (fun a r -> match r with TOption item -> $"FsRuntime.ParseJson({a.[1]}, {jsonDecodeName item})" | _ -> invalidOp "Expected option.")
            | "Xml.queryValues", _ -> build (fun a _ -> $"FsRuntime.QueryXml({a.[0]}, {a.[1]})")
            | "Task.spawn", _ -> build (fun a _ -> $"Task.Run(() => {a.[0]}(default(Unit)))")
            | "Task.await", _ -> build (fun a _ -> $"{a.[0]}.GetAwaiter().GetResult()")
            | "Task.map", _ -> build (fun a _ -> $"Task.Run(() => {a.[0]}({a.[1]}.GetAwaiter().GetResult()))")
            | "Task.bind", _ -> build (fun a _ -> $"Task.Run(async () => await {a.[0]}(await {a.[1]}))")
            | "Task.parallel", _ -> build (fun a _ -> $"Task.WhenAll({a.[0]})")
            | "Task.ignore", _ -> build (fun a _ -> $"Task.Run(async () => {{ await {a.[0]}; return default(Unit); }})")
            | _ -> fail "FSC6005" $"External or intrinsic '{name}' has no compiled CLR implementation. Bind host functions with ClrBindings.bindMethod." None
        let rec toClr semantic (native: System.Type) code =
            if native = typeof<Void> then code
            elif csType semantic = typeName native then code
            elif native = typeof<int64> || native = typeof<int> || native = typeof<float> || native = typeof<float32> || native = typeof<bool> || native = typeof<string> then $"(({typeName native})({code}))"
            elif native = typeof<unit> then "default(global::Microsoft.FSharp.Core.Unit)"
            elif native.IsArray then
                match semantic with TList item -> $"({code}).Select(item => {toClr item ((native.GetElementType() |> nonNull)) itemCode}).ToArray()" | _ -> code
            elif native.IsGenericType && native.GetGenericTypeDefinition() = typedefof<list<_>> then
                match semantic with TList item -> $"global::Microsoft.FSharp.Collections.ListModule.OfArray(({code}).Select(item => {toClr item (native.GetGenericArguments().[0]) itemCode}).ToArray())" | _ -> code
            elif native.IsGenericType && native.GetGenericTypeDefinition() = typedefof<Set<_>> then
                match semantic with TList item -> $"global::Microsoft.FSharp.Collections.SetModule.OfSeq(({code}).Select(item => {toClr item (native.GetGenericArguments().[0]) itemCode}))" | _ -> code
            elif native.IsGenericType && native.GetGenericTypeDefinition() = typedefof<option<_>> then
                match semantic with TOption item -> $"(({code}).HasValue ? {typeName native}.Some({toClr item (native.GetGenericArguments().[0]) (valueMember code)}) : null)" | _ -> code
            else code
        and fromClr (native: System.Type) semantic code =
            if native = typeof<Void> || native = typeof<unit> then wrap "Unit" $"{code}; return default;"
            elif csType semantic = typeName native then code
            else
                match semantic with
                | TInt | TFloat | TBool | TString -> $"(({csType semantic})({code}))"
                | TList item when native.IsArray || native.IsGenericType ->
                    let element = if native.IsArray then (native.GetElementType() |> nonNull) else native.GetGenericArguments().[0]
                    $"({code}).Select(item => {fromClr element item itemCode}).ToArray()"
                | TOption item when native.IsGenericType && native.GetGenericTypeDefinition() = typedefof<option<_>> ->
                    let original = fresh "foreignOption"
                    wrap (csType semantic) $"var {original}={code}; return {original} is null ? default : {csType semantic}.Some({fromClr (native.GetGenericArguments().[0]) item (valueMember original)});"
                | _ -> code
        let sdkProviders = Collections.Generic.Dictionary<string,string>()
        let targetImport (path: string) memberName signature span supplied =
          if path.StartsWith("clr-sdk:",StringComparison.Ordinal) then
            let methodPath="clr:"+path.Substring(8)
            let link=ClrBindings.tryMethodLink methodPath |> Option.defaultWith (fun () -> fail "FSC6006" "Invalid SDK provider linkage." (Some span))
            let key=path
            let provider=
                match sdkProviders.TryGetValue key with
                | true,value -> value
                | _ ->
                    let value=fresh "Provider"
                    sdkProviders.Add(key,value)
                    value
            let rec argsOf = function TFun(a,b) -> a :: argsOf b | _ -> []
            let argumentTypes=argsOf signature
            curryApplied supplied signature [] (fun args result ->
                let encoded=List.map2 (fun t arg -> $"{encName t}({arg})") argumentTypes args |> String.concat comma
                $"{decName result}(SdkInterop.Invoke(SdkInterop.Find({provider}.Value, {quote memberName}), new global::FScript.Extensibility.ScriptValue[] {{ {encoded} }}))")
          else
            match ClrBindings.tryMethodLink path with
            | None -> fail "FSC6002" $"Import '{path}.{memberName}' has no compiled CLR method binding." (Some span)
            | Some link ->
                let declaring = System.Type.GetType(link.DeclaringType, false)
                if isNull declaring then fail "FSC6003" $"Cannot resolve CLR declaring type '{link.DeclaringType}'." (Some span)
                let declaring = nonNull declaring
                let parameterTypes = link.Parameters |> Array.map (fun name ->
                    let t = System.Type.GetType(name,false)
                    if isNull t then fail "FSC6003" $"Cannot resolve CLR parameter type '{name}'." (Some span)
                    nonNull t)
                let method: MethodBase | null =
                    if link.IsConstructor then declaring.GetConstructor(parameterTypes)
                    else declaring.GetMethod(memberName, BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.Instance, null, parameterTypes, null)
                if isNull method then fail "FSC6006" $"Cannot resolve exact CLR overload '{declaring.FullName}.{memberName}'." (Some span)
                let method = nonNull method
                if method.IsStatic <> link.IsStatic || not method.IsPublic then fail "FSC6006" "CLR linkage accessibility or calling convention is invalid." (Some span)
                let rec arguments = function TFun(a,b) -> a :: arguments b | _ -> []
                let argumentTypes = arguments signature
                let receiverCount = if link.IsStatic || link.IsConstructor then 0 else 1
                if argumentTypes.Length <> parameterTypes.Length + receiverCount && not (parameterTypes.Length = 0 && argumentTypes = [TUnit]) then
                    fail "FSC6006" $"CLR binding '{memberName}' does not match the FScript arity." (Some span)
                curryApplied supplied signature [] (fun args result ->
                    let inputs = if parameterTypes.Length = 0 then [] else args |> List.skip receiverCount
                    let semanticInputs = if parameterTypes.Length = 0 then [] else argumentTypes |> List.skip receiverCount
                    let converted = List.map3 (fun t native value -> toClr t native value) semanticInputs (Array.toList parameterTypes) inputs |> String.concat comma
                    let call = if link.IsConstructor then $"new {typeName declaring}({converted})" elif link.IsStatic then $"{typeName declaring}.@{memberName}({converted})" else $"({args.[0]}).@{memberName}({converted})"
                    let returnType = match method with :? MethodInfo as m -> m.ReturnType | _ -> declaring
                    fromClr returnType result call)
        for declaration in program.Declarations do knownBindings.[declaration.Id] <- declaration.Value
        let mutable available=Set.ofSeq globalIds
        let within ids action =
            let previous=available
            available <- Set.union available (Set.ofSeq ids)
            try action () finally available <- previous
        let knownNode (root: Lir.Expression) =
            match root.Node with
            | Lir.Load id ->
                match knownBindings.TryGetValue id with
                | true,({Node=Lir.MakeClosure(_,captures)} as binding) when captures |> List.forall (fun capture -> Set.contains capture available) -> binding.Node
                | true,({Node=(Lir.Intrinsic _ | Lir.HostExternal _ | Lir.TargetImport _)} as binding) -> binding.Node
                | _ -> root.Node
            | node -> node
        let rec functionChain seen id =
            let fn=functions.[id]
            match fn.Body.Node with
            | Lir.MakeClosure(next,_) when not (Set.contains next seen) -> fn :: functionChain (Set.add id seen) next
            | _ -> [fn]
        let rec expression (value: Lir.Expression) =
            let emit = expression
            let directApplication () =
                let rec spine (node: Lir.Expression) =
                    match node.Node with
                    | Lir.CallClosure(fn,arg) | Lir.TailCallClosure(fn,arg) ->
                        let root,args,order=spine fn
                        let index=List.length args
                        root,args @ [index,arg],order @ [index,arg]
                    | Lir.PipeClosure(arg,fn) | Lir.TailPipeClosure(arg,fn) ->
                        let root,args,order=spine fn
                        let index=List.length args
                        root,args @ [index,arg],(index,arg)::order
                    | _ -> node,[],[]
                let root,args,order=spine value
                let rec signatureTypes = function TFun(a,b) -> a::signatureTypes b | _ -> []
                let parameters=signatureTypes root.Type
                let rootNode=knownNode root
                let supported =
                    match rootNode with
                    | Lir.Intrinsic _ | Lir.HostExternal _ | Lir.TargetImport _ -> args.Length=parameters.Length
                    | Lir.MakeClosure(id,_) -> args.Length=(functionChain Set.empty id).Length
                    | _ -> false
                if not supported || args.IsEmpty then None else
                    let names=args |> List.map (fun (index,_) -> index,fresh "nativeArgument") |> Map.ofList
                    let collectionCallback=
                        match rootNode,args with
                        | Lir.Intrinsic name,(_,callback)::_ when List.contains name ["List.map";"List.iter";"List.filter";"List.choose";"List.collect";"List.exists";"List.tryFind";"List.tryGet";"List.fold"] ->
                            match knownNode callback with
                            | Lir.MakeClosure(id,captures) when (functionChain Set.empty id).Length=(if name="List.fold" then 2 else 1) -> Some(name,id,captures)
                            | _ -> None
                        | _ -> None
                    let callbackCaptures=
                        match collectionCallback with
                        | Some(_,_,captures) -> captures |> List.filter (fun id -> not (globalIds.Contains id)) |> List.map (fun id -> id,fresh "callbackCapture")
                        | None -> []
                    let setup=order |> List.map (fun (index,argument) ->
                        if index=0 && collectionCallback.IsSome then
                            callbackCaptures |> List.map (fun (id,name) -> $"{csType symbolTypes.[id]} {name}={sym id};") |> String.concat space
                        else $"var {names.[index]}={emit argument};") |> String.concat space
                    let arguments=List.map2 (fun (index,(arg: Lir.Expression)) expected -> coerce arg.Type expected names.[index]) args (parameters |> List.take args.Length)
                    let result=
                        match collectionCallback,rootNode with
                        | Some(name,id,_),_ ->
                            let sourceIndex=if name="List.fold" then 2 else 1
                            let source=fresh "callbackSource"
                            let item=fresh "nativeItem"
                            let index=fresh "nativeIndex"
                            let state=fresh "nativeState"
                            let result=fresh "nativeList"
                            let chain=functionChain Set.empty id
                            let itemType=match parameters.[sourceIndex] with TList t -> t | _ -> invalidOp "Expected list argument."
                            let values=if name="List.fold" then [state;item] else [item]
                            let inputTypes=if name="List.fold" then [parameters.[1];itemType] else [itemType]
                            let inputs=List.map3 (fun code actual (parameter: Lir.Function) -> coerce actual parameter.ParameterType code) values inputTypes chain
                            let rawCall= $"Native{funName id}({String.concat comma ((List.map snd callbackCaptures) @ inputs)})"
                            let expectedReturn=
                                match name,value.Type with
                                | "List.map",TList output -> output
                                | "List.collect",TList output -> TList output
                                | "List.choose",TList output -> TOption output
                                | "List.fold",_ -> value.Type
                                | _ -> (List.last chain).ReturnType
                            let call=coerce (List.last chain).ReturnType expectedReturn rawCall
                            let body=
                                match name,value.Type with
                                | "List.map",TList output -> $"var {result}=new {csType output}[{source}.Length]; for (int {index}=0; {index}<{source}.Length; {index}++) {{ var {item}={source}[{index}]; {result}[{index}]={call}; }} return {result};"
                                | "List.iter",_ -> $"foreach (var {item} in {source}) {{ _={call}; }} return default;"
                                | "List.fold",_ -> $"{csType parameters.[1]} {state}={arguments.[1]}; foreach (var {item} in {source}) {{ {state}={call}; }} return {state};"
                                | "List.exists",_ -> $"foreach (var {item} in {source}) {{ if ({call}) return true; }} return false;"
                                | "List.tryFind",_ -> $"foreach (var {item} in {source}) {{ if ({call}) return {csType value.Type}.Some({item}); }} return default;"
                                | "List.tryGet",_ -> $"for (int {index}=0; {index}<{source}.Length; {index}++) {{ var {item}={source}[{index}]; if ({call}) return {csType value.Type}.Some({index}); }} return default;"
                                | "List.filter",_ -> $"var {result}=new System.Collections.Generic.List<{csType itemType}>(); foreach (var {item} in {source}) {{ if ({call}) {result}.Add({item}); }} return {result}.ToArray();"
                                | "List.collect",TList output -> $"var {result}=new System.Collections.Generic.List<{csType output}>(); foreach (var {item} in {source}) {{ {result}.AddRange({call}); }} return {result}.ToArray();"
                                | "List.choose",TList output -> $"var {result}=new System.Collections.Generic.List<{csType output}>(); foreach (var {item} in {source}) {{ var chosen={call}; if(chosen.HasValue) {result}.Add(chosen.Value); }} return {result}.ToArray();"
                                | _ -> invalidOp "Invalid specialized collection callback."
                            wrap (csType value.Type) $"var {source}={arguments.[sourceIndex]}; {body}"
                        | _,Lir.Intrinsic name | _,Lir.HostExternal(name,_) -> intrinsic name root.Type arguments
                        | _,Lir.TargetImport(path,name) -> targetImport path name root.Type root.Span arguments
                        | _,Lir.MakeClosure(id,captures) ->
                            let chain=functionChain Set.empty id
                            let captures=captures |> List.filter (fun id -> not (globalIds.Contains id)) |> List.map sym
                            let call= $"Native{funName id}({String.concat comma (captures @ arguments)})"
                            coerce (List.last chain).ReturnType value.Type call
                        | _ -> invalidOp "Unsupported direct application."
                    Some(wrap (csType value.Type) $"{setup} return {result};")
            let code =
                match (if assemblyMode then directApplication () else None) with
                | Some direct -> direct
                | None ->
                    match value.Node with
                    | Lir.Constant c -> constant c
                    | Lir.Load id -> coerce symbolTypes.[id] value.Type (sym id)
                    | Lir.MakeClosure(id,captures) ->
                        let fn = functions.[id]
                        let parameter = fresh "closureArg"
                        let captures = captures |> List.filter (fun id -> not (globalIds.Contains id)) |> List.map sym
                        let snapshots = if assemblyMode then captures |> List.map (fun original -> original,fresh "nativeCapture") else []
                        let passedCaptures=if assemblyMode then List.map snd snapshots else captures
                        let parameterType,resultType = match value.Type with TFun(a,b) -> a,b | _ -> fn.ParameterType,fn.ReturnType
                        let args = String.concat comma (passedCaptures @ [coerce parameterType fn.ParameterType parameter])
                        let call=coerce fn.ReturnType resultType $"{funName id}({args})"
                        let closure= $"(({csType value.Type})(({csType parameterType} {parameter}) => {call}))"
                        if assemblyMode && not snapshots.IsEmpty then
                            let setup=snapshots |> List.map (fun (original,snapshot) -> $"var {snapshot}={original};") |> String.concat space
                            wrap (csType value.Type) $"{setup} return {closure};"
                        else closure
                    | Lir.CallClosure(fn,arg) | Lir.TailCallClosure(fn,arg) ->
                        let parameterType,resultType = match fn.Type with TFun(a,b) -> a,b | _ -> arg.Type,value.Type
                        coerce resultType value.Type $"({emit fn}).Invoke({coerce arg.Type parameterType (emit arg)})"
                    | Lir.PipeClosure(arg,fn) | Lir.TailPipeClosure(arg,fn) ->
                        let parameter = fresh "pipelineArgument"
                        let parameterType,resultType=match fn.Type with TFun(a,b) -> a,b | _ -> arg.Type,value.Type
                        let call=coerce resultType value.Type $"({emit fn}).Invoke({coerce arg.Type parameterType parameter})"
                        wrap (csType value.Type) $"var {parameter} = {emit arg}; return {call};"
                    | Lir.Branch(condition,yes,no) ->
                        let yesCode=coerce yes.Type value.Type (emit yes)
                        let noCode=coerce no.Type value.Type (emit no)
                        if assemblyMode then $"({emit condition} ? ({csType value.Type})({yesCode}) : ({csType value.Type})({noCode}))"
                        else $"({emit condition} ? {yesCode} : {noCode})"
                    | Lir.Operation(operation,[left;right]) ->
                        let op =
                            match operation with
                            | Lir.IntAdd | Lir.FloatAdd | Lir.NumericAdd -> "+" | Lir.IntSubtract | Lir.FloatSubtract | Lir.NumericSubtract -> "-"
                            | Lir.IntMultiply | Lir.FloatMultiply | Lir.NumericMultiply -> "*" | Lir.IntDivide | Lir.FloatDivide | Lir.NumericDivide -> "/"
                            | Lir.IntRemainder -> "%" | Lir.NumericLessThan -> "<" | Lir.NumericGreaterThan -> ">"
                            | Lir.NumericLessThanOrEqual -> "<=" | Lir.NumericGreaterThanOrEqual -> ">="
                            | Lir.BooleanAnd -> "&&" | Lir.BooleanOr -> "||" | Lir.StructuralEqual -> "="
                        if op = "=" then $"{eqName left.Type}({emit left}, {coerce right.Type left.Type (emit right)})" else $"({emit left} {op} {emit right})"
                    | Lir.Tuple items ->
                        let itemTypes=match value.Type with TTuple items -> items | _ -> items |> List.map (fun (item: Lir.Expression) -> item.Type)
                        let args=List.map2 (fun t (item: Lir.Expression) -> coerce item.Type t (emit item)) itemTypes items |> String.concat comma
                        $"new {csType value.Type}({args})"
                    | Lir.List items ->
                        let itemType=match value.Type with TList t -> t | _ -> invalidOp "Expected list."
                        let args=items |> List.map (fun item -> coerce item.Type itemType (emit item)) |> String.concat comma
                        $"new {csType value.Type} {{ {args} }}"
                    | Lir.Record items ->
                        let byName = Map.ofList items
                        let destination = fields value.Type |> Map.toList
                        if List.map fst items = List.map fst destination || items |> List.forall (snd >> LirOptimization.effect >> (=) LirOptimization.Pure) then
                            let args = destination |> List.map (fun (name,t) -> coerce byName.[name].Type t (emit byName.[name])) |> String.concat comma
                            $"new {csType value.Type}({args})"
                        else
                            let temporaries = items |> List.map (fun (name,item) -> name,(fresh "recordField",item))
                            let setup = temporaries |> List.map (fun (_, (temp,item)) -> $"var {temp}={emit item};") |> String.concat space
                            let temporaries = Map.ofList temporaries
                            let args = destination |> List.map (fun (name,t) -> let temp,item=temporaries.[name] in coerce item.Type t temp) |> String.concat comma
                            wrap (csType value.Type) $"{setup} return new {csType value.Type}({args});"
                    | Lir.FieldGet(target,name) ->
                        let code = readField target.Type name (emit target)
                        let nativeName = match target.Type with TNamed name | THostManaged(name,_) -> Some name | _ -> None
                        match nativeName |> Option.bind nativeTypes.TryFind with
                        | Some native ->
                            let prop = native.GetProperty name
                            let field = native.GetField name
                            if not (isNull prop) then fromClr (nonNull prop).PropertyType value.Type code
                            elif not (isNull field) then fromClr (nonNull field).FieldType value.Type code
                            else fail "FSC6007" $"CLR type '{native.FullName}' has no public field or property '{name}'." (Some value.Span)
                        | None -> code
                    | Lir.SomeValue item ->
                        let itemType=match value.Type with TOption t -> t | _ -> item.Type
                        $"{csType value.Type}.Some({coerce item.Type itemType (emit item)})"
                    | Lir.NoneValue -> $"default({csType value.Type})"
                    | Lir.Cons(head,tail) ->
                        let itemType=match value.Type with TList t -> t | _ -> head.Type
                        $"new[] {{ {coerce head.Type itemType (emit head)} }}.Concat({coerce tail.Type value.Type (emit tail)}).ToArray()"
                    | Lir.Append(a,b) -> $"({coerce a.Type value.Type (emit a)}).Concat({coerce b.Type value.Type (emit b)}).ToArray()"
                    | Lir.Let(id,_,recursive,binding,body) ->
                        let result=within [id] (fun () -> if assemblyMode then coerce body.Type value.Type (emit body) else emit body)
                        let bindingCode=if recursive then within [id] (fun () -> emit binding) else emit binding
                        if recursive then wrap (csType value.Type) $"{csType binding.Type} {sym id} = default!; {sym id} = {bindingCode}; return {result};"
                        else wrap (csType value.Type) $"{csType binding.Type} {sym id} = {bindingCode}; return {result};"
                    | Lir.LetRecursiveGroup(bindings,body) ->
                        let declarations = bindings |> List.map (fun (id,_,t,_) -> $"{csType t} {sym id} = default!;") |> String.concat space
                        let scope=bindings |> List.map (fun (id,_,_,_) -> id)
                        let assignments = within scope (fun () -> bindings |> List.map (fun (id,_,_,v) -> $"{sym id} = {emit v};") |> String.concat space)
                        let result=within scope (fun () -> if assemblyMode then coerce body.Type value.Type (emit body) else emit body)
                        wrap (csType value.Type) $"{declarations} {assignments} return {result};"
                    | Lir.Fail message -> $"FsRuntime.Fail<{csType value.Type}>({emit message})"
                    | Lir.Range(first,last) -> $"FsRuntime.Range({emit first}, {emit last})"
                    | Lir.RecordUpdate(target,updates) ->
                        let original = fresh "original"
                        let setup,changed = updates |> List.map (fun (name,item) -> let temp=fresh "updatedField" in $"var {temp}={emit item};",(name,(item.Type,temp))) |> List.unzip
                        let changed = Map.ofList changed
                        let args = fields value.Type |> Map.toList |> List.map (fun (name,t) ->
                            match changed.TryFind name with Some(source,code) -> coerce source t code | None -> coerce (fields target.Type).[name] t $"{original}.{fieldName target.Type name}") |> String.concat comma
                        wrap (csType value.Type) $"var {original} = {emit target}; {String.concat space setup} return new {csType value.Type}({args});"
                    | Lir.RecordConvert(target,t) -> coerce target.Type t (emit target)
                    | Lir.IndexGet(target,index) ->
                        match target.Type with
                        | TList _ ->
                            let values=fresh "indexedValues"
                            wrap (csType value.Type) $"var {values}={emit target}; return FsRuntime.At({emit index}, {values});"
                        | _ -> $"({emit target}).Find({emit index})"
                    | Lir.Interpolate parts ->
                        parts |> List.map (function Lir.TextPart text -> quote text | Lir.ValuePart item -> $"{interpName item.Type}({emit item})") |> function [] -> "\"\"" | parts -> "(" + String.concat " + " parts + ")"
                    | Lir.UnionCase(_,caseName,payload) ->
                        let cases = match resolve value.Type with TUnion(_,cases) -> cases | _ -> Map.empty
                        if cases |> Map.forall (fun _ -> Option.isNone) then $"{csType value.Type}.{ident caseName}"
                        elif nativeTypes |> Map.exists (fun _ native -> typeName native = csType value.Type) then
                            match payload with None -> $"{csType value.Type}.@{caseName}" | Some item -> $"{csType value.Type}.New{caseName}({emit item})"
                        else $"new {csType value.Type}.{ident caseName}({payload |> Option.map emit |> Option.defaultValue String.Empty})"
                    | Lir.Map entries ->
                        let target = fresh "map"
                        let operations = entries |> List.map (function
                            | Lir.KeyValue(key,item) -> $"{target} = {target}.Add({emit key}, {emit item});"
                            | Lir.Spread item -> $"foreach (var entry in ({emit item}).Entries) {{ if (!{target}.Find(entry.Key).HasValue) {target} = {target}.Add(entry.Key,entry.Value); }}") |> String.concat space
                        wrap (csType value.Type) $"var {target} = new {csType value.Type}(); {operations} return {target};"
                    | Lir.Intrinsic name | Lir.HostExternal(name,_) -> intrinsic name value.Type []
                    | Lir.TargetImport(path,memberName) -> targetImport path memberName value.Type value.Span []
                    | Lir.RuntimeContext ->
                        $"new {csType value.Type}(Environment.GetCommandLineArgs().Skip(1).ToArray(), Option<string>.Some(Path.GetFileName(Environment.GetCommandLineArgs()[0])))"
                    | Lir.ForEach(id,_,t,source,body) ->
                        let bodyCode=within [id] (fun () -> emit body)
                        wrap "Unit" $"foreach ({csType t} {sym id} in {emit source}) {{ _ = {bodyCode}; }} return default;"
                    | Lir.TypeToken t -> $"new TypeToken({quote (Types.typeToString t)})"
                    | Lir.DecisionTree(target,cases) -> decision target cases value.Type None
                    | _ -> fail "FSC6008" "Invalid LIR expression reached C# generation." (Some value.Span)
            code
        and decision target cases resultType returnBody =
            let root = fresh "matchValue"
            let allChecks checks =
                if List.contains "false" checks then "false"
                else match List.filter ((<>) "true") checks with [] -> "true" | values -> String.concat " && " (values |> List.map (fun value -> "("+value+")"))
            let rec typeTest code actual expected =
                match resolve expected,resolve actual with
                | (TRecord required | TRecordShape(_,required,_)),(TRecord present | TRecordShape(_,present,_) | THostManaged(_,present)) ->
                    required |> Map.toList |> List.map (fun (name,t) ->
                        match Map.tryFind name present with
                        | Some fieldType -> typeTest (readField actual name code) fieldType t
                        | None -> "false") |> allChecks
                | TList required,TList present ->
                    let item=fresh "typeItem"
                    match typeTest item present required with
                    | "true" -> "true"
                    | "false" -> $"({code}).Length == 0"
                    | test -> $"({code}).All({item} => {test})"
                | TOption required,TOption present ->
                    match typeTest $"({code}).Value" present required with
                    | "true" -> "true"
                    | "false" -> $"!({code}).HasValue"
                    | test -> $"(!({code}).HasValue || ({test}))"
                | TTuple required,TTuple present when required.Length=present.Length ->
                    List.zip required present |> List.mapi (fun index (r,p) -> typeTest $"({code}).Item{index}" p r) |> allChecks
                | TMap _,TMap _ | TTask _,TTask _ -> "true"
                | required,present -> if required=present then "true" else "false"
            let projection checks root rootType path =
                let rec loop prefix root rootType path =
                    match path with
                    | [] -> root,rootType
                    | step :: rest ->
                        let code,t =
                            match step,resolve rootType with
                            | Lir.ListHead,TList t -> $"({root})[0]",t
                            | Lir.ListTail,TList t -> $"({root}).Skip(1).ToArray()",TList t
                            | Lir.TupleItem index,TTuple items -> $"({root}).Item{index}",items.[index]
                            | Lir.RecordField name,(TRecord fields | THostManaged(_,fields)) -> readField rootType name root,fields.[name]
                            | Lir.OptionValue,TOption t -> $"({root}).Value",t
                            | Lir.UnionPayload,TUnion(_,cases) ->
                                let caseName = checks |> List.tryPick (function Lir.UnionCaseCheck(path,_,name,true) when path=prefix -> Some name | _ -> None) |> Option.defaultWith (fun () -> fail "FSC6008" "Union payload projection requires a preceding case check." None)
                                let payload = cases.[caseName] |> Option.get
                                $"(({csType rootType}.{ident caseName})({root})).Value",payload
                            | _ -> fail "FSC6008" "Invalid pattern projection." None
                        loop (prefix @ [step]) code t rest
                loop [] root rootType path
            let rec bindings (plan: Lir.PatternPlan) =
                plan.Bindings @ (plan.Maps |> List.collect (fun map -> (map.Clauses |> List.collect (fun clause -> bindings clause.Key @ bindings clause.Value)) @ (map.Tail |> Option.map bindings |> Option.defaultValue [])))
            let caseCode (case: Lir.DecisionCase) =
                let declared = bindings case.Plan |> List.distinctBy _.Id |> List.map (fun b -> $"{csType b.Type} {sym b.Id} = default!;") |> String.concat space
                let rec planCode root rootType (plan: Lir.PatternPlan) seen success =
                    let checks = plan.Checks |> List.map (fun check ->
                        let checkAt path predicate = let code,t=projection plan.Checks root rootType path in predicate code t
                        match check with
                        | Lir.LiteralCheck(path,c) -> checkAt path (fun code t -> $"{eqName t}({code},{constant c})")
                        | Lir.EmptyListCheck path -> checkAt path (fun code _ -> $"({code}).Length == 0")
                        | Lir.NonEmptyListCheck path -> checkAt path (fun code _ -> $"({code}).Length > 0")
                        | Lir.SomeCheck path -> checkAt path (fun code _ -> $"({code}).HasValue")
                        | Lir.NoneCheck path -> checkAt path (fun code _ -> $"!({code}).HasValue")
                        | Lir.UnionCaseCheck(path,_,name,_) -> checkAt path (fun code t ->
                            let allEmpty = match resolve t with TUnion(_,cases) -> cases |> Map.forall (fun _ -> Option.isNone) | _ -> false
                            if allEmpty then $"({code}) == {csType t}.{ident name}" else $"({code}) is {csType t}.{ident name}")
                        | Lir.TypeCheck(path,expected) -> checkAt path (fun code actual -> typeTest code actual expected)
                        | Lir.TupleArityCheck _ | Lir.RecordFieldCheck _ -> "true")
                    let mutable seen = seen
                    let bindingCode = plan.Bindings |> List.map (fun b ->
                        let projected,sourceType = projection plan.Checks root rootType b.Projection
                        if Set.contains b.Id seen then $"if (!{eqName b.Type}({sym b.Id},{projected})) return false;"
                        else seen <- Set.add b.Id seen; $"{sym b.Id} = {coerce sourceType b.Type projected};") |> String.concat space
                    let boundIds plan = bindings plan |> List.map _.Id |> Set.ofList
                    let rec maps (remaining: Lir.MapPlan list) seen =
                        match remaining with
                        | [] -> success
                        | map :: rest ->
                            let source,t = projection plan.Checks root rootType map.Projection
                            let keyType,valueType = match t with TMap(k,v) -> k,v | _ -> invalidOp "Expected map pattern."
                            let entries = fresh "entries"
                            let mapBindings =
                                [ yield! map.Clauses |> List.collect (fun clause -> [boundIds clause.Key; boundIds clause.Value])
                                  yield! map.Tail |> Option.map boundIds |> Option.toList ] |> Set.unionMany
                            let afterMap=Set.union seen mapBindings
                            let rec clauses index remainingEntries seen locallyBound =
                                if index = map.Clauses.Length then
                                    let remaining=maps rest afterMap
                                    let tail = match map.Tail with Some tail -> planCode $"new {csType t}({remainingEntries})" t tail seen remaining | None -> if map.RequireEmptyRemainder then $"if ({remainingEntries}.Length != 0) return false; {remaining}" else remaining
                                    tail
                                else
                                    let candidate = fresh "candidate"
                                    let next = fresh "remaining"
                                    let attempt = fresh "attempt"
                                    let clause = map.Clauses.[index]
                                    let afterKey=Set.union seen (boundIds clause.Key)
                                    let afterValue=Set.union afterKey (boundIds clause.Value)
                                    let local=Set.union locallyBound (Set.union (boundIds clause.Key) (boundIds clause.Value))
                                    let continuation = $"var {next} = {remainingEntries}.Where((entry,index) => index != {candidate}).ToArray(); {clauses (index+1) next afterValue local}"
                                    let code = planCode $"{remainingEntries}[{candidate}].Key" keyType clause.Key seen (planCode $"{remainingEntries}[{candidate}].Value" valueType clause.Value afterKey "return true;")
                                    let firstOnly=Lir.simpleKeyBinding clause.Key |> Option.exists (fun id -> not (Set.contains id locallyBound))
                                    let limit=if firstOnly then $"Math.Min(1,{remainingEntries}.Length)" else $"{remainingEntries}.Length"
                                    $"for (int {candidate}=0; {candidate}<{limit}; {candidate}++) {{ bool {attempt}() {{ {code} }} if ({attempt}()) {{ {continuation} }} }} return false;"
                            $"var {entries} = ({source}).Entries; {clauses 0 entries seen Set.empty}"
                    let checks = checks |> List.filter ((<>) "true")
                    let prefix = if checks.IsEmpty then "" else "if (!(" + String.concat " && " (checks |> List.map (fun c -> "("+c+")")) + ")) return false; "
                    if List.contains "false" checks then "return false;"
                    else $"{prefix}{bindingCode} {maps plan.Maps seen}"
                let matcher = fresh "matches"
                let test = planCode root target.Type case.Plan Set.empty "return true;"
                within (Lir.planBindingIds case.Plan) (fun () ->
                    let guard = case.Guard |> Option.map expression |> Option.defaultValue "true"
                    match returnBody with
                    | Some emit -> $"{{ {declared} bool {matcher}() {{ {test} }} if ({matcher}() && ({guard})) {{ {emit case.Body} }} }}"
                    | None -> $"{{ {declared} bool {matcher}() {{ {test} }} if ({matcher}() && ({guard})) return {coerce case.Body.Type resultType (expression case.Body)}; }}")
            let cases = cases |> List.map caseCode |> String.concat "\n"
            let targetType=if assemblyMode then csType target.Type else "var"
            let statements= $"{targetType} {root} = {expression target}; {cases} throw new InvalidOperationException(\"Pattern match failure\");"
            match returnBody with Some _ -> statements | None -> wrap (csType resultType) statements
        let typeDeclarations = StringBuilder()
        let addLine (value: string) = typeDeclarations.AppendLine(value) |> ignore
        let emittedTypes = Collections.Generic.HashSet<string>()
        for t in types |> Seq.sort do
            let name = csType t
            if emittedTypes.Add name && not (nativeTypes |> Map.exists (fun _ native -> typeName native=name)) then
                match resolve t with
                | TTuple items ->
                    let args = items |> List.mapi (fun i t -> $"{csType t} Item{i}") |> String.concat comma
                    addLine $"public sealed record {name}({args});"
                | TRecord fields ->
                    let args = fields |> Map.toList |> List.map (fun (field,t) -> $"{csType t} {fieldName t field}") |> String.concat comma
                    addLine $"public sealed record {name}({args});"
                | TUnion(_,cases) when cases |> Map.forall (fun _ -> Option.isNone) ->
                    addLine $"public enum {name} {{ {cases |> Map.toList |> List.map (fst >> ident) |> String.concat comma} }}"
                | TUnion(_,cases) ->
                    addLine $"public abstract record {name} {{"
                    for case,payload in cases |> Map.toList do
                        let arg = payload |> Option.map (fun t -> $"{csType t} Value") |> Option.defaultValue String.Empty
                        addLine $"public sealed record {ident case}({arg}) : {name};"
                    addLine "}"
                | _ -> ()
        let helpers = StringBuilder()
        let addHelper (value: string) = helpers.AppendLine(value) |> ignore
        for t in types |> Seq.sort do
            if not (NativeSpecialization.containsVariable t) then
                let joinChecks values = if List.isEmpty values then "true" else String.concat " && " values
                let equalBody,formatBody =
                    match resolve t with
                    | TUnit -> "true",quote "()"
                    | TInt -> "a == b","value.ToString(CultureInfo.InvariantCulture)"
                    | TFloat -> "a == b","FsRuntime.FloatText(value)"
                    | TBool -> "a == b","(value ? \"true\" : \"false\")"
                    | TString -> "a == b","FsRuntime.Quote(value)"
                    | TTypeToken -> (if assemblyMode then "a.Name == b.Name" else "a == b"),"\"<type \" + value.Name + \">\""
                    | TFun _ -> "false",quote "<fun>"
                    | TTask _ -> "false",quote "<task>"
                    | TList item -> $"a.Length == b.Length && a.Where((item,index) => !{eqName item}(item,b[index])).Any() == false",$"\"[\" + string.Join(\";\", value.Select(item => {fmtName item}(item))) + \"]\""
                    | TOption item -> $"a.HasValue == b.HasValue && (!a.HasValue || {eqName item}(a.Value,b.Value))",$"(value.HasValue ? \"Some \" + {fmtName item}(value.Value) : \"None\")"
                    | TTuple items ->
                        items |> List.mapi (fun i t -> $"{eqName t}(a.Item{i},b.Item{i})") |> joinChecks,
                        "\"(\" + " + (items |> List.mapi (fun i t -> $"{fmtName t}(value.Item{i})") |> String.concat " + \", \" + ") + " + \")\""
                    | TRecord fields | THostManaged(_,fields) ->
                        let checks = fields |> Map.toList |> List.map (fun (field,ft) -> $"{eqName ft}({readField t field aCode},{readField t field bCode})") |> joinChecks
                        let parts = fields |> Map.toList |> List.map (fun (field,ft) -> quote (field+" = ") + " + " + fmtName ft + "(" + readField t field "value" + ")")
                        checks, "\"{ \" + " + (if parts.IsEmpty then "\"\"" else String.concat " + \"; \" + " parts) + " + \" }\""
                    | TMap(k,v) ->
                        $"a.Entries.Length == b.Entries.Length && !a.Entries.Where((e,i) => !{eqName k}(e.Key,b.Entries[i].Key) || !{eqName v}(e.Value,b.Entries[i].Value)).Any()",
                        $"\"map {{ \" + string.Join(\"; \",value.Entries.Select(e => {fmtName k}(e.Key) + \" => \" + {fmtName v}(e.Value))) + \" }}\""
                    | TUnion(_,cases) when cases |> Map.forall (fun _ -> Option.isNone) ->
                        "a == b", "value switch { " + (cases |> Map.toList |> List.map (fun (case,_) -> $"{csType t}.{ident case} => {quote case}") |> String.concat comma) + ", _ => throw new InvalidOperationException() }"
                    | TUnion(_,cases) ->
                        let equal = cases |> Map.toList |> List.map (fun (case,payload) ->
                            let test = payload |> Option.map (fun p -> $"{eqName p}(x.Value,y.Value)") |> Option.defaultValue "true"
                            $"({csType t}.{ident case} x, {csType t}.{ident case} y) => {test}") |> String.concat comma
                        let format = cases |> Map.toList |> List.map (fun (case,payload) ->
                            let result = payload |> Option.map (fun p -> quote (case+" ") + " + " + fmtName p + "(item.Value)") |> Option.defaultValue (quote case)
                            $"{csType t}.{ident case} item => {result}") |> String.concat comma
                        $"(a,b) switch {{ {equal}, _ => false }}",$"value switch {{ {format}, _ => throw new InvalidOperationException() }}"
                    | value -> fail "FSC6004" $"Unsupported compiled value '{Types.typeToString value}'." None
                addHelper $"private static bool {eqName t}({csType t} a, {csType t} b) => {equalBody};"
                addHelper $"private static string {fmtName t}({csType t} value) => {formatBody};"
                let interpolation =
                    match resolve t with
                    | TString -> "value"
                    | TMap(k,v) -> $"\"map {{ \" + string.Join(\"; \",value.Entries.Select(e => {fmtName k}(e.Key) + \" => \" + {interpName v}(e.Value))) + \" }}\""
                    | _ -> Text.RegularExpressions.Regex.Replace(formatBody,@"\bFormat_([0-9a-f]{12})\(","Interpolate_$1(")
                addHelper $"private static string {interpName t}({csType t} value) => {interpolation};"
        for t in types |> Seq.sort do
            if not (NativeSpecialization.containsVariable t) then
                let unsupported="throw new InvalidOperationException(\"Unsupported JSON value\")"
                let encode,decode =
                    match resolve t with
                    | TUnit -> quote "null","default(Unit)"
                    | TInt -> "value.ToString(CultureInfo.InvariantCulture)","value.GetInt64()"
                    | TFloat -> "value.ToString(CultureInfo.InvariantCulture)","value.GetDouble()"
                    | TBool -> "(value ? \"true\" : \"false\")","value.GetBoolean()"
                    | TString -> "JsonSerializer.Serialize(value)","value.GetString() ?? throw new InvalidOperationException()"
                    | TOption item ->
                        $"(value.HasValue ? {jsonEncodeName item}(value.Value) : \"null\")",
                        $"(value.ValueKind == JsonValueKind.Null ? default : {csType t}.Some({jsonDecodeName item}(value)))"
                    | TList item ->
                        $"\"[\" + string.Join(\",\",value.Select({jsonEncodeName item})) + \"]\"",
                        $"value.EnumerateArray().Select({jsonDecodeName item}).ToArray()"
                    | TTuple items ->
                        let encoded=items |> List.mapi (fun i t -> $"{jsonEncodeName t}(value.Item{i})") |> String.concat " + \",\" + "
                        let decoded=items |> List.mapi (fun i t -> $"{jsonDecodeName t}(value[{i}])") |> String.concat comma
                        $"\"[\" + {encoded} + \"]\"",$"(value.GetArrayLength()=={items.Length} ? new {csType t}({decoded}) : throw new InvalidOperationException())"
                    | TRecord fields | THostManaged(_,fields) ->
                        let encoded=fields |> Map.toList |> List.map (fun (name,ft) -> quote (quote name+":") + " + " + jsonEncodeName ft + "("+readField t name valueCode+")") |> String.concat " + \",\" + "
                        let decoded=fields |> Map.toList |> List.mapi (fun i (name,ft) ->
                            let missing=match ft with TOption _ -> "default" | _ -> "throw new InvalidOperationException()"
                            $"(value.TryGetProperty({quote name},out var field{i}) ? {jsonDecodeName ft}(field{i}) : {missing})") |> String.concat comma
                        let encode=if fields.IsEmpty then quote "{}" else $"\"{{\" + {encoded} + \"}}\""
                        let decode=match resolve t with THostManaged _ -> unsupported | _ -> $"new {csType t}({decoded})"
                        encode,decode
                    | TMap(k,v) ->
                        let keyText,keyParse =
                            match resolve k with
                            | TString -> "e.Key","property.Name"
                            | TInt -> "e.Key.ToString(CultureInfo.InvariantCulture)","long.Parse(property.Name,CultureInfo.InvariantCulture)"
                            | TBool -> "(e.Key ? \"true\" : \"false\")","bool.Parse(property.Name)"
                            | _ -> unsupported,unsupported
                        if keyText=unsupported then unsupported,unsupported else
                            $"\"{{\" + string.Join(\",\",value.Entries.Select(e=>JsonSerializer.Serialize({keyText})+\":\"+{jsonEncodeName v}(e.Value))) + \"}}\"",
                            $"value.EnumerateObject().Aggregate(new {csType t}(),(map,property)=>map.Add({keyParse},{jsonDecodeName v}(property.Value)))"
                    | _ -> unsupported,unsupported
                addHelper $"private static string {jsonEncodeName t}({csType t} value) => {encode};"
                addHelper $"private static {csType t} {jsonDecodeName t}(JsonElement value) => {decode};"
        if hasSdk then
            let sv="global::FScript.Extensibility.ScriptValue"
            let mapKey="global::FScript.Extensibility.MapKey"
            let list values = "global::Microsoft.FSharp.Collections.ListModule.OfArray(new " + sv + "[] { " + String.concat comma values + " })"
            let none="null"
            let some value="SdkInterop.Some("+value+")"
            let rec keyEncode t code =
                match resolve t with
                | TString -> $"{mapKey}.NewMKString({code})" | TInt -> $"{mapKey}.NewMKInt({code})" | TBool -> $"{mapKey}.NewMKBool({code})"
                | TUnion(name,cases) ->
                    let names=cases |> Map.toList |> List.map (fun (case,_) -> $"{csType t}.{ident case} => {quote case}") |> String.concat comma
                    $"{mapKey}.NewMKUnionCase({quote name}, {code} switch {{ {names}, _ => throw new InvalidOperationException() }})"
                | _ -> fail "FSC6008" "Unsupported SDK map key." None
            let keyDecode t code =
                match resolve t with
                | TString -> $"(({mapKey}.MKString)({code})).Item" | TInt -> $"(({mapKey}.MKInt)({code})).Item" | TBool -> $"(({mapKey}.MKBool)({code})).Item"
                | TUnion(_,cases) ->
                    let names=cases |> Map.toList |> List.map (fun (case,_) -> $"{quote case} => {csType t}.{ident case}") |> String.concat comma
                    $"(({mapKey}.MKUnionCase)({code})).Item2 switch {{ {names}, _ => throw new InvalidOperationException() }}"
                | _ -> fail "FSC6008" "Unsupported SDK map key." None
            for t in types |> Seq.sort do
                if not (NativeSpecialization.containsVariable t) then
                    let encode,decode =
                        match resolve t with
                        | TUnit -> sv+".VUnit","default(Unit)"
                        | TInt -> sv+".NewVInt(value)",$"(({sv}.VInt)value).Item"
                        | TFloat -> sv+".NewVFloat(value)",$"(({sv}.VFloat)value).Item"
                        | TBool -> sv+".NewVBool(value)",$"(({sv}.VBool)value).Item"
                        | TString -> sv+".NewVString(value)",$"(({sv}.VString)value).Item"
                        | TList item ->
                            $"{sv}.NewVList(global::Microsoft.FSharp.Collections.ListModule.OfArray(value.Select({encName item}).ToArray()))",
                            $"(({sv}.VList)value).Item.Select({decName item}).ToArray()"
                        | TTuple items ->
                            let values=items |> List.mapi (fun i t -> $"{encName t}(value.Item{i})")
                            let decoded=items |> List.mapi (fun i t -> $"{decName t}((({sv}.VTuple)value).Item[{i}])") |> String.concat comma
                            $"{sv}.NewVTuple({list values})",$"new {csType t}({decoded})"
                        | TOption item ->
                            let encodedValue=some (encName item+"(value.Value)")
                            $"{sv}.NewVOption(value.HasValue ? {encodedValue} : null)",
                            wrap (csType t) $"var option = (({sv}.VOption)value).Item; return option is null ? default : {csType t}.Some({decName item}(option.Value));"
                        | TRecord fields ->
                            let values=fields |> Map.toList |> List.map (fun (name,ft) -> $"Tuple.Create({quote name}, {encName ft}({readField t name valueCode}))") |> String.concat comma
                            let decoded=fields |> Map.toList |> List.map (fun (name,ft) -> $"{decName ft}((({sv}.VRecord)value).Item[{quote name}])") |> String.concat comma
                            $"{sv}.NewVRecord(SdkInterop.Fields(new Tuple<string,{sv}>[] {{ {values} }}))",$"new {csType t}({decoded})"
                        | THostManaged(name,_) ->
                            if nativeTypes.ContainsKey name then fail "FSC6009" "A program cannot combine native and SDK projections of the same host-managed type." None
                            $"{sv}.NewVHostManaged(value)",$"((({sv}.VHostManaged)value).Item.Descriptor.Name == {quote name} ? (({sv}.VHostManaged)value).Item : throw new InvalidOperationException(\"Host type mismatch\"))"
                        | TFun(a,b) ->
                            $"SdkInterop.Callable(argument => {encName b}(value({decName a}(argument))))",
                            $"({csType a} argument) => {decName b}(SdkInterop.Apply(value, {encName a}(argument)))"
                        | TTask item ->
                            $"SdkInterop.TaskValue(Task.Run(async () => {encName item}(await value)))",$"Task.Run(() => {decName item}(SdkInterop.Await(value)))"
                        | TMap(k,v) ->
                            let encodedKey=keyEncode k "e.Key"
                            let decodedKey=keyDecode k "e.Key"
                            $"{sv}.NewVMap(global::Microsoft.FSharp.Collections.MapModule.OfSeq(value.Entries.Select(e => Tuple.Create({encodedKey},{encName v}(e.Value)))))",
                            $"new {csType t}((({sv}.VMap)value).Item.Select(e => new {csType t}.Entry({decodedKey},{decName v}(e.Value))).ToArray())"
                        | TUnion(name,cases) ->
                            let isEnum=cases |> Map.forall (fun _ -> Option.isNone)
                            let encoded=cases |> Map.toList |> List.map (fun (case,payload) ->
                                let pattern=if isEnum then $"{csType t}.{ident case}" else $"{csType t}.{ident case} item"
                                let payload=payload |> Option.map (fun t -> some (encName t+"(item.Value)")) |> Option.defaultValue none
                                $"{pattern} => {sv}.NewVUnionCase({quote name},{quote case},{payload})") |> String.concat comma
                            let decoded=cases |> Map.toList |> List.map (fun (case,payload) ->
                                let result=if isEnum then $"{csType t}.{ident case}" else
                                                let arg=payload |> Option.map (fun t -> $"{decName t}(((({sv}.VUnionCase)value).Item3 ?? throw new InvalidOperationException()).Value)") |> Option.defaultValue String.Empty
                                                $"new {csType t}.{ident case}({arg})"
                                $"{quote case} => {result}") |> String.concat comma
                            $"value switch {{ {encoded}, _ => throw new InvalidOperationException() }}",
                            $"(({sv}.VUnionCase)value).Item2 switch {{ {decoded}, _ => throw new InvalidOperationException() }}"
                        | TTypeToken ->
                            let names=types |> Seq.filter (NativeSpecialization.containsVariable >> not) |> Seq.distinctBy Types.typeToString |> Seq.map (fun t -> $"{quote (Types.typeToString t)} => {sdkType (resolve t)}") |> String.concat comma
                            $"{sv}.NewVTypeToken(value.Name switch {{ {names}, _ => throw new InvalidOperationException(\"Unknown type token\") }})",$"new TypeToken(global::FScript.Extensibility.Types.typeToString((({sv}.VTypeToken)value).Item))"
                        | _ -> fail "FSC6008" "Unresolved SDK transport type." None
                    addHelper $"private static {sv} {encName t}({csType t} value) => {encode};"
                    addHelper $"private static {csType t} {decName t}({sv} value) => {decode};"
        let members = StringBuilder()
        let addMember (value: string) = members.AppendLine(value) |> ignore
        let nativeBody (fn: Lir.Function) (chain: Lir.Function list) =
            let final=List.last chain
            let mutable tailLoop=false
            let rec emitReturn (value: Lir.Expression) =
                let rec spine (node: Lir.Expression) =
                    match node.Node with
                    | Lir.CallClosure(callee,arg) | Lir.TailCallClosure(callee,arg) ->
                        let root,args,order=spine callee
                        let index=List.length args
                        root,args @ [index,arg],order @ [index,arg]
                    | Lir.PipeClosure(arg,callee) | Lir.TailPipeClosure(arg,callee) ->
                        let root,args,order=spine callee
                        let index=List.length args
                        root,args @ [index,arg],(index,arg)::order
                    | _ -> node,[],[]
                let root,args,order=spine value
                let rootNode=knownNode root
                let self=
                    match rootNode with
                    | Lir.MakeClosure(id,captures) -> id=fn.Id && captures=fn.Captures && args.Length=List.length chain
                    | _ -> false
                if self then
                    tailLoop <- true
                    let temps=args |> List.map (fun (index,_) -> index,fresh "tailNext") |> Map.ofList
                    let setup=order |> List.map (fun (index,arg) ->
                        let parameter=List.item index chain
                        $"{csType parameter.ParameterType} {temps.[index]}={coerce arg.Type parameter.ParameterType (expression arg)};")
                    let assign=chain |> List.mapi (fun index parameter -> $"{sym parameter.Parameter}={temps.[index]};")
                    String.concat space (setup @ assign @ ["continue;"])
                else
                    match value.Node with
                    | Lir.Let(id,_,false,binding,body) ->
                        let bindingCode=expression binding
                        let bodyCode=within [id] (fun () -> emitReturn body)
                        $"{{ {csType binding.Type} {sym id}={bindingCode}; {bodyCode} }}"
                    | Lir.Branch(condition,yes,no) -> $"if ({expression condition}) {{ {emitReturn yes} }} else {{ {emitReturn no} }}"
                    | Lir.DecisionTree(target,cases) -> decision target cases final.ReturnType (Some emitReturn)
                    | _ -> $"return {coerce value.Type final.ReturnType (expression value)};"
            let body=emitReturn final.Body
            if tailLoop then $"while (true) {{ {body} }}" else body
        for declaration in program.Declarations do addMember $"private static {csType declaration.Type} {sym declaration.Id} = default!;"
        for fn in program.Functions do
            let captures = fn.Captures |> List.filter (fun id -> not (globalIds.Contains id)) |> List.map (fun id -> $"{csType symbolTypes.[id]} {sym id}")
            let parameters = String.concat comma (captures @ [$"{csType fn.ParameterType} {sym fn.Parameter}"])
            let body=within (fn.Parameter::fn.Captures) (fun () -> coerce fn.Body.Type fn.ReturnType (expression fn.Body))
            addMember $"private static {csType fn.ReturnType} {funName fn.Id}({parameters}) {{ return {body}; }}"
            if assemblyMode then
                let chain=functionChain Set.empty fn.Id
                let last=List.last chain
                let parameters=String.concat comma (captures @ (chain |> List.map (fun parameter -> $"{csType parameter.ParameterType} {sym parameter.Parameter}")))
                let body=within (fn.Captures @ (chain |> List.map _.Parameter)) (fun () -> nativeBody fn chain)
                addMember $"private static {csType last.ReturnType} Native{funName fn.Id}({parameters}) {{ {body} }}"
        let needsInitialization=not assemblyMode || not program.Declarations.IsEmpty
        let forceInitialization=if needsInitialization then "_ = Initialization.Value;" else ""
        if needsInitialization then
            addMember "private static readonly Lazy<Unit> Initialization = new(Initialize);"
            addMember "private static Unit Initialize() {"
            for declaration in program.Declarations do addMember $"{sym declaration.Id} = {coerce declaration.Value.Type declaration.Type (expression declaration.Value)};"
            addMember "return default; }"
        for declaration in program.Declarations |> List.filter _.IsExported do
            addMember $"public static {csType declaration.Type} {ident declaration.Name} {{ get {{ {forceInitialization} return {sym declaration.Id}; }} }}"
        let resultType = program.Entry |> Option.map _.Type |> Option.defaultValue TUnit
        let result = program.Entry |> Option.map expression |> Option.defaultValue "default(Unit)"
        addType resultType
        addMember $"public static {csType resultType} Run() {{ {forceInitialization} return {result}; }}"
        addMember $"public static string FormatResult({csType resultType} value) => {fmtName resultType}(value);"
        addMember "public static void Main(string[] args) { Console.WriteLine(FormatResult(Run())); }"
        if hasSdk then
            let sdk="global::FScript.Extensibility."
            let sv=sdk+"ScriptValue"
            let ctx=sdk+"ExternalCallContext"
            let fnType=sdk+"ExternalFunction"
            let listType= $"global::Microsoft.FSharp.Collections.FSharpList<{sv}>"
            let wrappers=CompilerExternCatalog.defaults |> List.filter (fun fn -> fn.Name<>"Env") |> List.map (fun fn ->
                let (Forall(variables,t))=fn.Scheme
                let variables=variables |> List.map string |> String.concat comma
                let scheme= $"{sdk}FunctionScheme.NewForall(global::Microsoft.FSharp.Collections.ListModule.OfArray(new int[] {{ {variables} }}),{sdkType t})"
                let invoke= $"global::Microsoft.FSharp.Core.FuncConvert.FromFunc<{ctx},global::Microsoft.FSharp.Core.FSharpFunc<{listType},{sv}>>(ctx => global::Microsoft.FSharp.Core.FuncConvert.FromFunc<{listType},{sv}>(args => SdkInterop.Default({quote fn.Name},ctx,args.ToArray())))"
                $"new {fnType}({quote fn.Name},{scheme},{invoke},{fn.Arity})") |> String.concat comma
            addMember $"private static global::Microsoft.FSharp.Collections.FSharpList<{fnType}> ProviderDefaults() => global::Microsoft.FSharp.Collections.ListModule.OfArray(new {fnType}[] {{ {wrappers} }});"
        for KeyValue(path,name) in sdkProviders do
            let link=ClrBindings.tryMethodLink ("clr:"+path.Substring(8)) |> Option.get
            let declaring=System.Type.GetType(link.DeclaringType,true) |> nonNull
            let sdk="global::FScript.Extensibility."
            let sdkExternalFunction=sdk+"ExternalFunction"
            let sdkHostType=sdk+"HostTypeDescriptor"
            let sdkHostUnion=sdk+"HostUnionDescriptor"
            let empty element = $"global::Microsoft.FSharp.Collections.ListModule.Empty<{element}>()"
            let arguments=if link.Parameters.Length=0 then "" else $"new {sdk}HostContext(Directory.GetCurrentDirectory(), {empty stringCode}, ProviderDefaults())"
            let call= $"{typeName declaring}.@{link.MemberName}({arguments})"
            let method=declaring.GetMethods(BindingFlags.Public ||| BindingFlags.Static) |> Array.find (fun m -> m.Name=link.MemberName && m.GetParameters().Length=link.Parameters.Length)
            let call=if method.ReturnType.FullName="FScript.Extensibility.ExtensionRegistration" then call else $"new {sdk}ExtensionRegistration({empty sdkHostType},{empty sdkHostUnion},{call})"
            addMember $"private static readonly Lazy<{sdk}ExtensionRegistration> {name} = new(() => {call});"
        let header = "#nullable enable\nusing System;\nusing System.IO;\nusing System.Linq;\nusing System.Globalization;\nusing System.Text.Json;\nusing System.Threading.Tasks;\n"
        let source = $"{header}\nnamespace {options.Namespace};\n{typeDeclarations}\npublic static class {options.ClassName} {{\n{helpers}\n{members}\n}}\n"
        use stream = typeof<CSharpOptions>.Assembly.GetManifestResourceStream("FScript.Backend.CSharp.Runtime.cs") |> nonNull
        use reader = new StreamReader(stream)
        let runtime = reader.ReadToEnd().Replace("__NAMESPACE__",options.Namespace)
        let escapeXml (value: string) = System.Security.SecurityElement.Escape(value) |> nonNull
        let references = options.References |> List.distinct |> List.map (fun path -> $"<Reference Include=\"{escapeXml (Path.GetFileNameWithoutExtension path |> nonNull)}\"><HintPath>{escapeXml (Path.GetFullPath path)}</HintPath><Private>true</Private></Reference>") |> String.concat "\n"
        let compileItems = "<Compile Include=\"Program.cs\"/><Compile Include=\"Runtime.cs\"/>" + (if hasSdk then "<Compile Include=\"RuntimeSDK.cs\"/>" else "")
        let project = $"<Project Sdk=\"Microsoft.NET.Sdk\"><PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net10.0</TargetFramework><Nullable>enable</Nullable><TreatWarningsAsErrors>true</TreatWarningsAsErrors><EnableDefaultCompileItems>false</EnableDefaultCompileItems></PropertyGroup><ItemGroup>{compileItems}{references}</ItemGroup></Project>\n"
        [ yield { RelativePath = "Program.cs"; Content = source }; yield { RelativePath = "Runtime.cs"; Content = runtime }; yield { RelativePath = "Program.csproj"; Content = project }
          if hasSdk then
              use sdkStream=typeof<CSharpOptions>.Assembly.GetManifestResourceStream("FScript.Backend.CSharp.RuntimeSDK.cs") |> nonNull
              use sdkReader=new StreamReader(sdkStream)
              yield {RelativePath="RuntimeSDK.cs"; Content=sdkReader.ReadToEnd().Replace("__NAMESPACE__",options.Namespace)} ]

    let private specializeAssembly (program: Lir.Program) =
        let exports=program.Declarations |> List.filter _.IsExported
        match exports with
        | [] -> LirMonomorphization.specialize program
        | first::_ ->
            for declaration in exports do
                if NativeSpecialization.containsVariable declaration.Type then
                    fail "FSC6009" $"Export '{declaration.Name}' requires a concrete native type." (Some declaration.Span)
            // Analysis roots only: discard the synthetic tuple immediately after specialization.
            let entry=program.Entry |> Option.defaultValue {Type=TUnit;Span=first.Span;Node=Lir.Constant Lir.Unit}
            let roots=entry :: (exports |> List.map (fun declaration -> {Type=declaration.Type;Span=declaration.Span;Node=Lir.Load declaration.Id}))
            let rooted={program with Entry=Some {Type=TTuple(List.map (fun (root: Lir.Expression) -> root.Type) roots);Span=entry.Span;Node=Lir.Tuple roots}}
            LirMonomorphization.specialize rooted |> Result.map (fun specialized ->
                let entry=match specialized.Entry with Some {Node=Lir.Tuple(first::_)} when program.Entry.IsSome -> Some first | _ -> None
                {specialized with Entry=entry})

    let private generateWith assemblyMode optimize options (program: Lir.Program) =
        try
            for path in options.References do
                if not (File.Exists path) then fail "FSC6003" $"Reference assembly does not exist: {path}" None
            let options={options with References=ClrBindings.referenceClosure options.References}
            let validIdentifier (value: string) = not (String.IsNullOrWhiteSpace value) && SyntaxFacts.IsValidIdentifier(value) && SyntaxFacts.GetKeywordKind(value)=SyntaxKind.None
            if not (validIdentifier options.ClassName && options.Namespace.Split('.') |> Array.forall validIdentifier) then fail "FSC6001" "Namespace and class names must be valid C# identifiers." None
            for path in options.References do
                if not (File.Exists path) then fail "FSC6003" $"Reference assembly does not exist: {path}" None
                Reflection.Assembly.LoadFrom(Path.GetFullPath path) |> ignore
            program |> LirValidation.validate
            |> Result.bind (if assemblyMode then specializeAssembly else LirMonomorphization.specialize)
            |> Result.map LirSimplification.optimize
            |> Result.map optimize
            |> Result.bind LirValidation.validate
            |> Result.map (emit assemblyMode options)
            |> Result.map (List.map (fun file ->
                if not (file.RelativePath.EndsWith(".cs",StringComparison.Ordinal)) then file else
                    let tree=CSharpSyntaxTree.ParseText(file.Content,CSharpParseOptions(LanguageVersion.Latest))
                    let errors=tree.GetDiagnostics() |> Seq.filter (fun diagnostic -> diagnostic.Severity=DiagnosticSeverity.Error) |> Seq.map string |> Seq.toList
                    if not errors.IsEmpty then fail "FSC6008" (String.concat "\n" errors) None
                    {file with Content=tree.GetRoot().NormalizeWhitespace("    ","\n",false).ToFullString()+"\n"}))
            |> Result.mapError (List.map (fun d -> if d.Stage=GoGeneration then { d with Stage=CSharpGeneration; Code=d.Code.Replace("FSB4","FSC6"); Message=d.Message.Replace("Go specialization","CLR specialization") } else d))
        with
        | Failure diagnostic -> Error [diagnostic]
        | ex -> Error [{ Code="FSC6000"; Stage=CSharpGeneration; Message=ex.Message; Span=None }]

    let generate options program = generateWith false id options program

    let internal generateForAssembly optimize options program = generateWith true optimize options program
