namespace FScript.Compiler

open System
open System.Reflection
open System.Text.Json
open System.IO
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open FScript.Language

/// Declarative CLR linkage, independent of executable interpreter registrations.
[<CLIMutable>]
type ClrMethodLink =
    { DeclaringType: string
      MemberName: string
      Parameters: string array
      IsStatic: bool
      IsConstructor: bool
      Inline: bool }

module ClrBindings =
    /// Keep only explicitly supplied dependencies referenced by the emitted PE and their
    /// adjacent managed dependency closure. Merely loading a provider is not a runtime root.
    let referencedDependencies (assemblyBytes: byte array) paths =
        let candidates=Collections.Generic.Dictionary<string,string>(StringComparer.OrdinalIgnoreCase)
        for path in paths do
            let path=Path.GetFullPath path
            use stream=File.OpenRead(path)
            use pe=new PEReader(stream)
            if pe.HasMetadata then
                let metadata=pe.GetMetadataReader()
                if metadata.IsAssembly then candidates.[metadata.GetString(metadata.GetAssemblyDefinition().Name)] <- path
        let used=Collections.Generic.HashSet<string>(StringComparer.OrdinalIgnoreCase)
        let rec visitBytes bytes =
            use stream=new MemoryStream(bytes,false)
            use pe=new PEReader(stream)
            if pe.HasMetadata then
                let metadata=pe.GetMetadataReader()
                for handle in metadata.AssemblyReferences do
                    let name=metadata.GetString(metadata.GetAssemblyReference(handle).Name)
                    match candidates.TryGetValue name with
                    | true,path when used.Add path -> visitBytes (File.ReadAllBytes path)
                    | _ -> ()
        visitBytes assemblyBytes
        used |> Seq.sort |> Seq.toList

    /// Include managed dependencies colocated with explicit reference assemblies.
    let referenceClosure paths =
        let seen=Collections.Generic.HashSet<string>(StringComparer.Ordinal)
        let result=ResizeArray<string>()
        let rec visit path =
            let path=Path.GetFullPath path
            if File.Exists path && seen.Add path then
                result.Add path
                use stream=File.OpenRead path
                use pe=new PEReader(stream)
                if pe.HasMetadata then
                    let metadata=pe.GetMetadataReader()
                    for handle in metadata.AssemblyReferences do
                        let name=metadata.GetString(metadata.GetAssemblyReference(handle).Name)
                        let adjacent=Path.Combine(Path.GetDirectoryName(path) |> nonNull,name+".dll")
                        if File.Exists adjacent then visit adjacent
        List.iter visit paths
        result |> Seq.distinctBy Path.GetFileName |> Seq.sort |> Seq.toList

    let private typeName (value: System.Type) =
        match value.AssemblyQualifiedName with null -> invalidArg "value" "CLR types must have assembly-qualified names." | name -> name

    let methodLink (method: MethodBase) =
        let declaring = method.DeclaringType |> nonNull
        if not method.IsPublic || method.ContainsGenericParameters || not declaring.IsVisible then
            invalidArg "method" "Compiled FFI requires a public, closed method on a public CLR type."
        if method.GetParameters() |> Array.exists (fun parameter -> parameter.ParameterType.IsByRef || parameter.ParameterType.IsPointer) then
            invalidArg "method" "Compiled FFI does not accept by-reference or pointer parameters."
        "clr:" + JsonSerializer.Serialize<ClrMethodLink>(
            { DeclaringType = typeName declaring
              MemberName = method.Name
              Parameters = method.GetParameters() |> Array.map (fun parameter -> typeName parameter.ParameterType)
              IsStatic = method.IsStatic
              IsConstructor = method.IsConstructor; Inline = false })

    let tryMethodLink (path: string) : ClrMethodLink option =
        if path.StartsWith("clr:{", StringComparison.Ordinal) then
            try JsonSerializer.Deserialize<ClrMethodLink>(path.Substring(4)) |> Option.ofObj with :? JsonException -> None
        else None

    /// Stable temporary assembly-only call-site marker, removed by IL expansion.
    let inlineMarkerName (path:string) =
        "FScriptInline_" + (Security.Cryptography.SHA256.HashData(Text.Encoding.UTF8.GetBytes path) |> Convert.ToHexString).Substring(0,16)

    /// Bind an existing host-external declaration to an exact CLR overload.
    /// Instance methods take their receiver as the first curried FScript argument.
    let bindMethod name (method: MethodBase) (program: Lir.Program) =
        let link = methodLink method
        let mutable found = false
        let declarations = program.Declarations |> List.map (fun declaration ->
            if declaration.Name = name then
                found <- true
                { declaration with Value = { declaration.Value with Node = Lir.TargetImport(link, method.Name) } }
            else declaration)
        if not found then invalidArg "name" $"No reachable external declaration named '{name}' exists."
        { program with Declarations = declarations }

    /// Link existing SDK providers without retaining compiler-process delegates.
    /// Generated adapters invoke SDK implementations directly and marshal compiled callbacks.
    let bindProvider names (method: MethodInfo) (program: Lir.Program) =
        if not method.IsStatic then invalidArg "method" "SDK providers must be static methods."
        let link = methodLink method
        let target = "clr-sdk:" + link.Substring(4)
        let names = Set.ofList names
        let declarations =
            program.Declarations |> List.map (fun declaration ->
                if Set.contains declaration.Name names then
                    { declaration with Value = { declaration.Value with Node = Lir.TargetImport(target,declaration.Name) } }
                else declaration)
        { program with Declarations = declarations }

    /// Explicit permission to expand this exact typed FFI call in the .NET backend.
    /// Independent of purity; legacy SDK adapters are not eligible.
    let bindInlineMethod name (method: MethodInfo) program =
        let linked = bindMethod name method program
        let declarations = linked.Declarations |> List.map (fun declaration ->
            if declaration.Name <> name then declaration
            else
                let link = tryMethodLink (methodLink method) |> Option.get
                { declaration with Value = { declaration.Value with Node = Lir.TargetImport("clr:" + JsonSerializer.Serialize({ link with Inline = true }), method.Name) } })
        { linked with Declarations = declarations }

    /// Retain a native type projection in the ordinary serialized target-type linkage.
    let bindType name (clrType: System.Type) (program: Lir.Program) =
        if not clrType.IsVisible || clrType.ContainsGenericParameters || clrType.IsByRef || clrType.IsPointer then
            invalidArg "clrType" "Compiled FFI requires a public, closed CLR type."
        let definition = program.Types |> List.tryFind (fun declaration -> declaration.Name = name)
        match definition with
        | None -> invalidArg "name" $"No external type named '{name}' exists."
        | Some definition ->
            let fields = match definition.Definition with TRecord fields | THostManaged(_, fields) -> Map.toList fields | _ -> []
            let imported: Lir.ImportedRecord =
                { Name = name; ModulePath = "clr:" + typeName clrType; MemberName = clrType.FullName |> nonNull
                  IsUnion = match definition.Definition with TUnion _ -> true | _ -> false
                  Fields = fields; Span = definition.Span }
            { program with
                Types = program.Types |> List.map (fun item -> if item.Name = name then { item with Import = Some imported } else item)
                ImportedRecords = imported :: (program.ImportedRecords |> List.filter (fun item -> item.Name <> name)) }

    /// Remove unused declared/imported types before resolving target-specific metadata.
    let pruneUnusedTypes (program: Lir.Program) =
        let definitions=program.Types |> List.map (fun declaration -> declaration.Name,declaration.Definition) |> Map.ofList
        let names=Collections.Generic.HashSet<string>(StringComparer.Ordinal)
        let rec named name =
            if names.Add name then definitions.TryFind name |> Option.iter visitType
        and visitType = function
            | TNamed name -> named name
            | THostManaged(name,fields) -> named name; fields |> Map.iter (fun _ t -> visitType t)
            | TUnion(name,cases) -> named name; cases |> Map.iter (fun _ t -> Option.iter visitType t)
            | TRecord fields | TRecordShape(_,fields,_) -> fields |> Map.iter (fun _ t -> visitType t)
            | TFun(a,b) | TMap(a,b) -> visitType a; visitType b
            | TTuple items -> List.iter visitType items
            | TList t | TOption t | TTask t -> visitType t
            | _ -> ()
        let rec pattern (plan: Lir.PatternPlan) =
            for binding in plan.Bindings do visitType binding.Type
            for check in plan.Checks do match check with Lir.TypeCheck(_,t) -> visitType t | Lir.UnionCaseCheck(_,name,_,_) -> named name | _ -> ()
            for map in plan.Maps do
                for clause in map.Clauses do pattern clause.Key; pattern clause.Value
                Option.iter pattern map.Tail
        let rec expression (value: Lir.Expression) =
            visitType value.Type
            let both a b = expression a; expression b
            match value.Node with
            | Lir.CallClosure(a,b) | Lir.TailCallClosure(a,b) | Lir.PipeClosure(a,b) | Lir.TailPipeClosure(a,b)
            | Lir.Cons(a,b) | Lir.Append(a,b) | Lir.Range(a,b) | Lir.IndexGet(a,b) -> both a b
            | Lir.Branch(a,b,c) -> expression a; both b c
            | Lir.Operation(_,items) | Lir.Tuple items | Lir.List items -> List.iter expression items
            | Lir.Record fields -> fields |> List.iter (snd >> expression)
            | Lir.FieldGet(v,_) | Lir.SomeValue v | Lir.Fail v -> expression v
            | Lir.Let(_,_,_,binding,body) -> both binding body
            | Lir.LetRecursiveGroup(bindings,body) ->
                for _,_,t,binding in bindings do visitType t; expression binding
                expression body
            | Lir.DecisionTree(target,cases) -> expression target; for case in cases do pattern case.Plan; Option.iter expression case.Guard; expression case.Body
            | Lir.RecordUpdate(target,fields) -> expression target; fields |> List.iter (snd >> expression)
            | Lir.RecordConvert(target,t) -> expression target; visitType t
            | Lir.Interpolate parts -> for part in parts do match part with Lir.ValuePart value -> expression value | _ -> ()
            | Lir.UnionCase(name,_,payload) -> named name; Option.iter expression payload
            | Lir.Map entries -> for entry in entries do match entry with Lir.KeyValue(a,b) -> both a b | Lir.Spread value -> expression value
            | Lir.ForEach(_,_,t,source,body) -> visitType t; both source body
            | Lir.TypeToken t -> visitType t
            | _ -> ()
        for declaration in program.Declarations do visitType declaration.Type; expression declaration.Value
        for fn in program.Functions do visitType fn.ParameterType; visitType fn.ReturnType; expression fn.Body
        Option.iter expression program.Entry
        {program with Types=program.Types |> List.filter (fun declaration -> names.Contains declaration.Name)
                      ImportedRecords=program.ImportedRecords |> List.filter (fun record -> names.Contains record.Name)}

    /// Additive provider metadata; existing SDK registration records remain unchanged.
    let providerPurity (providers: MethodInfo list) : PurityContracts =
        let mutable functions = Set.empty
        let mutable types = Set.empty
        for typ in providers |> List.map (fun method -> method.DeclaringType |> nonNull) |> List.distinct do
            let names methodName =
                match typ.GetMethod(methodName, BindingFlags.Public ||| BindingFlags.Static, null, System.Type.EmptyTypes, null) |> Option.ofObj with
                | None -> Set.empty
                | Some factory ->
                    match factory.Invoke(null, [||]) with
                    | :? Collections.Generic.IEnumerable<string> as values ->
                        values |> Seq.map (fun value -> if String.IsNullOrWhiteSpace value then invalidOp $"{typ.FullName}.{methodName} contains an empty binding name." else value) |> Set.ofSeq
                    | _ -> invalidOp $"{typ.FullName}.{methodName} must return IEnumerable<string>."
            functions <- Set.union functions (names "FScriptPureFunctions")
            types <- Set.union types (names "FScriptPureTypes")
        { PureFunctions = functions; PureTypes = types }

    /// Optional compiler-only companion contract. Uses BCL metadata types, so existing
    /// SDK providers and their interpreter registration records remain binary-compatible.
    let bindCompiledProviders (providers: MethodInfo list) (program: Lir.Program) =
        let mutable program=pruneUnusedTypes program
        let types=providers |> List.map (fun method -> method.DeclaringType |> nonNull) |> List.distinct
        for typ in types do
            let companion name = typ.GetMethod(name,BindingFlags.Public ||| BindingFlags.Static,null,System.Type.EmptyTypes,null) |> Option.ofObj
            match companion "FScriptCompiledTypes" with
            | Some factory ->
                match factory.Invoke(null,[||]) with
                | :? Collections.Generic.IEnumerable<Collections.Generic.KeyValuePair<string,System.Type>> as bindings ->
                    for binding in bindings do
                        if program.Types |> List.exists (fun declaration -> declaration.Name=binding.Key) then program <- bindType binding.Key binding.Value program
                | _ -> invalidOp $"{typ.FullName}.FScriptCompiledTypes must return IEnumerable<KeyValuePair<string, Type>>."
            | None -> ()
            let inlineNames =
                match companion "FScriptInlineFunctions" with
                | None -> Set.empty
                | Some factory ->
                    match factory.Invoke(null,[||]) with
                    | :? Collections.Generic.IEnumerable<string> as names ->
                        names |> Seq.map (fun name -> if String.IsNullOrWhiteSpace name then invalidOp "FScriptInlineFunctions contains an empty name." else name) |> Set.ofSeq
                    | _ -> invalidOp "FScriptInlineFunctions must return IEnumerable<string>."
            let bindings =
                match companion "FScriptCompiledFunctions" with
                | None -> []
                | Some factory ->
                    match factory.Invoke(null,[||]) with
                    | :? Collections.Generic.IEnumerable<Collections.Generic.KeyValuePair<string,MethodInfo>> as bindings -> Seq.toList bindings
                    | _ -> invalidOp $"{typ.FullName}.FScriptCompiledFunctions must return IEnumerable<KeyValuePair<string, MethodInfo>>."
            let unknown = Set.difference inlineNames (bindings |> List.map _.Key |> Set.ofList)
            if not unknown.IsEmpty then invalidOp ("Inline requires an exact FScriptCompiledFunctions binding: " + String.concat ", " unknown)
            for binding in bindings do
                if program.Declarations |> List.exists (fun declaration -> declaration.Name = binding.Key) then
                    program <- if inlineNames.Contains binding.Key then bindInlineMethod binding.Key binding.Value program else bindMethod binding.Key binding.Value program
        program
