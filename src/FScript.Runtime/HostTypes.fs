namespace FScript.Runtime

open System
open System.Collections
open System.Reflection
open Microsoft.FSharp.Reflection
open FScript.Language

/// Expose a CLR type under a script-visible name; opaque values are the default.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct, AllowMultiple = false, Inherited = false)>]
type FScriptHostTypeAttribute(name: string) =
    inherit Attribute()
    member _.Name = name
    /// Allow scripts to construct ordinary record values instead of opaque host values.
    member val ScriptConstructible = false with get, set

/// Expose a public readable instance property as an immutable snapshot field.
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false, Inherited = true)>]
type FScriptVisibleAttribute() =
    inherit Attribute()

/// Optional CLR reflection adapters over the existing host registration/value bridge.
module HostTypes =
    let private fail path message = raise (HostCommon.evalError $"Host type projection '{path}': {message}")

    let private hostName (path: string) (clrType: System.Type) =
        let attribute = clrType.GetCustomAttribute<FScriptHostTypeAttribute>(false)
        if isNull (box attribute) then
            fail path $"CLR type '{clrType.FullName}' requires [<FScriptHostType>]."
        let name = attribute.Name
        let validIdentifier (part: string) =
            part.Length > 0 && (Char.IsLetter part[0] || part[0] = '_')
            && (part |> Seq.forall (fun c -> Char.IsLetterOrDigit c || c = '_'))
        if String.IsNullOrWhiteSpace name || not (name.Split('.') |> Array.forall validIdentifier) then
            fail path "The script type name must be a nonempty qualified identifier."
        if clrType.ContainsGenericParameters then fail path "Open generic CLR types are not supported."
        name

    let private visibleProperties path (clrType: System.Type) =
        let properties =
            clrType.GetProperties(BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
            |> Array.filter (fun property -> property.IsDefined(typeof<FScriptVisibleAttribute>, true))
        for property in properties do
            let getter = property.GetGetMethod(true)
            if isNull getter || not getter.IsPublic || getter.IsStatic || property.GetIndexParameters().Length <> 0 then
                fail $"{path}.{property.Name}" "Visible properties must have a public instance getter and cannot be indexers."
        properties
        |> Array.groupBy (fun property -> property.Name)
        |> Array.iter (fun (name, members) -> if members.Length > 1 then fail $"{path}.{name}" "Duplicate visible property name.")
        properties |> Array.sortBy (fun property -> property.Name)

    let private isGeneric definition (clrType: System.Type) =
        clrType.IsGenericType && clrType.GetGenericTypeDefinition() = definition

    let rec private fieldType stack path (clrType: System.Type) : FScript.Language.Type =
        if clrType = typeof<int> || clrType = typeof<int64> then TInt
        elif clrType = typeof<float> then TFloat
        elif clrType = typeof<bool> then TBool
        elif clrType = typeof<string> then TString
        elif clrType = typeof<unit> then TUnit
        elif clrType.IsArray && clrType.GetArrayRank() = 1 then TList(fieldType stack path (clrType.GetElementType() |> nonNull))
        elif isGeneric typedefof<list<_>> clrType then TList(fieldType stack path (clrType.GetGenericArguments()[0]))
        elif isGeneric typedefof<option<_>> clrType then TOption(fieldType stack path (clrType.GetGenericArguments()[0]))
        elif FSharpType.IsTuple clrType then TTuple(FSharpType.GetTupleElements clrType |> Array.map (fieldType stack path) |> Array.toList)
        elif isGeneric typedefof<Map<_, _>> clrType && clrType.GetGenericArguments()[0] = typeof<string> then
            TMap(TString, fieldType stack path (clrType.GetGenericArguments()[1]))
        elif clrType.IsDefined(typeof<FScriptHostTypeAttribute>, false) then
            let descriptor = describeType stack path clrType
            TNamed descriptor.Name
        else fail path $"Unsupported visible CLR type '{clrType.FullName}'."

    and private describeType stack path (clrType: System.Type) : HostTypeDescriptor =
        let name = hostName path clrType
        if List.contains clrType stack then fail path "Recursive visible type graphs are not supported. Keep cyclic references hidden."
        let properties = visibleProperties path clrType
        let attribute = clrType.GetCustomAttribute<FScriptHostTypeAttribute>(false)
        if attribute.ScriptConstructible then
            if not (FSharpType.IsRecord clrType) then
                fail path "ScriptConstructible currently requires an F# record with every field visible."
            let recordFields = FSharpType.GetRecordFields clrType
            let hidden = recordFields |> Array.filter (fun field -> not (properties |> Array.exists (fun property -> property.Name = field.Name)))
            if hidden.Length > 0 then
                let names = hidden |> Array.map (fun field -> field.Name) |> String.concat ", "
                fail path $"ScriptConstructible records must expose every constructor field with [<FScriptVisible>]. Hidden fields: {names}."
            if recordFields.Length <> properties.Length then
                fail path "ScriptConstructible records cannot expose computed properties outside their constructor fields."
        let fields =
            properties
            |> Array.map (fun property -> property.Name, fieldType (clrType :: stack) $"{path}.{property.Name}" property.PropertyType)
            |> Map.ofArray
        { Name = name; Fields = fields; HostManaged = not attribute.ScriptConstructible }

    let private projection (clrType: System.Type) =
        let root = describeType [] clrType.FullName clrType
        let rec collect path (current: System.Type) = seq {
            if current.IsDefined(typeof<FScriptHostTypeAttribute>, false) then
                yield current, describeType [] path current
                for property in visibleProperties path current do
                    yield! collect $"{path}.{property.Name}" property.PropertyType
            elif current.IsArray then yield! collect path (current.GetElementType() |> nonNull)
            elif FSharpType.IsTuple current then
                for item in FSharpType.GetTupleElements current do yield! collect path item
            elif current.IsGenericType then
                for item in current.GetGenericArguments() do yield! collect path item
        }
        let descriptors = collect clrType.FullName clrType |> Seq.distinctBy fst |> Seq.toList
        descriptors |> List.groupBy (fun (_, d) -> d.Name) |> List.iter (fun (name, types) ->
            if types.Length > 1 then fail clrType.FullName $"Multiple CLR types project to the script type name '{name}'.")
        root, descriptors |> List.map snd

    /// Derive a descriptor without constructing a value or invoking any property getter.
    let describe<'T> () = projection typeof<'T> |> fst

    let private ensureRegistered (registration: ExtensionRegistration) (descriptor: HostTypeDescriptor) path =
        HostInterop.typeDefinitions registration [] |> ignore
        match registration.Types |> List.tryFind (fun registered -> registered.Name = descriptor.Name) with
        | Some registered when registered = descriptor -> ()
        | _ -> fail path $"Registration must contain the matching descriptor '{descriptor.Name}'."

    let private readProperty path (property: PropertyInfo) (value: obj) =
        try property.GetValue(value)
        with ex -> fail path $"Property getter failed: {ex.GetBaseException().Message}"

    let rec private snapshot (registration: ExtensionRegistration) path (clrType: System.Type) (value: objnull) : Value =
        if clrType = typeof<unit> then VUnit
        elif isGeneric typedefof<option<_>> clrType then
            if isNull value then VOption None
            else
                let case, fields = FSharpValue.GetUnionFields(value, clrType)
                if case.Name = "None" then VOption None
                else VOption(Some(snapshot registration path (clrType.GetGenericArguments()[0]) fields[0]))
        elif isNull value then fail path "Null is not a script value; use an F# option for absence."
        else
            let value = nonNull value
            if clrType = typeof<int> then VInt(int64 (unbox<int> value))
            elif clrType = typeof<int64> then VInt(unbox<int64> value)
            elif clrType = typeof<float> then VFloat(unbox<float> value)
            elif clrType = typeof<bool> then VBool(unbox<bool> value)
            elif clrType = typeof<string> then VString(unbox<string> value)
            elif clrType.IsArray || isGeneric typedefof<list<_>> clrType then
                let elementType = if clrType.IsArray then clrType.GetElementType() |> nonNull else clrType.GetGenericArguments()[0]
                (value :?> IEnumerable) |> Seq.cast<objnull>
                |> Seq.mapi (fun i item -> snapshot registration $"{path}[{i}]" elementType item)
                |> Seq.toList |> VList
            elif FSharpType.IsTuple clrType then
                Array.zip (FSharpType.GetTupleElements clrType) (FSharpValue.GetTupleFields value |> Array.map (fun item -> item : objnull))
                |> Array.mapi (fun i (itemType, item) -> snapshot registration $"{path}[{i}]" itemType item)
                |> Array.toList |> VTuple
            elif isGeneric typedefof<Map<_, _>> clrType then
                let valueType = clrType.GetGenericArguments()[1]
                (value :?> IEnumerable) |> Seq.cast<obj>
                |> Seq.map (fun item ->
                    let key = item.GetType().GetProperty("Key") |> nonNull
                    let data = item.GetType().GetProperty("Value") |> nonNull
                    let keyValue = readProperty path key item
                    if isNull keyValue then fail path "Map keys cannot be null."
                    let keyText = unbox<string> keyValue
                    MKString keyText, snapshot registration $"{path}[{keyText}]" valueType (readProperty path data item))
                |> Map.ofSeq |> VMap
            else wrapObject registration path clrType value
    and private wrapObject registration path clrType (value: objnull) =
        let descriptor = describeType [] path clrType
        ensureRegistered registration descriptor path
        if isNull value then fail path "Cannot wrap a null host object."
        let payload = nonNull value
        let fields =
            visibleProperties path clrType
            |> Array.map (fun property ->
                let propertyPath = $"{path}.{property.Name}"
                property.Name, snapshot registration propertyPath property.PropertyType (readProperty propertyPath property payload))
            |> Map.ofArray
        if descriptor.HostManaged then HostInterop.create registration descriptor.Name fields payload
        else
            let value = VRecord fields
            let definitions = HostInterop.typeDefinitions registration []
            HostInterop.validate definitions (TNamed descriptor.Name) value
            value

    /// Convert visible properties to a snapshot; opaque projections also retain the CLR payload.
    let wrap<'T> registration (value: 'T) =
        let _, descriptors = projection typeof<'T>
        for descriptor in descriptors do ensureRegistered registration descriptor typeof<'T>.FullName
        wrapObject registration typeof<'T>.FullName typeof<'T> (box value)

    let rec private restore path (clrType: System.Type) value : objnull =
        let mismatch () = fail path $"Script value cannot be converted to CLR type '{clrType.FullName}'."
        if clrType = typeof<unit> then
            match value with VUnit -> null | _ -> mismatch ()
        elif clrType = typeof<int> then
            match value with
            | VInt number when number >= int64 Int32.MinValue && number <= int64 Int32.MaxValue -> box (int number)
            | VInt _ -> fail path "Script int is outside the CLR Int32 range."
            | _ -> mismatch ()
        elif clrType = typeof<int64> then
            match value with VInt number -> box number | _ -> mismatch ()
        elif clrType = typeof<float> then
            match value with VFloat number -> box number | _ -> mismatch ()
        elif clrType = typeof<bool> then
            match value with VBool boolean -> box boolean | _ -> mismatch ()
        elif clrType = typeof<string> then
            match value with VString text when not (isNull (box text)) -> box text | _ -> mismatch ()
        elif isGeneric typedefof<option<_>> clrType then
            let cases = FSharpType.GetUnionCases clrType
            match value with
            | VOption None -> FSharpValue.MakeUnion(cases |> Array.find (fun case -> case.Name = "None"), [||])
            | VOption(Some item) ->
                FSharpValue.MakeUnion(cases |> Array.find (fun case -> case.Name = "Some"), [|restore path (clrType.GetGenericArguments()[0]) item|])
            | _ -> mismatch ()
        elif isGeneric typedefof<list<_>> clrType then
            match value with
            | VList items ->
                let cases = FSharpType.GetUnionCases clrType
                let empty = cases |> Array.find (fun case -> case.GetFields().Length = 0)
                let cons = cases |> Array.find (fun case -> case.GetFields().Length = 2)
                let values = items |> List.mapi (fun i item -> restore $"{path}[{i}]" (clrType.GetGenericArguments()[0]) item)
                List.foldBack (fun item tail -> FSharpValue.MakeUnion(cons, [|item; tail|])) values (FSharpValue.MakeUnion(empty, [||]))
            | _ -> mismatch ()
        elif clrType.IsArray then
            match value with
            | VList items ->
                let elementType = clrType.GetElementType() |> nonNull
                let result = Array.CreateInstance(elementType, items.Length)
                items |> List.iteri (fun i item -> result.SetValue(restore $"{path}[{i}]" elementType item, i))
                box result
            | _ -> mismatch ()
        elif FSharpType.IsTuple clrType then
            match value with
            | VTuple items when items.Length = (FSharpType.GetTupleElements clrType).Length ->
                let fields = Array.zip (FSharpType.GetTupleElements clrType) (List.toArray items)
                             |> Array.mapi (fun i (itemType, item) -> restore $"{path}[{i}]" itemType item)
                FSharpValue.MakeTuple(fields, clrType)
            | _ -> mismatch ()
        elif isGeneric typedefof<Map<_, _>> clrType then
            match value with
            | VMap entries ->
                let valueType = clrType.GetGenericArguments()[1]
                let pairType = FSharpType.MakeTupleType [|typeof<string>; valueType|]
                let pairs = Array.CreateInstance(pairType, entries.Count)
                entries |> Map.toList |> List.iteri (fun i (key, item) ->
                    match key with
                    | MKString text when not (isNull (box text)) -> pairs.SetValue(FSharpValue.MakeTuple([|box text; restore $"{path}[{text}]" valueType item|], pairType), i)
                    | _ -> mismatch () |> ignore)
                let sequenceType = typedefof<seq<_>>.MakeGenericType(pairType)
                let constructor = clrType.GetConstructor([|sequenceType|]) |> nonNull
                constructor.Invoke([|pairs :> obj|])
            | _ -> mismatch ()
        else
            let descriptor = describeType [] path clrType
            if descriptor.HostManaged then
                let host = HostInterop.unwrap descriptor.Name value
                if host.Descriptor <> descriptor then fail path "Host descriptor does not match the CLR projection."
                if not (clrType.IsInstanceOfType host.Payload) then fail path "Opaque payload does not have the required CLR type."
                host.Payload
            else
                match value with
                | VRecord fields when fields.Count = descriptor.Fields.Count ->
                    let arguments =
                        FSharpType.GetRecordFields clrType
                        |> Array.map (fun property ->
                            match fields |> Map.tryFind property.Name with
                            | Some field -> restore $"{path}.{property.Name}" property.PropertyType field
                            | None -> fail path $"Missing field '{property.Name}'.")
                    FSharpValue.MakeRecord(clrType, arguments)
                | _ -> mismatch ()

    /// Retrieve opaque payloads or construct CLR records from script record values.
    let unwrap<'T> value : 'T =
        let descriptor, descriptors = projection typeof<'T>
        let typeDefs =
            descriptors |> List.map (fun d ->
                d.Name, if d.HostManaged then THostManaged(d.Name, d.Fields) else TRecord d.Fields) |> Map.ofList
        if descriptor.HostManaged then
            let host = HostInterop.unwrap descriptor.Name value
            if host.Descriptor <> descriptor then fail typeof<'T>.FullName "Host descriptor does not match the CLR projection."
        HostInterop.validate typeDefs (TNamed descriptor.Name) value
        restore typeof<'T>.FullName typeof<'T> value |> unbox<'T>
