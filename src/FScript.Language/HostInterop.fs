namespace FScript.Language

/// Explicit host registration and opaque value transport. No reflection or CLR conversion is performed.
module HostInterop =
    let private span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
    let private fail message = raise (EvalException { Message = message; Span = span })

    let empty : ExtensionRegistration = { Types = []; Unions = []; Functions = [] }
    let ofExterns functions = { empty with Functions = functions }

    let typeDefinitions (registration: ExtensionRegistration) (scriptNames: string list) =
        let reserved = [ "unit"; "int"; "float"; "bool"; "string"; "numeric"; "type" ] @ (BuiltinTypes.typeDefs |> List.map (fun d -> d.Name)) @ scriptNames
        let mutable definitions = Map.empty
        for descriptor in registration.Types do
            if System.String.IsNullOrWhiteSpace descriptor.Name then
                fail "Host type name must not be empty"
            if List.contains descriptor.Name reserved || Map.containsKey descriptor.Name definitions then
                fail $"Host type name conflict: '{descriptor.Name}'"
            definitions <- definitions.Add(descriptor.Name,
                if descriptor.HostManaged then THostManaged(descriptor.Name, descriptor.Fields)
                else TRecord descriptor.Fields)
        for descriptor in registration.Unions do
            if System.String.IsNullOrWhiteSpace descriptor.Name || descriptor.Cases.IsEmpty then
                fail "Host unions require a name and at least one case"
            if List.contains descriptor.Name reserved || Map.containsKey descriptor.Name definitions then
                fail $"Host type name conflict: '{descriptor.Name}'"
            if descriptor.Cases |> Map.exists (fun name _ -> System.String.IsNullOrWhiteSpace name) then
                fail $"Host union '{descriptor.Name}' has an empty case name"
            definitions <- definitions.Add(descriptor.Name, TUnion(descriptor.Name, descriptor.Cases))
        definitions

    let rec matches (definitions: Map<string, Type>) (expected: Type) (value: Value) =
        let fieldsMatch fields values =
            fields |> Map.forall (fun name fieldType ->
                values |> Map.tryFind name |> Option.exists (matches definitions fieldType))
        match expected, value with
        | TNamed name, _ -> definitions.TryFind name |> Option.exists (fun t -> matches definitions t value)
        | THostManaged (name, fields), VHostManaged host ->
            host.Descriptor.HostManaged && host.Descriptor.Name = name && host.Descriptor.Fields = fields
            && host.Fields.Count = fields.Count && fieldsMatch fields host.Fields
        | TRecord fields, VRecord values -> fieldsMatch fields values
        | TRecordShape (_, fields, constructible), VHostManaged host -> not constructible && fieldsMatch fields host.Fields
        | TRecordShape (_, fields, _), VRecord values -> fieldsMatch fields values
        | TUnit, VUnit | TInt, VInt _ | TFloat, VFloat _ | TBool, VBool _ | TString, VString _ -> true
        | TList t, VList values -> List.forall (matches definitions t) values
        | TTuple ts, VTuple values -> ts.Length = values.Length && List.forall2 (matches definitions) ts values
        | TOption _, VOption None -> true
        | TOption t, VOption (Some v) -> matches definitions t v
        | TMap (_, t), VMap values -> values |> Map.forall (fun _ v -> matches definitions t v)
        | TUnion (name, cases), VUnionCase (actualName, caseName, payload) ->
            name = actualName &&
            match cases.TryFind caseName, payload with
            | Some None, None -> true
            | Some (Some t), Some v -> matches definitions t v
            | _ -> false
        | TTask _, VTask _ | TTypeToken, VTypeToken _ -> true
        | TFun _, (VClosure _ | VExternal _ | VUnionCtor _) -> true
        | TVar _, _ -> true
        | TNumeric _, (VInt _ | VFloat _) -> true
        | _ -> false

    let validate definitions expected value =
        if not (matches definitions expected value) then
            fail $"Host value does not match {Types.typeToString expected}"

    let create (registration: ExtensionRegistration) typeName fields payload =
        let definitions = typeDefinitions registration []
        match registration.Types |> List.tryFind (fun d -> d.Name = typeName && d.HostManaged) with
        | Some descriptor ->
            let value = VHostManaged { Descriptor = descriptor; Fields = fields; Payload = payload }
            validate definitions (TNamed typeName) value
            value
        | None -> fail $"Unknown host-managed type '{typeName}'"

    let unwrap typeName value =
        match value with
        | VHostManaged host when host.Descriptor.Name = typeName && host.Descriptor.HostManaged -> host
        | _ -> fail $"Expected host-managed type '{typeName}'"

    /// Validate a call with one set of type-variable bindings, including polymorphic round trips.
    let internal prepareCall definitions signature arguments =
        let mutable bindings : Map<int, Type> = Map.empty
        let variables = Types.ftvType signature
        let mutable nextVariable = if Set.isEmpty variables then -1 else min -1 (Set.minElement variables - 1)
        let fresh () =
            let id = nextVariable
            nextVariable <- nextVariable - 1
            TVar id
        let keyValue key =
            match key with
            | MKString s -> VString s
            | MKInt n -> VInt n
            | MKBool b -> VBool b
            | MKUnionCase (name, caseName) -> VUnionCase(name, caseName, None)
        let valueType value =
            match value with
            | VUnit -> TUnit
            | VInt _ -> TInt
            | VFloat _ -> TFloat
            | VBool _ -> TBool
            | VString _ -> TString
            | VHostManaged host -> THostManaged(host.Descriptor.Name, host.Descriptor.Fields)
            | VList _ -> TList(fresh ())
            | VTuple values -> TTuple(values |> List.map (fun _ -> fresh ()))
            | VRecord fields -> TRecord(fields |> Map.map (fun _ _ -> fresh ()))
            | VMap _ -> TMap(fresh (), fresh ())
            | VOption _ -> TOption(fresh ())
            | VUnionCase (name, _, _) -> TNamed name
            | VTypeToken _ -> TTypeToken
            | VTask _ -> TTask(fresh ())
            | VClosure _ | VExternal _ | VUnionCtor _ -> TFun(fresh (), fresh ())
        let rec observe expected value =
            let observeFields (required: Map<string, Type>) (actual: Map<string, Value>) =
                required |> Map.iter (fun name t -> observe t actual.[name])
            match expected with
            | TVar id | TNumeric id ->
                match bindings.TryFind id with
                | Some bound -> observe bound value
                | None ->
                    validate definitions expected value
                    let bound = valueType value
                    bindings <- bindings.Add(id, bound)
                    observe bound value
            | TNamed name ->
                match Map.tryFind name definitions with
                | Some expanded -> observe expanded value
                | None -> validate definitions expected value
            | TRecordShape (id, fields, constructible) ->
                validate definitions expected value
                match bindings.TryFind id with
                | Some bound -> observe bound value
                | None ->
                    let bound = valueType value
                    bindings <- bindings.Add(id, bound)
                    observe bound value
                match value with
                | VHostManaged host when not constructible -> observeFields fields host.Fields
                | VRecord actual -> observeFields fields actual
                | _ -> validate definitions expected value
            | _ ->
                validate definitions expected value
                match expected, value with
                | THostManaged (name, fields), VHostManaged host ->
                    // A polymorphic parameter must also carry a registered descriptor.
                    match Map.tryFind name definitions with
                    | Some (THostManaged (_, registered)) when registered = fields -> ()
                    | _ -> fail $"Unregistered host-managed type '{name}'"
                    observeFields fields host.Fields
                | TRecord fields, VRecord actual -> observeFields fields actual
                | TList t, VList values -> values |> List.iter (observe t)
                | TTuple ts, VTuple values -> List.iter2 observe ts values
                | TOption t, VOption (Some inner) -> observe t inner
                | TMap (keyType, t), VMap values ->
                    values |> Map.iter (fun key item -> observe keyType (keyValue key); observe t item)
                | TUnion (_, cases), VUnionCase (_, name, Some inner) ->
                    cases.[name] |> Option.iter (fun t -> observe t inner)
                | _ -> ()
        let rec check remainingType remainingArguments =
            match remainingType, remainingArguments with
            | TFun (argumentType, resultType), argument :: tail ->
                observe argumentType argument
                check resultType tail
            | resultType, [] -> resultType
            | _ -> fail "Invalid argument count for host call"
        let resultType = check signature arguments
        fun result -> observe resultType result; result

    let validateCall definitions signature arguments action =
        let validateResult = prepareCall definitions signature arguments
        validateResult (action ())

    let invoke definitions (externalFunction: ExternalFunction) context arguments =
        let (Forall (_, signature)) = externalFunction.Scheme
        validateCall definitions signature arguments (fun () -> externalFunction.Impl context arguments)

    let (|RecordFields|_|) value =
        match value with
        | VRecord fields -> Some fields
        | VHostManaged host -> Some host.Fields
        | _ -> None
