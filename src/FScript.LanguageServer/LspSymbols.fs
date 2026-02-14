namespace FScript.LanguageServer

open System
open System.Collections.Generic
open System.IO
open System.Text.Json.Nodes
open FScript.Language
open FScript.CSharpInterop

module LspSymbols =
    open LspModel

    let private collectMapKeyDomainVars (t: Type) =
        let rec collect acc ty =
            match ty with
            | TMap (TVar v, valueType) ->
                collect (Set.add v acc) valueType
            | TMap (keyType, valueType) ->
                collect (collect acc keyType) valueType
            | TList inner
            | TOption inner -> collect acc inner
            | TTuple items ->
                items |> List.fold collect acc
            | TRecord fields ->
                fields |> Map.values |> Seq.fold collect acc
            | TFun (a, b) ->
                collect (collect acc a) b
            | _ -> acc

        collect Set.empty t

    let private lspTypeToStringWithKeyDomainVars (keyDomainVars: Set<int>) (t: Type) =
        let rec go t =
            match t with
            | TUnit -> "unit"
            | TInt -> "int"
            | TFloat -> "float"
            | TBool -> "bool"
            | TString -> "string"
            | TList t1 -> sprintf "%s list" (postfixArg t1)
            | TTuple ts -> ts |> List.map go |> String.concat " * " |> sprintf "(%s)"
            | TRecord fields ->
                fields
                |> Map.toList
                |> List.map (fun (name, fieldType) -> sprintf "%s: %s" name (go fieldType))
                |> String.concat "; "
                |> sprintf "{ %s }"
            | TMap (_, tv) ->
                sprintf "%s map" (postfixArg tv)
            | TOption t1 -> sprintf "%s option" (postfixArg t1)
            | TFun (a, b) -> sprintf "(%s -> %s)" (go a) (go b)
            | TNamed n -> n
            | TUnion (name, _) -> name
            | TTypeToken -> "type"
            | TVar v when Set.contains v keyDomainVars -> "int|string"
            | TVar _ -> "unknown"
        and postfixArg t =
            match t with
            | TFun _ | TTuple _ | TRecord _ -> sprintf "(%s)" (go t)
            | _ -> go t
        go t

    let private lspTypeToString (t: Type) =
        let keyDomainVars = collectMapKeyDomainVars t
        lspTypeToStringWithKeyDomainVars keyDomainVars t

    let private schemeTypeToString (scheme: Scheme) =
        match scheme with
        | Forall (_, t) -> Types.typeToString t

    let private stdlibFunctionSignatures : Lazy<Map<string, string>> =
        lazy
            let typedStdlib = InteropServices.inferStdlibWithExternsRaw []
            typedStdlib
            |> List.collect (function
                | TypeInfer.TSLet(name, _, t, _, _, _) ->
                    match t with
                    | TFun _ -> [ name, Types.typeToString t ]
                    | _ -> []
                | TypeInfer.TSLetRecGroup(bindings, _, _) ->
                    bindings
                    |> List.choose (fun (name, _, t, _) ->
                        match t with
                        | TFun _ -> Some (name, Types.typeToString t)
                        | _ -> None)
                | _ -> [])
            |> Map.ofList

    let private tryStdlibVirtualUriFromSource (sourceFile: string option) =
        match sourceFile with
        | Some file when file.EndsWith("Stdlib.Option.fss", StringComparison.Ordinal) || file.EndsWith("Option.fss", StringComparison.Ordinal) -> Some "fscript-stdlib:///Option.fss"
        | Some file when file.EndsWith("Stdlib.List.fss", StringComparison.Ordinal) || file.EndsWith("List.fss", StringComparison.Ordinal) -> Some "fscript-stdlib:///List.fss"
        | Some file when file.EndsWith("Stdlib.Map.fss", StringComparison.Ordinal) || file.EndsWith("Map.fss", StringComparison.Ordinal) -> Some "fscript-stdlib:///Map.fss"
        | _ -> None

    let private stdlibFunctionParameterNames : Lazy<Map<string, string list>> =
        lazy
            InteropServices.stdlibProgram()
            |> List.collect (function
                | SLet(name, args, _, _, _, _) ->
                    [ name, (args |> List.map (fun p -> p.Name)) ]
                | SLetRecGroup(bindings, _, _) ->
                    bindings
                    |> List.map (fun (name, args, _, _) -> name, (args |> List.map (fun p -> p.Name)))
                | _ -> [])
            |> Map.ofList

    let private stdlibFunctionDefinitions : Lazy<Map<string, (string * Span)>> =
        lazy
            InteropServices.stdlibProgram()
            |> List.collect (function
                | SLet(name, _, _, _, _, span) ->
                    match tryStdlibVirtualUriFromSource span.Start.File with
                    | Some uri -> [ name, (uri, span) ]
                    | None -> []
                | SLetRecGroup(bindings, _, _) ->
                    bindings
                    |> List.collect (fun (name, _, _, span) ->
                        match tryStdlibVirtualUriFromSource span.Start.File with
                        | Some uri -> [ name, (uri, span) ]
                        | None -> [])
                | _ -> [])
            |> Map.ofList

    let private buildInjectedFunctionData (externs: ExternalFunction list) =
        let fromExterns =
            externs
            |> List.map (fun ext -> ext.Name, schemeTypeToString ext.Scheme)
            |> Map.ofList

        let builtinSignatures =
            [ "ignore", "'a -> unit"
              "print", "string -> unit"
              "nameof", "string -> string"
              "typeof", "string -> type" ]
            |> Map.ofList

        let builtinParamNames =
            [ "ignore", [ "value" ]
              "print", [ "message" ]
              "nameof", [ "name" ]
              "typeof", [ "name" ] ]
            |> Map.ofList

        let signatures =
            stdlibFunctionSignatures.Value
            |> Map.fold (fun acc name signature -> acc |> Map.add name signature) fromExterns
            |> Map.fold (fun acc name signature -> acc |> Map.add name signature) builtinSignatures

        let paramNames =
            stdlibFunctionParameterNames.Value
            |> Map.fold (fun acc name names -> acc |> Map.add name names) builtinParamNames

        signatures, paramNames, stdlibFunctionDefinitions.Value

    let rec private typeRefToString (typeRef: TypeRef) =
        match typeRef with
        | TRName name -> name
        | TRTuple parts -> parts |> List.map typeRefToString |> String.concat " * " |> sprintf "(%s)"
        | TRFun (a, b) -> sprintf "%s -> %s" (typeRefToString a) (typeRefToString b)
        | TRPostfix (inner, suffix) -> sprintf "%s %s" (typeRefToString inner) suffix
        | TRRecord fields ->
            fields
            |> List.map (fun (name, t) -> sprintf "%s: %s" name (typeRefToString t))
            |> String.concat "; "
            |> sprintf "{ %s }"
        | TRStructuralRecord fields ->
            fields
            |> List.map (fun (name, t) -> sprintf "%s: %s" name (typeRefToString t))
            |> String.concat "; "
            |> sprintf "{| %s |}"

    let private canonicalRecordSignatureFromFields (fields: (string * string) list) =
        fields
        |> List.sortBy fst
        |> List.map (fun (name, t) -> $"{name}:{t}")
        |> String.concat ";"

    let buildSymbolsFromProgram (program: Program) (typed: TypeInfer.TypedProgram option) : TopLevelSymbol list =
        let typedByName = Dictionary<string, TypeInfer.TypedStmt>(StringComparer.Ordinal)
        let recordTypeDefsBySignature = Dictionary<string, ResizeArray<string>>(StringComparer.Ordinal)

        let canonicalRecordSignatureFromType (t: Type) =
            match t with
            | TRecord fields ->
                fields
                |> Map.toList
                |> List.map (fun (name, ty) -> name, Types.typeToString ty)
                |> canonicalRecordSignatureFromFields
                |> Some
            | _ -> None

        match typed with
        | Some typedProgram ->
            for stmt in typedProgram do
                match stmt with
                | TypeInfer.TSLet(name, _, _, _, _, _) ->
                    typedByName[name] <- stmt
                | TypeInfer.TSLetRecGroup(bindings, _, _) ->
                    for (name, _, _, _) in bindings do
                        typedByName[name] <- stmt
                | _ -> ()
        | None -> ()

        for stmt in program do
            match stmt with
            | SType typeDef when typeDef.Cases.IsEmpty ->
                let signature =
                    typeDef.Fields
                    |> List.map (fun (fieldName, t) -> fieldName, typeRefToString t)
                    |> canonicalRecordSignatureFromFields

                if not (recordTypeDefsBySignature.ContainsKey(signature)) then
                    recordTypeDefsBySignature[signature] <- ResizeArray<string>()
                recordTypeDefsBySignature[signature].Add(typeDef.Name)
            | _ -> ()

        let mkFromTyped (name: string) (span: Span) (fallbackKind: int) =
            let tryResolveRecordTarget (t: Type) =
                match canonicalRecordSignatureFromType t with
                | Some signature when recordTypeDefsBySignature.ContainsKey(signature) ->
                    let candidates = recordTypeDefsBySignature[signature] |> Seq.distinct |> Seq.toList
                    match candidates with
                    | [ one ] -> Some one
                    | _ -> None
                | _ -> None

            let rec typeTargetFromType (t: Type) =
                match t with
                | TNamed name -> Some name
                | TUnion (name, _) -> Some name
                | TRecord _ -> tryResolveRecordTarget t
                | TFun (_, ret) -> typeTargetFromType ret
                | _ -> None

            match typedByName.TryGetValue(name) with
            | true, TypeInfer.TSLet(_, _, t, _, _, _) ->
                { Name = name
                  Kind = symbolKindForType t
                  TypeText = Some (lspTypeToString t)
                  TypeTargetName = typeTargetFromType t
                  Span = span }
            | true, TypeInfer.TSLetRecGroup(bindings, _, _) ->
                match bindings |> List.tryFind (fun (n, _, _, _) -> n = name) with
                | Some (_, _, t, _) ->
                    { Name = name
                      Kind = symbolKindForType t
                      TypeText = Some (lspTypeToString t)
                      TypeTargetName = typeTargetFromType t
                      Span = span }
                | None ->
                    { Name = name
                      Kind = fallbackKind
                      TypeText = None
                      TypeTargetName = None
                      Span = span }
            | _ ->
                { Name = name
                  Kind = fallbackKind
                  TypeText = None
                  TypeTargetName = None
                  Span = span }

        program
        |> List.collect (fun stmt ->
            match stmt with
            | SType typeDef ->
                let typeText =
                    if typeDef.Cases.IsEmpty then
                        let fields =
                            typeDef.Fields
                            |> List.map (fun (name, t) -> $"{name}: {typeRefToString t}")
                            |> String.concat "; "
                        Some $"{{ {fields} }}"
                    else
                        Some typeDef.Name

                let typeSymbol =
                    { Name = typeDef.Name
                      Kind = 5
                      TypeText = typeText
                      TypeTargetName = None
                      Span = typeDef.Span }

                let caseSymbols =
                    typeDef.Cases
                    |> List.collect (fun (caseName, payload) ->
                        let caseType =
                            match payload with
                            | Some payloadType -> Some (sprintf "%s -> %s" (typeRefToString payloadType) typeDef.Name)
                            | None -> Some typeDef.Name

                        [ { Name = caseName
                            Kind = 22
                            TypeText = caseType
                            TypeTargetName = Some typeDef.Name
                            Span = typeDef.Span }
                          { Name = $"{typeDef.Name}.{caseName}"
                            Kind = 22
                            TypeText = caseType
                            TypeTargetName = Some typeDef.Name
                            Span = typeDef.Span } ])

                typeSymbol :: caseSymbols
            | SLet(name, args, _, _, _, span) ->
                [ mkFromTyped name span (declarationKindFromArgs args) ]
            | SLetRecGroup(bindings, _, _) ->
                bindings
                |> List.map (fun (name, args, _, span) -> mkFromTyped name span (declarationKindFromArgs args))
            | _ -> [])

    let private buildTopLevelTypeTargetFromProgram (program: Program) =
        let recordTypeBySignature =
            program
            |> List.choose (function
                | SType typeDef when typeDef.Cases.IsEmpty ->
                    let fields =
                        typeDef.Fields
                        |> List.map (fun (fieldName, t) -> fieldName, typeRefToString t)
                    Some (canonicalRecordSignatureFromFields fields, typeDef.Name)
                | _ -> None)
            |> List.groupBy fst
            |> List.choose (fun (signature, entries) ->
                let names = entries |> List.map snd |> List.distinct
                match names with
                | [ single ] -> Some (signature, single)
                | _ -> None)
            |> Map.ofList

        let canonicalRecordSignatureFromType (t: Type) =
            match t with
            | TRecord fields ->
                fields
                |> Map.toList
                |> List.map (fun (name, ty) -> name, Types.typeToString ty)
                |> canonicalRecordSignatureFromFields
                |> Some
            | _ -> None

        let rec resolveTypeTarget (t: Type) =
            match t with
            | TNamed name -> Some name
            | TUnion (name, _) -> Some name
            | TRecord _ ->
                match canonicalRecordSignatureFromType t with
                | Some signature -> recordTypeBySignature |> Map.tryFind signature
                | None -> None
            | TFun (_, ret) -> resolveTypeTarget ret
            | _ -> None

        resolveTypeTarget

    let private inferTopLevelTypesBestEffort (externs: ExternalFunction list) (program: Program) : Map<string, Type> =
        let mutable accepted: Program = []
        let mutable result: Map<string, Type> = Map.empty

        let tryInferWithCurrent (candidate: Program) =
            try
                let typed, _ = InteropServices.inferProgramWithExternsAndLocalVariableTypes externs candidate
                Some typed
            with
            | _ -> None

        let extractTypeForName (typed: TypeInfer.TypedProgram) (targetName: string) =
            typed
            |> List.tryPick (fun stmt ->
                match stmt with
                | TypeInfer.TSLet (name, _, t, _, _, _) when name = targetName ->
                    Some t
                | TypeInfer.TSLetRecGroup (bindings, _, _) ->
                    bindings
                    |> List.tryPick (fun (name, _, t, _) ->
                        if name = targetName then Some t else None)
                | _ -> None)

        for stmt in program do
            match stmt with
            | SType _ ->
                accepted <- accepted @ [ stmt ]
            | SLet (name, _, _, _, _, _) ->
                let candidate = accepted @ [ stmt ]
                match tryInferWithCurrent candidate with
                | Some typed ->
                    accepted <- candidate
                    match extractTypeForName typed name with
                    | Some t -> result <- result |> Map.add name t
                    | None -> ()
                | None -> ()
            | SLetRecGroup (bindings, _, _) ->
                let candidate = accepted @ [ stmt ]
                match tryInferWithCurrent candidate with
                | Some typed ->
                    accepted <- candidate
                    for (name, _, _, _) in bindings do
                        match extractTypeForName typed name with
                        | Some t -> result <- result |> Map.add name t
                        | None -> ()
                | None -> ()
            | SExpr _ ->
                ()
            | SImport _ ->
                ()

        result

    let private inferLocalVariableTypesBestEffort (externs: ExternalFunction list) (program: Program) : TypeInfer.LocalVariableTypeInfo list =
        let mutable accepted: Program = []
        let collected = Dictionary<(string * int * int * int * int * string), TypeInfer.LocalVariableTypeInfo>()

        let tryInferWithCurrent (candidate: Program) =
            try
                let _, localTypes = InteropServices.inferProgramWithExternsAndLocalVariableTypes externs candidate
                Some localTypes
            with
            | _ -> None

        let keyOf (entry: TypeInfer.LocalVariableTypeInfo) =
            let file = entry.Span.Start.File |> Option.defaultValue ""
            (entry.Name, entry.Span.Start.Line, entry.Span.Start.Column, entry.Span.End.Line, entry.Span.End.Column, file)

        for stmt in program do
            match stmt with
            | SType _ ->
                accepted <- accepted @ [ stmt ]
            | SLet _
            | SLetRecGroup _ ->
                let candidate = accepted @ [ stmt ]
                match tryInferWithCurrent candidate with
                | Some localTypes ->
                    accepted <- candidate
                    for entry in localTypes do
                        collected[keyOf entry] <- entry
                | None -> ()
            | SExpr _ ->
                ()
            | SImport _ ->
                ()

        collected.Values |> Seq.toList

    let collectVariableOccurrences (program: Program) : Map<string, Span list> =
        let addOccurrence name span (acc: Map<string, Span list>) =
            let existing = acc |> Map.tryFind name |> Option.defaultValue []
            acc |> Map.add name (span :: existing)

        let rec collectExpr (acc: Map<string, Span list>) (expr: Expr) =
            match expr with
            | EVar (name, span) -> addOccurrence name span acc
            | EParen (inner, _) -> collectExpr acc inner
            | ELambda (_, body, _) -> collectExpr acc body
            | EApply (fn, arg, _) ->
                let withFn = collectExpr acc fn
                collectExpr withFn arg
            | EIf (c, t, f, _) ->
                let withCond = collectExpr acc c
                let withThen = collectExpr withCond t
                collectExpr withThen f
            | ERaise (inner, _) -> collectExpr acc inner
            | EFor (_, source, body, _) ->
                let withSource = collectExpr acc source
                collectExpr withSource body
            | EMatch (scrutinee, cases, _) ->
                let withScrutinee = collectExpr acc scrutinee
                cases
                |> List.fold (fun state (_, guard, body, _) ->
                    let withGuard =
                        match guard with
                        | Some g -> collectExpr state g
                        | None -> state
                    collectExpr withGuard body) withScrutinee
            | ELet (_, value, body, _, _) ->
                let withValue = collectExpr acc value
                collectExpr withValue body
            | ELetRecGroup (bindings, body, _) ->
                let withBindings =
                    bindings
                    |> List.fold (fun state (_, _, value, _) -> collectExpr state value) acc
                collectExpr withBindings body
            | EList (items, _) ->
                items |> List.fold collectExpr acc
            | ERange (startExpr, endExpr, _) ->
                let withStart = collectExpr acc startExpr
                collectExpr withStart endExpr
            | ETuple (items, _) ->
                items |> List.fold collectExpr acc
            | ERecord (fields, _) ->
                fields |> List.fold (fun state (_, value) -> collectExpr state value) acc
            | EStructuralRecord (fields, _) ->
                fields |> List.fold (fun state (_, value) -> collectExpr state value) acc
            | EMap (entries, _) ->
                entries
                |> List.fold (fun state entry ->
                    match entry with
                    | MEKeyValue (keyExpr, valueExpr) ->
                        let withKey = collectExpr state keyExpr
                        collectExpr withKey valueExpr
                    | MESpread spreadExpr ->
                        collectExpr state spreadExpr) acc
            | ERecordUpdate (baseExpr, fields, _) ->
                let withBase = collectExpr acc baseExpr
                fields |> List.fold (fun state (_, value) -> collectExpr state value) withBase
            | EStructuralRecordUpdate (baseExpr, fields, _) ->
                let withBase = collectExpr acc baseExpr
                fields |> List.fold (fun state (_, value) -> collectExpr state value) withBase
            | EFieldGet (target, _, _) -> collectExpr acc target
            | EIndexGet (target, key, _) ->
                let withTarget = collectExpr acc target
                collectExpr withTarget key
            | ECons (head, tail, _) ->
                let withHead = collectExpr acc head
                collectExpr withHead tail
            | EAppend (left, right, _) ->
                let withLeft = collectExpr acc left
                collectExpr withLeft right
            | EBinOp (_, left, right, _) ->
                let withLeft = collectExpr acc left
                collectExpr withLeft right
            | ESome (inner, _) -> collectExpr acc inner
            | EInterpolatedString (parts, _) ->
                parts
                |> List.fold (fun state part ->
                    match part with
                    | IPText _ -> state
                    | IPExpr embedded -> collectExpr state embedded) acc
            | EUnit _
            | ELiteral _
            | ENone _
            | ETypeOf _
            | ENameOf _ -> acc

        let withDeclsAndExprs =
            program
            |> List.fold (fun state stmt ->
                match stmt with
                | SLet (name, _, expr, _, _, span) ->
                    let withDecl = addOccurrence name span state
                    collectExpr withDecl expr
                | SLetRecGroup (bindings, _, _) ->
                    bindings
                    |> List.fold (fun inner (name, _, expr, span) ->
                        let withDecl = addOccurrence name span inner
                        collectExpr withDecl expr) state
                | SExpr expr ->
                    collectExpr state expr
                | _ -> state) Map.empty

        withDeclsAndExprs
        |> Map.map (fun _ spans -> spans |> List.rev)

    let private buildRecordParameterFields (program: Program) =
        let typeRecordFields =
            program
            |> List.choose (function
                | SType typeDef when typeDef.Cases.IsEmpty ->
                    let fields =
                        typeDef.Fields
                        |> List.map (fun (fieldName, t) -> fieldName, typeRefToString t)
                    Some (typeDef.Name, fields)
                | _ -> None)
            |> Map.ofList

        let fieldsFromAnnotation (annotation: TypeRef option) =
            match annotation with
            | Some (TRRecord fields) ->
                Some (fields |> List.map (fun (fieldName, t) -> fieldName, typeRefToString t))
            | Some (TRName typeName) ->
                typeRecordFields |> Map.tryFind typeName
            | _ ->
                None

        let collectFromArgs (acc: Map<string, (string * string) list>) (args: Param list) =
            args
            |> List.fold (fun state arg ->
                match fieldsFromAnnotation arg.Annotation with
                | Some fields -> state |> Map.add arg.Name fields
                | None -> state) acc

        program
        |> List.fold (fun state stmt ->
            match stmt with
            | SLet (_, args, _, _, _, _) ->
                collectFromArgs state args
            | SLetRecGroup (bindings, _, _) ->
                bindings
                |> List.fold (fun inner (_, args, _, _) -> collectFromArgs inner args) state
            | _ ->
                state) Map.empty

    let private buildParameterTypeTargets (program: Program) =
        let namedRecordTypeBySignature =
            program
            |> List.choose (function
                | SType typeDef when typeDef.Cases.IsEmpty ->
                    let fields =
                        typeDef.Fields
                        |> List.map (fun (fieldName, t) -> fieldName, typeRefToString t)
                    Some (canonicalRecordSignatureFromFields fields, typeDef.Name)
                | _ -> None)
            |> List.groupBy fst
            |> List.choose (fun (sigText, entries) ->
                let names = entries |> List.map snd |> List.distinct
                match names with
                | [ one ] -> Some (sigText, one)
                | _ -> None)
            |> Map.ofList

        let resolveAnnotationTarget (annotation: TypeRef option) =
            match annotation with
            | Some (TRName typeName) -> Some typeName
            | Some (TRRecord fields) ->
                fields
                |> List.map (fun (fieldName, t) -> fieldName, typeRefToString t)
                |> canonicalRecordSignatureFromFields
                |> fun sigText -> namedRecordTypeBySignature |> Map.tryFind sigText
            | _ -> None

        let collectFromArgs (acc: Map<string, string>) (args: Param list) =
            args
            |> List.fold (fun state arg ->
                match resolveAnnotationTarget arg.Annotation with
                | Some typeName -> state |> Map.add arg.Name typeName
                | None -> state) acc

        program
        |> List.fold (fun state stmt ->
            match stmt with
            | SLet (_, args, _, _, _, _) ->
                collectFromArgs state args
            | SLetRecGroup (bindings, _, _) ->
                bindings
                |> List.fold (fun inner (_, args, _, _) -> collectFromArgs inner args) state
            | _ ->
                state) Map.empty

    let private buildFunctionParameters (program: Program) =
        let addBinding name (args: Param list) (acc: Map<string, string list>) =
            let paramNames =
                args
                |> List.map (fun p -> p.Name)
                |> List.filter (fun n -> not (String.IsNullOrWhiteSpace(n)))
            if paramNames.IsEmpty then acc else acc |> Map.add name paramNames

        program
        |> List.fold (fun state stmt ->
            match stmt with
            | SLet (name, args, _, _, _, _) ->
                addBinding name args state
            | SLetRecGroup (bindings, _, _) ->
                bindings
                |> List.fold (fun inner (name, args, _, _) -> addBinding name args inner) state
            | _ ->
                state) Map.empty

    let private buildFunctionAnnotationTypes (program: Program) =
        let addBinding name (args: Param list) (acc: Map<string, string list>) =
            let hasAnnotation = args |> List.exists (fun p -> p.Annotation.IsSome)
            if not hasAnnotation then acc
            else
                let annotated =
                    args
                    |> List.map (fun p ->
                        match p.Annotation with
                        | Some t -> typeRefToString t
                        | None -> "unknown")
                acc |> Map.add name annotated

        program
        |> List.fold (fun state stmt ->
            match stmt with
            | SLet (name, args, _, _, _, _) ->
                addBinding name args state
            | SLetRecGroup (bindings, _, _) ->
                bindings
                |> List.fold (fun inner (name, args, _, _) -> addBinding name args inner) state
            | _ ->
                state) Map.empty

    let private buildFunctionDeclaredReturnTargets (program: Program) =
        let recordTypeByFieldNames =
            program
            |> List.choose (function
                | SType typeDef when typeDef.Cases.IsEmpty ->
                    let signature =
                        typeDef.Fields
                        |> List.map fst
                        |> List.sort
                        |> String.concat ";"
                    Some (signature, typeDef.Name)
                | _ -> None)
            |> List.groupBy fst
            |> List.choose (fun (signature, entries) ->
                let names = entries |> List.map snd |> List.distinct
                match names with
                | [ one ] -> Some (signature, one)
                | _ -> None)
            |> Map.ofList

        let rec terminalExpr (expr: Expr) =
            match expr with
            | ELet (_, _, body, _, _) -> terminalExpr body
            | ELetRecGroup (_, body, _) -> terminalExpr body
            | EParen (inner, _) -> terminalExpr inner
            | _ -> expr

        let tryResolve expr =
            match terminalExpr expr with
            | ERecord (fields, _)
            | EStructuralRecord (fields, _) ->
                let signature =
                    fields
                    |> List.map fst
                    |> List.sort
                    |> String.concat ";"
                recordTypeByFieldNames |> Map.tryFind signature
            | _ -> None

        program
        |> List.fold (fun state stmt ->
            match stmt with
            | SLet (name, args, expr, _, _, _) when not args.IsEmpty ->
                match tryResolve expr with
                | Some returnType -> state |> Map.add name returnType
                | None -> state
            | SLetRecGroup (bindings, _, _) ->
                bindings
                |> List.fold (fun inner (name, args, expr, _) ->
                    if args.IsEmpty then inner
                    else
                        match tryResolve expr with
                        | Some returnType -> inner |> Map.add name returnType
                        | None -> inner) state
            | _ -> state) Map.empty

    let private buildRecordTypeFieldTypeMap (program: Program) =
        program
        |> List.choose (function
            | SType typeDef when typeDef.Cases.IsEmpty ->
                let fields =
                    typeDef.Fields
                    |> List.map (fun (fieldName, t) -> fieldName, typeRefToString t)
                    |> Map.ofList
                Some (typeDef.Name, fields)
            | _ -> None)
        |> Map.ofList

    let private spanContainsPosition1Based (span: Span) (line: int) (column: int) =
        let sameFile =
            match span.Start.File, span.End.File with
            | Some sf, Some ef when not (String.Equals(sf, ef, StringComparison.OrdinalIgnoreCase)) -> false
            | _ -> true
        let startsBefore =
            line > span.Start.Line
            || (line = span.Start.Line && column >= span.Start.Column)
        let endsAfter =
            line < span.End.Line
            || (line = span.End.Line && column <= span.End.Column)
        sameFile && startsBefore && endsAfter

    let private spanStartAtOrBefore (candidate: Span) (line: int) (column: int) =
        candidate.Start.Line < line
        || (candidate.Start.Line = line && candidate.Start.Column <= column)

    let private inferLocalTypesFromReturnedRecordFields
        (program: Program)
        (functionDeclaredReturnTargets: Map<string, string>)
        (localBindings: LocalBindingInfo list)
        : (Span * string * string) list =

        let recordFieldTypesByType = buildRecordTypeFieldTypeMap program

        let pickNearestBinding (name: string) (usageSpan: Span) =
            localBindings
            |> List.filter (fun binding ->
                String.Equals(binding.Name, name, StringComparison.Ordinal)
                && spanContainsPosition1Based binding.ScopeSpan usageSpan.Start.Line usageSpan.Start.Column
                && spanStartAtOrBefore binding.DeclSpan usageSpan.Start.Line usageSpan.Start.Column)
            |> List.sortByDescending (fun binding -> binding.DeclSpan.Start.Line, binding.DeclSpan.Start.Column)
            |> List.tryHead

        let rec collectFieldVarUses (fieldTypes: Map<string, string>) (expr: Expr) : (string * Span * string) list =
            let nested =
                match expr with
                | EApply (f, a, _) -> collectFieldVarUses fieldTypes f @ collectFieldVarUses fieldTypes a
                | EIf (c, t, f, _) ->
                    collectFieldVarUses fieldTypes c
                    @ collectFieldVarUses fieldTypes t
                    @ collectFieldVarUses fieldTypes f
                | ERaise (inner, _)
                | ESome (inner, _)
                | EParen (inner, _) -> collectFieldVarUses fieldTypes inner
                | EFor (_, source, body, _) ->
                    collectFieldVarUses fieldTypes source @ collectFieldVarUses fieldTypes body
                | EMatch (scrutinee, cases, _) ->
                    let inScrutinee = collectFieldVarUses fieldTypes scrutinee
                    let inCases =
                        cases
                        |> List.collect (fun (_, guard, body, _) ->
                            let inGuard =
                                match guard with
                                | Some g -> collectFieldVarUses fieldTypes g
                                | None -> []
                            inGuard @ collectFieldVarUses fieldTypes body)
                    inScrutinee @ inCases
                | ELet (_, value, body, _, _) ->
                    collectFieldVarUses fieldTypes value @ collectFieldVarUses fieldTypes body
                | ELetRecGroup (bindings, body, _) ->
                    let inBindings =
                        bindings
                        |> List.collect (fun (_, _, valueExpr, _) -> collectFieldVarUses fieldTypes valueExpr)
                    inBindings @ collectFieldVarUses fieldTypes body
                | ELambda (_, body, _) ->
                    collectFieldVarUses fieldTypes body
                | EList (items, _)
                | ETuple (items, _) ->
                    items |> List.collect (collectFieldVarUses fieldTypes)
                | ERange (a, b, _) ->
                    collectFieldVarUses fieldTypes a @ collectFieldVarUses fieldTypes b
                | ERecord (fields, _)
                | EStructuralRecord (fields, _) ->
                    fields |> List.collect (fun (_, valueExpr) -> collectFieldVarUses fieldTypes valueExpr)
                | EMap (entries, _) ->
                    entries
                    |> List.collect (function
                        | MEKeyValue (k, v) -> collectFieldVarUses fieldTypes k @ collectFieldVarUses fieldTypes v
                        | MESpread e -> collectFieldVarUses fieldTypes e)
                | ERecordUpdate (target, fields, _)
                | EStructuralRecordUpdate (target, fields, _) ->
                    collectFieldVarUses fieldTypes target
                    @ (fields |> List.collect (fun (_, v) -> collectFieldVarUses fieldTypes v))
                | EFieldGet (target, _, _) ->
                    collectFieldVarUses fieldTypes target
                | EIndexGet (a, b, _)
                | ECons (a, b, _)
                | EAppend (a, b, _)
                | EBinOp (_, a, b, _) ->
                    collectFieldVarUses fieldTypes a @ collectFieldVarUses fieldTypes b
                | EInterpolatedString (parts, _) ->
                    parts
                    |> List.collect (function
                        | IPText _ -> []
                        | IPExpr embedded -> collectFieldVarUses fieldTypes embedded)
                | EUnit _
                | ELiteral _
                | EVar _
                | ENone _
                | ETypeOf _
                | ENameOf _ -> []

            let fromRecord =
                match expr with
                | ERecord (fields, _)
                | EStructuralRecord (fields, _) ->
                    fields
                    |> List.choose (fun (fieldName, valueExpr) ->
                        match valueExpr, fieldTypes |> Map.tryFind fieldName with
                        | EVar (localName, usageSpan), Some fieldType ->
                            Some (localName, usageSpan, fieldType)
                        | _ -> None)
                | _ -> []

            fromRecord @ nested

        let fromBinding name expr =
            match functionDeclaredReturnTargets |> Map.tryFind name with
            | Some returnTypeName ->
                match recordFieldTypesByType |> Map.tryFind returnTypeName with
                | Some fieldTypes ->
                    collectFieldVarUses fieldTypes expr
                    |> List.choose (fun (localName, usageSpan, fieldType) ->
                        pickNearestBinding localName usageSpan
                        |> Option.map (fun binding -> binding.DeclSpan, binding.Name, fieldType))
                | None -> []
            | None -> []

        program
        |> List.collect (function
            | SLet (name, args, expr, _, _, _) when not args.IsEmpty ->
                fromBinding name expr
            | SLetRecGroup (bindings, _, _) ->
                bindings
                |> List.collect (fun (name, args, expr, _) ->
                    if args.IsEmpty then [] else fromBinding name expr)
            | _ -> [])
        |> List.distinctBy (fun (span, name, _) ->
            let file = span.Start.File |> Option.defaultValue ""
            name, span.Start.Line, span.Start.Column, span.End.Line, span.End.Column, file)

    let private buildLocalBindings (program: Program) =
        let mkBinding (name: string) (declSpan: Span) (scopeSpan: Span) (annotation: string option) =
            { Name = name
              DeclSpan = declSpan
              ScopeSpan = scopeSpan
              AnnotationType = annotation }

        let rec collectPatternBindings (scopeSpan: Span) (pattern: Pattern) =
            match pattern with
            | PVar (name, span) ->
                [ mkBinding name span scopeSpan None ]
            | PCons (head, tail, _) ->
                collectPatternBindings scopeSpan head @ collectPatternBindings scopeSpan tail
            | PTuple (items, _) ->
                items |> List.collect (collectPatternBindings scopeSpan)
            | PRecord (fields, _) ->
                fields |> List.collect (fun (_, p) -> collectPatternBindings scopeSpan p)
            | PMap (clauses, tailOpt, _) ->
                let fromClauses =
                    clauses
                    |> List.collect (fun (k, v) ->
                        collectPatternBindings scopeSpan k @ collectPatternBindings scopeSpan v)
                let fromTail =
                    match tailOpt with
                    | Some tail -> collectPatternBindings scopeSpan tail
                    | None -> []
                fromClauses @ fromTail
            | PSome (inner, _) ->
                collectPatternBindings scopeSpan inner
            | PUnionCase (_, _, payload, _) ->
                match payload with
                | Some p -> collectPatternBindings scopeSpan p
                | None -> []
            | PWildcard _
            | PLiteral _
            | PNil _
            | PNone _
            | PTypeRef _ -> []

        let rec collectExprBindings (expr: Expr) : LocalBindingInfo list =
            match expr with
            | ELambda (param, body, _) ->
                let annotation = param.Annotation |> Option.map typeRefToString
                mkBinding param.Name param.Span (Ast.spanOfExpr body) annotation
                :: collectExprBindings body
            | EFor (name, source, body, span) ->
                mkBinding name span (Ast.spanOfExpr body) None
                :: (collectExprBindings source @ collectExprBindings body)
            | EMatch (scrutinee, cases, _) ->
                let inScrutinee = collectExprBindings scrutinee
                let inCases =
                    cases
                    |> List.collect (fun (pat, guard, body, _) ->
                        let scope = Ast.spanOfExpr body
                        let fromPattern = collectPatternBindings scope pat
                        let fromGuard =
                            match guard with
                            | Some g -> collectExprBindings g
                            | None -> []
                        fromPattern @ fromGuard @ collectExprBindings body)
                inScrutinee @ inCases
            | ELet (name, value, body, _, span) ->
                mkBinding name span (Ast.spanOfExpr body) None
                :: (collectExprBindings value @ collectExprBindings body)
            | ELetRecGroup (bindings, body, _) ->
                let fromBindings =
                    bindings
                    |> List.collect (fun (name, args, valueExpr, bindingSpan) ->
                        let argBindings =
                            args
                            |> List.map (fun p ->
                                let annotation = p.Annotation |> Option.map typeRefToString
                                mkBinding p.Name p.Span (Ast.spanOfExpr valueExpr) annotation)
                        mkBinding name bindingSpan (Ast.spanOfExpr body) None
                        :: (argBindings @ collectExprBindings valueExpr))
                fromBindings @ collectExprBindings body
            | EApply (f, a, _) ->
                collectExprBindings f @ collectExprBindings a
            | EIf (c, t, f, _) ->
                collectExprBindings c @ collectExprBindings t @ collectExprBindings f
            | ERaise (inner, _)
            | ESome (inner, _)
            | EParen (inner, _) ->
                collectExprBindings inner
            | EList (items, _)
            | ETuple (items, _) ->
                items |> List.collect collectExprBindings
            | ERange (a, b, _) ->
                collectExprBindings a @ collectExprBindings b
            | ERecord (fields, _)
            | EStructuralRecord (fields, _) ->
                fields |> List.collect (fun (_, e) -> collectExprBindings e)
            | EMap (entries, _) ->
                entries
                |> List.collect (function
                    | MEKeyValue (k, v) -> collectExprBindings k @ collectExprBindings v
                    | MESpread e -> collectExprBindings e)
            | ERecordUpdate (target, fields, _)
            | EStructuralRecordUpdate (target, fields, _) ->
                collectExprBindings target @ (fields |> List.collect (fun (_, e) -> collectExprBindings e))
            | EFieldGet (target, _, _) ->
                collectExprBindings target
            | EIndexGet (a, b, _)
            | ECons (a, b, _)
            | EAppend (a, b, _)
            | EBinOp (_, a, b, _) ->
                collectExprBindings a @ collectExprBindings b
            | EInterpolatedString (parts, _) ->
                parts
                |> List.collect (function
                    | IPText _ -> []
                    | IPExpr embedded -> collectExprBindings embedded)
            | EUnit _
            | ELiteral _
            | EVar _
            | ENone _
            | ETypeOf _
            | ENameOf _ -> []

        let fromTopLevelFunction (args: Param list) (body: Expr) =
            let argBindings =
                args
                |> List.map (fun p ->
                    let annotation = p.Annotation |> Option.map typeRefToString
                    mkBinding p.Name p.Span (Ast.spanOfExpr body) annotation)
            argBindings @ collectExprBindings body

        program
        |> List.collect (fun stmt ->
            match stmt with
            | SLet (_, args, body, _, _, _) ->
                fromTopLevelFunction args body
            | SLetRecGroup (bindings, _, _) ->
                bindings
                |> List.collect (fun (_, args, body, _) -> fromTopLevelFunction args body)
            | SExpr expr ->
                collectExprBindings expr
            | _ -> [])

    let private buildParameterTypeHints (program: Program) (typed: TypeInfer.TypedProgram option) =
        let typedByName = Dictionary<string, Type>(StringComparer.Ordinal)

        match typed with
        | Some typedProgram ->
            for stmt in typedProgram do
                match stmt with
                | TypeInfer.TSLet(name, _, t, _, _, _) ->
                    typedByName[name] <- t
                | TypeInfer.TSLetRecGroup(bindings, _, _) ->
                    for (name, _, t, _) in bindings do
                        typedByName[name] <- t
                | _ -> ()
        | None -> ()

        let rec collectLambdaParams (expr: Expr) =
            match expr with
            | ELambda (param, body, _) ->
                param :: collectLambdaParams body
            | EParen (inner, _) ->
                collectLambdaParams inner
            | _ ->
                []

        let rec takeParamTypes t count =
            if count <= 0 then []
            else
                match t with
                | TFun (arg, rest) ->
                    arg :: takeParamTypes rest (count - 1)
                | _ ->
                    []

        let emitHints (name: string) (parameters: Param list) =
            match typedByName.TryGetValue(name) with
            | true, t when not parameters.IsEmpty ->
                let argTypes = takeParamTypes t parameters.Length
                let keyDomainVars = collectMapKeyDomainVars t
                (parameters, argTypes)
                ||> List.zip
                |> List.choose (fun (param, argType) ->
                    if param.Annotation.IsSome then
                        None
                    else
                        Some (param.Span, $": {lspTypeToStringWithKeyDomainVars keyDomainVars argType}"))
            | _ ->
                []

        program
        |> List.collect (fun stmt ->
            match stmt with
            | SLet (name, args, expr, _, _, _) ->
                let allParams =
                    if args.IsEmpty then collectLambdaParams expr else args
                emitHints name allParams
            | SLetRecGroup (bindings, _, _) ->
                bindings
                |> List.collect (fun (name, args, expr, _) ->
                    let allParams =
                        if args.IsEmpty then collectLambdaParams expr else args
                    emitHints name allParams)
            | _ ->
                [])

    let private buildFunctionReturnTypeHints (program: Program) (typed: TypeInfer.TypedProgram option) =
        let typedByName = Dictionary<string, Type>(StringComparer.Ordinal)

        match typed with
        | Some typedProgram ->
            for stmt in typedProgram do
                match stmt with
                | TypeInfer.TSLet(name, _, t, _, _, _) ->
                    typedByName[name] <- t
                | TypeInfer.TSLetRecGroup(bindings, _, _) ->
                    for (name, _, t, _) in bindings do
                        typedByName[name] <- t
                | _ -> ()
        | None -> ()

        let rec collectLambdaParams (expr: Expr) =
            match expr with
            | ELambda (param, body, _) ->
                param :: collectLambdaParams body
            | EParen (inner, _) ->
                collectLambdaParams inner
            | _ ->
                []

        let rec takeReturnType t argCount =
            if argCount <= 0 then t
            else
                match t with
                | TFun (_, rest) -> takeReturnType rest (argCount - 1)
                | _ -> t

        let emitHint (name: string) (parameters: Param list) =
            if parameters.IsEmpty then
                None
            else
                match typedByName.TryGetValue(name) with
                | true, t ->
                    let returnType = takeReturnType t parameters.Length
                    let anchor = parameters[parameters.Length - 1].Span
                    Some (anchor, $": {lspTypeToString returnType}")
                | _ ->
                    None

        let fromLet =
            program
            |> List.choose (fun stmt ->
                match stmt with
                | SLet (name, args, expr, _, _, _) ->
                    let allParams =
                        if args.IsEmpty then collectLambdaParams expr else args
                    emitHint name allParams
                | _ ->
                    None)

        let fromLetRec =
            program
            |> List.collect (fun stmt ->
                match stmt with
                | SLetRecGroup (bindings, _, _) ->
                    bindings
                    |> List.choose (fun (name, args, expr, _) ->
                        let allParams =
                            if args.IsEmpty then collectLambdaParams expr else args
                        emitHint name allParams)
                | _ ->
                    [])

        fromLet @ fromLetRec

    let private collectPatternVariableSpans (program: Program) =
        let rec collectPattern (acc: (string * Span) list) (pattern: Pattern) =
            match pattern with
            | PVar (name, span) ->
                (name, span) :: acc
            | PCons (head, tail, _) ->
                collectPattern (collectPattern acc head) tail
            | PTuple (items, _) ->
                items |> List.fold collectPattern acc
            | PRecord (fields, _) ->
                fields |> List.fold (fun state (_, p) -> collectPattern state p) acc
            | PMap (clauses, tailOpt, _) ->
                let withClauses =
                    clauses
                    |> List.fold (fun state (k, v) ->
                        let withKey = collectPattern state k
                        collectPattern withKey v) acc
                match tailOpt with
                | Some tail -> collectPattern withClauses tail
                | None -> withClauses
            | PSome (inner, _) ->
                collectPattern acc inner
            | PUnionCase (_, _, payload, _) ->
                match payload with
                | Some p -> collectPattern acc p
                | None -> acc
            | PWildcard _
            | PLiteral _
            | PNil _
            | PNone _
            | PTypeRef _ -> acc

        let rec collectExpr (acc: (string * Span) list) (expr: Expr) =
            match expr with
            | EMatch (scrutinee, cases, _) ->
                let withScrutinee = collectExpr acc scrutinee
                cases
                |> List.fold (fun state (pat, guard, body, _) ->
                    let withPattern = collectPattern state pat
                    let withGuard =
                        match guard with
                        | Some g -> collectExpr withPattern g
                        | None -> withPattern
                    collectExpr withGuard body) withScrutinee
            | ELambda (_, body, _) ->
                collectExpr acc body
            | EApply (f, a, _) ->
                collectExpr (collectExpr acc f) a
            | EIf (c, t, f, _) ->
                collectExpr (collectExpr (collectExpr acc c) t) f
            | ERaise (inner, _) ->
                collectExpr acc inner
            | EFor (_, source, body, _) ->
                collectExpr (collectExpr acc source) body
            | ELet (_, value, body, _, _) ->
                collectExpr (collectExpr acc value) body
            | ELetRecGroup (bindings, body, _) ->
                let withBindings =
                    bindings |> List.fold (fun state (_, _, value, _) -> collectExpr state value) acc
                collectExpr withBindings body
            | EList (items, _)
            | ETuple (items, _) ->
                items |> List.fold collectExpr acc
            | ERange (startExpr, endExpr, _) ->
                collectExpr (collectExpr acc startExpr) endExpr
            | ERecord (fields, _)
            | EStructuralRecord (fields, _) ->
                fields |> List.fold (fun state (_, value) -> collectExpr state value) acc
            | EMap (entries, _) ->
                entries
                |> List.fold (fun state entry ->
                    match entry with
                    | MEKeyValue (k, v) ->
                        collectExpr (collectExpr state k) v
                    | MESpread spread ->
                        collectExpr state spread) acc
            | ERecordUpdate (baseExpr, fields, _)
            | EStructuralRecordUpdate (baseExpr, fields, _) ->
                let withBase = collectExpr acc baseExpr
                fields |> List.fold (fun state (_, value) -> collectExpr state value) withBase
            | EFieldGet (target, _, _) ->
                collectExpr acc target
            | EIndexGet (target, key, _)
            | ECons (target, key, _)
            | EAppend (target, key, _)
            | EBinOp (_, target, key, _) ->
                collectExpr (collectExpr acc target) key
            | ESome (inner, _)
            | EParen (inner, _) ->
                collectExpr acc inner
            | EInterpolatedString (parts, _) ->
                parts
                |> List.fold (fun state part ->
                    match part with
                    | IPText _ -> state
                    | IPExpr embedded -> collectExpr state embedded) acc
            | EUnit _
            | ELiteral _
            | EVar _
            | ENone _
            | ETypeOf _
            | ENameOf _ -> acc

        program
        |> List.fold (fun state stmt ->
            match stmt with
            | SLet (_, _, expr, _, _, _) ->
                collectExpr state expr
            | SLetRecGroup (bindings, _, _) ->
                bindings |> List.fold (fun inner (_, _, expr, _) -> collectExpr inner expr) state
            | SExpr expr ->
                collectExpr state expr
            | _ -> state) []
        |> List.rev

    let private buildPatternTypeHints (program: Program) (localTypes: TypeInfer.LocalVariableTypeInfo list) =
        let localByNameAndSpan =
            localTypes
            |> List.map (fun entry -> (entry.Name, entry.Span.Start.Line, entry.Span.Start.Column, entry.Span.End.Line, entry.Span.End.Column), entry.Type)
            |> Map.ofList

        collectPatternVariableSpans program
        |> List.choose (fun (name, span) ->
            let key = (name, span.Start.Line, span.Start.Column, span.End.Line, span.End.Column)
            localByNameAndSpan
            |> Map.tryFind key
            |> Option.map (fun t -> span, $": {lspTypeToString t}"))

    let private buildCallArgumentHints (program: Program) (functionParameters: Map<string, string list>) =
        let tryParameterNames (name: string) =
            match functionParameters |> Map.tryFind name with
            | Some names -> Some names
            | None ->
                let segments = name.Split('.')
                if segments.Length > 1 then
                    functionParameters |> Map.tryFind segments[segments.Length - 1]
                else
                    None

        let rec decomposeApply (expr: Expr) (argsRev: Expr list) =
            match expr with
            | EApply (fn, arg, _) -> decomposeApply fn (arg :: argsRev)
            | _ -> expr, (argsRev |> List.rev)

        let tryCalledName (expr: Expr) =
            match expr with
            | EVar (name, _) -> Some name
            | _ -> None

        let rec collectExpr (acc: (Span * string) list) (isApplySpineParent: bool) (expr: Expr) =
            let withChildren =
                match expr with
                | EApply (f, a, _) ->
                    collectExpr (collectExpr acc true f) false a
                | EIf (c, t, f, _) ->
                    collectExpr (collectExpr (collectExpr acc false c) false t) false f
                | ERaise (inner, _) ->
                    collectExpr acc false inner
                | EFor (_, source, body, _) ->
                    collectExpr (collectExpr acc false source) false body
                | EMatch (scrutinee, cases, _) ->
                    let withScrutinee = collectExpr acc false scrutinee
                    cases
                    |> List.fold (fun state (_, guard, body, _) ->
                        let withGuard =
                            match guard with
                            | Some g -> collectExpr state false g
                            | None -> state
                        collectExpr withGuard false body) withScrutinee
                | ELet (_, value, body, _, _) ->
                    collectExpr (collectExpr acc false value) false body
                | ELetRecGroup (bindings, body, _) ->
                    let withBindings =
                        bindings |> List.fold (fun state (_, _, value, _) -> collectExpr state false value) acc
                    collectExpr withBindings false body
                | ELambda (_, body, _) ->
                    collectExpr acc false body
                | EList (items, _)
                | ETuple (items, _) ->
                    items |> List.fold (fun state item -> collectExpr state false item) acc
                | ERange (startExpr, endExpr, _) ->
                    collectExpr (collectExpr acc false startExpr) false endExpr
                | ERecord (fields, _)
                | EStructuralRecord (fields, _) ->
                    fields |> List.fold (fun state (_, value) -> collectExpr state false value) acc
                | EMap (entries, _) ->
                    entries
                    |> List.fold (fun state entry ->
                        match entry with
                        | MEKeyValue (k, v) ->
                            collectExpr (collectExpr state false k) false v
                        | MESpread spread ->
                            collectExpr state false spread) acc
                | ERecordUpdate (baseExpr, fields, _)
                | EStructuralRecordUpdate (baseExpr, fields, _) ->
                    let withBase = collectExpr acc false baseExpr
                    fields |> List.fold (fun state (_, value) -> collectExpr state false value) withBase
                | EFieldGet (target, _, _) ->
                    collectExpr acc false target
                | EIndexGet (target, key, _)
                | ECons (target, key, _)
                | EAppend (target, key, _)
                | EBinOp (_, target, key, _) ->
                    collectExpr (collectExpr acc false target) false key
                | ESome (inner, _)
                | EParen (inner, _) ->
                    collectExpr acc false inner
                | EInterpolatedString (parts, _) ->
                    parts
                    |> List.fold (fun state part ->
                        match part with
                        | IPText _ -> state
                        | IPExpr embedded -> collectExpr state false embedded) acc
                | EUnit _
                | ELiteral _
                | EVar _
                | ENone _
                | ETypeOf _
                | ENameOf _ -> acc

            if isApplySpineParent then
                withChildren
            else
                match expr with
                | EApply _ ->
                    let calledExpr, callArgs = decomposeApply expr []
                    match tryCalledName calledExpr |> Option.bind tryParameterNames with
                    | Some parameterNames ->
                        let normalizedArgs =
                            match callArgs with
                            | [ ETuple (items, _) ] when parameterNames.Length > 1 && items.Length > 1 -> items
                            | _ -> callArgs
                        let count = min normalizedArgs.Length parameterNames.Length
                        [ 0 .. count - 1 ]
                        |> List.fold (fun state index ->
                            (Ast.spanOfExpr normalizedArgs[index], $"{parameterNames[index]}:") :: state) withChildren
                    | None ->
                        withChildren
                | _ ->
                    withChildren

        program
        |> List.fold (fun state stmt ->
            match stmt with
            | SLet (_, _, expr, _, _, _) ->
                collectExpr state false expr
            | SLetRecGroup (bindings, _, _) ->
                bindings |> List.fold (fun inner (_, _, expr, _) -> collectExpr inner false expr) state
            | SExpr expr ->
                collectExpr state false expr
            | _ ->
                state) []
        |> List.rev

    let analyzeDocument (uri: string) (text: string) =
        let sourceName =
            if uri.StartsWith("file://", StringComparison.OrdinalIgnoreCase) then
                Uri(uri).LocalPath
            else
                uri
        let runtimeExterns = LspRuntimeExterns.forSourcePath sourceName

        let diagnostics = ResizeArray<JsonNode>()
        let mutable symbols : TopLevelSymbol list = []
        let mutable occurrences : Map<string, Span list> = Map.empty
        let mutable recordParamFields : Map<string, (string * string) list> = Map.empty
        let mutable parameterTypeTargets : Map<string, string> = Map.empty
        let mutable functionParameters : Map<string, string list> = Map.empty
        let mutable functionAnnotationTypes : Map<string, string list> = Map.empty
        let mutable functionDeclaredReturnTargets : Map<string, string> = Map.empty
        let mutable callArgumentHints : (Span * string) list = []
        let mutable functionReturnTypeHints : (Span * string) list = []
        let mutable parameterTypeHints : (Span * string) list = []
        let mutable patternTypeHints : (Span * string) list = []
        let mutable localVariableTypeHints : (Span * string * string) list = []
        let mutable localBindings : LocalBindingInfo list = []
        let mutable injectedFunctionSignatures : Map<string, string> = Map.empty
        let mutable injectedFunctionParameterNames : Map<string, string list> = Map.empty
        let mutable injectedFunctionDefinitions : Map<string, (string * Span)> = Map.empty

        let mutable parsedProgram : Program option = None

        try
            let program =
                if uri.StartsWith("file://", StringComparison.OrdinalIgnoreCase) then
                    InteropServices.parseProgramFromSourceWithIncludes sourceName text
                else
                    FScript.parseWithSourceName (Some sourceName) text
            parsedProgram <- Some program
            let signatures, parameterNames, definitions = buildInjectedFunctionData runtimeExterns
            injectedFunctionSignatures <- signatures
            injectedFunctionParameterNames <- parameterNames
            injectedFunctionDefinitions <- definitions
            occurrences <- collectVariableOccurrences program
            recordParamFields <- buildRecordParameterFields program
            parameterTypeTargets <- buildParameterTypeTargets program
            functionParameters <- buildFunctionParameters program
            functionAnnotationTypes <- buildFunctionAnnotationTypes program
            functionDeclaredReturnTargets <- buildFunctionDeclaredReturnTargets program
            callArgumentHints <- buildCallArgumentHints program functionParameters
            localBindings <- buildLocalBindings program
            try
                let typed, localTypes = InteropServices.inferProgramWithExternsAndLocalVariableTypes runtimeExterns program
                symbols <- buildSymbolsFromProgram program (Some typed)
                parameterTypeHints <- buildParameterTypeHints program (Some typed)
                functionReturnTypeHints <- buildFunctionReturnTypeHints program (Some typed)
                patternTypeHints <- buildPatternTypeHints program localTypes
                localVariableTypeHints <-
                    localTypes
                    |> List.filter (fun entry ->
                        match entry.Span.Start.File with
                        | Some file -> String.Equals(file, sourceName, StringComparison.OrdinalIgnoreCase)
                        | None -> true)
                    |> List.map (fun entry -> entry.Span, entry.Name, lspTypeToString entry.Type)
            with
            | TypeException err ->
                diagnostics.Add(diagnostic 1 "type" err.Span err.Message)
                symbols <- buildSymbolsFromProgram program None
                let bestEffortTypes = inferTopLevelTypesBestEffort runtimeExterns program
                let bestEffortLocalTypes = inferLocalVariableTypesBestEffort runtimeExterns program
                let localTypesFromReturnedRecords =
                    inferLocalTypesFromReturnedRecordFields program functionDeclaredReturnTargets localBindings
                let resolveTypeTarget = buildTopLevelTypeTargetFromProgram program
                symbols <-
                    symbols
                    |> List.map (fun sym ->
                        match bestEffortTypes |> Map.tryFind sym.Name with
                        | Some t ->
                            { sym with
                                Kind = symbolKindForType t
                                TypeText = Some (lspTypeToString t)
                                TypeTargetName = resolveTypeTarget t }
                        | None -> sym)
                parameterTypeHints <- buildParameterTypeHints program None
                functionReturnTypeHints <- []
                patternTypeHints <- buildPatternTypeHints program bestEffortLocalTypes
                let baseLocalHints =
                    bestEffortLocalTypes
                    |> List.filter (fun entry ->
                        match entry.Span.Start.File with
                        | Some file -> String.Equals(file, sourceName, StringComparison.OrdinalIgnoreCase)
                        | None -> true)
                    |> List.map (fun entry -> entry.Span, entry.Name, lspTypeToString entry.Type)

                let refinedLocalHints =
                    localTypesFromReturnedRecords
                    |> List.filter (fun (span, _, _) ->
                        match span.Start.File with
                        | Some file -> String.Equals(file, sourceName, StringComparison.OrdinalIgnoreCase)
                        | None -> true)

                let keyOf (span: Span, name: string, _) =
                    let file = span.Start.File |> Option.defaultValue ""
                    name, span.Start.Line, span.Start.Column, span.End.Line, span.End.Column, file

                let merged = Dictionary<string * int * int * int * int * string, Span * string * string>()

                for hint in baseLocalHints do
                    merged[keyOf hint] <- hint

                for hint in refinedLocalHints do
                    let key = keyOf hint
                    match merged.TryGetValue(key) with
                    | true, (_, _, existingType) when existingType.Contains("unknown", StringComparison.Ordinal) ->
                        merged[key] <- hint
                    | false, _ ->
                        merged[key] <- hint
                    | _ ->
                        ()

                localVariableTypeHints <- merged.Values |> Seq.toList

        with
        | ParseException err ->
            diagnostics.Add(diagnostic 1 "parse" err.Span err.Message)

        documents[uri] <-
            { Text = text
              Symbols = symbols
              RecordParameterFields = recordParamFields
              ParameterTypeTargets = parameterTypeTargets
              FunctionParameters = functionParameters
              FunctionAnnotationTypes = functionAnnotationTypes
              FunctionDeclaredReturnTargets = functionDeclaredReturnTargets
              CallArgumentHints = callArgumentHints
              FunctionReturnTypeHints = functionReturnTypeHints
              ParameterTypeHints = parameterTypeHints
              PatternTypeHints = patternTypeHints
              LocalVariableTypeHints = localVariableTypeHints
              LocalBindings = localBindings
              InjectedFunctionSignatures = injectedFunctionSignatures
              InjectedFunctionParameterNames = injectedFunctionParameterNames
              InjectedFunctionDefinitions = injectedFunctionDefinitions
              VariableOccurrences = occurrences }
        publishDiagnostics uri (diagnostics |> Seq.toList)

    let tryResolveSymbol (doc: DocumentState) (line: int) (character: int) : TopLevelSymbol option =
        match tryGetWordAtPosition doc.Text line character with
        | None -> None
        | Some word ->
            let candidates =
                if word.Contains('.') then
                    let segments = word.Split('.') |> Array.toList
                    word :: segments
                else
                    [ word ]
                |> List.distinct

            candidates
            |> List.tryPick (fun candidate ->
                doc.Symbols |> List.tryFind (fun s -> s.Name = candidate))

    let private splitTopLevelSemicolons (text: string) =
        let parts = ResizeArray<string>()
        let mutable depthParen = 0
        let mutable depthBrace = 0
        let mutable depthBracket = 0
        let mutable start = 0

        for i = 0 to text.Length - 1 do
            match text[i] with
            | '(' -> depthParen <- depthParen + 1
            | ')' -> if depthParen > 0 then depthParen <- depthParen - 1
            | '{' -> depthBrace <- depthBrace + 1
            | '}' -> if depthBrace > 0 then depthBrace <- depthBrace - 1
            | '[' -> depthBracket <- depthBracket + 1
            | ']' -> if depthBracket > 0 then depthBracket <- depthBracket - 1
            | ';' when depthParen = 0 && depthBrace = 0 && depthBracket = 0 ->
                let chunk = text.Substring(start, i - start).Trim()
                if chunk <> "" then
                    parts.Add(chunk)
                start <- i + 1
            | _ -> ()

        if start <= text.Length then
            let tail = text.Substring(start).Trim()
            if tail <> "" then
                parts.Add(tail)

        parts |> Seq.toList

    let private tryParseRecordFields (typeText: string) =
        let trimmed = typeText.Trim()
        if trimmed.StartsWith("{", StringComparison.Ordinal) && trimmed.EndsWith("}", StringComparison.Ordinal) then
            let inner = trimmed.Substring(1, trimmed.Length - 2).Trim()
            if inner = "" then
                Some []
            else
                let fields =
                    splitTopLevelSemicolons inner
                    |> List.choose (fun part ->
                        let idx = part.IndexOf(':')
                        if idx <= 0 then None
                        else
                            let name = part.Substring(0, idx).Trim()
                            let fieldType = part.Substring(idx + 1).Trim()
                            if name = "" || fieldType = "" then None else Some (name, fieldType))

                if fields.IsEmpty then None else Some fields
        else
            None

    let private tryRecordFieldsForQualifier (doc: DocumentState) (qualifier: string) =
        let normalized =
            if qualifier.Contains('.') then qualifier.Split('.') |> Array.last
            else qualifier

        let sym =
            doc.Symbols
            |> List.tryFind (fun s -> s.Name = qualifier || s.Name = normalized)

        let tryFromTypeName (typeName: string) =
            doc.Symbols
            |> List.tryFind (fun s -> s.Kind = 5 && s.Name = typeName)
            |> Option.bind (fun s -> s.TypeText)
            |> Option.bind tryParseRecordFields

        match sym with
        | None -> None
        | Some s ->
            match s.TypeText |> Option.bind tryParseRecordFields with
            | Some fields -> Some fields
            | None ->
                match s.TypeTargetName with
                | Some typeName ->
                    tryFromTypeName typeName
                | None ->
                    s.TypeText |> Option.bind tryFromTypeName
        |> function
            | Some fields -> Some fields
            | None -> doc.RecordParameterFields |> Map.tryFind normalized

    let private tryResolveNamedRecordTypeByFields (doc: DocumentState) (fields: (string * string) list) =
        let wanted = canonicalRecordSignatureFromFields fields
        let candidates =
            doc.Symbols
            |> List.choose (fun s ->
                if s.Kind = 5 then
                    s.TypeText
                    |> Option.bind tryParseRecordFields
                    |> Option.map (fun typeFields -> s.Name, canonicalRecordSignatureFromFields typeFields)
                else
                    None)
            |> List.choose (fun (name, signature) -> if signature = wanted then Some name else None)
            |> List.distinct
        match candidates with
        | [ one ] -> Some one
        | _ -> None

    let private tryResolveTypeNameForQualifier (doc: DocumentState) (qualifier: string) =
        let normalized =
            if qualifier.Contains('.') then qualifier.Split('.') |> Array.last
            else qualifier

        let fromSymbol =
            doc.Symbols
            |> List.tryFind (fun s -> s.Name = qualifier || s.Name = normalized)
            |> Option.bind (fun s ->
                match s.TypeTargetName with
                | Some t -> Some t
                | None ->
                    s.TypeText
                    |> Option.bind tryParseRecordFields
                    |> Option.bind (tryResolveNamedRecordTypeByFields doc))

        match fromSymbol with
        | Some typeName -> Some typeName
        | None -> doc.ParameterTypeTargets |> Map.tryFind normalized

    let private tryFindSymbolByName (doc: DocumentState) (name: string) =
        let normalized =
            if name.Contains('.') then name.Split('.') |> Array.last
            else name
        doc.Symbols
        |> List.tryFind (fun s -> s.Name = name || s.Name = normalized)

    let private tryResolveTypeNameFromSymbol (doc: DocumentState) (symbol: TopLevelSymbol) =
        match symbol.TypeTargetName with
        | Some typeName -> Some typeName
        | None ->
            symbol.TypeText
            |> Option.bind tryParseRecordFields
            |> Option.bind (tryResolveNamedRecordTypeByFields doc)

    let private trimWrappingParens (input: string) =
        let mutable value = input.Trim()
        let mutable changed = true
        while changed && value.Length >= 2 && value[0] = '(' && value[value.Length - 1] = ')' do
            changed <- false
            let mutable depth = 0
            let mutable wraps = true
            for i = 0 to value.Length - 1 do
                match value[i] with
                | '(' -> depth <- depth + 1
                | ')' ->
                    depth <- depth - 1
                    if depth = 0 && i < value.Length - 1 then
                        wraps <- false
                | _ -> ()
            if wraps then
                value <- value.Substring(1, value.Length - 2).Trim()
                changed <- true
        value

    let private tryFirstArgumentTypeNameFromFunctionSymbol (doc: DocumentState) (symbol: TopLevelSymbol) =
        let tryResolveTypeName (t: string) =
            let trimmed = trimWrappingParens t
            match doc.Symbols |> List.tryFind (fun s -> s.Kind = 5 && s.Name = trimmed) with
            | Some _ -> Some trimmed
            | None ->
                tryParseRecordFields trimmed
                |> Option.bind (tryResolveNamedRecordTypeByFields doc)

        symbol.TypeText
        |> Option.bind (fun t ->
            let normalizedTypeText = trimWrappingParens t
            let arrowIndex = normalizedTypeText.IndexOf("->", StringComparison.Ordinal)
            if arrowIndex <= 0 then None
            else
                let firstArg = normalizedTypeText.Substring(0, arrowIndex).Trim()
                tryResolveTypeName firstArg)

    let private tryResolveRecordLiteralCallArgTypeTarget (doc: DocumentState) (line: int) (character: int) =
        match getLineText doc.Text line with
        | Some lineText ->
            let pos = max 0 (min character lineText.Length)
            let leftBrace = lineText.LastIndexOf('{', max 0 (pos - 1))
            let rightBrace = if pos < lineText.Length then lineText.IndexOf('}', pos) else -1
            if leftBrace < 0 || rightBrace <= leftBrace then
                None
            else
                let segment = lineText.Substring(leftBrace, rightBrace - leftBrace + 1)
                if not (segment.Contains('='))
                   || segment.Contains(':') then
                    None
                else
                    let callPrefix = lineText.Substring(0, leftBrace).TrimEnd()
                    if String.IsNullOrWhiteSpace(callPrefix) then
                        None
                    else
                        let mutable finish = callPrefix.Length - 1
                        while finish >= 0 && Char.IsWhiteSpace(callPrefix[finish]) do
                            finish <- finish - 1
                        let mutable start = finish
                        while start >= 0 && isWordChar callPrefix[start] do
                            start <- start - 1
                        let tokenStart = start + 1
                        if tokenStart > finish then
                            None
                        else
                            let callTarget = callPrefix.Substring(tokenStart, finish - tokenStart + 1)
                            tryFindSymbolByName doc callTarget
                            |> Option.bind (tryFirstArgumentTypeNameFromFunctionSymbol doc)
        | None -> None

    let private tryResolveRecordLiteralBindingTypeTarget (doc: DocumentState) (line: int) (character: int) =
        match getLineText doc.Text line with
        | Some lineText ->
            let pos = max 0 (min character lineText.Length)
            let leftBrace = lineText.LastIndexOf('{', max 0 (pos - 1))
            let rightBrace = if pos < lineText.Length then lineText.IndexOf('}', pos) else -1
            if leftBrace < 0 || rightBrace <= leftBrace then
                None
            else
                let segment = lineText.Substring(leftBrace, rightBrace - leftBrace + 1)
                if not (segment.Contains('='))
                   || segment.Contains(':') then
                    None
                else
                    let prefix = lineText.Substring(0, leftBrace).TrimEnd()
                    let eqIndex = prefix.LastIndexOf('=')
                    if eqIndex <= 0 then
                        None
                    else
                        let lhs = prefix.Substring(0, eqIndex).Trim()
                        if not (lhs.StartsWith("let ", StringComparison.Ordinal)) then
                            None
                        else
                            let afterLet = lhs.Substring(4).Trim()
                            if String.IsNullOrWhiteSpace(afterLet) then
                                None
                            else
                                let mutable idx = 0
                                while idx < afterLet.Length && isWordChar afterLet[idx] do
                                    idx <- idx + 1
                                if idx = 0 then
                                    None
                                else
                                    let bindingName = afterLet.Substring(0, idx)
                                    tryFindSymbolByName doc bindingName
                                    |> Option.bind (tryResolveTypeNameFromSymbol doc)
        | None -> None

    let private tryResolveRecordLiteralFunctionReturnTypeTarget (doc: DocumentState) (line: int) (character: int) =
        match getLineText doc.Text line with
        | None -> None
        | Some lineText ->
            let pos = max 0 (min character lineText.Length)
            let leftBrace = lineText.LastIndexOf('{', max 0 (pos - 1))
            let rightBrace = if pos < lineText.Length then lineText.IndexOf('}', pos) else -1
            if leftBrace < 0 || rightBrace <= leftBrace then
                None
            else
                let segment = lineText.Substring(leftBrace, rightBrace - leftBrace + 1)
                if not (segment.Contains('='))
                   || segment.Contains(':') then
                    None
                else
                    let isInsideSpan (span: Span) =
                        let line1 = line + 1
                        let col1 = character + 1
                        let startsBefore =
                            line1 > span.Start.Line
                            || (line1 = span.Start.Line && col1 >= span.Start.Column)
                        let endsAfter =
                            line1 < span.End.Line
                            || (line1 = span.End.Line && col1 <= span.End.Column)
                        startsBefore && endsAfter

                    let spansContainingPosition =
                        doc.Symbols
                        |> List.filter (fun sym ->
                            sym.Kind = 12
                            && isInsideSpan sym.Span)
                        |> List.sortByDescending (fun sym -> sym.Span.Start.Line, sym.Span.Start.Column)

                    spansContainingPosition
                    |> List.tryPick (fun sym ->
                        match sym.TypeTargetName with
                        | Some typeName -> Some typeName
                        | None ->
                            doc.FunctionDeclaredReturnTargets
                            |> Map.tryFind sym.Name)

    let private tryExtractInlineRecordAnnotationAtPosition (doc: DocumentState) (line: int) (character: int) =
        match getLineText doc.Text line with
        | None -> None
        | Some lineText ->
            let pos = max 0 (min character lineText.Length)
            let left = lineText.LastIndexOf('{', max 0 (pos - 1))
            let right = if pos < lineText.Length then lineText.IndexOf('}', pos) else -1
            if left < 0 || right <= left then
                None
            else
                let segment = lineText.Substring(left, right - left + 1)
                if segment.Contains(':', StringComparison.Ordinal) && not (segment.Contains('=', StringComparison.Ordinal)) then
                    tryParseRecordFields segment
                else
                    None

    let tryResolveTypeTargetAtPosition (doc: DocumentState) (line: int) (character: int) : string option =
        let fromWord =
            match tryGetWordAtPosition doc.Text line character with
            | Some word when word.Contains('.') ->
                let idx = word.LastIndexOf('.')
                if idx > 0 then
                    let qualifier = word.Substring(0, idx)
                    tryResolveTypeNameForQualifier doc qualifier
                else
                    None
            | Some word ->
                tryResolveTypeNameForQualifier doc word
            | None -> None

        match fromWord with
        | Some typeName -> Some typeName
        | None ->
            match tryExtractInlineRecordAnnotationAtPosition doc line character with
            | Some fields ->
                tryResolveNamedRecordTypeByFields doc fields
            | None ->
                match tryResolveRecordLiteralCallArgTypeTarget doc line character with
                | Some t -> Some t
                | None ->
                    match tryResolveRecordLiteralBindingTypeTarget doc line character with
                    | Some t -> Some t
                    | None -> tryResolveRecordLiteralFunctionReturnTypeTarget doc line character

    let tryGetRecordFieldHoverInfo (doc: DocumentState) (line: int) (character: int) : (string * string) option =
        match tryGetWordAtPosition doc.Text line character with
        | Some word when word.Contains('.') ->
            let idx = word.LastIndexOf('.')
            if idx <= 0 || idx + 1 >= word.Length then
                None
            else
                let qualifier = word.Substring(0, idx)
                let fieldName = word.Substring(idx + 1)
                tryRecordFieldsForQualifier doc qualifier
                |> Option.bind (fun fields ->
                    fields
                    |> List.tryFind (fun (name, _) -> name = fieldName))
        | _ ->
            None

    let private spanContainsPosition (span: Span) (line: int) (character: int) =
        let line1 = line + 1
        let col1 = character + 1
        let startsBefore =
            line1 > span.Start.Line
            || (line1 = span.Start.Line && col1 >= span.Start.Column)
        let endsAfter =
            line1 < span.End.Line
            || (line1 = span.End.Line && col1 <= span.End.Column)
        startsBefore && endsAfter

    let tryGetLocalVariableHoverInfo (doc: DocumentState) (line: int) (character: int) : (string * string) option =
        let bySpan =
            doc.LocalVariableTypeHints
            |> List.tryFind (fun (span, _, _) -> spanContainsPosition span line character)
            |> Option.map (fun (_, name, typeText) -> name, typeText)

        match bySpan with
        | Some _ -> bySpan
        | None ->
            match tryGetWordAtPosition doc.Text line character with
            | Some word ->
                let isOnTopLevelSymbol =
                    doc.Symbols
                    |> List.exists (fun sym ->
                        String.Equals(sym.Name, word, StringComparison.Ordinal)
                        && spanContainsPosition sym.Span line character)

                if isOnTopLevelSymbol then
                    None
                else
                    let candidates =
                        doc.LocalBindings
                        |> List.filter (fun binding ->
                            String.Equals(binding.Name, word, StringComparison.Ordinal)
                            && (spanContainsPosition binding.ScopeSpan line character
                                || spanContainsPosition binding.DeclSpan line character))

                    let scoreBinding (binding: LocalBindingInfo) =
                        let line1 = line + 1
                        let col1 = character + 1
                        let lineDistance = abs (binding.DeclSpan.Start.Line - line1)
                        let colDistance = abs (binding.DeclSpan.Start.Column - col1)
                        let startsBefore =
                            binding.DeclSpan.Start.Line < line1
                            || (binding.DeclSpan.Start.Line = line1 && binding.DeclSpan.Start.Column <= col1)
                        if startsBefore then (0, lineDistance, colDistance) else (1, lineDistance, colDistance)

                    let nearestBinding =
                        candidates
                        |> List.sortBy scoreBinding
                        |> List.tryHead

                    let inferredTypeForBinding (binding: LocalBindingInfo) =
                        let byDeclSpan =
                            doc.LocalVariableTypeHints
                            |> List.tryFind (fun (span, name, _) ->
                                String.Equals(name, binding.Name, StringComparison.Ordinal)
                                && span.Start.Line = binding.DeclSpan.Start.Line
                                && span.Start.Column = binding.DeclSpan.Start.Column
                                && span.End.Line = binding.DeclSpan.End.Line
                                && span.End.Column = binding.DeclSpan.End.Column)
                            |> Option.map (fun (_, _, t) -> t)

                        match byDeclSpan with
                        | Some _ -> byDeclSpan
                        | None ->
                            doc.LocalVariableTypeHints
                            |> List.choose (fun (span, name, t) ->
                                if String.Equals(name, binding.Name, StringComparison.Ordinal)
                                   && spanContainsPosition binding.ScopeSpan (span.Start.Line - 1) (span.Start.Column - 1) then
                                    Some t
                                else
                                    None)
                            |> List.distinct
                            |> function
                                | [ one ] -> Some one
                                | _ -> None

                    nearestBinding
                    |> Option.map (fun binding ->
                        let typeText =
                            inferredTypeForBinding binding
                            |> Option.orElse binding.AnnotationType
                            |> Option.defaultValue "unknown"
                        binding.Name, typeText)
            | None ->
                None

    let private tryMemberCompletionItems (doc: DocumentState) (prefix: string option) =
        match prefix with
        | Some p when p.Contains('.') ->
            let idx = p.LastIndexOf('.')
            if idx <= 0 then None
            else
                let qualifier = p.Substring(0, idx)
                let memberPrefix =
                    if idx + 1 < p.Length then p.Substring(idx + 1) else ""

                match tryRecordFieldsForQualifier doc qualifier with
                | Some fields ->
                    let filtered =
                        fields
                        |> List.filter (fun (name, _) ->
                            memberPrefix = "" || name.StartsWith(memberPrefix, StringComparison.Ordinal))

                    let items =
                        filtered
                        |> List.map (fun (name, fieldType) ->
                            let item = JsonObject()
                            item["label"] <- JsonValue.Create(name)
                            item["kind"] <- JsonValue.Create(10)
                            item["detail"] <- JsonValue.Create(fieldType)
                            item["filterText"] <- JsonValue.Create(name)
                            item["sortText"] <- JsonValue.Create($"0_{name}")
                            if memberPrefix = name then
                                item["preselect"] <- JsonValue.Create(true)
                            item :> JsonNode)
                        |> List.toArray

                    Some (JsonArray(items))
                | None ->
                    None
        | _ -> None

    let makeCompletionItems (doc: DocumentState) (prefix: string option) =
        match tryMemberCompletionItems doc prefix with
        | Some memberItems -> memberItems
        | None ->
            let symbols = doc.Symbols
            let keywords =
                [ "let"; "rec"; "and"; "if"; "then"; "elif"; "else"; "match"; "with"; "when"
                  "for"; "in"; "do"; "type"; "module"; "true"; "false"; "None"; "Some" ]

            let keywordItems =
                keywords
                |> List.map (fun kw ->
                    let item = JsonObject()
                    item["label"] <- JsonValue.Create(kw)
                    item["kind"] <- JsonValue.Create(14)
                    item["filterText"] <- JsonValue.Create(kw)
                    item["sortText"] <- JsonValue.Create($"9_{kw}")
                    match prefix with
                    | Some p when p = kw -> item["preselect"] <- JsonValue.Create(true)
                    | _ -> ()
                    item)

            let namePool =
                [ for s in symbols -> s.Name
                  for kv in doc.InjectedFunctionSignatures -> kv.Key
                  yield! stdlibNames
                  yield! builtinNames ]
                |> List.distinct

            let filteredNames =
                namePool
                |> List.filter (fun name ->
                    match prefix with
                    | Some p when p <> "" ->
                        if p.Contains('.') then
                            // When user types a dotted qualifier, avoid flooding with unrelated names.
                            name.StartsWith(p, StringComparison.Ordinal)
                        else
                            name.StartsWith(p, StringComparison.Ordinal)
                    | _ -> true)
                |> List.sortBy (fun name ->
                    let localPriority =
                        if name.Contains('.') then 1 else 0
                    (localPriority, name.Length, name))

            let symbolItems =
                filteredNames
                |> List.map (fun name ->
                    let symbolType = symbols |> List.tryFind (fun s -> s.Name = name) |> Option.bind (fun s -> s.TypeText)
                    let injectedType = doc.InjectedFunctionSignatures |> Map.tryFind name
                    let kind =
                        match symbols |> List.tryFind (fun s -> s.Name = name) with
                        | Some s -> s.Kind
                        | None ->
                            if doc.InjectedFunctionSignatures.ContainsKey(name) then 12 else 3
                    let item = JsonObject()
                    item["label"] <- JsonValue.Create(name)
                    item["kind"] <- JsonValue.Create(kind)
                    item["filterText"] <- JsonValue.Create(name)
                    let sortPrefix = if name.Contains('.') then "1" else "0"
                    item["sortText"] <- JsonValue.Create($"{sortPrefix}_{name}")
                    match prefix with
                    | Some p when p = name -> item["preselect"] <- JsonValue.Create(true)
                    | _ -> ()
                    match symbolType with
                    | Some t -> item["detail"] <- JsonValue.Create(t)
                    | None ->
                        match injectedType with
                        | Some t -> item["detail"] <- JsonValue.Create(t)
                        | None -> ()
                    item)

            let rankedItems =
                match prefix with
                | Some p when p <> "" ->
                    let keywordMatches =
                        keywordItems
                        |> List.filter (fun item ->
                            match item["label"] with
                            | :? JsonValue as label ->
                                try
                                    let kw = label.GetValue<string>()
                                    if p.Contains('.') then false
                                    else kw.StartsWith(p, StringComparison.Ordinal)
                                with _ -> false
                            | _ -> false)
                    symbolItems @ keywordMatches
                | _ ->
                    keywordItems @ symbolItems

            let nodes = rankedItems |> List.map (fun n -> n :> JsonNode)
            JsonArray(nodes |> List.toArray)

    let tryGetCallTargetPrefixAtPosition (text: string) (line: int) (character: int) : string option =
        match getLineText text line with
        | None -> None
        | Some lineText ->
            if lineText.Length = 0 then None
            else
                let pos = max 0 (min character lineText.Length)
                let mutable idx = pos - 1

                while idx >= 0 && Char.IsWhiteSpace(lineText[idx]) do
                    idx <- idx - 1

                if idx < 0 then None
                else
                    let mutable finish = idx + 1
                    let mutable start = idx

                    while start >= 0 && isWordChar lineText[start] do
                        start <- start - 1

                    let tokenStart = start + 1
                    if tokenStart < finish then Some(lineText.Substring(tokenStart, finish - tokenStart)) else None

    let findSymbolRangesInText (text: string) (candidateNames: string list) =
        let names = candidateNames |> List.distinct |> Set.ofList
        let lines = text.Split('\n')

        lines
        |> Array.mapi (fun lineIndex rawLine ->
            let line = rawLine.TrimEnd('\r')
            let mutable i = 0
            let found = ResizeArray<Span>()

            while i < line.Length do
                if isWordChar line[i] then
                    let start = i
                    let mutable j = i + 1
                    while j < line.Length && isWordChar line[j] do
                        j <- j + 1

                    let token = line.Substring(start, j - start)
                    if names.Contains token then
                        let span =
                            Span.mk
                                (Span.pos (lineIndex + 1) (start + 1))
                                (Span.pos (lineIndex + 1) (j + 1))
                        found.Add(span)

                    i <- j
                else
                    i <- i + 1

            found |> Seq.toList)
        |> Array.toList
        |> List.concat
