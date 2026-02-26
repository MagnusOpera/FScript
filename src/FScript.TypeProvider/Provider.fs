namespace FScript.TypeProvider

open System
open System.IO
open System.Reflection
open System.Security.Cryptography
open System.Text
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open FScript.Language
open FScript.Runtime

[<TypeProvider>]
type FScriptScriptProvider(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config, addDefaultProbingLocation = true)

    let providerNamespace = "FScript.TypeProvider"
    let assembly = Assembly.GetExecutingAssembly()

    let normalizeParameterNames (names: string list) =
        let mutable seen = Set.empty
        names
        |> List.mapi (fun index rawName ->
            let baseName =
                if String.IsNullOrWhiteSpace(rawName) || rawName = "_" then
                    $"arg{index + 1}"
                else
                    rawName

            let rec ensureUnique candidate suffix =
                if seen.Contains(candidate) then
                    ensureUnique $"{baseName}_{suffix}" (suffix + 1)
                else
                    seen <- seen.Add(candidate)
                    candidate
            ensureUnique baseName 2)

    let buildContract
        (scriptPath: string)
        (rootDirectory: string)
        (externProviders: string)
        : string * FunctionContract list * string =
        let resolvedScriptPath =
            if Path.IsPathRooted(scriptPath) then
                Path.GetFullPath(scriptPath)
            else
                Path.GetFullPath(Path.Combine(config.ResolutionFolder, scriptPath))

        if not (File.Exists(resolvedScriptPath)) then
            failwith $"Script file not found: '{resolvedScriptPath}'."

        let resolvedRootDirectory =
            if String.IsNullOrWhiteSpace(rootDirectory) then
                match Path.GetDirectoryName(resolvedScriptPath) with
                | null
                | "" -> Directory.GetCurrentDirectory()
                | value -> value
            elif Path.IsPathRooted(rootDirectory) then
                Path.GetFullPath(rootDirectory)
            else
                Path.GetFullPath(Path.Combine(config.ResolutionFolder, rootDirectory))

        let externProviderNames = ScriptRuntime.parseExternProviderNames externProviders
        let externs = ScriptRuntime.resolveExterns resolvedRootDirectory externProviderNames
        let typed =
            resolvedScriptPath
            |> FScript.parseFileWithIncludes resolvedRootDirectory
            |> FScript.inferWithExterns externs

        let functionContracts =
            typed
            |> ExportSignatures.fromTypedProgram
            |> Map.toList
            |> List.sortBy fst
            |> List.map snd
            |> List.map (fun signature ->
                match Contract.fromExportedSignature signature with
                | Ok value -> value
                | Error err -> failwith err)

        let externProviderKey = String.concat ";" externProviderNames
        let contractIdSeed = $"{resolvedScriptPath}|{resolvedRootDirectory}|{externProviderKey}"
        let contractId =
            use sha = SHA256.Create()
            contractIdSeed
            |> Encoding.UTF8.GetBytes
            |> sha.ComputeHash
            |> Array.map (fun b -> b.ToString("X2"))
            |> String.concat ""
            |> fun value -> $"fscript:{value}"
        let contractJson =
            ScriptRuntime.createContractJson
                contractId
                resolvedScriptPath
                resolvedRootDirectory
                externProviderNames
                functionContracts

        contractJson, functionContracts, contractId

    let createRootType () =
        let rootType = ProvidedTypeDefinition(assembly, providerNamespace, "FScriptScriptProvider", Some typeof<obj>, hideObjectMethods = true)

        let parameters =
            [ ProvidedStaticParameter("ScriptPath", typeof<string>)
              ProvidedStaticParameter("RootDirectory", typeof<string>, "")
              ProvidedStaticParameter("ExternProviders", typeof<string>, "") ]

        rootType.DefineStaticParameters(
            parameters,
            fun generatedTypeName staticArgs ->
                try
                    let scriptPath = staticArgs.[0] :?> string
                    let rootDirectory = staticArgs.[1] :?> string
                    let externProviders = staticArgs.[2] :?> string
                    let contractJson, functions, contractId = buildContract scriptPath rootDirectory externProviders

                    let generatedType = ProvidedTypeDefinition(assembly, providerNamespace, generatedTypeName, Some typeof<obj>, hideObjectMethods = true)

                    let setResolverMethod =
                        ProvidedMethod(
                            "SetRuntimeResolver",
                            [ ProvidedParameter("resolver", typeof<RuntimeScriptResolver>) ],
                            typeof<unit>,
                            isStatic = true,
                            invokeCode =
                                (fun args ->
                                    match args with
                                    | [ resolver ] ->
                                        <@@ ScriptRuntime.setResolver contractId (%%resolver: RuntimeScriptResolver) @@>
                                    | _ -> invalidOp "Unexpected argument list for SetRuntimeResolver."))

                    let clearResolverMethod =
                        ProvidedMethod(
                            "ClearRuntimeResolver",
                            [],
                            typeof<unit>,
                            isStatic = true,
                            invokeCode =
                                (fun _ ->
                                    <@@ ScriptRuntime.clearResolver contractId @@>))

                    generatedType.AddMember(setResolverMethod)
                    generatedType.AddMember(clearResolverMethod)

                    for fn in functions do
                        let functionName = fn.Name
                        let parameterNames = normalizeParameterNames fn.ParameterNames
                        let parameters =
                            (parameterNames, fn.ParameterTypes)
                            ||> List.zip
                            |> List.map (fun (name, supportedType) ->
                                ProvidedParameter(name, RuntimeTypeBridge.toSystemType supportedType))
                        let returnType = RuntimeTypeBridge.toSystemType fn.ReturnType
                        let methodDefinition =
                            ProvidedMethod(
                                functionName,
                                parameters,
                                returnType,
                                isStatic = true,
                                invokeCode =
                                    (fun methodArgs ->
                                        let boxedArgs =
                                            methodArgs
                                            |> List.map (fun arg -> Expr.Coerce(arg, typeof<obj>))
                                            |> fun args -> Expr.NewArray(typeof<obj>, args)
                                        let call = <@@ ScriptRuntime.invoke contractJson functionName %%boxedArgs @@>
                                        Expr.Coerce(call, returnType)))
                        generatedType.AddMember(methodDefinition)

                    generatedType
                with
                | ParseException err ->
                    let scriptPath = staticArgs.[0] :?> string
                    failwith $"Failed to parse FScript '{scriptPath}': {err.Message}"
                | TypeException err ->
                    let scriptPath = staticArgs.[0] :?> string
                    failwith $"Failed to type-check FScript '{scriptPath}': {err.Message}"
                | ex ->
                    let scriptPath = staticArgs.[0] :?> string
                    failwith $"Failed to build FScript provider for '{scriptPath}': {ex.GetBaseException().Message}")

        rootType

    do
        this.AddNamespace(providerNamespace, [ createRootType () ])

[<assembly: TypeProviderAssembly>]
do ()
