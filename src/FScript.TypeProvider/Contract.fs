namespace FScript.TypeProvider

open System
open System.Security.Cryptography
open System.Text
open System.Text.Json
open FScript.Language

[<RequireQualifiedAccess>]
type SupportedType =
    | Unit
    | Int64
    | Float
    | Bool
    | String
    | List of SupportedType
    | Option of SupportedType
    | Tuple of SupportedType list
    | StringMap of SupportedType

type FunctionContract =
    { Name: string
      ParameterNames: string list
      ParameterTypes: SupportedType list
      ReturnType: SupportedType }

type ScriptContract =
    { ContractId: string
      CompileScriptPath: string
      CompileRootDirectory: string
      ExternProviderTypeNames: string list
      Fingerprint: string
      Functions: FunctionContract list }

type RuntimeScriptOverride =
    { RootDirectory: string
      EntryFile: string
      EntrySource: string
      ResolveImportedSource: (string -> string option) option }

type RuntimeScriptResolver = unit -> RuntimeScriptOverride option

module Contract =
    let private tupleMaxArity = 8
    let private toHex (bytes: byte array) =
        bytes
        |> Array.map (fun b -> b.ToString("X2"))
        |> String.concat ""

    let rec canonicalType (t: SupportedType) : string =
        match t with
        | SupportedType.Unit -> "unit"
        | SupportedType.Int64 -> "int64"
        | SupportedType.Float -> "float"
        | SupportedType.Bool -> "bool"
        | SupportedType.String -> "string"
        | SupportedType.List inner -> $"list<{canonicalType inner}>"
        | SupportedType.Option inner -> $"option<{canonicalType inner}>"
        | SupportedType.Tuple items ->
            items
            |> List.map canonicalType
            |> String.concat "*"
            |> fun payload -> $"tuple<{payload}>"
        | SupportedType.StringMap inner -> $"map<string,{canonicalType inner}>"

    let canonicalFunction (f: FunctionContract) =
        let args = f.ParameterTypes |> List.map canonicalType |> String.concat ","
        $"{f.Name}({args})->{canonicalType f.ReturnType}"

    let fingerprint (functions: FunctionContract list) : string =
        let canonical =
            functions
            |> List.sortBy (fun f -> f.Name)
            |> List.map canonicalFunction
            |> String.concat "\n"

        use sha = SHA256.Create()
        canonical
        |> Encoding.UTF8.GetBytes
        |> sha.ComputeHash
        |> toHex

    [<CLIMutable>]
    type FunctionContractDto =
        { Name: string
          ParameterNames: string list
          ParameterTypes: string list
          ReturnType: string }

    [<CLIMutable>]
    type ScriptContractDto =
        { ContractId: string
          CompileScriptPath: string
          CompileRootDirectory: string
          ExternProviderTypeNames: string list
          Fingerprint: string
          Functions: FunctionContractDto list }

    let private splitTopLevel (delimiter: char) (value: string) =
        let parts = ResizeArray<string>()
        let mutable depth = 0
        let mutable start = 0
        for i = 0 to value.Length - 1 do
            let c = value[i]
            if c = '<' then depth <- depth + 1
            elif c = '>' then depth <- depth - 1
            elif c = delimiter && depth = 0 then
                parts.Add(value.Substring(start, i - start))
                start <- i + 1
        parts.Add(value.Substring(start))
        parts |> Seq.toList

    let rec private parseSupportedType (value: string) : SupportedType =
        match value with
        | "unit" -> SupportedType.Unit
        | "int64" -> SupportedType.Int64
        | "float" -> SupportedType.Float
        | "bool" -> SupportedType.Bool
        | "string" -> SupportedType.String
        | _ when value.StartsWith("list<", StringComparison.Ordinal) && value.EndsWith(">", StringComparison.Ordinal) ->
            let inner = value.Substring(5, value.Length - 6)
            SupportedType.List (parseSupportedType inner)
        | _ when value.StartsWith("option<", StringComparison.Ordinal) && value.EndsWith(">", StringComparison.Ordinal) ->
            let inner = value.Substring(7, value.Length - 8)
            SupportedType.Option (parseSupportedType inner)
        | _ when value.StartsWith("map<string,", StringComparison.Ordinal) && value.EndsWith(">", StringComparison.Ordinal) ->
            let inner = value.Substring("map<string,".Length, value.Length - "map<string,".Length - 1)
            SupportedType.StringMap (parseSupportedType inner)
        | _ when value.StartsWith("tuple<", StringComparison.Ordinal) && value.EndsWith(">", StringComparison.Ordinal) ->
            let payload = value.Substring(6, value.Length - 7)
            let items =
                payload
                |> splitTopLevel '*'
                |> List.map parseSupportedType
            SupportedType.Tuple items
        | _ ->
            invalidOp $"Unsupported serialized type '{value}'."

    let toJson (contract: ScriptContract) =
        let dto : ScriptContractDto =
            { ContractId = contract.ContractId
              CompileScriptPath = contract.CompileScriptPath
              CompileRootDirectory = contract.CompileRootDirectory
              ExternProviderTypeNames = contract.ExternProviderTypeNames
              Fingerprint = contract.Fingerprint
              Functions =
                contract.Functions
                |> List.map (fun fn ->
                    { Name = fn.Name
                      ParameterNames = fn.ParameterNames
                      ParameterTypes = fn.ParameterTypes |> List.map canonicalType
                      ReturnType = canonicalType fn.ReturnType }) }
        JsonSerializer.Serialize(dto)

    let fromJson (json: string) : ScriptContract =
        match JsonSerializer.Deserialize<ScriptContractDto>(json) with
        | null -> invalidOp "Unable to parse script contract payload."
        | value ->
            { ContractId = value.ContractId
              CompileScriptPath = value.CompileScriptPath
              CompileRootDirectory = value.CompileRootDirectory
              ExternProviderTypeNames = value.ExternProviderTypeNames
              Fingerprint = value.Fingerprint
              Functions =
                value.Functions
                |> List.map (fun fn ->
                    { Name = fn.Name
                      ParameterNames = fn.ParameterNames
                      ParameterTypes = fn.ParameterTypes |> List.map parseSupportedType
                      ReturnType = parseSupportedType fn.ReturnType }) }

    let rec ofFScriptType (typePath: string) (t: Type) : Result<SupportedType, string> =
        let recurse childPath child = ofFScriptType childPath child
        match t with
        | TUnit -> Ok SupportedType.Unit
        | TInt -> Ok SupportedType.Int64
        | TFloat -> Ok SupportedType.Float
        | TBool -> Ok SupportedType.Bool
        | TString -> Ok SupportedType.String
        | TList inner ->
            recurse $"{typePath} list item" inner
            |> Result.map SupportedType.List
        | TOption inner ->
            recurse $"{typePath} option value" inner
            |> Result.map SupportedType.Option
        | TTuple items ->
            if items.Length < 2 then
                Error $"{typePath}: tuples must have arity 2..{tupleMaxArity}."
            elif items.Length > tupleMaxArity then
                Error $"{typePath}: tuple arity {items.Length} is not supported (max {tupleMaxArity})."
            else
                items
                |> List.mapi (fun i item -> recurse $"{typePath} tuple item #{i + 1}" item)
                |> List.fold (fun state next ->
                    match state, next with
                    | Error err, _ -> Error err
                    | _, Error err -> Error err
                    | Ok acc, Ok value -> Ok (acc @ [ value ])) (Ok [])
                |> Result.map SupportedType.Tuple
        | TMap (TString, valueType) ->
            recurse $"{typePath} map value" valueType
            |> Result.map SupportedType.StringMap
        | TMap _ ->
            Error $"{typePath}: only map<string, T> is supported."
        | TNamed name ->
            Error $"{typePath}: named type '{name}' is not supported in exported signatures."
        | TRecord _ ->
            Error $"{typePath}: record types are not supported in exported signatures."
        | TUnion (name, _) ->
            Error $"{typePath}: union type '{name}' is not supported in exported signatures."
        | TTypeToken ->
            Error $"{typePath}: type tokens are not supported in exported signatures."
        | TFun _ ->
            Error $"{typePath}: function values are not supported in exported signatures."
        | TVar id ->
            Error $"{typePath}: unresolved generic type variable '{id}' is not supported in exported signatures."

    let fromExportedSignature
        (signature: FScript.Runtime.ExportSignatures.ExportedFunctionSignature)
        : Result<FunctionContract, string> =
        let convertParameter index paramType =
            ofFScriptType $"Function '{signature.Name}' parameter #{index + 1}" paramType

        let convertedParams =
            signature.ParameterTypes
            |> List.mapi convertParameter
            |> List.fold (fun state next ->
                match state, next with
                | Error err, _ -> Error err
                | _, Error err -> Error err
                | Ok acc, Ok value -> Ok (acc @ [ value ])) (Ok [])

        let convertedReturn =
            ofFScriptType $"Function '{signature.Name}' return type" signature.ReturnType

        match convertedParams, convertedReturn with
        | Ok paramTypes, Ok returnType ->
            Ok
                { Name = signature.Name
                  ParameterNames = signature.ParameterNames
                  ParameterTypes = paramTypes
                  ReturnType = returnType }
        | Error err, _
        | _, Error err -> Error err
