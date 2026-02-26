namespace FScript.Runtime

open FScript.Language

module ExportSignatures =
    type ExportedFunctionSignature =
        { Name: string
          ParameterNames: string list
          ParameterTypes: Type list
          ReturnType: Type }

    let private flattenFunctionType (t: Type) : Type list * Type =
        let rec loop (acc: Type list) (current: Type) =
            match current with
            | TFun (arg, ret) -> loop (arg :: acc) ret
            | _ -> List.rev acc, current
        loop [] t

    let private flattenParameterNames (expr: Expr) : string list =
        let rec loop (acc: string list) (current: Expr) =
            match current with
            | ELambda (param, body, _) -> loop (param.Name :: acc) body
            | _ -> List.rev acc
        loop [] expr

    let private fromLet (name: string) (expr: Expr) (exprType: Type) : ExportedFunctionSignature option =
        let parameterNames = flattenParameterNames expr
        let parameterTypes, returnType = flattenFunctionType exprType
        if parameterNames.IsEmpty || parameterTypes.IsEmpty then
            None
        elif parameterNames.Length <> parameterTypes.Length then
            raise (HostCommon.evalError $"Signature mismatch for function '{name}'")
        else
            Some
                { Name = name
                  ParameterNames = parameterNames
                  ParameterTypes = parameterTypes
                  ReturnType = returnType }

    let fromTypedProgram (program: TypeInfer.TypedProgram) : Map<string, ExportedFunctionSignature> =
        program
        |> List.collect (function
            | TypeInfer.TSLet(name, expr, exprType, _, isExported, _) when isExported ->
                match fromLet name expr exprType with
                | Some signature -> [ signature.Name, signature ]
                | None -> []
            | TypeInfer.TSLetRecGroup(bindings, isExported, _) when isExported ->
                bindings
                |> List.choose (fun (name, expr, exprType, _) ->
                    fromLet name expr exprType
                    |> Option.map (fun signature -> signature.Name, signature))
            | _ -> [])
        |> Map.ofList
