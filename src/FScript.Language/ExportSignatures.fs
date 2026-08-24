namespace FScript.Language

module ExportSignatures =
    type ExportedFunctionSignature =
        { Name: string
          ParameterNames: string list
          ParameterTypes: Type list
          ReturnType: Type }

    let private flattenFunctionType (t: Type) : Type list * Type =
        let rec loop (acc: Type list) (current: Type) =
            match current with
            | TFun (argument, returnType) -> loop (argument :: acc) returnType
            | _ -> List.rev acc, current
        loop [] t

    let private flattenParameterNames (expression: Expr) : string list =
        let rec loop (acc: string list) (current: Expr) =
            match current with
            | ELambda (parameter, body, _) -> loop (parameter.Name :: acc) body
            | _ -> List.rev acc
        loop [] expression

    let private fromLet name expression expressionType =
        let parameterNames = flattenParameterNames expression
        let parameterTypes, returnType = flattenFunctionType expressionType
        if parameterNames.IsEmpty || parameterTypes.IsEmpty then
            None
        elif parameterNames.Length <> parameterTypes.Length then
            failwith $"Signature mismatch for function '{name}'"
        else
            Some
                { Name = name
                  ParameterNames = parameterNames
                  ParameterTypes = parameterTypes
                  ReturnType = returnType }

    let fromTypedProgram (program: TypeInfer.TypedProgram) : Map<string, ExportedFunctionSignature> =
        program
        |> List.collect (function
            | TypeInfer.TSLet(name, expression, expressionType, _, isExported, _) when isExported ->
                match fromLet name expression expressionType with
                | Some signature -> [ signature.Name, signature ]
                | None -> []
            | TypeInfer.TSLetRecGroup(bindings, isExported, _) when isExported ->
                bindings
                |> List.choose (fun (name, expression, expressionType, _) ->
                    fromLet name expression expressionType
                    |> Option.map (fun signature -> signature.Name, signature))
            | _ -> [])
        |> Map.ofList
