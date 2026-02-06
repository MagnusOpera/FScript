namespace FScript.Core

module Descriptor =
    type FunctionDescriptor =
        { Name: string
          Parameters: Type list
          ReturnType: Type
          Scheme: Scheme option
          IsRecursive: bool
          Span: Span }

    let private flattenFunctionType (t: Type) : Type list * Type =
        let rec loop (acc: Type list) (current: Type) =
            match current with
            | TFun (arg, ret) -> loop (arg :: acc) ret
            | _ -> List.rev acc, current
        loop [] t

    let describeFunctions (program: TypedProgram) (symbols: Map<string, Scheme>) : FunctionDescriptor list =
        program
        |> List.choose (function
            | TSLet (name, _, expr, isRec, span) ->
                let t = TypedAst.typeOf expr
                match flattenFunctionType t with
                | [], _ -> None
                | args, ret ->
                    Some
                        { Name = name
                          Parameters = args
                          ReturnType = ret
                          Scheme = symbols.TryFind name
                          IsRecursive = isRec
                          Span = span }
            | _ -> None)

    let tryFindFunction (program: TypedProgram) (symbols: Map<string, Scheme>) (name: string) : FunctionDescriptor option =
        describeFunctions program symbols
        |> List.tryFind (fun f -> f.Name = name)
