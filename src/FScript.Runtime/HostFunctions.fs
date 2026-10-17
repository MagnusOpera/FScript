namespace FScript.Runtime

open System
open System.Reflection
open Microsoft.FSharp.Reflection
open FScript.Language

/// Typed, synchronous F# function adapters over the existing external-function bridge.
module HostFunctions =
    let private fail name message =
        raise (HostCommon.evalError $"Host function '{name}': {message}")

    /// Derive a monomorphic scheme, arity, and argument/result conversions from a concrete F# handler.
    /// All attributed types referenced by the signature must already be registered.
    let create<'Handler> (registration: ExtensionRegistration) (name: string) (handler: 'Handler) : ExternalFunction =
        let validPart (part: string) =
            part.Length > 0 && (Char.IsLetter part[0] || part[0] = '_')
            && (part |> Seq.forall (fun c -> Char.IsLetterOrDigit c || c = '_'))
        if String.IsNullOrWhiteSpace name || not (name.Split('.') |> Array.forall validPart) then
            fail name "The script function name must be a nonempty qualified identifier."
        let handlerType = typeof<'Handler>
        if handlerType.ContainsGenericParameters || not (FSharpType.IsFunction handlerType) then
            fail name "Expected a concrete F# function. Use 'fun () -> value' for a parameterless handler."
        if isNull (box handler) then fail name "The handler cannot be null."

        let rec signature (index: int) (functionType: System.Type) : (HostTypes.ValueCodec * MethodInfo) list * HostTypes.ValueCodec =
            if FSharpType.IsFunction functionType then
                let argumentType, returnType = FSharpType.GetFunctionElements functionType
                if FSharpType.IsFunction argumentType then
                    fail name $"Argument {index} is a callback; use an ExternalFunction with ExternalCallContext.Apply."
                let codec = HostTypes.valueCodec registration $"{name}.argument[{index}]" argumentType
                let invoke = functionType.GetMethod("Invoke", [|argumentType|]) |> nonNull
                let rest, result = signature (index + 1) returnType
                (codec, invoke) :: rest, result
            else
                [], HostTypes.valueCodec registration $"{name}.result" functionType

        let parameters, result = signature 1 handlerType
        let projections =
            result.Projections @ (parameters |> List.collect (fun (codec, _) -> codec.Projections))
            |> List.distinctBy fst
        for typeName, types in projections |> List.groupBy (fun (_, descriptor) -> descriptor.Name) do
            if types.Length > 1 then
                fail name $"Multiple CLR types project to the script type name '{typeName}'."
        let scriptType =
            List.foldBack (fun ((codec: HostTypes.ValueCodec), _) rest -> TFun(codec.ScriptType, rest)) parameters result.ScriptType
        let definitions = HostInterop.typeDefinitions registration []
        { Name = name
          Scheme = Forall([], scriptType)
          Arity = parameters.Length
          Impl = fun _ arguments ->
              if arguments.Length <> parameters.Length then
                  fail name $"Expected {parameters.Length} arguments, received {arguments.Length}."
              HostInterop.validateCall definitions scriptType arguments (fun () ->
                  // Decode every argument before executing any part of a curried handler.
                  let decoded = List.map2 (fun ((codec: HostTypes.ValueCodec), _) value -> codec.Decode value) parameters arguments
                  let mutable current: objnull = box handler
                  for (_, invoke), argument in List.zip parameters decoded do
                      if isNull current then fail name "A curried handler returned a null function."
                      try
                          current <- invoke.Invoke(current, [|argument|])
                      with
                      | :? TargetInvocationException as error ->
                          fail name $"Handler failed: {error.GetBaseException().Message}"
                  result.Encode current) }
