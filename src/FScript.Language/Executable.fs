namespace FScript.Language

open System.Linq.Expressions

module Executable =
    let private evalProgramWithExternsStateMethod =
        match typeof<Value>.Assembly.GetType("FScript.Language.Eval") with
        | null -> failwith "FScript.Language.Eval type not found"
        | evalType ->
            match evalType.GetMethod("evalProgramWithExternsState") with
            | null -> failwith "Eval.evalProgramWithExternsState not found"
            | methodInfo -> methodInfo

    let private invokeValueMethod =
        match typeof<Value>.Assembly.GetType("FScript.Language.Eval") with
        | null -> failwith "FScript.Language.Eval type not found"
        | evalType ->
            match evalType.GetMethod("invokeValue") with
            | null -> failwith "Eval.invokeValue not found"
            | methodInfo -> methodInfo

    type CompiledFunctionSignature =
        { Name: string
          ParameterNames: string list
          ParameterTypes: Type list
          ReturnType: Type }

    type ExecutableProgram =
        private
            { RunState: unit -> Eval.ProgramState
              ExportedNames: string list
              FunctionSignatures: Map<string, CompiledFunctionSignature> }

    type LoadedExecutableProgram =
        private
            { Program: ExecutableProgram
              State: Eval.ProgramState
              ExportedFunctionNames: string list
              ExportedFunctionSet: Set<string>
              ExportedFunctions: Map<string, Value>
              ExportedFunctionInvokers: Map<string, (Value list -> Value)>
              ExportedFunctionSignatures: Map<string, CompiledFunctionSignature>
              ExportedValueNames: string list
              ExportedValueSet: Set<string>
              ExportedValues: Map<string, Value> }

    let private declaredExportedNames (program: TypeInfer.TypedProgram) =
        program
        |> List.collect (function
            | TypeInfer.TSLet(name, _, _, _, isExported, _) when isExported -> [ name ]
            | TypeInfer.TSLetRecGroup(bindings, isExported, _) when isExported -> bindings |> List.map (fun (name, _, _, _) -> name)
            | _ -> [])
        |> List.distinct
        |> List.sort

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

    let private collectFunctionSignatures (program: TypeInfer.TypedProgram) : Map<string, CompiledFunctionSignature> =
        let fromLet name expr exprType =
            let paramNames = flattenParameterNames expr
            let parameterTypes, returnType = flattenFunctionType exprType
            if paramNames.IsEmpty || parameterTypes.IsEmpty then
                None
            elif paramNames.Length <> parameterTypes.Length then
                failwith $"Signature mismatch for function '{name}'"
            else
                Some (name,
                      { Name = name
                        ParameterNames = paramNames
                        ParameterTypes = parameterTypes
                        ReturnType = returnType })

        program
        |> List.collect (function
            | TypeInfer.TSLet(name, expr, exprType, _, isExported, _) when isExported ->
                match fromLet name expr exprType with
                | Some signature -> [ signature ]
                | None -> []
            | TypeInfer.TSLetRecGroup(bindings, isExported, _) when isExported ->
                bindings
                |> List.choose (fun (name, expr, exprType, _) -> fromLet name expr exprType)
            | _ -> [])
        |> Map.ofList

    let private isCallable value =
        match value with
        | VClosure _ -> true
        | VExternal _ -> true
        | VUnionCtor _ -> true
        | _ -> false

    let private compileInvoker (typeDefs: Map<string, Type>) (fnValue: Value) : (Value list -> Value) =
        let argsParameter = Expression.Parameter(typeof<Value list>, "args")
        let callExpression =
            Expression.Call(
                invokeValueMethod,
                Expression.Constant(typeDefs, typeof<Map<string, Type>>),
                Expression.Constant(fnValue, typeof<Value>),
                argsParameter
            )
        let lambda = Expression.Lambda<System.Func<Value list, Value>>(callExpression, argsParameter)
        let compiled = lambda.Compile()
        fun args -> compiled.Invoke(args)

    let compileWithExterns (externs: ExternalFunction list) (program: TypeInfer.TypedProgram) : ExecutableProgram =
        let exportedNames = declaredExportedNames program
        let functionSignatures = collectFunctionSignatures program
        let call =
            Expression.Call(
                evalProgramWithExternsStateMethod,
                Expression.Constant(externs, typeof<ExternalFunction list>),
                Expression.Constant(program, typeof<TypeInfer.TypedProgram>)
            )
        let lambda = Expression.Lambda<System.Func<Eval.ProgramState>>(call)
        let compiled = lambda.Compile()

        { RunState = (fun () -> compiled.Invoke())
          ExportedNames = exportedNames
          FunctionSignatures = functionSignatures }

    let compile (program: TypeInfer.TypedProgram) : ExecutableProgram =
        compileWithExterns [] program

    let executeWithState (program: ExecutableProgram) : Eval.ProgramState =
        program.RunState()

    let execute (program: ExecutableProgram) : Value =
        (executeWithState program).LastValue

    let exportedNames (program: ExecutableProgram) : string list =
        program.ExportedNames

    let functionSignatures (program: ExecutableProgram) : Map<string, CompiledFunctionSignature> =
        program.FunctionSignatures

    let load (program: ExecutableProgram) : LoadedExecutableProgram =
        let state = executeWithState program
        let functionNames =
            program.ExportedNames
            |> List.filter (fun name ->
                match state.Env.TryFind name with
                | Some value -> isCallable value
                | None -> false)
        let functionSet = functionNames |> Set.ofList
        let functions =
            functionNames
            |> List.choose (fun name ->
                match state.Env.TryFind name with
                | Some value -> Some (name, value)
                | None -> None)
            |> Map.ofList
        let functionInvokers =
            functions
            |> Map.map (fun _ fnValue -> compileInvoker state.TypeDefs fnValue)
        let functionSignatures =
            program.FunctionSignatures
            |> Map.filter (fun name _ -> functionSet |> Set.contains name)

        let valueNames =
            program.ExportedNames
            |> List.filter (fun name ->
                match state.Env.TryFind name with
                | Some value -> not (isCallable value)
                | None -> false)
        let valueSet = valueNames |> Set.ofList
        let values =
            valueNames
            |> List.choose (fun name ->
                match state.Env.TryFind name with
                | Some value -> Some (name, value)
                | None -> None)
            |> Map.ofList

        { Program = program
          State = state
          ExportedFunctionNames = functionNames
          ExportedFunctionSet = functionSet
          ExportedFunctions = functions
          ExportedFunctionInvokers = functionInvokers
          ExportedFunctionSignatures = functionSignatures
          ExportedValueNames = valueNames
          ExportedValueSet = valueSet
          ExportedValues = values }

    let loadedState (loaded: LoadedExecutableProgram) : Eval.ProgramState =
        loaded.State

    let loadedFunctionNames (loaded: LoadedExecutableProgram) : string list =
        loaded.ExportedFunctionNames

    let loadedFunctionSet (loaded: LoadedExecutableProgram) : Set<string> =
        loaded.ExportedFunctionSet

    let loadedFunctions (loaded: LoadedExecutableProgram) : Map<string, Value> =
        loaded.ExportedFunctions

    let loadedFunctionInvokers (loaded: LoadedExecutableProgram) : Map<string, (Value list -> Value)> =
        loaded.ExportedFunctionInvokers

    let loadedFunctionSignatures (loaded: LoadedExecutableProgram) : Map<string, CompiledFunctionSignature> =
        loaded.ExportedFunctionSignatures

    let loadedValueNames (loaded: LoadedExecutableProgram) : string list =
        loaded.ExportedValueNames

    let loadedValueSet (loaded: LoadedExecutableProgram) : Set<string> =
        loaded.ExportedValueSet

    let loadedValues (loaded: LoadedExecutableProgram) : Map<string, Value> =
        loaded.ExportedValues

    let getValue (loaded: LoadedExecutableProgram) (valueName: string) : Value option =
        loaded.ExportedValues |> Map.tryFind valueName

    let invoke (loaded: LoadedExecutableProgram) (functionName: string) (args: Value list) : Value option =
        loaded.ExportedFunctionInvokers
        |> Map.tryFind functionName
        |> Option.map (fun invoker -> invoker args)
