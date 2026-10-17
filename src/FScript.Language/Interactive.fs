namespace FScript.Language

module Interactive =
    type Session =
        private
            { Registration: ExtensionRegistration
              RetainedProgramRev: Program
              TypeDeclarations: Program
              InferenceBindings: Map<string, ExternalFunction>
              EvaluationState: Eval.ProgramState option }

    type Submission =
        { Session: Session
          TypedProgram: TypeInfer.TypedProgram
          State: Eval.ProgramState
          RetainedCount: int }

    let private retainedStatements program =
        program
        |> List.filter (function
            | SExpr _ -> false
            | _ -> true)

    let private appendRetained reversed program =
        program
        |> List.fold (fun acc statement ->
            match statement with
            | SExpr _ -> acc
            | _ -> statement :: acc) reversed

    let private typeDeclarations program =
        program
        |> List.choose (function
            | SType _ as statement -> Some statement
            | _ -> None)

    let private inferenceBinding name bindingType =
        let scheme = Forall(Types.ftvType bindingType |> Set.toList, bindingType)
        { Name = name
          Scheme = scheme
          Arity = 0
          Impl = fun _ _ -> VUnit }

    let private updateInferenceBindings bindings typedProgram =
        typedProgram
        |> List.fold (fun current statement ->
            match statement with
            | TypeInfer.TSLet(name, _, bindingType, _, _, _) ->
                current |> Map.add name (inferenceBinding name bindingType)
            | TypeInfer.TSLetPattern(_, _, patternBindings, _, _) ->
                patternBindings
                |> Map.fold (fun state name bindingType ->
                    state |> Map.add name (inferenceBinding name bindingType)) current
            | TypeInfer.TSLetRecGroup(groupBindings, _, _) ->
                groupBindings
                |> List.fold (fun state (name, _, bindingType, _) ->
                    state |> Map.add name (inferenceBinding name bindingType)) current
            | TypeInfer.TSType _
            | TypeInfer.TSExpr _ -> current) bindings

    let createWithRegistration registration =
        { Registration = registration
          RetainedProgramRev = []
          TypeDeclarations = []
          InferenceBindings = Map.empty
          EvaluationState = None }

    let retainedCount session = session.RetainedProgramRev.Length

    let submit session program =
        let retainedRev = appendRetained session.RetainedProgramRev program
        let retainedCount = retainedRev.Length
        let containsTypeDeclaration =
            program
            |> List.exists (function
                | SType _ -> true
                | _ -> false)

        match session.EvaluationState with
        | None ->
            let typed = TypeInfer.inferProgramWithRegistration session.Registration program
            let state = Eval.evalProgramWithRegistrationState session.Registration typed
            let retained = retainedStatements program
            let nextSession =
                { session with
                    RetainedProgramRev = retainedRev
                    TypeDeclarations = typeDeclarations retained
                    InferenceBindings = updateInferenceBindings Map.empty typed
                    EvaluationState = Some state }
            { Session = nextSession
              TypedProgram = typed
              State = state
              RetainedCount = retainedCount }
        | Some _ when containsTypeDeclaration ->
            let candidate = (session.RetainedProgramRev |> List.rev) @ program
            let typed = TypeInfer.inferProgramWithRegistration session.Registration candidate
            let state = Eval.evalProgramWithRegistrationState session.Registration typed
            let submittedTyped = typed |> List.skip session.RetainedProgramRev.Length
            let retained = retainedRev |> List.rev
            let nextSession =
                { session with
                    RetainedProgramRev = retainedRev
                    TypeDeclarations = typeDeclarations retained
                    InferenceBindings = updateInferenceBindings Map.empty typed
                    EvaluationState = Some state }
            { Session = nextSession
              TypedProgram = submittedTyped
              State = state
              RetainedCount = retainedCount }
        | Some previousState ->
            let inferenceExterns =
                session.Registration.Functions @ (session.InferenceBindings |> Map.values |> Seq.toList)
            let inferenceProgram = session.TypeDeclarations @ program
            let typed = TypeInfer.inferProgramWithRegistration { session.Registration with Functions = inferenceExterns } inferenceProgram
            let submittedTyped = typed |> List.skip session.TypeDeclarations.Length
            let state = Eval.evalProgramFromState previousState submittedTyped
            let nextSession =
                { session with
                    RetainedProgramRev = retainedRev
                    InferenceBindings = updateInferenceBindings session.InferenceBindings submittedTyped
                    EvaluationState = Some state }
            { Session = nextSession
              TypedProgram = submittedTyped
              State = state
              RetainedCount = retainedCount }

    let create externs = createWithRegistration (HostInterop.ofExterns externs)
