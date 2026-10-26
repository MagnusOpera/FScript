namespace FScript.Compiler

open System
open System.IO
open System.Collections.Generic
open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks

/// Explicit cross-assembly expansion. Does not infer purity or change unmarked calls.
module internal DotNetIlInlining =
    exception private Rejected of CompilerDiagnostic

    let private reject span message =
        raise (Rejected {Code="FSN7201";Stage=DotNetGeneration;Span=span;Message=message})

    let rec private visible (typ:TypeDefinition) =
        if typ.IsNested then typ.IsNestedPublic && visible typ.DeclaringType else typ.IsPublic

    let private validate span (method:MethodDefinition) =
        let fail reason = reject span $"Cannot Inline '{method.FullName}': {reason}"
        if not method.HasBody || not method.IsStatic || method.IsConstructor || method.HasGenericParameters || method.DeclaringType.HasGenericParameters then
            fail "requires a static, nongeneric managed method body."
        if method.IsSynchronized || method.IsPInvokeImpl || method.NoInlining then fail "method implementation flags prohibit expansion."
        if method.DeclaringType.Methods |> Seq.exists (fun candidate -> candidate.IsConstructor && candidate.IsStatic) then
            fail "declaring type has a type initializer; its call-boundary semantics must be retained."
        if method.Module.Types |> Seq.exists (fun typ -> typ.Name="<Module>" && (typ.Methods |> Seq.exists (fun candidate -> candidate.IsConstructor && candidate.IsStatic))) then
            fail "declaring module has an initializer; its execution cannot be dropped."
        if method.Body.Variables |> Seq.exists _.IsPinned then fail "pinned locals require the original call frame."
        if method.Body.HasExceptionHandlers then fail "exception-handler bodies are not supported by this version of Inline."
        let rec safeType (typ:TypeReference) =
            match typ with
            | :? ByReferenceType | :? PointerType | :? FunctionPointerType -> fail "by-reference, pointer, and function-pointer types are unsupported."
            | :? GenericParameter -> fail "open generic operands are unsupported."
            | :? GenericInstanceType as generic -> safeType generic.ElementType; for arg in generic.GenericArguments do safeType arg
            | :? TypeSpecification as spec -> safeType spec.ElementType
            | _ ->
                let definition=typ.Resolve()
                if not (isNull definition) && not (visible definition) then fail $"operand type '{typ.FullName}' is not public."
        safeType method.ReturnType
        for parameter in method.Parameters do safeType parameter.ParameterType
        for local in method.Body.Variables do safeType local.VariableType
        for instruction in method.Body.Instructions do
            if instruction.OpCode.OpCodeType=OpCodeType.Prefix ||
               [Code.Calli;Code.Jmp;Code.Localloc;Code.Arglist;Code.Leave;Code.Leave_S;Code.Rethrow;Code.Endfinally;Code.Endfilter] |> List.contains instruction.OpCode.Code then
                fail $"instruction '{instruction.OpCode.Name}' requires unsupported call-frame handling."
            match instruction.Operand with
            | :? MethodReference as target ->
                let definition=target.Resolve()
                if isNull definition || not definition.IsPublic || not (visible definition.DeclaringType) then fail $"member '{target.FullName}' is not public."
                if definition.Module=method.Module && definition.MetadataToken=method.MetadataToken then fail "recursive expansion is unsupported."
                if target.DeclaringType.FullName="System.Diagnostics.StackTrace" || target.DeclaringType.FullName="System.Diagnostics.StackFrame" ||
                   (target.DeclaringType.FullName="System.Environment" && target.Name="get_StackTrace") || target.Name="GetCallingAssembly" || target.Name="GetExecutingAssembly" || target.Name="GetCurrentMethod" then
                    fail "stack-sensitive reflection is unsupported."
                safeType target.DeclaringType
            | :? FieldReference as field ->
                let definition=field.Resolve()
                if isNull definition || not definition.IsPublic || not (visible definition.DeclaringType) then fail $"field '{field.FullName}' is not public."
                safeType field.FieldType
            | :? TypeReference as typ -> safeType typ
            | :? CallSite -> fail "indirect call sites are unsupported."
            | _ -> ()

    let private boundaries (body:MethodBody) =
        let result=HashSet<Instruction>()
        for instruction in body.Instructions do
            match instruction.Operand with
            | :? Instruction as target -> result.Add target |> ignore
            | :? (Instruction array) as targets -> for target in targets do result.Add target |> ignore
            | _ -> ()
        for handler in body.ExceptionHandlers do
            for target in [handler.TryStart;handler.TryEnd;handler.HandlerStart;handler.HandlerEnd;handler.FilterStart] do
                if not (isNull target) then result.Add target |> ignore
        result

    let private nop (instruction:Instruction) = instruction.OpCode<-OpCodes.Nop;instruction.Operand<-null

    let private cleanup (body:MethodBody) (arguments:VariableDefinition array) =
        // Only substitute newly introduced argument slots, never arbitrary caller locals.
        // Constants and unassigned/unaddressed caller parameters are stable across calls.
        let targets=boundaries body
        let immutableArguments = body.Instructions |> Seq.forall (fun instruction -> instruction.OpCode<>OpCodes.Starg && instruction.OpCode<>OpCodes.Ldarga)
        for argument in Array.rev arguments do
            let uses=body.Instructions |> Seq.filter (fun instruction -> Object.ReferenceEquals(instruction.Operand,argument)) |> Seq.toArray
            let stores=uses |> Array.filter (fun instruction -> instruction.OpCode=OpCodes.Stloc)
            if stores.Length=1 && uses |> Array.forall (fun instruction -> instruction.OpCode=OpCodes.Ldloc || instruction.OpCode=OpCodes.Stloc) then
                let store=stores.[0]
                let producer=store.Previous
                if not (isNull producer) && not (targets.Contains store) &&
                   ([Code.Ldc_I4;Code.Ldc_I8;Code.Ldc_R4;Code.Ldc_R8;Code.Ldstr;Code.Ldnull] |> List.contains producer.OpCode.Code || producer.OpCode=OpCodes.Ldarg && immutableArguments) then
                    for useSite in uses do
                        if useSite.OpCode=OpCodes.Ldloc then useSite.OpCode<-producer.OpCode;useSite.Operand<-producer.Operand
                    nop producer;nop store
        // Remove nops while keeping protected-region endpoints and redirecting branches.
        let protectedPoints=HashSet<Instruction>()
        for handler in body.ExceptionHandlers do
            for point in [handler.TryStart;handler.TryEnd;handler.HandlerStart;handler.HandlerEnd;handler.FilterStart] do
                if not (isNull point) then protectedPoints.Add point |> ignore
        let rec next (instruction:Instruction) =
            if instruction.OpCode=OpCodes.Nop && not (protectedPoints.Contains instruction) && not (isNull instruction.Next) then next instruction.Next else instruction
        for instruction in body.Instructions do
            match instruction.Operand with
            | :? Instruction as target -> instruction.Operand<-next target
            | :? (Instruction array) as targets -> instruction.Operand<-Array.map next targets
            | _ -> ()
        for instruction in body.Instructions |> Seq.toArray do
            if instruction.OpCode=OpCodes.Nop && not (protectedPoints.Contains instruction) && not (isNull instruction.Next) then body.Instructions.Remove instruction |> ignore
        // Fold total integer operations exposed by expansion, with CLI wrap semantics.
        let protectedTargets=boundaries body
        let processor=body.GetILProcessor()
        let mutable cursor=if body.Instructions.Count=0 then null else body.Instructions.[0]
        while not (isNull cursor) do
            let second=cursor.Next
            let operation=if isNull second then null else second.Next
            let integer (instruction:Instruction) =
                if instruction.OpCode=OpCodes.Ldc_I4 then Some(32,bigint (unbox<int32> instruction.Operand))
                elif instruction.OpCode=OpCodes.Ldc_I8 then Some(64,bigint (unbox<int64> instruction.Operand)) else None
            let folded=
                if isNull second || isNull operation || protectedTargets.Contains second || protectedTargets.Contains operation then false
                else
                    match integer cursor,integer second with
                    | Some(bits,left),Some(otherBits,right) when bits=otherBits ->
                        let result=
                            match operation.OpCode.Code with
                            | Code.Add -> Some(left+right) | Code.Sub -> Some(left-right) | Code.Mul -> Some(left*right)
                            | Code.And -> Some(left &&& right) | Code.Or -> Some(left ||| right) | Code.Xor -> Some(left ^^^ right)
                            | _ -> None
                        match result with
                        | None -> false
                        | Some result ->
                            let modulus=1I <<< bits
                            let wrapped=result &&& (modulus-1I)
                            let signed=if wrapped >= (1I <<< (bits-1)) then wrapped-modulus else wrapped
                            cursor.Operand<-if bits=32 then box (int32 signed) else box (int64 signed)
                            processor.Remove second;processor.Remove operation
                            true
                    | _ -> false
            if not folded then cursor<-cursor.Next
        // A return redirected to the immediate continuation needs no branch.
        for instruction in body.Instructions do
            if instruction.OpCode=OpCodes.Br && Object.ReferenceEquals(instruction.Operand,instruction.Next) then nop instruction
        // Discard argument slots whose stores and reads were removed.
        for argument in arguments do
            if body.Instructions |> Seq.exists (fun instruction -> Object.ReferenceEquals(instruction.Operand,argument)) |> not then body.Variables.Remove argument |> ignore
        body.OptimizeMacros()

    /// Collapse exact forwarding bodies without introducing spill locals or branches.
    let private forwardingTarget (callee:MethodDefinition) =
        let body=callee.Body
        body.SimplifyMacros()
        let instructions=body.Instructions |> Seq.filter (fun instruction -> instruction.OpCode<>OpCodes.Nop) |> Seq.toArray
        let count=callee.Parameters.Count
        if body.Variables.Count<>0 || instructions.Length<>count+2 || instructions.[count].OpCode<>OpCodes.Call || instructions.[count+1].OpCode<>OpCodes.Ret then None
        else
            match instructions.[count].Operand with
            | :? MethodReference as target when not target.HasThis && target.Parameters.Count=count ->
                let sameType (a:TypeReference) (b:TypeReference) = a.FullName=b.FullName && a.Scope.Name=b.Scope.Name
                let matches=
                    sameType target.ReturnType callee.ReturnType &&
                    [0..count-1] |> List.forall (fun index ->
                        instructions.[index].OpCode=OpCodes.Ldarg && Object.ReferenceEquals(instructions.[index].Operand,callee.Parameters.[index]) &&
                        sameType target.Parameters.[index].ParameterType callee.Parameters.[index].ParameterType)
                if matches then Some target else None
            | _ -> None

    let private expand span (moduleDef:ModuleDefinition) (caller:MethodDefinition) (site:Instruction) (callee:MethodDefinition) =
        validate span callee
        if not (isNull site.Previous) && site.Previous.OpCode.OpCodeType=OpCodeType.Prefix then reject span "Inline cannot replace a prefixed call site."
        let source=callee.Body
        source.SimplifyMacros()
        let body=caller.Body
        let processor=body.GetILProcessor()
        let argumentSlots=callee.Parameters |> Seq.map (fun parameter -> VariableDefinition(moduleDef.ImportReference parameter.ParameterType)) |> Seq.toArray
        let locals=source.Variables |> Seq.map (fun local -> VariableDefinition(moduleDef.ImportReference local.VariableType)) |> Seq.toArray
        for local in Array.append argumentSlots locals do body.Variables.Add local
        body.InitLocals<-true
        let endPoint=Instruction.Create OpCodes.Nop
        let copies=Dictionary<Instruction,Instruction>()
        for instruction in source.Instructions do copies.Add(instruction,Instruction.Create OpCodes.Nop)
        for instruction in source.Instructions do
            let copy=copies.[instruction]
            match instruction.OpCode.Code, instruction.Operand with
            | Code.Ret,_ -> copy.OpCode<-OpCodes.Br;copy.Operand<-endPoint
            | Code.Ldarg,(:? ParameterDefinition as parameter) -> copy.OpCode<-OpCodes.Ldloc;copy.Operand<-argumentSlots.[parameter.Index]
            | Code.Starg,(:? ParameterDefinition as parameter) -> copy.OpCode<-OpCodes.Stloc;copy.Operand<-argumentSlots.[parameter.Index]
            | Code.Ldarga,(:? ParameterDefinition as parameter) -> copy.OpCode<-OpCodes.Ldloca;copy.Operand<-argumentSlots.[parameter.Index]
            | _,operand ->
                copy.OpCode<-instruction.OpCode
                copy.Operand<-
                    match operand with
                    | :? Instruction as target -> copies.[target] :> obj
                    | :? (Instruction array) as targets -> targets |> Array.map (fun target -> copies.[target]) :> obj
                    | :? VariableDefinition as local -> locals.[local.Index] :> obj
                    | :? MethodReference as method -> moduleDef.ImportReference method :> obj
                    | :? FieldReference as field -> moduleDef.ImportReference field :> obj
                    | :? TypeReference as typ -> moduleDef.ImportReference typ :> obj
                    | _ -> operand
        let emitted=ResizeArray<Instruction>()
        // Arguments already evaluated left-to-right; pop them in reverse stack order.
        for slot in Array.rev argumentSlots do emitted.Add(Instruction.Create(OpCodes.Stloc,slot))
        // Each invocation originally got fresh zeroed locals, even when the caller loops.
        if source.InitLocals then
            for local in locals do
                emitted.Add(Instruction.Create(OpCodes.Ldloca,local))
                emitted.Add(Instruction.Create(OpCodes.Initobj,local.VariableType))
        for instruction in source.Instructions do emitted.Add copies.[instruction]
        emitted.Add endPoint
        nop site
        let mutable previous=site
        for instruction in emitted do processor.InsertAfter(previous,instruction);previous<-instruction
        argumentSlots

    /// Remove only newly unreferenced provider assembly scopes, never guessed dependencies.
    let private pruneReferences (moduleDef:ModuleDefinition) candidates =
        let used=HashSet<string>(StringComparer.OrdinalIgnoreCase)
        let rec typ (value:TypeReference | null) =
            if not (isNull value) then
                let value=nonNull value
                match value.Scope with :? AssemblyNameReference as scope -> used.Add scope.Name |> ignore | _ -> ()
                match value with
                | :? GenericInstanceType as generic -> typ generic.ElementType; for arg in generic.GenericArguments do typ arg
                | :? TypeSpecification as spec -> typ spec.ElementType
                | _ -> ()
        let rec memberRef (value:MemberReference) =
            typ value.DeclaringType
            match value with
            | :? TypeReference as value -> typ value
            | :? FieldReference as field -> typ field.FieldType
            | :? MethodReference as method ->
                typ method.ReturnType
                for parameter in method.Parameters do typ parameter.ParameterType
                match method with
                | :? GenericInstanceMethod as generic -> for arg in generic.GenericArguments do typ arg
                | _ -> ()
            | _ -> ()
        let rec argument (value:CustomAttributeArgument) =
            typ value.Type
            match value.Value with
            | :? TypeReference as value -> typ value
            | :? (CustomAttributeArgument array) as values -> for value in values do argument value
            | :? CustomAttributeArgument as value -> argument value
            | _ -> ()
        let attributes (owner:ICustomAttributeProvider) =
            for attribute in owner.CustomAttributes do
                memberRef attribute.Constructor
                for arg in attribute.ConstructorArguments do argument arg
                for arg in attribute.Fields do argument arg.Argument
                for arg in attribute.Properties do argument arg.Argument
        let mutable security=moduleDef.Assembly.HasSecurityDeclarations
        let rec definition (value:TypeDefinition) =
            attributes value;typ value.BaseType
            security<-security || value.HasSecurityDeclarations
            for interfaceImpl in value.Interfaces do typ interfaceImpl.InterfaceType;attributes interfaceImpl
            for field in value.Fields do typ field.FieldType;attributes field
            for property in value.Properties do typ property.PropertyType;attributes property
            for event in value.Events do typ event.EventType;attributes event
            for method in value.Methods do
                memberRef method;attributes method;attributes method.MethodReturnType
                security<-security || method.HasSecurityDeclarations
                for parameter in method.Parameters do attributes parameter
                for generic in method.GenericParameters do
                    attributes generic
                    for constraintType in generic.Constraints do typ constraintType.ConstraintType;attributes constraintType
                for target in method.Overrides do memberRef target
                if method.HasBody then
                    for local in method.Body.Variables do typ local.VariableType
                    for handler in method.Body.ExceptionHandlers do typ handler.CatchType
                    for instruction in method.Body.Instructions do
                        match instruction.Operand with
                        | :? MemberReference as target -> memberRef target
                        | :? CallSite as signature -> typ signature.ReturnType;for parameter in signature.Parameters do typ parameter.ParameterType
                        | _ -> ()
            for generic in value.GenericParameters do
                attributes generic
                for constraintType in generic.Constraints do typ constraintType.ConstraintType;attributes constraintType
            for nested in value.NestedTypes do definition nested
        attributes moduleDef;attributes moduleDef.Assembly
        for value in moduleDef.Types do definition value
        if not security then
            for reference in moduleDef.AssemblyReferences |> Seq.toArray do
                if Set.contains reference.Name candidates && not (used.Contains reference.Name) then moduleDef.AssemblyReferences.Remove reference |> ignore

    let private encodedSize (body:MethodBody) = body.Instructions |> Seq.sumBy (fun instruction -> instruction.GetSize())

    /// Trial expansion never mutates the accepted caller; over-budget trials are discarded.
    let private copyBody (method:MethodDefinition) =
        let original=method.Body
        let body=MethodBody(method,InitLocals=original.InitLocals,MaxStackSize=original.MaxStackSize)
        let locals=original.Variables |> Seq.map (fun local -> VariableDefinition(local.VariableType)) |> Seq.toArray
        for local in locals do body.Variables.Add local
        let instructions=Dictionary<Instruction,Instruction>()
        for instruction in original.Instructions do
            let copy=Instruction.Create OpCodes.Nop
            copy.OpCode<-instruction.OpCode
            instructions.Add(instruction,copy)
            body.Instructions.Add copy
        for instruction in original.Instructions do
            instructions.[instruction].Operand<-
                match instruction.Operand with
                | :? Instruction as target -> instructions.[target] :> obj
                | :? (Instruction array) as targets -> targets |> Array.map (fun target -> instructions.[target]) :> obj
                | :? VariableDefinition as local -> locals.[local.Index] :> obj
                | operand -> operand
        let target (instruction:Instruction | null) : Instruction = if isNull instruction then Unchecked.defaultof<Instruction> else instructions.[nonNull instruction]
        for handler in original.ExceptionHandlers do
            body.ExceptionHandlers.Add(ExceptionHandler(handler.HandlerType,CatchType=handler.CatchType,
                TryStart=target handler.TryStart,TryEnd=target handler.TryEnd,
                HandlerStart=target handler.HandlerStart,HandlerEnd=target handler.HandlerEnd,FilterStart=target handler.FilterStart))
        body,instructions

    // .NET 10 RyuJIT screens tiny callees at 16 IL bytes and ordinary candidates
    // at 100. We add a static net-growth screen, not its profile/native-cost model.
    let private tinyBytes=16
    let private maximumBytes=100

    let rewrite (program:Lir.Program) references (binary:byte array) =
        try
            let requested=program.Declarations |> List.choose (fun declaration ->
                match declaration.Value.Node with
                | Lir.TargetImport(path,_) -> ClrBindings.tryMethodLink path |> Option.filter _.Inline |> Option.map (fun link -> link,Some declaration.Span)
                | _ -> None)
            if requested.IsEmpty then Ok binary
            else
                use resolver=new DefaultAssemblyResolver()
                let platform=match AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") with :? string as paths -> paths.Split(Path.PathSeparator) |> Array.toList | _ -> []
                for directory in references @ platform |> List.map Path.GetDirectoryName |> List.distinct do resolver.AddSearchDirectory directory
                use stream=new MemoryStream(binary,false)
                let reader=ReaderParameters(AssemblyResolver=resolver,InMemory=true)
                use moduleDef=ModuleDefinition.ReadModule(stream,reader)
                let sources=Dictionary<string,ModuleDefinition>()
                try
                    let allowed=Dictionary<string,MethodDefinition * FScript.Language.Span option>()
                    for link,span in requested do
                        let typ=System.Type.GetType(link.DeclaringType,true) |> nonNull
                        let parameters=link.Parameters |> Array.map (fun name -> System.Type.GetType(name,true) |> nonNull)
                        let method=typ.GetMethod(link.MemberName,Reflection.BindingFlags.Public ||| Reflection.BindingFlags.Static ||| Reflection.BindingFlags.Instance,null,parameters,null)
                        if isNull method then reject span $"Inline method '{link.MemberName}' cannot be resolved."
                        let method=nonNull method
                        let path=method.Module.FullyQualifiedName
                        let source=
                            match sources.TryGetValue path with
                            | true,value -> value
                            | _ -> let value=ModuleDefinition.ReadModule(path,reader) in sources.Add(path,value);value
                        let definition=source.LookupToken(method.MetadataToken) :?> MethodDefinition
                        allowed.[ClrBindings.inlineMarkerName ("clr:"+Text.Json.JsonSerializer.Serialize link)]<-(definition,span)
                    let ownsMarker (target:MethodReference) =
                        allowed.ContainsKey target.Name && Object.ReferenceEquals(target.DeclaringType.GetElementType().Scope,moduleDef)
                    let rec types (typ:TypeDefinition) = seq {yield typ;for nested in typ.NestedTypes do yield! types nested}
                    let mutable changed=false
                    for typ in moduleDef.Types |> Seq.collect types do
                        for method in typ.Methods do
                            if method.HasBody then
                                let firstSite () = method.Body.Instructions |> Seq.tryPick (fun instruction ->
                                    match instruction.Operand with
                                    | :? MethodReference as target when instruction.OpCode=OpCodes.Call && ownsMarker target -> Some(instruction,allowed.[target.Name])
                                    | _ -> None)
                                let budget=max 32 (min 256 (encodedSize method.Body))
                                let mutable growth=0
                                let mutable site=firstSite ()
                                while site.IsSome do
                                    let instruction,(callee,span)=site.Value
                                    let originalCall () = instruction.Operand<-moduleDef.ImportReference callee
                                    if not callee.HasBody then validate span callee
                                    let size=callee.Body.CodeSize
                                    match forwardingTarget callee with
                                    | Some target ->
                                        validate span callee
                                        instruction.Operand<-moduleDef.ImportReference target
                                    | None when size>maximumBytes || callee.Parameters.Count>32 || callee.Body.Variables.Count>32 || growth>=budget -> originalCall ()
                                    | None ->
                                        validate span callee
                                        let original=method.Body
                                        let before=encodedSize original
                                        let trial,copies=copyBody method
                                        method.Body<-trial
                                        trial.SimplifyMacros()
                                        let arguments=expand span moduleDef method copies.[instruction] callee
                                        cleanup trial arguments
                                        let delta=encodedSize trial-before
                                        let limit=if size<=tinyBytes then 32 else 16
                                        if delta<=limit && growth+delta<=budget then growth<-max 0 (growth+delta)
                                        else method.Body<-original;originalCall ()
                                    changed<-true
                                    site<-firstSite ()
                    for typ in moduleDef.Types |> Seq.collect types do
                        for method in typ.Methods do
                            if method.HasBody then
                                for instruction in method.Body.Instructions do
                                    match instruction.Operand with
                                    | :? MethodReference as target when ownsMarker target ->
                                        let _,span=allowed.[target.Name]
                                        reject span $"Inline requires a direct typed call; unsupported '{instruction.OpCode.Name}' reference remains."
                                    | _ -> ()
                    if not changed then Ok binary
                    else
                        for typ in moduleDef.Types |> Seq.collect types do
                            for method in typ.Methods |> Seq.toArray do
                                if allowed.ContainsKey method.Name then typ.Methods.Remove method |> ignore
                        pruneReferences moduleDef (sources.Values |> Seq.map (fun source -> source.Assembly.Name.Name) |> Set.ofSeq)
                        // Cecil rebuilds metadata tokens, branch offsets, and maxstack on write.
                        use output=new MemoryStream()
                        moduleDef.Write(output,WriterParameters(Timestamp=Nullable 0u,DeterministicMvid=true))
                        Ok(output.ToArray())
                finally
                    for source in sources.Values do source.Dispose()
        with
        | Rejected diagnostic -> Error [diagnostic]
        | error -> Error [{Code="FSN7202";Stage=DotNetGeneration;Span=None;Message="FFI Inline rewriting failed: "+error.Message}]
