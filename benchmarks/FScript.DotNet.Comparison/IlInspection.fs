namespace FScript.DotNet.Comparison

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Collections.Generic

module IlInspection =
    type Instruction =
        { Offset: int; Opcode: OpCode; Operand: string
          Target: MethodBase option; BranchTargets: int list }

    type MethodSummary =
        { Name: string; IlBytes: int; Instructions: int; BackwardBranches: int
          SelfCalls: int; TailPrefixes: int; DelegateCalls: int; NewObjects: int
          Boxing: int; Multiplications: int; UnresolvedTokens: int }

    let rec private typeName (typ: Type) =
        if typ.IsGenericParameter then typ.Name
        elif typ.IsArray then typeName (typ.GetElementType() |> nonNull) + "[]"
        elif typ.IsGenericType then
            let name = (typ.GetGenericTypeDefinition().FullName |> Option.ofObj |> Option.defaultValue typ.Name).Split('`').[0]
            name + "<" + (typ.GetGenericArguments() |> Array.map typeName |> String.concat ",") + ">"
        else typ.FullName |> Option.ofObj |> Option.defaultValue typ.Name

    let methodName (method: MethodBase) =
        let owner = method.DeclaringType |> Option.ofObj |> Option.map typeName |> Option.defaultValue "<module>"
        owner + "::" + method.Name + "(" + (method.GetParameters() |> Array.map (fun parameter -> typeName parameter.ParameterType) |> String.concat ",") + ")"

    let methods (assembly: Assembly) =
        assembly.GetTypes()
        |> Array.collect (fun typ ->
            let flags = BindingFlags.DeclaredOnly ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance
            Array.append (typ.GetMethods(flags) |> Array.map (fun method -> method :> MethodBase)) (typ.GetConstructors(flags) |> Array.map (fun method -> method :> MethodBase)))
        |> Array.filter (fun method -> not method.IsAbstract && not (isNull (method.GetMethodBody())))
        |> Array.sortBy methodName

    let private opcodes =
        typeof<OpCodes>.GetFields(BindingFlags.Public ||| BindingFlags.Static)
        |> Array.map (fun field -> let code = field.GetValue(null) |> nonNull :?> OpCode in code.Value, code)
        |> Map.ofArray

    let decode (method: MethodBase) =
        let body = method.GetMethodBody() |> nonNull
        let bytes = body.GetILAsByteArray() |> nonNull
        let result = ResizeArray<Instruction>()
        let mutable position = 0
        while position < bytes.Length do
            let offset = position
            let first = bytes.[position]
            position <- position + 1
            let value =
                if first = 0xfeuy then
                    let second = bytes.[position]
                    position <- position + 1
                    int16 (0xfe00 ||| int second)
                else int16 first
            let code = opcodes.[value]
            let operandStart = position
            let length =
                match code.OperandType with
                | OperandType.InlineNone -> 0
                | OperandType.ShortInlineBrTarget | OperandType.ShortInlineI | OperandType.ShortInlineVar -> 1
                | OperandType.InlineVar -> 2
                | OperandType.InlineI8 | OperandType.InlineR -> 8
                | OperandType.InlineSwitch -> 4 + 4 * BitConverter.ToInt32(bytes, position)
                | _ -> 4
            position <- position + length
            if position > bytes.Length then invalidOp $"Truncated IL in {methodName method}."
            let mutable target = None
            let mutable branches = []
            let operand =
                match code.OperandType with
                | OperandType.InlineNone -> ""
                | OperandType.ShortInlineBrTarget ->
                    let destination = position + int (sbyte bytes.[operandStart])
                    branches <- [destination]
                    $"IL_{destination:x4}"
                | OperandType.InlineBrTarget ->
                    let destination = position + BitConverter.ToInt32(bytes, operandStart)
                    branches <- [destination]
                    $"IL_{destination:x4}"
                | OperandType.InlineSwitch ->
                    let count = BitConverter.ToInt32(bytes, operandStart)
                    branches <- [for index in 0..count-1 -> position + BitConverter.ToInt32(bytes, operandStart+4+4*index)]
                    branches |> List.map (fun destination -> $"IL_{destination:x4}") |> String.concat ", "
                | OperandType.InlineMethod | OperandType.InlineField | OperandType.InlineType | OperandType.InlineTok ->
                    let token = BitConverter.ToInt32(bytes, operandStart)
                    try
                        let typeArguments = method.DeclaringType |> Option.ofObj |> Option.map _.GetGenericArguments() |> Option.defaultValue [||]
                        let methodArguments = match method with :? MethodInfo as method -> method.GetGenericArguments() | _ -> [||]
                        let memberInfo = method.Module.ResolveMember(token, typeArguments, methodArguments) |> nonNull
                        match memberInfo with
                        | :? MethodBase as memberMethod -> target <- Some memberMethod; methodName memberMethod
                        | :? Type as typ -> typeName typ
                        | _ -> memberInfo.ToString() |> nonNull
                    with _ -> $"<unresolved 0x{token:x8}>"
                | OperandType.InlineString -> method.Module.ResolveString(BitConverter.ToInt32(bytes, operandStart)) |> sprintf "%A"
                | OperandType.InlineI8 -> string (BitConverter.ToInt64(bytes, operandStart))
                | OperandType.InlineR -> string (BitConverter.ToDouble(bytes, operandStart))
                | OperandType.ShortInlineR -> string (BitConverter.ToSingle(bytes, operandStart))
                | OperandType.ShortInlineI -> string (sbyte bytes.[operandStart])
                | OperandType.ShortInlineVar -> string bytes.[operandStart]
                | OperandType.InlineVar -> string (BitConverter.ToUInt16(bytes, operandStart))
                | _ -> string (BitConverter.ToInt32(bytes, operandStart))
            result.Add { Offset=offset; Opcode=code; Operand=operand; Target=target; BranchTargets=branches }
        result.ToArray()

    let summarize method =
        let instructions = decode method
        let count predicate = instructions |> Array.filter predicate |> Array.length
        { Name=methodName method; IlBytes=(method.GetMethodBody() |> nonNull).GetILAsByteArray() |> nonNull |> Array.length
          Instructions=instructions.Length
          BackwardBranches=count (fun instruction -> instruction.BranchTargets |> List.exists (fun target -> target <= instruction.Offset))
          SelfCalls=count (fun instruction -> instruction.Target |> Option.exists (fun target -> target.Module=method.Module && target.MetadataToken=method.MetadataToken) && (instruction.Opcode=OpCodes.Call || instruction.Opcode=OpCodes.Callvirt))
          TailPrefixes=count (fun instruction -> instruction.Opcode=OpCodes.Tailcall)
          DelegateCalls=count (fun instruction -> instruction.Target |> Option.exists (fun target ->
              let owner = target.DeclaringType |> Option.ofObj |> Option.map typeName |> Option.defaultValue ""
              (target.Name="Invoke" || target.Name="InvokeFast") && (owner.Contains("FSharpFunc") || owner.StartsWith("System.Func",StringComparison.Ordinal))))
          NewObjects=count (fun instruction -> instruction.Opcode=OpCodes.Newobj)
          Boxing=count (fun instruction -> instruction.Opcode=OpCodes.Box)
          Multiplications=count (fun instruction -> instruction.Opcode=OpCodes.Mul || instruction.Opcode=OpCodes.Mul_Ovf || instruction.Opcode=OpCodes.Mul_Ovf_Un)
          UnresolvedTokens=count (fun instruction -> instruction.Operand.StartsWith("<unresolved",StringComparison.Ordinal)) }

    /// Own-assembly reachability only; external FSharp.Core/BCL implementations
    /// remain external calls, so these counts are not a complete runtime call graph.
    let reachable (assembly: Assembly) (root: MethodBase) =
        let visited = HashSet<int>()
        let queue = Queue<MethodBase>()
        let result = ResizeArray<MethodBase>()
        queue.Enqueue root
        while queue.Count > 0 do
            let method = queue.Dequeue()
            if method.Module.Assembly=assembly && visited.Add method.MetadataToken then
                result.Add method
                for instruction in decode method do
                    match instruction.Target with
                    | Some target when target.Module.Assembly=assembly ->
                        queue.Enqueue target
                        if target.IsConstructor then
                            let owner = target.DeclaringType |> nonNull
                            for memberMethod in owner.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly) do
                                if memberMethod.Name="Invoke" then queue.Enqueue memberMethod
                    | _ -> ()
        result.ToArray()

    let write path assembly =
        let text = System.Text.StringBuilder()
        for method in methods assembly do
            text.AppendLine(methodName method) |> ignore
            for instruction in decode method do
                text.AppendLine($"  IL_{instruction.Offset:x4}: {instruction.Opcode.Name} {instruction.Operand}") |> ignore
            text.AppendLine() |> ignore
        File.WriteAllText(path, text.ToString())

    /// Detect direct-call cycles, including recursion split across lifted helpers.
    /// A zero SelfCalls count alone does not imply that recursion became a loop.
    let hasCallCycle assembly root =
        let reached = reachable assembly root
        let nodes = reached |> Array.map (fun method -> method.MetadataToken, method) |> Map.ofArray
        let visiting, finished = HashSet<int>(), HashSet<int>()
        let rec visit token =
            if visiting.Contains token then true
            elif finished.Contains token then false
            else
                visiting.Add token |> ignore
                let cyclic =
                    decode nodes.[token]
                    |> Array.exists (fun instruction ->
                        if instruction.Opcode<>OpCodes.Call && instruction.Opcode<>OpCodes.Callvirt then false
                        else
                            instruction.Target |> Option.exists (fun target -> target.Module.Assembly=assembly && nodes.ContainsKey target.MetadataToken && visit target.MetadataToken))
                visiting.Remove token |> ignore
                finished.Add token |> ignore
                cyclic
        reached |> Array.exists (fun method -> visit method.MetadataToken)
