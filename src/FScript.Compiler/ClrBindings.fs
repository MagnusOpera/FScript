namespace FScript.Compiler

open System
open System.Reflection
open System.Text.Json
open FScript.Language

/// Declarative CLR linkage, independent of executable interpreter registrations.
[<CLIMutable>]
type ClrMethodLink =
    { DeclaringType: string
      Parameters: string array
      IsStatic: bool
      IsConstructor: bool }

module ClrBindings =
    let private typeName (value: System.Type) =
        match value.AssemblyQualifiedName with null -> invalidArg "value" "CLR types must have assembly-qualified names." | name -> name

    let methodLink (method: MethodBase) =
        let declaring = method.DeclaringType |> nonNull
        if not method.IsPublic || method.ContainsGenericParameters || not declaring.IsVisible then
            invalidArg "method" "Compiled FFI requires a public, closed method on a public CLR type."
        if method.GetParameters() |> Array.exists (fun parameter -> parameter.ParameterType.IsByRef || parameter.ParameterType.IsPointer) then
            invalidArg "method" "Compiled FFI does not accept by-reference or pointer parameters."
        "clr:" + JsonSerializer.Serialize<ClrMethodLink>(
            { DeclaringType = typeName declaring
              Parameters = method.GetParameters() |> Array.map (fun parameter -> typeName parameter.ParameterType)
              IsStatic = method.IsStatic
              IsConstructor = method.IsConstructor })

    let tryMethodLink (path: string) : ClrMethodLink option =
        if path.StartsWith("clr:{", StringComparison.Ordinal) then
            try JsonSerializer.Deserialize<ClrMethodLink>(path.Substring(4)) |> Option.ofObj with :? JsonException -> None
        else None

    /// Bind an existing host-external declaration to an exact CLR overload.
    /// Instance methods take their receiver as the first curried FScript argument.
    let bindMethod name (method: MethodBase) (program: Lir.Program) =
        let link = methodLink method
        let mutable found = false
        let declarations = program.Declarations |> List.map (fun declaration ->
            if declaration.Name = name then
                found <- true
                { declaration with Value = { declaration.Value with Node = Lir.TargetImport(link, method.Name) } }
            else declaration)
        if not found then invalidArg "name" $"No reachable external declaration named '{name}' exists."
        { program with Declarations = declarations }

    /// Retain a native type projection in the ordinary serialized target-type linkage.
    let bindType name (clrType: System.Type) (program: Lir.Program) =
        if not clrType.IsVisible || clrType.ContainsGenericParameters || clrType.IsByRef || clrType.IsPointer then
            invalidArg "clrType" "Compiled FFI requires a public, closed CLR type."
        let definition = program.Types |> List.tryFind (fun declaration -> declaration.Name = name)
        match definition with
        | None -> invalidArg "name" $"No external type named '{name}' exists."
        | Some definition ->
            let fields = match definition.Definition with TRecord fields | THostManaged(_, fields) -> Map.toList fields | _ -> []
            let imported: Lir.ImportedRecord =
                { Name = name; ModulePath = "clr:" + typeName clrType; MemberName = clrType.FullName |> nonNull
                  IsUnion = match definition.Definition with TUnion _ -> true | _ -> false
                  Fields = fields; Span = definition.Span }
            { program with
                Types = program.Types |> List.map (fun item -> if item.Name = name then { item with Import = Some imported } else item)
                ImportedRecords = imported :: (program.ImportedRecords |> List.filter (fun item -> item.Name <> name)) }
