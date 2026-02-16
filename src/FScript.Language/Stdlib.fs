namespace FScript.Language

open System.IO
open System.Reflection

module Stdlib =
    let private stdlibResources =
        [ "FScript.Language.Stdlib.List.fss", "List.fss"
          "FScript.Language.Stdlib.Option.fss", "Option.fss"
          "FScript.Language.Stdlib.Map.fss", "Map.fss"
          "FScript.Language.Stdlib.Environment.fss", ".Environment.fss" ]

    let private readResourceText (assembly: Assembly) (resourceName: string) =
        use stream = assembly.GetManifestResourceStream(resourceName)
        if isNull stream then
            failwith $"Missing stdlib resource '{resourceName}'"
        use reader = new StreamReader(stream)
        reader.ReadToEnd()

    let loadProgram () : Program =
        let assembly = typeof<Span>.Assembly
        stdlibResources
        |> List.collect (fun (resourceName, logicalSourceName) ->
            let source = readResourceText assembly resourceName
            IncludeResolver.parseIncludedSource logicalSourceName source)

    let reservedNames () : Set<string> =
        let rec patternBindingNames (pat: Pattern) : string list =
            match pat with
            | PWildcard _
            | PLiteral _
            | PNil _
            | PNone _
            | PTypeRef _ -> []
            | PVar (name, _) -> [ name ]
            | PCons (head, tail, _) -> patternBindingNames head @ patternBindingNames tail
            | PTuple (items, _) -> items |> List.collect patternBindingNames
            | PRecord (fields, _) -> fields |> List.collect (fun (_, p) -> patternBindingNames p)
            | PMap (clauses, tailPattern, _) ->
                let fromClauses =
                    clauses
                    |> List.collect (fun (k, v) -> patternBindingNames k @ patternBindingNames v)
                let fromTail =
                    match tailPattern with
                    | Some tail -> patternBindingNames tail
                    | None -> []
                fromClauses @ fromTail
            | PSome (inner, _) -> patternBindingNames inner
            | PUnionCase (_, _, payload, _) ->
                match payload with
                | Some inner -> patternBindingNames inner
                | None -> []

        loadProgram ()
        |> List.collect (function
            | SLet(name, _, _, _, _, _) -> [ name ]
            | SLetPattern(pattern, _, _, _) -> patternBindingNames pattern
            | SLetRecGroup(bindings, _, _) -> bindings |> List.map (fun (name, _, _, _) -> name)
            | _ -> [])
        |> Set.ofList
