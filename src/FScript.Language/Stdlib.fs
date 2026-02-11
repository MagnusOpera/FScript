namespace FScript.Language

open System.IO
open System.Reflection

module Stdlib =
    let private stdlibResourceNames =
        [ "FScript.Language.Stdlib.List.fss"
          "FScript.Language.Stdlib.Option.fss"
          "FScript.Language.Stdlib.Map.fss" ]

    let private readResourceText (assembly: Assembly) (resourceName: string) =
        use stream = assembly.GetManifestResourceStream(resourceName)
        if isNull stream then
            failwith $"Missing stdlib resource '{resourceName}'"
        use reader = new StreamReader(stream)
        reader.ReadToEnd()

    let loadProgram () : Program =
        let assembly = typeof<Span>.Assembly
        stdlibResourceNames
        |> List.collect (fun resourceName ->
            let source = readResourceText assembly resourceName
            IncludeResolver.parseIncludedSource resourceName source)

    let reservedNames () : Set<string> =
        loadProgram ()
        |> List.collect (function
            | SLet(name, _, _, _, _, _) -> [ name ]
            | SLetRecGroup(bindings, _, _) -> bindings |> List.map (fun (name, _, _, _) -> name)
            | _ -> [])
        |> Set.ofList
