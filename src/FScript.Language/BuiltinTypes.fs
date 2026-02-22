namespace FScript.Language

module BuiltinTypes =
    let private unknownSpan = Span.mk (Span.pos 0 0) (Span.pos 0 0)

    let environmentType : TypeDef =
        { Name = "Environment"
          IsRecursive = false
          Fields =
              [ "ScriptName", TRPostfix(TRName "string", "option")
                "Arguments", TRPostfix(TRName "string", "list") ]
          Cases = []
          Span = unknownSpan }

    let fsKindType : TypeDef =
        { Name = "FsKind"
          IsRecursive = false
          Fields = []
          Cases =
              [ "File", Some (TRName "string")
                "Directory", Some (TRName "string")
                "Missing", None ]
          Span = unknownSpan }

    let typeDefs : TypeDef list =
        [ environmentType
          fsKindType ]
