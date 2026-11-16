namespace FScript.Language

module BuiltinTypes =
    let private unknownSpan = Span.mk (Span.pos 0 0) (Span.pos 0 0)

    let environmentType : TypeDef =
        { Name = "Environment"
          TypeParameters = []
          IsRecursive = false
          Fields =
              [ "ScriptName", TRPostfix(TRName "string", "option")
                "Arguments", TRPostfix(TRName "string", "list") ]
          Cases = []
          Import = None
          Span = unknownSpan }

    let typeDefs : TypeDef list = [environmentType]
