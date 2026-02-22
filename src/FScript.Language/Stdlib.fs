namespace FScript.Language

module Stdlib =
    let reservedNames () : Set<string> =
        BuiltinSignatures.builtinReservedNames
