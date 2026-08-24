namespace FScript.Runtime

open FScript.Language

module ExportSignatures =
    type ExportedFunctionSignature = FScript.Language.ExportSignatures.ExportedFunctionSignature

    let fromTypedProgram = FScript.Language.ExportSignatures.fromTypedProgram
