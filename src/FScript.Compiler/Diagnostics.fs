namespace FScript.Compiler

open FScript.Language

type DiagnosticStage =
    | Hir
    | Lowering
    | Interpreter
    | GoGeneration
    | Artifact

type CompilerDiagnostic =
    { Code: string
      Stage: DiagnosticStage
      Message: string
      Span: Span option }

module Diagnostic =
    let hir code message span =
        { Code = code
          Stage = Hir
          Message = message
          Span = span }
