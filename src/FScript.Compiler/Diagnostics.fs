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

type CompilerExternKind =
    | RuntimeContextIntrinsic
    | PortableRuntimeIntrinsic
    | UnsupportedHostExtern

type CompilerExtern =
    { Name: string
      Scheme: Scheme
      Arity: int
      Kind: CompilerExternKind }

module RuntimeIntrinsics =
    let schemes =
        Map.ofList
            [ "Console.writeLine", Forall([], TFun(TString, TUnit)) ]

module Diagnostic =
    let hir code message span =
        { Code = code
          Stage = Hir
          Message = message
          Span = span }
