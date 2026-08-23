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

/// Declarative descriptions of the runtime surface known to the compiler.
/// These values deliberately contain no executable .NET delegates.
module CompilerExternCatalog =
    let private descriptor name scheme arity kind : CompilerExtern =
        { Name = name
          Scheme = scheme
          Arity = arity
          Kind = kind }

    let runtimeContext =
        descriptor "Env" (Forall([], TNamed "Environment")) 0 RuntimeContextIntrinsic

    let portableRuntime =
        RuntimeIntrinsics.schemes
        |> Map.toList
        |> List.map (fun (name, scheme) -> descriptor name scheme 1 PortableRuntimeIntrinsic)

    let unsupportedHostRuntime =
        [ descriptor "Console.readLine" (Forall([], TFun(TUnit, TOption TString))) 1 UnsupportedHostExtern
          descriptor "Fs.readText" (Forall([], TFun(TString, TOption TString))) 1 UnsupportedHostExtern
          descriptor "Fs.exists" (Forall([], TFun(TString, TBool))) 1 UnsupportedHostExtern
          descriptor "Fs.kind" (Forall([], TFun(TString, TNamed "FsKind"))) 1 UnsupportedHostExtern
          descriptor "Fs.createDirectory" (Forall([], TFun(TString, TBool))) 1 UnsupportedHostExtern
          descriptor "Fs.writeText" (Forall([], TFun(TString, TFun(TString, TBool)))) 2 UnsupportedHostExtern
          descriptor "Fs.combinePath" (Forall([], TFun(TString, TFun(TString, TString)))) 2 UnsupportedHostExtern
          descriptor "Fs.parentDirectory" (Forall([], TFun(TString, TOption TString))) 1 UnsupportedHostExtern
          descriptor "Fs.extension" (Forall([], TFun(TString, TOption TString))) 1 UnsupportedHostExtern
          descriptor "Fs.fileNameWithoutExtension" (Forall([], TFun(TString, TString))) 1 UnsupportedHostExtern
          descriptor "Fs.glob" (Forall([], TFun(TString, TOption(TList TString)))) 1 UnsupportedHostExtern
          descriptor "Fs.enumerateFiles" (Forall([], TFun(TString, TFun(TString, TOption(TList TString))))) 2 UnsupportedHostExtern
          descriptor "Regex.matchGroups" (Forall([], TFun(TString, TFun(TString, TOption(TList TString))))) 2 UnsupportedHostExtern
          descriptor "Hash.md5" (Forall([], TFun(TString, TOption TString))) 1 UnsupportedHostExtern
          descriptor "Guid.new" (Forall([ 0 ], TFun(TVar 0, TOption TString))) 1 UnsupportedHostExtern
          descriptor "Json.deserialize" (Forall([ 0 ], TFun(TTypeToken, TFun(TString, TOption(TVar 0))))) 2 UnsupportedHostExtern
          descriptor "Json.serialize" (Forall([ 0 ], TFun(TVar 0, TOption TString))) 1 UnsupportedHostExtern
          descriptor "Xml.queryValues" (Forall([], TFun(TString, TFun(TString, TOption(TList TString))))) 2 UnsupportedHostExtern
          descriptor "Task.spawn" (Forall([ 0 ], TFun(TFun(TUnit, TVar 0), TTask(TVar 0)))) 1 UnsupportedHostExtern
          descriptor "Task.await" (Forall([ 0 ], TFun(TTask(TVar 0), TVar 0))) 1 UnsupportedHostExtern ]

    let defaults = runtimeContext :: (portableRuntime @ unsupportedHostRuntime)

module Diagnostic =
    let hir code message span =
        { Code = code
          Stage = Hir
          Message = message
          Span = span }
