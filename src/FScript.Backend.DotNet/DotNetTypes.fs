namespace FScript.Compiler

type DotNetOutputKind = Library | ConsoleApplication
type DotNetOptions =
    { AssemblyName: string
      Namespace: string
      ClassName: string
      OutputKind: DotNetOutputKind
      References: string list }

type GeneratedBinary = { RelativePath: string; Content: byte array }

/// Executable publishing requires an explicit portable .NET runtime identifier.
type DotNetExecutableOptions =
    { RuntimeIdentifier: string
      AheadOfTime: bool
      /// Opt in to invariant globalization, resource-key errors and reduced diagnostics.
      MinimizeSize: bool }
