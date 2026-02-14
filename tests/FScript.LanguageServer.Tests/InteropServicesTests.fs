namespace FScript.LanguageServer.Tests

open NUnit.Framework
open FsUnit
open FScript.Language
open FScript.CSharpInterop

[<TestFixture>]
type InteropServicesTests () =
    [<Test>]
    member _.``Interop loads stdlib virtual source`` () =
        let source = InteropServices.tryLoadStdlibSourceText "fscript-stdlib:///Option.fss"
        source.IsSome |> should equal true

    [<Test>]
    member _.``Interop parses and infers a simple script`` () =
        let script = "let add x y = x + y"
        let sourcePath = "/tmp/interop-test.fss"
        let externs = InteropServices.runtimeExternsForSourcePath sourcePath
        let program = InteropServices.parseProgramFromSourceWithIncludes sourcePath script
        let typed, _ = InteropServices.inferProgramWithExternsAndLocalVariableTypes externs program

        let hasAddBinding =
            typed
            |> List.exists (function
                | TypeInfer.TSLet(name, _, _, _, _, _) when name = "add" -> true
                | _ -> false)

        hasAddBinding |> should equal true
