namespace FScript.Language

module FScript =
    let parse = Parser.parseProgram
    let parseWithSourceName = Parser.parseProgramWithSourceName
    let parseFileWithIncludes = IncludeResolver.parseProgramFromFile
    let parseSourceWithIncludes = IncludeResolver.parseProgramFromSourceWithIncludes
    let parseSourceWithIncludesResolver = IncludeResolver.parseProgramFromSourceWithIncludesResolver
    let infer = TypeInfer.inferProgram
    let inferWithExterns = TypeInfer.inferProgramWithExterns
    let eval = Eval.evalProgram
    let evalWithExterns = Eval.evalProgramWithExterns

    let run (source: string) =
        source |> parse |> infer |> eval
