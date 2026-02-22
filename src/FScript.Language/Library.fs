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
    let compile = Executable.compile
    let compileWithExterns = Executable.compileWithExterns
    let loadExecutable = Executable.load
    let execute = Executable.execute
    let executeWithState = Executable.executeWithState

    let run (source: string) =
        source |> parse |> infer |> compile |> execute
