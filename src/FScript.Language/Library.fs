namespace FScript.Language

module FScript =
    let parse = Parser.parseProgram
    let infer = TypeInfer.inferProgram
    let inferWithExterns = TypeInfer.inferProgramWithExterns
    let eval = Eval.evalProgram
    let evalWithExterns = Eval.evalProgramWithExterns

    let run (source: string) =
        source |> parse |> infer |> eval
