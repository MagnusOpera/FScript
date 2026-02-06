namespace FScript.Core.Tests

open FScript.Core

module Helpers =
    let parse src = Parser.parseProgram src
    let infer src = src |> parse |> TypeInfer.inferProgram
    let eval src = src |> infer |> Eval.evalProgram
    let evalToString src = src |> eval |> Pretty.valueToString

    let inferWithExterns externs src = src |> parse |> TypeInfer.inferProgramWithExterns externs
    let evalWithExterns externs src = src |> inferWithExterns externs |> Eval.evalProgramWithExterns externs
