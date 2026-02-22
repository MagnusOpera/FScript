namespace FScript.Language

module Executable =
    type ExecutableProgram =
        private
            { Externs: ExternalFunction list
              TypedProgram: TypeInfer.TypedProgram }

    let compileWithExterns (externs: ExternalFunction list) (program: TypeInfer.TypedProgram) : ExecutableProgram =
        { Externs = externs
          TypedProgram = program }

    let compile (program: TypeInfer.TypedProgram) : ExecutableProgram =
        compileWithExterns [] program

    let executeWithState (program: ExecutableProgram) : Eval.ProgramState =
        Eval.evalProgramWithExternsState program.Externs program.TypedProgram

    let execute (program: ExecutableProgram) : Value =
        (executeWithState program).LastValue
