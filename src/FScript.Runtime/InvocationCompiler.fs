namespace FScript.Runtime

open System.Linq.Expressions
open FScript.Language

module InvocationCompiler =
    let private invokeValueMethod =
        match typeof<Value>.Assembly.GetType("FScript.Language.Eval") with
        | null -> failwith "FScript.Language.Eval type not found"
        | evalType ->
            match evalType.GetMethod("invokeValue") with
            | null -> failwith "Eval.invokeValue not found"
            | methodInfo -> methodInfo

    let compile (typeDefs: Map<string, Type>) (fnValue: Value) : (Value list -> Value) =
        let argsParameter = Expression.Parameter(typeof<Value list>, "args")
        let callExpression =
            Expression.Call(
                invokeValueMethod,
                Expression.Constant(typeDefs, typeof<Map<string, Type>>),
                Expression.Constant(fnValue, typeof<Value>),
                argsParameter
            )
        let lambda = Expression.Lambda<System.Func<Value list, Value>>(callExpression, argsParameter)
        let compiled = lambda.Compile()
        fun args -> compiled.Invoke(args)
