namespace FScript.Runtime

open System

[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false)>]
type FScriptExternProviderAttribute() =
    inherit Attribute()

