namespace FScript.Runtime

open FScript.Language

/// Data-only process contracts for the process FFI library.
module internal ProcessSignatures =
    let optionsFields =
        Map.ofList [ "workingDirectory", TOption TString; "environment", TMap(TString,TString)
                     "stdin", TOption TString; "timeoutMilliseconds", TOption TInt; "maxOutputBytes", TInt ]
    let optionsType = TRecord optionsFields
    let resultCases =
        Map.ofList [ "Exited", Some(TRecord(Map.ofList ["exitCode", TInt; "stdout", TString; "stderr", TString]))
                     "Failed", Some(TRecord(Map.ofList ["message", TString; "stdout", TString; "stderr", TString])) ]
    let defaultsScheme = Forall([], optionsType)
    let execScheme = Forall([], TFun(TString, TFun(TList TString, TFun(optionsType, TTask (TNamed "ProcessResult")))))
    let optionsDescriptor : HostTypeDescriptor =
        { Name = "ProcessOptions"; HostManaged = false; Fields = optionsFields; TypeParameters = [] }
    let resultDescriptor : HostUnionDescriptor = { Name = "ProcessResult"; Cases = resultCases }
