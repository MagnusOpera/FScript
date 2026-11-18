namespace FScript.Runtime

open FScript.Language

module internal FScriptSignatures =
    let optionsFields = Map.ofList ["rootDirectory", TOption TString; "permissions", TOption(TList TString); "timeoutMilliseconds", TOption TInt]
    let optionsType = TRecord optionsFields
    let optionsDescriptor : HostTypeDescriptor =
        {Name="FScriptOptions"; HostManaged=false; Fields=optionsFields; TypeParameters=[]}
    let resultDescriptor : HostUnionDescriptor =
        {Name="FScriptResult"; Cases=Map.ofList [
            "Completed", Some(TRecord(Map.ofList ["value",TString; "exitCode",TInt]))
            "Failed", Some(TRecord(Map.ofList ["message",TString]))]}
    let execScheme = Forall([],TFun(TString,TFun(TList TString,TFun(optionsType,TTask(TNamed "FScriptResult")))))
    let defaultValue = VRecord(Map.ofList ["rootDirectory",VOption None; "permissions",VOption None; "timeoutMilliseconds",VOption None])
    let failed message = VUnionCase("FScriptResult","Failed",Some(VRecord(Map.ofList ["message",VString message])))
    let completed value exitCode =
        VUnionCase("FScriptResult","Completed",Some(VRecord(Map.ofList ["value",VString value; "exitCode",VInt(int64 exitCode)])))
