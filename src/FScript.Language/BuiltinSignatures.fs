namespace FScript.Language

module BuiltinSignatures =
    let builtinSchemes : Map<string, Scheme> =
        [ "ignore", Forall([ 0 ], TFun(TVar 0, TUnit))
          "print", Forall([], TFun(TString, TUnit))
          "Int.tryParse", Forall([], TFun(TString, TOption TInt))
          "Float.tryParse", Forall([], TFun(TString, TOption TFloat))
          "Bool.tryParse", Forall([], TFun(TString, TOption TBool))
          "Int.toString", Forall([], TFun(TInt, TString))
          "Float.toString", Forall([], TFun(TFloat, TString))
          "Bool.toString", Forall([], TFun(TBool, TString))
          "String.replace", Forall([], TFun(TString, TFun(TString, TFun(TString, TString))))
          "String.indexOf", Forall([], TFun(TString, TFun(TString, TOption TInt)))
          "String.toLower", Forall([], TFun(TString, TString))
          "String.toUpper", Forall([], TFun(TString, TString))
          "String.substring", Forall([], TFun(TInt, TFun(TInt, TFun(TString, TOption TString))))
          "String.concat", Forall([], TFun(TString, TFun(TList TString, TString)))
          "String.split", Forall([], TFun(TString, TFun(TString, TList TString)))
          "String.endsWith", Forall([], TFun(TString, TFun(TString, TBool)))
          "List.empty", Forall([ 0 ], TList(TVar 0))
          "List.map", Forall([ 0; 1 ], TFun(TFun(TVar 0, TVar 1), TFun(TList(TVar 0), TList(TVar 1))))
          "List.iter", Forall([ 0 ], TFun(TFun(TVar 0, TUnit), TFun(TList(TVar 0), TUnit)))
          "List.choose", Forall([ 0; 1 ], TFun(TFun(TVar 0, TOption(TVar 1)), TFun(TList(TVar 0), TList(TVar 1))))
          "List.collect", Forall([ 0; 1 ], TFun(TFun(TVar 0, TList(TVar 1)), TFun(TList(TVar 0), TList(TVar 1))))
          "List.exists", Forall([ 0 ], TFun(TFun(TVar 0, TBool), TFun(TList(TVar 0), TBool)))
          "List.contains", Forall([ 0 ], TFun(TVar 0, TFun(TList(TVar 0), TBool)))
          "List.rev", Forall([ 0 ], TFun(TList(TVar 0), TList(TVar 0)))
          "List.distinct", Forall([ 0 ], TFun(TList(TVar 0), TList(TVar 0)))
          "List.fold", Forall([ 0; 1 ], TFun(TFun(TVar 1, TFun(TVar 0, TVar 1)), TFun(TVar 1, TFun(TList(TVar 0), TVar 1))))
          "List.filter", Forall([ 0 ], TFun(TFun(TVar 0, TBool), TFun(TList(TVar 0), TList(TVar 0))))
          "List.length", Forall([ 0 ], TFun(TList(TVar 0), TInt))
          "List.tryFind", Forall([ 0 ], TFun(TFun(TVar 0, TBool), TFun(TList(TVar 0), TOption(TVar 0))))
          "List.tryGet", Forall([ 0 ], TFun(TFun(TVar 0, TBool), TFun(TList(TVar 0), TOption TInt)))
          "List.tryHead", Forall([ 0 ], TFun(TList(TVar 0), TOption(TVar 0)))
          "List.tail", Forall([ 0 ], TFun(TList(TVar 0), TList(TVar 0)))
          "List.append", Forall([ 0 ], TFun(TList(TVar 0), TFun(TList(TVar 0), TList(TVar 0))))
          "Option.defaultValue", Forall([ 0 ], TFun(TVar 0, TFun(TOption(TVar 0), TVar 0)))
          "Option.defaultWith", Forall([ 0 ], TFun(TFun(TUnit, TVar 0), TFun(TOption(TVar 0), TVar 0)))
          "Option.isNone", Forall([ 0 ], TFun(TOption(TVar 0), TBool))
          "Option.isSome", Forall([ 0 ], TFun(TOption(TVar 0), TBool))
          "Option.map", Forall([ 0; 1 ], TFun(TFun(TVar 0, TVar 1), TFun(TOption(TVar 0), TOption(TVar 1))))
          "Map.empty", Forall([ 0 ], TMap(TString, TVar 0))
          "Map.tryGet", Forall([ 0 ], TFun(TString, TFun(TMap(TString, TVar 0), TOption(TVar 0))))
          "Map.containsKey", Forall([ 0 ], TFun(TString, TFun(TMap(TString, TVar 0), TBool)))
          "Map.add", Forall([ 0 ], TFun(TString, TFun(TVar 0, TFun(TMap(TString, TVar 0), TMap(TString, TVar 0)))))
          "Map.ofList", Forall([ 0 ], TFun(TList(TTuple [ TString; TVar 0 ]), TMap(TString, TVar 0)))
          "Map.fold", Forall([ 0; 1 ], TFun(TFun(TVar 1, TFun(TString, TFun(TVar 0, TVar 1))), TFun(TVar 1, TFun(TMap(TString, TVar 0), TVar 1))))
          "Map.count", Forall([ 0 ], TFun(TMap(TString, TVar 0), TInt))
          "Map.filter", Forall([ 0 ], TFun(TFun(TString, TFun(TVar 0, TBool)), TFun(TMap(TString, TVar 0), TMap(TString, TVar 0))))
          "Map.choose", Forall([ 0; 1 ], TFun(TFun(TString, TFun(TVar 0, TOption(TVar 1))), TFun(TMap(TString, TVar 0), TMap(TString, TVar 1))))
          "Map.map", Forall([ 0; 1 ], TFun(TFun(TVar 0, TVar 1), TFun(TMap(TString, TVar 0), TMap(TString, TVar 1))))
          "Map.iter", Forall([ 0 ], TFun(TFun(TString, TFun(TVar 0, TUnit)), TFun(TMap(TString, TVar 0), TUnit)))
          "Map.remove", Forall([ 0 ], TFun(TString, TFun(TMap(TString, TVar 0), TMap(TString, TVar 0)))) ]
        |> Map.ofList

    let builtinReservedNames : Set<string> =
        builtinSchemes
        |> Map.keys
        |> Seq.filter (fun name ->
            name.StartsWith("List.", System.StringComparison.Ordinal)
            || name.StartsWith("Option.", System.StringComparison.Ordinal)
            || name.StartsWith("Map.", System.StringComparison.Ordinal))
        |> Set.ofSeq
