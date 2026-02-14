namespace FScript.LanguageServer

#nowarn "3261"
#nowarn "3264"

open System
open System.Collections
open System.Text.Json.Nodes
open Microsoft.FSharp.Reflection
open FScript.Language

module AstJson =
    let private jsonNull : JsonNode = JsonValue.Create<string option>(None)

    let private isOptionType (t: System.Type) =
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

    let rec private toJsonNodeInternal (value: objnull) : JsonNode =
        match value with
        | null -> jsonNull
        | :? JsonNode as node -> node
        | :? string as s -> JsonValue.Create(s)
        | :? bool as b -> JsonValue.Create(b)
        | :? int as i -> JsonValue.Create(i)
        | :? int64 as i -> JsonValue.Create(i)
        | :? float as f -> JsonValue.Create(f)
        | :? decimal as d -> JsonValue.Create(d)
        | :? char as c -> JsonValue.Create(string c)
        | _ ->
            let t = value.GetType()

            if isOptionType t then
                let case, fields = FSharpValue.GetUnionFields(value, t, true)
                if case.Name = "None" then
                    jsonNull
                else
                    toJsonNodeInternal fields[0]
            elif FSharpType.IsUnion(t, true) then
                let case, fields = FSharpValue.GetUnionFields(value, t, true)
                let result = JsonObject()
                result["kind"] <- JsonValue.Create(case.Name)
                let fieldInfos = case.GetFields()
                for i = 0 to fields.Length - 1 do
                    result[fieldInfos[i].Name] <- toJsonNodeInternal fields[i]
                result :> JsonNode
            elif FSharpType.IsRecord(t, true) then
                let result = JsonObject()
                let fieldInfos = FSharpType.GetRecordFields(t, true)
                let fieldValues = FSharpValue.GetRecordFields(value, true)
                for i = 0 to fieldInfos.Length - 1 do
                    result[fieldInfos[i].Name] <- toJsonNodeInternal fieldValues[i]
                result :> JsonNode
            elif t.IsArray then
                let result = JsonArray()
                for item in value :?> IEnumerable do
                    result.Add(toJsonNodeInternal item)
                result :> JsonNode
            elif value :? IDictionary then
                let result = JsonArray()
                for item in value :?> IEnumerable do
                    let entry = item.GetType()
                    let keyProp = entry.GetProperty("Key")
                    let valueProp = entry.GetProperty("Value")
                    let pair = JsonObject()
                    let keyValue =
                        if isNull keyProp then null
                        else keyProp.GetValue(item)
                    let itemValue =
                        if isNull valueProp then null
                        else valueProp.GetValue(item)
                    pair["key"] <- toJsonNodeInternal keyValue
                    pair["value"] <- toJsonNodeInternal itemValue
                    result.Add(pair)
                result :> JsonNode
            elif value :? IEnumerable then
                let result = JsonArray()
                for item in value :?> IEnumerable do
                    result.Add(toJsonNodeInternal item)
                result :> JsonNode
            elif t.IsEnum then
                JsonValue.Create(value.ToString())
            else
                JsonValue.Create(value.ToString())

    let toJsonNode (value: objnull) =
        toJsonNodeInternal value

    let programToJson (sourcePath: string) (program: Program) =
        let root = JsonObject()
        root["version"] <- JsonValue.Create("1")
        root["source"] <- JsonValue.Create(sourcePath)
        root["kind"] <- JsonValue.Create("program")
        root["items"] <- toJsonNode (box program)
        root

    let typedProgramToJson (sourcePath: string) (typedProgram: TypeInfer.TypedProgram) =
        let root = JsonObject()
        root["version"] <- JsonValue.Create("1")
        root["source"] <- JsonValue.Create(sourcePath)
        root["kind"] <- JsonValue.Create("typedProgram")
        root["items"] <- toJsonNode (box typedProgram)
        root
