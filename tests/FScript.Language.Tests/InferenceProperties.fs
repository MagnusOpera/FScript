namespace FScript.Language.Tests

open System
open FsCheck
open FsCheck.FSharp
open FScript.Language

/// A deliberately small source generator. Expectations come from construction,
/// never from filtering candidates through the inference engine under test.
module internal InferenceProperties =
    type Primitive = Integer | Boolean | Text
    type Shape = Hole | ListOf of Shape | OptionOf of Shape | PairOf of Shape * Shape | BoxOf of Shape | ChoiceOf of Shape
    type Transport =
        | Alias | Forward | Tuple | Record | Option | List | Closure | Branch | LocalLet
        | GenericRecordProjection | GenericRecordPattern | GenericRecordUpdate | GenericUnionMatch
    type Aggregate = RecordContainer | UnionContainer | RecordInUnion | UnionInRecord
    type Sample =
        { First: Primitive
          Second: Primitive
          Shape: Shape
          Transports: Transport list
          Explicit: bool }

    type Scenario =
        | PureBinding | CapturedParameter | SharedOpaque | FreshFactories | IndependentCallback
        | GenericValues of Aggregate | GenericCapturedOpaque of Aggregate
        | GenericFactories of Aggregate | GenericCallbacks of Aggregate
    type Expectation = Accept of FScript.Language.Type | Reject
    type ProgramCase = { Source: string; Expected: Expectation }

    let transports =
        [Alias; Forward; Tuple; Record; Option; List; Closure; Branch; LocalLet
         GenericRecordProjection; GenericRecordPattern; GenericRecordUpdate; GenericUnionMatch]
    let aggregates = [RecordContainer; UnionContainer; RecordInUnion; UnionInRecord]
    let primitivePairs =
        [ for first in [Integer; Boolean; Text] do
              for second in [Integer; Boolean; Text] do
                  if first <> second then yield first, second ]

    let rec private shapeGenerator depth =
        if depth = 0 then Gen.constant Hole
        else
            Gen.frequency
                [ 3, Gen.constant Hole
                  2, shapeGenerator (depth - 1) |> Gen.map ListOf
                  2, shapeGenerator (depth - 1) |> Gen.map OptionOf
                  2, shapeGenerator (depth - 1) |> Gen.map BoxOf
                  2, shapeGenerator (depth - 1) |> Gen.map ChoiceOf
                  1, Gen.map2 (fun left right -> PairOf(left, right)) (shapeGenerator (depth - 1)) (shapeGenerator (depth - 1)) ]

    let generator =
        Gen.sized (fun size -> gen {
            let! first, second = Gen.elements primitivePairs
            let! shape = shapeGenerator (min 2 (size / 10))
            let! count = Gen.choose (0, min 6 (size / 5))
            let! wrappers = Gen.listOfLength count (Gen.elements transports)
            let! explicit = Gen.elements [false; true]
            return { First = first; Second = second; Shape = shape; Transports = wrappers; Explicit = explicit }
        })

    let rec private shrinkShape = function
        | Hole -> Seq.empty
        | ListOf inner -> seq { yield inner; yield! shrinkShape inner |> Seq.map ListOf }
        | OptionOf inner -> seq { yield inner; yield! shrinkShape inner |> Seq.map OptionOf }
        | BoxOf inner -> seq { yield inner; yield! shrinkShape inner |> Seq.map BoxOf }
        | ChoiceOf inner -> seq { yield inner; yield! shrinkShape inner |> Seq.map ChoiceOf }
        | PairOf(left, right) -> seq {
            yield left
            yield right
            yield! shrinkShape left |> Seq.map (fun smaller -> PairOf(smaller, right))
            yield! shrinkShape right |> Seq.map (fun smaller -> PairOf(left, smaller))
          }

    let shrink sample =
        seq {
            // Removing wrappers cannot remove allocation, capture, or the two
            // conflicting uses: those belong to the scenario renderer below.
            if not sample.Transports.IsEmpty then
                yield { sample with Transports = [] }
                for index in 0 .. sample.Transports.Length - 1 do
                    yield { sample with Transports = List.removeAt index sample.Transports }
            for shape in shrinkShape sample.Shape |> Seq.distinct do
                yield { sample with Shape = shape }
            if sample.Explicit then yield { sample with Explicit = false }
            if (sample.First, sample.Second) <> (Integer, Text) then
                yield { sample with First = Integer; Second = Text }
        } |> Seq.distinct

    let arbitrary = Arb.fromGenShrink(generator, shrink)

    // All literals are concrete; no generated acceptance expectation contains
    // an unresolved type variable whose identity could depend on process state.
    let rec ground primitive = function
        | Hole ->
            match primitive with
            | Integer -> TInt, "int", "1"
            | Boolean -> TBool, "bool", "true"
            | Text -> TString, "string", "\"text\""
        | ListOf inner ->
            let typ, annotation, literal = ground primitive inner
            TList typ, $"({annotation}) list", $"[{literal}]"
        | OptionOf inner ->
            let typ, annotation, literal = ground primitive inner
            TOption typ, $"({annotation}) option", $"(Some ({literal}))"
        | PairOf(left, right) ->
            let lt, la, lv = ground primitive left
            let rt, ra, rv = ground primitive right
            TTuple [lt; rt], $"({la} * {ra})", $"({lv}, {rv})"
        | BoxOf inner ->
            let typ, annotation, literal = ground primitive inner
            TApplied("PropertyBox", [typ; TUnit]), $"PropertyBox<{annotation}, unit>", $"(packBox ({literal}) ())"
        | ChoiceOf inner ->
            let typ, annotation, literal = ground primitive inner
            TApplied("PropertyChoice", [typ; TUnit]), $"PropertyChoice<{annotation}, unit>", $"(packChoice ({literal}) ())"

    let private genericPrelude sample =
        let parameters = if sample.Explicit then "<'a, 'b>" else ""
        let arguments = if sample.Explicit then "<'a, 'b>" else ""
        "type PropertyBox<'a, 'b> = { BoxValue: 'a; BoxOther: 'b }\n"
        + "type PropertyChoice<'a, 'b> = | Carried of ('a * 'b) | Vacant\n"
        + $"let packBox{parameters} (value: 'a) (other: 'b) : PropertyBox<'a, 'b> = {{ BoxValue = value; BoxOther = other }}\n"
        + $"let packChoice{parameters} (value: 'a) (other: 'b) : PropertyChoice<'a, 'b> = Carried{arguments} (value, other)\n"
        + "let boxValue (box: PropertyBox<'a, 'b>) = box.BoxValue\n"
        + "let boxOther (box: PropertyBox<'a, 'b>) = box.BoxOther\n"
        + "let choiceValue (choice: PropertyChoice<'a, 'b>) = match choice with | Carried (value, _) -> value | Vacant -> raise \"empty property choice\"\n"
        + "let choiceOther (choice: PropertyChoice<'a, 'b>) = match choice with | Carried (_, other) -> other | Vacant -> raise \"empty property choice\"\n"

    let private pack aggregate value other =
        match aggregate with
        | RecordContainer -> $"(packBox ({value}) ({other}))"
        | UnionContainer -> $"(packChoice ({value}) ({other}))"
        | RecordInUnion -> $"(packChoice (packBox ({value}) ({other})) ())"
        | UnionInRecord -> $"(packBox (packChoice ({value}) ({other})) ())"

    let private unpack aggregate value =
        match aggregate with
        | RecordContainer -> $"(boxValue {value})", $"(boxOther {value})"
        | UnionContainer -> $"(choiceValue {value})", $"(choiceOther {value})"
        | RecordInUnion -> $"(boxValue (choiceValue {value}))", $"(boxOther (choiceValue {value}))"
        | UnionInRecord -> $"(choiceValue (boxValue {value}))", $"(choiceOther (boxValue {value}))"

    let private aggregateType aggregate value other =
        match aggregate with
        | RecordContainer -> TApplied("PropertyBox", [value; other])
        | UnionContainer -> TApplied("PropertyChoice", [value; other])
        | RecordInUnion -> TApplied("PropertyChoice", [TApplied("PropertyBox", [value; other]); TUnit])
        | UnionInRecord -> TApplied("PropertyBox", [TApplied("PropertyChoice", [value; other]); TUnit])

    let private transport index previous = function
        | Alias -> previous
        | Forward -> $"(fun forwarded -> forwarded) {previous}"
        | Tuple -> $"match ({previous}, ()) with | (item, _) -> item"
        | Record -> $"({{| Item = {previous} |}}).Item"
        | Option -> $"match Some {previous} with | Some item -> item | None -> {previous}"
        | List -> $"match [{previous}] with | item :: _ -> item | [] -> {previous}"
        | Closure -> $"fun argument -> {previous} argument"
        | Branch -> $"if true then {previous} else {previous}"
        | LocalLet -> $"\n    let local{index} = {previous}\n    local{index}"
        | GenericRecordProjection -> $"boxValue (packBox {previous} ())"
        | GenericRecordPattern -> $"match packBox {previous} () with | {{ BoxValue = item; BoxOther = _ }} -> item"
        | GenericRecordUpdate -> $"boxValue {{ (packBox {previous} ()) with BoxOther = () }}"
        | GenericUnionMatch -> $"match packChoice {previous} () with | Carried (item, _) -> item | Vacant -> {previous}"

    let private forwarded sample initial =
        sample.Transports
        |> List.indexed
        |> List.fold (fun (bindings, previous) (index, wrapper) ->
            let name = $"stage{index}"
            bindings + $"let {name} = {transport index previous wrapper}\n", name) ("", initial)

    let private identity sample =
        if sample.Explicit then "let identity<'a> (value: 'a) : 'a = value\n"
        else "let identity value = value\n"

    let private cellFactory sample =
        let annotation = if sample.Explicit then " : (unit -> 'a list)" else ""
        // The instance is allocated inside make. The returned function captures
        // that instance; repeated calls to the returned function do not allocate.
        $"let make (){annotation} =\n    let cell = Cell.create []\n    fun () -> Cell.read cell\n"

    let private programsCore scenario sample =
        let firstType, firstAnnotation, firstValue = ground sample.First sample.Shape
        let secondType, secondAnnotation, secondValue = ground sample.Second sample.Shape
        let wrappers, target = forwarded sample "original"
        let checkBindings =
            $"let checkFirst (value: ({firstAnnotation}) list) = value\nlet checkSecond (value: ({secondAnnotation}) list) = value\n"
        let accepted source typ = { Source = source; Expected = Accept typ }
        let rejected source = { Source = source; Expected = Reject }
        match scenario with
        | PureBinding ->
            let source = identity sample + "let original = identity\n" + wrappers
            [ accepted (source + $"({target} ({firstValue}), {target} ({secondValue}))") (TTuple [firstType; secondType]) ]
        | CapturedParameter ->
            let nested = wrappers.Split('\n') |> Array.filter ((<>) "") |> Array.map (fun line -> "    " + line) |> String.concat "\n"
            let prefix = checkBindings + "let capture value =\n    let original () = value\n" + nested + "\n"
            [ accepted (prefix + $"    (checkFirst ({target} ()), checkFirst ({target} ()))\ncapture []") (TTuple [TList firstType; TList firstType])
              rejected (prefix + $"    (checkFirst ({target} ()), checkSecond ({target} ()))\ncapture []") ]
        | SharedOpaque ->
            let prefix = checkBindings + cellFactory sample + "let original = make ()\n" + wrappers
            [ accepted (prefix + $"(checkFirst ({target} ()), checkFirst (original ()))") (TTuple [TList firstType; TList firstType])
              rejected (prefix + $"(checkFirst ({target} ()), checkSecond (original ()))") ]
        | FreshFactories ->
            let prefix = checkBindings + cellFactory sample + "let original = make\n" + wrappers
            [ accepted (prefix + $"let first = {target} ()\nlet second = {target} ()\n(checkFirst (first ()), checkSecond (second ()))") (TTuple [TList firstType; TList secondType]) ]
        | IndependentCallback ->
            let prefix = checkBindings + identity sample + "let cell = Cell.create []\nlet pair = ((fun () -> Cell.read cell), identity)\nlet (read, original) = pair\n" + wrappers
            [ accepted (prefix + $"({target} ({firstValue}), {target} ({secondValue}), checkFirst (read ()))") (TTuple [firstType; secondType; TList firstType]) ]
        | GenericValues aggregate ->
            let constructed = pack aggregate "value" "()"
            let constructors = $"let original value = {constructed}\n" + wrappers
            let first = aggregateType aggregate firstType TUnit
            let second = aggregateType aggregate secondType TUnit
            let value, callback = unpack aggregate "packed"
            let packed = pack aggregate "[]" "identity"
            let purePrefix = checkBindings + identity sample + $"let packed = {packed}\n"
            let mixedPrefix = constructors + $"let first = {target} ({firstValue})\nlet second = {target} ({secondValue})\n"
            [ accepted (constructors + $"({target} ({firstValue}), {target} ({secondValue}))") (TTuple [first; second])
              // A pure empty list is generalizable even inside the same generic
              // value. This is the positive counterpart to an allocated cell.
              accepted (purePrefix + $"(checkFirst {value}, checkSecond {value}, {callback} ({firstValue}), {callback} ({secondValue}))")
                  (TTuple [TList firstType; TList secondType; firstType; secondType])
              accepted (mixedPrefix + "[first; first]") (TList first)
              rejected (mixedPrefix + "[first; second]") ]
        | GenericCapturedOpaque aggregate ->
            let value, _ = unpack aggregate "packed"
            let packed = pack aggregate "(Cell.create [])" "()"
            let prefix = checkBindings
                         + $"let make () =\n    let packed = {packed}\n    fun () -> Cell.read {value}\n"
                         + "let original = make ()\n" + wrappers
            [ accepted (prefix + $"(checkFirst ({target} ()), checkFirst (original ()))") (TTuple [TList firstType; TList firstType])
              rejected (prefix + $"(checkFirst ({target} ()), checkSecond (original ()))") ]
        | GenericFactories aggregate ->
            let first, _ = unpack aggregate "first"
            let second, _ = unpack aggregate "second"
            let packed = pack aggregate "(Cell.create [])" "()"
            let prefix = checkBindings + $"let make () = {packed}\nlet original = make\n" + wrappers
            [ accepted (prefix + $"let first = {target} ()\nlet second = {target} ()\n(checkFirst (Cell.read {first}), checkSecond (Cell.read {second}))")
                  (TTuple [TList firstType; TList secondType]) ]
        | GenericCallbacks aggregate ->
            let value, callback = unpack aggregate "packed"
            let packed = pack aggregate "(Cell.create [])" "identity"
            let prefix = checkBindings + identity sample + $"let packed = {packed}\n"
                         + $"let read () = Cell.read {value}\nlet original = {callback}\n" + wrappers
            let callbacks = $"{target} ({firstValue}), {target} ({secondValue})"
            [ accepted (prefix + $"({callbacks}, checkFirst (read ()))") (TTuple [firstType; secondType; TList firstType])
              rejected (prefix + $"({callbacks}, checkFirst (read ()), checkSecond (read ()))") ]

    let programs scenario sample =
        let rec genericShape = function
            | BoxOf _ | ChoiceOf _ -> true
            | ListOf inner | OptionOf inner -> genericShape inner
            | PairOf(left, right) -> genericShape left || genericShape right
            | Hole -> false
        let genericScenario =
            match scenario with
            | GenericValues _ | GenericCapturedOpaque _ | GenericFactories _ | GenericCallbacks _ -> true
            | _ -> false
        let genericTransport = function
            | GenericRecordProjection | GenericRecordPattern | GenericRecordUpdate | GenericUnionMatch -> true
            | _ -> false
        let prelude =
            if genericScenario || genericShape sample.Shape || List.exists genericTransport sample.Transports then genericPrelude sample
            else ""
        programsCore scenario sample
        |> List.map (fun program -> { program with Source = prelude + program.Source })

    let registration =
        let variable = TVar 0
        let cell = TApplied("Cell", [variable])
        let external name argument result =
            { Name = name; Arity = 1; Scheme = Forall([0], TFun(argument, result))
              RequiredPermissions = HostPermissions.None
              Impl = fun _ _ -> failwith "Inference properties must not execute host code" }
        { HostInterop.empty with
            Types = [{ Name = "Cell"; Fields = Map.empty; HostManaged = true; TypeParameters = [0] }]
            Functions = [external "Cell.create" variable cell; external "Cell.read" cell variable] }

    let check program =
        // Parse failures and unexpected exceptions must fail even for a case
        // whose expected outcome is rejection by the type checker.
        let parsed = Parser.parseProgram program.Source
        let actual =
            try
                let typed = TypeInfer.inferProgramWithRegistration registration parsed
                match List.last typed with
                | TypeInfer.TSExpr expr -> Ok expr.Type
                | _ -> failwith "Generated program must end with an expression"
            with TypeException error -> Error error.Message
        match program.Expected, actual with
        | Accept expected, Ok actual when expected = actual -> ()
        | Reject, Error _ -> ()
        | expected, actual -> failwithf "Expected %A; actual %A\n%s" expected actual program.Source

    let describe scenario sample =
        let sources = programs scenario sample |> List.map (fun program -> $"Expected: {program.Expected}\n{program.Source}") |> String.concat "\n\n"
        $"Generator version: 2\nScenario: {scenario}\n{sample}\n\n{sources}"

    let parseReplay (value: string) =
        let parts = value.Split(',')
        match parts with
        | [| seed; gamma |] -> { Rnd = Rnd(UInt64.Parse seed, UInt64.Parse gamma); Size = None }
        | [| seed; gamma; size |] ->
            let size = Int32.Parse size
            if size < 0 then invalidArg "value" "Replay size must be nonnegative"
            { Rnd = Rnd(UInt64.Parse seed, UInt64.Parse gamma); Size = Some size }
        | _ -> invalidArg "value" "FSCRIPT_INFERENCE_REPLAY must be seed,gamma or seed,gamma,size"

    let defaultReplay seed = { Rnd = Rnd seed; Size = None }

    /// Keep the FsCheck result so the NUnit adapter and harness tests exercise
    /// exactly the same generation, shrinking, and replay path.
    let run replay maxTests arb property =
        let mutable result = None
        let runner =
            { new IRunner with
                member _.OnStartFixture _ = ()
                member _.OnArguments(_, _, _) = ()
                member _.OnShrink(_, _) = ()
                member _.OnFinished(_, value) = result <- Some value }
        let config =
            Config.Quick.WithMaxTest(maxTests).WithStartSize(0).WithEndSize(40)
                .WithReplay(Some replay).WithRunner(runner)
        Check.One(config, Prop.forAll arb property)
        result |> Option.defaultWith (fun () -> failwith "FsCheck did not report a result")

    let failureMessage name scenario result =
        let details = Runner.onFinishedToStringWithFormatter (fun value -> describe scenario (unbox<Sample> value)) name result
        match result with
        | TestResult.Failed(_, _, _, _, _, seed, size) ->
            let aggregateFilter =
                match scenario with
                | GenericValues aggregate | GenericCapturedOpaque aggregate | GenericFactories aggregate | GenericCallbacks aggregate -> $"&Name~{aggregate}"
                | _ -> ""
            details + $"\nReplay the failing case:\nFSCRIPT_INFERENCE_REPLAY='{seed.Seed},{seed.Gamma},{size}' dotnet test tests/FScript.Language.Tests/FScript.Language.Tests.fsproj -c Release --filter 'FullyQualifiedName~InferencePropertyTests.{name}{aggregateFilter}'"
        | _ -> details
