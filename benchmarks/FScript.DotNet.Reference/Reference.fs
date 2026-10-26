namespace FScript.DotNet.Reference

module Model =
    type Event = { Id: int64; Amount: int64; Lane: int64 }
    type Adjustment = { Gross: int64; Credit: int64 }
    type Decision = Reject | Accept of Event | Revise of Adjustment

module Lists =
    open Model

    // Private in F#: matches an unexported FScript declaration.
    type private UnusedPayload = { Label: string; Value: int64 }
    let private unused_helper value = { Label = "unreachable"; Value = value * value }

    let private affine (scale: int64) (bias: int64) (value: int64) = value * scale + bias

    let numeric (seed: int64) =
        let bias = seed % 97L
        let step = affine 17L bias
        let rec loop remaining state total =
            if remaining = 0L then total
            else
                let next = step state % 1000003L
                let weight = (remaining % 7L) + 1L
                let repeated = next * next + 3L * next + 11L
                let contribution = repeated + (next * next + 3L * next + 11L)
                loop (remaining - 1L) next ((total + contribution * weight) % 1000000007L)
        loop 100000L ((seed % 1000003L) + 1L) 0L

    let private make_event price seed index =
        let row = { Id = index; Amount = price ((index * 37L + seed) % 1009L); Lane = (index + seed) % 7L }
        { row with Amount = row.Amount + 7L }

    let pipeline (seed: int64) =
        let price = affine ((seed % 5L) + 2L) (seed % 17L)
        let values = [1L..5000L] |> List.map (make_event price seed)
        values
        |> List.filter (fun row -> row.Amount % 3L <> 0L)
        |> List.fold (fun total row -> total + row.Amount * (row.Lane + 1L) + row.Id) 0L

    let private classify seed index =
        let amount = (index * 73L + seed) % 1009L
        if index % 5L = 0L then Reject
        elif index % 3L = 0L then Revise { Gross = amount * 4L; Credit = (amount % 19L) + 1L }
        else Accept { Id = index; Amount = amount; Lane = (index + seed) % 7L }

    let private decision_value decision =
        match decision with
        | Reject -> 0L
        | Accept row -> row.Amount * (row.Lane + 1L) + row.Id
        | Revise adjustment -> adjustment.Gross - adjustment.Credit

    let routing (seed: int64) =
        [1L..5000L]
        |> List.map (classify seed)
        |> List.fold (fun total decision -> total + decision_value decision) 0L

    let rec private quicksort (values: int64 list) =
        match values with
        | [] -> []
        | pivot :: rest ->
            let smaller = rest |> List.filter (fun value -> value < pivot)
            let greater = rest |> List.filter (fun value -> value >= pivot)
            (quicksort smaller) @ [pivot] @ (quicksort greater)

    let sorting (seed: int64) =
        let values = [0L..511L] |> List.map (fun index -> (index * 7919L + seed * 17L) % 10007L)
        let sorted = quicksort values
        let count, checksum =
            sorted |> List.fold (fun (index, total) value -> (index + 1L, total + (index + 1L) * value)) (0L, 0L)
        checksum + count

/// Same collection operations on immutable-by-convention arrays: matches the
/// backend representation without replacing the algorithm with hand-written loops.
module Arrays =
    open Model

    let private affine (scale: int64) (bias: int64) (value: int64) = value * scale + bias
    let private make_event price seed index =
        let row = { Id = index; Amount = price ((index * 37L + seed) % 1009L); Lane = (index + seed) % 7L }
        { row with Amount = row.Amount + 7L }

    let pipeline (seed: int64) =
        let price = affine ((seed % 5L) + 2L) (seed % 17L)
        let values = [|1L..5000L|] |> Array.map (make_event price seed)
        values
        |> Array.filter (fun row -> row.Amount % 3L <> 0L)
        |> Array.fold (fun total row -> total + row.Amount * (row.Lane + 1L) + row.Id) 0L

    let private classify seed index =
        let amount = (index * 73L + seed) % 1009L
        if index % 5L = 0L then Reject
        elif index % 3L = 0L then Revise { Gross = amount * 4L; Credit = (amount % 19L) + 1L }
        else Accept { Id = index; Amount = amount; Lane = (index + seed) % 7L }

    let private decision_value decision =
        match decision with
        | Reject -> 0L
        | Accept row -> row.Amount * (row.Lane + 1L) + row.Id
        | Revise adjustment -> adjustment.Gross - adjustment.Credit

    let routing (seed: int64) =
        [|1L..5000L|]
        |> Array.map (classify seed)
        |> Array.fold (fun total decision -> total + decision_value decision) 0L

    let rec private quicksort (values: int64 array) =
        if values.Length = 0 then [||]
        else
            let pivot = values.[0]
            let rest = Array.tail values
            let smaller = rest |> Array.filter (fun value -> value < pivot)
            let greater = rest |> Array.filter (fun value -> value >= pivot)
            Array.append (quicksort smaller) (Array.append [|pivot|] (quicksort greater))

    let sorting (seed: int64) =
        let values = [|0L..511L|] |> Array.map (fun index -> (index * 7919L + seed * 17L) % 10007L)
        let sorted = quicksort values
        let count, checksum =
            sorted |> Array.fold (fun (index, total) value -> (index + 1L, total + (index + 1L) * value)) (0L, 0L)
        checksum + count
