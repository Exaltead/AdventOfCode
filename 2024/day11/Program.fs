open System.IO

let readNodes fname =
    (File.ReadAllText fname).Split(" ") |> Array.map uint64 |> Array.toList

let splitNumber (number: uint64) =
    let serialized = string number
    let halfPoint = serialized.Length / 2

    let fPart = serialized[.. halfPoint - 1]
    let sPart = serialized[halfPoint..]
    [ uint64 fPart; uint64 sPart ]

let applyRules number =
    match number with
    | 0UL -> [ 1UL ]
    | _ when (string number).Length % 2 = 0 -> splitNumber number
    | _ -> [ number * 2024UL ]

let iterateTimes stones depth =

    Seq.fold (fun acc _ -> List.collect applyRules acc) stones (seq { 1..depth } |> Seq.toList)


let countMemoized targetDepth contents =
    let memoizationTable =
        Array.init 100 (fun _ -> Array.init (targetDepth + 1) (fun _ -> 0UL))

    let rec counter (number: uint64) depth =

        let hit =
            if number < 100UL then
                memoizationTable[int number][depth]
            else
                0UL

        let result =
            match depth, hit with
            | _, x when x > 0UL -> x
            | 0, _ -> 1UL
            | _, _ ->
                match number with
                | 0UL -> counter 1UL (depth - 1)
                | _ ->
                    let serialized = string number

                    match serialized.Length % 2 with
                    | 0 ->
                        let halfPoint = serialized.Length / 2
                        let f = counter (uint64 serialized[.. halfPoint - 1]) (depth - 1)
                        let s = counter (uint64 serialized[halfPoint..]) (depth - 1)
                        f + s
                    | _ -> counter (number * 2024UL) (depth - 1)

        if number < 100UL && hit = 0UL then
            memoizationTable[int number][depth] <- result

        result

    List.sumBy (fun x -> counter x targetDepth) contents




let contents = readNodes "data.txt"
// Part 1 (far suboptimla)
iterateTimes contents 25 |> List.length |> printfn "Depth 25 %A"

// Part 2
countMemoized 75 contents |> printfn "Depth 75 %A"
