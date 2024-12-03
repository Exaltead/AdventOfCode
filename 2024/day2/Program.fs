open System.IO

let readFile filename =
    File.ReadAllLines(filename)
    |> Array.map (fun line -> line.Split(" ") |> Array.map int |> Array.toList)



let validateCondition (comparator: int -> int -> bool) input =
    match input with
    | [] -> failwith "Invalid argument"
    | (f :: rest) ->
        Seq.fold
            (fun (prev: int, isValid) (current: int) -> (current, isValid && comparator prev current))
            (f, true)
            rest
        |> snd

let isSafe input =
    let validSteps f s =
        let step = abs (f - s)
        step > 0 && step < 4

    ((validateCondition (fun f s -> f > s) input)
     || validateCondition (fun f s -> s > f) input)
    && validateCondition validSteps input

let sequencePermutations input =
    let sequenceWithSkip (skipIndex: int) =
        List.indexed
        >> List.filter (fun (index, _) -> index <> skipIndex)
        >> List.map snd

    List.replicate (List.length input) input |> List.mapi sequenceWithSkip

let isSafeWithDampening input =
    sequencePermutations input |> List.exists isSafe

let countSafe safeFunc input = Seq.filter safeFunc input |> Seq.length


//for line in (readFile "test.data.txt") do
//    printfn "Is ascending %b descending %b safe %b" (isInOrder (fun f s -> f > s) line) (isInOrder (fun f s -> s > f) line) (isSafe line)

let fileContents = readFile "data.txt"
countSafe isSafe fileContents |> printfn "Safe count: %d"
countSafe isSafeWithDampening fileContents |> printfn "Safe with dampening count: %d"
