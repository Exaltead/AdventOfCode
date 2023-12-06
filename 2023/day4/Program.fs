open System.IO

type Card =
    { WinningNumbers: Set<int>
      MyNumbers: array<int> }

let parseLine (line: string) =

    let parseNumberSegment (segment: string) =
        segment.Trim().Split(' ')
        |> Array.filter (fun f -> f.Length > 0)
        |> Array.map int

    let parts = line.Split(':')
    let segments = (parts[1]).Split('|') |> Array.map parseNumberSegment

    { WinningNumbers = Set.ofArray segments[0]
      MyNumbers = segments[1] }


let countMatching card =
    Array.filter (fun t -> Set.contains t card.WinningNumbers) card.MyNumbers
    |> Array.length


let part1 fname =
    File.ReadAllLines(fname)
    |> Seq.map (parseLine >> countMatching >> (fun t -> pown 2 (t - 1)))
    |> Seq.sum

let rec score index cardList =
    let matching = countMatching (Array.get cardList index)

    if matching = 0 then
        1
    else
        let treeSum =
            Seq.map (fun index -> score index cardList) (seq { (index + 1) .. (index + matching) })
            |> Seq.sum

        treeSum + 1




let part2 fname =
    let cardArray = File.ReadAllLines(fname) |> Seq.map parseLine |> Array.ofSeq

    let sum, _ =
        Array.fold (fun (sum, i) card -> sum + (score i cardArray), i + 1) (0, 0) cardArray

    sum

let fname = "input.txt"

printfn "%d %d" (part1 fname) (part2 fname)
