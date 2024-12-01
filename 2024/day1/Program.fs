// For more information see https://aka.ms/fsharp-console-apps
open System.IO

let lines filename = File.ReadAllLines(filename)

let formHistogram (input: seq<int>) =
    let matcher x =
        match x with
        | Some c -> Some(c + 1)
        | None -> Some 1

    Seq.fold (fun (state: Map<int, int>) current -> state.Change(current, matcher)) (Map<int, int> []) input

let applySimilarityScore input (histogram: Map<int, int>) =
    let similarityGetter value =
        match histogram.TryFind(value) with
        | Some x -> x
        | None -> 0

    Seq.sumBy (fun x -> x * similarityGetter x) input

let partSegments (contents: array<string>) =
    let splitter (line: string) =
        let contents = line.Split("   ")
        (contents[0], contents[1])


    Seq.map splitter contents
    |> Seq.fold (fun (first, second) (l, r) -> (int l :: first, int r :: second)) ([], [])



let calculateLocationDiff (left, right) =
    Seq.zip (Seq.sort left) (Seq.sort right)
    |> Seq.map (fun (l: int, r) -> abs (l - r))
    |> Seq.sum

let (f, s) = lines "data.txt" |> partSegments

(f, s) |> calculateLocationDiff |> printfn "%d"
applySimilarityScore f (formHistogram s) |> printfn "%d"
