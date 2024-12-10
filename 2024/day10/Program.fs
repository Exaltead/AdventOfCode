open System.IO

let readGrid filename =
    File.ReadAllLines filename
    |> Array.map (fun x -> x.ToCharArray() |> Array.map (string >> int))

let makeIndexes (grid: array<array<'a>>) =
    seq {
        for y in 0 .. (grid.Length - 1) do
            for x in 0 .. (grid[0].Length - 1) -> (x, y)
    }

let isInBounds (grid: array<array<'a>>) x y =
    x >= 0 && x < grid[0].Length && y >= 0 && y < grid.Length

let rec depthSearch (contents: array<array<int>>) curX curY =
    let indexes =
        [ (curX - 1, curY); (curX, curY - 1); (curX + 1, curY); (curX, curY + 1) ]

    let curValue = contents[curY][curX]

    match curValue with
    | 9 -> [ (curX, curY) ]
    | _ ->
        List.filter (fun (x, y) -> isInBounds contents x y) indexes
        |> List.filter (fun (x, y) -> (contents[y][x] - curValue) = 1)
        |> List.collect (fun (x, y) -> depthSearch contents x y)

let countTrails (contents: array<array<int>>) x y =
    depthSearch contents x y |> Set.ofList |> Set.count

let countDistinctTrails contents x y = depthSearch contents x y |> List.length


let trailStarts contents =
    makeIndexes contents |> Seq.filter (fun (x, y) -> contents[y][x] = 0)


let contents = readGrid "data.txt"


// Part 1
trailStarts contents
|> Seq.sumBy (fun (x, y) -> countTrails contents x y)
|> printfn "Scores: %d"


// Part 2
trailStarts contents
|> Seq.sumBy (fun (x, y) -> countDistinctTrails contents x y)
|> printfn "Distinct trails %d"
