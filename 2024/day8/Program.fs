open System.IO

let readGrid filename =
    File.ReadAllLines filename |> Array.map (fun x -> Seq.toArray x)

let makeIndexes (grid: array<array<'a>>) =
    seq {
        for y in 0 .. (grid.Length - 1) do
            for x in 0 .. (grid[0].Length - 1) -> (x, y)
    }

let groupAntennas (contents: array<array<char>>) =
    makeIndexes contents
    |> Seq.filter (fun (x, y) -> contents[y][x] <> '.')
    |> Seq.groupBy (fun (x, y) -> contents[y][x])

type Move = { down: int; right: int }

let getResonantPointsNonHarmonic (x, y) movements =
    List.collect (fun move -> [ (x - move.right, y - move.down); (x + 2 * move.right, y + 2 * move.down) ]) movements


let isInBounds (grid: array<array<'a>>) x y =
    x >= 0 && x < grid[0].Length && y >= 0 && y < grid.Length

let getResonantPointsHarmonic grid (x, y) movements =
    let rec moveWhile condition x y xUpdate yUpdate =
        match condition x y with
        | true -> (y, x) :: moveWhile condition (x + xUpdate) (y + yUpdate) xUpdate yUpdate
        | _ -> []

    let resonantPoints x y move =
        (moveWhile (isInBounds grid) x y -move.right -move.down)
        @ (moveWhile (isInBounds grid) x y move.right move.down)

    List.collect (fun move -> resonantPoints x y move) movements



let calculateResonantPoints (_freq, points) getResonantPoints =


    let rec calculateResonantsForPoint points =
        match points with
        | [] -> []
        | (x, y) :: rest ->
            let movements = List.map (fun (px, py) -> { down = py - y; right = px - x }) rest
            (getResonantPoints (x, y) movements) @ calculateResonantsForPoint rest

    calculateResonantsForPoint (Seq.toList points)

let getNonDuplicateLength grid contents =
    List.filter (fun (x, y) -> isInBounds grid x y) contents
    |> Set.ofList
    |> Set.toList
    |> List.length


let grid = readGrid "data.txt"
let contents = grid |> groupAntennas

// Part 1
Seq.toList contents
|> List.collect (fun x -> calculateResonantPoints x getResonantPointsNonHarmonic)
|> getNonDuplicateLength grid
|> printfn "Resonant point count: %d"

// Part 2
Seq.toList contents
|> List.collect (fun x -> calculateResonantPoints x (getResonantPointsHarmonic grid))
|> List.filter (fun (x, y) -> isInBounds grid x y)
|> getNonDuplicateLength grid
|> printfn "Resonant point with harmonics count: %d"
