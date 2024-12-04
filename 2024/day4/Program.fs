open System.IO

let readContents filename =
    File.ReadAllLines filename |> Array.map (fun x -> Seq.toArray x)

type Direction =
    | LeftDiagonal
    | RightDiagonal
    | Down
    | Right


let getGridString (grid: array<array<char>>) count x y direction =
    let gridHeight = grid.Length
    let gridWidth = grid[0].Length

    let nextIndexes =
        match direction with
        | LeftDiagonal -> (fun x y -> (x - 1, y + 1))
        | RightDiagonal -> fun x y -> (x + 1, y + 1)
        | Right -> fun x y -> (x + 1, y)
        | Down -> fun x y -> (x, y + 1)

    let rec getCharsRec (nx: int, ny: int) depth nextFunc =
        match depth with
        | 0 -> []
        | _ when nx >= gridWidth || nx < 0 -> []
        | _ when ny >= gridHeight || ny < 0 -> []
        | _ -> grid[ny][nx] :: getCharsRec (nextFunc nx ny) (depth - 1) nextFunc

    getCharsRec (x, y) count nextIndexes |> List.toArray |> System.String


let getMatchesFromGrid grid matcher x y =
    Seq.map
        (fun direction -> (direction, getGridString grid 4 x y direction))
        [ LeftDiagonal; RightDiagonal; Right; Down ]
    |> Seq.filter (fun (_, x) -> matcher x)


let countMatches (grid: array<array<char>>) =
    let matcher content =
        match content with
        | "XMAS" -> true
        | "SAMX" -> true
        | _ -> false

    seq {
    for y in 0 .. (grid.Length - 1) do
        for x in 0 .. (grid[0].Length - 1) -> (x, y)
    }
    |> Seq.map (fun (x, y) -> (x, y, getMatchesFromGrid grid matcher x y))
    |> Seq.map (fun (_, _, x) -> Seq.length x) 
    |> Seq.sum


let isCrossCorner grid x y =
    let rightDiagonal = getGridString grid 3 x y RightDiagonal
    let leftDiagonal = getGridString grid 3 (x + 2) y LeftDiagonal

    let matcher content =
        match content with
        | "MAS"
        | "SAM" -> true
        | _ -> false

    (matcher rightDiagonal) && (matcher leftDiagonal)

let countCrosses (grid: array<array<char>>) =
    seq {
        for y in 0 .. (grid.Length - 3) do
            for x in 0 .. (grid[0].Length - 3) -> (x, y)
    }
    |> Seq.filter (fun (x, y) -> isCrossCorner grid x y)
    |> Seq.length


let contents = readContents "data.txt"
// Part 1

countMatches contents |> printfn "%d"

// Part 2
countCrosses contents |> printfn "%d"
