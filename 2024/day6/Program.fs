open System.IO

let readContents filename =
    File.ReadAllLines filename |> Array.map (fun x -> x.ToCharArray())

type Cell =
    | Empty of visited: bool
    | Guard
    | Obstacle

let formMap (contents: array<array<char>>) =
    let parseContent input =
        match input with
        | '#' -> Obstacle
        | '^' -> Guard
        | _ -> Empty false

    Array.map (Array.map parseContent) contents

type Dirrection =
    | Left
    | Right
    | Down
    | Up

let getFromGrid (grid: array<array<'a>>) x y =
    let inBounds (x, y) =
        x >= 0 && x < grid[0].Length && y >= 0 && y < grid.Length

    match (x, y) with
    | (x, y) when inBounds (x, y) -> Some(grid[y][x])
    | _ -> None

let rotateDirection direction =
    match direction with
    | Left -> Up
    | Up -> Right
    | Right -> Down
    | Down -> Left


let makeGuardTrail (grid: array<array<Cell>>) =
    let makeIndexes =
        seq {
            for y in 0 .. grid.Length - 1 do
                for x in 0 .. (grid[0].Length - 1) -> (x, y)
        }

    let isGuard x =
        match x with
        | Guard -> true
        | _ -> false

    let currentPosition =
        Seq.find
            (fun (x, y) ->
                match grid[y][x] with
                | Guard -> true
                | _ -> false)
            makeIndexes

    let rec guardNextPosition (curX, curY) direction =
        let (x, y) =
            match direction with
            | Left -> (curX - 1, curY)
            | Right -> (curX + 1, curY)
            | Down -> (curX, curY + 1)
            | Up -> (curX, curY - 1)


        match getFromGrid grid x y with
        | Some(Empty _) -> Some(x, y, direction)
        | Some Obstacle -> guardNextPosition (curX, curY) (rotateDirection direction)
        | _ -> None

    let rec simulateGuard (curX, curY) direction =
        let moveGuard (nextX, nextY) =
            Array.set (grid[curY]) curX (Empty(true) )
            Array.set (grid[nextY]) nextX Guard


        match guardNextPosition (curX, curY) direction with
        | None ->
            Array.set (grid[curY]) curX (Empty(true) )
            grid
        | Some(x, y, dir) ->
            moveGuard (x, y) |> ignore
            simulateGuard (x, y) dir

    simulateGuard currentPosition Up

let countVisited = 
    let convertToNumber cell = 
        match cell with 
        | Empty(true) -> 1
        | _ -> 0
    Array.map (Array.map convertToNumber >> Array.sum ) >> Array.sum
let printGrid (grid: array<array<Cell>>) =
    for y in 0..(grid.Length - 1) do
        for x in 0..grid[0].Length - 1 do
            let printChar = 
                match grid[y][x] with
                | Guard -> "^"
                | Obstacle -> "#"
                | Empty(true) -> "X"
                | Empty(false) -> "."
            printf "%s" (printChar)
        printfn ""

readContents "data.txt" |> formMap 
|> makeGuardTrail 
|> countVisited
|> printfn "Visited cells %d"
//|> printGrid
