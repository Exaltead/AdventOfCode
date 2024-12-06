open System.IO

type Cell =
    | Empty
    | Guard
    | Obstacle

let readMap filename =
    let parseContent input =
        match input with
        | '#' -> Obstacle
        | '^' -> Guard
        | _ -> Empty

    File.ReadAllLines filename
    |> Array.map (fun x -> x.ToCharArray())
    |> Array.map (Array.map parseContent)


type Direction =
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

type Step =
    { x: int; y: int; direction: Direction }

let rec nextStep grid prevStep =
    let curX = prevStep.x
    let curY = prevStep.y

    let (x, y) =
        match prevStep.direction with
        | Left -> (curX - 1, curY)
        | Right -> (curX + 1, curY)
        | Down -> (curX, curY + 1)
        | Up -> (curX, curY - 1)

    match getFromGrid grid x y with
    | Some Guard
    | Some Empty ->
        Some(
            { x = x
              y = y
              direction = prevStep.direction }
        )
    | Some Obstacle ->
        nextStep
            grid
            { prevStep with
                direction = rotateDirection prevStep.direction }
    | _ -> None

let guardStartStep (grid: array<array<Cell>>) =
    let makeIndexes =
        seq {
            for y in 0 .. grid.Length - 1 do
                for x in 0 .. (grid[0].Length - 1) -> (x, y)
        }

    let (x, y) =
        Seq.find
            (fun (x, y) ->
                match grid[y][x] with
                | Guard -> true
                | _ -> false)
            makeIndexes

    { x = x; y = y; direction = Up }

let makeGuardTrail (grid: array<array<Cell>>) =
    let rec simulateGuard takenSteps prevStep =
        match nextStep grid prevStep with
        | None -> Some []
        | Some(step) ->
            match Set.contains step takenSteps with
            | true -> None
            | _ ->
                let restTrail = simulateGuard (Set.add step takenSteps) step

                match restTrail with
                | None -> None
                | Some x -> Some(step :: x)

    let startPosition = guardStartStep grid

    (simulateGuard (Set([ startPosition ])) startPosition)

let rec formPotentialLoopMaps originalMap trail =
    let createModifiedMap (x, y) =
        let copy = Array.map (fun x -> Array.copy x) originalMap
        copy[y][x] <- Obstacle
        copy

    let guardStartPosition = guardStartStep originalMap

    trail
    |> List.map (fun x -> (x.x, x.y))
    |> List.filter (fun (x, y) -> x <> guardStartPosition.x || y <> guardStartPosition.y)
    |> Set.ofList
    |> Set.toList
    |> List.map (createModifiedMap)

let map = readMap "data.txt"
let trail = makeGuardTrail map

// Part 1
trail.Value
|> List.groupBy (fun x -> (x.x, x.y))
|> List.length
|> printfn "Visited cells %d + 1"

// Part 2
formPotentialLoopMaps map (trail.Value)
|> List.filter (fun map -> (makeGuardTrail map).IsNone)
|> List.length
|> printfn "Potential loop points: %d"
