open System.IO
open System.Text.RegularExpressions

type Robot =
    { Current: int * int
      Velocity: int * int }

let parseRobots fname =
    let lines = File.ReadAllText(fname)

    let rx =
        Regex(@"(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)", RegexOptions.Compiled ||| RegexOptions.Multiline)

    rx.Matches(lines)
    |> Seq.map (fun (x: Match) ->
        { Current = (int x.Groups[1].Value, int x.Groups[2].Value)
          Velocity = (int x.Groups[3].Value, int x.Groups[4].Value) })


let getRobotPositionIn time width height robot =
    let cx, cy = robot.Current
    let vx, vy = robot.Velocity

    let newPos =
        ((cx + time * (vx + width)) % width, (cy + time * (vy + height)) % height)

    { robot with Current = newPos }


let countQuadrants width height robots =
    let xHalf = width / 2
    let yHalf = height / 2

    let getQuadrant robot =
        let x, y = robot.Current
        x < xHalf, y < yHalf

    List.filter
        (fun x ->
            let (x, y) = x.Current
            x <> xHalf && y <> yHalf)
        robots
    |> List.groupBy getQuadrant
    |> List.map (fun ((a, b), robots) ->
        printfn "%b %b %A" a b (List.length robots)
        List.length robots)
    |> List.reduce (fun a b -> a * b)


let (width, height, file) = (101, 103, "data.txt")

let robots = parseRobots file |> List.ofSeq

let simulated = List.map (getRobotPositionIn 100 width height) robots


simulated |> countQuadrants width height |> printfn "\n%d"


let formGrid width height robots =
    let grid = Array.init (height) (fun _ -> Array.init width (fun _ -> '.'))

    for robo in robots do
        let x, y = robo.Current
        grid[y][x] <- 'X'

    grid

let renderRobots width height robots =
    for y in 0 .. height - 1 do
        for x in 0 .. width - 1 do
            let count = List.filter (fun r -> r.Current = (x, y)) robots |> List.length

            let stringValue =
                match count with
                | 0 -> "."
                | x -> string x

            printf "%s" stringValue

        printfn ""

let renderGrid (grid: array<array<char>>) =
    for y in 0 .. grid.Length - 1 do
        printfn "%s" (System.String grid[y])

let hasDiagonal width length robots =
    let makeDiagonalIndexes (x, y) =
        [ (x, y); ((x - 1), (y + 1)); ((x - 2), (y + 2)); ((x - 3), (y + 3)) ]


    seq {
        for y in 0 .. (length - 5) do
            for x in 0 .. (width - 5) -> (x, y)
    }
    |> Seq.exists (fun (x, y) ->
        let diagonalRobots =
            List.filter (fun robo -> List.contains (x, y) (makeDiagonalIndexes robo.Current)) robots

        (List.length diagonalRobots > 4))

let maybeTree (grid: array<array<char>>) =
    let leftDiagonalIndexes x y =
        [ (x, y); ((x - 1), (y + 1)); ((x - 2), (y + 2)); ((x - 3), (y + 3)) ]

    let rightDiagonalIndexes x y =
        [ (x, y); ((x + 1), (y + 1)); ((x + 2), (y + 2)); ((x + 3), (y + 3)) ]

    let hasValueInIndexes indexes =
        let matches = List.filter (fun (x, y) -> grid[y][x] = 'X') indexes |> List.length
        matches > 3

    seq {
        for y in 0 .. (grid.Length - 5) do
            for x in 5 .. (grid[0].Length - 5) -> (x, y)
    }

    |> Seq.exists (fun (x, y) ->
        hasValueInIndexes (leftDiagonalIndexes x y)
        && hasValueInIndexes (rightDiagonalIndexes x y))

let mutable continueLoop = true
let mutable counter = 0

while continueLoop do
    let robos = List.map (getRobotPositionIn counter width height) robots
    let grid = formGrid width height robos

    if maybeTree grid then

        renderGrid grid

        printf "XMax tree found at %d y/n " counter
        let found = System.Console.ReadLine()

        if found = "y" then
            continueLoop <- false
        else
            counter <- counter + 1
    else
        counter <- counter + 1

printfn "Found at %d" counter
