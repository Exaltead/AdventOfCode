open System.IO

type Move =
    | Red of count: int
    | Green of count: int
    | Blue of count: int

type GameTotal = { blue: int; red: int; green: int }

let newGameTotal = { blue = 0; green = 0; red = 0 }

let parseMove (moveString: string) =
    let parts = moveString.Split(' ')
    let count = parts[0] |> int

    match parts[1] with
    | "blue" -> Blue count
    | "red" -> Red count
    | "green" -> Green count
    | _ -> failwith ("Unknown type")

let parseTurn (input: string) =
    input.Split(',') |> Seq.map (fun t -> t.Trim() |> parseMove)

let parseGame (input: string) =

    let parts = input.Split(':')
    let id = parts[0].Split(' ')[1] |> int
    let moves = parts[1].Split(';') |> Seq.collect parseTurn

    let getMax state current =
        match current with
        | Red count -> { state with red = max state.red count }
        | Green count ->
            { state with
                green = max state.green count }
        | Blue count ->
            { state with
                blue = max state.blue count }

    id, Seq.fold getMax newGameTotal moves

let possibleGame game =
    let upperLimit = { red = 12; green = 13; blue = 14 }

    upperLimit.blue >= game.blue
    && upperLimit.green >= game.green
    && upperLimit.red >= game.red

let part1 =
    File.ReadAllLines
    >> Seq.map parseGame
    >> Seq.filter (fun (_, game) -> possibleGame game)
    >> Seq.map (fun (id, _) -> id)
    >> Seq.sum

let part2 =
    File.ReadAllLines
    >> Seq.map parseGame
    >> Seq.map (fun (_, game) -> game.blue * game.green * game.red)
    >> Seq.sum

let file = "input.txt"
printfn "%A %d" (part1 file) (part2 file)
