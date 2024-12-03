open System.Text.RegularExpressions
open System.IO

let readContents filename = File.ReadAllText filename

// Part 1
let getMuls input =
    let rx =
        Regex(@"mul\((\d{1,3}),(\d{1,3})\)", RegexOptions.Compiled ||| RegexOptions.Multiline)

    let matches = rx.Matches(input)
    Seq.map (fun (x: Match) -> (int (x.Groups[1].ToString()) * int (x.Groups[2].ToString()))) matches


// part 2
type Command =
    | Mul of f: int * s: int
    | Deactivate
    | Active

let getCommands input =
    let rx =
        Regex(@"mul\((\d{1,3}),(\d{1,3})\)|don't\(\)|do\(\)", RegexOptions.Compiled ||| RegexOptions.Multiline)

    let matches = rx.Matches(input)

    let parseCommand (finding: Match) =
        match finding.Value with
        | "don't()" -> Deactivate
        | "do()" -> Active
        | _ -> Mul(int (finding.Groups[1].ToString()), (int (finding.Groups[2].ToString())))

    Seq.map parseCommand matches

let executeCommands (commandList: seq<Command>) =
    let evaluateCommand (active, acc) command =
        match (active, command) with
        | (true, Mul(f, s)) -> (true, acc + (f * s))
        | (_, Active) -> (true, acc)
        | (_, Deactivate) -> (false, acc)
        | _ -> (active, acc)

    Seq.fold evaluateCommand (true, 0) commandList |> snd


let fileContents = readContents "data.txt"

// Part 1
fileContents |> getMuls |> Seq.sum |> printfn "%d"
// Part 2
fileContents |> getCommands |> executeCommands |> printfn "%d"
