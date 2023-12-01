// For more information see https://aka.ms/fsharp-console-apps
open System
open System.IO

let replace (string: string) (old: string) newString = string.Replace(old, newString)

let replaceTable = [("one", "o1e"); ("two", "t2o"); ("three", "t3e"); ("four", "4"); ("five", "5e"); ("six", "6"); ("seven", "7n"); ("eight", "e8t"); ("nine", "n9e") ]

let replaceAll input = Seq.fold (fun current  (old, newString) -> replace current old newString) input replaceTable

let getFirstNumber input = 
    Seq.find  Char.IsDigit input
    |> fun c -> int c - int '0'
let getLastNumber  input = Seq.rev input |> getFirstNumber 

let lines = File.ReadLines


let result file = 
     File.ReadLines(file)
    |> Seq.sumBy (fun line -> 
        let replaced = line  |>  replaceAll 
        sprintf "%d%d" (getFirstNumber replaced)  (getLastNumber replaced) |> int)

    
let file = "input.txt"
printfn "%s %d" file  (result file)
