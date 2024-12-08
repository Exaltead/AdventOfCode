open System.IO

type Equation =
    { testNumber: uint64
      stepNumbers: list<uint64> }

let parseContents filename =
    File.ReadAllLines filename
    |> Seq.map (fun x ->
        let parts = x.Split(": ")
        let steps = parts[1].Split(" ") |> Array.map uint64

        { testNumber = uint64 (parts[0])
          stepNumbers = Array.toList steps })

type Operator =
    | Add
    | Mul
    | Concat

let evaluateOperator operation acc cur =
    match operation with
    | Add -> acc + cur
    | Mul -> acc * cur
    | Concat -> (string acc) + (string cur) |> uint64

let hasSolution equation operators =

    let rec resolveEquation steps acc =
        match steps with
        | [] -> acc = equation.testNumber
        | head :: tail ->
            Seq.map (fun op -> evaluateOperator op acc head) operators
            |> Seq.exists (fun x -> resolveEquation tail x)

    resolveEquation equation.stepNumbers 0UL


let contents = parseContents "data.txt"

// Part 1
Seq.filter (fun x -> hasSolution x [ Add; Mul ]) contents
|> Seq.sumBy (fun x -> x.testNumber)
|> printfn "Test value sum: %d"

//Part 2
Seq.filter (fun x -> hasSolution x [ Add; Mul; Concat ]) contents
|> Seq.sumBy (fun x -> x.testNumber)
|> printfn "Test value sum with concat: %d"