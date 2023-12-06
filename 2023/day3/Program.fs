open System
open System.IO

type Part =
    | Symbol
    | ConnectedNumber of number: int
    | Number of number: int
    | Blank

type State = Part[][]

let inStateRange i j (state: State) =
    i >= 0 && i < state.Length && j >= 0 && j < state[i].Length

let getValue i j (state: State) =
    //printfn "%d/%d %d/%d" i state.Length j state[i].Length
    if inStateRange i j state then state[i][j] else Blank

let setValue i j (state: State) value =
    if inStateRange i j state then
        //printfn "Setting %d %d %A" i j value
        state[i][j] <- value
        true
    else
        false

let indexedIterator (state: State) =
    seq {
        for i in 0 .. state.Length - 1 do
            for j in 0 .. state[i].Length - 1 do
                i, j, state[i][j]
    }

let parseState fname =
    let parseLine =
        Seq.map (fun c ->
            match c with
            | '.' -> Blank
            | c when Char.IsDigit(c) -> Number(int <| string c)
            | _ -> Symbol)
        >> Seq.toArray


    File.ReadAllLines(fname) |> Array.map parseLine

let connectNumbersFromSymbols (state: State) =
    let applyNumberConnection i j iRange jRange =
        let indexes =
            Seq.collect (fun iAdjust -> jRange |> Seq.map (fun jAdjust -> i + iAdjust, j + jAdjust)) iRange

        Seq.map
            (fun (ni, nj) ->
                match getValue ni nj state with
                | Number num -> setValue ni nj state (ConnectedNumber num)
                | _ -> false)
            indexes
        |> Seq.fold (fun s c -> s || c) false

    let rec connectNumbersIter (i, j) =
        let foo =
            if i = state.Length then
                state
            else
                let nextIndexes =
                    match i, j with
                    | _, j when j < state[i].Length - 1 -> i, j + 1
                    | _ -> i + 1, 0

                match getValue i j state with
                | Symbol ->
                    printfn "%d %d %A" i j (getValue i j state)
                    applyNumberConnection i j (seq { -1 .. 1 }) (seq { -1 .. 1 }) |> ignore
                    connectNumbersIter nextIndexes
                | _ -> connectNumbersIter nextIndexes

        foo

    connectNumbersIter (0, 0)

let applyNumberConnection (i, j) indexAdjustment (state: State) =
    Seq.fold
        (fun changes (ia, ja) ->
            let ni, nj = i + ia, j + ja

            match getValue ni nj state with
            | Number num -> setValue ni nj state (ConnectedNumber num)
            | _ -> false && changes)
        false
        indexAdjustment

let connectNumbers (state: State) =
    let diagonalIndex =
        seq {
            for i in -1 .. 1 do
                for j in -1 .. 1 do
                    i, j
        }

    let horizontalIndex =
        seq {
            for j in -1 .. 1 do
                (0, j)
        }

    let connectedFromSymbols =
        indexedIterator state
        |> Seq.filter (fun (_, _, c) -> c = Symbol)
        |> Seq.fold
            (fun state (i, j, _) ->
                applyNumberConnection (i, j) diagonalIndex state |> ignore
                state)
            state

    let rec propagateNumberConnection state =
        let changes =
            indexedIterator state
            |> Seq.filter (fun (_, _, c) ->
                match c with
                | ConnectedNumber _ -> true
                | _ -> false)
            |> Seq.fold (fun changes (i, j, _) -> (applyNumberConnection (i, j) horizontalIndex state) || changes) false

        if changes then propagateNumberConnection state else state

    connectedFromSymbols |> propagateNumberConnection

let parseConnectedNumbers (state: State) =
    let parseNumbersFromRow row =
        let stringLine =
            row
            |> Seq.map (fun t ->
                match t with
                | ConnectedNumber c -> string c
                | _ -> " ")
            |> String.Concat

        //printfn "%s %A" stringLine (stringLine.Trim().Split())
        stringLine.Trim().Split() |> Seq.filter (fun x -> x.Length > 0) |> Seq.map int

    let rowHasConnectedNumber =
        Array.exists (fun c ->
            match c with
            | ConnectedNumber _ -> true
            | _ -> false)

    Seq.filter rowHasConnectedNumber state |> Seq.collect parseNumbersFromRow


let part1 fname =
    let state = parseState fname
    let connectedState = connectNumbers state
    parseConnectedNumbers connectedState |> Seq.sum

let filename = "input.txt"
printfn "%A" (part1 filename)
