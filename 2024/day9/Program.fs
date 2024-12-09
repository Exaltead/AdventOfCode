open System.IO

let parseContents filename =
    (File.ReadAllText filename).ToCharArray() |> Array.map (string >> int)

type Block =
    | Segment of length: int * value: int
    | Empty of int

let getLength block =
    match block with
    | Segment(x, _) -> x
    | Empty x -> x

let decodeFileChecksum (contents: array<int>) =
    let folder (value: int) trail =
        match trail with
        | [] -> Segment(value = 0, length = value) :: trail
        | head :: _ ->
            match head with
            | Empty _ -> (Segment(length = value, value = 0)) :: trail
            | Segment _ -> Empty value :: trail

    let filesystemIdFolder i cur =
        match cur with
        | Empty _ -> (cur, i)
        | Segment(length, value) -> (Segment(length, value = i), i + 1)

    Array.foldBack folder contents [] |> List.mapFold filesystemIdFolder 0 |> fst

let flattenSegments (decoded: list<Block>) =
    List.toArray decoded |> Array.collect (fun x -> Array.replicate (getLength x) x)

let swap fi si (array: array<'a>) =
    let temp = array[fi]
    array[fi] <- array[si]
    array[si] <- temp

let compactIndividual (contents: array<Block>) =
    let copy = Array.copy contents

    let findLastSegment from =
        Array.findIndexBack
            (fun x ->
                match x with
                | Segment _ -> true
                | _ -> false)
            from

    let compactor (acc: array<Block>) i =
        match acc[i] with
        | Segment _ -> acc
        | Empty _ ->
            let lastIndex = findLastSegment acc

            if lastIndex < i then
                acc
            else
                swap i lastIndex acc
                acc

    let index = seq { for x in 0 .. copy.Length - 1 -> x }

    Seq.fold compactor copy index

let renderBlockList (contents: list<Block>) =
    for x in contents do
        let res =
            match x with
            | Segment(length, value) -> String.replicate length (string value)
            | Empty(length) -> String.replicate length "."

        printf "%s" res

    printfn ""

let compactChunk (contents: array<Block>) =
    let attemptToInsertChunk emptyLength length value =
        match compare emptyLength length with
        | 0 -> [ Segment(length, value) ]
        | 1 -> [ Segment(length, value); Empty(emptyLength - length) ]
        | _ -> failwith "Invalid comparison result"

    let replaceAtIndex (index: int) (replacement: list<'a>) (target: list<'a>) =
        let front, back = List.splitAt index target

        match back with
        | [] -> failwith "Block cannot move backwards"
        | _ :: tail -> front @ replacement @ tail

    let moveSegment blockIndex (length, value) (blocks: list<Block>) =
        let findIndexForInsert (length, _) =
            let foundMatch =
                List.indexed blocks
                |> List.tryFind (fun (i, x) ->
                    match x with
                    | Empty t when i < blockIndex -> t >= length
                    | _ -> false)

            match foundMatch with
            | Some index -> Some index
            | None -> None


        let found = findIndexForInsert (length, value)

        match found with
        | None -> blocks
        | Some i ->
            match i with
            | (index, Empty x) ->
                let replacement = attemptToInsertChunk x length value

                let afterBlockMoved = replaceAtIndex index replacement blocks

                let originalIndex =
                    List.findIndexBack (fun x -> x = Segment(length, value)) afterBlockMoved

                replaceAtIndex originalIndex [ Empty length ] afterBlockMoved
            | _ -> blocks

    let contentsList = Array.toList contents

    let folder (n, blockList) index =
        match contents[index] with
        | Empty _ -> (n + 1, blockList)
        | Segment(length, value) ->
            let foo = moveSegment ((List.length blockList) - n - 1) (length, value) blockList
            (n + 1, foo)


    Seq.fold folder (0, contentsList) (seq { for x in (contents.Length - 1) .. -1 .. 0 -> x })
    |> snd



let contents = parseContents "data.txt" |> decodeFileChecksum

let getChecksum =
    Array.mapi (fun i x ->
        match x with
        | Segment(_, value) -> uint64 (i * value)
        | _ -> 0UL)
    >> Array.sum

// Part 1
flattenSegments contents
|> compactIndividual
|> getChecksum
|> printfn "Compacted file checksum %d"


// Part 2
compactChunk (Array.ofList contents)
|> flattenSegments
|> getChecksum
|> printfn "Non fragmented outcome %d"
