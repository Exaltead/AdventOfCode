open System.IO

let readContents filename =
    let lines = File.ReadAllText(filename)
    let parts = lines.Split("\n\n")

    let ordering =
        Array.map (fun (x: string) -> x.Split("|")) (parts[0].Split("\n"))
        |> Array.map (fun commandParts -> 
            (int commandParts[0], int commandParts[1]))

    let printOrders =
        Array.map (fun (x: string) -> x.Split(",")) (parts[1].Split("\n"))
        |> Array.map (fun x -> Array.map int x)

    (ordering, printOrders)

type OrderingRule = { before: list<int>; after: list<int> }

let createOrderingMap (orderings: array<int * int>) : Map<int, OrderingRule> =
    let addToMap (state: Map<int, OrderingRule>) (before, after) =
        Map.change
            before
            (fun x ->
                match x with
                | Some existing ->
                    Some
                        { existing with
                            after = after :: existing.after }
                | None -> Some { after = [ after ]; before = [] })
            state
        |> Map.change after (fun x ->
            match x with
            | Some existing ->
                Some
                    { existing with
                        before = before :: existing.before }
            | None -> Some { after = []; before = [ before ] })

    Array.fold addToMap Map.empty orderings

let orderCompliantFor (orderings: Map<int, OrderingRule>) (order: array<int>) i =
    let value = order[i]
    let orderingRule = orderings[value]

    let isValid banValues seekValue =
        seekValue = value || (List.exists (fun x -> x = seekValue) banValues |> not)
    
    let beforeValid = Array.forall (isValid orderingRule.after) order[..i]
    let afterValid = Array.forall (isValid orderingRule.before) order[i..]
 
    beforeValid && afterValid 

let printOrderValid orderings printOrder =
    Array.indexed printOrder
    |> Array.forall (fun (i, _) -> orderCompliantFor orderings printOrder i)

let getMiddle contents =
    let length = Array.length contents
    contents[(length / 2)]

let sortByOrderings printOrder (orderings: Map<int, OrderingRule>) = 
    let comparator f s = 
        let ordering = orderings[f]
        match (List.contains s ordering.before) with
        | true -> -1
        | false -> 1
        
    Array.sortWith comparator printOrder

let (orderBase, printOrders) = readContents "data.txt"
let orderings = createOrderingMap orderBase

// Part 1
Array.filter (printOrderValid orderings) printOrders
|> Array.map getMiddle
|> Array.sum
|> printfn "%d"

// Part2
Array.filter (fun x -> printOrderValid orderings x |> not) printOrders
|> Array.map (fun x -> sortByOrderings x orderings)
|> Array.map getMiddle
|> Array.sum
|> printfn "%d"
