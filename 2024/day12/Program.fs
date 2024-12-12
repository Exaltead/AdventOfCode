open System.IO

let readGrid filename =
    File.ReadAllLines filename |> Array.map (fun x -> x.ToCharArray())

let isInBounds (grid: array<array<'a>>) x y =
    x >= 0 && x < grid[0].Length && y >= 0 && y < grid.Length

let makeIndexes (grid: array<array<'a>>) =
    seq {
        for y in 0 .. (grid.Length - 1) do
            for x in 0 .. (grid[0].Length - 1) -> (x, y)
    }


let rotateUntil (value: 'a) (content: list<'a>) =
    let rec rotator (value: 'a) (content: list<'a>) =
        match content with
        | head :: tail when head = value -> Some(head :: tail)
        | head :: tail -> rotator value (tail @ [ head ])
        | [] -> failwith "Empty list cannot be rotated"

    if List.contains value content then
        rotator value content
    else
        None


type Direction =
    | Left
    | Up
    | Down
    | Right

let nextInDirection x y dir =
    match dir with
    | Left -> x - 1, y
    | Up -> x, y - 1
    | Right -> x + 1, y
    | Down -> x, y + 1

let rotateClockwise dir =
    match dir with
    | Left -> Up
    | Up -> Right
    | Right -> Down
    | Down -> Left

let rotateCounterClockwise dir =
    match dir with
    | Left -> Down
    | Up -> Left
    | Right -> Up
    | Down -> Right

let parallelDirections f s =
    let (fx, fy) = nextInDirection 0 0 f
    let (sx, sy) = nextInDirection 0 0 s
    (fx - sx = 0) || (fy - sy = 0)

let getAdjacent x y =
    [ Left; Up; Right; Down ] |> List.map (nextInDirection x y)



let getPointParameter (grid: array<array<'a>>) (x, y) =
    List.filter
        (fun dir ->
            match nextInDirection x y dir with
            | (nx, ny) when isInBounds grid nx ny -> grid[ny][nx] <> grid[y][x]
            | _ -> true)
        [ Left; Up; Right; Down ]


let formGroup (startX, startY) (grid: array<array<char>>) =
    let startValue = grid[startY][startX]

    let rec mapper (grouping: Set<int * int>) x y =

        match grid[y][x] = startValue with
        | false -> grouping
        | _ ->
            let updatedGrouping = Set.add (x, y) grouping

            let nextIndexes =
                getAdjacent x y
                |> List.filter (fun (x, y) -> isInBounds grid x y)
                |> List.filter (fun x -> Set.contains x updatedGrouping |> not)

            List.fold (fun grouping (x, y) -> mapper grouping x y) updatedGrouping nextIndexes

    mapper (Set([ (startX, startY) ])) startX startY

let calculateFenceCost grid (group: Set<int * int>) =
    let (area, perimeter) =
        Set.fold
            (fun (area, perimeter) point -> (area + 1, perimeter + (getPointParameter grid point |> List.length)))
            (0, 0)
            group

    area * perimeter

let formAllGroups grid =
    makeIndexes grid
    |> Seq.fold
        (fun (groupings: list<Set<int * int>>) point ->
            match List.exists (fun x -> Set.contains point x) groupings with
            | true -> groupings
            | false -> (formGroup point grid) :: groupings)
        []

let countedEdges (grid: array<array<char>>) (grouping: Set<int * int>) =
    let edges: Map<(int * int), list<Direction>> =
        Set.toList grouping
        |> List.map (fun (x, y) -> (x, y, getPointParameter grid (x, y)))
        |> List.filter (fun (_, _, x) -> List.length x > 0)
        |> List.fold (fun map (x, y, dir) -> Map.add (x, y) dir map) Map.empty

    let getNextIndex faceDirection x y =
        let (linearX, linearY), (diagonalX, diagonalY) =
            match faceDirection with
            | Left -> (x, y - 1), (x - 1, y - 1)
            | Up -> (x + 1, y), (x + 1, y - 1)
            | Right -> (x, y + 1), (x + 1, y + 1)
            | Down -> (x - 1, y), (x - 1, y + 1)


        match Map.tryFind (linearX, linearY) edges with
        | Some(x) when List.contains faceDirection x -> (linearX, linearY, faceDirection)
        | None -> (diagonalX, diagonalY, (rotateCounterClockwise faceDirection))
        | Some(value) -> (diagonalX, diagonalY, (rotateCounterClockwise faceDirection))

    let rotateEdgesToBeConnectedTo from edges =
        let rotatedToContinuing = rotateUntil from edges

        match rotatedToContinuing with
        | Some x -> x
        | None ->
            match rotateUntil (rotateClockwise from) edges with
            | None -> failwith "Incomplete shape"
            | Some x -> x

    let clockwiseStartingPosition edges =
        if List.length edges = 4 then
            Left
        else
            List.filter (fun x -> List.contains (rotateCounterClockwise x) edges |> not) edges
            |> List.head

    let pickEdges edges currentDirection =
        match List.length edges with
        | 2 when parallelDirections edges[0] edges[1] -> [ currentDirection ]
        | _ -> edges

    let crawlEdge x y dir visited =

        let pickFace faces currentFace =
            match List.length faces with
            | 1 -> List.head faces
            | 2 when parallelDirections faces[0] faces[1] -> currentFace
            | 2
            | 3 -> List.last faces
            | _ -> failwith "Should not happen"

        let rec crawler x y prevFaceDirection visited =
            if Set.contains (x, y, prevFaceDirection) visited then
                [], visited
            else
                let filteredEdges = pickEdges edges[(x, y)] prevFaceDirection
                let rotatedFaces = rotateEdgesToBeConnectedTo prevFaceDirection filteredEdges

                let newVisited =
                    match List.length rotatedFaces with
                    | 2 when parallelDirections rotatedFaces[0] rotatedFaces[1] ->
                        Set.add (x, y, prevFaceDirection) visited
                    | _ -> List.fold (fun acc faceDir -> Set.add (x, y, faceDir) acc) visited rotatedFaces

                let nextFaceDirection = pickFace rotatedFaces prevFaceDirection
                let (x, y, nextFace) = getNextIndex nextFaceDirection x y

                let trailEdges, visited = crawler x y nextFace newVisited
                rotatedFaces @ trailEdges, visited


        if List.length edges[(x, y)] = 4 then
            edges[(x, y)], Set(List.map (fun t -> (x, y, t)) edges[(x, y)])
        else
            let trail, visited = crawler x y dir visited

            if (trail.Length > 4) then
                let lastElement = List.last trail
                List.skipWhile (fun x -> x = lastElement) trail, visited
            else
                trail, visited

    let folder (trails: list<list<Direction>>, visited: Set<int * int * Direction>) (x, y) dirs =
        let newTrail, visited = crawlEdge x y (clockwiseStartingPosition dirs) visited

        match newTrail.Length with
        | 0 -> (trails, visited)
        | _ -> (newTrail :: trails), visited

    Map.fold folder ([], Set.empty) edges |> fst

let countUniqueFaces trail =
    List.fold
        (fun (count, prev) current ->
            if current = prev then
                (count, current)
            else
                (count + 1, current))
        (1, List.head trail)
        trail
    |> fst

let countScoreFromGroupings (groupTrails: list<list<Direction>>) area =
    List.sumBy (fun x -> area * countUniqueFaces x) groupTrails

let contents = readGrid "data.txt"
let grouping = formAllGroups contents
// Part 1
grouping
|> List.sumBy (fun x -> calculateFenceCost contents x)
|> printfn "Fence cost %d"


// Part 2
List.map (fun x -> x, countedEdges contents x) grouping
|> List.sumBy (fun (group, trails) -> countScoreFromGroupings trails (Set.count group))
|> printfn "Fence cost with discount %A"
