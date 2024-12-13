open System.IO
open System.Text.RegularExpressions
open MathNet.Numerics
open System.Numerics


type PointValue =
    { X: uint64
      Y: uint64 }

    static member (-)(p1: PointValue, p2: PointValue) = { X = p1.X - p2.X; Y = p1.Y - p2.Y }

    static member (*)(p1: PointValue, times: uint64) =
        { X = p1.X * (uint64 times)
          Y = p1.Y * (uint64 times) }

    override this.ToString() : string = sprintf "(%d, %d)" this.X this.Y

//let multiplyPoint point times =
//    {X = point.X * times ; Y = point.Y * times}

//let substract

type Machine =
    { A: PointValue
      B: PointValue
      PrizeCoord: PointValue }

let parseMachines fname =
    let contents = File.ReadAllText fname
    let rx = Regex(@"(\d+)+\D+(\d+)", RegexOptions.Compiled ||| RegexOptions.Multiline)

    Array.map
        (fun machineLine ->
            let matches = rx.Matches(machineLine)

            { A =
                { X = uint64 matches[0].Groups[1].Value
                  Y = uint64 matches[0].Groups[2].Value }
              B =
                { X = uint64 matches[1].Groups[1].Value
                  Y = uint64 matches[1].Groups[2].Value }
              PrizeCoord =
                { X = uint64 matches[2].Groups[1].Value
                  Y = uint64 matches[2].Groups[2].Value } })
        (contents.Split("\n\n"))

let solveMachine (machine: Machine) maxPresses =
    let getAllXSolutions maxBRepeats =
        seq { 0..maxBRepeats }
        |> Seq.map (fun bRepeats ->
            let remaining = machine.PrizeCoord - (machine.B * uint64 bRepeats)

            match remaining.X / machine.A.X, remaining.Y / machine.A.Y with
            | countX, countY when countX > maxPresses || countY > maxPresses -> None
            | countX, countY when countX = countY && machine.A * countX = remaining ->
                Some(3UL * countX + uint64 bRepeats)
            | _ -> None)

        |> Seq.choose id
        |> Seq.toList

    let maxBRepeats =
        min (machine.PrizeCoord.X / machine.B.X) (machine.PrizeCoord.Y / machine.B.Y)

    let solutions = getAllXSolutions (min maxBRepeats maxPresses |> int)

    if List.length solutions > 0 then
        List.min solutions
    else
        0UL

let raisePrizePoint machine =
    { machine with
        PrizeCoord =
            { X = machine.PrizeCoord.X + 10000000000000UL
              Y = machine.PrizeCoord.Y + 10000000000000UL } }


let getLineCoefficients (targetValue: uint64) (aIncrement: uint64) (bIncrement: uint64) =
    let a = new BigInteger(aIncrement)
    let bias = BigRational.FromBigIntFraction(new BigInteger(targetValue), a)
    let angle = BigRational.FromBigIntFraction(-new BigInteger(bIncrement), a)
    angle, bias

let getEquationSolution (a1: BigRational) (b1: BigRational) a2 b2 =
    let x = (b2 - b1) / (a1 - a2)
    let y = a1 * x + b1
    x, y

let getCost (machine: Machine) =
    let xAngle, xBias = getLineCoefficients machine.PrizeCoord.X machine.A.X machine.B.X
    let yAngle, yBias = getLineCoefficients machine.PrizeCoord.Y machine.A.Y machine.B.Y

    match xAngle = yAngle, xBias = yBias with
    | true, true -> failwith "Implement later, apparently there was no need"
    | true, false -> 0UL
    | _ ->
        let bPresses, aPresses = getEquationSolution xAngle xBias yAngle yBias

        match aPresses.IsInteger, aPresses.IsPositive, bPresses.IsInteger, bPresses.IsPositive with
        | true, true, true, true ->
            (uint64 <| BigRational.ToBigInt aPresses) * 3UL
            + (BigRational.ToBigInt bPresses |> uint64)
        | _ -> 0UL

let timeCall (func: ('a -> 'b)) input =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let result = func input
    stopWatch.Stop()
    printfn "Result %A in %A" result stopWatch.Elapsed



let machines = parseMachines "data.txt"

// Part 1
let part1 x =
    Array.map (fun x -> solveMachine x 100UL) x |> Array.sum

timeCall part1 machines

// Part 2
let part2 x =
    Array.map (fun x -> getCost x) x |> Array.sum

timeCall part2 (Array.map raisePrizePoint machines)
