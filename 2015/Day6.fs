module Day6

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day6.txt")

type Range = { Min: int; Max: int }

type Command = TurnOn | TurnOff | Toggle
type Instruction = { Command: Command; X: Range; Y: Range }

let parsePair (s: string) =
    match s.Split(',') with
    | [| n1; n2 |] -> Int32.Parse(n1), Int32.Parse(n2)
    | _ -> failwith "unexpected"

let parseLine (s: string) =
    let command, remaining =
        match s with
        | st when st.StartsWith("toggle") -> Toggle, s.Substring(7)
        | st when st.StartsWith("turn on") -> TurnOn, s.Substring(8)
        | st when st.StartsWith("turn off") -> TurnOff, s.Substring(9)
        | _ -> failwith "unexpected"
    
    let xRange, yRange =
        match remaining.Split(" through ") with
        [| r1; r2 |] ->
            let xMin, yMin = parsePair r1
            let xMax, yMax = parsePair r2
            { Min = xMin; Max = xMax }, { Min = yMin; Max = yMax }
        | _ -> failwith "unexpected"

    { Command = command; X = xRange; Y = yRange }

let instructions =
    input
    |> Array.map parseLine

let updateGrid (grid: bool[,]) instruction =
    for x in instruction.X.Min .. instruction.X.Max do
        for y in instruction.Y.Min .. instruction.Y.Max do
            match instruction.Command with
                | TurnOn -> grid.[x, y] <- true
                | TurnOff -> grid.[x, y] <- false
                | Toggle -> grid.[x, y] <- not grid.[x, y]

let part1() =
    let grid = Array2D.create 1000 1000 false
    
    Array.iter (updateGrid grid) instructions

    let mutable count = 0
    for x in 0 .. 999 do
        for y in 0 .. 999 do
            if grid.[x, y] then count <- count + 1
    
    printfn "Lights on: %i" count

let updateGrid2 (grid: int[,]) instruction =
    for x in instruction.X.Min .. instruction.X.Max do
        for y in instruction.Y.Min .. instruction.Y.Max do
            match instruction.Command with
                | TurnOn -> grid.[x, y] <- grid.[x, y] + 1
                | TurnOff -> grid.[x, y] <- max 0 (grid.[x, y] - 1)
                | Toggle -> grid.[x, y] <- grid.[x, y] + 2

let part2() =
    let grid = Array2D.create 1000 1000 0
    
    Array.iter (updateGrid2 grid) instructions

    let mutable totalBrightness = 0
    for x in 0 .. 999 do
        for y in 0 .. 999 do
            totalBrightness <- totalBrightness + grid.[x, y]
    
    printfn "Total brightness: %i" totalBrightness
