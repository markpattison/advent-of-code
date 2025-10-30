module Day22

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day22.txt")
    |> Array.skip 2

type NodeData = { Size: int; Used: int; Avail: int }

let parseLine (s: string) =
    let parts = s.Split(' ', StringSplitOptions.RemoveEmptyEntries)

    let parts1 = parts.[0].Split('-')
    let x = parts1.[1].Substring(1) |> Int32.Parse
    let y = parts1.[2].Substring(1) |> Int32.Parse

    let size = parts.[1].Substring(0, parts.[1].Length - 1) |> Int32.Parse
    let used = parts.[2].Substring(0, parts.[2].Length - 1) |> Int32.Parse
    let avail = parts.[3].Substring(0, parts.[3].Length - 1) |> Int32.Parse

    (x, y, { Size = size; Used = used; Avail = avail })

let allNodeData = input |> Array.map parseLine

let maxX = allNodeData |> Array.map (fun (x, _, _) -> x) |> Array.max
let maxY = allNodeData |> Array.map (fun (_, y, _) -> y) |> Array.max

let initialGrid =
    let grid : NodeData[,] = Array2D.zeroCreate (1 + maxX) (1 + maxY)

    allNodeData
    |> Array.iter (fun (x, y, data) -> grid.[x, y] <- data)

    grid

let sizeGrid =
    Array2D.init (1 + maxX) (1 + maxY) (fun x y -> initialGrid.[x, y].Size)

let initialUsedGrid =
    Array2D.init (1 + maxX) (1 + maxY) (fun x y -> initialGrid.[x, y].Used)

let initialEmptyX, initialEmptyY =
    let mutable x = 0
    let mutable y = 0
    
    for i in 0 .. maxX do
        for j in 0 .. maxY do
            if initialUsedGrid.[i, j] = 0 then
                x <- i
                y <- j
    
    x, y

let part1() =
    let mutable viablePairs = 0

    for nodeA in allNodeData do
        for nodeB in allNodeData do
            let _, _, dataA = nodeA
            let _, _, dataB = nodeB
            if nodeA <> nodeB && dataA.Used <> 0 && dataA.Used <= dataB.Avail then
                viablePairs <- viablePairs + 1

    printfn "Viable pairs: %i" viablePairs

type State =
    {
        UsedGrid: int[,]
        TargetX: int
        TargetY: int
        EmptyX: int
        EmptyY: int
        Steps: int
    }

let initialState =
    {
        UsedGrid = initialUsedGrid
        TargetX = maxX
        TargetY = 0
        EmptyX = initialEmptyX
        EmptyY = initialEmptyY
        Steps = 0
    }

let move state (fromX, fromY, toX, toY) =
    let sizeToMove = state.UsedGrid.[fromX, fromY]
    let availTo = sizeGrid.[toX, toY] - state.UsedGrid.[toX, toY]

    if availTo < sizeToMove then
        failwithf "cannot move"
    else
        let newGrid = Array2D.copy state.UsedGrid
        newGrid.[fromX, fromY] <- 0
        newGrid.[toX, toY] <- newGrid.[toX, toY] + sizeToMove

        {
            UsedGrid = newGrid
            TargetX = if fromX = state.TargetX && fromY = state.TargetY then toX else state.TargetX
            TargetY = if fromX = state.TargetX && fromY = state.TargetY then toY else state.TargetY
            EmptyX = fromX
            EmptyY = fromY
            Steps = state.Steps + 1
        }

let part2() =
    let moves = seq {
        for x in 16 .. -1 .. 0 do
            x, 22, x + 1, 22
        for y in 22 .. -1 .. 1 do
            0, y - 1, 0, y
        for x in 1 .. maxX do
            x, 0, x - 1, 0
        for x in (maxX - 1) .. -1 .. 1 do
            x + 1, 1, x + 1, 0
            x    , 1, x + 1, 1
            x - 1, 1, x    , 1
            x - 1, 0, x - 1, 1
            x    , 0, x - 1, 0
    }

    let finalState = Seq.fold move initialState moves

    printfn "Steps: %i" finalState.Steps
