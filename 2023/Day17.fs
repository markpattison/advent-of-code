module Day17

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day17.txt")

let xLength = input.[0].Length
let yLength = input.Length

let blocks =
    Array2D.init xLength yLength (fun x y -> input.[y].[x] |> string |> Int32.Parse)

type State =
    {
        X: int
        Y: int
        Dir: int // N = 0; E = 1; S = 2; W = 3
        Straight: int
    }

let findMinHeatLoss minStraightBlocks maxStraightBlocks =
    // indices are x, y, dir, straight blocks
    let visited = Array4D.create xLength yLength 4 (maxStraightBlocks + 1) false
    let mutable unvisitedBelowInfinity : Set<int * State> = Set.empty
    let distance = Array4D.create xLength yLength 4 (maxStraightBlocks + 1) Int32.MaxValue

    let setDistance node currentDist dist =
        distance.[node.X, node.Y, node.Dir, node.Straight] <- dist
        unvisitedBelowInfinity <- unvisitedBelowInfinity |> Set.remove (currentDist, node)
        unvisitedBelowInfinity <- unvisitedBelowInfinity |> Set.add (dist, node)

    let isVisited node = visited.[node.X, node.Y, node.Dir, node.Straight]
    let setVisited node distance =
        visited.[node.X, node.Y, node.Dir, node.Straight] <- true
        unvisitedBelowInfinity <- unvisitedBelowInfinity |> Set.remove (distance, node)

    let isDestination node = node.X = xLength - 1 && node.Y = yLength - 1 && node.Straight >= minStraightBlocks

    let start1 = { X = 0; Y = 0; Dir = 1; Straight = 0 }
    let start2 = { X = 0; Y = 0; Dir = 2; Straight = 0 }
    setVisited start1 0
    setVisited start2 0
    setDistance start1 0 0
    setDistance start2 0 0

    let findUnvisitedNeighbours node =
        let initial =
            match node.Dir with
            | 0 -> 
                [|
                    if node.Straight >= minStraightBlocks then { X = node.X - 1; Y = node.Y; Dir = 3; Straight = 1 }
                    if node.Straight < maxStraightBlocks then { X = node.X; Y = node.Y - 1; Dir = 0; Straight = node.Straight + 1 }
                    if node.Straight >= minStraightBlocks then { X = node.X + 1; Y = node.Y; Dir = 1; Straight = 1 }
                |]
            | 1 -> 
                [|
                    if node.Straight >= minStraightBlocks then { X = node.X ; Y = node.Y - 1; Dir = 0; Straight = 1 }
                    if node.Straight < maxStraightBlocks then { X = node.X + 1; Y = node.Y; Dir = 1; Straight = node.Straight + 1 }
                    if node.Straight >= minStraightBlocks then { X = node.X; Y = node.Y + 1; Dir = 2; Straight = 1 }
                |]
            | 2 -> 
                [|
                    if node.Straight >= minStraightBlocks then { X = node.X + 1; Y = node.Y; Dir = 1; Straight = 1 }
                    if node.Straight < maxStraightBlocks then { X = node.X; Y = node.Y + 1; Dir = 2; Straight = node.Straight + 1 }
                    if node.Straight >= minStraightBlocks then { X = node.X - 1; Y = node.Y; Dir = 3; Straight = 1 }
                |]
            | 3 -> 
                [|
                    if node.Straight >= minStraightBlocks then { X = node.X; Y = node.Y + 1; Dir = 2; Straight = 1 }
                    if node.Straight < maxStraightBlocks then { X = node.X - 1; Y = node.Y; Dir = 3; Straight = node.Straight + 1 }
                    if node.Straight >= minStraightBlocks then { X = node.X; Y = node.Y - 1; Dir = 0; Straight = 1 }
                |]
            | _ -> failwith "invalid dir"
        
        initial
        |> Array.filter (fun n -> n.X >= 0 && n.X <= xLength - 1 && n.Y >= 0 && n.Y <= yLength - 1 && (not (isVisited n)))
    
    let mutable current = start1
    let mutable currentDistance = 0
    let mutable found = false

    while not found do
        let unvisitedNeighbours = findUnvisitedNeighbours current

        unvisitedNeighbours
        |> Array.iter (fun neighbour ->
            let tentativeDistance = currentDistance + blocks.[neighbour.X, neighbour.Y]
            let currentNeighbourDistance = distance.[neighbour.X, neighbour.Y, neighbour.Dir, neighbour.Straight]
            if tentativeDistance < currentNeighbourDistance then setDistance neighbour currentNeighbourDistance tentativeDistance)
        
        setVisited current currentDistance

        if isDestination current then
            found <- true
        else
            let smallest = Set.minElement unvisitedBelowInfinity  // relies on tuples being sorted by first element
            currentDistance <- fst smallest
            current <- snd smallest

    currentDistance

let part1() =
    let minHeatLoss = findMinHeatLoss 0 3
    printfn "Minimum heat loss: %i" minHeatLoss

let part2() =
    let minHeatLoss = findMinHeatLoss 4 10
    printfn "Minimum heat loss: %i" minHeatLoss
