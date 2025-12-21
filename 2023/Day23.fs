module Day23

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day23.txt")

type Dir = N | S | E | W
type Tile = Path | Forest | Slope of Dir

let parseChar c =
    match c with
    | '.' -> Path
    | '#' -> Forest
    | '^' -> Slope N
    | '>' -> Slope E
    | 'v' -> Slope S
    | '<' -> Slope W
    | _ -> failwith "unexpected"

let xLength = input.[0].Length
let yLength = input.Length

let tiles =
    Array2D.init xLength yLength (fun x y -> input.[y].[x] |> parseChar)

let xStart = tiles.[*, 0] |> Array.findIndex (fun t -> t = Path)
let yStart = 0

let yEnd = yLength - 1

type State = { X: int; Y: int; Unvisited: bool[,]; Steps: int }

let part1() =
    let initialState = { X = xStart; Y = yStart; Unvisited = Array2D.create xLength yLength true; Steps = 0 }

    let rec maxSteps state =
        let x, y = state.X, state.Y

        if y = yEnd then
            state.Steps
        else
            let nextPositions =
                match tiles.[x, y] with
                | Path -> seq {
                    if x > 0 && tiles.[x - 1, y] <> Forest && state.Unvisited.[x - 1, y] then (x - 1, y)
                    if x < xLength - 1 && tiles.[x + 1, y] <> Forest && state.Unvisited.[x + 1, y] then (x + 1, y)
                    if y > 0 && tiles.[x, y - 1] <> Forest && state.Unvisited.[x, y - 1] then (x, y - 1)
                    if y < yLength - 1 && tiles.[x, y + 1] <> Forest && state.Unvisited.[x, y + 1] then (x, y + 1) }
                | Slope N -> seq { if y > 0 && tiles.[x, y - 1] <> Forest && state.Unvisited.[x, y - 1] then (x, y - 1) }
                | Slope E -> seq { if x < xLength - 1 && tiles.[x + 1, y] <> Forest && state.Unvisited.[x + 1, y] then (x + 1, y) }
                | Slope S -> seq { if y < yLength - 1 && tiles.[x, y + 1] <> Forest && state.Unvisited.[x, y + 1] then (x, y + 1) }
                | Slope W -> seq { if x > 0 && tiles.[x - 1, y] <> Forest && state.Unvisited.[x - 1, y] then (x - 1, y) }
                | Forest -> failwith "unexpected"
            
            if Seq.isEmpty nextPositions then
                0
            else
                let newSteps = state.Steps + 1

                nextPositions
                |> Seq.map (fun (nx, ny) ->
                    state.Unvisited.[x, y] <- false
                    let ms = maxSteps { X = nx; Y = ny; Unvisited = state.Unvisited; Steps = newSteps }
                    state.Unvisited.[x, y] <- true
                    ms)
                |> Seq.max
    
    let longestPath = maxSteps initialState

    printfn "Longest path: %i" longestPath

//

type Node = Start | End | Junction

let allNodes = [|
    for x in 0 .. xLength - 1 do
        for y in 0 .. yLength - 1 do
            if tiles.[x, y] <> Forest then
                if y = yStart then
                    yield Start, (x, y)
                elif y = yEnd then
                    yield End, (x, y)
                else
                    let numDirs =
                        (if x > 0 && tiles.[x - 1, y] <> Forest then 1 else 0)
                        + (if x < xLength - 1 && tiles.[x + 1, y] <> Forest then 1 else 0)
                        + (if y > 0 && tiles.[x, y - 1] <> Forest then 1 else 0)
                        + (if y < yLength - 1 && tiles.[x, y + 1] <> Forest then 1 else 0)
                    if numDirs > 2 then
                        yield Junction, (x, y)
    |]

let numNodes = allNodes.Length
let startIndex = allNodes |> Array.findIndex (fun (n, _) -> n = Start)
let endIndex = allNodes |> Array.findIndex (fun (n, _) -> n = End)

let findAdjacentNodesFrom nodeIndex =
    let nodeX, nodeY = snd allNodes.[nodeIndex]
    let initialUnvisited = Array2D.init xLength yLength (fun x y -> tiles.[x, y] <> Forest)
    initialUnvisited.[nodeX, nodeY] <- false

    let rec findSingleNode state =
        let x, y = state.X, state.Y

        match allNodes |> Array.tryFindIndex (fun (_, (nX, nY)) -> x = nX && y = nY) with
        | Some i -> Some (i, state.Steps)
        | None ->
            let nextPositions = [
                if x > 0 && state.Unvisited.[x - 1, y] then (x - 1, y)
                if x < xLength - 1 && state.Unvisited.[x + 1, y] then (x + 1, y)
                if y > 0 && state.Unvisited.[x, y - 1] then (x, y - 1)
                if y < yLength - 1 && state.Unvisited.[x, y + 1] then (x, y + 1) ]

            match nextPositions with
            | [] ->
                None // dead end
            | [ (nx, ny) ] ->
                state.Unvisited.[x, y] <- false
                findSingleNode { state with X = nx; Y = ny; Steps = state.Steps + 1 }
            | _ -> failwith "unexpected junction"
    
    let initialStates =
        [|
            if nodeX > 0 && initialUnvisited.[nodeX - 1, nodeY] then (nodeX - 1, nodeY)
            if nodeX < xLength - 1 && initialUnvisited.[nodeX + 1, nodeY] then (nodeX + 1, nodeY)
            if nodeY > 0 && initialUnvisited.[nodeX, nodeY - 1] then (nodeX, nodeY - 1)
            if nodeY < yLength - 1 && initialUnvisited.[nodeX, nodeY + 1] then (nodeX, nodeY + 1)
        |]
        |> Array.map (fun (x, y) -> { X = x; Y = y; Unvisited = Array2D.copy initialUnvisited; Steps = 1 })
    
    initialStates |> Array.choose findSingleNode

type NodeState = { Current: int; RemainingNodes: Set<int>; Steps: int }

let part2() =
    let adjacentWithSteps = Array.init numNodes findAdjacentNodesFrom

    let initialRemaining = seq { 0 .. numNodes - 1} |> Set.ofSeq |> Set.remove startIndex
    let initialState = { Current = startIndex; RemainingNodes = initialRemaining; Steps = 0 }

    let rec longestPath state =
        if state.Current = endIndex then
            state.Steps
        else
                let options =
                    adjacentWithSteps.[state.Current]
                    |> Array.filter (fun (adj, _) -> state.RemainingNodes |> Set.contains adj)
                if options.Length = 0 then
                    0
                else
                    options
                    |> Array.map (fun (adj, steps) -> longestPath { Current = adj; RemainingNodes = state.RemainingNodes |> Set.remove adj; Steps = state.Steps + steps })
                    |> Array.max
    
    let longest = longestPath initialState

    printfn "Longest path: %i" longest
