module Day14

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day14.txt")

type Space = Empty | CubeRock | RoundRock

let parseSpace c =
    match c with
    | '.' -> Empty
    | 'O' -> RoundRock
    | '#' -> CubeRock
    | _ -> failwith "unexpected"

let xLength = input.[0].Length
let yLength = input.Length

let platform =
    let spaces =
        Array2D.init xLength yLength (fun x y -> parseSpace input.[y].[x])
    
    spaces

let tiltNorth (spaces: Space[,]) =
    let mutable canMove = 0

    for x in 0 .. xLength - 1 do
        for y in 1 .. yLength - 1 do
            if spaces.[x, y] = RoundRock && spaces.[x, y - 1] = Empty then
                canMove <- 1
                while y - canMove >= 1 && spaces.[x, y - canMove - 1] = Empty do
                    canMove <- canMove + 1
                spaces.[x, y - canMove] <- RoundRock
                spaces.[x, y] <- Empty

    spaces

let tiltSouth (spaces: Space[,]) =
    let mutable canMove = 0

    for x in 0 .. xLength - 1 do
        for y in yLength - 2 .. -1 .. 0 do
            if spaces.[x, y] = RoundRock && spaces.[x, y + 1] = Empty then
                canMove <- 1
                while y + canMove <= yLength - 2 && spaces.[x, y + canMove + 1] = Empty do
                    canMove <- canMove + 1
                spaces.[x, y + canMove] <- RoundRock
                spaces.[x, y] <- Empty

    spaces

let tiltWest (spaces: Space[,]) =
    let mutable canMove = 0

    for y in 0 .. yLength - 1 do
        for x in 1 .. xLength - 1 do
            if spaces.[x, y] = RoundRock && spaces.[x - 1, y] = Empty then
                canMove <- 1
                while x - canMove >= 1 && spaces.[x - canMove - 1, y] = Empty do
                    canMove <- canMove + 1
                spaces.[x - canMove, y] <- RoundRock
                spaces.[x, y] <- Empty

    spaces

let tiltEast (spaces: Space[,]) =
    let mutable canMove = 0

    for y in 0 .. yLength - 1 do
        for x in xLength - 2 .. -1 .. 0 do
            if spaces.[x, y] = RoundRock && spaces.[x + 1, y] = Empty then
                canMove <- 1
                while x + canMove <= xLength - 2 && spaces.[x + canMove + 1, y] = Empty do
                    canMove <- canMove + 1
                spaces.[x + canMove, y] <- RoundRock
                spaces.[x, y] <- Empty

    spaces

let spinCycle spaces =
    spaces
    |> tiltNorth
    |> tiltWest
    |> tiltSouth
    |> tiltEast

let scorePlatform (spaces: Space[,]) =
    let mutable score = 0

    for x = 0 to xLength - 1 do
        for y = 0 to yLength - 1 do
            if spaces.[x, y] = RoundRock then score <- score + (yLength - y)
    
    score

let part1() =
    let load =
        platform
        |> Array2D.copy
        |> tiltNorth
        |> scorePlatform
    
    printfn "Load: %i" load

type Cycle<'a> =
    {
        StartTime: int64
        Length: int64
        StateAtCycleEnd: 'a
    }

let cycleDetector (initialState: 'a) (toCachedState: 'a -> 'b) (canCheckForCycle: 'a -> bool) update =
    let mutable time = 0L
    let mutable foundCycle = false
    let mutable state = initialState
    let mutable cache : Map<'b, int64> = Map.empty

    let mutable cycle = { StartTime = 0L; Length = 0L; StateAtCycleEnd = initialState }

    while not foundCycle do
        let cachedState = toCachedState state
        if canCheckForCycle state then
            match Map.tryFind cachedState cache with
            | None ->
                cache <- cache.Add(cachedState, time)
                time <- time + 1L
                state <- update state
            | Some cycleStart ->
                cycle <- { StartTime = cycleStart; Length = time - cycleStart; StateAtCycleEnd = state }
                foundCycle <- true
        else
            cache <- cache.Add(cachedState, time)
            time <- time + 1L
            state <- update state            
    
    cycle

let part2() =
    let mutable tilted =
        platform
        |> Array2D.copy

    let cycle = cycleDetector tilted (Array2D.copy) (fun _ -> true) spinCycle
    
    let target = 1000000000L

    let cyclesToSkip = (target - cycle.StartTime) / cycle.Length
    let remainingCycles = target - cyclesToSkip * cycle.Length - cycle.StartTime

    for i in 1L .. remainingCycles do
        tilted <- spinCycle tilted

    let load = scorePlatform tilted
    printfn "Load: %i" load
