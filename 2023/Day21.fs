module Day21

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day21.txt")

type Tile = Plot | Rock

let xLength = input.[0].Length
let yLength = input.Length

let grid, startX, startY =
    let mutable startX = -1
    let mutable startY = -1

    let parseTile x y =
        match input.[y].[x] with
        | 'S' ->
            startX <- x
            startY <- y
            Plot
        | '.' -> Plot
        | '#' -> Rock
        | _ -> failwith "unexpected"
    
    let plots = Array2D.init xLength yLength parseTile

    plots, startX, startY

let mod2 (x: int) m : int = (x % m + m) % m
let modX x = mod2 x xLength
let modY y = mod2 y yLength

// known correct but doesn't scale - not used in final solution
let part2a targetSteps =
    let mutable prevLiveCells : Set<int * int> = Set.empty
    let mutable liveCells : Set<int * int> = Set.empty

    liveCells <- liveCells |> Set.add (startX, startY)

    let mutable prevPrevCount = 0
    let mutable prevCount = 1
    let mutable count = 0

    let update() =
        let mutable nextLiveCells : Set<int * int> = Set.empty
        let add cell = nextLiveCells <- nextLiveCells |> Set.add cell

        liveCells
        |> Set.iter (fun (x, y) ->
                        if grid.[modX (x + 1), modY y] = Plot then add (x + 1, y)
                        if grid.[modX (x - 1), modY y] = Plot then add (x - 1, y)
                        if grid.[modX x, modY (y + 1)] = Plot then add (x, y + 1)
                        if grid.[modX x, modY (y - 1)] = Plot then add (x, y - 1))

        let newLiveCells = Set.difference nextLiveCells prevLiveCells
        count <- prevPrevCount + Set.count newLiveCells

        prevLiveCells <- liveCells
        liveCells <- newLiveCells
        prevPrevCount <- prevCount
        prevCount <- count
    
    for i in 1 .. targetSteps do
        update()

    printfn "Reachable plots: %i" count

let countsFrom startingCells =
    let mutable prevLiveCells : Set<int * int> = Set.empty
    let mutable liveCells : Set<int * int> = startingCells

    let mutable prevPrevCount = 0
    let mutable prevPrevPrevCount = -1
    let mutable prevCount = startingCells.Count
    let mutable count = 0
    let mutable allCountsRev = [ startingCells.Count ]

    let update() =
        let mutable nextLiveCells : Set<int * int> = Set.empty
        let add cell = nextLiveCells <- nextLiveCells |> Set.add cell

        liveCells
        |> Set.iter (fun (x, y) ->
                        if x < xLength - 1 && grid.[modX (x + 1), modY y] = Plot then add (x + 1, y)
                        if x > 0 && grid.[modX (x - 1), modY y] = Plot then add (x - 1, y)
                        if y < yLength - 1 && grid.[modX x, modY (y + 1)] = Plot then add (x, y + 1)
                        if y > 0 && grid.[modX x, modY (y - 1)] = Plot then add (x, y - 1))

        let newLiveCells = Set.difference nextLiveCells prevLiveCells
        count <- prevPrevCount + Set.count newLiveCells

        prevLiveCells <- liveCells
        liveCells <- newLiveCells

        prevPrevPrevCount <- prevPrevCount
        prevPrevCount <- prevCount
        prevCount <- count

        allCountsRev <- count :: allCountsRev
    
    let mutable x = 0
    while count > prevPrevCount || prevCount > prevPrevPrevCount do
        update()
        x <- x + 1

    allCountsRev
    |> List.rev
    |> List.toArray

let getFromStart cell =
    let startingCells = [ cell ] |> Set.ofList
    let results = countsFrom startingCells
    
    let maxEven, maxOdd =
        if results.Length % 2 = 0 then
            results.[results.Length - 2], results.[results.Length - 1]
        else
            results.[results.Length - 1], results.[results.Length - 2]

    let f steps =
        if steps < 0 then
            0
        elif steps = 0 then
            1
        elif steps >= results.Length then
            if steps % 2 = 0 then maxEven else maxOdd
        else
            results.[steps]
    f

let fromStartPoint = getFromStart (startX, startY)

let part1() =
    let reachable = fromStartPoint 64
    
    printfn "Reachable plots: %i" reachable

let part2() =
    let targetSteps = 26501365

    let extentX = 1 + targetSteps / xLength
    let extentY = 1 + targetSteps / yLength

    let mutable total = 0UL
    let fullOdd = fromStartPoint 1000001
    let fullEven = fromStartPoint 1000000

    let fromBottom = getFromStart (startX, yLength - 1)
    let fromTop = getFromStart (startX, 0)
    let fromLeft = getFromStart (0, startY)
    let fromRight = getFromStart (yLength - 1, startY)
    let fromBottomLeft = getFromStart (0, yLength - 1)
    let fromBottomRight = getFromStart (xLength - 1, yLength - 1)
    let fromTopLeft = getFromStart (0, 0)
    let fromTopRight = getFromStart (xLength - 1, 0)

    let processSquare xSq ySq =
        let minX = xSq * xLength - startX
        let maxX = minX + xLength - 1
        let minY = ySq * yLength - startY
        let maxY = minY + yLength - 1
        let count =
            match xSq, ySq with
            | (0, 0) -> fromStartPoint targetSteps
            | (x, 0) when x > 0 -> fromLeft (targetSteps - minX)
            | (x, 0) when x < 0 -> fromRight (targetSteps + maxX)
            | (0, y) when y > 0 -> fromTop (targetSteps - minY)
            | (0, y) when y < 0 -> fromBottom (targetSteps + maxY)
            | (x, y) when x > 0 && y > 0 -> fromTopLeft (targetSteps - minX - minY)
            | (x, y) when x > 0 && y < 0 -> fromBottomLeft (targetSteps - minX + maxY)
            | (x, y) when x < 0 && y > 0 -> fromTopRight (targetSteps + maxX - minY)
            | (x, y) when x < 0 && y < 0 -> fromBottomRight (targetSteps + maxX + maxY)
            | _ -> failwith "impossible"
        total <- total + uint64 count

    let doSides = 4

    for xSq in -extentX .. extentX do
        let maxY = extentY - (abs xSq)
        if maxY <= doSides then
            for ySq in -maxY .. maxY do
                processSquare xSq ySq
        else
            for ySq in -maxY .. -maxY + doSides do // include one extra, so we always have an even number to fill in later
                processSquare xSq ySq
            
            for ySq in maxY .. -1 .. maxY - (doSides - 1) do
                processSquare xSq ySq
            
            let oddsAndEvens = maxY - doSides
            total <- total + uint64 oddsAndEvens * uint64(fullOdd + fullEven)

    printfn "Reachable plots: %i" total
