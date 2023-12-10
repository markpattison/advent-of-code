module Day10

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day10.txt")

type Dir = N | E | S | W
type Tile =
    | Ground
    | Animal
    | Pipe of Dir * Dir

let parseChar c =
    match c with
    | '.' -> Ground
    | 'S' -> Animal
    | '|' -> Pipe (N, S)
    | '-' -> Pipe (E, W)
    | 'L' -> Pipe (N, E)
    | 'J' -> Pipe (N, W)
    | '7' -> Pipe (S, W)
    | 'F' -> Pipe (S, E)
    | _ -> failwith "unexpected"

let fromDir tile dir =
    match tile with
    | Ground -> false
    | Animal -> dir = N || dir = W // for our main input anyway, not always true
    | Pipe (d1, d2) when d1 = dir || d2 = dir -> true
    | _ -> false

let grid =
    let xLength = input.[0].Length
    let yLength = input.Length

    let tiles = Array2D.init
                    xLength
                    yLength
                    (fun x y -> input.[y].[x] |> parseChar)

    tiles

let withLoop =
    let steps = Array2D.init
                    (Array2D.length1 grid)
                    (Array2D.length2 grid)
                    (fun x y -> if grid.[x, y] = Animal then 0 else -1)
    
    let mutable keepGoing = true
    let mutable currentSteps = 0

    while keepGoing do
        keepGoing <- false
        let stepsCopy = Array2D.copy steps

        for x in 0 .. Array2D.length1 grid - 1 do
            for y in 0 .. Array2D.length2 grid - 1 do
                if steps.[x, y] = -1 && grid.[x, y] <> Ground then
                    if (y > 0 && steps.[x, y - 1] = currentSteps && fromDir grid.[x, y - 1] S && fromDir grid.[x, y] N) ||
                       (x < Array2D.length1 grid - 1 && steps.[x + 1, y] = currentSteps && fromDir grid.[x + 1, y] W && fromDir grid.[x, y] E) ||
                       (y < Array2D.length2 grid - 1 && steps.[x, y + 1] = currentSteps && fromDir grid.[x, y + 1] N && fromDir grid.[x, y] S) ||
                       (x > 0 && steps.[x - 1, y] = currentSteps && fromDir grid.[x - 1, y] E && fromDir grid.[x, y] W) then
                            stepsCopy.[x, y] <- currentSteps + 1
                            keepGoing <- true
        currentSteps <- currentSteps + 1
        
        Array2D.blit stepsCopy 0 0 steps 0 0 (Array2D.length1 steps) (Array2D.length2 steps)

    steps

let part1() =
    let mutable maxSteps = 0
    for x in 0 .. Array2D.length1 withLoop - 1 do
        for y in 0 .. Array2D.length2 withLoop - 1 do
            if withLoop.[x, y] > maxSteps then maxSteps <- withLoop.[x, y]

    printfn "Max steps: %i" maxSteps

let part2() =
    let enclosed = Array2D.zeroCreate (Array2D.length1 grid) (Array2D.length2 grid)
    for x in 0 .. Array2D.length1 withLoop - 1 do
        let mutable insideEast = false
        let mutable insideWest = false
        for y in 0 .. Array2D.length2 withLoop - 1 do
            if withLoop.[x, y] >= 0 && fromDir grid.[x, y] E then
                insideEast <- not insideEast
            if withLoop.[x, y] >= 0 && fromDir grid.[x, y] W then
                insideWest <- not insideWest            
            if withLoop.[x, y] < 0 && insideEast && insideWest then
                enclosed.[x, y] <- 1

    let mutable count = 0
    for x in 0 .. Array2D.length1 withLoop - 1 do
        for y in 0 .. Array2D.length2 withLoop - 1 do
            if enclosed.[x, y] = 1 then count <- count + 1

    printfn "Count %i" count
