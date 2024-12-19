module Day18

open System
open System.IO

let input = File.ReadAllLines(@"input\day18.txt")

let sizeX = 71
let sizeY = 71

let parseLine (s: string) =
    let parts = s.Split(',') |> Array.map Int32.Parse
    parts.[0], parts.[1]

let locations =
    input
    |> Array.map parseLine

let getCorrupted n =
    let arr = Array2D.create sizeX sizeY false
    for i in 0 .. (n - 1) do
        let x, y = locations.[i]
        arr.[x, y] <- true
    arr

let minSteps (corrupted: bool[,]) =

    let dists = Array2D.create sizeX sizeY Int32.MaxValue
    dists.[0, 0] <- 0

    let mutable unvisited =
        seq {
        for x in 0 .. sizeX - 1 do
            for y in 0 .. sizeY - 1 do
                if not corrupted.[x, y] then
                    if x = 0 && y = 0 then
                        (0, x, y)
                    else
                        (Int32.MaxValue, x, y)
        } |> Set.ofSeq
    
    let mutable finished = false

    while not finished do
        if Set.isEmpty unvisited then
            finished <- true
        else
            let (cDist, cx, cy) = Set.minElement unvisited
            if cDist = Int32.MaxValue then
                finished <- true
            else
                let neighbours =
                    seq {
                        if cx > 0 then (cx - 1, cy)
                        if cx < sizeX - 1 then (cx + 1, cy)
                        if cy > 0 then (cx, cy - 1)
                        if cy < sizeY - 1 then (cx, cy + 1)
                    } |> Seq.filter (fun (nx, ny) -> not corrupted.[nx, ny])
                let nDist = cDist + 1
                neighbours
                |> Seq.iter (fun (nx, ny) ->
                    let oldDist = dists.[nx, ny]
                    if nDist < oldDist then
                        unvisited <- Set.remove (oldDist, nx, ny) unvisited
                        unvisited <- Set.add (nDist, nx, ny) unvisited
                        dists.[nx, ny] <- nDist)
                unvisited <- Set.remove (cDist, cx, cy) unvisited
    
    dists.[sizeX - 1, sizeY - 1]

let part1() =
    let corrupted = getCorrupted 1024
    let steps = minSteps corrupted
    printfn "Steps: %i" steps

let part2() =
    let mutable lowerBound = 1024
    let mutable upperBound = locations.Length

    while upperBound - lowerBound > 1 do
        let toTry = (lowerBound + upperBound) / 2
        if toTry |> getCorrupted |> minSteps = Int32.MaxValue then
            upperBound <- toTry
        else
            lowerBound <- toTry
    
    let x, y = locations.[lowerBound]

    printfn "Location: %i,%i" x y
