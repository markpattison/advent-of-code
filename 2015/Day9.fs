module Day9

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day9.txt")

type Trip = { From: string; To: string; Distance: int }

let parseLine (s: string) =
    let parts = s.Split([| " to "; " = " |], StringSplitOptions.None)
    { From = parts.[0]; To = parts.[1]; Distance = parts.[2] |> Int32.Parse }

let allTrips = input |> Array.map parseLine

let locationIndices =
    allTrips
    |> Array.collect (fun t -> [| t.From; t.To |] )
    |> Array.distinct
    |> Array.mapi (fun i loc -> (loc, i + 1))
    |> Map.ofArray

let numLocations = Map.count locationIndices

let distances =
    // location 0 is start, with distance 0 to any other location
    let matrix = Array2D.zeroCreate (numLocations + 1) (numLocations + 1)

    allTrips
    |> Array.iter (fun t ->
        let loc1 = locationIndices.[t.From]
        let loc2 = locationIndices.[t.To]
        matrix.[loc1, loc2] <- t.Distance
        matrix.[loc2, loc1] <- t.Distance)
    matrix

let extremePath comparison =
    let rec bestPath acc currentLoc (remainingLocations: int[]) =
        if remainingLocations.Length = 0 then
            acc
        else
            remainingLocations
            |> Array.map (fun nextLoc ->
                bestPath (acc + distances.[currentLoc, nextLoc]) nextLoc (Array.except [| nextLoc |] remainingLocations))
            |> comparison
    
    bestPath 0 0 [| 1 .. numLocations |]

let part1() =
    let shortest = extremePath Array.min
    printfn "Shortest route: %i" shortest

let part2() =
    let longest = extremePath Array.max
    printfn "Longest route: %i" longest
