module Day5

open System
open System.IO

open Common
open Common.Types

let input =
    File.ReadAllLines(@"input\day5.txt")

let freshRanges =
    input
    |> Array.takeWhile (fun s -> s.Length > 0)
    |> Array.map (fun s ->
        let parts = s.Split('-')
        { Min = parts.[0] |> Int64.Parse; Max = parts.[1] |> Int64.Parse })

let available =
    input
    |> Array.skipWhile (fun s -> s.Contains('-'))
    |> Array.skip 1
    |> Array.map Int64.Parse

let isFresh a =
    freshRanges |> Array.exists (Range.contains a)

let part1() =
    let availableFresh =
        available
        |> Array.filter isFresh
        |> Array.length
    
    printfn "Available fresh: %i" availableFresh

let part2() =
    let merged = Range.mergeAll freshRanges
    
    let totalFresh =
        merged
        |> Array.sumBy Range.size
    
    printfn "Total fresh: %i" totalFresh
