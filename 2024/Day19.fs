module Day19

open System
open System.IO

let input = File.ReadAllLines(@"input\day19.txt")

let patterns =
    input.[0].Split(", ")

let designs =
    input
    |> Array.skip 2

let mutable cache : Map<string, int64> = Map.empty

let rec possibleOptions design =
    if design = "" then
        1L
    else
        match Map.tryFind design cache with
        | Some result -> result
        | None ->
            let result =
                patterns
                |> Seq.filter (fun p -> design.StartsWith(p))
                |> Seq.sumBy (fun p -> possibleOptions (design.Substring(p.Length)))
            cache <- Map.add design result cache
            result

let part1() =
    let possibleCount =
        designs
        |> Array.filter (fun d -> possibleOptions d > 0)
        |> Array.length
    
    printfn "Possible: %i" possibleCount

let part2() =
    let possibleCount =
        designs
        |> Array.sumBy possibleOptions
    
    printfn "Possible options: %i" possibleCount
