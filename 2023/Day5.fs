module Day5

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day5.txt")

type MapEntry =
    {
        DestStart: int64
        SourceStart: int64
        RangeLength: int64
    }

type Map =
    {
        Entries: MapEntry[]
    }

let seeds =
    input.[0].Substring(7).Split(" ")
    |> Array.map Int64.Parse

let parseMapEntry (s: string) =
    let values = s.Split(" ") |> Array.map Int64.Parse

    { DestStart = values.[0]; SourceStart = values.[1]; RangeLength = values.[2] }

let findLine line =
    input
    |> Array.findIndex (fun s -> s = line)

let mapTitles =
    input
    |> Array.mapi (fun i s -> (i, s))
    |> Array.filter (fun (_, s) -> s.Contains("map"))
    |> Array.map fst

let mapTilesPlusEnd =
    Array.append mapTitles [| input.Length + 1 |]

let getMap titleIndex nextTitleIndex =
    {
        Entries = input.[(titleIndex + 1) .. (nextTitleIndex - 2)] |> Array.map parseMapEntry
    }

let maps =
    mapTilesPlusEnd
    |> Array.pairwise
    |> Array.map (fun (title, next) -> getMap title next)

let isInRange number mapEntry =
    let offset = number - mapEntry.SourceStart
    offset >= 0L && offset <= mapEntry.RangeLength - 1L

let convert number map =
    match map.Entries |> Array.tryFind (isInRange number) with
    | Some mapEntry -> mapEntry.DestStart + number - mapEntry.SourceStart
    | None -> number

let convertThrough seed =
    Array.fold convert seed maps

let part1() =
    let locations =
        seeds
        |> Array.map convertThrough
    
    let minimum = Array.min locations

    printfn "Minimum location: %i" minimum

let seedRanges =
    seeds
    |> Array.chunkBySize 2

let part2() =
    let mutable minimumLocation = Int64.MaxValue

    seedRanges
    |> Array.iter (fun seedRange ->
        for seed in seedRange.[0] .. (seedRange.[0] + seedRange.[1] - 1L) do
            let location = convertThrough seed
            if location < minimumLocation then minimumLocation <- location)
    
    printfn "Minimum location: %i" minimumLocation
