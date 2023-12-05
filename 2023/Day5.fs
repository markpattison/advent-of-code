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
    |> Array.map (fun seedRange -> (seedRange.[0], seedRange.[1]))

let rec convertRange acc map (startRange, rangeLength) =
    if rangeLength = 0L then
        acc
    else
        match map.Entries |> Array.tryFind (isInRange startRange) with
        | Some mapEntry ->
            let mappedRangeStart = mapEntry.DestStart + startRange - mapEntry.SourceStart
            let mappedRangeLength = min rangeLength (mapEntry.RangeLength - startRange + mapEntry.SourceStart)
            let remainingRangeLength = rangeLength - mappedRangeLength
            convertRange ((mappedRangeStart, mappedRangeLength) :: acc) map (startRange + mappedRangeLength, remainingRangeLength)
        | None ->
            let laterMaps = map.Entries |> Array.filter (fun m -> m.SourceStart > startRange)
            if laterMaps.Length = 0 then
                (startRange, rangeLength) :: acc
            else
                let firstLaterMap = laterMaps |> Array.minBy _.SourceStart
                if firstLaterMap.SourceStart > startRange + rangeLength then
                    (startRange, rangeLength) :: acc
                else
                    let mappedRangeLength = firstLaterMap.SourceStart - startRange
                    let remainingRangeLength = rangeLength - mappedRangeLength
                    convertRange ((startRange, mappedRangeLength) :: acc) map (startRange + mappedRangeLength, remainingRangeLength)

let part2() =
    let folder ranges map = List.collect (convertRange [] map) ranges

    let locationRanges =
        Array.fold folder (List.ofArray seedRanges) maps
    
    let minimumLocation =
        locationRanges |> List.map fst |> List.min

    printfn "Minimum location: %i" minimumLocation
    