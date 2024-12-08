module Day8

open System
open System.IO

let input = File.ReadAllLines(@"input\day8.txt")

let sizeX = input.[0].Length
let sizeY = input.Length

type Node = { X: int; Y: int; Freq: char }

let nodes =
    seq {
        for x in 0 .. sizeX - 1 do
            for y in 0 .. sizeY - 1 do
                if input.[y].[x] <> '.' then
                    { X = x; Y = y; Freq = input.[y].[x] }
    } |> Seq.toArray

let frequencies =
    nodes
    |> Array.map (fun n -> n.Freq)
    |> Array.distinct

let nodesByFreq =
    frequencies
    |> Array.map (fun f ->
        let positions = nodes |> Array.filter (fun n -> n.Freq = f) |> Array.map (fun n -> (n.X, n.Y))
        (f, positions))
    |> Map.ofArray

let pairs (arr: 'a[]) =
    seq {
        for i in 0 .. arr.Length - 2 do
            for j in i + 1 .. arr.Length - 1 do
                (arr.[i], arr.[j])
    } |> Seq.toArray

let offset (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

let isAntinode1 pos freq =
    let freqNodes = nodesByFreq.[freq]
    let nodePairs = pairs freqNodes
    
    nodePairs
    |> Array.exists (fun (n1, n2) ->
        let (x1, y1) = offset pos n1
        let (x2, y2) = offset pos n2
        (x1 = x2 * 2 && y1 = y2 * 2) || (x2 = x1 * 2 && y2 = y1 * 2))

let part1() =
    let mutable count = 0

    for x in 0 .. sizeX - 1 do
        for y in 0 .. sizeY - 1 do
            if (frequencies |> Array.exists (isAntinode1 (x, y))) then count <- count + 1
    
    printfn "Antinodes: %i" count

let isAntinode2 pos freq =
    let freqNodes = nodesByFreq.[freq]

    if Array.contains pos freqNodes then
        true
    else
        let nodePairs = pairs freqNodes

        nodePairs
        |> Array.exists (fun (n1, n2) ->
            let (x1, y1) = offset pos n1
            let (x2, y2) = offset pos n2
            Math.Round(float x1 / float x2, 6) = Math.Round(float y1 / float y2, 6))

let part2() =
    let mutable count = 0

    for x in 0 .. sizeX - 1 do
        for y in 0 .. sizeY - 1 do
            if (frequencies |> Array.exists (isAntinode2 (x, y))) then count <- count + 1
    
    printfn "Antinodes: %i" count
