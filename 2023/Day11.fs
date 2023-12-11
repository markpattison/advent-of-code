module Day11

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day11.txt")

let xLength = input.[0].Length
let yLength = input.Length

let data =
    Array2D.init xLength yLength (fun x y -> if input.[y].[x] = '#' then 1 else 0)

let blankRows =
    [|
        for y in 0 .. yLength - 1 do
            if data.[*, y] |> Array.sum = 0 then yield int64 y
    |]

let blankColumns =
    [|
        for x in 0 .. xLength - 1 do
            if data.[x, *] |> Array.sum = 0 then yield int64 x
    |]

let galaxies =
    [|
        for x in 0 .. xLength - 1 do
            for y in 0 .. yLength - 1 do
                if data.[x, y] = 1 then yield (int64 x, int64 y)
    |]

let distance (x1, y1) (x2, y2) (expansion: int64) =
    let minX = min x1 x2
    let maxX = max x1 x2
    let minY = min y1 y2
    let maxY = max y1 y2

    let extraRows = blankRows |> Seq.filter (fun r -> r > minY && r < maxY) |> Seq.length |> int64
    let extraColumns = blankColumns |> Seq.filter (fun c -> c > minX && c < maxX) |> Seq.length |> int64

    (maxX - minX) + (maxY - minY) + (extraRows + extraColumns) * (expansion - 1L)

let allPairs =
    [|
        for i in 0 .. galaxies.Length - 2 do
            for j in i + 1 .. galaxies.Length - 1 do
                yield (galaxies.[i], galaxies.[j])
    |]

let part1() =
    let totalDistance =
        allPairs
        |> Array.map (fun (g1, g2) -> distance g1 g2 2L)
        |> Array.sum

    printfn "Total distance: %i" totalDistance

let part2() =
    let totalDistance =
        allPairs
        |> Array.map (fun (g1, g2) -> distance g1 g2 1000000L)
        |> Array.sum

    printfn "Total distance: %i" totalDistance
