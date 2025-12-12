module Day12

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day12.txt")

type Shape =
    {
        Filled: bool[,] // always 3x3
    }

type Region =
    {
        SizeX: int
        SizeY: int
        Num: int[] // always 6
    }

let allShapes =
    input
    |> Array.take 30
    |> Array.chunkBySize 5
    |> Array.map (fun lines ->
        let shapeLines = lines |> Array.skip 1 |> Array.take 3
        let filled = Array2D.init 3 3 (fun x y -> shapeLines.[y].[x] = '#')
        { Filled = filled })

let parseRegion (s: string) =
    let parts = s.Split(':')
    let sizes = parts.[0].Split('x') |> Array.map Int32.Parse
    let nums = parts.[1].Substring(1).Split(' ') |> Array.map Int32.Parse
    { SizeX = sizes.[0]; SizeY = sizes.[1]; Num = nums }

let allRegions =
    input
    |> Array.skip 30
    |> Array.map parseRegion

let allVolumes =
    allShapes
    |> Array.map (fun sh ->
        let mutable count = 0
        for x in 0 .. 2 do
            for y in 0 .. 2 do
                if sh.Filled.[x, y] then count <- count + 1
        count)

let part1() =
    let mutable count = 0
    for i in 0 .. allRegions.Length - 1 do
        let totalVol = [ 0 .. 5 ] |> List.sumBy (fun j -> allRegions.[i].Num.[j] * allVolumes.[j])
        let size = allRegions.[i].SizeX * allRegions.[i].SizeY
        if size >= totalVol then count <- count + 1
    printfn "Can fit in volume: %i" count

let part2() =
    ()
