module Day22

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day22.txt")

type Range = { Min: int; Max: int }

let intersect r1 r2 =
    let newMin = max r1.Min r2.Min
    let newMax = min r1.Max r2.Max
    if newMin > newMax then
        None
    else
        Some { Min = newMin; Max = newMax }

type Step =
    {
        Switch: int
        X: Range
        Y: Range
        Z: Range
    }

let parseLine (s: string) =
    let switch, remaining =
        match s.Substring(0, 3) with
        | "on " -> 1, s.Substring(3)
        | "off" -> 0, s.Substring(4)
        | _ -> failwith "unexpected"
    
    let parseRange (r: string) =
        let ends = r.Split("..")
        { Min = ends.[0] |> Int32.Parse; Max = ends.[1] |> Int32.Parse }

    let ranges =
        remaining.Split(',')
        |> Array.map (fun r -> r.Substring(2) |> parseRange)

    {
        Switch = switch
        X = ranges.[0]
        Y = ranges.[1]
        Z = ranges.[2]
    }

let allSteps =
    input
    |> Array.map parseLine

let part1() =
    let initializationArea = { Min = -50; Max = 50 }
    let initializationSteps =
        allSteps
        |> Array.choose (fun step ->
            let newX = intersect initializationArea step.X
            let newY = intersect initializationArea step.Y
            let newZ = intersect initializationArea step.Z
            match newX, newY, newZ with
            | Some rX, Some rY, Some rZ -> Some { step with X = rX; Y = rY; Z = rZ }
            | _ -> None)
    
    let cubes = Array3D.zeroCreate 101 101 101

    let applyStep step =
        for x in step.X.Min .. step.X.Max do
            for y in step.Y.Min .. step.Y.Max do
                for z in step.Z.Min .. step.Z.Max do
                    cubes.[x + 50, y + 50, z + 50] <- step.Switch
    
    initializationSteps |> Array.iter applyStep

    let mutable count = 0
    for x in 0 .. 100 do
        for y in 0 .. 100 do
            for z in 0 .. 100 do
                count <- count + cubes.[x, y, z]
    
    printfn "Cubes on: %i" count

let part2() =
    let allRanges stepRanges =
        stepRanges
        |> Array.collect (fun r -> [| r.Min; r.Max + 1 |])
        |> Array.distinct
        |> Array.sort
        |> Array.pairwise
        |> Array.map (fun (rMin, rMax) -> { Min = rMin; Max = rMax - 1 })
    
    let allRangesX = allSteps |> Array.map _.X |> allRanges
    let allRangesY = allSteps |> Array.map _.Y |> allRanges
    let allRangesZ = allSteps |> Array.map _.Z |> allRanges

    let sizeX = allRangesX.Length
    let sizeY = allRangesY.Length
    let sizeZ = allRangesZ.Length

    let cubes = Array3D.zeroCreate sizeX sizeY sizeZ

    let applyStep step =
        let startIndexX = allRangesX |> Array.findIndex (fun r -> r.Min >= step.X.Min)
        let stopIndexX = match allRangesX |> Array.tryFindIndex (fun r -> r.Min > step.X.Max) with | Some i -> i | None -> sizeX
        let startIndexY = allRangesY |> Array.findIndex (fun r -> r.Min >= step.Y.Min)
        let stopIndexY = match allRangesY |> Array.tryFindIndex (fun r -> r.Min > step.Y.Max) with | Some i -> i | None -> sizeY
        let startIndexZ = allRangesZ |> Array.findIndex (fun r -> r.Min >= step.Z.Min)
        let stopIndexZ = match allRangesZ |> Array.tryFindIndex (fun r -> r.Min > step.Z.Max) with | Some i -> i | None -> sizeZ

        for ix in startIndexX .. stopIndexX - 1 do
            for iy in startIndexY .. stopIndexY - 1 do
                for iz in startIndexZ .. stopIndexZ - 1 do
                    cubes.[ix, iy, iz] <- step.Switch
    
    allSteps |> Array.iter applyStep

    let mutable count = 0L
    for ix in 0 .. sizeX - 1 do
        for iy in 0 .. sizeY - 1 do
            for iz in 0 .. sizeZ - 1 do
                if cubes.[ix, iy, iz] = 1 then
                    count <- count + (int64 (1 + allRangesX.[ix].Max - allRangesX.[ix].Min))
                                    * (int64 (1 + allRangesY.[iy].Max - allRangesY.[iy].Min))
                                    * (int64 (1 + allRangesZ.[iz].Max - allRangesZ.[iz].Min))
    
    printfn "Cubes on: %i" count
    