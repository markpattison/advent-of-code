module Day13

open System
open System.IO

let input = File.ReadAllLines(@"input\day13.txt")

type Position = { X: int64; Y: int64 }

let parseMachine (arr: string[] ) =
    let partsA = arr.[0].Split([| '+'; ',' |])
    let a = { X = partsA.[1] |> Int64.Parse; Y = partsA.[3] |> Int64.Parse }

    let partsB = arr.[1].Split([| '+'; ',' |])
    let b = { X = partsB.[1] |> Int64.Parse; Y = partsB.[3] |> Int64.Parse }

    let partsPrize = arr.[2].Split([| '='; ',' |])
    let prize = { X = partsPrize.[1] |> Int64.Parse; Y = partsPrize.[3] |> Int64.Parse }

    (a, b, prize)

let machines =
    input
    |> Array.chunkBySize 4
    |> Array.map parseMachine

let solve (a, b, prize) =
    let denom = a.X * b.Y - a.Y * b.X
    let s = (prize.X * b.Y - prize.Y * b.X) / denom
    let t = (prize.Y * a.X - prize.X * a.Y) / denom

    if s * a.X + t * b.X = prize.X && s * a.Y + t * b.Y = prize.Y then
        Some (s * 3L + t)
    else
        None

let part1() =
    let totalCost =
        machines
        |> Array.choose solve
        |> Array.sum
    
    printfn "Total cost: %i" totalCost

let part2() =
    let totalCost =
        machines
        |> Array.map (fun (a, b, prize) -> (a, b, { X = prize.X + 10000000000000L; Y = prize.Y + 10000000000000L }))
        |> Array.choose solve
        |> Array.sum
    
    printfn "Total cost: %i" totalCost
