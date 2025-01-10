module Day14

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day14.txt")

type Reindeer =
    {
        Name: string
        Speed: int
        FlyTime: int
        RestTime: int
    }

let parseLine (s: string) =
    let parts = s.Split(' ')
    {
        Name = parts.[0]
        Speed = parts.[3] |> Int32.Parse
        FlyTime = parts.[6] |> Int32.Parse
        RestTime = parts.[13] |> Int32.Parse
    }

let reindeers =
    input |> Array.map parseLine

let distanceBy time reindeer =
    let cycleTime = reindeer.FlyTime + reindeer.RestTime
    let completeCycles = time / cycleTime
    let completeCyclesDistance = completeCycles * reindeer.FlyTime * reindeer.Speed

    let remainingTime = time - cycleTime * completeCycles
    let extraFlyingTime = min reindeer.FlyTime remainingTime
    let extraDistance = extraFlyingTime * reindeer.Speed

    completeCyclesDistance + extraDistance

let part1() =
    let maxDistance =
        reindeers
        |> Array.map (distanceBy 2503)
        |> Array.max
    
    printfn "Max distance: %i" maxDistance

let part2() =
    let numReindeers = Array.length reindeers

    let distances =
        Array.init 2504 (fun t ->
            Array.init numReindeers (fun ri -> distanceBy t reindeers.[ri]))
    
    let scores = Array.create numReindeers 0

    for t in 1 .. 2503 do
        let highest = distances.[t] |> Array.max
        for ri in 0 .. numReindeers - 1 do
            if distances.[t].[ri] = highest then scores.[ri] <- scores.[ri] + 1
    
    let maxScore = scores |> Array.max

    printfn "Max score: %i" maxScore
