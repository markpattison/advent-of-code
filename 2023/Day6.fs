module Day6

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day6.txt")

type Race =
    {
        Time: int64
        Distance: int64
    }

let parseLine (s: string) =
    s.Split(" ")
    |> Array.skip 1
    |> Array.map (fun s -> s.Trim())
    |> Array.filter (fun s -> s <> "")
    |> Array.map Int64.Parse

let parseLine2 (s: string) =
    s.Split(" ")
    |> Array.skip 1
    |> Array.map (fun s -> s.Trim())
    |> Array.filter (fun s -> s <> "")
    |> Array.fold (+) ""
    |> Int64.Parse

let races =
    let times = input.[0] |> parseLine
    let distances = input.[1] |> parseLine

    Array.zip times distances
    |> Array.map (fun (t, d) -> { Time = t; Distance = d })

let race2 =
    let time = input.[0] |> parseLine2
    let distance = input.[1] |> parseLine2

    { Time = time; Distance = distance }

let distanceTravelled race chargingTime =
    let speed = chargingTime
    let travellingTime = race.Time - chargingTime

    speed * travellingTime

let beatsDistance race chargingTime =
    let distance = distanceTravelled race chargingTime
    distance > race.Distance

let waysToWin race =
    let firstToWin =
        seq { 0L .. race.Time }
        |> Seq.find (beatsDistance race)

    let lastToWin =
        seq { race.Time .. -1L .. firstToWin }
        |> Seq.find (beatsDistance race)

    lastToWin + 1L - firstToWin

let part1() =
    let allWaysToWin =
        races
        |> Array.map waysToWin
    
    let product = Array.fold (*) 1L allWaysToWin

    printfn "Product: %i" product

let part2() =
    let numWaysToWin = waysToWin race2

    printfn "Ways to win: %i" numWaysToWin
