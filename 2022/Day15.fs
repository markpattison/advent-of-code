module Day15

open System
open System.IO

open Common
open Common.Types

type Position = int64 * int64

type Sensor =
    {
        Position: Position
        ClosestBeacon: Position
        Distance: int64
    }

let distance (p1: Position) (p2: Position) =
    abs (fst p1 - fst p2) + abs (snd p1 - snd p2)

let parseLine (s: string) =
    let firstComma = s.IndexOf(',')
    let colon = s.IndexOf(':')
    let sensorX = s.Substring(12, firstComma - 12) |> int64
    let sensorY = s.Substring(firstComma + 4, colon - firstComma - 4) |> int64

    let isAt = s.IndexOf("is at")
    let secondComma = s.IndexOf(',', firstComma + 1)
    let beaconX = s.Substring(isAt + 8, secondComma - isAt - 8) |> int64
    let beaconY = s.Substring(secondComma + 4) |> int64

    let position = sensorX, sensorY
    let closestBeacon = beaconX, beaconY
    {
        Position = position
        ClosestBeacon = closestBeacon
        Distance = distance position closestBeacon
    }

let sensors =
    File.ReadAllLines(@"input\day15.txt")
    |> Array.map parseLine

let orderedRangeContains x oir = Array.exists (Range.contains x) oir

let rangeIsContainedByRange inner outer = inner.Min >= outer.Min && inner.Max <= outer.Max
let orderedRangeContainsRange oir r = Array.exists (rangeIsContainedByRange r) oir

let getRanges y =
    sensors
    |> Array.choose (fun s ->
        let sx, sy = s.Position
        let yDist = abs (y - sy)
        if yDist > s.Distance then
            None
        else
            let size = s.Distance - yDist
            Some { Min = sx - size; Max = sx + size })
    |> Range.mergeAll

let part1() =
    let y = 2000000L

    let ranges = getRanges y

    let containedBeacons =
        sensors
        |> Array.filter (fun s -> y = snd s.ClosestBeacon)
        |> Array.map (fun s -> fst s.ClosestBeacon) // x values
        |> Array.distinct
        |> Array.filter (fun x -> orderedRangeContains x ranges)
        |> Array.length
        |> int64

    let notBeacons = (ranges |> Array.sumBy Range.size) - containedBeacons

    printfn "Positions not containing a beacon: %i" notBeacons

let part2() =
    let targetRange = { Min = 0L; Max = 4000000L }
    let mutable foundBeacon = false
    let mutable x, y = 0L, 0L
    
    while not foundBeacon && y <= 4000000L do
        let ranges = getRanges y
        if orderedRangeContainsRange ranges targetRange then
            y <- y + 1L
        else
            foundBeacon <- true
            x <-
                ranges
                |> Array.find (fun r -> r.Max >= targetRange.Min)
                |> (fun r -> r.Max + 1L)
    
    let frequency = x * 4000000L + y

    printfn "Frequency: %i" frequency
