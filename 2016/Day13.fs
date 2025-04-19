module Day13

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day13.txt").[0] |> UInt32.Parse

let isOpen offset (x: uint32, y: uint32) =
    let value = x * x + 3u * x + 2u * x * y + y + y * y + offset
    let bitsSet = System.Runtime.Intrinsics.X86.Popcnt.PopCount value
    bitsSet % 2u = 0u

let dijkstra finishCondition =
    let isOpen = isOpen input

    let mutable unvisited = [ (0, 1u, 1u) ] |> Set.ofList
    let mutable distances = [ ((1u, 1u), 0) ] |> Map.ofList
    let mutable finished = false

    while not finished do
        let currDist, x, y = Set.minElement unvisited
        if finishCondition (x, y) currDist then
            finished <- true
        else
            let nextDist = currDist + 1
            if x > 0u && isOpen (x - 1u, y) then
                match Map.tryFind (x - 1u, y) distances with
                | None ->
                    distances <- Map.add (x - 1u, y) nextDist distances
                    unvisited <- Set.add (nextDist, x - 1u, y) unvisited
                | Some n when n > nextDist ->
                    distances <- Map.add (x - 1u, y) nextDist distances
                | _ -> ()
            if isOpen (x + 1u, y) then
                match Map.tryFind (x + 1u, y) distances with
                | None ->
                    distances <- Map.add (x + 1u, y) nextDist distances
                    unvisited <- Set.add (nextDist, x + 1u, y) unvisited
                | Some n when n > nextDist ->
                    distances <- Map.add (x + 1u, y) nextDist distances
                | _ -> ()
            if y > 0u && isOpen (x, y - 1u) then
                match Map.tryFind (x, y - 1u) distances with
                | None ->
                    distances <- Map.add (x, y - 1u) nextDist distances
                    unvisited <- Set.add (nextDist, x, y - 1u) unvisited
                | Some n when n > nextDist ->
                    distances <- Map.add (x, y - 1u) nextDist distances
                | _ -> ()
            if isOpen (x, y + 1u) then
                match Map.tryFind (x, y + 1u) distances with
                | None ->
                    distances <- Map.add (x, y + 1u) nextDist distances
                    unvisited <- Set.add (nextDist, x, y + 1u) unvisited
                | Some n when n > nextDist ->
                    distances <- Map.add (x, y + 1u) nextDist distances
                | _ -> ()
            unvisited <- Set.remove (currDist, x, y) unvisited
    distances

let part1() =
    let target = 31u, 39u
    let isFinished (x, y) _ = (x, y) = target
    let distances = dijkstra isFinished

    printfn "Distance: %i" distances.[target]

let part2() =
    let maxDist = 50
    let isFinished _ currDist = currDist >= maxDist

    let distances = dijkstra isFinished

    printfn "Locations: %i" distances.Count
