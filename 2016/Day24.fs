module Day24

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day24.txt")

let sizeX = input.[0].Length
let sizeY = input.Length

let isWall, locations =
    let walls = Array2D.create sizeX sizeY false
    let mutable startX = -1
    let mutable startY = -1
    let mutable locs : Map<int, int * int> = Map.empty

    for y in 0 .. (sizeY - 1) do
        for x in 0 .. (sizeX - 1) do
            match input.[y].[x] with
            | '#' -> walls.[x, y] <- true
            | '.' -> ()
            | c ->
                let n = Int32.Parse(c.ToString())
                locs <- locs |> Map.add n (x, y)

    walls, locs

let dijkstra (startX, startY) =
    let mutable visited : Set<int * int> = Set.empty
    let mutable unvisited = [ (0, startX, startY) ] |> Set.ofList
    let mutable distances = [ ((startX, startY), 0) ] |> Map.ofList

    let mutable finished = false

    while not finished do
        if Set.isEmpty unvisited then
            finished <- true
        else
            let currDist, x, y = Set.minElement unvisited
            let nextDist = currDist + 1
            let neighbours = seq { x + 1, y; x - 1, y; x, y + 1; x, y - 1 }
            let unvisitedNeighbours =
                neighbours |> Seq.filter (fun (nx, ny) -> not isWall.[nx, ny] && not (Set.contains (nx, ny) visited))
            unvisitedNeighbours |> Seq.iter (fun (nx, ny) ->
                match Map.tryFind (nx, ny) distances with
                | None ->
                    distances <- distances |> Map.add (nx, ny) nextDist
                    unvisited <- unvisited |> Set.add (nextDist, nx, ny)
                | Some oldDist when oldDist <= nextDist -> ()
                | Some oldDist ->
                    distances <- distances |> Map.add (nx, ny) nextDist
                    unvisited <- unvisited |> Set.remove (oldDist, nx, ny) |> Set.add (nextDist, nx, ny))
            unvisited <- unvisited |> Set.remove (currDist, x, y)
            visited <- visited |> Set.add (x, y)
    
    distances

let distancesBetweenLocations =
    let numLocs = Map.count locations

    let distances = Array2D.zeroCreate numLocs numLocs

    for iLoc in 0 .. (numLocs - 1) do
        let shortestDistances = dijkstra locations.[iLoc]

        for jLoc in 0 .. (numLocs - 1) do
            distances.[iLoc, jLoc] <- shortestDistances.[locations.[jLoc]]
    
    distances

let part1() =
    let numLocs = Map.count locations

    let rec shortestPath (distanceSoFar: int) currentLocation locationsToVisit =
        match locationsToVisit with
        | [] -> distanceSoFar
        | locs ->
            locs
            |> List.map (fun loc -> shortestPath (distanceSoFar + distancesBetweenLocations.[currentLocation, loc]) loc (locs |> List.filter (fun l -> l <> loc)))
            |> List.min
    
    let shortest = shortestPath 0 0 [ 1 .. (numLocs - 1) ]

    printfn "Shortest path: %i" shortest

let part2() =
    let numLocs = Map.count locations

    let rec shortestPath (distanceSoFar: int) currentLocation locationsToVisit =
        match locationsToVisit with
        | [] -> distanceSoFar + distancesBetweenLocations.[currentLocation, 0]
        | locs ->
            locs
            |> List.map (fun loc -> shortestPath (distanceSoFar + distancesBetweenLocations.[currentLocation, loc]) loc (locs |> List.filter (fun l -> l <> loc)))
            |> List.min
    
    let shortest = shortestPath 0 0 [ 1 .. (numLocs - 1) ]

    printfn "Shortest path: %i" shortest
