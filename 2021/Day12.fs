module Day12

open System
open System.IO

let lines = File.ReadAllLines(@"input\day12.txt")

let test1 =
    [|
        "start-A"
        "start-b"
        "A-c"
        "A-b"
        "b-d"
        "A-end"
        "b-end"
    |]

let test2 =
    [|
        "dc-end"
        "HN-start"
        "start-kj"
        "dc-start"
        "dc-HN"
        "LN-dc"
        "HN-end"
        "kj-sa"
        "kj-HN"
        "kj-dc"
    |]

let test3 =
    [|
        "fs-end"
        "he-DX"
        "fs-he"
        "start-DX"
        "pj-DX"
        "end-zg"
        "zg-sl"
        "zg-pj"
        "pj-he"
        "RW-he"
        "fs-DX"
        "pj-RW"
        "zg-RW"
        "start-pj"
        "he-WI"
        "zg-he"
        "pj-fs"
        "start-RW"
    |]

let toConnections connections =
    let tuples =
        connections
        |> Array.map (fun (c: string) ->
            let caves = c.Split('-')
            caves.[0], caves.[1])

    let allCaves =
        tuples
        |> Array.collect (fun (c1, c2) -> [| c1; c2 |])
        |> Array.distinct

    let connectedTo cave =
        let to1 = tuples |> Array.filter (fun (c1, _) -> c1 = cave) |> Array.map snd |> Array.toList
        let to2 = tuples |> Array.filter (fun (_, c2) -> c2 = cave) |> Array.map fst |> Array.toList

        to1 @ to2

    let allConnections =
        allCaves
        |> Array.map (fun cave -> cave, connectedTo cave)
        |> Map.ofArray

    allConnections

let connections = toConnections lines

let isSmall (s: string) = Char.IsLower(s.[0])

let part1() =
    
    let rec allPaths revVisited =
        let current = List.head revVisited

        match current with
        | "end" -> [ revVisited ]
        | _ ->
            let connectedTo = connections.[current]
            let visitedSmall = revVisited |> List.filter isSmall
            let canVisit = connectedTo |> List.except visitedSmall

            match canVisit with
            | [] -> []
            | toVisit -> toVisit |> List.collect (fun v -> allPaths (v :: revVisited))

    let paths = allPaths [ "start" ] |> List.map (List.rev)

    printfn "Number of paths: %i" paths.Length

let part2() =

    let rec allPaths revVisited =
        let current = List.head revVisited

        match current with
        | "end" -> [ revVisited ]
        | _ ->
            let connectedTo = connections.[current]
            let visitedSmall = revVisited |> List.filter isSmall
            let haveVisitedASmallTwice = visitedSmall.Length > (List.distinct visitedSmall).Length

            let canVisit =
                if haveVisitedASmallTwice then
                    connectedTo |> List.except visitedSmall
                else
                    connectedTo |> List.except [ "start" ]

            match canVisit with
            | [] -> []
            | toVisit -> toVisit |> List.collect (fun v -> allPaths (v :: revVisited))

    let paths = allPaths [ "start" ] |> List.map (List.rev)

    printfn "Number of paths: %i" paths.Length
