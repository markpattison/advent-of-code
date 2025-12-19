module Day12

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day12.txt")

let parseLine (s: string) =
    let parts = s.Split(" <-> ")
    let progId = parts.[0] |> Int32.Parse

    let pipesTo = parts.[1].Split(", ") |> Array.map Int32.Parse

    progId, pipesTo

let allPrograms =
    input
    |> Array.map parseLine

let allPipes =
    allPrograms
    |> Array.map (fun (progId, pipesTo) -> pipesTo |> Array.map (fun p -> progId, p))
    |> Array.concat

let findId groups progId =
    groups
    |> Seq.tryFind (Set.contains progId)

let addPipe groups (id1, id2) =
    if id2 < id1 then
        groups
    else
        match findId groups id1, findId groups id2 with
        | Some g1, Some g2 when g1 = g2 ->
            groups
        | Some g1, Some g2 ->
            groups |> Set.remove g1 |> Set.remove g2 |> Set.add (Set.union g1 g2)
        | Some g1, None ->
            groups |> Set.remove g1 |> Set.add (Set.add id2 g1)
        | None, Some g2 ->
            groups |> Set.remove g2 |> Set.add (Set.add id1 g2)
        | None, None ->
            groups |> Set.add ([ id1; id2 ] |> Set.ofList)

let allGroups = Array.fold addPipe Set.empty allPipes

let part1() =
    let sizeOfGroupWithZero = allGroups |> Seq.find (Set.contains 0) |> Set.count

    printfn "Size: %i" sizeOfGroupWithZero

let part2() =
    let numGroups = allGroups |> Set.count

    printfn "Number of groups: %i" numGroups
