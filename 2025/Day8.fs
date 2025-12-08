module Day8

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day8.txt")

let junctions =
    input
    |> Array.map (fun s ->
        let parts = s.Split(',')
        Int64.Parse(parts.[0]), Int64.Parse(parts.[1]), Int64.Parse(parts.[2]))

let numJunctions = junctions.Length

let pairsByDistance =
    let distance i j =
        let xi, yi, zi = junctions.[i]
        let xj, yj, zj = junctions.[j]
        (xi - xj) * (xi - xj) + (yi - yj) * (yi - yj) + (zi - zj) * (zi - zj)
    
    seq {
        for i in 0 .. numJunctions - 2 do
            for j in i + 1 .. numJunctions - 1 do
                yield distance i j, i, j
    }
    |> Seq.sortBy (fun (dist, _, _) -> dist)
    |> Seq.map (fun (_, i, j) -> i, j)
    |> Seq.toList

let addCircuit circuits toAdd =
    let i, j = toAdd
    match List.tryFind (fun c -> List.contains i c) circuits, List.tryFind (fun c -> List.contains j c) circuits with
    | None, None ->
        [ i; j ] :: circuits
    | Some c, None ->
        (j :: c) :: (List.except [ c ] circuits)
    | None, Some c ->
        (i :: c) :: (List.except [ c ] circuits)
    | Some c1, Some c2 when c1 = c2 ->
        circuits
    | Some c1, Some c2 ->
        (c1 @ c2) :: (List.except [ c1; c2 ] circuits)

let part1() =
    let shortest1000 =
        pairsByDistance
        |> List.take 1000
    
    let circuits = List.fold addCircuit [] shortest1000

    let product =
        circuits
        |> List.map _.Length
        |> List.sortDescending
        |> List.take 3
        |> List.fold (*) 1
    
    printfn "Product: %i" product

let part2() =
    let rec findLastPair circuits remaining =
        let next = List.head remaining
        let newCircuits = addCircuit circuits next
        if newCircuits.Length = 1 && (List.head newCircuits).Length = numJunctions then
            next
        else
            findLastPair newCircuits (List.tail remaining)

    let j1, j2 = findLastPair [] pairsByDistance

    let x1, _, _ = junctions.[j1]
    let x2, _, _ = junctions.[j2]

    let product = x1 * x2    

    printfn "Product: %i" product
