module Day8

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day8.txt")

let junctions =
    input
    |> Array.map (fun s ->
        let parts = s.Split(',') |> Array.map Int64.Parse
        parts.[0], parts.[1], parts.[2])

let numJunctions = junctions.Length

let pairsByDistance =
    let distance i j =
        let xi, yi, zi = junctions.[i]
        let xj, yj, zj = junctions.[j]
        (xi - xj) * (xi - xj) + (yi - yj) * (yi - yj) + (zi - zj) * (zi - zj)
    
    seq {
        for i in 0 .. numJunctions - 2 do
            for j in i + 1 .. numJunctions - 1 do
                yield distance i j, (i, j)
    }
    |> Seq.sortBy fst
    |> Seq.map snd
    |> Seq.toList

let addCircuit circuits toAdd =
    let i, j = toAdd
    match List.tryFind (List.contains i) circuits, List.tryFind (List.contains j) circuits with
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
        |> List.reduce (*)
    
    printfn "Product: %i" product

let part2() =
    let rec findLastPair circuits remaining =
        match remaining with
        | [] -> failwith "unexpected"
        | next :: rest ->
            let newCircuits = addCircuit circuits next
            match newCircuits with
                | [ head ] when head.Length = numJunctions -> next
                | _ -> findLastPair newCircuits rest

    let j1, j2 = findLastPair [] pairsByDistance

    let x1, _, _ = junctions.[j1]
    let x2, _, _ = junctions.[j2]

    let product = x1 * x2    

    printfn "Product: %i" product
