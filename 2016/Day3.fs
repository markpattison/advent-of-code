module Day3

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day3.txt")

let allTriangles =
    input
    |> Array.map (fun s ->
        let parts =
            s.Split(" ", StringSplitOptions.RemoveEmptyEntries)
            |> Array.map Int32.Parse
        match parts with
        | [| a; b; c |] -> (a, b, c)
        | _ -> failwith "unexpected")

let isPossible (a, b, c) =
    let longest = max a (max b c)
    let others = a + b + c - longest
    others > longest

let part1() =
    let numPossible =
        allTriangles
        |> Array.filter isPossible
        |> Array.length
    
    printfn "Possible: %i" numPossible

let part2() =
    let newTriangles =
        allTriangles
        |> Array.chunkBySize 3
        |> Array.collect (fun chunk ->
            let (a, b, c) = chunk.[0]
            let (d, e, f) = chunk.[1]
            let (g, h, i) = chunk.[2]
            [| (a, d, g); (b, e, h); (c, f, i) |])
    
    let numPossible =
        newTriangles
        |> Array.filter isPossible
        |> Array.length
    
    printfn "Possible: %i" numPossible
