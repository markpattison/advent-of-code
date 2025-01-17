module Day17

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day17.txt")

let allContainers =
    input
    |> Array.map Int32.Parse
    |> Array.toList

let combinations containers toStore =
    let rec allCombinations acc sizes rem =
        seq {
            match rem with
            | n when n < 0 -> ()
            | 0 -> yield acc
            | _ ->
                match sizes with
                | [] -> ()
                | x :: xs ->
                    yield! allCombinations acc xs rem
                    yield! allCombinations (x :: acc) xs (rem - x)
        } |> Seq.toList
    
    allCombinations [] containers toStore

let part1() =
    let numCombinations = combinations allContainers 150 |> List.length

    printfn "Combinations: %i" numCombinations

let part2() =
    let combinationLengths = combinations allContainers 150 |> List.map List.length
    let minComb = combinationLengths |> List.min
    let numMinCombs = combinationLengths |> List.filter (fun n -> n = minComb) |> List.length

    printfn "Minimal combinations: %i" numMinCombs
