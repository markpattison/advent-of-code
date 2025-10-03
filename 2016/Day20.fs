module Day20

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day20.txt")

type Block =
    {
        First: uint
        Last: uint
    }

let parseLine (s: string) =
    match s.Split("-") with
    | [| a; b |] -> { First = UInt32.Parse(a); Last = UInt32.Parse(b) }
    | _ -> failwith "unexpected"

let allSortedBlocks =
    input
    |> Array.map parseLine
    |> Array.sortBy _.First
    |> Array.toList

let part1() =
    let rec findFirstAllowed tryNext remainingBlocks =
        match remainingBlocks with
        | [] -> tryNext
        | x :: _ when x.First > tryNext -> tryNext
        | x :: xs when x.Last < tryNext -> findFirstAllowed tryNext xs
        | x :: xs -> findFirstAllowed (x.Last + 1u) xs

    let firstAllowed = findFirstAllowed 0u allSortedBlocks
    
    printfn "First allowed address: %i" firstAllowed

let part2() =
    let rec findNumberAllowed numSoFar tryNext remainingBlocks =
        match remainingBlocks with
        | [] -> numSoFar + 1u + UInt32.MaxValue - tryNext
        | x :: xs when x.First > tryNext ->
            let newNum = numSoFar + x.First - tryNext
            if x.Last = UInt32.MaxValue then newNum else findNumberAllowed (numSoFar + x.First - tryNext) (x.Last + 1u) xs
        | x :: xs when x.Last < tryNext -> findNumberAllowed numSoFar tryNext xs
        | x :: xs -> if x.Last = UInt32.MaxValue then numSoFar else findNumberAllowed numSoFar (x.Last + 1u) xs
    
    let numAllowed = findNumberAllowed 0u 0u allSortedBlocks

    printfn "Number of allowed addresses: %i" numAllowed
