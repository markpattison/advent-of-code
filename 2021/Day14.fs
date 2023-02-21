module Day14

open Checked
open System.IO

let lines = File.ReadAllLines(@"input\day14.txt")

let testLines =
    [|
        "NNCB"
        ""
        "CH -> B"
        "HH -> N"
        "CB -> H"
        "NH -> C"
        "HB -> C"
        "HC -> B"
        "HN -> C"
        "NN -> C"
        "BH -> H"
        "NC -> B"
        "NB -> B"
        "BN -> B"
        "BB -> N"
        "BC -> B"
        "CC -> N"
        "CN -> C"       
    |]

let toInputs (l: string[]) =
    let template = l.[0] |> Seq.toArray

    let toInsertion (s: string) =
        let parts = s.Split(" -> ")
        (parts.[0].[0], parts.[0].[1]), parts.[1].[0]

    let insertions =
        l.[2..]
        |> Array.map toInsertion
        |> Map.ofArray

    template, insertions

let template, insertions = toInputs lines

let allChars =
    insertions
    |> Map.toSeq
    |> Seq.collect (fun ((c1, c2), c3) -> seq { c1; c2; c3 })
    |> Seq.append template
    |> Seq.distinct
    |> Seq.toArray

let numChars = allChars.Length

let charMap =
    allChars
    |> Array.mapi (fun i c -> (c, i))
    |> Map.ofArray

let startingPairCounts =
    template
    |> Array.pairwise
    |> Array.countBy id

let startingPairCountArray =
    let arr : uint64[,] = Array2D.zeroCreate numChars numChars

    startingPairCounts
    |> Array.iter (fun ((c1, c2), count) -> arr.[charMap.[c1], charMap.[c2]] <- uint64 count)

    arr

let insertionsInt =
    insertions
    |> Map.toSeq
    |> Seq.map (fun ((c1, c2), cInsert) -> ((charMap.[c1], charMap.[c2]), charMap.[cInsert]))
    |> Map.ofSeq

let insert (insertions2: Map<int * int, int>) (countArray: uint64[,]) =
    let arr : uint64[,] = Array2D.zeroCreate numChars numChars

    for c1 in 0 .. (numChars - 1) do
        for c2 in 0 .. (numChars - 1) do
            match Map.tryFind (c1, c2) insertions2 with
            | Some cInsert ->
                arr.[c1, cInsert] <- arr.[c1, cInsert] + countArray.[c1, c2]
                arr.[cInsert, c2] <- arr.[cInsert, c2] + countArray.[c1, c2]
            | None -> arr.[c1, c2] <- arr.[c1, c2] + countArray.[c1, c2]
    arr

let rec buildPolymer countArray iterations  =
    match iterations with
    | 0 -> countArray
    | n -> buildPolymer (insert insertionsInt countArray) (n - 1)

let toCounts (polymer: uint64[,]) =
    let arr: uint64[] = Array.zeroCreate numChars

    for i in 0 .. (numChars - 1) do
        for j in 0 .. (numChars - 1) do
            arr.[i] <- arr.[i] + polymer.[i, j] // count the first element of each pair once
        
    let lastELement = Seq.last template
    arr.[charMap.[lastELement]] <- arr.[charMap.[lastELement]] + 1UL  // and count the last element once (the last element never changes)

    arr

let part1() =
    let steps = 10

    let counts = steps |> buildPolymer startingPairCountArray |> toCounts

    let lowest = counts |> Array.min
    let highest = counts |> Array.max

    printfn "Difference: %i" (highest - lowest)

let part2() =
    let steps = 40

    let counts = steps |> buildPolymer startingPairCountArray |> toCounts

    let lowest = counts |> Array.min
    let highest = counts |> Array.max

    printfn "Difference: %i" (highest - lowest)
