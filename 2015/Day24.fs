module Day24

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day24.txt")

let allWeights =
    input
    |> Array.map UInt64.Parse
    |> Array.toList

let totalWeights = List.sum allWeights

let allCombinations target weights =
    let rec findCombinations alreadyIn remaining targetLeft =
        match targetLeft with
        | 0UL -> [ alreadyIn ]
        | n when n < 0UL -> []
        | n ->
            match remaining with
            | [] -> []
            | x :: _ when x = n ->
                [ x :: alreadyIn ]
            | x :: xs when x > n ->
                    findCombinations alreadyIn xs targetLeft
            | x :: xs ->
                List.append
                    (findCombinations (x :: alreadyIn) xs (targetLeft - x))
                    (findCombinations alreadyIn xs targetLeft)
    
    findCombinations [] weights target

let product l = List.fold (*) 1UL l

let part1() =
    let target = totalWeights / 3UL
    let combs = allCombinations target allWeights
    let sorted = combs |> List.sortBy (fun comb -> (List.length comb, product comb))
    
    // in theory should check that remaining packages can be evenly distributed

    let entanglement = product (List.head sorted)

    printfn "Quantum entanglement: %i" entanglement

let part2() =
    let target = totalWeights / 4UL
    let combs = allCombinations target allWeights
    let sorted = combs |> List.sortBy (fun comb -> (List.length comb, product comb))
    
    // in theory should check that remaining packages can be evenly distributed

    let entanglement = product (List.head sorted)

    printfn "Quantum entanglement: %i" entanglement
