module Day7

open System
open System.IO

let input = File.ReadAllLines(@"input\day7.txt")

let parseLine (s: string) =
    let split = s.Split(":")
    let target = split.[0] |> (Int64.Parse)
    let values =
        split.[1].Substring(1).Split(" ")
        |> Array.map (Int64.Parse)
        |> Array.toList
    
    (target, values)

let equations = input |> Array.map parseLine

let canBeTrue useConcat (target : int64, values: int64 list) =
    let rec canBeTrueInner acc rem =
        if acc > target then
            false
        else
            match rem with
            | [] -> (acc = target)
            | x :: xs ->
                (canBeTrueInner (acc + x) xs) || (canBeTrueInner (acc * x) xs) || (useConcat && canBeTrueInner (Int64.Parse(acc.ToString() + x.ToString())) xs)
    
    canBeTrueInner values.Head values.Tail

let part1() =
    let total =
        equations
        |> Array.filter (canBeTrue false)
        |> Array.sumBy fst
    
    printfn "Total: %i" total

let part2() =
    let total =
        equations
        |> Array.filter (canBeTrue true)
        |> Array.sumBy fst
    
    printfn "Total: %i" total
