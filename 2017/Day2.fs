module Day2

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day2.txt")

let parseLine (s: string) =
    s.Split('\t')
    |> Array.map (Int32.Parse)

let allValues =
    input
    |> Array.map parseLine

let part1() =
    let checksum =
        allValues
        |> Array.map (fun arr -> Array.max arr - Array.min arr)
        |> Array.sum
    
    printfn "Checksum: %i" checksum

let part2() =
    let sum =
        allValues
        |> Array.map (fun arr ->
            Array.allPairs arr arr
            |> Array.map (fun (a, b) -> min a b, max a b)
            |> Array.find (fun (a, b) -> b % a = 0 && b <> a)
            |> fun (a, b) -> b / a)
        |> Array.sum
    
    printfn "Sum: %i" sum
