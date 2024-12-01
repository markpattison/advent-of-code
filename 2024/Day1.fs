module Day1

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day1.txt")

let parseLine (s: string) =
    let numbers =
        s.Split("   ")
        |> Array.map(fun n -> Int32.Parse(n))
    
    (numbers.[0], numbers.[1])

let allPairs =
    input |> Array.map parseLine

let part1() =
    let orderedFirst =
        allPairs
        |> Array.map fst
        |> Array.sort
    
    let orderedSecond =
        allPairs
        |> Array.map snd
        |> Array.sort    
    
    let orderedPairs =
        orderedFirst
        |> Array.zip orderedSecond
    
    let difference =
        orderedPairs
        |> Array.map (fun (a, b) -> abs (a - b))
        |> Array.sum
    
    printfn "Difference: %i" difference

let part2() =
    let countSeconds =
        allPairs
        |> Array.countBy snd
    
    let similarity =
        allPairs
        |> Array.map (fun (a, _) ->
            match countSeconds |> Array.tryFind (fun (second, count) -> second = a) with
            | Some (_, count) -> a * count
            | None -> 0)
        |> Array.sum

    printfn "Similarity: %i" similarity
