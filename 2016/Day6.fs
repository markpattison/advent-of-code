module Day6

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day6.txt")

let charArrays = input |> Array.map (fun s -> s.ToCharArray())
let messageLength = input.[0].Length

let part1() =
    let message =
        Array.init messageLength (fun i ->
            charArrays
            |> Array.map (fun ca -> ca.[i])
            |> Array.countBy id
            |> Array.sortByDescending snd
            |> Array.head
            |> fst)
        |> String.Concat
    
    printfn "Message: %s" message

let part2() =
    let message =
        Array.init messageLength (fun i ->
            charArrays
            |> Array.map (fun ca -> ca.[i])
            |> Array.countBy id
            |> Array.sortByDescending snd
            |> Array.last
            |> fst)
        |> String.Concat
    
    printfn "Message: %s" message
