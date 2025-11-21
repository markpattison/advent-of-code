module Day4

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day4.txt")

let allWords =
    input
    |> Array.map (fun s -> s.Split())

let isValid arr =
    arr |> Array.length = (arr |> Array.distinct |> Array.length)

let part1() =
    let numValid =
        allWords
        |> Array.filter isValid
        |> Array.length
    
    printfn "Number valid: %i" numValid

let isAnagram (s1: string) (s2: string) =
    s1.ToCharArray() |> Array.sort = (s2.ToCharArray() |> Array.sort)

let uniquePairs arr =
    let length = Array.length arr

    seq {
        for i in 0 .. length - 2 do
            for j in i + 1 .. length - 1 do
                yield arr.[i], arr.[j]
    }

let isValid2 arr =
    arr
    |> uniquePairs
    |> Seq.forall (fun (s1, s2) -> not (isAnagram s1 s2))

let part2() =
    let numValid =
        allWords
        |> Array.filter isValid2
        |> Array.length
    
    printfn "Number valid: %i" numValid
