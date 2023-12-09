module Day9

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day9.txt")

let parseLine (s: string) =
    s.Split(' ')
    |> Array.map Int64.Parse

let sequences =
    input
    |> Array.map parseLine

let diffs sequence =
    sequence
    |> Array.pairwise
    |> Array.map (fun (a, b) -> b - a)

let isAllZeros sequence =
    sequence
    |> Array.forall (fun n -> n = 0L)

let generator sequence =
    if isAllZeros sequence then
        None
    else
        let d = diffs sequence
        Some (d, d)

let allDiffs sequence =
    let subDiffs =
        sequence
        |> Array.unfold generator
    
    Array.append [| sequence |] subDiffs

let extrapolateFromDiffs allDiffs =
    allDiffs |> Array.Reverse

    let newValues = Array.create allDiffs.Length 0L

    for i in 0 .. (allDiffs.Length - 1) do
        newValues.[i] <-
            if i = 0 then
                0L
            else
                newValues.[i - 1] + Array.last allDiffs.[i]
    
    Array.last newValues

let extrapolate sequence =
    sequence
    |> allDiffs
    |> extrapolateFromDiffs

let part1() =
    let values =
        sequences
        |> Array.map extrapolate
    
    let sum = Array.sum values

    printfn "Sum: %i" sum

let extrapolateBackFromDiffs (allDiffs: int64[][]) =
    allDiffs |> Array.Reverse

    let newValues = Array.create allDiffs.Length 0L

    for i in 0 .. (allDiffs.Length - 1) do
        newValues.[i] <-
            if i = 0 then
                0L
            else
                allDiffs.[i].[0] - newValues.[i - 1]
    
    Array.last newValues

let extrapolateBack sequence =
    sequence
    |> allDiffs
    |> extrapolateBackFromDiffs

let part2() =
    let values =
        sequences
        |> Array.map extrapolateBack
    
    let sum = Array.sum values

    printfn "Sum: %i" sum
