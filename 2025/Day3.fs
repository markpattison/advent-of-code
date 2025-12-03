module Day3

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day3.txt")

let sizeX = input.[0].Length
let sizeY = input.Length

let allJoltages =
    Array.init sizeY (fun y -> Array.init sizeX (fun x -> int input.[y].[x] - int '0'))

let highest digits arr =
    let rec findHighest acc toFind remaining  =
        if toFind = 0 then
            acc
        else
            let maxDigit = remaining |> Array.take (1 + remaining.Length - toFind) |> Array.max
            let firstIndex = remaining |> Array.findIndex (fun n -> n = maxDigit)

            findHighest (10L * acc + int64 maxDigit) (toFind - 1) (remaining |> Array.skip (firstIndex + 1))
    
    findHighest 0L digits arr

let part1() =
    let total = allJoltages |> Array.map (highest 2) |> Array.sum

    printfn "Total: %i" total

let part2() =
    let total = allJoltages |> Array.map (highest 12) |> Array.sum

    printfn "Total: %i" total
