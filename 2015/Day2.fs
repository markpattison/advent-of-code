module Day2

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day2.txt")

type Box = { L: int; W: int; H: int }

let parseLine (s: string) =
    match s.Split('x') |> Array.map Int32.Parse with
    | [| l; w; h |] -> { L = l; W = w; H = h}
    | _ -> failwith "unexpected"

let boxes = input |> Array.map parseLine

let part1() =
    let paperNeeded box =
        let side1 = box.L * box.W
        let side2 = box.W * box.H
        let side3 = box.H * box.L

        let extra = min side1 (min side2 side3)

        extra + 2 * (side1 + side2 + side3)
    
    let totalPaper =
        boxes
        |> Array.sumBy paperNeeded

    printfn "Total paper: %i" totalPaper

let part2() =
    let ribbonNeeded box =
        let perimeter1 = 2 * (box.L + box.W)
        let perimeter2 = 2 * (box.W + box.H)
        let perimeter3 = 2 * (box.H + box.L)

        let smallest = min perimeter1 (min perimeter2 perimeter3)

        let bow = box.L * box.W * box.H

        smallest + bow
    
    let totalRibbon =
        boxes
        |> Array.sumBy ribbonNeeded
    
    printfn "Total ribbon: %i" totalRibbon
