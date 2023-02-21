module Day1

open System.IO

let values =
    File.ReadAllLines(@"input\day1.txt")
    |> Array.map(fun s -> System.Int32.Parse(s))

let part1() =
    let pairs = values |> Array.pairwise
    let largerThanPrevious = pairs |> Array.filter (fun (a, b) -> b > a) |> Array.length
    printfn "Larger than previous: %i" largerThanPrevious

let part2() =
    let threeDaySums = values |> Array.windowed 3 |> Array.map (Array.sum)
    let largerSumsThanPrevious = threeDaySums |> Array.pairwise |> Array.filter (fun (a, b) -> b > a) |> Array.length
    printfn "Larger than previous sum: %i" largerSumsThanPrevious
