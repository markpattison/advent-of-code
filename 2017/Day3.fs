module Day3

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day3.txt").[0]

let start = input |> Int32.Parse

let location square =
    let rec xy n =
        if square > 4 * n * n + 4 * n + 1 then
            xy (n + 1)
        else
            if square <= 4 * n * n - 2 * n + 1 then
                (n, n - (4 * n * n - 2 * n + 1 - square))   // right side
            elif square <= 4 * n * n + 1 then
                (-n + (4 * n * n + 1 - square), n)          // top side
            elif square <= 4 * n * n + 2 * n + 1 then
                (-n, -n + (4 * n * n + 2 * n + 1 - square)) // left side
            else
                (n - (4 * n * n + 4 * n + 1 - square), -n)  // bottom side
    xy 0 

let part1() =
    let x, y = location start
    let distance = abs x + abs y

    printfn "Distance: %i" distance

let part2() =
    let mutable finished = false
    let mutable lastValue = 0
    let mutable values : Map<int * int, int> = Map.empty
    let mutable i = 1

    while not finished do
        let x, y = location i
        lastValue <-
            if i = 1 then
                1
            else
                [ 1, 0; 1, 1; 0, 1; -1, 1; -1, 0; -1, -1; 0, -1; 1, -1 ]
                |> List.choose (fun (dx, dy) -> Map.tryFind (x + dx, y + dy) values)
                |> List.sum
        if lastValue > start then finished <- true
        values <- values |> Map.add (x, y) lastValue
        i <- i + 1
    
    printfn "First larger value: %i" lastValue
