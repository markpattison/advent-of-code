module Day6

open System.IO

let line = File.ReadAllLines(@"input\day6.txt").[0]

let initialArray = line.Split(',') |> Array.map System.Int32.Parse

let initialState = Array.init 9 (fun n -> initialArray |> Seq.filter (fun m -> m = n) |> Seq.length |> uint64)

let update (state: uint64[]) _ =
    Array.init 9 (fun n ->
        match n with
        | 8 -> state.[0]
        | 6 -> state.[7] + state.[0]
        | n -> state.[n + 1])

let projected initial days = Array.fold update initial [| 1 .. days |] |> Array.sum

let part1() =
    let days = 80

    printfn "Number of lanternfish after %i days: %i" days (projected initialState days)

let part2() =
    let days = 256

    printfn "Number of lanternfish after %i days: %i" days (projected initialState days)
