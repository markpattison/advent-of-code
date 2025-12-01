module Day1

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day1.txt")

type Dir = L | R
type Move = Dir * int

let parseLine (s: string) : Move =
    let dir =
        match s.[0] with
        | 'L' -> L
        | 'R' -> R
        | _ -> failwith "unexpected"
    let num = s.Substring(1) |> Int32.Parse

    dir, num

let allMoves = input |> Array.map parseLine

let applyMove pos (dir, num) =
    match dir with
    | L -> (10000 + pos - num) % 100
    | R -> (pos + num) % 100

let part1() =
    let allPos = Array.scan applyMove 50 allMoves
    let zeroes = allPos |> Array.filter (fun p -> p = 0) |> Array.length

    printfn "Zeroes: %i" zeroes

let allSingle =
    Array.collect (fun (dir, num) -> Array.create num dir) allMoves

let applyMove2 pos dir =
    match dir with
    | L -> (100 + pos - 1) % 100
    | R -> (pos + 1) % 100

let part2() =
    let allPos = Array.scan applyMove2 50 allSingle
    let zeroes = allPos |> Array.filter (fun p -> p = 0) |> Array.length

    printfn "Zeroes: %i" zeroes
