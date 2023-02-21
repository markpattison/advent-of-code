module Day4

open System
open System.IO

let lines = File.ReadAllLines(@"input\day4.txt")

let calls = lines.[0].Split(',') |> Array.map System.Int32.Parse

let toBoard (sixLines: string[]) =
    let lines =
        sixLines
        |> Array.skip 1
        |> Array.map (fun s -> s.Split(' ', StringSplitOptions.RemoveEmptyEntries))

    Array2D.init 5 5 (fun x y -> System.Int32.Parse(lines.[y].[x]))

let boards =
    lines
    |> Array.skip 1
    |> Array.chunkBySize 6
    |> Array.map toBoard

let isRowOrColumnWinning rc = rc |> Array.contains false |> not

let isWinning (marked: bool[,]) =
    seq {
        for i in 0..4 -> marked.[i, *] |> isRowOrColumnWinning
        for i in 0..4 -> marked.[*, i] |> isRowOrColumnWinning
    } |> Seq.contains true

let winTimeWithScore (board: int[,]) =
    let marked: bool[,] = Array2D.create 5 5 false

    let rec turnsNeeded i = 

        let call = calls.[i]

        for x in 0 .. 4 do
            for y in 0 .. 4 do
                if board.[x, y] = call then marked.[x, y] <- true

        if isWinning marked then
            let mutable unmarked = 0

            for x in 0 .. 4 do
                for y in 0 .. 4 do
                    if not marked.[x, y] then unmarked <- unmarked + board.[x, y]

            (i + 1, unmarked * call)
        else
            turnsNeeded (i + 1)

    turnsNeeded 0

let winTimesAndScores =
    boards
    |> Array.map winTimeWithScore

let part1() =

    let winningScore =
        winTimesAndScores
        |> Array.minBy fst
        |> snd

    printfn "First winning score: %i" winningScore

let part2() =

    let winningScore =
        winTimesAndScores
        |> Array.maxBy fst
        |> snd

    printfn "Last winning score: %i" winningScore
