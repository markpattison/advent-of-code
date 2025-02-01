module Day25

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day25.txt")

let targetRow, targetCol =
    let parts = input.[0].Replace(",", "").Replace(".", "").Replace("  ", " ").Split(" ")

    (parts.[15] |> Int32.Parse, parts.[17] |> Int32.Parse)

let nextCode code =
    (code * 252533UL) % 33554393UL

let codeAt n =
    let mutable code = 20151125UL

    for _ in 1 .. n - 1 do
        code <- nextCode code
    
    code

let indexAt row col =
    row * (row - 1) / 2 + col * (col + 1) / 2 + (row - 1) * (col - 1)

let part1() =
    let index = indexAt targetRow targetCol
    let code = codeAt index

    printfn "Code: %i" code

let part2() =
    ()
