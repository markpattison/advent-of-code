module Day2

open System.IO

type Instruction =
    | Forward of int
    | Up of int
    | Down of int

let parse (s: string) =
    let words = s.Split(' ')
    let word = words.[0]
    let distance = System.Int32.Parse(words.[1])

    match word with
    | "forward" -> Forward distance
    | "up" -> Up distance
    | "down" -> Down distance
    | _ -> failwithf "%s" word

let instructions =
    File.ReadAllLines(@"input\day2.txt")
    |> Array.map parse

let part1() =
    let start = (0, 0)

    let move (horizontal, depth) instruction =
        match instruction with
        | Forward x -> (horizontal + x, depth)
        | Up x -> (horizontal, depth - x)
        | Down x -> (horizontal, depth + x)

    let (finalHorizontal, finalDepth) = Array.fold move start instructions

    let multiplied = finalHorizontal * finalDepth

    printfn "Final position: (%i, %i), multiplied: %i" finalHorizontal finalDepth multiplied

let part2() =
    let start = (0, 0, 0) // horizontal, depth, aim

    let move (horizontal, depth, aim) instruction =
        match instruction with
        | Forward x -> (horizontal + x, depth + x * aim, aim)
        | Up x -> (horizontal, depth, aim - x)
        | Down x -> (horizontal, depth, aim + x)

    let (finalHorizontal, finalDepth, _) = Array.fold move start instructions

    let multiplied = finalHorizontal * finalDepth

    printfn "Final position 2: (%i, %i), multiplied: %i" finalHorizontal finalDepth multiplied
