module Day10

open Checked
open System.IO

let lines = File.ReadAllLines(@"input\day10.txt")

type Bracket = Round | Square | Angle | Squiggly

type Type =
    | Open of Bracket
    | Close of Bracket

let parse c =
    match c with
    | '(' -> Open Round
    | ')' -> Close Round
    | '[' -> Open Square
    | ']' -> Close Square
    | '<' -> Open Angle
    | '>' -> Close Angle
    | '{' -> Open Squiggly
    | '}' -> Close Squiggly
    | _ -> failwithf "Unknown: %c" c

let parsed =
    lines
    |> Array.map (Seq.toList >> List.map parse)

type ParseResult = ParsedOk of Bracket list | Failed of Bracket

let rec parseChunks revAcc remaining =
    match remaining with
    | [] -> ParsedOk revAcc
    | (Open br) :: rem -> parseChunks (br :: revAcc) rem
    | (Close br) :: rem ->
        match revAcc with
        | openChunk :: remAcc when openChunk = br -> parseChunks remAcc rem
        | _ -> Failed br

let parseLine = parseChunks []

let part1() =

    let lineResults =
        parsed
        |> Array.map parseLine

    let score parseResult =
        match parseResult with
        | ParsedOk _ -> 0
        | Failed Round -> 3
        | Failed Square -> 57
        | Failed Squiggly -> 1197
        | Failed Angle -> 25137

    let syntaxErrorScore = lineResults |> Array.sumBy score

    printfn "Syntax error score: %i" syntaxErrorScore

let part2() =
    
    let incompleteFragments =
        parsed
        |> Array.map parseLine
        |> Array.choose (fun pr -> match pr with | ParsedOk revAcc -> Some revAcc | Failed _ -> None)

    let bracketScore br =
        match br with
        | Round -> 1UL
        | Square -> 2UL
        | Squiggly -> 3UL
        | Angle -> 4UL

    let rec score acc brackets =
        match brackets with
        | [] -> acc
        | br :: rem -> score (5UL * acc + bracketScore br) rem

    let scores =
        incompleteFragments
        |> Array.map (score 0UL)

    let middleScore =
        scores
        |> Array.sort
        |> Array.item ((scores.Length - 1) / 2)

    printfn "Middle score: %i" middleScore
