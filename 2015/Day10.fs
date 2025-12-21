module Day10

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day10.txt")

let charToInt c = int c - int '0'

let initialSequence =
    input.[0].ToCharArray()
    |> Array.map charToInt
    |> Array.toList

let parseIntoChunks sequence =
    let rec parse acc currentNum currentVal rem =
        match rem with
        | [] ->
            ((currentNum, currentVal) :: acc) |> List.rev
        | x :: xs when x = currentVal ->
            parse acc (currentNum + 1) currentVal xs
        | x :: xs when currentNum > 0 ->
            parse ((currentNum, currentVal) :: acc) 1 x xs
        | x :: xs ->
            parse acc 1 x xs
        
    parse [] 0 0 sequence

let writeOut chunks =
    chunks
    |> Seq.collect (fun (num, value) ->
        seq {
            yield! num.ToString().ToCharArray() |> Array.map charToInt
            yield value
        })
    |> Seq.toList

let applyRepeatedly n startingSequence =
    seq { 1 .. n } |> Seq.fold (fun sequence _ -> sequence |> parseIntoChunks |> writeOut) startingSequence

let part1() =
    let sequence = applyRepeatedly 40 initialSequence
    printfn "Length: %i" sequence.Length

let part2() =
    let sequence = applyRepeatedly 50 initialSequence
    printfn "Length: %i" sequence.Length
