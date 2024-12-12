module Day12

open System
open System.IO
open FSharp.Data

let input =
    File.ReadAllText(@"input\day12.txt")

let allNumbers (s: string) =
    let rec extractNumbers acc currentDigits currentPosition =
        if currentPosition >= s.Length then
            acc |> List.rev
        else
            let c = s.[currentPosition]
            if c = '-' || Char.IsAsciiDigit c then
                extractNumbers acc (c :: currentDigits) (currentPosition + 1)
            else
                if List.isEmpty currentDigits then
                    extractNumbers acc [] (currentPosition + 1)
                else
                    let num = currentDigits |> List.rev |> String.Concat |> Int32.Parse
                    extractNumbers (num :: acc) [] (currentPosition + 1)
    
    extractNumbers [] [] 0

let part1() =
    let sum = allNumbers input |> List.sum
    
    printfn "Sum: %i" sum

let containsRed jsonObjects =
    jsonObjects
    |> Array.exists (fun (_, jsonValue) ->
        match jsonValue with
        | JsonValue.String "red" -> true
        | _ -> false)

let rec sumNonRedNumbers jsonValue =
    match jsonValue with
    | JsonValue.Number n -> int n
    | JsonValue.Record record when not (containsRed record) ->
        record |> Array.sumBy (snd >> sumNonRedNumbers)
    | JsonValue.Array arr ->
        arr |> Array.sumBy sumNonRedNumbers
    | _ -> 0

let part2() =
    let json = JsonValue.Parse input
    let sum = sumNonRedNumbers json

    printfn "Sum not red: %i" sum
