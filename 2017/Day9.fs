module Day9

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day9.txt").[0]

type Thing =
    | Garbage of char list
    | Group of Thing list

type Token =
    | StartGroup
    | EndGroup
    | SomeGarbage of char list

let tokenise (s: string) =
    let rec tokeniseChars acc cancelNext currentGarbage chars =
        match chars with
        | [] -> List.rev acc
        | x :: xs ->
            match x, cancelNext, currentGarbage with
            | _, true, None -> failwith "unexpected"
            | _, true, Some g -> tokeniseChars acc false (Some g) xs
            | '>', false, Some g -> tokeniseChars (SomeGarbage (List.rev g) :: acc) false None xs
            | '!', false, Some g -> tokeniseChars acc true (Some g) xs
            | c, false, Some g -> tokeniseChars acc false (Some (c :: g)) xs
            | ',', false, None -> tokeniseChars acc false None xs
            | '{', false, None -> tokeniseChars (StartGroup :: acc) false None xs
            | '}', false, None -> tokeniseChars (EndGroup :: acc) false None xs
            | '<', false, None -> tokeniseChars acc false (Some []) xs
            | _, false, None -> failwith "unexpected"

    tokeniseChars [] false None (s.ToCharArray() |> Array.toList)

let parse tokenList =
    let findGroupSize tokens =
        tokens
        |> Seq.scan (fun gc t -> gc + (if t = StartGroup then 1 elif t = EndGroup then -1 else 0)) 1
        |> Seq.takeWhile (fun gc -> gc > 0)
        |> Seq.length
        |> fun x -> x - 1

    let rec parseGroup acc tokens =
        match tokens with
        | [] -> List.rev acc
        | SomeGarbage g :: xs -> parseGroup (Garbage g :: acc) xs
        | StartGroup :: xs ->
            let groupSize = findGroupSize xs
            let group = List.take groupSize xs
            let remaining = List.skip (groupSize + 1) xs
            let parsedGroup = parseGroup [] group |> Group
            parseGroup (parsedGroup :: acc) remaining
        | EndGroup :: _ -> failwith "unexpected"
    
    parseGroup [] tokenList

let score things =
    let rec scoreLevel currLevel acc thingsAtLevel =
        match thingsAtLevel with
        | [] -> acc
        | Garbage _ :: xs -> scoreLevel currLevel acc xs
        | Group g :: xs ->
            let subGroup = scoreLevel (currLevel + 1) 0 g
            scoreLevel currLevel (acc + subGroup + currLevel) xs
    
    scoreLevel 1 0 things

let parsedInput = input |> tokenise |> parse

let part1() =
    let totalScore = parsedInput |> score
    
    printfn "Total score: %i" totalScore

let countGarbage things =
    let rec count acc remaining =
        match remaining with
        | [] -> acc
        | Garbage g :: xs -> count (acc + List.length g) xs
        | Group g :: xs ->
            let subGroup = count 0 g
            count (acc + subGroup) xs
    
    count 0 things

let part2() =
    let count = countGarbage parsedInput

    printfn "Count: %i" count
