module Day3

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day3.txt")

let blank = '.'
let symbols = @"/*=%@&-+$#".ToCharArray()

type Token =
    | Number of int
    | Symbol of char

type FoundToken = Token * int

let rec parseLine (revAcc: FoundToken list) positionReached (s: string) =
    if s.Length = 0 then
        List.rev revAcc
    elif s.StartsWith(blank) then
        parseLine revAcc (positionReached + 1) (s.Substring(1))
    elif Array.contains (s.Chars(0)) symbols then
        let foundToken = (Symbol (s.Chars(0)), positionReached)
        parseLine (foundToken :: revAcc) (positionReached + 1) (s.Substring(1))
    else // number
        let howManyDigits =
            match s.ToCharArray() |> Array.tryFindIndex (fun c -> not (Char.IsDigit(c))) with
                | None -> s.Length
                | Some n -> n
        let number = Int32.Parse(s.Substring(0, howManyDigits))
        let foundToken = (Number number, positionReached)
        parseLine (foundToken :: revAcc) (positionReached + howManyDigits) (s.Substring(howManyDigits))
        
let isSymbolBetween minPos maxPos (ft: FoundToken) =
    match ft with
    | Number _, _ -> false
    | Symbol _, pos -> pos >= minPos && pos <= maxPos

let tokens =
    input
    |> Array.map (parseLine [] 0)

let numLines = tokens.Length

let part1() =
    let isPartNumber (ft: FoundToken) lineNum =
        match ft with
        | (Symbol _, _) -> false
        | (Number n, pos) ->
            let endPos = pos + n.ToString().Length - 1

            (lineNum > 0 && tokens.[lineNum - 1] |> List.exists (fun t -> isSymbolBetween (pos - 1) (endPos + 1) t)) ||
            (tokens.[lineNum] |> List.exists (fun t -> isSymbolBetween (pos - 1) (endPos + 1) t)) ||
            (lineNum < numLines - 1 && tokens.[lineNum + 1] |> List.exists (fun t -> isSymbolBetween (pos - 1) (endPos + 1) t))
    
    let partNumbers = tokens |> Array.mapi (fun lineNum ts -> ts |> List.filter (fun t -> isPartNumber t lineNum)) |> Seq.concat
    let sum = partNumbers |> Seq.map fst |> Seq.sumBy (fun t -> match t with Number n -> n | _ -> 0)

    printfn "Sum: %i" sum

let isNumberAdjacentTo symbolPos (ft: FoundToken) =
    match ft with
    | (Symbol _, _) -> None
    | (Number n, pos) ->
        let endPos = pos + n.ToString().Length - 1
        if (pos <= symbolPos - 1 && endPos >= symbolPos + 1) ||
           (pos >= symbolPos - 1 && pos <= symbolPos + 1) ||
           (endPos >= symbolPos - 1 && endPos <= symbolPos + 1) then
            Some n
        else
            None

let part2() =
    let gearRatio (ft: FoundToken) lineNum =
        match ft with
        | (Symbol '*', pos) ->
            let adjacentNumbers = seq {
                if lineNum > 0 then yield! tokens.[lineNum - 1] |> List.choose (isNumberAdjacentTo pos)
                yield! tokens.[lineNum] |> List.choose (isNumberAdjacentTo pos)
                if lineNum < numLines - 1 then yield! tokens.[lineNum + 1] |> List.choose (isNumberAdjacentTo pos)
            }
            match Seq.toList adjacentNumbers with
            | [ a; b ] -> Some (a * b)
            | _ -> None
        | _ -> None
    
    let gearRatios = tokens |> Array.mapi (fun lineNum ts -> ts |> Seq.choose (fun ft -> gearRatio ft lineNum)) |> Seq.concat

    let sum = gearRatios |> Seq.sum

    printfn "Sum: %i" sum
