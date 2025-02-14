module Day9

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day9.txt")

let decompress (s: string) =
    let rec dec acc rem =
        if rem = "" then
            acc
        elif rem.StartsWith("(") then
            let closeIndex = rem.IndexOf(")")
            let parts = rem.Substring(1, closeIndex - 1).Split("x")
            let numToRepeat = parts.[0] |> Int32.Parse
            let repetitions = parts.[1] |> Int32.Parse
            let toRepeat = rem.Substring(closeIndex + 1, numToRepeat)
            dec (acc + String.replicate repetitions toRepeat) (rem.Substring(closeIndex + 1 + numToRepeat))
        else
            dec (acc + rem.Substring(0, 1)) (rem.Substring(1))
    
    dec "" s

let rec v2lengthDecompressed (s: string) =
    match s.IndexOf('(') with
    | -1 -> int64 s.Length
    | startIndex ->
        let closeIndex = s.IndexOf(')', startIndex)
        let parts = s.Substring(startIndex + 1, closeIndex - startIndex - 1).Split("x")
        let numToRepeat = parts.[0] |> Int32.Parse
        let repetitions = parts.[1] |> Int64.Parse
        let toRepeat = s.Substring(closeIndex + 1, numToRepeat)
        let remaining = s.Substring(closeIndex + numToRepeat + 1)
        (int64 startIndex) + (repetitions * v2lengthDecompressed toRepeat) + v2lengthDecompressed remaining

let part1() =
    let decompressed = decompress input.[0]
    let length = decompressed.Length

    printfn "Length: %i" length

let part2() =
    let length = v2lengthDecompressed input.[0]

    printfn "Length: %i" length
