module Day16

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day16.txt").[0]

let flipChar c =
    match c with
    | '0' -> '1'
    | '1' -> '0'
    | _ -> failwith "unexpected"

let expand (a: string) =
    let b =
        a.ToCharArray()
        |> Array.rev
        |> Array.map flipChar
        |> String
    
    a + "0" + b

let rec expandToLength requiredLength (s: string) =
    if s.Length >= requiredLength then
        s.Substring(0, requiredLength)
    else
        expand s |> expandToLength requiredLength

let matchPair arr =
    match arr with
    | [| |] -> None
    | [| _ |] -> None
    | [| a; b |] when a = b -> Some '1'
    | [| _; _ |] -> Some '0'
    | _ -> failwith "unexpected"

let rec checksum (s: string) =
    let interimChecksum =
        s.ToCharArray()
        |> Array.chunkBySize 2
        |> Array.choose matchPair
        |> String
    
    if interimChecksum.Length % 2 = 0 then
        checksum interimChecksum
    else
        interimChecksum

let part1() =
    let data = expandToLength 272 input
    let c = checksum data

    printfn "Checksum: %s" c

let part2() =
    let data = expandToLength 35651584 input
    let c = checksum data

    printfn "Checksum: %s" c
