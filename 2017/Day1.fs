module Day1

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day1.txt").[0]

let part1() =
    let chars = input.ToCharArray()
    let length = chars.Length

    let captcha =
        seq { 0 .. (length - 1) }
        |> Seq.filter (fun i -> input.[i] = input.[(i + 1) % length])
        |> Seq.map (fun i -> int input.[i] - int '0')
        |> Seq.sum
    
    printfn "Captcha: %i" captcha

let part2() =
    let chars = input.ToCharArray()
    let length = chars.Length

    let captcha =
        seq { 0 .. (length - 1) }
        |> Seq.filter (fun i -> input.[i] = input.[(i + length / 2) % length])
        |> Seq.map (fun i -> int input.[i] - int '0')
        |> Seq.sum
    
    printfn "Captcha: %i" captcha
