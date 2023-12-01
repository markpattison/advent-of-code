module Day1

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day1.txt")

let parseLine (s: string) =
    let digits =
        s.ToCharArray()
        |> Array.filter (fun c -> Char.IsAsciiDigit(c))
    
    let value = Int32.Parse(string digits.[0]) * 10 + Int32.Parse(string digits.[digits.Length - 1])

    value

let part1() =
    let sum =
        input
        |> Array.map parseLine
        |> Array.sum
    
    printfn "Sum of values: %i" sum

let replaceStringDigits (s: string) =
    s
        .Replace("one", "o1e")
        .Replace("two", "t2o")
        .Replace("three", "t3e")
        .Replace("four", "f4r")
        .Replace("five", "f5e")
        .Replace("six", "s6x")
        .Replace("seven", "s7n")
        .Replace("eight", "e8t")
        .Replace("nine", "n9e")

let part2() =
    let sum =
        input
        |> Array.map replaceStringDigits
        |> Array.map parseLine
        |> Array.sum
    
    printfn "Sum of values: %i" sum
