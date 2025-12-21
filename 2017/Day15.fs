module Day15

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day15.txt")

let startA = input.[0].Split(' ') |> Array.last |> UInt64.Parse
let startB = input.[1].Split(' ') |> Array.last |> UInt64.Parse
let start = startA, startB

let nextA a = a * 16807UL % 2147483647UL
let nextB b = b * 48271UL % 2147483647UL

let match16bits (a, b) = a % 65536UL = b % 65536UL

let part1() =
    let next (a, b) = nextA a, nextB b
    
    let count =
        seq { 1 .. 40000000 }
        |> Seq.scan (fun ab _ -> next ab) start
        |> Seq.skip 1
        |> Seq.filter match16bits
        |> Seq.length
    
    printfn "Count: %i" count

let rec nextA2 a =
    let next = nextA a
    if next % 4UL = 0UL then next else nextA2 next

let rec nextB2 b =
    let next = nextB b
    if next % 8UL = 0UL then next else nextB2 next

let part2() =
    let next (a, b) = nextA2 a, nextB2 b

    let count =
        seq { 1 .. 5000000 }
        |> Seq.scan (fun ab _ -> next ab) start
        |> Seq.skip 1
        |> Seq.filter match16bits
        |> Seq.length
    
    printfn "Count: %i" count
