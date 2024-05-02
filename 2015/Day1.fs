module Day1

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day1.txt")

type Direction = Up | Down

let parseDir c =
    match c with
    | '(' -> Up
    | ')' -> Down
    | _ -> failwith "unexpected"

let parseLine (s: string) =
    s.ToCharArray()
    |> Array.map parseDir

let directions = input.[0] |> parseLine

let floorInc dir =
    match dir with
    | Up -> 1
    | Down -> -1

let part1() =
    let floor =
        directions
        |> Array.sumBy floorInc
    
    printfn "Floor: %i" floor

let part2() =
    let firstBasementTime =
        directions
        |> Seq.scan (fun floor dir -> floor + floorInc dir) 0
        |> Seq.findIndex (fun floor -> floor < 0)
    
    // don't need to add 1, as Seq.scan includes the initial state at index 0

    printfn "Position: %i" firstBasementTime
