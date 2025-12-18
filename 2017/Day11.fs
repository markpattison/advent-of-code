module Day11

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day11.txt").[0]

type Direction = N | NE | SE | S | SW | NW

type AxialCoordinate =
    {
        Q: int
        R: int
    }

let parseDir s =
    match s with
    | "n" -> N
    | "ne" -> NE
    | "se" -> SE
    | "s" -> S
    | "sw" -> SW
    | "nw" -> NW
    | _ -> failwith "unexpected"

let allDirections =
    input.Split(',') |> Array.map parseDir

let move pos dir =
    match dir with
    | N  -> { Q = pos.Q;     R = pos.R - 1 }
    | NE -> { Q = pos.Q + 1; R = pos.R - 1 }
    | SE -> { Q = pos.Q + 1; R = pos.R     }
    | S  -> { Q = pos.Q;     R = pos.R + 1 }
    | SW -> { Q = pos.Q - 1; R = pos.R + 1 }
    | NW -> { Q = pos.Q - 1; R = pos.R     }

let distance pos1 pos2 =
    (abs(pos1.Q - pos2.Q) 
          + abs(pos1.Q + pos1.R - pos2.Q - pos2.R)
          + abs(pos1.R - pos2.R)) / 2

let origin = { Q = 0; R = 0 }

let part1() =
    let distance =
        allDirections
        |> Array.fold move origin
        |> distance origin

    printfn "Distance: %i" distance

let part2() =
    let maxDistance =
        allDirections
        |> Array.scan move origin
        |> Array.map (distance origin)
        |> Array.max
    
    printfn "Max distance: %i" maxDistance
