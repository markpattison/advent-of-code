module Day15

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day15.txt")

type Disc =
    {
        Index: int
        Positions: int
        StartPosition: int
    }

let parseLine index (s: string) =
    let parts = s.Split(' ')
    {
        Index = index + 1
        Positions = parts.[3] |> Int32.Parse
        StartPosition = parts.[11].Replace(".", "") |> Int32.Parse
    }

let allDiscs =
    input
    |> Array.mapi parseLine

let isWinningMove discs startTime =
    discs
    |> Array.forall (fun disc -> (disc.StartPosition + startTime + disc.Index) % disc.Positions = 0)

let part1() =
    let firstWinningTime =
        Seq.initInfinite id
        |> Seq.find (isWinningMove allDiscs)
    
    printfn "First winning time: %i" firstWinningTime

let part2() =
    let newDiscs =
        [| { Index = 7; Positions = 11; StartPosition = 0 } |]
        |> Array.append allDiscs
    
    let firstWinningTime =
        Seq.initInfinite id
        |> Seq.find (isWinningMove newDiscs)
    
    printfn "First winning time: %i" firstWinningTime    
