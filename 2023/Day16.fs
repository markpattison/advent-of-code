module Day16

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day16.txt")

let xLength = input.[0].Length
let yLength = input.Length

type Tile = Empty | MirrorNWSE | MirrorSWNE | SplitterH | SplitterV
type Dir = N | E | S | W

let backslashChar = @"\".[0]

let parseChar c =
    match c with
    | '.' -> Empty
    | x when x = backslashChar -> MirrorNWSE
    | '/' -> MirrorSWNE
    | '-' -> SplitterH
    | '|' -> SplitterV
    | _ -> failwith "unexpected"

let tiles =
    Array2D.init xLength yLength (fun x y -> parseChar input.[y].[x])

let getNumEnergised (dir, x, y) =
    let beamN = Array2D.create xLength yLength false
    let beamE = Array2D.create xLength yLength false
    let beamS = Array2D.create xLength yLength false
    let beamW = Array2D.create xLength yLength false

    let mutable activeBeams = [ dir, x, y ]

    let processBeam (dir, x, y) =
        if dir = E && x < xLength - 1 && not beamE.[x + 1, y] then
            beamE.[x + 1, y] <- true
            match tiles.[x + 1, y] with
            | Empty | SplitterH -> [ E, x + 1, y ]
            | MirrorNWSE -> [ S, x + 1, y ]
            | MirrorSWNE -> [ N, x + 1, y ]
            | SplitterV -> [ S, x + 1, y; N, x + 1, y ]
        elif dir = W && x > 0 && not beamW.[x - 1, y] then
            beamW.[x - 1, y] <- true
            match tiles.[x - 1, y] with
            | Empty | SplitterH -> [ W, x - 1, y ]
            | MirrorNWSE -> [ N, x - 1, y ]
            | MirrorSWNE -> [ S, x - 1, y ]
            | SplitterV -> [ S, x - 1, y; N, x - 1, y ]
        elif dir = S && y < yLength - 1 && not beamS.[x, y + 1] then
            beamS.[x, y + 1] <- true
            match tiles.[x, y + 1] with
            | Empty | SplitterV -> [ S, x, y + 1 ]
            | MirrorNWSE -> [ E, x, y + 1 ]
            | MirrorSWNE -> [ W, x, y + 1 ]
            | SplitterH -> [ E, x, y + 1; W, x, y + 1 ]
        elif dir = N && y > 0 && not beamN.[x, y - 1] then
            beamN.[x, y - 1] <- true
            match tiles.[x, y - 1] with
            | Empty | SplitterV -> [ N, x, y - 1 ]
            | MirrorNWSE -> [ W, x, y - 1 ]
            | MirrorSWNE -> [ E, x, y - 1 ]
            | SplitterH -> [ E, x, y - 1; W, x, y - 1 ]
        else
            []

    while not (List.isEmpty activeBeams) do
        activeBeams <- activeBeams |> List.collect processBeam
    
    let energised = Array2D.init xLength yLength (fun x y -> if beamN.[x, y] || beamE.[x, y] || beamS.[x, y] || beamW.[x, y] then 1 else 0)

    energised |> Seq.cast<int> |> Seq.sum

let part1() =
    let numEnergised = getNumEnergised (E, -1, 0)

    printfn "Number energised: %i" numEnergised

let part2() =
    let maxEnergised =
        seq {
            for y in 0 .. yLength - 1 do
                yield (E, -1, y)
                yield (W, xLength, y)
            for x in 0 .. xLength - 1 do
                yield (S, x, -1)
                yield (N, x, yLength)
        }
        |> Seq.map getNumEnergised
        |> Seq.max

    printfn "Max energised: %i" maxEnergised
