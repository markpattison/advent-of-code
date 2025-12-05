module Day5

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day5.txt")

type InclusiveRange = { Min: int64; Max: int64 }

let freshRanges =
    input
    |> Array.takeWhile (fun s -> s.Length > 0)
    |> Array.map (fun s ->
        let parts = s.Split('-')
        { Min = parts.[0] |> Int64.Parse; Max = parts.[1] |> Int64.Parse })

let available =
    input
    |> Array.skipWhile (fun s -> s.Contains('-'))
    |> Array.skip 1
    |> Array.map Int64.Parse

let isFresh a =
    freshRanges |> Array.exists (fun r -> r.Min <= a && r.Max >= a)

let part1() =
    let availableFresh =
        available
        |> Array.filter isFresh
        |> Array.length
    
    printfn "Available fresh: %i" availableFresh

type OrderedInclusiveRanges =
    {
        Ranges: InclusiveRange [] // ordered by Min
    }

let merge r1 r2 =
    let ra, rb = if r1.Min > r2.Min then r2, r1 else r1, r2 // now ra.Min >= rb.Min

    if rb.Min > ra.Max + 1L then
        [| ra; rb |] // do not overlap
    else
        let joint = { Min = ra.Min; Max = max ra.Max rb.Max}
        [| joint |]

let mergeOrdered oir r =
    if oir.Ranges.Length = 0 then
        { Ranges = [| r |] }
    else
        let allButLast = oir.Ranges |> Array.take (oir.Ranges.Length - 1)
        let lastRange = oir.Ranges |> Array.last
        let merged = merge lastRange r
        { Ranges = Array.append allButLast merged }

let part2() =
    let merged =
        freshRanges
        |> Array.sortBy _.Min
        |> Array.fold mergeOrdered { Ranges = [||] }
    
    let totalFresh =
        merged.Ranges
        |> Array.sumBy (fun r -> 1L + r.Max - r.Min)
    
    printfn "Total fresh: %i" totalFresh
