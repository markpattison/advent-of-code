module Day2

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day2.txt").[0]

type Range = { Min: int64; Max: int64 }

let allRanges =
    input.Split(',')
    |> Array.map (fun s ->
        let parts = s.Split('-')
        { Min = parts.[0] |> Int64.Parse; Max = parts.[1] |> Int64.Parse })

let isInvalid (n: int64) =
    let s = n.ToString()
    match s.Length % 2 with
    | 1 -> false
    | _ ->
        s.Substring(0, s.Length / 2) = s.Substring(s.Length / 2)

let totalInvalid range =
    seq { range.Min .. range.Max } |> Seq.filter isInvalid |> Seq.sum

let part1() =
    let total =
        allRanges
        |> Array.sumBy totalInvalid
    
    printfn "Total invalid: %i" total

let isInvalid2 (n: int64) =
    let s = n.ToString()
    if s.Length % 2 = 0
        && s.Substring(0, s.Length / 2) = s.Substring(s.Length / 2) then
        true
    elif s.Length % 3 = 0
        && s.Substring(0, s.Length / 3) = s.Substring(s.Length / 3, s.Length / 3)
        && s.Substring(s.Length / 3, s.Length / 3) = s.Substring(s.Length * 2 / 3, s.Length / 3) then
        true
    elif s.Length % 4 = 0
        && s.Substring(0, s.Length / 4) = s.Substring(s.Length / 4, s.Length / 4)
        && s.Substring(s.Length / 4, s.Length / 4) = s.Substring(s.Length * 2 / 4, s.Length / 4)
        && s.Substring(s.Length * 2 / 4, s.Length / 4) = s.Substring(s.Length * 3 / 4, s.Length / 4) then
        true
    elif s.Length % 5 = 0
        && s.Substring(0, s.Length / 5) = s.Substring(s.Length / 5, s.Length / 5)
        && s.Substring(s.Length / 5, s.Length / 5) = s.Substring(s.Length * 2 / 5, s.Length / 5)
        && s.Substring(s.Length * 2 / 5, s.Length / 5) = s.Substring(s.Length * 3 / 5, s.Length / 5)
        && s.Substring(s.Length * 3 / 5, s.Length / 5) = s.Substring(s.Length * 4 / 5, s.Length / 5) then
        true
    elif s.Length > 1 && s.ToCharArray() |> Array.distinct |> Array.length = 1 then
        true
    else
        false

let totalInvalid2 range =
    seq { range.Min .. range.Max } |> Seq.filter isInvalid2 |> Seq.sum

let part2() =
    let total =
        allRanges
        |> Array.sumBy totalInvalid2
    
    printfn "Total invalid: %i" total
