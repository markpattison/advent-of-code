module Day2

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day2.txt")

let parseLine (s: string) =
    s.Split(" ")
    |> Array.map Int32.Parse

let reports =
    input
    |> Array.map parseLine

let allIncreasing report =
    report
    |> Array.pairwise
    |> Array.forall (fun (l1, l2) -> l1 < l2)

let allDecreasing report =
    report
    |> Array.pairwise
    |> Array.forall (fun (l1, l2) -> l1 > l2)

let adjacency report =
    report
    |> Array.pairwise
    |> Array.forall (fun (l1, l2) -> abs (l1 - l2) >=1 && abs (l1 - l2) <= 3)

let isSafe report =
    (allIncreasing report || allDecreasing report)
    && (adjacency report)

let part1() =
    let safeReports =
        reports
        |> Array.filter isSafe
        |> Array.length
    
    printfn "Safe reports: %i" safeReports

let isSafeWithDampening report =
    if isSafe report then
        true
    else
        report
        |> Array.mapi (fun i level -> (i, level))
        |> Array.exists (fun (i, _) ->
            let newReport = report |> Array.removeAt i
            isSafe newReport)

let part2() =
    let safeReports =
        reports
        |> Array.filter isSafeWithDampening
        |> Array.length
    
    printfn "Safe reports: %i" safeReports
