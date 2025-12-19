module Day13

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day13.txt")

type Firewall = { Depth: int; Range: int }

let parseLine (s: string) =
    let parts = s.Split(": ") |> Array.map Int32.Parse

    { Depth = parts.[0]; Range = parts.[1] }

let allFirewalls =
    input
    |> Array.map parseLine

let part1() =
    let totalSeverity =
        allFirewalls
        |> Array.sumBy (fun fw ->
            if fw.Depth % (2 * (fw.Range - 1)) = 0 then fw.Range * fw.Depth else 0)

    printfn "Total severity: %i" totalSeverity

let part2() =
    let notCaught delay =
        allFirewalls
        |> Array.forall (fun fw -> (fw.Depth + delay) % (2 * (fw.Range - 1)) <> 0)

    let minDelay =
        Seq.initInfinite id
        |> Seq.find notCaught
    
    printfn "Minimum delay: %i" minDelay
