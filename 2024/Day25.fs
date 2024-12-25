module Day25

open System
open System.IO

let input = File.ReadAllLines(@"input\day25.txt")

let locks, keys =
    let mutable l = []
    let mutable k = []

    let rec parse (lines: string[]) =
        if lines.Length < 7 then
            ()
        else
            let arr = Array.take 7 lines
            let values = [| 0 .. 4 |] |> Array.map (fun i -> (arr |> Array.sumBy (fun a -> if a.[i] = '#' then 1 else 0)) - 1)
            if arr.[0] = "#####" then
                l <- values :: l
            else
                k <- values :: k
            lines |> Array.skip (min 8 lines.Length) |> parse
    
    parse input
    (l |> List.rev |> Array.ofList, k |> List.rev |> Array.ofList)

let part1() =
    let fitPairs =
        seq {
            for l in locks do
                for k in keys do
                    let fit = Array.zip l k |> Array.map (fun (a, b) -> a + b) |> Array.forall (fun x -> x <= 5)
                    if fit then 1 else 0
        } |> Seq.sum
    
    printfn "Fitting pairs: %i" fitPairs

let part2() =
    ()
