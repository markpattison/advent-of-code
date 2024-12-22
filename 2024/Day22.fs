module Day22

open System
open System.IO

let input = File.ReadAllLines(@"input\day22.txt")

let initialSecrets =
    input
    |> Array.map Int64.Parse

let nextSecret n =
    let a = ((n * 64L) ^^^ n) % 16777216L
    let b = ((a / 32L) ^^^ a) % 16777216L
    let c = ((b * 2048L) ^^^ b) % 16777216L
    c

let nextN t n =
    let mutable secret = n
    for _ in 1 .. t do
        secret <- nextSecret secret
    secret

let part1() =
    let sum =
        initialSecrets
        |> Array.map (nextN 2000)
        |> Array.sum

    printfn "Sum: %i" sum

let allPrices, allChanges =
    let getPricesAndChanges initial =
        let prices = Array.create 2000 0
        let changes = Array.create 2000 0
        let mutable secret = initial
        for i in 0 .. 1999 do
            let next = nextSecret secret
            let change = (next % 10L) - (secret % 10L)
            prices.[i] <- next % 10L |> int32
            changes.[i] <- change |> int32
            secret <- next
        prices, changes
    initialSecrets
    |> Array.map getPricesAndChanges
    |> Array.unzip

let hash a b c d = (a + 9) * 8000 + (b + 9) * 400 + (c + 9) * 20 + d + 9

let allHashes =
    let hashChanges (changes: int[]) =
        let hashes = Array.create 2000 -1
        for i in 3 .. 1999 do
            hashes.[i] <- hash changes.[i - 3] changes.[i - 2] changes.[i - 1] changes.[i]
        hashes

    allChanges |> Array.map hashChanges

let plausibleHashes =
    seq {
        for a in -9 .. 9 do
            for b in max -9 (-9 - a) .. min 9 (9 - a) do
                for c in max -9 (-9 - a - b) .. min 9 (9 - a - b) do
                    for d in max 0 (0 - a - b - c) .. min 9 (9 - a - b - c) do
                        hash a b c d
    } |> Seq.toArray

let part2() =
    let maxHash = plausibleHashes |> Array.max

    let pricesForHash =
        Array.init allHashes.Length (fun i ->
            let arr = Array.create (maxHash + 1) -1
            for j in 3 .. 1999 do
                let hash = allHashes.[i].[j]
                if arr.[hash] = -1 then arr.[hash] <- allPrices.[i].[j]
            arr)
    
    let maxBananas =
        plausibleHashes
        |> Array.map (fun h ->
            let mutable sum = 0
            for i in 0 .. allHashes.Length - 1 do
                if pricesForHash.[i].[h] > -1 then
                    sum <- sum + pricesForHash.[i].[h]
            sum)
        |> Array.max
    
    printfn "Max bananas: %i" maxBananas
