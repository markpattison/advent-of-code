module Day11

open System
open System.IO

let input = File.ReadAllLines(@"input\day11.txt")

let initialStones =
    input.[0].Split(' ')
    |> Array.map UInt64.Parse

let numStonesWithCache blinks stones =
    let mutable cache : Map<uint64 * int, uint64> = Map.empty

    let rec numStones blinks stone =
        if blinks = 0 then
            1UL
        else
            match Map.tryFind (stone, blinks) cache with
            | Some length -> length
            | None ->
                let st = stone.ToString()
                let num =
                    match stone with
                    | 0UL -> numStones (blinks - 1) 1UL
                    | _ when st.Length % 2 = 0 ->
                        numStones (blinks - 1) (st.Substring(0, st.Length / 2) |> UInt64.Parse)
                        + numStones (blinks - 1) (st.Substring(st.Length / 2) |> UInt64.Parse)
                    | n -> numStones (blinks - 1) (n * 2024UL)
                cache <- cache |> Map.add (stone, blinks) num
                num
    
    stones
    |> Array.map (numStones blinks)
    |> Array.sum

let part1() =
    let totalStones = numStonesWithCache 25 initialStones
    printfn "Stones: %i" totalStones

let part2() =
    let totalStones = numStonesWithCache 75 initialStones
    printfn "Stones: %i" totalStones
