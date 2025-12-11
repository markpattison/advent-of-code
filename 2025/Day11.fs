module Day11

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day11.txt")

let parseLine (s: string) =
    let parts = s.Split(' ')
    let name = parts.[0].Substring(0, parts.[0].Length - 1)
    let outputs = parts |> Array.skip 1
    name, outputs

let deviceMaps = input |> Array.map parseLine |> Map.ofArray |> Map.add "out" [||]

let part1() =
    let rec countPaths from =
        if from = "out" then
            1
        else
            deviceMaps.[from] |> Array.sumBy countPaths

    let numPaths = countPaths "you"
    
    printfn "Number of paths: %i" numPaths

let part2() =
    let nextLocs dest (currLoc, num) =
        if currLoc = dest then
            num, [||]
        else
            0L, deviceMaps.[currLoc] |> Array.map (fun next -> next, num)
    
    let rec countPaths dest found currLocs =
        if Array.length currLocs = 0 then
            found
        else
            let allFoundAndNext = currLocs |> Array.map (nextLocs dest)
            let newFound = allFoundAndNext |> Array.sumBy fst

            let squashedNext =
                allFoundAndNext
                |> Array.map snd
                |> Array.concat
                |> Array.groupBy fst
                |> Array.map (fun (next, group) -> next, group |> Array.sumBy snd)

            countPaths dest (found + newFound) squashedNext
    
    let countFromTo from dest =
        countPaths dest 0L [| from, 1L |]

    let pathsSvrDac = countFromTo "svr" "dac"
    let pathsDacFft = countFromTo "dac" "fft"
    let pathsFftOut = countFromTo "fft" "out"

    let pathsSvrFft = countFromTo "svr" "fft"
    let pathsFftDac = countFromTo "fft" "dac"
    let pathsDacOut = countFromTo "dac" "out"
    
    let numPaths = pathsSvrDac * pathsDacFft * pathsFftOut + pathsSvrFft * pathsFftDac * pathsDacOut

    printfn "Number of paths: %i" numPaths
