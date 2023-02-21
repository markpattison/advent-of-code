module Day8

open System.IO

let lines = File.ReadAllLines(@"input\day8.txt")

let stringSort (s: string) = s |> Seq.toArray |> Array.sort

let parseLine (line: string) =
    let split = line.Split('|') |> Array.map (fun s -> s.Trim())

    let allPatterns, allOutput = split.[0], split.[1]

    let patterns = allPatterns.Split(' ') |> Array.map stringSort
    let outputs = allOutput.Split(' ') |> Array.map stringSort

    patterns, outputs

let data = lines |> Array.map parseLine

let part1() =

    let oneFourSevenEight l =
        l = 2 || l = 4 || l = 3 || l = 7

    let onesFoursSevensEights =
        data
        |> Array.collect snd
        |> Array.filter (fun output -> oneFourSevenEight output.Length)
        |> Array.length

    printfn "Ones, fours, sevens, eights: %i" onesFoursSevensEights

let part2() =

    let includes inner outer =
        inner |> Array.forall (fun c -> Array.contains c outer)

    let overlaps a b =
        a |> Array.filter (fun c -> Array.contains c b) |> Array.length

    let parse (patterns, outputs) =
        let found : char[][] = Array.zeroCreate 10

        let allOfLength n = patterns |> Array.filter (fun ch -> Array.length ch = n)
        let onlyOfLength n = allOfLength n |> Array.exactlyOne
        
        found.[1] <- onlyOfLength 2
        found.[4] <- onlyOfLength 4
        found.[7] <- onlyOfLength 3
        found.[8] <- onlyOfLength 7

        let lengthFive = allOfLength 5
        let lengthSix = allOfLength 6

        found.[2] <- lengthFive |> Array.filter (fun ch -> overlaps ch found.[4] = 2) |> Array.exactlyOne
        found.[3] <- lengthFive |> Array.filter (includes found.[1]) |> Array.exactlyOne
        found.[5] <- lengthFive |> Array.filter (fun ch -> ch <> found.[2] && ch <> found.[3]) |> Array.exactlyOne

        found.[9] <- lengthSix |> Array.filter (includes found.[4]) |> Array.exactlyOne
        found.[0] <- lengthSix |> Array.filter (fun ch -> includes found.[1] ch && ch <> found.[9]) |> Array.exactlyOne
        found.[6] <- lengthSix |> Array.filter (fun ch -> ch <> found.[0] && ch <> found.[9]) |> Array.exactlyOne

        let parseDigit ch =
            Array.findIndex (fun c -> c = ch) found
        
        let digits =
            outputs
            |> Array.map parseDigit
            |> Array.map (fun i -> i.ToString())
        
        System.String.Join("", digits) |> System.Int32.Parse

    let results =
        data
        |> Array.map parse
    
    let sum = results |> Array.sum

    printfn "Sum of outputs: %i" sum
