module Day19

open System
open System.IO
open System.Text.RegularExpressions

let input =
    File.ReadAllLines(@"input\day19.txt")

let numReplacements =
    input
    |> Array.findIndex (fun s -> s = "")

let allReplacements =
    input
    |> Array.take numReplacements
    |> Array.map (fun s ->
        let parts = s.Split(" => ")
        parts.[0], parts.[1])

let startingMolecule = input.[numReplacements + 1]

let allMolecules (s: string) (rFrom : string, rTo : string) =
    let fromLength = rFrom.Length
    seq {
        for i in 0 .. s.Length - fromLength do
            if s.Substring(i, fromLength) = rFrom then
                yield s.Substring(0, i) + rTo + s.Substring(i + fromLength)
    }

let distinctMolecules s =
        allReplacements
        |> Seq.collect (allMolecules s)
        |> Seq.distinct

let part1() =
    let numDistinct =
        distinctMolecules startingMolecule
        |> Seq.length
    
    printfn "Distinct molecules: %i" numDistinct

let part2() =
    let countElements = Regex.Matches(startingMolecule, "[A-Z]").Count
    let countRn = Regex.Matches(startingMolecule, "Rn").Count
    let countAr = Regex.Matches(startingMolecule, "Ar").Count
    let countY = Regex.Matches(startingMolecule, "Y").Count

    let numSteps = countElements - countRn - countAr - 2 * countY - 1

    printfn "Steps: %O" numSteps
