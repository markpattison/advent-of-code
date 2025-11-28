module Day5

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day5.txt")

let allJumps =
    input
    |> Array.map Int32.Parse

let part1() =
    let jumps = Array.copy allJumps
    let length = Array.length jumps

    let rec stepsToExit stepsSoFar index =
        if index < 0 || index >= length then
            stepsSoFar
        else
            let offset = jumps.[index]
            let newIndex = index + offset
            jumps.[index] <- jumps.[index] + 1
            stepsToExit (stepsSoFar + 1) newIndex
    
    let steps = stepsToExit 0 0

    printfn "Steps: %i" steps

let part2() =
    let jumps = Array.copy allJumps
    let length = Array.length jumps

    let rec stepsToExit stepsSoFar index =
        if index < 0 || index >= length then
            stepsSoFar
        else
            let offset = jumps.[index]
            let newIndex = index + offset
            jumps.[index] <-
                if jumps.[index] >= 3 then
                    jumps.[index] - 1
                else
                    jumps.[index] + 1
            stepsToExit (stepsSoFar + 1) newIndex
    
    let steps = stepsToExit 0 0

    printfn "Steps: %i" steps
