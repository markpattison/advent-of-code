module Day7

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day7.txt")

let sizeX = input.[0].Length
let sizeY = input.Length

let startX = input.[0].ToCharArray() |> Array.findIndex (fun c -> c = 'S')

let isSplitter =
    Array2D.init sizeX sizeY (fun x y -> input.[y].[x] = '^')

let part1() =
    let hasBeam = Array2D.create sizeX sizeY false
    hasBeam.[startX, 0] <- true

    let mutable splits = 0

    for y in 0 .. sizeY - 2 do
        for x in 0 .. sizeX - 1 do
            if hasBeam.[x, y] then
                if isSplitter.[x, y] then
                    splits <- splits + 1
                    if x > 0 then hasBeam.[x - 1, y + 1] <- true
                    if x < sizeX - 1 then hasBeam.[x + 1, y + 1] <- true
                else
                    hasBeam.[x, y + 1] <- true
    
    printfn "Splits: %i" splits

let part2() =
    let numBeams = Array2D.create sizeX sizeY 0L
    numBeams.[startX, 0] <- 1L

    for y in 0 .. sizeY - 2 do
        for x in 0 .. sizeX - 1 do
            if isSplitter.[x, y] then
                if x > 0 then numBeams.[x - 1, y + 1] <- numBeams.[x - 1, y + 1] + numBeams.[x, y]
                if x < sizeX - 1 then numBeams.[x + 1, y + 1] <- numBeams.[x + 1, y + 1] + numBeams.[x, y]
            else
                numBeams.[x, y + 1] <- numBeams.[x, y + 1] + numBeams.[x, y]

    let worlds = numBeams.[*, sizeY - 1] |> Array.sum
    
    printfn "Worlds: %i" worlds
