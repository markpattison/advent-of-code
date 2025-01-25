module Day20

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day20.txt")

let targetPresents = input.[0] |> Int32.Parse

let part1() =
    let maxHouse = 1000000

    let presents = Array.zeroCreate (1 + maxHouse)

    for elf in 1 .. maxHouse do
        for house in elf .. elf .. maxHouse do
            presents.[house] <- presents.[house] + (10 * elf)
    
    let lowest = Array.findIndex (fun p -> p >= targetPresents) presents
    
    printfn "Lowest house: %i" lowest

let part2() =
    let maxHouse = 1000000

    let presents = Array.zeroCreate (1 + maxHouse)

    for elf in 1 .. maxHouse do
        for house in elf .. elf .. (min maxHouse (elf * 50)) do
            presents.[house] <- presents.[house] + (11 * elf)
    
    let lowest = Array.findIndex (fun p -> p >= targetPresents) presents
    
    printfn "Lowest house: %i" lowest
