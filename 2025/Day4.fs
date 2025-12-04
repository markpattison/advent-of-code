module Day4

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day4.txt")

let sizeX = input.[0].Length
let sizeY = input.Length

let initialGrid =
    Array2D.init sizeX sizeY (fun x y -> input.[y].[x] = '@')

let canRemove (grid: bool[,]) x y =
    let mutable neighbours = 0
    if grid.[x, y] then
        if x > 0 && grid.[x - 1, y] then neighbours <- neighbours + 1
        if x > 0 && y > 0 && grid.[x - 1, y - 1] then neighbours <- neighbours + 1
        if x > 0 && y < sizeY - 1 && grid.[x - 1, y + 1] then neighbours <- neighbours + 1
        if y > 0 && grid.[x, y - 1] then neighbours <- neighbours + 1
        if y < sizeY - 1 && grid.[x, y + 1] then neighbours <- neighbours + 1
        if x < sizeX - 1 && y > 0 && grid.[x + 1, y - 1] then neighbours <- neighbours + 1
        if x < sizeX - 1 && grid.[x + 1, y] then neighbours <- neighbours + 1
        if x < sizeX - 1 && y < sizeY - 1 && grid.[x + 1, y + 1] then neighbours <- neighbours + 1
        neighbours < 4
    else
        false

let part1() =
    let mutable count = 0

    for x in 0 .. sizeX - 1 do
        for y in 0 .. sizeY - 1 do
            if canRemove initialGrid x y then
                count <- count + 1
    
    printfn "Accessible: %i" count

let part2() =
    let mutable count = 0
    let mutable finished = false
    let grid = Array2D.copy initialGrid

    // in this case it is safe to remove from the live grid

    while not finished do
        finished <- true

        for x in 0 .. sizeX - 1 do
            for y in 0 .. sizeY - 1 do
                if canRemove grid x y then
                    count <- count + 1
                    finished <- false
                    grid.[x, y] <- false
    
    printfn "Accessible: %i" count
