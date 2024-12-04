module Day4

open System
open System.IO

let input = File.ReadAllLines(@"input\day4.txt")

let sizeX = input.[0].Length
let sizeY = input.Length

let letters =
    Array2D.init sizeX sizeY (fun x y ->
        input.[y].[x])

let part1() =
    let mutable count = 0
    for x in 0 .. (sizeX - 1) do
        for y in 0 .. (sizeY - 1) do
            if letters.[x, y] = 'X' then
                if x <= sizeX - 4 && letters.[x + 1, y] = 'M' && letters.[x + 2, y] = 'A' && letters.[x + 3, y] = 'S' then count <- count + 1
                if y <= sizeY - 4 && letters.[x, y + 1] = 'M' && letters.[x, y + 2] = 'A' && letters.[x, y + 3] = 'S' then count <- count + 1
                if x <= sizeX - 4 && y <= sizeY - 4 && letters.[x + 1, y + 1] = 'M' && letters.[x + 2, y + 2] = 'A' && letters.[x + 3, y + 3] = 'S' then count <- count + 1
                if x >= 3 && y <= sizeY - 4 && letters.[x - 1, y + 1] = 'M' && letters.[x - 2, y + 2] = 'A' && letters.[x - 3, y + 3] = 'S' then count <- count + 1
            if letters.[x, y] = 'S' then
                if x <= sizeX - 4 && letters.[x + 1, y] = 'A' && letters.[x + 2, y] = 'M' && letters.[x + 3, y] = 'X' then count <- count + 1
                if y <= sizeY - 4 && letters.[x, y + 1] = 'A' && letters.[x, y + 2] = 'M' && letters.[x, y + 3] = 'X' then count <- count + 1
                if x <= sizeX - 4 && y <= sizeY - 4 && letters.[x + 1, y + 1] = 'A' && letters.[x + 2, y + 2] = 'M' && letters.[x + 3, y + 3] = 'X' then count <- count + 1
                if x >= 3 && y <= sizeY - 4 && letters.[x - 1, y + 1] = 'A' && letters.[x - 2, y + 2] = 'M' && letters.[x - 3, y + 3] = 'X' then count <- count + 1
    
    printfn "Count %i" count

let part2() =
    let mutable count = 0
    for x in 1 .. (sizeX - 2) do
        for y in 1 .. (sizeY - 2) do
            if letters.[x, y] = 'A' then
                if ((letters.[x - 1, y - 1] = 'M' && letters.[x + 1, y + 1] = 'S') || (letters.[x - 1, y - 1] = 'S' && letters.[x + 1, y + 1] = 'M'))
                    && ((letters.[x + 1, y - 1] = 'M' && letters.[x - 1, y + 1] = 'S') || (letters.[x + 1, y - 1] = 'S' && letters.[x - 1, y + 1] = 'M'))
                then
                    count <- count + 1
    
    printfn "Count %i" count
