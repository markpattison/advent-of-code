module Day10

open System
open System.IO

let input = File.ReadAllLines(@"input\day10.txt")

let sizeX = input.[0].Length
let sizeY = input.Length

let heights =
    Array2D.init sizeX sizeY (fun x y -> input.[y].[x].ToString() |> Int32.Parse)

let zeroes =
    seq {
        for x in 0 .. sizeX - 1 do
            for y in 0 .. sizeY - 1 do
                if heights.[x, y] = 0 then
                    (x, y)
    } |> Seq.toArray

let scoreTrailHead (thx, thy) =
    let reachable = Array2D.create sizeX sizeY false

    let rec markTrail x y height =
        reachable.[x, y] <- true
        if height < 9 then
            let nextHeight = height + 1
            if x > 0 && heights.[x - 1, y] = nextHeight then markTrail (x - 1) y nextHeight
            if x < sizeX - 1 && heights.[x + 1, y] = nextHeight then markTrail (x + 1) y nextHeight
            if y > 0 && heights.[x, y - 1] = nextHeight then markTrail x (y - 1) nextHeight
            if y < sizeY - 1 && heights.[x, y + 1] = nextHeight then markTrail x (y + 1) nextHeight
    
    markTrail thx thy 0

    let mutable count = 0
    for x in 0 .. sizeX - 1 do
        for y in 0 .. sizeY - 1 do
            if reachable.[x, y] && heights.[x, y] = 9 then
                count <- count + 1
    count

let part1() =
    let total =
        zeroes
        |> Array.sumBy scoreTrailHead
    
    printfn "Total: %i" total

let scoreTrailHead2 (thx, thy) =
    let rec scoreTrail x y height =
        if height = 9 then
            1
        else
            let nextHeight = height + 1
            let mutable score = 0
            if x > 0 && heights.[x - 1, y] = nextHeight then score <- score + scoreTrail (x - 1) y nextHeight
            if x < sizeX - 1 && heights.[x + 1, y] = nextHeight then score <- score + scoreTrail (x + 1) y nextHeight
            if y > 0 && heights.[x, y - 1] = nextHeight then score <- score + scoreTrail x (y - 1) nextHeight
            if y < sizeY - 1 && heights.[x, y + 1] = nextHeight then score <- score + scoreTrail x (y + 1) nextHeight
            score
    
    scoreTrail thx thy 0

let part2() =
    let total =
        zeroes
        |> Array.sumBy scoreTrailHead2
    
    printfn "Total: %i" total
