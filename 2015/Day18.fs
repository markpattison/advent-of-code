module Day18

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day18.txt")

let sizeX = input.[0].Length
let sizeY = input.Length

let initialLights =
    Array2D.init sizeX sizeY (fun x y -> input.[y].[x] = '#')

let isOnNext (state: bool[,]) x y =
    let mutable neighboursOn = 0
    for ix in max 0 (x - 1) .. min (sizeX - 1) (x + 1) do
        for iy in max 0 (y - 1) .. min (sizeY - 1) (y + 1) do
            if state.[ix, iy] && (x <> ix || y <> iy) then
                neighboursOn <- neighboursOn + 1
    match state.[x, y], neighboursOn with
    | true, 2 | true, 3 | false, 3 -> true
    | _ -> false

let update state =
    Array2D.init sizeX sizeY (isOnNext state)

let rec updateN state n =
    match n with
    | x when x <= 0 -> state
    | x -> updateN (update state) (x - 1)

let part1() =
    let result = updateN initialLights 100

    let mutable lightsOn = 0
    for x in 0 .. sizeX - 1 do
        for y in 0 .. sizeY - 1 do
            if result.[x, y] then lightsOn <- lightsOn + 1
    
    printfn "Lights on: %i" lightsOn

let turnOnCorners (state: bool[,]) =
    state.[0, 0] <- true
    state.[sizeX - 1, 0] <- true
    state.[0, sizeY - 1] <- true
    state.[sizeX - 1, sizeY - 1] <- true
    state

let rec update2N state n =
    match n with
    | x when x <= 0 -> state
    | x -> update2N (state |> update |> turnOnCorners) (x - 1)

let part2() =
    let result = update2N (turnOnCorners initialLights) 100

    let mutable lightsOn = 0
    for x in 0 .. sizeX - 1 do
        for y in 0 .. sizeY - 1 do
            if result.[x, y] then lightsOn <- lightsOn + 1
    
    printfn "Lights on: %i" lightsOn
