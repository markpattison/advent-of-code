module Day8

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day8.txt")

let sizeX = 50
let sizeY = 6

type Instruction =
    | Rect of int * int
    | RotRow of int * int
    | RotCol of int * int

let parseLine (s: string) =
    if s.StartsWith("rect") then
        let parts = s.Substring(5).Split("x") |> Array.map Int32.Parse
        Rect (parts.[0], parts.[1])
    elif s.StartsWith("rotate row") then
        let parts = s.Substring(13).Split(" by ") |> Array.map Int32.Parse
        RotRow (parts.[0], parts.[1])
    elif s.StartsWith("rotate column") then
        let parts = s.Substring(16).Split(" by ") |> Array.map Int32.Parse
        RotCol (parts.[0], parts.[1])
    else
        failwith "unexpected"

let allInstructions =
    input
    |> Array.map parseLine

let apply (screen: bool[,]) instruction =
    match instruction with
    | Rect (a, b) ->
        Array2D.init sizeX sizeY (fun x y ->
            (x < a && y < b) || screen.[x, y])
    | RotRow (a, b) ->
        Array2D.init sizeX sizeY (fun x y ->
            if a = y then
                screen.[(x + sizeX - b) % sizeX, y]
            else
                screen.[x, y])
    | RotCol (a, b) ->
        Array2D.init sizeX sizeY (fun x y ->
            if a = x then
                screen.[x, (y + sizeY - b) % sizeY]
            else
                screen.[x, y])

let startScreen = Array2D.zeroCreate sizeX sizeY

let finalScreen =
    Array.fold apply startScreen allInstructions

let part1() =

    
    let mutable litPixels = 0

    for x in 0 .. sizeX - 1 do
        for y in 0 .. sizeY - 1 do
            if finalScreen.[x, y] then litPixels <- litPixels + 1
    
    printfn "Lit pixels: %i" litPixels

let part2() =
    for y in 0 .. sizeY - 1 do
        for x in 0 .. sizeX - 1 do
            printf "%c" (if finalScreen.[x, y] then '#' else '.')
        printfn ""
