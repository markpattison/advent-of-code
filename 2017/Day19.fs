module Day19

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day19.txt")

type Dir = Up | Right | Down | Left

type Block =
    | Empty
    | UpDown
    | LeftRight
    | Cross
    | Letter of char

let sizeX = input.[0].Length
let sizeY = input.Length

let parseChar c =
    match c with
    | ' ' -> Empty
    | '|' -> UpDown
    | '-' -> LeftRight
    | '+' -> Cross
    | letter -> Letter letter

let grid =
    Array2D.init sizeX sizeY (fun x y -> input.[y].[x] |> parseChar)

let startX =
    grid.[*, 0] |> Array.findIndex (fun b -> b = UpDown)
let startY = 0
let startDir = Down

let Dxy dir =
    match dir with
    | Up -> 0, -1
    | Right -> 1, 0
    | Down -> 0, 1
    | Left -> -1, 0

let okLeftRight c =
    match c with
    | LeftRight | Letter _ -> true
    | _ -> false

let okUpDown c =
    match c with
    | UpDown | Letter _ -> true
    | _ -> false

let rec findLetters acc steps x y dir =
    let dx, dy = Dxy dir
    let block = grid.[x, y]
    match block with
    | Empty -> acc, steps
    | Letter l -> findLetters (acc + string l) (steps + 1) (x + dx) (y + dy) dir
    | UpDown | LeftRight -> findLetters acc (steps + 1) (x + dx) (y + dy) dir
    | Cross ->
        match dir with
        | Up | Down ->
            let leftPossible = x > 0 && grid.[x - 1, y] |> okLeftRight
            let rightPossible = x < sizeX - 1 && grid.[x + 1, y] |> okLeftRight
            match leftPossible, rightPossible with
            | true, false -> findLetters acc (steps + 1) (x - 1) y Left
            | false, true -> findLetters acc (steps + 1) (x + 1) y Right
            | _ -> failwith "unexpected"
        | Left | Right ->
            let upPossible = y > 0 && grid.[x, y - 1] |> okUpDown
            let downPossible = y < sizeY - 1 && grid.[x, y + 1] |> okUpDown
            match upPossible, downPossible with
            | true, false -> findLetters acc (steps + 1) x (y - 1) Up
            | false, true -> findLetters acc (steps + 1) x (y + 1) Down
            | _ -> failwith "unexpected"

let part1() =
    let letters, _ = findLetters "" 0 startX startY startDir

    printfn "Letters: %s" letters

let part2() =
    let _, steps = findLetters "" 0 startX startY startDir

    printfn "Steps: %i" steps
