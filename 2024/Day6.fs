module Day6

open System
open System.IO

let input = File.ReadAllLines(@"input\day6.txt")

let xSize = input.[0].Length
let ySize = input.Length

let obstructions = Array2D.init xSize ySize (fun x y -> input.[y].[x] = '#')

let yGuard =
    input
    |> Array.findIndex (fun s -> s.Contains('^'))

let xGuard =
    input.[yGuard].ToCharArray()
    |> Array.findIndex (fun c -> c = '^')

type Direction = N | E | S | W

let turned dir =
    match dir with
    | N -> E
    | E -> S
    | S -> W
    | W -> N

let originalVisited =
    let visited = Array2D.create xSize ySize false
    let mutable x = xGuard
    let mutable y = yGuard
    let mutable dir = N
    let mutable finished = false

    while not finished do
        visited.[x, y] <- true
        let nextX, nextY =
            match dir with
            | N -> (x, y - 1)
            | E -> (x + 1, y)
            | S -> (x, y + 1)
            | W -> (x - 1, y)
        if nextX < 0 || nextX >= xSize || nextY < 0 || nextY >= ySize then
            finished <- true
        elif obstructions.[nextX, nextY] then
            dir <- turned dir
        else
            x <- nextX
            y <- nextY
    visited

let part1() =
    let mutable count = 0
    for xi in 0 .. xSize - 1 do
        for yi in 0 .. ySize - 1 do
            if originalVisited.[xi, yi] then count <- count + 1
    
    printfn "Visited: %i" count

let createsLoop xObs yObs =
    let newMap = Array2D.copy obstructions
    newMap.[xObs, yObs] <- true

    let visitedN = Array2D.create xSize ySize false
    let visitedE = Array2D.create xSize ySize false
    let visitedS = Array2D.create xSize ySize false
    let visitedW = Array2D.create xSize ySize false

    let mutable x = xGuard
    let mutable y = yGuard
    let mutable dir = N
    let mutable finished = false
    let mutable isLoop = false

    while not finished do
        match dir with
        | N -> visitedN.[x, y] <- true
        | E -> visitedE.[x, y] <- true
        | S -> visitedS.[x, y] <- true
        | W -> visitedW.[x, y] <- true

        let nextX, nextY =
            match dir with
            | N -> (x, y - 1)
            | E -> (x + 1, y)
            | S -> (x, y + 1)
            | W -> (x - 1, y)
        if nextX < 0 || nextX >= xSize || nextY < 0 || nextY >= ySize then
            finished <- true
        elif newMap.[nextX, nextY] then
            dir <- turned dir
        else
            x <- nextX
            y <- nextY
            let visitedNext =
                match dir with
                | N -> visitedN.[x, y]
                | E -> visitedE.[x, y]
                | S -> visitedS.[x, y]
                | W -> visitedW.[x, y]
            if visitedNext then
                finished <- true
                isLoop <- true
    isLoop

let part2() =
    let mutable count = 0
    for x in 0 .. xSize - 1 do
        for y in 0 .. ySize - 1 do
            if originalVisited.[x, y] && (x <> xGuard || y <> yGuard) then
                if createsLoop x y then
                    count <- count + 1
    
    printfn "Options: %i" count
