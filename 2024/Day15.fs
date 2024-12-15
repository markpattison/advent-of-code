module Day15

open System
open System.IO

let input = File.ReadAllLines(@"input\day15.txt")

let sizeX = input.[0].Length
let sizeY = Array.findIndex (fun s -> s = "") input

let walls, initialBoxes, startX, startY  =
    let w = Array2D.create sizeX sizeY false
    let b = Array2D.create sizeX sizeY false
    let mutable lfx = 0
    let mutable lfy = 0
    for x in 0 .. sizeX - 1 do
        for y in 0 .. sizeY - 1 do
            match input.[y].[x] with
            | '#' -> w.[x, y] <- true
            | 'O' -> b.[x, y] <- true
            | '.' -> ()
            | '@' ->
                lfx <- x
                lfy <- y
            | _ -> failwith "unexpected"
    w, b, lfx, lfy

type Dir = N | E | S | W

let parseDir c =
    match c with
    | '^' -> N
    | '>' -> E
    | 'v' -> S
    | '<' -> W
    | _ -> failwith "unexpected"

let moves =
    input
    |> Array.skip (sizeY + 1)
    |> Array.collect (fun s ->
        s.ToCharArray() |> Array.map parseDir)

let score (boxes: bool[,]) =
    let mutable s = 0
    for x in 0 .. (Array2D.length1 boxes) - 1 do
        for y in 0 .. (Array2D.length2 boxes) - 1 do
            if boxes.[x, y] then s <- s + x + 100 * y
    s

let dxy dir =
    match dir with
    | N -> 0, -1 | E -> 1, 0 | S -> 0, 1 | W -> -1, 0

let part1() =
    let boxes = Array2D.copy initialBoxes
    let mutable lfx = startX
    let mutable lfy = startY

    let update move =
        let dx, dy = dxy move
        let mutable boxCount = 0
        while boxes[lfx + (boxCount + 1) * dx, lfy + (boxCount + 1) * dy] do
            boxCount <- boxCount + 1
        if not walls.[lfx + (boxCount + 1) * dx, lfy + (boxCount + 1) * dy] then
            if boxCount > 0 then
                boxes.[lfx + (boxCount + 1) * dx, lfy + (boxCount + 1) * dy] <- true
                boxes.[lfx + dx, lfy + dy] <- false
            lfx <- lfx + dx
            lfy <- lfy + dy
    
    moves |> Array.iter update

    let gps = score boxes

    printfn "GPS: %i" gps

let sizeX2 = sizeX * 2

let walls2, leftBoxes2 =
    let w = Array2D.create sizeX2 sizeY false
    let b = Array2D.create sizeX2 sizeY false
    for x in 0 .. sizeX - 1 do
        for y in 0 .. sizeY - 1 do
            if walls.[x, y] then
                w.[x * 2, y] <- true
                w.[x * 2 + 1, y] <- true
            if initialBoxes.[x, y] then
                b.[x * 2, y] <- true
    w, b

let startX2 = startX * 2
let startY2 = startY

let part2() =
    let boxes = Array2D.copy leftBoxes2
    let mutable lfx = startX2
    let mutable lfy = startY

    let update move =
        let dx, dy = dxy move
        if boxes.[lfx + dx, lfy + dy] || boxes.[lfx + dx - 1, lfy + dy] then
            let mutable finished = false
            let mutable movingBoxes : (int * int) list =
                if boxes.[lfx + dx, lfy + dy] then [ (lfx + dx, lfy + dy) ] else [ (lfx + dx - 1, lfy + dy) ]
            let mutable foundMoreBoxes = false
            let addIfNew (bx, by) =
                if not (movingBoxes |> List.contains (bx, by)) then
                    movingBoxes <- (bx, by) :: movingBoxes
                    foundMoreBoxes <- true
            while not finished do
                foundMoreBoxes <- false
                movingBoxes |> List.iter (fun (bx, by) ->
                    if boxes.[bx + dx - 1, by + dy] then addIfNew (bx + dx - 1, by + dy)
                    if boxes.[bx + dx, by + dy] then addIfNew (bx + dx, by + dy)
                    if boxes.[bx + dx + 1, by + dy] then addIfNew (bx + dx + 1, by + dy))
                if not foundMoreBoxes then finished <- true
            let canMoveBoxes =
                movingBoxes
                |> List.forall (fun (bx, by) -> walls2.[bx + dx, by + dy] = false && walls2.[bx + dx + 1, by + dy] = false)
            if canMoveBoxes then
                lfx <- lfx + dx
                lfy <- lfy + dy
                movingBoxes
                |> List.iter (fun (bx, by) ->
                    boxes.[bx, by] <- false
                    boxes.[bx + dx, by + dy] <- true)
        
        elif not walls2.[lfx + dx, lfy + dy] then
            lfx <- lfx + dx
            lfy <- lfy + dy

    moves |> Array.iter update

    let gps = score boxes

    printfn "GPS: %i" gps
