module Day13

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day13.txt")

type Terrain = Ash | Rock
type Mirror = Horizontal of int | Vertical of int

let parseTerrain c =
    match c with
    | '.' -> Ash
    | '#' -> Rock
    | _ -> failwith "unexpected"

let parsePattern yStart ySize =
    let xSize = input.[yStart].Length

    Array2D.init xSize ySize (fun x y -> parseTerrain input.[yStart + y].[x])

let patterns =
    let starts =
        [|
            yield 0
            for i in 0 .. input.Length - 1 do
                if input.[i].Length = 0 then yield (i + 1)
            yield input.Length + 1
        |]
    
    starts
    |> Array.pairwise
    |> Array.map (fun (st1, st2) -> parsePattern st1 (st2 - st1 - 1))

let isHorizontalMirror (pattern: Terrain[,]) afterY =
    let rowsBefore = afterY + 1
    let rowsAfter = Array2D.length2 pattern - afterY - 1
    let rowsToReflect = min rowsBefore rowsAfter

    let mutable notMirror = false
    let mutable finished = false
    let mutable x = 0
    let mutable y = 0

    while not (notMirror || finished) do
        if pattern.[x, afterY - y] <> pattern.[x, afterY + y + 1] then
            notMirror <- true
        else
            if x = Array2D.length1 pattern - 1 then
                if y = rowsToReflect - 1 then
                    finished <- true
                else
                    x <- 0
                    y <- y + 1
            else
                x <- x + 1

    not notMirror

let isVerticalMirror (pattern: Terrain[,]) afterX =
    let rowsBefore = afterX + 1
    let rowsAfter = Array2D.length1 pattern - afterX - 1
    let rowsToReflect = min rowsBefore rowsAfter

    let mutable notMirror = false
    let mutable finished = false
    let mutable y = 0
    let mutable x = 0

    while not (notMirror || finished) do
        if pattern.[afterX - x, y] <> pattern.[afterX + x + 1, y] then
            notMirror <- true
        else
            if y = Array2D.length2 pattern - 1 then
                if x = rowsToReflect - 1 then
                    finished <- true
                else
                    y <- 0
                    x <- x + 1
            else
                y <- y + 1

    not notMirror

let tryFindMirror ignoreMirror pattern =
    let vertical =
        [| 0 .. Array2D.length1 pattern - 2 |]
        |> Array.filter (fun x -> ignoreMirror <> Some (Vertical (x + 1)))
        |> Array.tryFind (fun x -> isVerticalMirror pattern x)
    
    match vertical with
    | Some x -> Vertical (x + 1) |> Some
    | None ->
        let horizontal =
            [| 0 .. Array2D.length2 pattern - 2 |]
            |> Array.filter (fun y -> ignoreMirror <> Some (Horizontal (y + 1)))
            |> Array.tryFind (fun y -> isHorizontalMirror pattern y)
        
        match horizontal with
        | Some y -> Horizontal (y + 1) |> Some
        | None -> None

let findMirror pattern =
    match tryFindMirror None pattern with
    | Some mirror -> mirror
    | None -> failwith "no mirror found"

let scoreMirror mirror =
    match mirror with
    | Horizontal y -> 100 * y
    | Vertical x -> x

let part1() =
    let total =
        patterns
        |> Array.map findMirror
        |> Array.map scoreMirror
        |> Array.sum

    printfn "Total: %i" total

let findNewMirror pattern =
    let originalMirror = findMirror pattern

    let mutable newMirror = None
    let mutable finished = false
    let mutable x = 0
    let mutable y = 0

    while not finished do
        //smudge
        pattern.[x, y] <- if pattern.[x, y] = Ash then Rock else Ash

        newMirror <- tryFindMirror (Some originalMirror) pattern

        // de-smudge
        pattern.[x, y] <- if pattern.[x, y] = Ash then Rock else Ash

        match newMirror with
            | Some _ -> finished <- true
            | None ->
                if x = Array2D.length1 pattern - 1 then
                    if y = Array2D.length2 pattern - 1 then
                        finished <- true
                    else
                        x <- 0
                        y <- y + 1
                else
                    x <- x + 1
    
    match newMirror with
    | Some mirror -> mirror
    | None -> failwith "no mirror found"

let part2() =
    let total =
        patterns
        |> Array.map findNewMirror
        |> Array.map scoreMirror
        |> Array.sum

    printfn "Total: %i" total
