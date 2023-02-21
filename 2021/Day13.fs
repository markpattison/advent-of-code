module Day13

open System.IO

let lines = File.ReadAllLines(@"input\day13.txt")

let test =
    [|
        "6,10"
        "0,14"
        "9,10"
        "0,3"
        "10,4"
        "4,11"
        "6,0"
        "6,12"
        "4,1"
        "0,13"
        "10,12"
        "3,4"
        "3,0"
        "8,4"
        "1,10"
        "2,14"
        "8,10"
        "9,0"
        ""
        "fold along y=7"
        "fold along x=5"
    |]

type Fold = FoldAlongX of int | FoldAlongY of int

type Instruction = Point of int * int | Fold of Fold

let parseLine line =
    match line with
    | "" -> None
    | s when s.StartsWith("fold along x=") -> s.Substring(13, s.Length - 13) |> System.Int32.Parse |> FoldAlongX |> Fold |> Some
    | s when s.StartsWith("fold along y=") -> s.Substring(13, s.Length - 13) |> System.Int32.Parse |> FoldAlongY |> Fold |> Some
    | s ->
        let values = s.Split(',') |> Array.map System.Int32.Parse
        (values.[0], values.[1]) |> Point |> Some

let instructions =
    lines
    |> Array.choose parseLine

let points = instructions |> Array.choose (fun i -> match i with Point (x, y) -> Some (x, y) | _ -> None)
let folds = instructions |> Array.choose (fun i -> match i with Fold f -> Some f | _ -> None)
    
let maxX = points |> Array.map fst |> Array.max
let maxY = points |> Array.map snd |> Array.max

let startGrid =
    let grid = Array2D.create (1 + maxX) (1 + maxY) false
    points |> Array.iter (fun (x, y) -> grid.[x, y] <- true)
    grid

let foldX grid foldAt =
    let oldSizeX = Array2D.length1 grid
    let sizeY = Array2D.length2 grid

    let sizeLeft = foldAt
    let sizeRight = oldSizeX - foldAt - 1

    let newSizeX = max sizeLeft sizeRight
    let newGrid = Array2D.create newSizeX sizeY false

    let leftOffset, rightOffset =
        if sizeLeft >= sizeRight then
            0, 2 * foldAt
        else
            sizeRight - sizeLeft, oldSizeX - 1

    for x in 0 .. (foldAt - 1) do
        for y in 0 .. (sizeY - 1) do
            newGrid.[leftOffset + x, y] <- grid.[x, y]

    for x in (foldAt + 1) .. (oldSizeX - 1) do
        for y in 0 .. (sizeY - 1) do
            newGrid.[rightOffset - x, y] <- newGrid.[rightOffset - x, y] || grid.[x, y]

    newGrid

let foldY grid foldAt =
    let sizeX = Array2D.length1 grid
    let oldSizeY = Array2D.length2 grid

    let sizeTop = foldAt
    let sizeBottom = oldSizeY - foldAt - 1

    let newSizeY = max sizeTop sizeBottom
    let newGrid = Array2D.create sizeX newSizeY false

    let topOffset, bottomOffset =
        if sizeTop >= sizeBottom then
            0, 2 * foldAt
        else
            sizeBottom - sizeTop, oldSizeY - 1

    for x in 0 .. (sizeX - 1) do
        for y in 0 .. (foldAt - 1) do
            newGrid.[x, topOffset + y] <- grid.[x, y]

    for x in 0 .. (sizeX - 1) do
        for y in (foldAt + 1) .. (oldSizeY - 1) do
            newGrid.[x, bottomOffset - y] <- newGrid.[x, bottomOffset - y] || grid.[x, y]

    newGrid

let folder grid fold =
    match fold with
    | FoldAlongX foldAt -> foldX grid foldAt
    | FoldAlongY foldAt -> foldY grid foldAt

let part1() =

    let gridAfterOneFold =
        folds
        |> Array.take 1
        |> Array.fold folder startGrid

    let mutable dots = 0

    gridAfterOneFold |> Array2D.iter (fun b -> if b then dots <- dots + 1)

    printfn "Number of dots after 1 fold: %i" dots

let part2() =
    
    let finalGrid =
        folds
        |> Array.fold folder startGrid

    for y in 0 .. (Array2D.length2 finalGrid - 1) do
        printfn ""
        for x in 0 .. (Array2D.length1 finalGrid - 1) do
            if finalGrid.[x, y] then printf "#" else printf "."
    
    ()
