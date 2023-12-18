module Day18

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day18.txt")

type Dir = N | E | S | W

type Dig =
    {
        Dir: Dir
        Length: int
        Colour: string
    }

let parseDir (s: string) =
    match s with
    | "U" -> N
    | "R" -> E
    | "D" -> S
    | "L" -> W
    | _ -> failwith "unexpected"

let parseLine (s: string) =
    let splits = s.Split(' ')
    let dir = parseDir splits.[0]
    let length = Int32.Parse(splits.[1])
    let colour = splits.[2].Substring(2, 6)

    { Dir = dir; Length = length; Colour = colour }

let digs = input |> Array.map parseLine

let dxy dir =
    match dir with
    | N -> 0, -1
    | E -> 1, 0
    | S -> 0, 1
    | W -> -1, 0

let toPoints (steps: Dig[]) =
    let mutable x = 0
    let mutable y = 0

    [|
        for i in 0 .. steps.Length - 1 do
            yield (x, y)
            let dig = steps.[i]
            let dx, dy = dxy dig.Dir
            x <- x + dx * dig.Length
            y <- y + dy * dig.Length
    |]

let findArea (points: (int * int)[]) =
    let mutable total = 0L
    let mutable boundary = 0L

    for i in 0 .. points.Length - 1 do
        let px, py = points.[i]
        let pnx, pny = points.[(i + 1) % points.Length]
        total <- total + int64 (py + pny) * int64 (px - pnx)
        boundary <- boundary + int64 (abs (pny - py)) + int64 (abs (pnx - px))
    
    // 1. area inside
    // 2. half a square for each boundary space on average (inside and outside corners mostly cancel out)
    // 3. one extra square for four extra outside corners

    abs (total / 2L) + (boundary / 2L) + 1L

let part1() =
    let area =
        digs
        |> toPoints
        |> findArea

    printfn "Area: %i" area

let transform dig =
    let length = Int32.Parse(dig.Colour.Substring(0, 5), System.Globalization.NumberStyles.HexNumber)
    let dir =
        match dig.Colour.Substring(5, 1) with
        | "0" -> E
        | "1" -> S
        | "2" -> W
        | "3" -> N
        | _ -> failwith "unexpected"
    {
        Dir = dir
        Length = length
        Colour = dig.Colour
    }

let part2() =
    let area =
        digs
        |> Array.map transform
        |> toPoints
        |> findArea

    printfn "Area: %i" area
