module Day2

open System
open System.IO
open Microsoft.FSharp.Reflection

let input =
    File.ReadAllLines(@"input\day2.txt")

type Direction = U | R | D | L

let fromString<'a> (s:string) =
    match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
    |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
    |_ -> None

let allInstructions =
    input
    |> Array.map (fun s -> s.ToCharArray() |> Array.choose (fun c -> c.ToString() |> fromString<Direction>))

let move (x, y) dir =
    match dir with
    | U -> x, max 0 (y - 1)
    | R -> min 2 (x + 1), y
    | D -> x, min 2 (y + 1)
    | L -> max 0 (x - 1), y

let moveAll (x, y) dirs =
    Array.fold move (x, y) dirs

let key (x, y) = 1 + y * 3 + x

let part1() =
    let keys =
        allInstructions
        |> Array.scan moveAll (1, 1)
        |> Array.skip 1
        |> Array.map key
    
    let code = keys |> Array.map _.ToString() |> String.concat("")

    printfn "Code: %s" code

let move2 (x, y) dir =
    match dir with
    | U -> x, max (abs (x - 2)) (y - 1)
    | R -> min (4 - abs (y - 2)) (x + 1), y
    | D -> x, min (4 - abs (x - 2)) (y + 1)
    | L -> max (abs (y - 2)) (x - 1), y

let moveAll2 (x, y) dirs =
    Array.fold move2 (x, y) dirs

let key2 (x, y) =
    match x, y with
    | 2, 0 -> "1"
    | 1, 1 -> "2"
    | 2, 1 -> "3"
    | 3, 1 -> "4"
    | 0, 2 -> "5"
    | 1, 2 -> "6"
    | 2, 2 -> "7"
    | 3, 2 -> "8"
    | 4, 2 -> "9"
    | 1, 3 -> "A"
    | 2, 3 -> "B"
    | 3, 3 -> "C"
    | 2, 4 -> "D"
    | _ -> failwith "unexpected"

let part2() =
    let keys =
        allInstructions
        |> Array.scan moveAll2 (0, 2)
        |> Array.skip 1
        |> Array.map key2
    
    let code = keys |> String.concat("")

    printfn "Code: %s" code
