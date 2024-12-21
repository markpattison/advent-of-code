module Day21

open System
open System.IO

let input = File.ReadAllLines(@"input\day21.txt")

type NumericButton = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | NA
type DirectionButton = Up | Down | Left | Right | DirA

let parseLine (s: string) =
    let numButtons =
        s.ToCharArray()
        |> Array.map (fun c ->
            match c with
            | 'A' -> NA | '0' -> N0 | '1' -> N1 | '2' -> N2 | '3' -> N3 | '4' -> N4 | '5' -> N5
            | '6' -> N6 | '7' -> N7 | '8' -> N8 | '9' -> N9 | _ -> failwith "unexpected")
        |> Array.toList
    let numericPart = s.Substring(0, 3) |> Int32.Parse
    numericPart, numButtons

let targetCodes =
    input
    |> Array.map parseLine

let numPos = function
    | N0 -> 1, 3
    | N1 -> 0, 2
    | N2 -> 1, 2
    | N3 -> 2, 2
    | N4 -> 0, 1
    | N5 -> 1, 1
    | N6 -> 2, 1
    | N7 -> 0, 0
    | N8 -> 1, 0
    | N9 -> 2, 0
    | NA -> 2, 3

let dirPos = function
    | Up -> 1, 0
    | Down -> 1, 1
    | Left -> 0, 1
    | Right -> 2, 1
    | DirA -> 2, 0

let movesForButtons fDir (sx, sy) (gx, gy) buttons =
    let isValid (x, y) dbs =
        dbs
        |> List.scan (fun (tx, ty) db -> match db with Left -> (tx - 1, ty) | Right -> (tx + 1, ty) | Up -> (tx, ty - 1) | Down -> (tx, ty + 1) | _ -> (tx, ty)) (x, y)
        |> List.contains (gx, gy)
        |> not

    let rec moves (x, y) rem =
        match rem with
        | [] -> [[]]
        | next :: xs ->
            let nextX, nextY = fDir next
            let nextMoves = moves (nextX, nextY) xs
            let thisMoves =
                seq {
                    match (nextX - x, nextY - y) with
                    | (0, 0) ->
                        []
                    | (dx, 0) when dx < 0 ->
                        List.replicate -dx Left
                    | (dx, 0) when dx > 0 ->
                        List.replicate dx Right
                    | (0, dy) when dy < 0 ->
                        List.replicate -dy Up
                    | (0, dy) when dy > 0 ->
                        List.replicate dy Down
                    | (dx, dy) when dx < 0 && dy < 0 ->
                        List.replicate -dx Left @ List.replicate -dy Up
                        List.replicate -dy Up @ List.replicate -dx Left
                    | (dx, dy) when dx < 0 && dy > 0 ->
                        List.replicate -dx Left @ List.replicate dy Down
                        List.replicate dy Down @ List.replicate -dx Left
                    | (dx, dy) when dx > 0 && dy < 0 ->
                        List.replicate dx Right @ List.replicate -dy Up
                        List.replicate -dy Up @ List.replicate dx Right
                    | (dx, dy) when dx > 0 && dy > 0 ->
                        List.replicate dx Right @ List.replicate dy Down
                        List.replicate dy Down @ List.replicate dx Right
                    | _ -> failwith "unexpected"
                } |> Seq.toList |> List.filter (isValid (x, y))
            
            List.allPairs thisMoves nextMoves
            |> List.map (fun (tm, nm)-> tm @ [ DirA ] @ nm)

    moves (sx, sy) buttons

let movesForDir = movesForButtons dirPos (2, 0) (0, 0)
let movesForNum = movesForButtons numPos (2, 3) (0, 3)

let mutable cache : Map<int * DirectionButton list, int64> = Map.empty

let rec minMovesForDirButtons depth buttons =
    if List.isEmpty buttons then
        0L
    elif depth = 0 then
        List.length buttons |> int64
    else
        let firstA = List.findIndex (fun b -> b = DirA) buttons
        let chunk, rem = List.splitAt (firstA + 1) buttons
        let chunkMoves =
            match Map.tryFind (depth, chunk) cache with
            | Some minMoves -> minMoves
            | None ->
                let possibleMoves = movesForDir chunk
                let minMoves = possibleMoves |> List.map (minMovesForDirButtons (depth - 1)) |> List.min |> int64
                cache <- Map.add (depth, chunk) minMoves cache
                minMoves
        chunkMoves + minMovesForDirButtons depth rem

let totalComplexityForDepth depth =
    let complexity (numericPart, numButtons) =
        let minMoves = numButtons |> movesForNum |> List.map (minMovesForDirButtons depth) |> List.min
        minMoves * int64 numericPart

    targetCodes |> Array.sumBy complexity

let part1() =
    let totalComplexity = totalComplexityForDepth 2
    printfn "Total complexity: %i" totalComplexity

let part2() =
    let totalComplexity = totalComplexityForDepth 25
    printfn "Total complexity: %i" totalComplexity
