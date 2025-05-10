module Day17

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day17.txt").[0]

let md5 = System.Security.Cryptography.MD5.Create()

let hash (s: string) =
    let bytes = System.Text.Encoding.ASCII.GetBytes(s);
    let hash = md5.ComputeHash(bytes)
    Convert.ToHexString(hash).ToLower()

type State =
    {
        X: int
        Y: int
        Path: string
    }

type Dir = U | R | D | L

let dirToString dir =
    match dir with
    | U -> "U"
    | R -> "R"
    | D -> "D"
    | L -> "L"

let initialState =
    {
        X = 0
        Y = 0
        Path = ""
    }

let isOpen c =
    match c with
    | 'b' | 'c' | 'd' | 'e' | 'f' -> true
    | _ -> false

let newState state dir =
    let newX, newY =
        match dir with
        | U -> state.X, state.Y - 1
        | R -> state.X + 1, state.Y
        | D -> state.X, state.Y + 1
        | L -> state.X - 1, state.Y
    {
        X = newX
        Y = newY
        Path = state.Path + dirToString dir
    }

let availableMoves passcode state =
    let h = hash (passcode + state.Path)
    let hashChars = h.ToCharArray()
    seq
        {
            if state.Y > 0 && isOpen hashChars.[0] then U
            if state.Y < 3 && isOpen hashChars.[1] then D
            if state.X > 0 && isOpen hashChars.[2] then L
            if state.X < 3 && isOpen hashChars.[3] then R
        }
    |> Seq.map (newState state)
    |> Seq.toList

let isFinished state =
    state.X = 3 && state.Y = 3

let rec shortestPath states =
    match states |> List.tryFind isFinished with
    | Some f -> f.Path
    | None ->
        states |> List.collect (availableMoves input) |> shortestPath

let part1() =
    let path = shortestPath [ initialState ]

    printfn "Path: %s" path

let part2() =
    let mutable pathLength = 0
    let mutable longest = 0
    let mutable states = [ initialState ]

    while states.Length > 0 do
        let finishedPaths, unfinishedPaths = states |> List.partition isFinished
        if not (List.isEmpty finishedPaths) then
            longest <- pathLength
        states <- unfinishedPaths |> List.collect (availableMoves input)
        pathLength <- pathLength + 1

    printfn "Longest: %i" longest
