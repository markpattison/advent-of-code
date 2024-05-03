module Day3

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day3.txt")

type Dir = N | E | S | W

let parseChar c =
    match c with
    | '^' -> N
    | '>' -> E
    | 'v' -> S
    | '<' -> W
    | _ -> failwith "unexpected"

let allDirs = input.[0].ToCharArray() |> Array.map parseChar

type State =
    {
        Presents: Map<int * int, int>
        X: int
        Y: int
    }

let initialState =
    {
        Presents = [ ((0, 0), 1) ] |> Map.ofList
        X = 0
        Y = 0
    }

let update state dir =
    let newX, newY =
        match dir with
        | N -> state.X, state.Y + 1
        | E -> state.X + 1, state.Y
        | S -> state.X, state.Y - 1
        | W -> state.X - 1, state.Y
    
    let newPresents =
        match Map.tryFind (newX, newY) state.Presents with
        | None ->
            Map.add (newX, newY) 1 state.Presents
        | Some p ->
            Map.add (newX, newY) (p + 1) state.Presents
    
    { Presents = newPresents; X = newX; Y = newY }

let part1() =
    let endState = Array.fold update initialState allDirs

    let countHouses = Map.count endState.Presents

    printfn "Houses: %i" countHouses

let part2() =
    let santaDirs = allDirs |> Array.mapi (fun i d -> (i, d)) |> Array.filter (fun (i, _) -> i % 2 = 0) |> Array.map snd
    let robotDirs = allDirs |> Array.mapi (fun i d -> (i, d)) |> Array.filter (fun (i, _) -> i % 2 = 1) |> Array.map snd

    let endSantaState = Array.fold update initialState santaDirs

    let initialRobotState = { Presents = Map.add (0, 0) (1 + endSantaState.Presents.[(0, 0)]) endSantaState.Presents; X = 0; Y = 0 }
    let endRobotState = Array.fold update initialRobotState robotDirs

    let countHouses = Map.count endRobotState.Presents

    printfn "Houses: %i" countHouses
