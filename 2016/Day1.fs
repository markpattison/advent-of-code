module Day1

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day1.txt")

type Turn = Left | Right | Nothing
type Direction = N | E | S | W
type State = { X: int; Y: int; Dir: Direction }

let parseInstruction (s: string) =
    let turn =
        match s.Substring(0, 1) with
        | "L" -> Left
        | "R" -> Right
        | _ -> failwith "Unexpected"
    let distance = s.Substring(1) |> Int32.Parse

    turn, distance

let allInstructions =
    input.[0].Split(", ")
    |> Array.map parseInstruction

let dxy dir =
    match dir with
    | N -> 0, 1
    | E -> 1, 0
    | S -> 0, -1
    | W -> -1, 0
    
let updateState state (turn, distance) =
    let newDir =
        match state.Dir, turn with
        | N, Right | S, Left -> E
        | E, Right | W, Left -> S
        | S, Right | N, Left -> W
        | W, Right | E, Left -> N
        | dir, Nothing -> dir
    
    let dx, dy = dxy newDir
    
    { X = state.X + distance * dx; Y = state.Y + distance * dy; Dir = newDir }

let startState = { X = 0; Y = 0; Dir = N }

let part1() =
    let finalState = Array.fold updateState startState allInstructions

    let manhattanDistance = (abs finalState.X) + (abs finalState.Y)

    printfn "Distance: %i" manhattanDistance

let part2() =
    let singleInstructions =
        allInstructions |> Array.collect (fun (turn, dist) -> Array.append [| (turn, 1) |] (Array.create (dist - 1) (Nothing, 1)))

    let allLocations =
        Array.scan updateState startState singleInstructions
        |> Array.map (fun state -> (state.X, state.Y))
    
    let mutable visited : Set<int * int> = Set.empty

    let firstTwiceX, firstTwiceY =
        allLocations
        |> Array.find (fun (x, y) ->
            if Set.contains (x, y) visited then
                true
            else
                visited <- Set.add (x, y) visited
                false)
    
    let manhattanDistance = (abs firstTwiceX) + (abs firstTwiceY)

    printfn "Distance: %i" manhattanDistance
