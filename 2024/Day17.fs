module Day17

open System
open System.IO

let input = File.ReadAllLines(@"input\day17.txt")

type State =
    {
        A: int64
        B: int64
        C: int64
        Pointer: int
    }

let initialState =
    {
        A = input.[0].Substring(12) |> Int64.Parse
        B = input.[1].Substring(12) |> Int64.Parse
        C = input.[2].Substring(12) |> Int64.Parse
        Pointer = 0
    }

let program =
    input.[4].Substring(9).Split(',') |> Array.map Int32.Parse

let literalOperand state =
    program.[state.Pointer + 1]

let max32 = int64 Int32.MaxValue

let comboOperand state =
    match program.[state.Pointer + 1] with
    | n when n <= 3 -> int64 n
    | 4 -> state.A
    | 5 -> state.B
    | 6 -> state.C
    | _ -> failwith "unexpected"

let comboOperand32 state =
    match program.[state.Pointer + 1] with
    | n when n <= 3 -> n
    | 4 -> if state.A > max32 then Int32.MaxValue else int32 state.A
    | 5 -> if state.B > max32 then Int32.MaxValue else int32 state.B
    | 6 -> if state.C > max32 then Int32.MaxValue else int32 state.C
    | _ -> failwith "unexpected"

let update state =
    if state.Pointer >= program.Length then
        None
    else
        let newState, output =
            match program.[state.Pointer] with
            | 0 ->
                { state with A = state.A >>> comboOperand32 state; Pointer = state.Pointer + 2 }, None
            | 1 ->
                { state with B = state.B ^^^ literalOperand state; Pointer = state.Pointer + 2}, None
            | 2 ->
                { state with B = int64 (comboOperand state % 8L); Pointer = state.Pointer + 2 }, None
            | 3 ->
                if state.A = 0 then
                    { state with Pointer = state.Pointer + 2 }, None
                else
                    { state with Pointer = literalOperand state }, None
            | 4 ->
                { state with B = state.B ^^^ state.C; Pointer = state.Pointer + 2 }, None
            | 5 ->
                { state with Pointer = state.Pointer + 2 }, Some (comboOperand state % 8L |> int32)
            | 6 ->
                { state with B = state.A >>> comboOperand32 state; Pointer = state.Pointer + 2 }, None
            | 7 ->
                { state with C = state.A >>> comboOperand32 state; Pointer = state.Pointer + 2 }, None
            | _ -> failwith "unexpected"
        Some (newState, output)

let getOutput startState =
    let rec runProgram acc state =
        let p = update state
        match p with
        | None -> List.rev acc |> List.toArray
        | Some (newState, None) -> runProgram acc newState
        | Some (newState, Some output) -> runProgram (output :: acc) newState
    
    runProgram [] startState

let part1() =
    let output = getOutput initialState

    let formatted = String.Join(',', output |> Array.map (fun n -> n.ToString()))

    printfn "Output: %s" formatted

let part2() =
    let resultA a = getOutput { initialState with A = a }

    let rec smallest acc rem =
        match rem with
        | [] -> Some acc
        | target :: xs ->
            let results = Array.init 8 (fun i -> i, resultA (8L * acc + int64 i))
            let valid = results |> Array.filter (fun (_, r) -> r.[0] = target) |> Array.map fst
            valid |> Array.tryPick (fun i -> smallest (8L * acc + int64 i) xs)
    
    let targets = program |> List.ofArray |> List.rev

    let aOpt = smallest 0 targets

    match aOpt with
    | Some a -> printfn "A: %i" a
    | None -> printfn "not found"
