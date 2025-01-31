module Day23

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day23.txt")

type Register = A | B

type Instruction =
    | Hlf of Register
    | Tpl of Register
    | Inc of Register
    | Jmp of int
    | Jie of Register * int
    | Jio of Register * int

let parseLine (s: string) =
    let parts = s.Replace(",", "").Split(" ")

    let reg n =
        match parts.[n] with
        | "a" -> A
        | "b" -> B
        | _ -> failwith "unexpected"
    let value n =
        Int32.Parse(parts.[n])
    
    match parts.[0] with
    | "hlf" -> Hlf (reg 1)
    | "tpl" -> Tpl (reg 1)
    | "inc" -> Inc (reg 1)
    | "jmp" -> Jmp (value 1)
    | "jie" -> Jie (reg 1, value 2)
    | "jio" -> Jio (reg 1, value 2)
    | _ -> failwith "unexpected"

let program =
    input
    |> Array.map parseLine

type state =
    {
        RegA: uint64
        RegB: uint64
        Counter: int
    }

let startState = { RegA = 0UL; RegB = 0UL; Counter = 0 }

let upC state = { state with Counter = state.Counter + 1}

let update (program: Instruction[]) state =
    match program.[state.Counter] with
    | Hlf A -> { state with RegA = state.RegA / 2UL } |> upC
    | Hlf B -> { state with RegB = state.RegB / 2UL } |> upC
    | Tpl A -> { state with RegA = state.RegA * 3UL } |> upC
    | Tpl B -> { state with RegB = state.RegB * 3UL } |> upC
    | Inc A -> { state with RegA = state.RegA + 1UL } |> upC
    | Inc B -> { state with RegB = state.RegB + 1UL } |> upC
    | Jmp n -> { state with Counter = state.Counter + n }
    | Jie (A, n) -> if state.RegA % 2UL = 0UL then { state with Counter = state.Counter + n } else state |> upC
    | Jie (B, n) -> if state.RegB % 2UL = 0UL then { state with Counter = state.Counter + n } else state |> upC
    | Jio (A, n) -> if state.RegA = 1UL then { state with Counter = state.Counter + n } else state |> upC
    | Jio (B, n) -> if state.RegB = 1UL then { state with Counter = state.Counter + n } else state |> upC

let execute program initialState =
    let rec exec state =
        if state.Counter >= Array.length program then
            state
        else
            state |> update program |> exec
    
    exec initialState

let part1() =
    let result = execute program startState
    
    printfn "Reg B: %i" result.RegB

let part2() =
    let startState2 = { startState with RegA = 1UL }

    let result = execute program startState2
    
    printfn "Reg B: %i" result.RegB
