module Day12

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day12.txt")

type Register = A | B | C | D
type ValueOrRegister = Register of Register | Value of int

type Instruction =
    | Cpy of ValueOrRegister * Register
    | Inc of Register
    | Dec of Register
    | Jnz of ValueOrRegister * int

let parseReg s =
    match s with
        | "a" -> A
        | "b" -> B
        | "c" -> C
        | "d" -> D
        | _ -> failwithf "unexpected %s" s

let parseRegOrValue s =
    match s with
        | "a" -> Register A
        | "b" -> Register B
        | "c" -> Register C
        | "d" -> Register D
        | v -> Value (Int32.Parse(v))

let parseLine (s: string) =
    let parts = s.Split(' ')
    match parts.[0] with
    | "cpy" ->
        Cpy (parseRegOrValue parts.[1], parseReg parts.[2])
    | "inc" ->
        Inc (parseReg parts.[1])
    | "dec" ->
        Dec (parseReg parts.[1])
    | "jnz" ->
        Jnz (parseRegOrValue parts.[1], Int32.Parse(parts.[2]))
    | _ ->
        failwithf "unexpected %s" s

let instructions =
    input
    |> Array.map parseLine

type State =
    {
        Registers: Map<Register, int>
        NextInstruction: int
        Finished: bool
    }

let initialState =
    {
        Registers = [ (A, 0); (B, 0); (C, 0); (D, 0) ] |> Map.ofList
        NextInstruction = 0
        Finished = false
    }

let update instructions state =
    if state.NextInstruction >= Array.length instructions then
        { state with Finished = true }
    else
        match instructions.[state.NextInstruction] with
        | Cpy (Register rFrom, rTo) ->
            { state with Registers = Map.add rTo state.Registers.[rFrom] state.Registers; NextInstruction = state.NextInstruction + 1 }
        | Cpy (Value v, rTo) ->
            { state with Registers = Map.add rTo v state.Registers; NextInstruction = state.NextInstruction + 1 }
        | Inc r ->
            { state with Registers = Map.add r (state.Registers.[r] + 1) state.Registers; NextInstruction = state.NextInstruction + 1 }
        | Dec r ->
            { state with Registers = Map.add r (state.Registers.[r] - 1) state.Registers; NextInstruction = state.NextInstruction + 1 }
        | Jnz (Register r, offset) ->
            if state.Registers.[r] = 0 then
                { state with NextInstruction = state.NextInstruction + 1 }
            else
                { state with NextInstruction = state.NextInstruction + offset }
        | Jnz (Value v, offset) ->
            if v = 0 then
                { state with NextInstruction = state.NextInstruction + 1 }
            else
                { state with NextInstruction = state.NextInstruction + offset }

let part1() =
    let mutable state = initialState

    while not state.Finished do
        state <- update instructions state
    
    printfn "Register A: %i" state.Registers.[A]

let part2() =
    let mutable state = { initialState with Registers = Map.add C 1 initialState.Registers }

    while not state.Finished do
        state <- update instructions state
    
    printfn "Register A: %i" state.Registers.[A]
