module Day23

open System
open System.IO

let input = File.ReadAllLines(@"input\day23.txt")

type Register = A | B | C | D
type ValueOrRegister = Register of Register | Value of int

type Instruction =
    | Cpy of ValueOrRegister * ValueOrRegister
    | Inc of ValueOrRegister
    | Dec of ValueOrRegister
    | Jnz of ValueOrRegister * ValueOrRegister
    | Tgl of ValueOrRegister

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
        Cpy (parseRegOrValue parts.[1], parseRegOrValue parts.[2])
    | "inc" ->
        Inc (parseRegOrValue parts.[1])
    | "dec" ->
        Dec (parseRegOrValue parts.[1])
    | "jnz" ->
        Jnz (parseRegOrValue parts.[1], parseRegOrValue parts.[2])
    | "tgl" ->
        Tgl (parseRegOrValue parts.[1])
    | _ ->
        failwithf "unexpected %s" s

let initialInstructions =
    input
    |> Array.map parseLine

type State =
    {
        Instructions: Instruction[]
        Registers: Map<Register, int>
        NextInstruction: int
        Finished: bool
    }

let toggle instruction =
    match instruction with
    | Cpy (arg1, arg2) -> Jnz (arg1, arg2)
    | Inc arg1 -> Dec arg1
    | Dec arg1 -> Inc arg1
    | Jnz (arg1, arg2) -> Cpy (arg1, arg2)
    | Tgl arg1 -> Inc arg1

let toggleAt instructions index =
    if index < 0 || index >= Array.length instructions then
        instructions
    else
        let newInstructions = Array.copy instructions
        newInstructions.[index] <- toggle newInstructions.[index]
        newInstructions

let next state = { state with NextInstruction = state.NextInstruction + 1 }

let update state =
    if state.NextInstruction >= Array.length state.Instructions then
        { state with Finished = true }
    else
        let (|Evaluated|) regOrValue =
            match regOrValue with
            | Register r -> state.Registers.[r]
            | Value v -> v
        
        // optimisations
        match state.Instructions |> Array.skip state.NextInstruction |> Array.truncate 5 with
        | [| Inc (Register r1); Dec (Register r2); Jnz (Register r3, Value -2); Dec (Register r4); Jnz (Register r5, Value -5) |]
        | [| Dec (Register r2); Inc (Register r1); Jnz (Register r3, Value -2); Dec (Register r4); Jnz (Register r5, Value -5) |] when r2 = r3 && r4 = r5 && state.Registers.[r2] >= 0 && state.Registers.[r4] >= 0 ->
            { state with Registers = state.Registers |> Map.add r1 (state.Registers.[r1] + state.Registers.[r2] * state.Registers.[r4]) |> Map.add r2 0 |> Map.add r4 0; NextInstruction = state.NextInstruction + 5 }
        | [| Inc (Register r1); Dec (Register r2); Jnz (Register r3, Value -2); _; _ |]
        | [| Dec (Register r2); Inc (Register r1); Jnz (Register r3, Value -2); _; _ |] when r2 = r3 && state.Registers.[r2] >= 0 ->
            { state with Registers = state.Registers |> Map.add r1 (state.Registers.[r1] + state.Registers.[r2]) |> Map.add r2 0; NextInstruction = state.NextInstruction + 3 }
        | _ ->
            // standard cases
            match state.Instructions.[state.NextInstruction] with
            | Inc (Value _ )
            | Dec (Value _ )
            | Cpy (_, Value _) -> state |> next
            | Cpy (Evaluated v, Register rTo) ->
                { state with Registers = Map.add rTo v state.Registers } |> next
            | Inc (Register r) ->
                { state with Registers = Map.add r (state.Registers.[r] + 1) state.Registers } |> next
            | Dec (Register r) ->
                { state with Registers = Map.add r (state.Registers.[r] - 1) state.Registers } |> next
            | Jnz (Evaluated v, Evaluated offset) ->
                if v = 0 then
                    state |> next
                else
                    { state with NextInstruction = state.NextInstruction + offset }
            | Tgl (Evaluated offset) ->
                { state with Instructions = toggleAt state.Instructions (state.NextInstruction + offset) } |> next

let rec loop state =
    if state.Finished then
        state
    else
        state |> update |> loop

let finalRegisterA eggs =
    let finalState =
        {
            Instructions = initialInstructions
            Registers = [ (A, eggs); (B, 0); (C, 0); (D, 0) ] |> Map.ofList
            NextInstruction = 0
            Finished = false
        } |> loop
    
    finalState.Registers.[A]

let part1() =
    printfn "Register A: %i" (finalRegisterA 7)

let part2() =
    printfn "Register A: %i" (finalRegisterA 12)
