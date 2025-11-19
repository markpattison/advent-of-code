module Day25

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day25.txt")

type Register = A | B | C | D
type ValueOrRegister = Register of Register | Value of int

type Instruction =
    | Cpy of ValueOrRegister * ValueOrRegister
    | Inc of ValueOrRegister
    | Dec of ValueOrRegister
    | Jnz of ValueOrRegister * ValueOrRegister
    | Out of ValueOrRegister

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
    | "out" ->
        Out (parseRegOrValue parts.[1])
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

let next state = { state with NextInstruction = state.NextInstruction + 1 }

let update (state, emittedRev) =
    if state.NextInstruction >= Array.length state.Instructions then
        { state with Finished = true }, emittedRev
    else
        let (|Evaluated|) regOrValue =
            match regOrValue with
            | Register r -> state.Registers.[r]
            | Value v -> v
        
        // optimisations
        match state.Instructions |> Array.skip state.NextInstruction |> Array.truncate 8 with
        | [| Cpy (Value 2, Register r1); Jnz (Register r2, Value 2); Jnz (Value 1, Value 6); Dec (Register r3); Dec (Register r4); Jnz (Register r5, Value -4); Inc (Register r6); Jnz (Value 1, Value -7) |] when r1 = r4 && r4 = r5 && r2 = r3 ->
            { state with Registers = state.Registers |> Map.add r2 0 |> Map.add r1 (2 - state.Registers.[r2] % 2) |> Map.add r6 (state.Registers.[r6] + state.Registers.[r2] / 2); NextInstruction = state.NextInstruction + 8 }, emittedRev
        | [| Inc (Register r1); Dec (Register r2); Jnz (Register r3, Value -2); Dec (Register r4); Jnz (Register r5, Value -5); _; _; _ |]
        | [| Dec (Register r2); Inc (Register r1); Jnz (Register r3, Value -2); Dec (Register r4); Jnz (Register r5, Value -5); _; _; _ |] when r2 = r3 && r4 = r5 && state.Registers.[r2] >= 0 && state.Registers.[r4] >= 0 ->
            { state with Registers = state.Registers |> Map.add r1 (state.Registers.[r1] + state.Registers.[r2] * state.Registers.[r4]) |> Map.add r2 0 |> Map.add r4 0; NextInstruction = state.NextInstruction + 5 }, emittedRev
        | [| Inc (Register r1); Dec (Register r2); Jnz (Register r3, Value -2); _; _; _; _; _ |]
        | [| Dec (Register r2); Inc (Register r1); Jnz (Register r3, Value -2); _; _; _; _; _ |] when r2 = r3 && state.Registers.[r2] >= 0 ->
            { state with Registers = state.Registers |> Map.add r1 (state.Registers.[r1] + state.Registers.[r2]) |> Map.add r2 0; NextInstruction = state.NextInstruction + 3 }, emittedRev
        | _ ->
            // standard cases
            match state.Instructions.[state.NextInstruction] with
            | Inc (Value _ )
            | Dec (Value _ )
            | Cpy (_, Value _) -> state |> next, emittedRev
            | Cpy (Evaluated v, Register rTo) ->
                { state with Registers = Map.add rTo v state.Registers } |> next, emittedRev
            | Inc (Register r) ->
                { state with Registers = Map.add r (state.Registers.[r] + 1) state.Registers } |> next, emittedRev
            | Dec (Register r) ->
                { state with Registers = Map.add r (state.Registers.[r] - 1) state.Registers } |> next, emittedRev
            | Jnz (Evaluated v, Evaluated offset) ->
                if v = 0 then
                    state |> next, emittedRev
                else
                    { state with NextInstruction = state.NextInstruction + offset }, emittedRev
            | Out (Evaluated v) ->
                state |> next, v :: emittedRev



let part1() =
    let mutable memo : Map<State, int> = Map.empty

    let rec loop (state, emittedRev) =
        if state.Registers.[B] < 5 then
            ()
        if state.Finished then
            (state, emittedRev)
        else
            match Map.tryFind state memo with
            | Some emitLength ->
                (state, emittedRev)
            | None ->
                memo <- Map.add state (List.length emittedRev) memo
                (state, emittedRev) |> update |> loop
    
    let rec firstClockSignal next =
        let resultRev =
            ({
                Instructions = initialInstructions
                Registers = [ (A, next); (B, 0); (C, 0); (D, 0) ] |> Map.ofList
                NextInstruction = 0
                Finished = false
            }, []) |> loop |> snd
        if resultRev |> List.rev |> List.chunkBySize 2 |> List.forall (fun l -> l = [ 0; 1 ]) then
            next
        else
            memo <- Map.empty
            firstClockSignal (next + 1)
    
    let first = firstClockSignal 0

    printfn "First clock signal: %i" first

let part2() =
    ()
