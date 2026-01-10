module Day18

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day18.txt")

type RegisterOrValue = Register of char | Value of int64

type Instruction =
    | Snd of RegisterOrValue
    | Set of char * RegisterOrValue
    | Add of char * RegisterOrValue
    | Mul of char * RegisterOrValue
    | Mod of char * RegisterOrValue
    | Rcv of char
    | Jgz of RegisterOrValue * RegisterOrValue

let parseRegister (s: string) = s.[0]

let parseRegisterOrValue (s: string) =
    match Int64.TryParse(s) with
    | true, n -> Value n
    | _ -> Register s.[0]

let parseLine (s: string) =
    let parts = s.Split(' ')
    match parts.[0] with
    | "snd" -> Snd (parseRegisterOrValue parts.[1])
    | "set" -> Set (parseRegister parts.[1], parseRegisterOrValue parts.[2])
    | "add" -> Add (parseRegister parts.[1], parseRegisterOrValue parts.[2])
    | "mul" -> Mul (parseRegister parts.[1], parseRegisterOrValue parts.[2])
    | "mod" -> Mod (parseRegister parts.[1], parseRegisterOrValue parts.[2])
    | "rcv" -> Rcv (parseRegister parts.[1])
    | "jgz" -> Jgz (parseRegisterOrValue parts.[1], parseRegisterOrValue parts.[2])
    | _ -> failwith "unexpected"

let allInstructions =
    input
    |> Array.map parseLine

let extractReg regOrValue =
    match regOrValue with
    | Register r -> Some r
    | _ -> None

let extractRegisters instr =
    match instr with
    | Snd rv -> [| extractReg rv |]
    | Set (r, rv) -> [| Some r; extractReg rv |]
    | Add (r, rv) -> [| Some r; extractReg rv |]
    | Mul (r, rv) -> [| Some r; extractReg rv |]
    | Mod (r, rv) -> [| Some r; extractReg rv |]
    | Rcv r -> [| Some r |]
    | Jgz (rv1, rv2) -> [| extractReg rv1; extractReg rv2 |]

let allRegisters =
    allInstructions
    |> Array.collect extractRegisters
    |> Array.choose id
    |> Array.distinct

type State =
    {
        ProgramCounter: int
        LastSound: int64 option
        LastRecovered: int64 option
        Registers: Map<char, int64>
        Terminated: bool
    }

let initialState =
    {
        ProgramCounter = 0
        LastSound = None
        LastRecovered = None
        Registers = allRegisters |> Array.map (fun r -> r, 0L) |> Map.ofArray
        Terminated = false
    }

let next state = { state with ProgramCounter = state.ProgramCounter + 1 }

let update state =
    if state.ProgramCounter < 0 || state.ProgramCounter >= allInstructions.Length then
        { state with Terminated = true }
    else
        let (|Evaluated|) regOrValue =
            match regOrValue with
            | Register r -> state.Registers.[r]
            | Value v -> v
        match allInstructions.[state.ProgramCounter] with
        | Snd (Evaluated v) ->
            { state with LastSound = Some v } |> next
        | Set (r, Evaluated v) ->
            { state with Registers = state.Registers |> Map.add r v } |> next
        | Add (r, Evaluated v) ->
            let (Evaluated rv) = Register r
            { state with Registers = state.Registers |> Map.add r (rv + v) } |> next
        | Mul (r, Evaluated v) ->
            let (Evaluated rv) = Register r
            { state with Registers = state.Registers |> Map.add r (rv * v) } |> next
        | Mod (r, Evaluated v) ->
            let (Evaluated rv) = Register r
            { state with Registers = state.Registers |> Map.add r (rv % v) } |> next
        | Rcv r ->
            if state.Registers.[r] > 0L then
                { state with LastRecovered = state.LastSound } |> next
            else
                state |> next
        | Jgz (Evaluated v, Evaluated offset) ->
            if v > 0L then
                { state with ProgramCounter = state.ProgramCounter + (int offset) }
            else
                state |> next

let part1() =
    let generator state =
        if state.Terminated then
            None
        else
            let nextState = update state
            Some (nextState.LastRecovered, nextState)
    
    let firstRecovered =
        Seq.unfold generator initialState
        |> Seq.choose id
        |> Seq.head
    
    printfn "First sound recovered: %i" firstRecovered

type State2 =
    {
        ProgramCounter: int
        Queue: int64 list
        Registers: Map<char, int64>
        Terminated: bool
        Waiting: bool
    }

type MetaState2 =
    {
        Program0: State2
        Program1: State2
        ValuesSent1: int
        BothTerminated: bool
    }

let initialState2 =
    {
        Program0 =
            {
                ProgramCounter = 0
                Queue = []
                Registers = allRegisters |> Array.map (fun r -> r, 0L) |> Map.ofArray
                Terminated = false
                Waiting = false
            }
        Program1 =
            {
                ProgramCounter = 0
                Queue = []
                Registers = allRegisters |> Array.map (fun r -> r, if r = 'p' then 1L else 0L) |> Map.ofArray
                Terminated = false
                Waiting = false
            }
        ValuesSent1 = 0
        BothTerminated = false
    }

let next2 state : State2 = { state with ProgramCounter = state.ProgramCounter + 1 }

let update2 state =
    if state.ProgramCounter < 0 || state.ProgramCounter >= allInstructions.Length then
        { state with Terminated = true }, None
    else
        let (|Evaluated|) regOrValue =
            match regOrValue with
            | Register r -> state.Registers.[r]
            | Value v -> v
        match allInstructions.[state.ProgramCounter] with
        | Snd (Evaluated v) ->
            state |> next2, Some v
        | Set (r, Evaluated v) ->
            { state with Registers = state.Registers |> Map.add r v } |> next2, None
        | Add (r, Evaluated v) ->
            let (Evaluated rv) = Register r
            { state with Registers = state.Registers |> Map.add r (rv + v) } |> next2, None
        | Mul (r, Evaluated v) ->
            let (Evaluated rv) = Register r
            { state with Registers = state.Registers |> Map.add r (rv * v) } |> next2, None
        | Mod (r, Evaluated v) ->
            let (Evaluated rv) = Register r
            { state with Registers = state.Registers |> Map.add r (rv % v) } |> next2, None
        | Rcv r ->
            match state.Queue with
            | x :: xs -> { state with Registers = state.Registers |> Map.add r x; Queue = xs; Waiting = false } |> next2, None
            | [] -> { state with Waiting = true }, None
        | Jgz (Evaluated v, Evaluated offset) ->
            if v > 0L then
                { state with ProgramCounter = state.ProgramCounter + (int offset) }, None
            else
                state |> next2, None

let metaUpdate2 metaState =
    if (metaState.Program0.Terminated && metaState.Program1.Terminated) || (metaState.Program0.Waiting && metaState.Program1.Waiting) then
        { metaState with BothTerminated = true }
    else
        let newState0, sent0 = update2 metaState.Program0
        let newState1, sent1 = update2 metaState.Program1
        {
            Program0 =
                match sent1 with
                | Some s -> { newState0 with Queue = newState0.Queue @ [ s ] }
                | None -> newState0
            Program1 =
                match sent0 with
                | Some s -> { newState1 with Queue = newState1.Queue @ [ s ] }
                | None -> newState1
            ValuesSent1 = metaState.ValuesSent1 + if sent1.IsSome then 1 else 0
            BothTerminated = false
        }

let part2() =
    let rec updateAll metaState =
        if metaState.BothTerminated then
            metaState
        else
            metaState |> metaUpdate2 |> updateAll
    
    let finalState = updateAll initialState2
    
    printfn "Times program 0 sent value: %i" finalState.ValuesSent1
