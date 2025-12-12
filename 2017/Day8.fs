module Day8

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day8.txt")

type Action = Inc | Dec

type Condition = Eq | Ne | Gt | Lt | Ge | Le

type Instruction =
    {
        Register: string
        Action: Action
        Amount: int
        ConditionRegister: string
        Condition: Condition
        Comparator: int
    }

let parseAction (s: string) =
    match s with
    | "inc" -> Inc
    | "dec" -> Dec
    | _ -> failwith "unexpected"

let parseCondition (s: string) =
    match s with
    | "==" -> Eq
    | "!=" -> Ne
    | ">" -> Gt
    | "<" -> Lt
    | ">=" -> Ge
    | "<=" -> Le
    | _ -> failwith "unexpected"

let parseLine (s: string) =
    let parts = s.Split(' ')
    {
        Register = parts.[0]
        Action = parts.[1] |> parseAction
        Amount = parts.[2] |> Int32.Parse
        ConditionRegister = parts.[4]
        Condition = parts.[5] |> parseCondition
        Comparator = parts.[6] |> Int32.Parse
    }

let allInstructions =
    input
    |> Array.map parseLine

let allRegisters =
    allInstructions
    |> Array.collect (fun instr -> [| instr.Register; instr.ConditionRegister |])
    |> Array.distinct

let initialRegisters = allRegisters |> Array.map (fun r -> r, 0) |> Map.ofArray

let evaluate registerValue condition comparator =
    match condition with
    | Eq -> registerValue = comparator
    | Ne -> registerValue <> comparator
    | Gt -> registerValue > comparator
    | Lt -> registerValue < comparator
    | Ge -> registerValue >= comparator
    | Le -> registerValue <= comparator

let apply (registers : Map<string, int>, highestEver) instruction =
    let conditionRegisterValue = registers.[instruction.ConditionRegister]
    if evaluate conditionRegisterValue instruction.Condition instruction.Comparator then
        let change = match instruction.Action with | Inc -> instruction.Amount | Dec -> -instruction.Amount
        let newValue = registers.[instruction.Register] + change
        Map.add instruction.Register newValue registers, max highestEver newValue
    else
        registers, highestEver

let part1() =
    let finalRegisters, _ = Array.fold apply (initialRegisters, 0) allInstructions

    let maxRegister = finalRegisters |> Map.toArray |> Array.map snd |> Array.max

    printfn "Max register: %i" maxRegister

let part2() =
    let _, highestEver = Array.fold apply (initialRegisters, 0) allInstructions

    printfn "Max ever: %i" highestEver
