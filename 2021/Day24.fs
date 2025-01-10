module Day24

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day24.txt")

type Register = W | X | Y | Z
type Operand = Reg of Register | Value of int64

type Instruction =
    | Inp of Register
    | Add of Register * Operand
    | Mul of Register * Operand
    | Div of Register * Operand
    | Mod of Register * Operand
    | Eql of Register * Operand

type State =
    {
        Registers: Map<Register, int64>
        RemainingInputs: int64 list
    }

let parseRegister (s: string) =
    match s with | "w" -> W | "x" -> X | "y" -> Y | "z" -> Z | _ -> failwith "unexpected"

let parseOperand (s: string) =
    match s with | "w" -> Reg W | "x" -> Reg X | "y" -> Reg Y | "z" -> Reg Z | v -> Int64.Parse(v) |> Value

let parseLine (s: string) =
    match s.Split(' ') with
    | [| "inp"; r |] -> Inp (parseRegister r)
    | [| "add"; r; o |] -> Add (parseRegister r, parseOperand o)
    | [| "mul"; r; o |] -> Mul (parseRegister r, parseOperand o)
    | [| "div"; r; o |] -> Div (parseRegister r, parseOperand o)
    | [| "mod"; r; o |] -> Mod (parseRegister r, parseOperand o)
    | [| "eql"; r; o |] -> Eql (parseRegister r, parseOperand o)
    | _ -> failwith "unexpected"

let inputProgram =
    input |> Array.map parseLine

let processInstruction state instruction =
    let update reg value = { state with Registers = Map.add reg value state.Registers }

    match instruction with
    | Inp reg ->
        match state.RemainingInputs with
        | [] -> failwith "ran out of inputs"
        | x :: xs ->
            printfn "Z = %i" state.Registers.[Z]
            { update reg x with RemainingInputs = xs }
    | Add (reg, Reg reg2) ->
        update reg (state.Registers.[reg] + state.Registers.[reg2])
    | Add (reg, Value v) ->
        update reg (state.Registers.[reg] + v)
    | Mul (reg, Reg reg2) ->
        update reg (state.Registers.[reg] * state.Registers.[reg2])
    | Mul (reg, Value v) ->
        update reg (state.Registers.[reg] * v)
    | Div (reg, Reg reg2) ->
        update reg (state.Registers.[reg] / state.Registers.[reg2])
    | Div (reg, Value v) ->
        update reg (state.Registers.[reg] / v)
    | Mod (reg, Reg reg2) ->
        update reg (state.Registers.[reg] % state.Registers.[reg2])
    | Mod (reg, Value v) ->
        update reg (state.Registers.[reg] % v)
    | Eql (reg, Reg reg2) ->
        printfn "%i ?= %i" state.Registers.[reg] state.Registers.[reg2]
        update reg (if state.Registers.[reg] = state.Registers.[reg2] then 1 else 0)
    | Eql (reg, Value v) ->
        update reg (if state.Registers.[reg] = v then 1 else 0)

let runProgram instructions inputs =
    let initialState =
        {
            Registers = [ (W, 0L); (X, 0L); (Y, 0L); (Z, 0L) ] |> Map.ofList
            RemainingInputs = inputs
        }
    
    let r = Array.fold processInstruction initialState instructions
    printfn "Z = %i" r.Registers.[Z]
    r

let testModelNumberIsValid n =
    let digits = n.ToString().ToCharArray()
    if Array.contains '0' digits then
        false
    else
        let inputs = digits |> Array.map (_.ToString() >> Int64.Parse) |> Array.toList
        let result = runProgram inputProgram inputs
        if result.Registers.[Z] = 0 then
            true
        else
            false

let part1() =
    // let highestValidModelNumber =
    //     { 99999999999999L .. -1L .. 11111111111111L }
    //     |> Seq.find testModelNumberIsValid
    
    // printfn "Highest valid model number: %i" highestValidModelNumber
    let x = testModelNumberIsValid 99997996997156L
    ()
