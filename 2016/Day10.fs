module Day10

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day10.txt")

type Location =
    | Output of int
    | Bot of int

type Instruction =
    | FromInput of int * Location               // value, bot
    | Compare of int * Location * Location      // bot, low output, high output

let parseOutput s n =
    match s with
    | "bot" -> Bot n
    | "output" -> Output n
    | _ -> failwith "unexpected"

let parseLine (s: string) =
    let parts = s.Split(' ')
    let p i = parts.[i] |> Int32.Parse
    match parts.[0] with
    | "value" ->
        FromInput (p 1, Bot (p 5))
    | "bot" ->
        let out1 = parseOutput parts.[5] (p 6)
        let out2 = parseOutput parts.[10] (p 11)
        Compare (p 1, out1, out2)
    | _ -> failwith "unexpected"

let allInstructions =
    input
    |> Array.map parseLine

let allBotNumbers =
    seq {
        for instruction in allInstructions do
            match instruction with
            | FromInput (_, Bot bot) -> yield bot
            | Compare (bot, _, _) -> yield bot
            | _ -> ()
    } |> Seq.distinct |> Seq.sort |> Seq.toArray

let allOutputs =
    seq {
        for instruction in allInstructions do
            match instruction with
            | FromInput _ -> ()
            | Compare (_, loc1, loc2) ->
                match loc1 with | Output n -> yield n | _ -> ()
                match loc2 with | Output n -> yield n | _ -> ()
    } |> Seq.distinct |> Seq.sort |> Seq.toArray

type BotValues =
    | Unknown
    | One of int
    | LowHigh of int * int

let botInputs, outputs =
    let mutable botInputs =
        allBotNumbers
        |> Seq.map (fun bot -> bot, Unknown)
        |> Map.ofSeq
    
    let mutable outputs =
        allOutputs
        |> Seq.map (fun output -> output, Unknown)
        |> Map.ofSeq
    
    let mutable remainingInstructions = allInstructions |> Array.toList
    let mutable haveUpdated = true

    let addToLocation location value =
        match location with
        | Bot bot ->
            match botInputs.[bot] with
            | Unknown -> botInputs <- Map.add bot (One value) botInputs
            | One existing -> botInputs <- Map.add bot (LowHigh (min existing value, max existing value)) botInputs
            | _ -> failwith "unexpected"
        | Output output ->
            match outputs.[output] with
            | Unknown -> outputs <- Map.add output (One value) outputs
            | _ -> failwith "unexpected"
    
    while haveUpdated do
        let mutable clearedInstructions = []
        haveUpdated <- false

        remainingInstructions
        |> List.iter (fun instruction ->
            match instruction with
            | FromInput (value, bot) ->
                addToLocation bot value
                clearedInstructions <- instruction :: clearedInstructions
                haveUpdated <- true
            | Compare (bot, outputLow, outputHigh) ->
                match botInputs.[bot] with
                | Unknown | One _ -> ()
                | LowHigh (low, high) ->
                    addToLocation outputLow low
                    addToLocation outputHigh high
                    clearedInstructions <- instruction :: clearedInstructions
                    haveUpdated <- true)
        
        remainingInstructions <- List.except clearedInstructions remainingInstructions
    
    botInputs, outputs

let part1() =
    let bot17and61 =
        botInputs
        |> Map.toSeq
        |> Seq.find (fun (_, values) -> values = LowHigh (17, 61))
        |> fst
    
    printfn "Bot number: %i" bot17and61

let part2() =
    let outputValue output =
        match outputs.[output] with
        | One value -> value
        | _ -> failwith "unexpected"
    
    let product = (outputValue 0) * (outputValue 1) * (outputValue 2)

    printfn "Product: %i" product
