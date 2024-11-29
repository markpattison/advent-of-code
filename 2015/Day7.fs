module Day7

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day7.txt")

type Source =
    | SourceWire of string
    | SourceValue of uint16

type Input =
    | Value of Source
    | And of Source * Source
    | Or of Source * Source
    | Not of Source
    | LShift of Source * int
    | RShift of Source * int

let parseSource (s: string) =
    match UInt16.TryParse(s) with
    | true, value -> SourceValue value
    | _ -> SourceWire s

let parseInput (s: string) =
    match s.Split(" ") with
    | [| s1 |] -> Value (parseSource s1)
    | [| "NOT"; s1 |] -> Not (parseSource s1)
    | [| s1; "AND"; s2 |] -> And (parseSource s1, parseSource s2)
    | [| s1; "OR"; s2 |] -> Or (parseSource s1, parseSource s2)
    | [| s1; "LSHIFT"; v1 |] -> LShift (parseSource s1, int v1)
    | [| s1; "RSHIFT"; v1 |] -> RShift (parseSource s1, int v1)
    | _ -> failwithf "unexpected %s" s

let parseLine (s: string) =
    match s.Split(" -> ") with
    | [| input; output |] ->
        (output, parseInput input)
    | _ -> failwith "unexpected"

let allInstructions =
    input
    |> Array.map parseLine
    |> Map.ofArray

let resolve instructions finalTarget =
    let mutable cache : Map<string, uint16> = Map.empty

    let rec resolveSource source =
        match source with
        | SourceValue v -> v
        | SourceWire wire ->
        match Map.tryFind wire cache with
        | Some value -> value
        | None ->
            let value =
                match Map.tryFind wire instructions with
                | Some input ->
                    match input with
                    | Value source -> resolveSource source
                    | And (s1, s2) -> (resolveSource s1) &&& (resolveSource s2)
                    | Or (s1, s2) -> (resolveSource s1) ||| (resolveSource s2)
                    | Not s1 -> ~~~ (resolveSource s1)
                    | LShift (s1, bits) -> (resolveSource s1) <<< bits
                    | RShift (s1, bits) -> (resolveSource s1) >>> bits
                | None -> failwith "unexpected"
            cache <- Map.add wire value cache 
            value
    
    resolveSource (SourceWire finalTarget)

let part1() =
    let result = resolve allInstructions "a"
    
    printfn "Wire a: %i" result

let part2() =
    let result1 = resolve allInstructions "a"
    
    let newInstructions =
        allInstructions
        |> Map.add "b" (Value (SourceValue result1))
    
    let result = resolve newInstructions "a"
    
    printfn "Wire a: %i" result
