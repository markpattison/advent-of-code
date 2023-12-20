module Day20

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day20.txt")

type FlipFlopState = On | Off
type PulseType = Low | High

type ModuleType =
    | Broadcaster
    | FlipFlop of FlipFlopState
    | Conjunction of Map<string, PulseType>

type Module =
    {
        ModuleType: ModuleType
        Outputs: string list
    }

let flipState ffs =
    match ffs with
    | On -> Off
    | Off -> On

let parseModuleType (s: string) =
    if s = "broadcaster" then
        "broadcaster", Broadcaster
    elif s.StartsWith("%") then
        s.Substring(1), FlipFlop Off
    elif s.StartsWith("&") then
        s.Substring(1), Conjunction Map.empty
    else
        failwith "unexpected"

let parseOutputs (s: string) =
    s.Split(",")
    |> Array.map (fun o -> o.Trim())

let parseLine (s: string) =
    let splits = s.Split(" -> ")
    let name, moduleType = parseModuleType splits.[0]
    let outputs = parseOutputs splits.[1] |> Array.toList

    name, { ModuleType = moduleType; Outputs = outputs }

let modules =
    let modulesWithoutInputs =
        input
        |> Array.map parseLine
    
    let allInputs name =
        modulesWithoutInputs
        |> Seq.filter (fun (_, m) -> List.contains name m.Outputs)
        |> Seq.map (fun (name, _) -> (name, Low))
        |> Map.ofSeq

    modulesWithoutInputs
    |> Array.map (fun (name, m) ->
                    match m.ModuleType with
                    | Conjunction _ -> (name, { m with ModuleType = Conjunction (allInputs name) })
                    | other -> (name, m))
    |> Map.ofArray

type Pulse =
    {
        From: string
        PulseType: PulseType
        To: string
    }

type State =
    {
        Modules: Map<string, Module>
        PulsesToProcess: Pulse list
    }

let initialPulse = { From = "button"; PulseType = Low; To = "broadcaster" }

let rec updateState f state =
    match state.PulsesToProcess with
    | [] -> state.Modules
    | pulse :: remainingPulses ->
        f pulse
        let name = pulse.To
        match Map.tryFind name state.Modules with
        | None -> updateState f { state with PulsesToProcess = remainingPulses }
        | Some receiver ->
            let updatedReceiver, pulseToSend =
                match receiver.ModuleType, pulse.PulseType with
                | Broadcaster, pt ->
                    receiver, Some pt
                | FlipFlop _, High ->
                    receiver, None
                | FlipFlop Off, Low ->
                    { receiver with ModuleType = FlipFlop On }, Some High
                | FlipFlop On, Low ->
                    { receiver with ModuleType = FlipFlop Off }, Some Low
                | Conjunction inputs, pt ->
                    let updatedInputs = inputs |> Map.add pulse.From pt
                    let toSend = if updatedInputs |> Map.forall (fun _ p -> p = High) then Low else High
                    { receiver with ModuleType = Conjunction updatedInputs }, Some toSend
            
            let updatedModules = state.Modules |> Map.add name updatedReceiver
            let newPulses =
                match pulseToSend with
                | None -> []
                | Some pt ->
                    receiver.Outputs
                    |> List.map (fun output -> { From = name; PulseType = pt; To = output })
            
            updateState f { Modules = updatedModules; PulsesToProcess = remainingPulses @ newPulses }

let runModules f mods = updateState f { Modules = mods; PulsesToProcess = [ initialPulse ] }

let part1() =
    let mutable lowPulses = 0L
    let mutable highPulses = 0L
    let f pulse = if pulse.PulseType = Low then lowPulses <- lowPulses + 1L else highPulses <- highPulses + 1L

    let mutable currentModules = modules

    for _ in 1 .. 1000 do
        currentModules <- runModules f currentModules

    printfn "Produce of pulses: %i" (lowPulses * highPulses)

let rec gcd a b =
    if b = 0L then a else gcd b (a % b)

let lcm a b = a * b / (gcd a b)

let part2() =
    let countToHit moduleName =
        let mutable buttonCount = 0L
        let mutable hit = false
        let f pulse = if pulse.PulseType = Low && pulse.To = moduleName then hit <- true

        let mutable currentModules = modules

        while not hit do
            buttonCount <- buttonCount + 1L
            currentModules <- runModules f currentModules
        
        buttonCount

    // relies on there being one module before rx,
    // and several before that one,
    // which all need to be hit together

    let beforeRx =
        modules
        |> Map.toSeq
        |> Seq.filter (fun (_, m) -> m.Outputs = [ "rx" ])
        |> Seq.map fst
        |> Seq.exactlyOne
    
    let toHitRx =
        modules
        |> Map.toSeq
        |> Seq.filter (fun (_, m) -> m.Outputs = [ beforeRx ])
        |> Seq.map fst
        |> Seq.map countToHit
        |> Seq.reduce lcm

    printfn "Button presses: %i" toHitRx
