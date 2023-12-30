module Main

open System
open System.Reflection

let header s = printfn "\n%s\n" s

let timeMethod (method: MethodInfo) =
    let sw = Diagnostics.Stopwatch.StartNew()
    method.Invoke(null, null) |> ignore
    Console.ForegroundColor <- ConsoleColor.DarkGray
    printfn "  (elapsed Time: %.3fs)" (float sw.ElapsedMilliseconds / 1000.0)
    Console.ResetColor()

let runDay day =
    let moduleName = sprintf "Day%i" day

    match Type.GetType(moduleName) with
    | null -> ()
    | md ->
        printfn "\nDay %i\n" day

        match md.GetMethod("part1") with
        | null -> ()
        | method -> timeMethod method

        match md.GetMethod("part2") with
        | null -> ()
        | method -> timeMethod method

// for day in 1 .. 25 do
//     runDay day

runDay 25
