module Common.Runner

open System
open System.Reflection

let private timeMethod (method: MethodInfo) =
    let sw = Diagnostics.Stopwatch.StartNew()
    method.Invoke(null, null) |> ignore
    Console.ForegroundColor <- ConsoleColor.DarkGray
    printfn "  (elapsed time: %.3fs)" (float sw.ElapsedMilliseconds / 1000.0)
    Console.ResetColor()

let private runOneDay (assembly: Assembly) day =
    let moduleName = sprintf "Day%i" day

    match assembly.GetType(moduleName) with
    | null -> ()
    | md ->
        printfn "\nDay %i\n" day

        match md.GetMethod("part1") with
        | null -> ()
        | method -> timeMethod method

        match md.GetMethod("part2") with
        | null -> ()
        | method -> timeMethod method

let runDay day =
    let assembly = Assembly.GetEntryAssembly()
    runOneDay assembly day

let runAllDays() =
    let assembly = Assembly.GetEntryAssembly()
    let sw = Diagnostics.Stopwatch.StartNew()

    for day in 1 .. 25 do
        runOneDay assembly day

    Console.ForegroundColor <- ConsoleColor.Green
    printfn "\nTotal elapsed time: %.3fs" (float sw.ElapsedMilliseconds / 1000.0)
    Console.ResetColor()
