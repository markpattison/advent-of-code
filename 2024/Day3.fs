module Day3

open System
open System.IO

let input =
    File.ReadAllText(@"input\day3.txt")

let initialDigits (s: string) =
    let mutable found = 0
    let mutable finished = false

    while (not finished) && found < s.Length do
        if Char.IsDigit(s.[found]) then
            found <- found + 1
        else
            finished <- true
    
    found

let numIfValid (s: string) =
    match initialDigits s with
    | 0 -> None
    | n -> Some(Int32.Parse(s.Substring(0, n)), s.Substring(n))

let productIfValid (s: string) =
    if s.Length >= 2 && s.[0] = '(' then
        match numIfValid (s.Substring(1)) with
        | None -> None
        | Some (n1, rem1) ->
            if rem1.[0] = ',' then
                match numIfValid (rem1.Substring(1)) with
                | None -> None
                | Some (n2, rem2) ->
                    if rem2.Length >= 1 && rem2.[0] = ')' then
                        Some (n1 * n2)
                    else
                        None
            else
                None
    else
        None

let part1() =
    let sum =
        input.Split("mul")
        |> Array.choose productIfValid
        |> Array.sum

    printfn "Sum: %i" sum

let removeAfterDont (s: string) =
    let mutable enabled = true
    let sb = new System.Text.StringBuilder()

    for i in 0 .. (s.Length - 1) do
        if i <= s.Length - 7 && s.Substring(i, 7) = "don't()" then
            enabled <- false
        elif i <= s.Length - 4 && s.Substring(i, 4) = "do()" then
            enabled <- true
        if enabled then
            sb.Append(s.Substring(i, 1)) |> ignore
    
    sb.ToString()

let part2() =
    let x = removeAfterDont input
    let sum =
        input
        |> removeAfterDont
        |> fun s -> s.Split("mul")
        |> Array.choose productIfValid
        |> Array.sum

    printfn "Sum: %i" sum