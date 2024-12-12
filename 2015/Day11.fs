module Day11

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day11.txt")

let currentPassword = input.[0]

let containsStraightThree (s: string) =
    s.ToCharArray()
    |> Array.windowed 3
    |> Array.exists
        (function
            | [| a; b; c |] -> int b - int a = 1 && int c - int b = 1
            | _ -> false)

let doesNotContainIOL (s: string) =
    not (s.Contains('o') || s.Contains('i') || s.Contains('l'))

let hasDifferentPairs (s: string) =
    s.ToCharArray()
    |> Array.windowed 2
    |> Array.filter
        (function
        | [| a; b |] -> a = b
        | _ -> false)
    |> Array.distinct
    |> fun arr -> arr.Length > 1

let isValid (s: string) =
    containsStraightThree s
    && doesNotContainIOL s
    && hasDifferentPairs s

let increment (s: string) =
    let rec incChar n (s: string) =
        if n < 0 then
            String.replicate (s.Length) "a"
        else
            match s.[n] with
            | 'z' ->
                incChar (n - 1) (s |> String.mapi (fun i c -> if i = n then 'a' else c))
            | _ ->
                s |> String.mapi (fun i c -> if i = n then char (1 + int c) else c)
    incChar (s.Length - 1) s

let part1() =
    let mutable password = currentPassword
    
    while not (isValid password) do
        password <- increment password
    
    printfn "Next password: %s" password

let part2() =
    let mutable password = currentPassword
    
    while not (isValid password) do
        password <- increment password

    password <- increment password

    while not (isValid password) do
        password <- increment password    
    
    printfn "Next password: %s" password
