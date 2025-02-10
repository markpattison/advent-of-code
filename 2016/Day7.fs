module Day7

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day7.txt")

type Address = Out | In

let first (s: string) =
    if s.Length = 0 then
        None
    else
        s.Substring(0, 1) |> Some

let parseAddress (address: string) =
    let rec parse acc currState currString rem =
        match currState, first rem with
        | In, None ->
            failwith "unexpected"
        | Out, None ->
            List.rev ((Out, currString) :: acc)
        | In, Some "[" ->
            failwith "unexpected"
        | Out, Some "[" ->
            parse ((Out, currString) :: acc) In "" (rem.Substring(1))
        | In, Some "]" ->
            parse ((In, currString) :: acc) Out "" (rem.Substring(1))
        | _, Some s ->
            parse acc currState (currString + s) (rem.Substring(1))
    
    parse [] Out "" address

let containsAbba (s: string) =
    let hasAbba chars =
        match chars with
        | [| c1; c2; c3; c4 |] when c1 = c4 && c2 = c3 && c1 <> c2 -> true
        | _ -> false
    
    if s.Length < 4 then
        false
    else
        s.ToCharArray()
        |> Array.windowed 4
        |> Array.exists hasAbba

let supportsTls addressParts =
    addressParts |> List.exists (fun (a, s) -> a = Out && containsAbba s)
    && addressParts |> List.exists (fun (a, s) -> a = In && containsAbba s) |> not

let part1() =
    let numSupporting =
        input
        |> Array.filter (parseAddress >> supportsTls)
        |> Array.length
    
    printfn "Number supporting TLS: %i" numSupporting

let containedAbas (s: string) =
    let hasAba chars =
        match chars with
        | [| c1; c2; c3 |] when c1 = c3 && c1 <> c2 -> true
        | _ -> false
    
    if s.Length < 3 then
        [||]
    else
        s.ToCharArray()
        |> Array.windowed 3
        |> Array.filter hasAba

let abasCorrespond (chars1: char[]) (chars2: char[]) =
    chars1.[0] = chars2.[1] && chars1.[1] = chars2.[0]

let supportsSsl addressParts =
    let abas =
        addressParts
        |> List.map (fun (a, s) -> a, containedAbas s)
    
    let outAbas =
        abas
        |> List.filter (fun (a, _) -> a = Out)
        |> List.map snd
        |> Seq.concat
    
    let inAbas =
        abas
        |> List.filter (fun (a, _) -> a = In)
        |> List.map snd
        |> Seq.concat
    
    let x =
        outAbas
        |> Seq.exists (fun chars1 -> inAbas |> Seq.exists (fun chars2 -> abasCorrespond chars1 chars2))
    x

let part2() =
    let numSupporting =
        input
        |> Array.filter (parseAddress >> supportsSsl)
        |> Array.length
    
    printfn "Number supporting SSL: %i" numSupporting
