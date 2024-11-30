module Day8

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day8.txt")

let codeLength (s: string) =
    s.Length

let memoryLength (s: string) =
    if s.Substring(0, 1) = "\"" && s.Substring(s.Length - 1, 1) = "\"" then
        let rec decode (acc: string) (rem: string) =
            if rem = "" then
                acc
            else
                let firstChar = rem.Substring(0, 1)
                if firstChar = @"\" then
                    if rem.Substring(1, 1) = "\\" then
                        decode (acc + "\\") (rem.Substring(2))
                    elif rem.Substring(1, 1) = "\"" then
                        decode (acc + "\"") (rem.Substring(2))
                    elif rem.Substring(1, 1) = "x" then
                        let asc = System.Convert.ToUInt32(rem.Substring(2, 2), 16)
                        let char = System.Convert.ToChar(asc).ToString()
                        decode (acc + char) (rem.Substring(4))
                    else
                        failwith "unexpected"
                else
                    decode (acc + firstChar) (rem.Substring(1))
        
        let decoded = decode "" (s.Substring(1, s.Length - 2))
        decoded.Length
    else
        failwith "unexpected"

let part1() =
    let parsed =
        input
        |> Array.map (fun s -> (codeLength s, memoryLength s))
    
    let difference =
        (parsed |> Array.sumBy fst) - (parsed |> Array.sumBy snd)
    
    printfn "Difference: %i" difference

let encodedLength (s: string) =
    let rec encode (acc: string) (rem: string) =
        if rem = "" then
            acc
        else
            let firstChar = rem.Substring(0, 1)
            if firstChar = @"\" then
                encode (acc + "\\\\") (rem.Substring(1))
            elif firstChar = "\"" then
                encode (acc + "\\\"") (rem.Substring(1))
            else
                encode (acc + firstChar) (rem.Substring(1))
    
    let encoded = "\"" + encode "" s + "\""
    encoded.Length

let part2() =
    let parsed =
        input
        |> Array.map (fun s -> (encodedLength s, codeLength s))
    
    let difference =
        (parsed |> Array.sumBy fst) - (parsed |> Array.sumBy snd)
    
    printfn "Difference: %i" difference
