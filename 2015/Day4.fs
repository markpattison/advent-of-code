module Day4

open System
open System.IO
open System.Security
open System.Text

let input =
    File.ReadAllLines(@"input\day4.txt")

let secretKey = input.[0]

let findFirstStartingWith (startText: string) =
    let md5 = Cryptography.MD5.Create()
    let checkNum (n: int) =
        let text = secretKey + n.ToString()
        let bytes = md5.ComputeHash(Encoding.UTF8.GetBytes(text))
        let hash = String.Join("", bytes |> Array.map (fun b -> b.ToString("x2")))
        hash.StartsWith(startText)

    Seq.initInfinite id
    |> Seq.findIndex checkNum

let part1() =
    let firstNum = findFirstStartingWith "00000"
    
    printfn "First number: %i" firstNum

let part2() =
    let firstNum = findFirstStartingWith "000000"
    
    printfn "First number: %i" firstNum
