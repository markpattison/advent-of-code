module Day5

open System
open System.IO
open System.Security
open System.Text

let input =
    File.ReadAllLines(@"input\day5.txt")

let doorId = input.[0]

let md5 = Cryptography.MD5.Create() 

let toHash (n: int) =
    let text = doorId + n.ToString()
    let bytes = md5.ComputeHash(Encoding.UTF8.GetBytes(text))
    String.Join("", bytes |> Array.map (fun b -> b.ToString("x2")))

let part1() =
    let password =
        Seq.initInfinite id
        |> Seq.map toHash
        |> Seq.filter (fun s -> s.StartsWith("00000"))
        |> Seq.map (fun s -> s.Substring(5, 1))
        |> Seq.take 8
        |> String.Concat
    
    printfn "Password: %s" password

let part2() =
    let passChars = Array.create 8 ' '
    let mutable i = 0

    while Array.contains ' ' passChars do
        let hash = toHash i
        if hash.StartsWith("00000") then
            match Int32.TryParse(hash.Substring(5, 1)) with
            | true, pos ->
                if pos < 8 && passChars.[pos] = ' ' then passChars.[pos] <- hash.ToCharArray().[6]
            | _ -> ()

        i <- i + 1
    
    let password = String.Concat passChars
    
    printfn "Password: %s" password
