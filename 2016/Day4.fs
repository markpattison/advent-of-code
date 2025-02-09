module Day4

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day4.txt")

type Room =
    {
        Name: string
        Sector: int
        Checksum: string
    }

let parseLine (s: string) =
    let checksum = s.Substring(s.Length - 6, 5)
    let parts = s.Substring(0, s.Length - 7).Split('-')
    let sector = parts.[parts.Length - 1] |> Int32.Parse
    let name = parts.[0 .. parts.Length - 2] |> String.Concat

    { Name = name; Sector = sector; Checksum = checksum }

let allRooms =
    input
    |> Array.map parseLine

let realRooms =
    let expectedChecksum (s: string) =
        s.ToCharArray()
        |> Array.countBy id
        |> Seq.sortBy fst
        |> Seq.sortByDescending snd
        |> Seq.map fst
        |> Seq.take 5
        |> String.Concat
    
    let isReal room =
        room.Checksum = expectedChecksum room.Name
    
    allRooms
    |> Array.filter isReal

let part1() =
    let sumRealSectors =
        realRooms
        |> Array.sumBy _.Sector
    
    printfn "Sum of real sectors: %i" sumRealSectors

let rotate n (c: char) =
    char (int 'a' + ((int c - int 'a') + n) % 26)

let rotateRoom n (s: string) =
    s.ToCharArray()
    |> Array.map (rotate n)
    |> String.Concat

let part2() =
    let sector =
        realRooms
        |> Array.map (fun room -> room.Sector, rotateRoom room.Sector room.Name)
        |> Array.find (fun (_, s) -> s = "northpoleobjectstorage")
        |> fst
    
    printfn "Sector: %i" sector
