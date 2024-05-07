module Day5

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day5.txt")

let hasThreeVowels (s: string) =
    let numVowels =
        s.ToCharArray()
        |> Array.filter (fun c -> c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u')
        |> Array.length
    
    numVowels >= 3

let hasRepeat (s: string) =
    s.ToCharArray()
    |> Array.pairwise
    |> Array.exists (fun (a, b) -> a = b)

let hasNoDisallowedSubstrings (s: string) =
    not (s.Contains("ab"))
    && not  (s.Contains("cd"))
    && not  (s.Contains("pq"))
    && not  (s.Contains("xy"))

let isNice s =
    hasThreeVowels s && hasRepeat s && hasNoDisallowedSubstrings s

let part1() =
    let nice =
        input
        |> Array.filter isNice
        |> Array.length

    printfn "Nice strings: %i" nice

let hasPairs (s: string) =
    let indices = seq {
        for i in 0 .. s.Length - 4 do
            for j in i + 2 .. s.Length - 2 do
                yield (i, j) }
    
    indices
    |> Seq.exists (fun (i, j) -> s.Substring(i, 2) = s.Substring(j, 2))

let hasRepeatWithLetterBetween (s: string) =
    let indices = seq { 0 .. s.Length - 3 }

    indices
    |> Seq.exists (fun i -> s.Substring(i, 1) = s.Substring(i + 2, 1))

let isNice2 s =
    hasPairs s && hasRepeatWithLetterBetween s

let part2() =
    let nice =
        input
        |> Array.filter isNice2
        |> Array.length

    printfn "Nice strings: %i" nice
