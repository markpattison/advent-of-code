module Day20

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day20.txt")

let parseChar c =
    match c with
    | '#' -> 1
    | '.' -> 0
    | _ -> failwith "unexpected char"

let imageEnhancement =
    input.[0].ToCharArray()
    |> Array.map parseChar

let initialImage =
    let xLength = input.[2].Length
    let yLength = input.Length - 2
    Array2D.init xLength yLength (fun x y -> input.[y + 2].[x] |> parseChar)

let getPixel image background x y =
    if x >= 0 && x < Array2D.length1 image && y >= 0 && y < Array2D.length2 image then
        image.[x, y]
    else
        background

let ninePixels image x y background =
    let p = getPixel image background
    256 *   p (x - 1) (y - 1)
    + 128 * p  x      (y - 1)
    + 64 *  p (x + 1) (y - 1)
    + 32 *  p (x - 1)  y
    + 16 *  p  x       y
    + 8 *   p (x + 1)  y
    + 4 *   p (x - 1) (y + 1)
    + 2 *   p  x      (y + 1)
    +       p (x + 1) (y + 1)

let enhance (image, background) =
    let xLength = 2 + Array2D.length1 image
    let yLength = 2 + Array2D.length2 image
    
    let enhanced = Array2D.init xLength yLength (fun x y -> imageEnhancement.[ninePixels image (x - 1) (y - 1) background])
    let newBackground = imageEnhancement.[if background = 0 then 0 else 511]

    enhanced, newBackground

let repeatedEnhance image background numSteps =
    let mutable enhanced = (image, background)

    for _ in 1 .. numSteps do
        enhanced <- enhance enhanced
    
    fst enhanced

let part1() =
    let enhanced = repeatedEnhance initialImage 0 2

    let totalLit = enhanced |> Seq.cast<int> |> Seq.sum

    printfn "Total lit: %i" totalLit

let part2() =
    let enhanced = repeatedEnhance initialImage 0 50

    let totalLit = enhanced |> Seq.cast<int> |> Seq.sum

    printfn "Total lit: %i" totalLit
