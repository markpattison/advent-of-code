module Day20

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day20.txt")

type Vector =
    { X: int64; Y: int64; Z: int64 }
    static member (+) (v1: Vector, v2: Vector) =
        { X = v1.X + v2.X; Y = v1.Y + v2.Y; Z = v1.Z + v2.Z }

type Particle = { Position: Vector; Velocity: Vector; Acceleration: Vector }

let parseVector (s: string) =
    let parts = s.Split(',') |> Array.map Int64.Parse
    { X = parts.[0]; Y = parts.[1]; Z = parts.[2] }

let parseLine (s: string) =
    let parts =
        s.Split(", ")
        |> Array.map (fun p -> p.Substring(3, p.Length - 4))
        |> Array.map parseVector
    
    { Position = parts.[0]; Velocity = parts.[1]; Acceleration = parts.[2] }

let allParticles = input |> Array.map parseLine

let part1() =
    let longTermClosestParticle =
        allParticles
        |> Array.mapi (fun i p -> i, abs p.Acceleration.X + abs p.Acceleration.Y + abs p.Acceleration.Z)
        |> Array.minBy snd
        |> fst
    
    printfn "Long term closest particle: %i" longTermClosestParticle

let part2() =
    let updateParticle p =
        let newVelocity = p.Velocity + p.Acceleration
        { p with Position = p.Position + newVelocity; Velocity = newVelocity }
    
    let update particles =
        let newParticles = particles |> Array.map updateParticle
        let collisionPositions =
            newParticles
            |> Array.countBy _.Position
            |> Array.filter (fun (_, count) -> count > 1)
            |> Array.map fst
        let remainingParticles =
            newParticles
            |> Array.filter (fun p -> Array.contains p.Position collisionPositions |> not)
        remainingParticles
    
    let rec updateN particles n = if n = 0 then particles else updateN (update particles) (n - 1)

    let longTerm = updateN allParticles 1000

    printfn "Particles remaining: %i" longTerm.Length
