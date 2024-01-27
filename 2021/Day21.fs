module Day21

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day21.txt")

let startPosition1 = input.[0].Substring(input.[0].Length - 1) |> Int32.Parse
let startPosition2 = input.[1].Substring(input.[0].Length - 1) |> Int32.Parse

type PlayerState =
    {
        Position: int
        Score: int
    }

type GameState =
    {
        NextPlayer: PlayerState
        RecentPlayer: PlayerState
        DiceRolls: int
        NextRoll: int
    }

let nextRoll thisRoll =
    if thisRoll = 100 then 1 else thisRoll + 1

let loopPosition position = 1 + ((position - 1) % 10)

let playTurn state =
    let roll1 = state.NextRoll
    let roll2 = nextRoll roll1
    let roll3 = nextRoll roll2

    let newPosition = loopPosition (state.NextPlayer.Position + roll1 + roll2 + roll3)
    let newScore = state.NextPlayer.Score + newPosition

    {
        NextPlayer = state.RecentPlayer
        RecentPlayer = { Position = newPosition; Score = newScore }
        DiceRolls = state.DiceRolls + 3
        NextRoll = nextRoll roll3
    }

let part1() =
    let initialState =
        {
            NextPlayer = { Position = startPosition1; Score = 0 }
            RecentPlayer = { Position = startPosition2; Score = 0 }
            DiceRolls = 0
            NextRoll = 1
        }
    
    let mutable state = initialState

    while state.RecentPlayer.Score < 1000 do
        state <- playTurn state

    let product = state.NextPlayer.Score * state.DiceRolls

    printfn "Product: %i" product

type QuantumState =
    {
        NextPlayerId: int
        NextPlayer: PlayerState
        RecentPlayer: PlayerState
    }

type PlayerWins =
    {
        Player1Wins: int64
        Player2Wins: int64
    }
    static member (*) (n: int64, pw: PlayerWins) = { Player1Wins = n * pw.Player1Wins; Player2Wins = n * pw.Player2Wins }
    static member (+) (pw1: PlayerWins, pw2: PlayerWins) = { Player1Wins = pw1.Player1Wins + pw2.Player1Wins; Player2Wins = pw1.Player2Wins + pw2.Player2Wins }

let updateTurn state totalRolls =
    let newPosition = loopPosition (state.NextPlayer.Position + totalRolls)
    let newScore = state.NextPlayer.Score + newPosition

    {
        NextPlayerId = 3 - state.NextPlayerId
        NextPlayer = state.RecentPlayer
        RecentPlayer = { Position = newPosition; Score = newScore }
    }

let toWin = 21

let rec winsForEachPlayer state =
    if state.RecentPlayer.Score >= toWin then
        if state.NextPlayerId = 1 then
            { Player1Wins = 0L; Player2Wins = 1L }
        else
            { Player1Wins = 1L; Player2Wins = 0L }
    else
        winsForEachPlayer (updateTurn state 3)
        + 3L * winsForEachPlayer (updateTurn state 4)
        + 6L * winsForEachPlayer (updateTurn state 5)
        + 7L * winsForEachPlayer (updateTurn state 6)
        + 6L * winsForEachPlayer (updateTurn state 7)
        + 3L * winsForEachPlayer (updateTurn state 8)
        + winsForEachPlayer (updateTurn state 9)

let part2() =
    let initialState =
        {
            NextPlayerId = 1
            NextPlayer = { Position = startPosition1; Score = 0 }
            RecentPlayer = { Position = startPosition2; Score = 0 }
        }
    
    let wins = winsForEachPlayer initialState
    let maxWins = max wins.Player1Wins wins.Player2Wins

    printfn "Max wins: %i" maxWins
