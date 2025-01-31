module Day22

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day22.txt")

type Spell = MagicMissile | Drain | Shield | Poison | Recharge

type GameState =
    {
        IsPlayerTurn: bool
        BossHitPoints: int
        BossDamage: int
        PlayerHitPoints: int
        PlayerMana: int
        ShieldDuration: int
        PoisonDuration: int
        RechargeDuration: int
        LossOnPlayerTurn: int
        ManaSpent: int
    }

let startingState =
    {
        IsPlayerTurn = true
        BossHitPoints = input.[0].Split(": ").[1] |> Int32.Parse
        BossDamage = input.[1].Split(": ").[1] |> Int32.Parse
        PlayerHitPoints = 50
        PlayerMana = 500
        ShieldDuration = 0
        PoisonDuration = 0
        RechargeDuration = 0
        LossOnPlayerTurn = 0
        ManaSpent = 0
    }

let spend mana state = { state with PlayerMana = state.PlayerMana - mana; ManaSpent = state.ManaSpent + mana }

let playerTurn state =
    let mutable updated = { state with PlayerHitPoints = state.PlayerHitPoints - state.LossOnPlayerTurn }
    
    if updated.PlayerHitPoints > 0 then
        if state.ShieldDuration > 0 then updated <- { updated with ShieldDuration = updated.ShieldDuration - 1 }
        if state.PoisonDuration > 0 then updated <- { updated with BossHitPoints = updated.BossHitPoints - 3; PoisonDuration = updated.PoisonDuration - 1 }
        if state.RechargeDuration > 0 then updated <- { updated with PlayerMana = updated.PlayerMana + 101; RechargeDuration = updated.RechargeDuration - 1 }

    { updated with IsPlayerTurn = false }

let castSpell spell state =
    match spell with
    | MagicMissile -> { state with BossHitPoints = state.BossHitPoints - 4 } |> spend 53
    | Drain -> { state with BossHitPoints = state.BossHitPoints - 2; PlayerHitPoints = state.PlayerHitPoints + 2 } |> spend 73
    | Shield -> { state with ShieldDuration = 6 } |> spend 113
    | Poison -> { state with PoisonDuration = 6 } |> spend 173
    | Recharge -> { state with RechargeDuration = 5 } |> spend 229

let bossTurn state =
    let mutable updated = state
    
    if state.ShieldDuration > 0 then updated <- { updated with ShieldDuration = updated.ShieldDuration - 1 }
    if state.PoisonDuration > 0 then updated <- { updated with BossHitPoints = updated.BossHitPoints - 3; PoisonDuration = updated.PoisonDuration - 1 }
    if state.RechargeDuration > 0 then updated <- { updated with PlayerMana = updated.PlayerMana + 101; RechargeDuration = updated.RechargeDuration - 1 }

    let playerDamage =
        if updated.BossHitPoints > 0 then
            if updated.ShieldDuration = 0 then updated.BossDamage else (updated.BossDamage - 7)
        else
            0 // boss already dead

    { updated with PlayerHitPoints = updated.PlayerHitPoints - playerDamage; IsPlayerTurn = true }

let availableSpells state =
    seq {
        if state.PlayerMana >= 53 then MagicMissile
        if state.PlayerMana >= 73 then Drain
        if state.ShieldDuration = 0 && state.PlayerMana >= 113 then Shield
        if state.PoisonDuration = 0 && state.PlayerMana >= 173 then Poison
        if state.RechargeDuration = 0 && state.PlayerMana >= 229 then Recharge
    } |> Seq.toList

let minSpend state =
    let mutable minSpendSoFar = Int32.MaxValue

    let win state =
        minSpendSoFar <- min minSpendSoFar state.ManaSpent
        Some state.ManaSpent

    let rec minWinningSpend state =
        if state.PlayerHitPoints <= 0 || state.ManaSpent >= minSpendSoFar then
            None
        elif state.BossHitPoints <= 0 then
            win state
        elif state.IsPlayerTurn then
            let afterPlayerTurn = playerTurn state
            if afterPlayerTurn.BossHitPoints <= 0 then
                win afterPlayerTurn
            else
                let options =
                    availableSpells afterPlayerTurn
                    |> List.choose (fun spell -> afterPlayerTurn |> castSpell spell |> minWinningSpend)
                if List.isEmpty options then None else Some (List.min options)
        else
            minWinningSpend (bossTurn state)
    
    minWinningSpend state

let part1() =
    let minSpend = (minSpend startingState).Value

    printfn "Minimum spend: %i" minSpend

let part2() =
    let hardState = { startingState with LossOnPlayerTurn = 1 }

    let minSpend = (minSpend hardState).Value

    printfn "Minimum spend: %i" minSpend
