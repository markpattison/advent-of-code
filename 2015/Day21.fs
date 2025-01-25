module Day21

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day21.txt")

type Character =
    {
        HitPoints: int
        Damage: int
        Armor: int
    }

type Item =
    {
        Cost: int
        Damage: int
        Armor: int
    }

let startingBoss =
    {
        HitPoints = input.[0].Split(": ").[1] |> Int32.Parse
        Damage = input.[1].Split(": ").[1] |> Int32.Parse
        Armor = input.[2].Split(": ").[1] |> Int32.Parse
    }

let weapons =
    [
        { Cost = 8; Damage = 4; Armor = 0 }
        { Cost = 10; Damage = 5; Armor = 0 }
        { Cost = 25; Damage = 6; Armor = 0 }
        { Cost = 40; Damage = 7; Armor = 0 }
        { Cost = 74; Damage = 8; Armor = 0 }
    ]

let armor =
    [
        { Cost = 13; Damage = 0; Armor = 1 }
        { Cost = 31; Damage = 0; Armor = 2 }
        { Cost = 53; Damage = 0; Armor = 3 }
        { Cost = 75; Damage = 0; Armor = 4 }
        { Cost = 102; Damage = 0; Armor = 5 }
    ]

let rings =
    [
        { Cost = 25; Damage = 1; Armor = 0 }
        { Cost = 50; Damage = 2; Armor = 0 }
        { Cost = 100; Damage = 3; Armor = 0 }
        { Cost = 20; Damage = 0; Armor = 1 }
        { Cost = 40; Damage = 0; Armor = 2 }
        { Cost = 80; Damage = 0; Armor = 3 }
    ]

let damaged (attacker: Character) defender =
    { defender with HitPoints = defender.HitPoints - max 1 (attacker.Damage - defender.Armor) }

let doesWin startingPlayer =
    let mutable boss = startingBoss
    let mutable player = startingPlayer
    let mutable isPlayerTurn = true

    while boss.HitPoints > 0 && player.HitPoints > 0 do
        if isPlayerTurn then
            boss <- damaged player boss
        else
            player <- damaged boss player
        isPlayerTurn <- not isPlayerTurn
    
    player.HitPoints > 0

let ringCombinations =
    seq {
        yield []
        for ring in rings do
            yield [ ring ]
        for ring1 in rings do
            for ring2 in rings do
                if ring1 <> ring2 then yield [ ring1; ring2 ]
    }

let armorCombinations =
    seq {
        yield []
        for armo in armor do
            yield [ armo ]
    }

let weaponCombinations =
    seq {
        for weapon in weapons do
            yield [ weapon ]
    }

let allCombinations =
    seq {
        for ringCo in ringCombinations do
            for armorCo in armorCombinations do
                for weaponCo in weaponCombinations do
                    yield List.concat [ ringCo; armorCo; weaponCo ]
    }

let upgradedPlayer items =
    let cost = items |> List.sumBy _.Cost
    {
        HitPoints = 100
        Damage = items |> List.sumBy _.Damage
        Armor = items |> List.sumBy _.Armor
    }, cost

let part1() =
    let cheapestWin =
        allCombinations
        |> Seq.map upgradedPlayer
        |> Seq.map (fun (player, cost) -> doesWin player, cost)
        |> Seq.filter fst
        |> Seq.map snd
        |> Seq.min
    
    printfn "Cheapest win: %i" cheapestWin

let part2() =
    let mostExpensiveLoss =
        allCombinations
        |> Seq.map upgradedPlayer
        |> Seq.map (fun (player, cost) -> doesWin player, cost)
        |> Seq.filter (fst >> not)
        |> Seq.map snd
        |> Seq.max
    
    printfn "Most expensive loss: %i" mostExpensiveLoss
