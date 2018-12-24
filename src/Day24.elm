module Day24 exposing (Army, ArmyType(..), DamageType(..), Group, allUnits, applyBoost, dieIfDead, exampleUnits, fightToTheDeath, findSmallestWinningBoost, findTargeting, findTargetsHelper, getDamageToTarget, getEffectivePower, immuneSystemArmy, infectionArmy, inputText, makeArmiesFight, part1, part2, performAttack, scorePart1, shouldKeepFighting, youAreAttacked)

import Dict exposing (Dict)
import Dict.Extra
import List.Extra


type alias Army =
    List Group


type alias Group =
    { units : Int
    , hpPerUnit : Int
    , weaknesses : List DamageType
    , immunities : List DamageType
    , attackPower : Int
    , attackType : DamageType
    , initiative : Int
    , allegiance : ArmyType
    }


type ArmyType
    = Reindeer
    | Infection


type DamageType
    = Radiation
    | Fire
    | Cold
    | Bludgeoning
    | Slashing


immuneSystemArmy =
    [ Group 337 6482 [ Radiation, Fire ] [ Cold, Bludgeoning ] 189 Slashing 15 Reindeer
    , Group 571 3178 [ Fire ] [] 47 Slashing 12 Reindeer
    , Group 116 7940 [] [] 638 Fire 18 Reindeer
    , Group 6017 9349 [ Cold ] [] 14 Cold 6 Reindeer
    , Group 2246 4002 [ Radiation, Slashing ] [] 16 Cold 3 Reindeer
    , Group 3950 4493 [ Bludgeoning ] [ Radiation, Fire ] 10 Radiation 8 Reindeer
    , Group 7494 1141 [] [ Bludgeoning ] 1 Cold 17 Reindeer
    , Group 2501 9007 [] [] 35 Cold 7 Reindeer
    , Group 844 3222 [] [ Bludgeoning, Slashing ] 37 Radiation 9 Reindeer
    , Group 1371 3695 [] [ Cold ] 25 Cold 10 Reindeer
    ]


infectionArmy =
    [ Group 2295 16577 [] [ Radiation ] 12 Fire 13 Infection
    , Group 837 6736 [ Fire ] [] 14 Radiation 19 Infection
    , Group 2841 9360 [ Radiation, Cold ] [ Bludgeoning ] 6 Fire 14 Infection
    , Group 7374 51597 [ Cold ] [ Bludgeoning, Fire ] 12 Radiation 1 Infection
    , Group 1544 29226 [ Fire, Bludgeoning ] [] 35 Bludgeoning 5 Infection
    , Group 293 13961 [ Radiation ] [ Slashing ] 79 Radiation 2 Infection
    , Group 1219 38142 [] [ Cold, Fire ] 53 Bludgeoning 4 Infection
    , Group 5233 30595 [ Bludgeoning, Cold ] [ Fire ] 11 Radiation 11 Infection
    , Group 397 43710 [ Slashing, Radiation ] [ Cold, Bludgeoning ] 171 Slashing 16 Infection
    , Group 1316 36203 [ Slashing, Bludgeoning ] [] 50 Cold 20 Infection
    ]


allUnits =
    immuneSystemArmy ++ infectionArmy


exampleUnits =
    [ Group 17 5390 [ Radiation, Bludgeoning ] [] 4507 Fire 2 Reindeer
    , Group 989 1274 [ Bludgeoning, Slashing ] [ Fire ] 25 Slashing 3 Reindeer
    , Group 801 4706 [ Radiation ] [] 116 Bludgeoning 1 Infection
    , Group 4485 2961 [ Fire, Cold ] [ Radiation ] 12 Slashing 4 Infection
    ]


fightToTheDeath : Dict Int Group -> Dict Int Group -> Dict Int Group
fightToTheDeath lastUnits units =
    if shouldKeepFighting lastUnits units then
        fightToTheDeath units (makeArmiesFight units)

    else
        units


shouldKeepFighting lastUnits units =
    let
        allegiances =
            Dict.values units |> List.map .allegiance
    in
    List.member Reindeer allegiances
        && List.member Infection allegiances
        && (lastUnits /= units)


makeArmiesFight : Dict Int Group -> Dict Int Group
makeArmiesFight unitDict =
    let
        targetingList =
            findTargeting unitDict
    in
    List.foldl performAttack unitDict targetingList


getEffectivePower unit =
    unit.units * unit.attackPower


findTargeting : Dict Int Group -> List ( Int, Int )
findTargeting unitDict =
    let
        unitsByEffectivePower =
            Dict.values unitDict
                |> List.sortBy getEffectivePower
                |> List.reverse
    in
    findTargetsHelper unitsByEffectivePower unitsByEffectivePower []



-- |> Debug.log "targets"


findTargetsHelper aliveUnits unitsLeft targeting =
    case unitsLeft of
        [] ->
            List.sortBy Tuple.first targeting |> List.reverse

        next :: rest ->
            let
                alreadyTargeted =
                    List.map Tuple.second targeting

                bestTarget =
                    aliveUnits
                        |> List.filter (\u -> u.allegiance /= next.allegiance && getDamageToTarget next u /= 0 && not (List.member u.initiative alreadyTargeted))
                        |> List.sortBy (\t -> ( getDamageToTarget next t, getEffectivePower t, t.initiative ))
                        |> List.reverse
                        |> List.head
            in
            case bestTarget of
                Nothing ->
                    findTargetsHelper aliveUnits rest targeting

                Just target ->
                    findTargetsHelper aliveUnits rest (( next.initiative, target.initiative ) :: targeting)


getDamageToTarget : Group -> Group -> Int
getDamageToTarget fighter target =
    if List.member fighter.attackType target.immunities then
        0

    else if List.member fighter.attackType target.weaknesses then
        getEffectivePower fighter * 2

    else
        getEffectivePower fighter


performAttack : ( Int, Int ) -> Dict Int Group -> Dict Int Group
performAttack ( attackerInit, defenderInit ) unitDict =
    case Dict.get attackerInit unitDict of
        Nothing ->
            unitDict

        Just attacker ->
            Dict.update defenderInit (youAreAttacked attacker) unitDict


youAreAttacked : Group -> Maybe Group -> Maybe Group
youAreAttacked attacker mDefender =
    case mDefender of
        Nothing ->
            Nothing

        Just defender ->
            -- let
            --     _ =
            --         Debug.log "attacker" attacker
            --     _ =
            --         Debug.log "defender" defender
            --     _ =
            --         Debug.log "units killed" (getDamageToTarget attacker defender // defender.hpPerUnit)
            -- in
            { defender | units = defender.units - (getDamageToTarget attacker defender // defender.hpPerUnit) }
                |> dieIfDead


dieIfDead defender =
    if defender.units <= 0 then
        Nothing

    else
        Just defender


part1 units =
    units
        |> Dict.Extra.fromListBy .initiative
        |> fightToTheDeath Dict.empty
        |> scorePart1


scorePart1 units =
    Dict.values units |> List.map .units |> List.sum


part2 units =
    let
        unitDict =
            units
                |> Dict.Extra.fromListBy .initiative

        boostAmount =
            unitDict
                |> findSmallestWinningBoost 95
    in
    unitDict |> applyBoost boostAmount |> fightToTheDeath Dict.empty |> scorePart1


findSmallestWinningBoost boost unitDict =
    let
        _ =
            Debug.log "evaluating boost" boost

        survivingUnits =
            unitDict
                |> applyBoost boost
                |> fightToTheDeath Dict.empty

        -- |> Debug.log "survivors"
    in
    case Dict.values survivingUnits |> List.all (.allegiance >> (==) Reindeer) of
        True ->
            boost

        False ->
            findSmallestWinningBoost (boost + 1) unitDict


applyBoost boost unitDict =
    Dict.map
        (\k group ->
            { group
                | attackPower =
                    if group.allegiance == Reindeer then
                        group.attackPower + boost

                    else
                        group.attackPower
            }
        )
        unitDict


inputText =
    """Immune System:
337 units each with 6482 hit points (weak to radiation, fire; immune to cold, bludgeoning) with an attack that does 189 slashing damage at initiative 15
571 units each with 3178 hit points (weak to fire) with an attack that does 47 slashing damage at initiative 12
116 units each with 7940 hit points with an attack that does 638 fire damage at initiative 18
6017 units each with 9349 hit points (weak to cold) with an attack that does 14 cold damage at initiative 6
2246 units each with 4002 hit points (weak to radiation, slashing) with an attack that does 16 cold damage at initiative 3
3950 units each with 4493 hit points (weak to bludgeoning; immune to radiation, fire) with an attack that does 10 radiation damage at initiative 8
7494 units each with 1141 hit points (immune to bludgeoning) with an attack that does 1 cold damage at initiative 17
2501 units each with 9007 hit points with an attack that does 35 cold damage at initiative 7
844 units each with 3222 hit points (immune to bludgeoning, slashing) with an attack that does 37 radiation damage at initiative 9
1371 units each with 3695 hit points (immune to cold) with an attack that does 25 cold damage at initiative 10

Infection:
2295 units each with 16577 hit points (immune to radiation) with an attack that does 12 fire damage at initiative 13
837 units each with 6736 hit points (weak to fire) with an attack that does 14 radiation damage at initiative 19
2841 units each with 9360 hit points (immune to bludgeoning; weak to radiation, cold) with an attack that does 6 fire damage at initiative 14
7374 units each with 51597 hit points (weak to cold; immune to bludgeoning, fire) with an attack that does 12 radiation damage at initiative 1
1544 units each with 29226 hit points (weak to fire, bludgeoning) with an attack that does 35 bludgeoning damage at initiative 5
293 units each with 13961 hit points (immune to slashing; weak to radiation) with an attack that does 79 radiation damage at initiative 2
1219 units each with 38142 hit points (immune to cold, fire) with an attack that does 53 bludgeoning damage at initiative 4
5233 units each with 30595 hit points (weak to bludgeoning, cold; immune to fire) with an attack that does 11 radiation damage at initiative 11
397 units each with 43710 hit points (weak to slashing, radiation; immune to cold, bludgeoning) with an attack that does 171 slashing damage at initiative 16
1316 units each with 36203 hit points (weak to slashing, bludgeoning) with an attack that does 50 cold damage at initiative 20"""
