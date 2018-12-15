module Day15 exposing (Fighter, Race(..), State, StepResult(..), Terrain, WallOrFloor(..), attackPower, damageAtPosition, displayFighter, exampleText1, exampleText2, exampleText3, exampleText4, exampleText5, exampleText6, findAllSurroundingPositions, findBestNextStep, findMoreDistances, getDistanceToTargetWithReadingTieBreaker, getReadingOrder, inputText, isNotOccupied, isOnFloor, moveDown, moveLeft, moveRight, moveUp, movementExample2, parseFighters, parseFightersChar, parseFightersLine, parseState, parseTerrain, parseTerrainLine, part1, posToReadingOrder, recursePart1, scorePart1, stepState, stepXTimes, takeNextTurn, timeToFight, toPosition, visualizeState, visualizeTerrainLine, visualizeTerrainPoint)

import Array exposing (Array)
import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)


type alias Terrain =
    Array (Array WallOrFloor)


type WallOrFloor
    = Wall
    | Floor


type Race
    = Elf
    | Goblin


type alias Fighter =
    { race : Race
    , x : Int
    , y : Int
    , health : Int
    }


attackPower =
    3


getAttackPowerForRace race =
    case race of
        Goblin ->
            3

        Elf ->
            23


type alias State =
    { terrain : Terrain
    , fighters : List Fighter
    , completedTurns : Int
    }


parseState : String -> State
parseState text =
    { terrain = parseTerrain text
    , fighters = parseFighters text
    , completedTurns = 0
    }


parseTerrain text =
    String.lines text
        |> List.map (String.toList >> parseTerrainLine)
        |> Array.fromList


parseTerrainLine : List Char -> Array WallOrFloor
parseTerrainLine chars =
    List.map
        (\char ->
            if char == '#' then
                Wall

            else
                Floor
        )
        chars
        |> Array.fromList


parseFighters text =
    String.lines text
        |> List.indexedMap parseFightersLine
        |> List.concat


parseFightersLine y line =
    String.toList line
        |> List.indexedMap (parseFightersChar y)
        |> List.filterMap identity


parseFightersChar : Int -> Int -> Char -> Maybe Fighter
parseFightersChar y x char =
    case char of
        'G' ->
            Just { race = Goblin, x = x, y = y, health = 200 }

        'E' ->
            Just { race = Elf, x = x, y = y, health = 200 }

        _ ->
            Nothing


part1 text =
    text |> parseState |> recursePart1 |> scorePart1


scorePart1 : State -> Int
scorePart1 state =
    let
        _ =
            Debug.log "completed Turns" state.completedTurns

        totalHealth =
            state.fighters |> List.map .health |> List.sum |> Debug.log "total health"
    in
    state.completedTurns * totalHealth


recursePart1 : State -> State
recursePart1 state =
    case stepState state of
        Finished newState ->
            newState

        Ongoing newState ->
            recursePart1 newState



-- I solved part2 by manually tweaking the getAttackPowerForRace function
--  and seeing when all ten elves survived using
--  `stepXTimes 65 (parseState inputText) |> scorePart1`


type StepResult
    = Finished State
    | Ongoing State


stepXTimes : Int -> State -> State
stepXTimes stepsLeft state =
    if stepsLeft <= 0 then
        visualizeState state

    else
        case stepState state of
            Finished newState ->
                visualizeState newState

            Ongoing newState ->
                stepXTimes (stepsLeft - 1) (visualizeState newState)


stepState : State -> StepResult
stepState state =
    let
        orderedFighters =
            List.sortBy getReadingOrder state.fighters
    in
    takeNextTurn orderedFighters [] state.terrain state.completedTurns


takeNextTurn : List Fighter -> List Fighter -> Terrain -> Int -> StepResult
takeNextTurn waitingFighters completedFighters terrain completedTurns =
    case waitingFighters of
        [] ->
            Ongoing { terrain = terrain, fighters = completedFighters, completedTurns = completedTurns + 1 }

        fighter :: restOfWaiting ->
            let
                targets =
                    List.filter (\f -> f.race /= fighter.race) (restOfWaiting ++ completedFighters)
            in
            case targets of
                [] ->
                    Finished { terrain = terrain, fighters = waitingFighters ++ completedFighters, completedTurns = completedTurns }

                _ ->
                    let
                        adjacent =
                            findAllSurroundingPositions (toPosition fighter)
                                |> Set.fromList

                        targetPositionSet =
                            List.map toPosition targets |> Set.fromList

                        ( nextX, nextY ) =
                            if Set.isEmpty (Set.intersect adjacent targetPositionSet) then
                                findBestNextStep terrain
                                    (toPosition fighter)
                                    (List.map toPosition (restOfWaiting ++ completedFighters) |> Set.fromList)
                                    (List.map toPosition targets)

                            else
                                toPosition fighter

                        ( newWaiting, newCompleted ) =
                            timeToFight
                                restOfWaiting
                                { fighter | x = nextX, y = nextY }
                                completedFighters
                    in
                    takeNextTurn newWaiting newCompleted terrain completedTurns


getReadingOrder fighter =
    (fighter.y * 1000) + fighter.x


posToReadingOrder ( x, y ) =
    (y * 1000) + x


toPosition fighter =
    ( fighter.x, fighter.y )


findBestNextStep terrain ourPosition allUnitPositions targetPositionList =
    let
        _ =
            logIfISaySo "finding step for " ourPosition

        allTargetPositions =
            targetPositionList
                |> List.concatMap findAllSurroundingPositions
                |> List.filter (\pos -> isOnFloor terrain pos && isNotOccupied pos allUnitPositions)

        bestTarget =
            findBestTarget terrain allUnitPositions allTargetPositions [ ourPosition ] Set.empty

        knownDistances =
            case bestTarget of
                Just target ->
                    -- We don't actually need to use this now.
                    -- We could just use `findBestTarget` again, but with our target as the source
                    --  and our filtered adjacent positions as the targets.
                    findMoreDistances terrain ourPosition allUnitPositions (Dict.singleton target 0) Dict.empty 0

                Nothing ->
                    Dict.empty

        _ =
            List.range 15 17
                |> List.concatMap (\x -> List.range 21 28 |> List.map (Tuple.pair x))
                |> List.map (\( x, y ) -> logIfISaySo (String.fromInt x ++ "," ++ String.fromInt y) (Dict.get ( x, y ) knownDistances))

        reachablePositions =
            Dict.keys knownDistances |> Set.fromList

        logIfISaySo message thing =
            if ourPosition == ( 5, 220 ) then
                Debug.log message thing

            else
                thing
    in
    [ moveUp ourPosition
    , moveLeft ourPosition
    , moveRight ourPosition
    , moveDown ourPosition
    ]
        |> List.filter (\pos -> isOnFloor terrain pos && isNotOccupied pos allUnitPositions && Set.member pos reachablePositions)
        |> List.sortBy (getDistanceToTargetWithReadingTieBreaker knownDistances)
        |> logIfISaySo "ranked steps"
        |> List.head
        |> Maybe.withDefault ourPosition


getDistanceToTargetWithReadingTieBreaker distances pos =
    ( Dict.get pos distances |> Maybe.withDefault 1000000, posToReadingOrder pos )


moveUp ( x, y ) =
    ( x, y - 1 )


moveLeft ( x, y ) =
    ( x - 1, y )


moveRight ( x, y ) =
    ( x + 1, y )


moveDown ( x, y ) =
    ( x, y + 1 )


isOnFloor terrain ( x, y ) =
    Array.get y terrain
        |> Maybe.andThen (Array.get x)
        |> Maybe.withDefault Wall
        |> (==) Floor


isNotOccupied pos unitPositions =
    not <| Set.member pos unitPositions


findBestTarget terrain allUnitPositions allTargetPositionsList frontierList interiorSet =
    let
        newInteriorPositions =
            Set.union interiorSet (Set.fromList frontierList)

        newFrontierPositions =
            frontierList
                |> List.concatMap findAllSurroundingPositions
                |> List.Extra.unique
                |> List.filter (\pos -> isOnFloor terrain pos && isNotOccupied pos allUnitPositions && isNotOccupied pos newInteriorPositions)

        foundTargets =
            allTargetPositionsList |> List.filter (\pos -> List.member pos newFrontierPositions)
    in
    case foundTargets of
        [] ->
            case newFrontierPositions of
                [] ->
                    Nothing

                _ ->
                    findBestTarget terrain allUnitPositions allTargetPositionsList newFrontierPositions newInteriorPositions

        _ ->
            foundTargets
                |> List.sortBy posToReadingOrder
                |> List.head


findMoreDistances terrain ourPosition allUnitPositions frontierSpaces interiorSpaces distance =
    let
        newInteriorPositions =
            Dict.keys interiorSpaces ++ Dict.keys frontierSpaces |> Set.fromList

        _ =
            logIfISaySo "distance" distance

        newFrontierPositions =
            Dict.keys frontierSpaces
                |> List.concatMap findAllSurroundingPositions
                |> List.Extra.unique
                |> List.filter (\pos -> isOnFloor terrain pos && isNotOccupied pos allUnitPositions && isNotOccupied pos newInteriorPositions)

        _ =
            newFrontierPositions
                |> List.filter (\( x, y ) -> y > 20 && y < 24 && x > 4 && x < 20)
                |> logIfISaySo "new frontiers"

        newFrontierSpaces =
            newFrontierPositions
                |> List.map (\pos -> ( pos, distance ))
                |> Dict.fromList

        newInterior =
            Dict.union interiorSpaces frontierSpaces

        logIfISaySo message thing =
            if ourPosition == ( 5, 220 ) then
                Debug.log message thing

            else
                thing
    in
    if Dict.member ourPosition newInterior then
        Dict.union newInterior newFrontierSpaces

    else
        case newFrontierPositions of
            [] ->
                newInterior

            _ ->
                findMoreDistances terrain ourPosition allUnitPositions newFrontierSpaces newInterior (distance + 1)


findAllSurroundingPositions pos =
    [ moveUp pos
    , moveLeft pos
    , moveRight pos
    , moveDown pos
    ]



-- FIGHTING


timeToFight : List Fighter -> Fighter -> List Fighter -> ( List Fighter, List Fighter )
timeToFight waitingFighters fighter completedFighters =
    let
        adjacent =
            findAllSurroundingPositions (toPosition fighter)

        targetPosition =
            List.filter (\f -> f.race /= fighter.race) (waitingFighters ++ completedFighters)
                |> List.filter (\f -> List.member (toPosition f) adjacent)
                |> List.sortBy (\f -> ( f.health, getReadingOrder f ))
                |> List.head
                |> Maybe.map toPosition

        applyDamageToList fighterList =
            case targetPosition of
                Just targetPos ->
                    List.map (damageAtPosition (getAttackPowerForRace fighter.race) targetPos) fighterList
                        |> List.filter (\f -> f.health > 0)

                Nothing ->
                    fighterList

        newWaiting =
            applyDamageToList waitingFighters

        newCompleted =
            fighter :: applyDamageToList completedFighters
    in
    ( newWaiting, newCompleted )


damageAtPosition damage targetPos fighter =
    if targetPos == toPosition fighter then
        { fighter | health = fighter.health - damage }

    else
        fighter



-- VISUALIZATION


visualizeState state =
    let
        _ =
            Array.toList state.terrain
                |> List.indexedMap (visualizeTerrainLine state.fighters)
                |> String.join "\n"
                |> (\viz -> Debug.log ("\n" ++ viz) state.completedTurns)
    in
    state


visualizeTerrainLine fighters y line =
    let
        terrainDisplay =
            Array.toList line
                |> List.indexedMap (visualizeTerrainPoint fighters y)
                |> String.join ""

        fightersDisplay =
            List.filter (\f -> f.y == y) fighters
                |> List.sortBy .x
                |> List.map displayFighter
                |> String.join ""
    in
    terrainDisplay ++ fightersDisplay


visualizeTerrainPoint fighters y x wallOrFloor =
    case wallOrFloor of
        Wall ->
            "#"

        Floor ->
            case fighters |> List.filter (\f -> f.x == x && f.y == y) |> List.head of
                Nothing ->
                    "."

                Just fighter ->
                    case fighter.race of
                        Elf ->
                            "E"

                        Goblin ->
                            "G"


displayFighter fighter =
    let
        raceDisplay =
            case fighter.race of
                Elf ->
                    "E"

                Goblin ->
                    "G"
    in
    "  " ++ raceDisplay ++ "(" ++ String.fromInt fighter.health ++ ")"


movementExample2 =
    """#########
#G..G..G#
#.......#
#.......#
#G..E..G#
#.......#
#.......#
#G..G..G#
#########"""


exampleText1 =
    """#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######"""


exampleText2 =
    """#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######"""


exampleText3 =
    """#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######"""


exampleText4 =
    """#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
#######"""


exampleText5 =
    """#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######"""


exampleText6 =
    """#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########"""


exampleText7 =
    """#########
#.......#
#.......#
#.......#
#...E...#
#......G#
#.G.....#
#.......#
#########"""


inputText =
    """################################
##########..####################
##########..G###################
##########..#.....########.#####
##########........########G#####
############...#..########.#####
################....######.#####
#################..G####...#####
################...#..#....#####
################...G..#.....E###
##############.G..........G....#
###########.G...G..............#
###########G..#####..........###
###########..#######.........###
##########.G#########........#.#
#########...#########....G.....#
#########...#########.........##
##..........#########.........##
######....G.#########.....E....#
##...........#######.......#...#
#...G.........#####E.......#####
##....................#..#######
##.G.................##.########
##..#GG.............###...#..###
#G..#..G.G........G.####.#..E###
#.....#.##...........###.....###
#######...............###EE..###
########.....E........###....###
########..............####..####
##########....E....#...###.#####
###########...EE....#.##########
################################"""
