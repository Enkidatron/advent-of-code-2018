module Day22 exposing (Cave, CaveType(..), Frontier, Point, State, Tool(..), caveTypeToRiskLevel, caveTypeToString, depth, erosionLevelToCaveType, exampleDepth, exampleTarget, findNewFrontiers, findPathTime, findPointsAround, frontierSorting, gIndexToErosionLevel, makeCave, makeCaveCell, makeCaveRow, makeNewFrontiers, makeState, manhattanDistance, part1Answer, part2, part2test1, part2test2, target, unsafeMaybe, visualizeCave)

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Point =
    ( Int, Int )


type CaveType
    = Rocky
    | Wet
    | Narrow


gIndexToErosionLevel caveDepth gIndex =
    gIndex
        + caveDepth
        |> Basics.modBy 20183


erosionLevelToCaveType level =
    case Basics.modBy 3 level of
        0 ->
            Rocky

        1 ->
            Wet

        2 ->
            Narrow

        _ ->
            Rocky


caveTypeToRiskLevel caveType =
    case caveType of
        Rocky ->
            0

        Wet ->
            1

        Narrow ->
            2


makeCave caveDepth ourTarget ( maxX, maxY ) =
    List.foldl (makeCaveRow caveDepth ourTarget maxX maxY) Dict.empty (List.range 0 maxY)


makeCaveRow caveDepth ourTarget maxX maxY y cave =
    List.foldl (makeCaveCell caveDepth ourTarget maxX maxY y) cave (List.range 0 maxX)


makeCaveCell caveDepth ourTarget maxX maxY y x cave =
    let
        geologicIndex =
            if x == 0 && y == 0 then
                0

            else if ( x, y ) == ourTarget then
                0

            else if y == 0 then
                x * 16807

            else if x == 0 then
                y * 48271

            else
                Maybe.map2 (*)
                    (Dict.get ( x - 1, y ) cave |> Maybe.map (gIndexToErosionLevel caveDepth))
                    (Dict.get ( x, y - 1 ) cave |> Maybe.map (gIndexToErosionLevel caveDepth))
                    |> unsafeMaybe
    in
    Dict.insert ( x, y ) geologicIndex cave


unsafeMaybe maybe =
    case maybe of
        Just thing ->
            thing

        Nothing ->
            Debug.todo "unsafeMaybe"


part1Answer =
    makeCave depth target target
        |> Dict.toList
        |> List.map Tuple.second
        |> List.map (gIndexToErosionLevel depth >> erosionLevelToCaveType >> caveTypeToRiskLevel)
        |> List.sum


depth =
    11109


exampleDepth =
    510


target =
    ( 9, 731 )


exampleTarget =
    ( 10, 10 )


type Tool
    = ClimbingGear
    | Torch
    | Neither


visualizeCave : Point -> Dict Point CaveType -> Dict Point CaveType
visualizeCave ourTarget cave =
    let
        maxX =
            Dict.keys cave |> List.map Tuple.first |> List.maximum |> Maybe.withDefault 0

        maxY =
            Dict.keys cave |> List.map Tuple.second |> List.maximum |> Maybe.withDefault 0

        message =
            List.range 0 maxY
                |> List.map
                    (\y ->
                        List.range 0 maxX
                            |> List.map
                                (\x ->
                                    if ( x, y ) == ( 0, 0 ) then
                                        "M"

                                    else if ( x, y ) == ourTarget then
                                        "T"

                                    else
                                        Dict.get ( x, y ) cave |> Maybe.map caveTypeToString |> Maybe.withDefault " "
                                )
                            |> String.join ""
                    )
                |> String.join "\n"

        _ =
            Debug.log message ()
    in
    cave


caveTypeToString caveType =
    case caveType of
        Rocky ->
            "."

        Wet ->
            "="

        Narrow ->
            "|"


part2 caveDepth ( x, y ) =
    let
        cave =
            makeCave caveDepth ( x, y ) ( x + 100, y + 100 )
                |> Dict.map (\point gIndex -> gIndex |> gIndexToErosionLevel caveDepth |> erosionLevelToCaveType)
                |> visualizeCave ( x, y )
    in
    makeState ( x, y ) cave
        |> findPathTime ( x, y )


type alias State =
    { cave : Cave
    , frontier : FrontierQueue
    , interior : Dict Point (List Tool)
    }


type alias Cave =
    Dict Point CaveType


type alias FrontierQueue =
    List ( Int, Frontier )


type alias Frontier =
    { point : Point
    , equipment : Tool
    , timeElapsed : Int
    }


insertFrontier : Point -> Frontier -> FrontierQueue -> FrontierQueue
insertFrontier ourTarget frontier queue =
    let
        frontierCost =
            frontierSorting ourTarget frontier
    in
    insertFrontierHelper ( frontierCost, frontier ) [] queue


insertFrontierHelper : ( Int, Frontier ) -> FrontierQueue -> FrontierQueue -> FrontierQueue
insertFrontierHelper ( cost, frontier ) closerStack queue =
    case queue of
        [] ->
            List.reverse (( cost, frontier ) :: closerStack)

        ( frontCost, frontFrontier ) :: queueRest ->
            if frontFrontier.point == frontier.point && frontFrontier.equipment == frontier.equipment then
                if frontCost > cost then
                    List.reverse (( cost, frontier ) :: closerStack) ++ queueRest

                else
                    List.reverse closerStack ++ queue

            else if frontCost >= cost then
                List.reverse (( cost, frontier ) :: closerStack)
                    ++ List.filter (\( c, f ) -> f.point /= frontier.point || f.equipment /= frontier.equipment) queue

            else
                insertFrontierHelper ( cost, frontier ) (( frontCost, frontFrontier ) :: closerStack) queueRest


makeState : Point -> Dict Point CaveType -> State
makeState ourTarget cave =
    { cave = cave
    , frontier = insertFrontier ourTarget (Frontier ( 0, 0 ) Torch 0) []
    , interior = Dict.empty
    }


findPathTime : Point -> State -> Int
findPathTime ourTarget state =
    case state.frontier of
        [] ->
            Debug.todo "ran out of frontier"

        ( cost, nextFrontier ) :: restFrontier ->
            if nextFrontier.point == ourTarget && nextFrontier.equipment == Torch then
                nextFrontier.timeElapsed

            else
                let
                    _ =
                        ( frontierSorting ourTarget nextFrontier, nextFrontier )
                            |> Debug.log "nextFrontier"

                    newFrontiers =
                        nextFrontier
                            |> findNewFrontiers state.cave state.interior
                            |> List.map (\f -> ( frontierSorting ourTarget f, f ))
                            |> Debug.log "newFrontiers"
                            |> List.map Tuple.second
                            |> List.foldl (insertFrontier ourTarget) restFrontier

                    -- _ =
                    --     newFrontiers
                    --         |> List.take 3
                    --         |> List.map (\f -> ( frontierSorting ourTarget f, f ))
                    --         |> Debug.log "newFrontiers (3)"
                    _ =
                        Debug.log "" ""

                    newInterior =
                        Dict.update nextFrontier.point
                            (Maybe.withDefault [] >> (::) nextFrontier.equipment >> Just)
                            state.interior
                in
                findPathTime ourTarget
                    { state | frontier = newFrontiers, interior = newInterior }


frontierSorting : Point -> Frontier -> Int
frontierSorting ourTarget frontier =
    let
        toolChangeOffset =
            if frontier.equipment == Torch then
                0

            else
                7
    in
    frontier.timeElapsed
        + manhattanDistance frontier.point ourTarget
        + toolChangeOffset


manhattanDistance ( x1, y1 ) ( x2, y2 ) =
    abs (x1 - x2) + abs (y1 - y2)


findNewFrontiers : Cave -> Dict Point (List Tool) -> Frontier -> List Frontier
findNewFrontiers cave interior frontier =
    findPointsAround frontier.point
        |> List.concatMap (makeNewFrontiers cave frontier)
        |> List.filter (\f -> Dict.get f.point interior |> Maybe.withDefault [] |> List.member f.equipment |> not)


findPointsAround : Point -> List Point
findPointsAround ( x, y ) =
    [ ( x - 1, y )
    , ( x + 1, y )
    , ( x, y - 1 )
    , ( x, y + 1 )
    ]


makeNewFrontiers : Cave -> Frontier -> Point -> List Frontier
makeNewFrontiers cave prevFrontier point =
    let
        nextCaveType =
            Dict.get point cave

        prevCaveType =
            Dict.get prevFrontier.point cave |> unsafeMaybe

        toolChoices =
            case ( nextCaveType, prevCaveType ) of
                ( Nothing, _ ) ->
                    []

                ( Just Rocky, Rocky ) ->
                    [ ClimbingGear, Torch ]

                ( Just Rocky, Wet ) ->
                    [ ClimbingGear ]

                ( Just Rocky, Narrow ) ->
                    [ Torch ]

                ( Just Wet, Rocky ) ->
                    [ ClimbingGear ]

                ( Just Wet, Wet ) ->
                    [ ClimbingGear, Neither ]

                ( Just Wet, Narrow ) ->
                    [ Neither ]

                ( Just Narrow, Rocky ) ->
                    [ Torch ]

                ( Just Narrow, Wet ) ->
                    [ Neither ]

                ( Just Narrow, Narrow ) ->
                    [ Torch, Neither ]

        makeFrontier newTool =
            { point = point
            , equipment = newTool
            , timeElapsed =
                if newTool == prevFrontier.equipment then
                    prevFrontier.timeElapsed + 1

                else
                    prevFrontier.timeElapsed + 8
            }
    in
    List.map makeFrontier toolChoices


part2test1 =
    Dict.fromList
        [ ( ( 0, 0 ), Rocky )
        , ( ( 0, 1 ), Wet )
        , ( ( 0, 2 ), Rocky )
        , ( ( 0, 3 ), Rocky )
        , ( ( 1, 0 ), Rocky )
        , ( ( 1, 1 ), Wet )
        , ( ( 1, 2 ), Wet )
        , ( ( 1, 3 ), Rocky )
        , ( ( 2, 0 ), Rocky )
        , ( ( 2, 1 ), Rocky )
        , ( ( 2, 2 ), Rocky )
        , ( ( 2, 3 ), Rocky )
        ]
        |> makeState ( 0, 2 )
        |> findPathTime ( 0, 2 )


part2test2 =
    Dict.fromList
        [ ( ( 0, 0 ), Rocky )
        , ( ( 0, 1 ), Wet )
        , ( ( 0, 2 ), Rocky )
        , ( ( 0, 3 ), Rocky )
        , ( ( 0, 4 ), Rocky )
        , ( ( 1, 0 ), Rocky )
        , ( ( 1, 1 ), Wet )
        , ( ( 1, 2 ), Wet )
        , ( ( 1, 3 ), Wet )
        , ( ( 1, 4 ), Rocky )
        , ( ( 2, 0 ), Rocky )
        , ( ( 2, 1 ), Rocky )
        , ( ( 2, 2 ), Rocky )
        , ( ( 2, 3 ), Rocky )
        , ( ( 2, 4 ), Rocky )
        ]
        |> makeState ( 0, 2 )
        |> findPathTime ( 0, 2 )
