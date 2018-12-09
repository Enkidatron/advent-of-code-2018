module Day09Alt exposing (Circle(..), GameConfig, GameState, GameState2, advanceNormal, advanceScoringRound, circleGetCurrent, circleInsertAfter, circleRemoveCurrent, circleSingleton, getHighestScore, initialState, lastMarble, moveCircle, numPlayers, part1, part1Answer, part2Answer, playGame, startGame, test1, unsafeMaybe)

import Array exposing (Array)
import Exts.Array
import List.Extra


numPlayers =
    428


lastMarble =
    70825


type Circle a
    = Circle
        { left : List a
        , current : a
        , right : List a
        }


circleSingleton : a -> Circle a
circleSingleton thing =
    Circle
        { left = []
        , current = thing
        , right = []
        }


circleToList : Circle a -> List a
circleToList (Circle circle) =
    List.reverse circle.left ++ (circle.current :: circle.right)


moveCircle : Int -> Circle a -> Circle a
moveCircle amount (Circle circle) =
    let
        ( newAmount, newCircle ) =
            case compare amount 0 of
                Basics.EQ ->
                    ( 0, Circle circle )

                Basics.LT ->
                    Tuple.pair (amount + 1) <|
                        case circle.left of
                            [] ->
                                case List.Extra.unconsLast circle.right of
                                    Nothing ->
                                        Circle circle

                                    Just ( endRight, restOfRight ) ->
                                        Circle
                                            { circle
                                                | current = endRight
                                                , right = circle.current :: restOfRight
                                            }

                            leftHead :: restOfLeft ->
                                Circle
                                    { left = restOfLeft
                                    , current = leftHead
                                    , right = circle.current :: circle.right
                                    }

                Basics.GT ->
                    Tuple.pair (amount - 1) <|
                        case circle.right of
                            rightHead :: restOfRight ->
                                Circle
                                    { left = circle.current :: circle.left
                                    , current = rightHead
                                    , right = restOfRight
                                    }

                            [] ->
                                case List.Extra.unconsLast circle.left of
                                    Nothing ->
                                        Circle circle

                                    Just ( farLeft, restOfLeft ) ->
                                        Circle
                                            { left = [ circle.current ]
                                            , current = farLeft
                                            , right = List.reverse restOfLeft
                                            }
    in
    if amount == 0 then
        Circle circle

    else
        moveCircle newAmount newCircle


unsafeMaybe : Maybe a -> a
unsafeMaybe maybe =
    case maybe of
        Just thing ->
            thing

        Nothing ->
            Debug.todo "unsafeMaybe"


circleInsertAfter : a -> Circle a -> Circle a
circleInsertAfter thing (Circle circle) =
    Circle
        { circle
            | current = thing
            , left = circle.current :: circle.left
        }


circleGetCurrent : Circle a -> a
circleGetCurrent (Circle circle) =
    circle.current


circleRemoveCurrent : Circle a -> Circle a
circleRemoveCurrent (Circle circle) =
    case circle.right of
        x :: restOfRight ->
            Circle
                { circle
                    | right = restOfRight
                    , current = x
                }

        [] ->
            case List.Extra.unconsLast circle.left of
                Just ( farLeft, restOfLeft ) ->
                    Circle
                        { left = []
                        , right = List.reverse restOfLeft
                        , current = farLeft
                        }

                Nothing ->
                    Debug.todo "empty circle"


type alias GameState =
    { nextMarble : Int
    , playerScores : Array Int
    , circleOfMarbles : Circle Int
    }


type alias GameState2 =
    { nextMarble : Int
    , playerScores : Array Int
    , circleOfMarbles : Circle Int
    }


type alias GameConfig =
    { numPlayers : Int
    , lastMarble : Int
    }


initialState config =
    { nextMarble = 1
    , playerScores = Array.repeat config.numPlayers 0
    , circleOfMarbles = circleSingleton 0
    }


startGame : GameConfig -> GameState
startGame config =
    playGame config (initialState config)


playGame : GameConfig -> GameState -> GameState
playGame config state =
    let
        newState =
            if remainderBy 23 state.nextMarble == 0 then
                advanceScoringRound config state

            else
                advanceNormal config state
    in
    if newState.nextMarble > config.lastMarble then
        newState

    else
        playGame config newState


advanceNormal : GameConfig -> GameState -> GameState
advanceNormal config state =
    let
        newCircle =
            state.circleOfMarbles |> moveCircle 1 |> circleInsertAfter state.nextMarble
    in
    { state
        | nextMarble = state.nextMarble + 1
        , circleOfMarbles = newCircle
    }


advanceScoringRound : GameConfig -> GameState -> GameState
advanceScoringRound config state =
    let
        rotatedCircle =
            state.circleOfMarbles |> moveCircle -7

        removedMarble =
            rotatedCircle
                |> circleGetCurrent

        score =
            state.nextMarble + removedMarble

        newCircle =
            rotatedCircle |> circleRemoveCurrent

        playerNumber =
            state.nextMarble |> Basics.modBy config.numPlayers

        newScores =
            Exts.Array.update playerNumber (\x -> x + score) state.playerScores
    in
    { nextMarble = state.nextMarble + 1
    , playerScores = newScores
    , circleOfMarbles = newCircle
    }


getHighestScore : GameState -> Int
getHighestScore state =
    state.playerScores |> Array.toList |> List.maximum |> Maybe.withDefault 0


test1 =
    part1 10 1618


part1 players marbles =
    startGame (GameConfig players marbles) |> getHighestScore


part1Answer =
    part1 numPlayers lastMarble


part2Answer =
    part1 numPlayers (lastMarble * 100)
