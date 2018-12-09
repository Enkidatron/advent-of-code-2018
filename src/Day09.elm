module Day09 exposing (GameConfig, GameState, advanceNormal, advanceScoringRound, getHighestScore, initialState, insertAfter, lastMarble, numPlayers, part1, part1Answer, part2Answer, playGame, removeAt, startGame, test1)

import Array exposing (Array)
import Exts.Array


numPlayers =
    428


lastMarble =
    70825


type alias GameState =
    { nextMarble : Int
    , playerScores : Array Int
    , circleOfMarbles : Array Int
    , currentIndex : Int
    }


type alias GameConfig =
    { numPlayers : Int
    , lastMarble : Int
    }


initialState config =
    { nextMarble = 1
    , playerScores = Array.repeat config.numPlayers 0
    , circleOfMarbles = Array.fromList [ 0 ]
    , currentIndex = 0
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
        insertAfterIndex =
            state.currentIndex
                + 1
                |> Basics.remainderBy (Array.length state.circleOfMarbles)

        newCircle =
            insertAfter insertAfterIndex state.nextMarble state.circleOfMarbles
    in
    { state
        | nextMarble = state.nextMarble + 1
        , circleOfMarbles = newCircle
        , currentIndex = insertAfterIndex + 1 |> Basics.remainderBy (Array.length newCircle)
    }


insertAfter : Int -> a -> Array a -> Array a
insertAfter breakpoint item array =
    let
        left =
            Array.slice 0 (breakpoint + 1) array
                |> Array.push item

        right =
            Array.slice (breakpoint + 1) (Array.length array) array
    in
    Array.append left right


advanceScoringRound : GameConfig -> GameState -> GameState
advanceScoringRound config state =
    let
        newIndex =
            state.currentIndex
                - 7
                |> Basics.modBy (Array.length state.circleOfMarbles)

        removedMarble =
            Array.get newIndex state.circleOfMarbles
                |> Maybe.withDefault 0

        score =
            state.nextMarble + removedMarble

        newCircle =
            removeAt newIndex state.circleOfMarbles

        playerNumber =
            state.nextMarble |> Basics.modBy config.numPlayers

        newScores =
            Exts.Array.update playerNumber (\x -> x + score) state.playerScores
    in
    { nextMarble = state.nextMarble + 1
    , playerScores = newScores
    , circleOfMarbles = newCircle
    , currentIndex = newIndex
    }


removeAt : Int -> Array a -> Array a
removeAt index array =
    let
        left =
            Array.slice 0 index array

        right =
            Array.slice (index + 1) (Array.length array) array
    in
    Array.append left right


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
    -- Unusably slow. See Day09Alt for the solution that worked for this
    part1 numPlayers (lastMarble * 100)
